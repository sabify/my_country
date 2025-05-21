use anyhow::{Context, Result};
use proc_macro2::{Ident, Span, TokenStream};
use quote::{ToTokens, quote};
use serde::{Deserialize, Deserializer, de::DeserializeOwned};
use std::{
    collections::{HashMap, HashSet},
    env,
    ffi::OsStr,
    fmt::Display,
    fs::{self, File},
    io::{BufWriter, Write},
    path::{Path, PathBuf},
    str::FromStr,
};
use syn::{Field, Fields, FieldsNamed, ItemStruct, parse_quote};
use walkdir::WalkDir;

type Features = HashSet<String>;
type Countries = HashMap<String, HashMap<String, Country>>;
type Subdivisions = HashMap<String, HashMap<String, Subdivision>>;
type Translations = HashMap<String, HashMap<String, String>>;
type Currencies = Vec<Currency>;

#[derive(Deserialize)]
struct Country {
    alpha2: String,
    alpha3: String,
    alt_currency: Option<String>,
    continent: String,
    country_code: String,
    currency_code: String,
    distance_unit: String,
    gec: Option<String>,
    geo: Geo,
    international_prefix: Option<String>,
    ioc: Option<String>,
    iso_long_name: String,
    iso_short_name: String,
    languages_official: Option<Vec<String>>,
    languages_spoken: Option<Vec<String>>,
    national_destination_code_lengths: Option<Vec<i32>>,
    national_number_lengths: Option<Vec<i32>>,
    national_prefix: Option<String>,
    nationality: Option<String>,
    #[serde(rename = "number", deserialize_with = "deserialize_string_to_scalar")]
    numeric_code: u16,
    postal_code: bool,
    postal_code_format: Option<String>,
    region: Option<String>,
    start_of_week: String,
    subregion: Option<String>,
    un_locode: String,
    un_member: bool,
    unofficial_names: Vec<String>,
    vehicle_registration_code: Option<String>,
    world_region: String,
    nanp_prefix: Option<String>,
    address_format: Option<String>,
    vat_rates: Option<VatRates>,
    eea_member: Option<bool>,
    eu_member: Option<bool>,
    g20_member: Option<bool>,
    euvat_member: Option<bool>,
    esm_member: Option<bool>,
    g7_member: Option<bool>,
    emoji_flag: Option<String>,
}

#[derive(Clone, Deserialize)]
struct Geo {
    latitude: Option<f64>,
    longitude: Option<f64>,
    min_latitude: Option<f64>,
    min_longitude: Option<f64>,
    max_latitude: Option<f64>,
    max_longitude: Option<f64>,
    bounds: Option<Bounds>,
}

#[derive(Clone, Deserialize)]
struct Bounds {
    northeast: Coordinates,
    southwest: Coordinates,
}

#[derive(Clone, Deserialize)]
struct Coordinates {
    lat: f64,
    lng: f64,
}

#[derive(Deserialize)]
struct VatRates {
    standard: Option<f64>,
    reduced: Option<Vec<f64>>,
    super_reduced: Option<f64>,
    parking: Option<f64>,
}

#[derive(Deserialize)]
struct Subdivision {
    name: String,
    code: String,
    translations: HashMap<String, String>,
    #[serde(rename = "type")]
    r#type: String,
    #[serde(default, deserialize_with = "deserialize_opt_vec_string")]
    unofficial_names: Option<Vec<String>>,
    geo: Option<Geo>,
    comments: Option<String>,
}

#[derive(Debug, Deserialize)]
struct Currency {
    #[serde(rename = "Entity")]
    entity: String,
    #[serde(rename = "Currency")]
    name: String,
    #[serde(rename = "AlphabeticCode")]
    alphabetic_code: String,
    #[serde(
        rename = "NumericCode",
        deserialize_with = "deserialize_currency_number"
    )]
    numeric_code: u16,
    #[serde(rename = "MinorUnit", deserialize_with = "deserialize_currency_number")]
    minor_unit: u8,
    #[serde(rename = "WithdrawalDate")]
    withdrawal_date: Option<String>,
}

// Custom deserializer for converting strings to integer types
fn deserialize_currency_number<'de, D, T>(deserializer: D) -> Result<T, D::Error>
where
    D: Deserializer<'de>,
    T: FromStr + Deserialize<'de> + Default,
    T::Err: Display,
{
    let string_value = String::deserialize(deserializer)?;
    if string_value.is_empty() || string_value == "-" {
        return Ok(Default::default());
    }
    string_value.parse::<T>().map_err(serde::de::Error::custom)
}

// Custom deserializer for converting strings to integer types
fn deserialize_string_to_scalar<'de, D, T>(deserializer: D) -> Result<T, D::Error>
where
    D: Deserializer<'de>,
    T: FromStr + Deserialize<'de>,
    T::Err: Display,
{
    let string_value = String::deserialize(deserializer)?;
    string_value.parse::<T>().map_err(serde::de::Error::custom)
}

// A helper enum to deserialize either a single `T` or a `Vec<T>`.
#[derive(Deserialize)]
#[serde(untagged)]
enum VecOr<T> {
    Single(T),
    Multiple(Vec<T>),
}

// Custom deserialization function for Option<Vec<T>>.
// It supports cases where the input is a single T or a vector of T.
fn deserialize_opt_vec_string<'de, D, T>(deserializer: D) -> Result<Option<Vec<T>>, D::Error>
where
    D: Deserializer<'de>,
    T: FromStr + Deserialize<'de>,
    T::Err: Display,
{
    // First attempt to deserialize to an Option<StringOrVec>.
    let opt = Option::<VecOr<T>>::deserialize(deserializer)?;
    // Map the untagged enum to our desired Option<Vec<String>> output.
    let result = match opt {
        Some(VecOr::Single(s)) => Some(vec![s]),
        Some(VecOr::Multiple(v)) => Some(v),
        None => None,
    };
    Ok(result)
}

fn make_safe_ident<T: Display>(name: T) -> Ident {
    // Try to parse as a normal identifier
    match syn::parse_str::<Ident>(&format!("{name}")) {
        Ok(ident) => ident,
        Err(_) => {
            // If parsing fails (likely because it's a keyword), use raw identifier
            let raw_name = format!("r#{}", name);
            syn::parse_str::<Ident>(&raw_name).unwrap_or_else(|_| {
                // Fall back to creating a new identifier if all else fails
                Ident::new(&raw_name, Span::call_site())
            })
        }
    }
}

fn load_yaml_files<T>(dir_path: impl AsRef<Path>, recursive: bool) -> Result<HashMap<String, T>>
where
    T: DeserializeOwned,
{
    load_yaml_files_with_modifier(dir_path, recursive, |s: String| s)
}
// Deserialize all YAML files in a directory into the specified type
fn load_yaml_files_with_modifier<T, F>(
    dir_path: impl AsRef<Path>,
    recursive: bool,
    key_modifier: F,
) -> Result<HashMap<String, T>>
where
    F: Fn(String) -> String,
    T: DeserializeOwned,
{
    let dir_path = dir_path.as_ref();

    // Ensure directory exists
    if !dir_path.is_dir() {
        anyhow::bail!("Not a directory: {}", dir_path.display());
    }

    let mut results = HashMap::new();

    // Use WalkDir to iterate through directory contents
    let walker = if recursive {
        WalkDir::new(dir_path)
    } else {
        WalkDir::new(dir_path).max_depth(1)
    };

    for entry in walker {
        let entry = entry.context("Failed to access directory entry")?;
        let path = entry.path();

        // Process only YAML files
        if path.is_file()
            && (path.extension() == Some(OsStr::new("yaml"))
                || path.extension() == Some(OsStr::new("yml")))
        {
            // Read file contents
            let content = fs::read_to_string(path)
                .with_context(|| format!("Failed to read YAML file: {}", path.display()))?;

            // Deserialize YAML content
            let data: T = serde_yaml::from_str(&content)
                .with_context(|| format!("Failed to parse YAML file: {}", path.display()))?;

            results.insert(
                key_modifier(
                    path.file_stem()
                        .unwrap()
                        .to_string_lossy()
                        .to_string()
                        .to_lowercase(),
                ),
                data,
            );
        }
    }

    Ok(results)
}

macro_rules! generate_method {
    (@if_true $features:ident, $name:ident, $if_true:block) => {
        if $features.contains(stringify!($name)) {
            $if_true
        } else {
            TokenStream::new()
        }
    };
    (@iter $countries:ident, $name:ident, $fn:ident) => {
        $countries
            .iter()
            .map(|(_, h)| {
                h.iter()
                    .map(|(_, country)| {
                        let variant = make_safe_ident(&country.alpha2);
                        let $name = $fn(country);

                        quote! {
                            Self::#variant => #$name,
                        }
                    })
                    .collect::<TokenStream>()
            })
            .collect()
    };
    (@ret $ret:ty, $name:ident, $doc:literal, $code:ident) => {
            quote! {
                impl Country {
                    #[doc = $doc]
                    pub fn $name(&self) -> $ret {
                        match self {
                            #$code
                        }
                    }
                }
            }
    };
    ($name:tt, $doc:literal, Option<Vec<$ret:ty>>, $features:ident, $countries:ident) => {{
        generate_method!(@if_true $features, $name,
            {
                let iter_fn =
                    |country: &Country| if let Some($name) = &country.$name {
                        let items = $name;
                        quote! {
                            Some(&[#(#items),*])
                        }
                    } else {
                        quote! {
                            None
                        }
                    };
                let all: TokenStream = generate_method!(@iter $countries, $name, iter_fn);
                generate_method!(@ret Option<&[$ret]>, $name, $doc, all)
            }
        )
    }};
    ($name:tt, $doc:literal, Vec<$ret:ty>, $features:ident, $countries:ident) => {{
        generate_method!(@if_true $features, $name,
            {
                let iter_fn = |country: &Country| {
                    let items = country.$name.clone();
                    quote! {
                        &[#(#items),*]
                    }
                };
                let all: TokenStream = generate_method!(@iter $countries, $name, iter_fn);
                generate_method!(@ret &[$ret], $name, $doc, all)
            }
        )
    }};
    ($name:tt, $doc:literal, Option<$ret:ty>, $features:ident, $countries:ident) => {{
        generate_method!(@if_true $features, $name,
            {
                let iter_fn = |country: &Country|  if let Some($name) = &country.$name {
                    quote! {
                        Some(#$name)
                    }
                } else {
                    quote! {
                        None
                    }
                };
                let all: TokenStream = generate_method!(@iter $countries, $name, iter_fn);
                generate_method!(@ret Option<$ret>, $name, $doc, all)
            }
        )
    }};
    ($name:tt, $doc:literal, $ret:ty, $features:ident, $countries:ident) => {{
        generate_method!(@if_true $features, $name,
            {
                let iter_fn = |country: &Country| {
                    let item = country.$name.clone();
                        quote!{
                            #item
                        }
                };
                let all: TokenStream = generate_method!(@iter $countries, $name, iter_fn);
                generate_method!(@ret $ret, $name, $doc, all)
            }
        )
    }};
    (trusted $name:tt, $doc:literal, Option<$ret:ty>, $features:ident, $countries:ident) => {{
        generate_method!(@if_true $features, $name,
            {
                let iter_fn = |country: &Country| {
                    let item = country.$name.clone().unwrap();
                        quote!{
                            #item
                        }
                };
                let all: TokenStream = generate_method!(@iter $countries, $name, iter_fn);
                generate_method!(@ret $ret, $name, $doc, all)
            }
        )
    }};
    (currency $name:tt, $doc:literal, $ret:ty, $features:ident, $countries:ident) => {{
        generate_method!(@if_true $features, $name,
            {
                let iter_fn = |country: &Country| {
                    let item = make_safe_ident(&country.$name.to_uppercase());
                        quote!{
                            Currency::#item
                        }
                };
                let all: TokenStream = generate_method!(@iter $countries, $name, iter_fn);
                generate_method!(@ret $ret, $name, $doc, all)
            }
        )
    }};
}

macro_rules! generate_currency_method {
    (@if_true $features:ident, $name:ident, $if_true:block) => {
        if $features.contains(&format!("currency_{}", stringify!($name))) {
            $if_true
        } else {
            TokenStream::new()
        }
    };
    (@iter $currencies:ident, $name:ident, $fn:ident) => {
        $currencies
            .iter()
            .map(|currency| {
                let variant = make_safe_ident(&currency.alphabetic_code);
                let $name = $fn(currency);

                quote! {
                    Self::#variant => #$name,
                }
            })
            .collect()
    };
    (@ret $ret:ty, $name:ident, $doc:literal, $code:ident) => {
            quote! {
                impl Currency {
                    #[doc = $doc]
                    pub fn $name(&self) -> $ret {
                        match self {
                            #$code
                        }
                    }
                }
            }
    };
    ($name:tt, $doc:literal, $ret:ty, $features:ident, $currencies:ident) => {{
        generate_currency_method!(@if_true $features, $name,
            {
                let iter_fn = |currency: &Currency| {
                    let item = currency.$name.clone();
                        quote!{
                            #item
                        }
                };
                let all: TokenStream = generate_currency_method!(@iter $currencies, $name, iter_fn);
                generate_currency_method!(@ret $ret, $name, $doc, all)
            }
        )
    }};
}

macro_rules! field_tokens {
    ($features:ident, $item:ident, Option<Vec<$name:tt>>) => {{
        if $features.contains(&format!("{}_{}", stringify!($item), stringify!($name))) {
            if let Some($name) = &$item.$name {
                let $name = $name.iter().collect::<Vec<_>>();
                quote! { $name: Some(&[#(#$name),*]), }
            } else {
                quote! { $name: None, }
            }
        } else {
            quote! {}
        }
    }};
    ($features:ident, $item:ident, Option<$name:tt>) => {{
        if $features.contains(&format!("{}_{}", stringify!($item), stringify!($name))) {
            if let Some($name) = &$item.$name {
                quote! { $name: Some(#$name), }
            } else {
                quote! { $name: None, }
            }
        } else {
            quote! {}
        }
    }};
    ($features:ident, $item:ident, $name:tt, $func:ident) => {{
        if $features.contains(&format!("{}_{}", stringify!($item), stringify!($name))) {
            let $name = $func($features, &$item.$name);
            quote! { $name: #$name, }
        } else {
            quote! {}
        }
    }};
    ($features:ident, $item:ident, $name:tt) => {{
        if $features.contains(&format!("{}_{}", stringify!($item), stringify!($name))) {
            let $name = &$item.$name;
            quote! { $name: #$name, }
        } else {
            quote! {}
        }
    }};
}

fn generate_impl_from_country(features: &Features) -> TokenStream {
    let has_currency_feature = features.iter().filter(|f| f.contains("currency")).count() > 0;
    let alpha2_related = if features.contains("alpha2") {
        let for_currency = if has_currency_feature {
            let iterator_feature = if features.contains("iterator") {
                quote! {
                    use strum::IntoEnumIterator;

                    // Get countries from currency
                    impl From<Currency> for Vec<Country> {
                        fn from(currency: Currency) -> Self {
                            Country::iter()
                                .filter(|c| c.currency_code() == currency)
                                .collect()
                        }
                    }

                    impl From<&Currency> for Vec<Country> {
                        fn from(currency: &Currency) -> Self {
                            (*currency).into()
                        }
                    }
                }
            } else {
                quote! {}
            };
            quote! {
                // Get currency from country
                impl From<Country> for Currency {
                    fn from(country: Country) -> Self {
                        country.currency_code()
                    }
                }

                impl From<&Country> for Currency {
                    fn from(country: &Country) -> Self {
                        (*country).into()
                    }
                }

                #iterator_feature
            }
        } else {
            quote! {}
        };

        quote! {
            // Alpha-2 string representation (two-letter codes)
            impl From<Country> for &'static str {
                fn from(country: Country) -> Self {
                    country.alpha2()
                }
            }

            // Alpha-2 string representation (two-letter codes)
            impl From<Country> for String {
                fn from(country: Country) -> Self {
                    country.alpha2().to_string()
                }
            }

            #for_currency
        }
    } else {
        quote! {}
    };

    let name_related = if features.contains("name") {
        quote! {
            // String representation for display
            impl fmt::Display for Country {
                fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                    write!(f, "{}", self.name())
                }
            }
        }
    } else {
        quote! {}
    };

    quote! {
        #alpha2_related
        #name_related
    }
}

fn generate_impl_from_numeric_code(features: &Features, countries: &Countries) -> TokenStream {
    if !features.contains("numeric_code") {
        return quote! {};
    }
    let from_numeric_code_arms: TokenStream = countries
        .iter()
        .map(|(_, h)| {
            h.iter()
                .map(|(_, country)| {
                    let variant = make_safe_ident(&country.alpha2);
                    let numeric_code = &country.numeric_code;

                    quote! {
                        #numeric_code => Ok(Self::#variant),
                    }
                })
                .collect::<TokenStream>()
        })
        .collect();

    quote! {
        impl TryFrom<u16> for Country {
            type Error = ParseError;

            fn try_from(num: u16) -> Result<Self, Self::Error> {
                match num {
                    #from_numeric_code_arms
                    _ => Err(ParseError::Unknown(num.to_string())),
                }
            }
        }

    }
}

fn generate_parse_error(features: &Features) -> TokenStream {
    let has_currency_feature = features.iter().filter(|f| f.contains("currency")).count() > 0;
    if !features.contains("alpha2")
        && !features.contains("alpha3")
        && !features.contains("numeric_code")
        && !has_currency_feature
    {
        return quote! {};
    }

    quote! {
        // Error type for parsing failures
        #[derive(thiserror::Error, Debug, Clone, PartialEq)]
        pub enum ParseError {
            #[error("unknown code: {0}")]
            Unknown(String),
        }

    }
}

fn generate_impl_from_str(features: &Features, countries: &Countries) -> TokenStream {
    if !features.contains("alpha2") {
        return quote! {};
    }
    let from_str_arms: TokenStream = countries
        .iter()
        .map(|(_, h)| {
            h.iter()
                .map(|(_, country)| {
                    let variant = make_safe_ident(&country.alpha2);
                    let alpha2 = &country.alpha2;

                    quote! {
                         #alpha2 => Ok(Self::#variant),
                    }
                })
                .collect::<TokenStream>()
        })
        .collect();

    quote! {
        // Parse from alpha-2 code
        impl std::str::FromStr for Country {
            type Err = ParseError;

            fn from_str(s: &str) -> Result<Self, Self::Err> {
                match s {
                    #from_str_arms
                    _ => Err(ParseError::Unknown(s.to_string())),
                }
            }
        }
    }
}

fn generate_impl_from_str_alpha3(features: &Features, countries: &Countries) -> TokenStream {
    if !features.contains("alpha3") {
        return quote! {};
    }
    let from_str_arms: TokenStream = countries
        .iter()
        .map(|(_, h)| {
            h.iter()
                .map(|(_, country)| {
                    let variant = make_safe_ident(&country.alpha2);
                    let alpha3 = &country.alpha3;

                    quote! {
                         #alpha3 => Ok(Self::#variant),
                    }
                })
                .collect::<TokenStream>()
        })
        .collect();

    quote! {
        // Parse from alpha-3 code
        impl TryFrom<&str> for Country {
            type Error = ParseError;

            fn try_from(s: &str) -> Result<Self, Self::Error> {
                match s {
                    #from_str_arms
                    _ => {
                        // Try parsing as alpha-2 if it's a two-letter code
                        if s.len() == 2 {
                            s.parse()
                        } else {
                            Err(ParseError::Unknown(s.to_string()))
                        }
                    }
                }
            }
        }
    }
}

fn generate_impl_from_currency_numeric_code(
    features: &Features,
    currencies: &Currencies,
) -> TokenStream {
    if !features.contains("currency_numeric_code") {
        return quote! {};
    }
    let from_numeric_code_arms: TokenStream = currencies
        .iter()
        .map(|currency| {
            let variant = make_safe_ident(&currency.alphabetic_code);
            let numeric_code = &currency.numeric_code;

            quote! {
                #numeric_code => Ok(Self::#variant),
            }
        })
        .collect();

    quote! {
        impl TryFrom<u16> for Currency {
            type Error = ParseError;

            fn try_from(num: u16) -> Result<Self, Self::Error> {
                match num {
                    #from_numeric_code_arms
                    _ => Err(ParseError::Unknown(num.to_string())),
                }
            }
        }

    }
}

fn generate_impl_from_currency_str(features: &Features, currencies: &Currencies) -> TokenStream {
    if features.iter().filter(|f| f.contains("currency")).count() == 0 {
        return quote! {};
    }
    let from_str_arms: TokenStream = currencies
        .iter()
        .map(|currency| {
            let variant = make_safe_ident(&currency.alphabetic_code);
            let alphabetic_code = &currency.alphabetic_code;

            quote! {
                 #alphabetic_code => Ok(Self::#variant),
            }
        })
        .collect();

    quote! {
        // Parse from alpha-2 code
        impl std::str::FromStr for Currency {
            type Err = ParseError;

            fn from_str(s: &str) -> Result<Self, Self::Err> {
                match s {
                    #from_str_arms
                    _ => Err(ParseError::Unknown(s.to_string())),
                }
            }
        }
    }
}

fn filter_struct_fields<F>(input: ItemStruct, predicate: F) -> TokenStream
where
    F: Fn(&Field) -> bool,
{
    // Extract the structure parts
    let struct_name = &input.ident;
    let vis = &input.vis;
    let attrs = &input.attrs;

    // Get the named fields
    if let Fields::Named(FieldsNamed { named, .. }) = &input.fields {
        // Filter fields based on the predicate
        let filtered_fields = named
            .iter()
            .filter(|field| predicate(field))
            .collect::<Vec<_>>();

        // Create the new struct with filtered fields
        quote! {
            #(#attrs)*
            #vis struct #struct_name {
                #(#filtered_fields),*
            }
        }
    } else {
        // Return the original struct if it doesn't have named fields
        quote! { #input }
    }
}

fn generate_country(countries: &Countries) -> TokenStream {
    let variants: TokenStream = countries
        .iter()
        .map(|(_, h)| {
            h.iter()
                .map(|(_, country)| {
                    let country_name = country.iso_long_name.as_str();
                    let variant = make_safe_ident(&country.alpha2);

                    quote! {
                        #[doc = #country_name]
                        #variant,
                    }
                })
                .collect::<TokenStream>()
        })
        .collect();

    quote! {
        /// Represents a country according to the ISO 3166-1 standard.
        ///
        /// This enum contains variants for all countries defined in the ISO 3166-1 standard,
        /// which provides codes for countries and their subdivisions. Each variant corresponds
        /// to a single country.
        ///
        /// # Standards Support
        ///
        /// This enum is part of a comprehensive collection that supports:
        /// - ISO 3166-1: Country codes
        /// - ISO 3166-2: Subdivision codes (states, provinces, etc.)
        /// - ISO 4217: Currency codes
        /// - E.164: International phone number formats
        ///
        /// # Localization
        ///
        /// The crate provides translations for country names and subdivisions in multiple locales,
        /// though not all locales are supported for all countries.
        ///
        /// # Features
        ///
        /// - With the `serde` feature enabled, this type implements `serde::Serialize` and `serde::Deserialize`.
        /// - With the `iterator` feature enabled, this type implements `strum::EnumIter` for iterating over all currency variants.
        #[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
        #[cfg_attr(feature = "iterator", derive(strum::EnumIter))]
        #[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Debug)]
        pub enum Country {
            #variants
        }
    }
}

fn generate_currency(currencies: &Currencies) -> TokenStream {
    if currencies.is_empty() {
        return quote! {};
    }
    let variants: TokenStream = currencies
        .iter()
        .map(|currency| {
            let currency_name = currency.name.as_str();
            let variant = make_safe_ident(&currency.alphabetic_code);

            quote! {
                #[doc = #currency_name]
                #variant,
            }
        })
        .collect();

    quote! {
        /// Represents currencies according to the ISO 4217 standard.
        ///
        /// This enum provides a type-safe way to represent currencies with their associated metadata
        /// such as alphabetic codes, numeric codes, and minor units.
        ///
        /// # Features
        ///
        /// - With the `serde` feature enabled, this type implements `serde::Serialize` and `serde::Deserialize`.
        /// - With the `iterator` feature enabled, this type implements `strum::EnumIter` for iterating over all currency variants.
        #[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
        #[cfg_attr(feature = "iterator", derive(strum::EnumIter))]
        #[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Debug)]
        pub enum Currency {
            #variants
        }
    }
}

impl ToTokens for Geo {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let latitude = match self.latitude {
            Some(lat) => quote! { Some(#lat) },
            None => quote! { None },
        };

        let longitude = match self.longitude {
            Some(lng) => quote! { Some(#lng) },
            None => quote! { None },
        };

        let min_latitude = match self.min_latitude {
            Some(min_lat) => quote! { Some(#min_lat) },
            None => quote! { None },
        };

        let min_longitude = match self.min_longitude {
            Some(min_lng) => quote! { Some(#min_lng) },
            None => quote! { None },
        };

        let max_latitude = match self.max_latitude {
            Some(max_lat) => quote! { Some(#max_lat) },
            None => quote! { None },
        };

        let max_longitude = match self.max_longitude {
            Some(max_lng) => quote! { Some(#max_lng) },
            None => quote! { None },
        };

        let bounds = match &self.bounds {
            Some(b) => quote! { Some(#b) },
            None => quote! { None },
        };

        tokens.extend(quote! {
            Geo {
                latitude: #latitude,
                longitude: #longitude,
                min_latitude: #min_latitude,
                min_longitude: #min_longitude,
                max_latitude: #max_latitude,
                max_longitude: #max_longitude,
                bounds: #bounds,
            }
        });
    }
}

impl ToTokens for Bounds {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let northeast = &self.northeast;
        let southwest = &self.southwest;

        tokens.extend(quote! {
            Bounds {
                northeast: #northeast,
                southwest: #southwest,
            }
        });
    }
}

impl ToTokens for Coordinates {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let lat = self.lat;
        let lng = self.lng;

        tokens.extend(quote! {
            Coordinates {
                lat: #lat,
                lng: #lng,
            }
        });
    }
}

impl ToTokens for VatRates {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let standard = match self.standard {
            Some(std_rate) => quote! { Some(#std_rate) },
            None => quote! { None },
        };

        let reduced = match &self.reduced {
            Some(red_rates) => {
                let rates = red_rates.iter().collect::<Vec<_>>();
                quote! { Some(vec![#(#rates),*]) }
            }
            None => quote! { None },
        };

        let super_reduced = match self.super_reduced {
            Some(super_rate) => quote! { Some(#super_rate) },
            None => quote! { None },
        };

        let parking = match self.parking {
            Some(park_rate) => quote! { Some(#park_rate) },
            None => quote! { None },
        };

        tokens.extend(quote! {
            VatRates {
                standard: #standard,
                reduced: #reduced,
                super_reduced: #super_reduced,
                parking: #parking,
            }
        });
    }
}

fn get_subdivision_translation_tokens(
    features: &Features,
    translations: &HashMap<String, String>,
) -> TokenStream {
    let locales: TokenStream = features
        .iter()
        .filter_map(|f| f.strip_prefix("locale_"))
        .map(|l| {
            let field = make_safe_ident(l.to_lowercase());
            if let Some(value) = translations.get(l) {
                quote! { #field: Some(#value), }
            } else {
                quote! { #field: None, }
            }
        })
        .collect();

    if locales.is_empty() {
        return quote! {};
    }

    quote! {
        Translation {
            #locales
        }
    }
}

fn generate_method_subdivision(
    features: &Features,
    subdivisions: &Subdivisions,
    doc: &str,
) -> TokenStream {
    if features
        .iter()
        .filter(|f| f.contains("subdivision"))
        .count()
        == 0
    {
        return quote! {};
    }
    let (statics, arms): (Vec<TokenStream>, Vec<TokenStream>) = subdivisions
        .iter()
        .map(|(country, subdivisions)| {
            if !features.contains(country) {
                return (quote! {}, quote! {});
            }
            let subdivisions = subdivisions
                .iter()
                .map(|(_, subdivision)| {
                    let name = field_tokens!(features, subdivision, name);
                    let code = field_tokens!(features, subdivision, code);
                    let translations = field_tokens!(
                        features,
                        subdivision,
                        translations,
                        get_subdivision_translation_tokens
                    );
                    let country_type = field_tokens!(features, subdivision, r#type);

                    let unofficial_names =
                        field_tokens!(features, subdivision, Option<Vec<unofficial_names>>);

                    let geo = field_tokens!(features, subdivision, Option<geo>);

                    let comments = field_tokens!(features, subdivision, Option<comments>);

                    quote! {
                        &Subdivision {
                            #name
                            #code
                            #translations
                            #country_type
                            #unofficial_names
                            #geo
                            #comments
                        }
                    }
                })
                .collect::<Vec<_>>();

            let country_subdivision =
                make_safe_ident(format!("{}_SUBDIVISIONS", &country.to_uppercase()));
            let country = make_safe_ident(country.to_uppercase());
            let count = subdivisions.len();

            let statics = quote! {
                pub const #country_subdivision: [&Subdivision; #count] = [
                    #(#subdivisions),*
                ];

            };
            let arms = quote! {
                Self::#country => &#country_subdivision,
            };
            (statics, arms)
        })
        .unzip();

    quote! {
        #(#statics)*
        impl Country {
            #[doc = #doc]
            pub fn subdivision(&self) -> &'static [&'static Subdivision] {
                match self {
                    #(#arms)*
                    #[allow(unreachable_patterns)]
                    _=> unimplemented!(),
                }
            }
        }
    }
}

fn generate_method_translation(
    features: &Features,
    translations: &Translations,
    doc: &str,
) -> TokenStream {
    let locales: Vec<TokenStream> = translations
        .iter()
        .filter(|(country, _)| features.contains(&country.to_lowercase()))
        .map(|(country, locales)| {
            let some_values: Vec<TokenStream> = locales
                .iter()
                .map(|(locale, translation)| {
                    let field = make_safe_ident(locale);

                    if features.contains(&format!("locale_{locale}")) {
                        quote! { #field: Some(#translation), }
                    } else {
                        quote! {}
                    }
                })
                .collect();

            let none_values: Vec<TokenStream> = features
                .iter()
                .filter_map(|f| {
                    let locale = f.strip_prefix("locale_")?;
                    if locales.contains_key(locale) {
                        return None;
                    }

                    let field = make_safe_ident(locale);
                    Some(quote! { #field: None, })
                })
                .collect();

            if some_values.is_empty() && none_values.is_empty() {
                return quote! {};
            }

            let country = make_safe_ident(country);

            quote! {
                Self::#country => &Translation {
                    #(#some_values)*
                    #(#none_values)*
                }
            }
        })
        .collect();

    if locales.is_empty() {
        return quote! {};
    }

    quote! {
        impl Country {
            #[doc = #doc]
            pub fn translation(&self) -> &'static Translation {
                match self {
                    #(#locales),*
                }
            }
        }
    }
}

fn generate_translation(features: &Features) -> TokenStream {
    let locales: Vec<TokenStream> = features
        .iter()
        .filter_map(|f| f.strip_prefix("locale_"))
        .map(|locale| {
            let field = make_safe_ident(locale);
            quote! { pub #field: Option<&'static str> }
        })
        .collect();

    quote! {
        #[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
        #[derive(Clone, Copy, PartialEq, PartialOrd, Debug)]
        pub struct Translation {
            #(#locales),*
        }
    }
}

fn generate_subdivision(features: &Features) -> TokenStream {
    if features
        .iter()
        .filter(|f| f.contains("subdivision"))
        .count()
        == 0
    {
        return quote! {};
    }

    let input: ItemStruct = parse_quote! {
        /// Represents a country subdivision such as a state, province, or territory.
        ///
        /// This struct contains information about a political or administrative subdivision
        /// of a country, including its official name, code, and various metadata.
        #[derive(Clone, PartialEq, PartialOrd, Debug)]
        pub struct Subdivision {
            /// The official name of the subdivision.
            pub name: &'static str,

            /// The standardized code for the subdivision, typically following
            /// ISO 3166-2 or similar standards.
            pub code: &'static str,

            /// Translations of the subdivision name in various languages.
            pub translations: Translation,

            /// The type of subdivision (e.g., "state", "province", "territory", "prefecture").
            pub r#type: &'static str,

            /// Alternative or colloquial names for the subdivision.
            ///
            /// This field is optional and may be `None` if no unofficial names are available.
            pub unofficial_names: Option<&'static [&'static str]>,

            /// Geographical information about the subdivision, such as coordinates or boundaries.
            ///
            /// This field is optional and may be `None` if no geographical data is available.
            pub geo: Option<Geo>,

            /// Additional information or notes about the subdivision.
            ///
            /// This field is optional and may be `None` if no comments are available.
            pub comments: Option<&'static str>,
        }

    };

    filter_struct_fields(input, |field| {
        features.contains(&format!("subdivision_{}", field.ident.clone().unwrap()))
    })
}

fn generate_geo(features: &Features) -> TokenStream {
    if features.iter().filter(|f| f.contains("geo")).count() == 0 {
        return quote! {};
    }

    quote! {
        /// Represents geographic coordinates and boundaries.
        ///
        /// This struct can represent either a single geographic point (using `latitude`/`longitude`),
        /// a rectangular bounding box (using min/max values), or a more complex boundary (using `bounds`).
        ///
        #[derive(Clone, Copy, PartialEq, PartialOrd, Debug)]
        pub struct Geo {
            /// The latitude coordinate of a geographic point in decimal degrees.
            ///
            /// Positive values represent locations north of the equator,
            /// while negative values represent locations south of the equator.
            pub latitude: Option<f64>,

            /// The longitude coordinate of a geographic point in decimal degrees.
            ///
            /// Positive values represent locations east of the Prime Meridian,
            /// while negative values represent locations west of the Prime Meridian.
            pub longitude: Option<f64>,

            /// The minimum latitude of a bounding box in decimal degrees.
            ///
            /// Used together with `min_longitude`, `max_latitude`, and `max_longitude`
            /// to define a rectangular geographic region.
            pub min_latitude: Option<f64>,

            /// The minimum longitude of a bounding box in decimal degrees.
            ///
            /// Used together with `min_latitude`, `max_latitude`, and `max_longitude`
            /// to define a rectangular geographic region.
            pub min_longitude: Option<f64>,

            /// The maximum latitude of a bounding box in decimal degrees.
            ///
            /// Used together with `min_latitude`, `min_longitude`, and `max_longitude`
            /// to define a rectangular geographic region.
            pub max_latitude: Option<f64>,

            /// The maximum longitude of a bounding box in decimal degrees.
            ///
            /// Used together with `min_latitude`, `min_longitude`, and `max_latitude`
            /// to define a rectangular geographic region.
            pub max_longitude: Option<f64>,

            /// A complex boundary representation using northeast and southwest corner coordinates.
            ///
            /// This provides an alternative way to define a bounding box using the `Bounds` struct,
            /// which may be preferred for compatibility with certain APIs or data formats.
            pub bounds: Option<Bounds>,
        }

        /// Represents a rectangular geographic boundary defined by its northeast and southwest corners.
        ///
        /// This provides a structured way to represent a bounding box using two coordinate pairs,
        /// which is a common format in mapping and geocoding APIs.
        #[derive(Clone, Copy, PartialEq, PartialOrd, Debug)]
        pub struct Bounds {
            /// The northeast corner of the bounding box.
            pub northeast: Coordinates,

            /// The southwest corner of the bounding box.
            pub southwest: Coordinates,
        }

        /// Represents a single geographic coordinate with latitude and longitude values.
        ///
        /// This struct is used as a building block for more complex geographic structures
        /// like bounds and regions.
        #[derive(Clone, Copy, PartialEq, PartialOrd, Debug)]
        pub struct Coordinates {
            /// The latitude coordinate in decimal degrees.
            ///
            /// Positive values represent locations north of the equator,
            /// while negative values represent locations south of the equator.
            pub lat: f64,

            /// The longitude coordinate in decimal degrees.
            ///
            /// Positive values represent locations east of the Prime Meridian,
            /// while negative values represent locations west of the Prime Meridian.
            pub lng: f64,
        }
    }
}

fn generate_vat_rates(features: &Features) -> TokenStream {
    if features.iter().filter(|f| f.contains("vat_rates")).count() == 0 {
        return quote! {};
    }

    quote! {
        /// Represents the Value Added Tax (VAT) rates applicable in a tax jurisdiction.
        ///
        /// This structure contains different categories of VAT rates that may be applied to goods and services.
        /// All fields are optional to accommodate jurisdictions that may not use all rate types.
        #[derive(Clone, PartialEq, PartialOrd, Debug)]
        pub struct VatRates {
            /// The standard VAT rate as a percentage (e.g., 20.0 for 20%).
            ///
            /// This is the default rate applied to most goods and services in a jurisdiction.
            pub standard: Option<f64>,

            /// A collection of reduced VAT rates as percentages.
            ///
            /// Many jurisdictions offer multiple reduced rates for specific categories of goods or services
            /// (e.g., food, books, pharmaceuticals).
            pub reduced: Option<Vec<f64>>,

            /// The super-reduced VAT rate as a percentage.
            ///
            /// Some jurisdictions have an extra-low rate for essential goods or services.
            /// This is typically lower than the regular reduced rates.
            pub super_reduced: Option<f64>,

            /// The parking VAT rate as a percentage.
            ///
            /// This is a special rate used in some jurisdictions for specific categories
            /// of goods or services that don't fit into other rate categories.
            /// The term "parking" comes from EU VAT regulations.
            pub parking: Option<f64>,
        }
    }
}

fn generate(
    features: Features,
    countries: Countries,
    subdivisions: Subdivisions,
    country_translations: Translations,
    currencies: Currencies,
) -> TokenStream {
    let country_enum = generate_country(&countries);
    let geo_struct = generate_geo(&features);
    let vat_rates_struct = generate_vat_rates(&features);
    let subdivision_struct = generate_subdivision(&features);
    let translation_struct = generate_translation(&features);

    let alpha2 = generate_method!(
        alpha2,
        "ISO 3166-1 alpha-2 code",
        &'static str,
        features,
        countries
    );
    let alpha3 = generate_method!(
        alpha3,
        "ISO 3166-1 alpha-3 code",
        &'static str,
        features,
        countries
    );
    let alt_currency = generate_method!(
        alt_currency,
        "Alternative currency code(s) used in the country",
        Option<&'static str>,
        features,
        countries
    );
    let continent = generate_method!(
        continent,
        "Continent code the country is situated in",
        &'static str,
        features,
        countries
    );
    let country_code = generate_method!(
        country_code,
        "International calling code prefix for the country",
        &'static str,
        features,
        countries
    );
    let currency_code = generate_method!(
        currency
        currency_code,
        "ISO 4217 currency code",
        Currency,
        features,
        countries
    );
    let distance_unit = generate_method!(
        distance_unit,
        "Common unit of distance measurement used in the country",
        &'static str,
        features,
        countries
    );
    let gec = generate_method!(
        gec,
        "GEC (Geopolitical Entities and Codes) code",
        Option<&'static str>,
        features,
        countries
    );
    let geo = generate_method!(
        geo,
        "Geographic coordinates and boundaries information",
        Geo,
        features,
        countries
    );
    let international_prefix = generate_method!(
        international_prefix,
        "International prefix used to dial out of the country",
        Option<&'static str>,
        features,
        countries
    );
    let ioc = generate_method!(
        ioc,
        "International Olympic Committee code",
        Option<&'static str>,
        features,
        countries
    );
    let iso_long_name = generate_method!(
        iso_long_name,
        "ISO official long name for the country",
        &'static str,
        features,
        countries
    );
    let iso_short_name = generate_method!(
        iso_short_name,
        "ISO official short name for the country",
        &'static str,
        features,
        countries
    );
    let languages_official = generate_method!(
        languages_official,
        "List of officially recognized languages in the country",
        Option<Vec<&'static str>>,
        features,
        countries
    );
    let languages_spoken = generate_method!(
        languages_spoken,
        "List of languages spoken in the country",
        Option<Vec<&'static str>>,
        features,
        countries
    );
    let national_destination_code_lengths = generate_method!(
        national_destination_code_lengths,
        "Lengths of national destination codes used in the country",
        Option<Vec<i32>>,
        features,
        countries
    );
    let national_number_lengths = generate_method!(
        national_number_lengths,
        "Lengths of national telephone numbers in the country",
        Option<Vec<i32>>,
        features,
        countries
    );
    let national_prefix = generate_method!(
        national_prefix,
        "National prefix used for dialing within the country",
        Option<&'static str>,
        features,
        countries
    );
    let nationality = generate_method!(
        nationality,
        "Term used to refer to citizens or natives of the country",
        Option<&'static str>,
        features,
        countries
    );
    let numeric_code = generate_method!(
        numeric_code,
        "ISO 3166-1 numeric code",
        u16,
        features,
        countries
    );
    let postal_code = generate_method!(
        postal_code,
        "Whether the country uses postal codes",
        bool,
        features,
        countries
    );
    let postal_code_format = generate_method!(
        postal_code_format,
        "Regex format of the country's postal codes",
        Option<&'static str>,
        features,
        countries
    );
    let region = generate_method!(
        region,
        "Geographical region the country belongs to",
        Option<&'static str>,
        features,
        countries
    );
    let start_of_week = generate_method!(
        start_of_week,
        "First day of the work week in the country",
        &'static str,
        features,
        countries
    );
    let subregion = generate_method!(
        subregion,
        "Geographical subregion the country belongs to",
        Option<&'static str>,
        features,
        countries
    );
    let un_locode = generate_method!(
        un_locode,
        "ISO 3166-1 numeric code",
        &'static str,
        features,
        countries
    );
    let un_member = generate_method!(
        un_member,
        "Whether the country is a United Nations member",
        bool,
        features,
        countries
    );
    let unofficial_names = generate_method!(
        unofficial_names,
        "Alternative or colloquial names for the country",
        Vec<&'static str>,
        features,
        countries
    );
    let vehicle_registration_code = generate_method!(
        vehicle_registration_code,
        "International vehicle registration code",
        Option<&'static str>,
        features,
        countries
    );
    let world_region = generate_method!(
        world_region,
        "World region classification for the country",
        &'static str,
        features,
        countries
    );
    let nanp_prefix = generate_method!(
        nanp_prefix,
        "North American Numbering Plan prefix, if applicable",
        Option<&'static str>,
        features,
        countries
    );
    let address_format = generate_method!(
        address_format,
        "Format used for postal addresses in the country",
        Option<&'static str>,
        features,
        countries
    );
    let vat_rates = generate_method!(
        vat_rates,
        "Value Added Tax rates applied in the country",
        Option<VatRates>,
        features,
        countries
    );
    let eea_member = generate_method!(
        eea_member,
        "Whether the country is a European Economic Area member",
        Option<bool>,
        features,
        countries
    );
    let eu_member = generate_method!(
        eu_member,
        "Whether the country is a European Union member",
        Option<bool>,
        features,
        countries
    );
    let g20_member = generate_method!(
        g20_member,
        "Whether the country is a G20 member",
        Option<bool>,
        features,
        countries
    );
    let euvat_member = generate_method!(
        euvat_member,
        "Whether the country participates in the EU VAT system",
        Option<bool>,
        features,
        countries
    );
    let esm_member = generate_method!(
        esm_member,
        "Whether the country is a European Stability Mechanism member",
        Option<bool>,
        features,
        countries
    );
    let g7_member = generate_method!(
        g7_member,
        "Whether the country is a G7 member",
        Option<bool>,
        features,
        countries
    );
    let emoji_flag = generate_method!(trusted
        emoji_flag,
        "Whether the country is a G7 member",
        Option<&'static str>,
        features,
        countries
    );

    let subdivision = generate_method_subdivision(
        &features,
        &subdivisions,
        "Returns the subdivision (state/province/region) with the specified code for this country.",
    );
    let translations = generate_method_translation(
        &features,
        &country_translations,
        "Returns this country's localized information.",
    );

    let impl_from_str_alpha3 = generate_impl_from_str_alpha3(&features, &countries);
    let impl_from_str = generate_impl_from_str(&features, &countries);
    let parse_error = generate_parse_error(&features);
    let impl_from_numeric_code = generate_impl_from_numeric_code(&features, &countries);
    let impl_from_country = generate_impl_from_country(&features);

    let currency_enum = generate_currency(&currencies);
    let currency_name = generate_currency_method!(
        name,
        "Returns the full name of the currency.",
        &'static str,
        features,
        currencies
    );
    let currency_numeric_code = generate_currency_method!(
        numeric_code,
        "Returns the three-digit numeric code of the currency as defined in ISO 4217.",
        u16,
        features,
        currencies
    );
    let currency_minor_unit = generate_currency_method!(
        minor_unit,
        "Returns the number of digits after the decimal separator for the currency.\n\nFor example, USD has 2 minor units (cents), while JPY has 0 (no minor units).",
        u8,
        features,
        currencies
    );

    let impl_from_currency_numeric_code =
        generate_impl_from_currency_numeric_code(&features, &currencies);
    let impl_from_currency_str = generate_impl_from_currency_str(&features, &currencies);

    quote! {
        #country_enum
        #geo_struct
        #vat_rates_struct
        #subdivision_struct
        #translation_struct
        #alpha2
        #alpha3
        #alt_currency
        #continent
        #country_code
        #currency_code
        #distance_unit
        #gec
        #geo
        #international_prefix
        #ioc
        #iso_long_name
        #iso_short_name
        #languages_official
        #languages_spoken
        #national_destination_code_lengths
        #national_number_lengths
        #national_prefix
        #nationality
        #numeric_code
        #postal_code
        #postal_code_format
        #region
        #start_of_week
        #subregion
        #un_locode
        #un_member
        #unofficial_names
        #vehicle_registration_code
        #world_region
        #nanp_prefix
        #address_format
        #vat_rates
        #eea_member
        #eu_member
        #g20_member
        #euvat_member
        #esm_member
        #g7_member
        #emoji_flag
        #subdivision
        #translations

        #impl_from_str_alpha3
        #impl_from_str
        #parse_error
        #impl_from_numeric_code
        #impl_from_country

        #currency_enum
        #currency_name
        #currency_numeric_code
        #currency_minor_unit

        #impl_from_currency_numeric_code
        #impl_from_currency_str
    }
}

fn main() {
    let data_path_buf = PathBuf::from("countries")
        .join("lib")
        .join("countries")
        .join("data");
    let currency_data_path_buf = PathBuf::from("data").join("currencies.csv");

    let features = std::env::vars()
        .filter_map(|(key, _)| {
            if key.starts_with("CARGO_FEATURE_") {
                Some(key.strip_prefix("CARGO_FEATURE_").unwrap().to_lowercase())
            } else {
                None
            }
        })
        .collect::<Features>();

    let out_path = Path::new(&env::var("OUT_DIR").unwrap()).join("my_country.rs");

    let mut countries: Countries = load_yaml_files(data_path_buf.join("countries"), false).unwrap();

    countries.retain(|c, _| features.contains(c));

    // extending the countries data
    for (_, c) in countries.iter_mut() {
        for (_, c) in c.iter_mut() {
            c.emoji_flag = Some(country_code_to_emoji_flag(&c.alpha2));
        }
    }

    let mut subdivisions: Subdivisions =
        load_yaml_files(data_path_buf.join("subdivisions"), false).unwrap();

    subdivisions.retain(|c, _| features.contains(c));

    let mut country_translations: Translations =
        load_yaml_files_with_modifier(data_path_buf.join("translations"), false, |s| {
            s.strip_prefix("countries-").unwrap().to_string()
        })
        .unwrap();

    country_translations.retain(|l, c| {
        if features.contains(&format!("locale_{l}")) {
            c.retain(|c, _| features.contains(&c.to_lowercase()));
            return !c.is_empty();
        }
        false
    });

    // Changing map orientation from map<locale,map<country,translation>> to
    // map<country,map<locale,translation>>
    let country_translations: Translations = country_translations
        .into_iter()
        .flat_map(|(locale, countries)| {
            countries
                .into_iter()
                .map(move |(country, translation)| (country, locale.clone(), translation))
        })
        .fold(HashMap::new(), |mut acc, (country, locale, translation)| {
            acc.entry(country.clone())
                .or_default()
                .insert(locale.clone(), translation.clone());
            acc
        });

    let mut available_currencies: HashSet<String> = features
        .iter()
        .filter_map(|f| f.strip_prefix("currency_code_"))
        .map(|f| f.to_uppercase())
        .collect();

    let currencies: Currencies = if features.iter().filter(|f| f.contains("currency")).count() > 0 {
        let currencies: HashMap<String, Currency> = csv::Reader::from_path(currency_data_path_buf)
            .expect("Valid currency data path")
            .deserialize()
            .filter_map(|c| {
                let c: Currency = c.expect("Valid currency row in CSV file");

                if c.withdrawal_date.is_none()
                    && !c.alphabetic_code.is_empty()
                    && c.numeric_code != 0
                {
                    // extending the countries data
                    'outer: for (_, cy) in countries.iter_mut() {
                        for (_, cy) in cy.iter_mut() {
                            if cy.iso_short_name.to_uppercase() == c.entity {
                                cy.currency_code = c.alphabetic_code.clone();
                                available_currencies.insert(c.alphabetic_code.clone());
                                break 'outer;
                            }
                        }
                    }

                    if available_currencies.contains(&c.alphabetic_code) {
                        return Some((c.alphabetic_code.clone(), c));
                    }
                }
                None
            })
            .collect();
        currencies.into_values().collect()
    } else {
        Default::default()
    };

    if countries.is_empty() && currencies.is_empty() {
        panic!(
            "\n\x1b[31;1mNo code can be generated for my_country crate.\x1b[0m\nYou should enable at least one country or currency and their related features via features.\n"
        );
    }
    let content = generate(
        features,
        countries,
        subdivisions,
        country_translations,
        currencies,
    );

    // Parse the TokenStream into a syn::File
    let content = syn::parse2::<syn::File>(content.clone())
        .map_err(|err| render_location(&err, &content.to_string()))
        .expect("Failed to parse TokenStream into syn::File");

    let content = prettyplease::unparse(&content);
    // for line in content.lines() {
    //     println!("cargo:warning={}", line);
    // }
    let mut file = BufWriter::with_capacity(
        1024 * 1024,
        File::create(out_path).expect("Couldn't create the output file"),
    );
    write!(file, "{}", content).expect("Couldn't write to output file");
}

fn render_location(err: &syn::Error, code: &str) {
    use colored::Colorize;

    let start = err.span().start();
    let mut end = err.span().end();

    let code_line = match start.line.checked_sub(1).and_then(|n| code.lines().nth(n)) {
        Some(line) => line,
        None => return render_fallback(err),
    };

    if end.line > start.line {
        end.line = start.line;
        end.column = code_line.len();
    }

    panic!(
        "\n\
         {error}{header}\n\
         {indent}{arrow} {linenum}:{colnum}\n\
         {indent} {pipe}\n\
         {label} {pipe} {code}\n\
         {indent} {pipe} {offset}{underline} {message}\n\
         ",
        error = "error".red().bold(),
        header = ": Syn unable to parse file".bold(),
        indent = " ".repeat(start.line.to_string().len()),
        arrow = "-->".blue().bold(),
        linenum = start.line,
        colnum = start.column,
        pipe = "|".blue().bold(),
        label = start.line.to_string().blue().bold(),
        code = code_line.trim_end(),
        offset = " ".repeat(start.column),
        underline = "^"
            .repeat(end.column.saturating_sub(start.column).max(1))
            .red()
            .bold(),
        message = err.to_string().red(),
    );
}
fn render_fallback(err: &syn::Error) {
    panic!("Unable to parse file: {}", err);
}

fn country_code_to_emoji_flag(alpha2: &str) -> String {
    // Unicode flags work by combining regional indicator symbols
    // Each regional indicator symbol is 127397 (0x1F1E6) more than the ASCII code for 'A'
    alpha2
        .to_uppercase() // Convert to uppercase for consistency
        .chars()
        .map(|c| {
            // Convert each letter to the corresponding regional indicator symbol
            let scalar = (c as u32) + 127397;
            // Create a char from the scalar value
            char::from_u32(scalar).unwrap()
        })
        .collect()
}
