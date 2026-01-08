# my_country

[![crates.io](https://img.shields.io/crates/v/my_country.svg)](https://crates.io/crates/my_country)
[![Documentation](https://docs.rs/my_country/badge.svg)](https://docs.rs/my_country)
[![License: MIT OR Apache-2.0](https://img.shields.io/crates/l/my_country.svg)](#license)
[![Rust: 2024 Edition](https://img.shields.io/badge/Rust-2024_Edition-orange.svg)](#rust-version)

Comprehensive information about every country, with support for ISO standards:

- ISO 3166-1 (country codes)
- ISO 3166-2 (country subdivisions)
- ISO 4217 (currency codes)
- E.164 (international phone numbers)

## Features

`my_country` provides a unique feature-based code generation approach that allows you to:

- Only include the data you need for code generation
- Dramatically reduce compile times, binary size and LSP (rust-analyzer) wait times

> ⭐ **Found this useful?** Give it a star ⭐ to show your support and help others discover it!

## Table of Contents

- [Installation](#installation)
- [Feature-Based Optimization](#feature-based-optimization)
- [Usage Examples](#usage-examples)
- [Country Data](#country-data)
- [Currency Data](#currency-data)
- [Subdivision Data](#subdivision-data)
- [Localization](#localization)
- [Serialization](#serialization)
- [Available Features](#available-features)
- [Compile Time Optimization](#compile-time-optimization)
- [Contributing](#contributing)
- [License](#license)

## Installation

Add `my_country` to your `Cargo.toml` with the specific features you need:

```toml
[dependencies]
my_country = { version = "0.1.14", default-features = false, features = ["us", "alpha2", "iso_short_name", "currency_code"] }
```

## Feature-Based Optimization

Unlike traditional crates that provide all country data at once, `my_country` uses Rust's feature flags system to generate only the code you need. This approach offers several benefits:

### 1. Compiler Performance

By only generating code for the country properties you actually use, `my_country` can significantly improve compile times. For large projects, this can be the difference between waiting seconds versus minutes for compilation.

### 2. Binary Size Optimization

When building for deployment, especially on embedded systems or WebAssembly, every byte counts. The feature-based design ensures your final binary only includes the exact country data needed.

### 3. Customizable Data Selection

Need only currency information for European countries? Or just postal codes for North America? Simply enable the specific features to include only what your application requires.

## Usage Examples

### Basic Example

```rust
use my_country::{Country, ParseError};
use std::str::FromStr;

let country = Country::from_str("US").unwrap();
println!("Country name: {}", country.iso_short_name());
println!("Currency: {}", country.currency_code().name());

// Convert between different representations
let alpha3 = country.alpha3();
println!("Alpha-3 code: {}", alpha3);

// Code generation with feature flags
// my_country = { version = "0.1.14", default-features = false, features = ["us", "name", "currency_code", "alpha3"] }
```

### Checking Country Properties

```rust
use my_country::Country;

let us = Country::US;

// Geographic information
println!("US region: {}", us.region().unwrap_or("Unknown"));
println!("US continent: {}", us.continent());

// Membership in international organizations
if let Some(true) = us.g7_member() {
    println!("The US is a G7 member");
}

// Working with phone numbers
println!("Country code for the US: +{}", us.country_code());
println!("Phone number lengths in the US: {:?}", us.national_number_lengths());

// Code generation with feature flags
// my_country = { version = "0.1.14", default-features = false, features = ["us", "region", "g7_member", "country_code", "national_number_lengths"] }
```

### Working with Currencies

```rust
use my_country::{Country, Currency};

let usd = Currency::USD;

// Currency metadata
println!("Currency name: {}", usd.name());
println!("Numeric code: {}", usd.numeric_code());
println!("Minor units: {}", usd.minor_unit());

// Countries using a specific currency
let us = Country::US;
assert_eq!(us.currency_code(), Currency::USD);

// Convert country to its currency
let currency: Currency = us.into();
println!("Currency for US: {}", currency.name());

// Code generation with feature flags
// my_country = { version = "0.1.14", default-features = false, features = ["us", "currency_name", "currency_numeric_code", "currency_minor_unit", "currency_code"] }
```

### Handling Subdivisions

```rust
use my_country::Country;

let us = Country::US;

// Access all subdivisions
for subdivision in us.subdivision() {
    println!("State: {} ({})", subdivision.name, subdivision.code);

    // Access translations if available
    if let Some(name) = subdivision.translations.en {
        println!("  English name: {}", name);
    }

    // Geographic data for subdivisions
    if let Some(geo) = subdivision.geo {
        if let Some(lat) = geo.latitude {
            if let Some(lng) = geo.longitude {
                println!("  Coordinates: {}, {}", lat, lng);
            }
        }
    }
}

// Code generation with feature flags
// my_country = { version = "0.1.14", default-features = false, features = ["us", "subdivision_name", "locale_en", "subdivision_geo"] }
```

## Country Data

The `Country` enum provides comprehensive information about each country:

```rust
use my_country::Country;

// Get basic ISO information
let country = Country::US;
println!("Alpha-2 code: {}", country.alpha2());
println!("Alpha-3 code: {}", country.alpha3());
println!("Numeric code: {}", country.numeric_code());

// Get localized names
println!("Short name: {}", country.iso_short_name());
println!("Long name: {}", country.iso_long_name());
println!("Unofficial names: {:?}", country.unofficial_names());

// Get geographical information
println!("Continent: {}", country.continent());
println!("Region: {:?}", country.region());
println!("Subregion: {:?}", country.subregion());

// Detailed geographic data
let geo = country.geo();
println!("Latitude: {:?}", geo.latitude);
println!("Longitude: {:?}", geo.longitude);

// Get telephone dialing information
println!("Country code: +{}", country.country_code());
println!("International prefix: {:?}", country.international_prefix());

// Get postal information
if country.postal_code() {
    println!("Postal code format: {:?}", country.postal_code_format());
}

// Get emoji flag, 🇺🇸
println!("Flag: {}", country.emoji_flag());

// Code generation with feature flags
// my_country = { version = "0.1.14", default-features = false, features = ["us", "alpha2", "alpha3", "numeric_code", "iso_short_name", "iso_long_name", "continent", "region", "subregion", "geo", "country_code", "international_prefix", "postal_code_format", "emoji_flag"] }
```

## Currency Data

The `Currency` enum provides details for all ISO 4217 currencies:

```rust
use my_country::Currency;

let currency = Currency::USD;

// Basic currency information
println!("Currency name: {}", currency.name());
println!("Numeric code: {}", currency.numeric_code());
println!("Minor units: {}", currency.minor_unit());

// Parse from string representation
let parsed_currency = "USD".parse::<Currency>().unwrap();
assert_eq!(parsed_currency, Currency::USD);

// Parse from numeric code
let numeric_currency = Currency::try_from(840u16).unwrap();
assert_eq!(numeric_currency, Currency::USD);

// Code generation with feature flags
// my_country = { version = "0.1.14", default-features = false, features = ["currency_code_usd", "currency_numeric_code", "currency_minor_unit"] }
```

## Subdivision Data

Each country provides access to its subdivisions (states, provinces, regions):

```rust
use my_country::Country;

let country = Country::US;
let subdivisions = country.subdivision();

for subdivision in subdivisions {
    println!("Name: {}", subdivision.name);
    println!("Code: {}", subdivision.code);

    // Access localized names
    if let Some(fa_name) = subdivision.translations.fa {
        println!("Persian name: {}", fa_name);
    }

    // Access geographical data
    if let Some(geo) = subdivision.geo {
        println!("Coordinates: {:?}, {:?}", geo.latitude, geo.longitude);
    }

    // Additional information
    if let Some(comments) = subdivision.comments {
        println!("Comments: {}", comments);
    }
}

// Code generation with feature flags
// my_country = { version = "0.1.14", default-features = false, features = ["us", subdivision_name", "subdivision_code", "subdivision_geo", "subdivision_comments", "locale_fa"] }
```

## Localization

The crate supports localized country names through the translation system:

```rust
// features = en,locale_en,locale_fa
use my_country::Country;

let country = Country::US;
let translation = country.translation();

println!("English name: {:?}", translation.en);
println!("Persian name: {:?}", translation.fa);

// Enable additional locales with feature flags
// my_country = { version = "0.1.14", default-features = false, features = ["locale_fa", "locale_es"] }
```

## Serialization

With the `serde` feature enabled, all types can be serialized and deserialized:

```rust
use my_country::Country;
use serde_json;

let country = Country::US;

// Serialize to JSON
let json = serde_json::to_string(&country).unwrap();
println!("JSON: {}", json);

// Deserialize from JSON
let deserialized: Country = serde_json::from_str(&json).unwrap();
assert_eq!(country, deserialized);
```

## Available Features

### Core Features

- `serde`: Adds serialization/deserialization support via serde
- `iterator`: Adds iteration capabilities via strum

### Country Method Features

- `alpha2`: ISO 3166-1 alpha-2 code (2-letter country code)
- `alpha3`: ISO 3166-1 alpha-3 code (3-letter country code)
- `numeric_code`: ISO 3166-1 numeric code
- `iso_short_name`: ISO official short name
- `iso_long_name`: ISO official long name
- `unofficial_names`: Alternative or colloquial names
- `continent`: Continent the country is on
- `region`: Geographic region
- `subregion`: Geographic subregion
- `world_region`: Classification by world region
- `currency_code`: ISO 4217 currency code
- `alt_currency`: Alternative currencies used
- `country_code`: International calling code
- `international_prefix`: Prefix to dial out
- `national_prefix`: Prefix for domestic calls
- `national_number_lengths`: Length of phone numbers
- `national_destination_code_lengths`: Length of area/city codes
- `postal_code`: Whether country uses postal codes
- `postal_code_format`: Format of postal codes
- `geo`: Geographic coordinates and boundaries
- `languages_official`: Official languages
- `languages_spoken`: Languages in common use
- `nationality`: Term for citizens/natives
- `address_format`: Format used for postal addresses
- `distance_unit`: Common unit of measurement
- `start_of_week`: First day of the work week
- `emoji_flag`: Emoji flag representation of the country

### Organization Membership Features

- `un_member`: United Nations membership
- `eu_member`: European Union membership
- `eea_member`: European Economic Area membership
- `euvat_member`: EU VAT system participation
- `g7_member`: G7 membership
- `g20_member`: G20 membership
- `esm_member`: European Stability Mechanism membership

### Subdivision Features

- `subdivision_name`: Names of states/provinces
- `subdivision_code`: Codes for states/provinces
- `subdivision_translations`: Localized names for subdivisions
- `subdivision_type`: Type of subdivision (state, province, etc.)
- `subdivision_geo`: Geographic data for subdivisions
- `subdivision_comments`: Additional notes about subdivisions
- `subdivision_unofficial_names`: Alternative names for subdivisions

### Currency Features

- `currency`: Enables all currency-related features
- `currency_name`: Name of the currency
- `currency_numeric_code`: ISO 4217 numeric code
- `currency_minor_unit`: Number of decimal places

### Currency Code Features

- `currency_code_gbp`: Pound Sterling (additional to other currencies imported by enabled countries)
- ... (and many more)

### Locale Features

Enable translations for specific languages:

- `locale_af`: Afrikaans
- `locale_am`: Amharic
- `locale_ar`: Arabic
- `locale_fa`: Persian/Farsi
- `locale_es`: Spanish
- `locale_en`: English
- `locale_it`: Italy
- `locale_de`: German
- `locale_fr`: French
- `locale_pt_br`: Portuguese in Brazil
- ... (and many more)

### Country-Specific Features

Enable only specific countries to minimize compile time and binary size:

- `us`: United States
- `au`: Australia
- `fr`: France
- `de`: Germany
- `it`: Italy
- `gb`: Great Britain
- `es`: Spain
- `br`: Brazil
- ... (and all other countries by ISO alpha-2 code)

## Compile Time Optimization

The feature-based approach significantly improves compile times by only generating the code you need:

### Selecting Only Methods You Need

Instead of including all country information, selectively enable just what you use:

```toml
[dependencies]
my_country = { version = "0.1.14", default-features = false, features = ["us", "alpha2", "currency_code", "iso_short_name"] }
```

This creates only the necessary methods, reducing the amount of code the Rust compiler needs to process.

### Enabling Specific Countries

For applications targeting specific regions, you can enable only relevant countries:

```toml
[dependencies]
my_country = { version = "0.1.14", default-features = false, features = ["us", "ca", "mx"] }
```

This approach is particularly effective for:

1. **Embedded systems**: Minimize binary size for constrained environments
2. **WebAssembly**: Reduce download size for web applications
3. **Development workflow**: Speed up compile-test cycles during development
4. **Large applications**: Reduce incremental compilation times in complex projects

## Contributing

For countries data updates, please open a pull request in the `countries` repository at <https://github.com/countries/countries>. Otherwise, feel free to open an issue or open a pull request here.

## License

Licensed under either of:

- Apache License, Version 2.0 ([LICENSE-APACHE](LICENSE-APACHE) or <http://www.apache.org/licenses/LICENSE-2.0>)
- MIT license ([LICENSE-MIT](LICENSE-MIT) or <http://opensource.org/licenses/MIT>)

at your option.

Unless you explicitly state otherwise, any contribution intentionally submitted for inclusion in this crate by you shall be dual licensed as above, without any additional terms or conditions.

Countries data license: <https://github.com/countries/countries/blob/master/LICENSE>

Currencies data license: <http://opendatacommons.org/licenses/pddl/>
