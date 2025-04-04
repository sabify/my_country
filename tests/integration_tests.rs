#![allow(unused_imports)]

// 1. Core Country Functionality Tests

#[cfg(test)]
#[cfg(feature = "us")]
mod country_core_tests {
    use my_country::*;
    use std::str::FromStr;

    #[test]
    fn test_country_creation() {
        let country = Country::US;
        assert_eq!(country, Country::US);
    }

    #[test]
    #[cfg(feature = "alpha2")]
    fn test_country_from_str() {
        assert_eq!(Country::from_str("US").unwrap(), Country::US);
        assert!(Country::from_str("INVALID").is_err());
    }

    #[test]
    #[cfg(feature = "alpha3")]
    fn test_country_from_alpha3() {
        assert_eq!(Country::try_from("USA").unwrap(), Country::US);
        assert!(Country::try_from("INVALID").is_err());
    }

    #[test]
    #[cfg(feature = "numeric_code")]
    fn test_country_from_numeric() {
        assert_eq!(Country::try_from(840u16).unwrap(), Country::US);
        assert!(Country::try_from(9999u16).is_err());
    }

    #[test]
    #[cfg(feature = "alpha2")]
    fn test_country_to_str() {
        let country_str: &str = Country::US.into();
        assert_eq!(country_str, "US");
    }

    #[test]
    #[cfg(feature = "alpha2")]
    fn test_country_to_string() {
        let country_string: String = Country::US.into();
        assert_eq!(country_string, "US");
    }

    #[test]
    #[cfg(feature = "alpha2")]
    fn test_parse_error() {
        let error = Country::from_str("INVALID").unwrap_err();
        assert_eq!(error.to_string(), "unknown code: INVALID");
    }
}

// 2. ISO Code Feature Tests

#[cfg(test)]
#[cfg(feature = "us")]
mod iso_code_tests {
    use my_country::*;

    #[cfg(feature = "alpha2")]
    #[test]
    fn test_alpha2() {
        assert_eq!(Country::US.alpha2(), "US");
    }

    #[cfg(feature = "alpha3")]
    #[test]
    fn test_alpha3() {
        assert_eq!(Country::US.alpha3(), "USA");
    }

    #[cfg(feature = "numeric_code")]
    #[test]
    fn test_numeric_code() {
        assert_eq!(Country::US.numeric_code(), 840u16);
    }

    #[cfg(feature = "un_locode")]
    #[test]
    fn test_un_locode() {
        assert_eq!(Country::US.un_locode(), "US");
    }

    #[cfg(feature = "gec")]
    #[test]
    fn test_gec() {
        assert_eq!(Country::US.gec(), Some("US"));
    }

    #[cfg(feature = "ioc")]
    #[test]
    fn test_ioc() {
        assert_eq!(Country::US.ioc(), Some("USA"));
    }

    #[cfg(feature = "vehicle_registration_code")]
    #[test]
    fn test_vehicle_registration_code() {
        assert_eq!(Country::US.vehicle_registration_code(), Some("USA"));
    }

    #[test]
    #[cfg(feature = "emoji_flag")]
    fn test_emoji_flag() {
        assert_eq!(Country::US.emoji_flag(), "🇺🇸");
    }
}

// 3. Country Name and Description Tests

#[cfg(test)]
#[cfg(feature = "us")]
mod country_name_tests {
    use my_country::*;

    #[cfg(feature = "iso_short_name")]
    #[test]
    fn test_iso_short_name() {
        assert_eq!(Country::US.iso_short_name(), "United States of America");
    }

    #[cfg(feature = "iso_long_name")]
    #[test]
    fn test_iso_long_name() {
        assert_eq!(Country::US.iso_long_name(), "The United States of America");
    }

    #[cfg(feature = "unofficial_names")]
    #[test]
    fn test_unofficial_names() {
        let names = Country::US.unofficial_names();
        assert!(names.contains(&"United States"));
        assert!(names.contains(&"USA"));
    }

    #[cfg(feature = "nationality")]
    #[test]
    fn test_nationality() {
        assert_eq!(Country::US.nationality(), Some("American"));
    }
}

// 4. Geographic Information Tests

#[cfg(test)]
#[cfg(feature = "us")]
mod geographic_tests {
    use my_country::*;

    #[cfg(feature = "continent")]
    #[test]
    fn test_continent() {
        assert_eq!(Country::US.continent(), "North America");
    }

    #[cfg(feature = "region")]
    #[test]
    fn test_region() {
        assert_eq!(Country::US.region(), Some("Americas"));
    }

    #[cfg(feature = "subregion")]
    #[test]
    fn test_subregion() {
        assert_eq!(Country::US.subregion(), Some("Northern America"));
    }

    #[cfg(feature = "world_region")]
    #[test]
    fn test_world_region() {
        assert_eq!(Country::US.world_region(), "AMER");
    }

    #[cfg(feature = "geo")]
    #[test]
    fn test_geo() {
        let geo = Country::US.geo();

        // Check latitude and longitude
        assert_eq!(geo.latitude, Some(37.09024));
        assert_eq!(geo.longitude, Some(-95.712891));

        // Check boundaries
        assert_eq!(geo.min_latitude, Some(18.91619));
        assert_eq!(geo.min_longitude, Some(-171.791110603));
        assert_eq!(geo.max_latitude, Some(71.3577635769));
        assert_eq!(geo.max_longitude, Some(-66.96466));

        // Check bounds struct
        let bounds = geo.bounds.unwrap();
        assert_eq!(bounds.northeast.lat, 71.3577635769);
        assert_eq!(bounds.northeast.lng, -66.96466);
        assert_eq!(bounds.southwest.lat, 18.91619);
        assert_eq!(bounds.southwest.lng, -171.791110603);
    }
}

// 5. Political and Economic Membership Tests

#[cfg(test)]
#[cfg(feature = "us")]
mod membership_tests {
    use my_country::*;

    #[cfg(feature = "un_member")]
    #[test]
    fn test_un_member() {
        assert_eq!(Country::US.un_member(), true);
    }

    #[cfg(feature = "eu_member")]
    #[test]
    fn test_eu_member() {
        assert_eq!(Country::US.eu_member(), None);
    }

    #[cfg(feature = "eea_member")]
    #[test]
    fn test_eea_member() {
        assert_eq!(Country::US.eea_member(), None);
    }

    #[cfg(feature = "euvat_member")]
    #[test]
    fn test_euvat_member() {
        assert_eq!(Country::US.euvat_member(), None);
    }

    #[cfg(feature = "g7_member")]
    #[test]
    fn test_g7_member() {
        assert_eq!(Country::US.g7_member(), Some(true));
    }

    #[cfg(feature = "g20_member")]
    #[test]
    fn test_g20_member() {
        assert_eq!(Country::US.g20_member(), Some(true));
    }

    #[cfg(feature = "esm_member")]
    #[test]
    fn test_esm_member() {
        assert_eq!(Country::US.esm_member(), None);
    }
}

// 6. Currency Feature Tests

#[cfg(test)]
#[cfg(any(
    feature = "currency_code_usd",
    all(feature = "us", feature = "currency_code")
))]
mod currency_tests {
    use my_country::*;
    use std::str::FromStr;

    #[cfg(feature = "currency_code")]
    #[test]
    fn test_currency_code() {
        assert_eq!(Country::US.currency_code(), Currency::USD);
    }

    #[cfg(feature = "alt_currency")]
    #[test]
    fn test_alt_currency() {
        assert_eq!(Country::US.alt_currency(), None);
    }

    #[test]
    fn test_country_to_currency() {
        let currency: Currency = Country::US.into();
        assert_eq!(currency, Currency::USD);

        // Test reference conversion
        let country_ref = &Country::US;
        let currency_from_ref: Currency = country_ref.into();
        assert_eq!(currency_from_ref, Currency::USD);
    }

    #[test]
    fn test_currency_creation() {
        let currency = Currency::USD;
        assert_eq!(currency, Currency::USD);
    }

    #[test]
    fn test_currency_from_str() {
        assert_eq!(Currency::from_str("USD").unwrap(), Currency::USD);
        assert!(Currency::from_str("INVALID").is_err());
    }

    #[test]
    fn test_currency_from_numeric() {
        assert_eq!(Currency::try_from(840u16).unwrap(), Currency::USD);
        assert!(Currency::try_from(9999u16).is_err());
    }

    #[cfg(feature = "currency_name")]
    #[test]
    fn test_currency_name() {
        assert_eq!(Currency::USD.name(), "US Dollar");
    }

    #[cfg(feature = "currency_numeric_code")]
    #[test]
    fn test_currency_numeric_code() {
        assert_eq!(Currency::USD.numeric_code(), 840u16);
    }

    #[cfg(feature = "currency_minor_unit")]
    #[test]
    fn test_currency_minor_unit() {
        assert_eq!(Currency::USD.minor_unit(), 2u8);
    }
}

// 7. Telephone and Address Format Tests

#[cfg(test)]
#[cfg(feature = "us")]
mod telecom_address_tests {
    use my_country::*;

    #[cfg(feature = "country_code")]
    #[test]
    fn test_country_code() {
        assert_eq!(Country::US.country_code(), "1");
    }

    #[cfg(feature = "international_prefix")]
    #[test]
    fn test_international_prefix() {
        assert_eq!(Country::US.international_prefix(), Some("011"));
    }

    #[cfg(feature = "national_prefix")]
    #[test]
    fn test_national_prefix() {
        assert_eq!(Country::US.national_prefix(), Some("1"));
    }

    #[cfg(feature = "national_destination_code_lengths")]
    #[test]
    fn test_national_destination_code_lengths() {
        let lengths = Country::US.national_destination_code_lengths().unwrap();
        assert_eq!(lengths, &[3]);
    }

    #[cfg(feature = "national_number_lengths")]
    #[test]
    fn test_national_number_lengths() {
        let lengths = Country::US.national_number_lengths().unwrap();
        assert_eq!(lengths, &[10]);
    }

    #[cfg(feature = "nanp_prefix")]
    #[test]
    fn test_nanp_prefix() {
        assert_eq!(Country::US.nanp_prefix(), None);
    }

    #[cfg(feature = "postal_code")]
    #[test]
    fn test_postal_code() {
        assert_eq!(Country::US.postal_code(), true);
    }

    #[cfg(feature = "postal_code_format")]
    #[test]
    fn test_postal_code_format() {
        assert_eq!(
            Country::US.postal_code_format(),
            Some("(\\d{5})(?:[ \\-](\\d{4}))?")
        );
    }

    #[cfg(feature = "address_format")]
    #[test]
    fn test_address_format() {
        let format = Country::US.address_format().unwrap();
        assert!(format.contains("{{recipient}}"));
        assert!(format.contains("{{street}}"));
        assert!(format.contains("{{city}}"));
        assert!(format.contains("{{region_short}}"));
        assert!(format.contains("{{postalcode}}"));
        assert!(format.contains("{{country}}"));
    }
}

// 8. Language and Culture Tests

#[cfg(test)]
#[cfg(feature = "us")]
mod language_culture_tests {
    use my_country::*;

    #[cfg(feature = "languages_official")]
    #[test]
    fn test_languages_official() {
        let languages = Country::US.languages_official().unwrap();
        assert_eq!(languages, &["en"]);
    }

    #[cfg(feature = "languages_spoken")]
    #[test]
    fn test_languages_spoken() {
        let languages = Country::US.languages_spoken().unwrap();
        assert_eq!(languages, &["en"]);
    }

    #[cfg(feature = "start_of_week")]
    #[test]
    fn test_start_of_week() {
        assert_eq!(Country::US.start_of_week(), "sunday");
    }

    #[cfg(feature = "distance_unit")]
    #[test]
    fn test_distance_unit() {
        assert_eq!(Country::US.distance_unit(), "MI");
    }
}

// 9. VAT and Tax Tests

#[cfg(test)]
#[cfg(feature = "us")]
mod vat_tests {
    use my_country::*;

    #[cfg(feature = "vat_rates")]
    #[test]
    fn test_vat_rates() {
        assert_eq!(Country::US.vat_rates(), None);

        // For a country with VAT rates (e.g., UK), would test:
        // let vat = Country::GB.vat_rates().unwrap();
        // assert_eq!(vat.standard, Some(20.0));
        // assert_eq!(vat.reduced.as_ref().unwrap().contains(&5.0), true);
    }
}

// 10. Subdivision Tests

#[cfg(test)]
#[cfg(feature = "us")]
mod subdivision_tests {
    use my_country::*;

    #[cfg(any(
        feature = "subdivision_name",
        feature = "subdivision_code",
        feature = "subdivision_translations",
        feature = "subdivision_unofficial_names",
        feature = "subdivision_geo",
        feature = "subdivision_comments"
    ))]
    fn get_ca_subdivision() -> Subdivision {
        (**my_country::US_SUBDIVISIONS
            .iter()
            .filter(|s| s.code == "CA")
            .next()
            .unwrap())
        .clone()
    }

    #[cfg(feature = "subdivision_name")]
    #[test]
    fn test_subdivision_name() {
        // Access US subdivisions (assuming US_SUBDIVISIONS is accessible)
        // This is more of an integration test that would depend on the actual implementation
        let vermont_subdivision = get_ca_subdivision();
        assert_eq!(vermont_subdivision.name, "California");
    }

    #[cfg(feature = "subdivision_code")]
    #[test]
    fn test_subdivision_code() {
        let vermont_subdivision = get_ca_subdivision();
        assert_eq!(vermont_subdivision.code, "CA");
    }

    #[cfg(feature = "subdivision_translations")]
    #[cfg(feature = "locale_en")]
    #[test]
    fn test_subdivision_translations() {
        let vermont_subdivision = get_ca_subdivision();
        assert_eq!(vermont_subdivision.translations.en, Some("California"));
    }

    #[cfg(feature = "subdivision_unofficial_names")]
    #[test]
    fn test_subdivision_unofficial_names() {
        let vermont_subdivision = get_ca_subdivision();
        let names = vermont_subdivision.unofficial_names.unwrap();
        assert!(names.contains(&"California"));
    }

    #[cfg(feature = "subdivision_geo")]
    #[test]
    fn test_subdivision_geo() {
        let vermont_subdivision = get_ca_subdivision();
        let geo = vermont_subdivision.geo.unwrap();

        assert_eq!(geo.latitude, Some(36.778261));
        assert_eq!(geo.longitude, Some(-119.4179324));
    }

    #[cfg(feature = "subdivision_comments")]
    #[test]
    fn test_subdivision_comments() {
        let vermont_subdivision = get_ca_subdivision();
        assert_eq!(vermont_subdivision.comments, None);
    }
}

// 11. Serde Feature Tests

#[cfg(test)]
#[cfg(all(feature = "serde", feature = "us"))]
mod serde_tests {
    use my_country::*;
    use serde_json;

    #[test]
    fn test_country_serialization() {
        let country = Country::US;
        let serialized = serde_json::to_string(&country).unwrap();
        assert_eq!(serialized, "\"US\"");

        let deserialized: Country = serde_json::from_str(&serialized).unwrap();
        assert_eq!(deserialized, Country::US);
    }

    #[test]
    fn test_currency_serialization() {
        let currency = Currency::USD;
        let serialized = serde_json::to_string(&currency).unwrap();
        assert_eq!(serialized, "\"USD\"");

        let deserialized: Currency = serde_json::from_str(&serialized).unwrap();
        assert_eq!(deserialized, Currency::USD);
    }
}

// 12. Iterator Feature Tests

#[cfg(test)]
#[cfg(all(feature = "iterator", feature = "us"))]
mod iterator_tests {
    use my_country::*;
    use strum::IntoEnumIterator;

    #[test]
    fn test_country_iteration() {
        let countries: Vec<Country> = Country::iter().collect();
        assert!(countries.contains(&Country::US));

        // Count of countries should match the total number of country variants
        // (this would be a more extensive test in the actual crate)
        assert!(countries.len() > 0);
    }

    #[test]
    fn test_currency_iteration() {
        let currencies: Vec<Currency> = Currency::iter().collect();
        assert!(currencies.contains(&Currency::USD));

        // Count of currencies should match the total number of currency variants
        assert!(currencies.len() > 0);
    }
}

// 13. Specific Currency Code Tests

#[cfg(test)]
#[cfg(all(feature = "currency_code_usd", feature = "currency_name"))]
mod specific_currency_tests {
    // These tests would verify that specific currencies are properly supported
    // when their features are enabled

    #[test]
    fn test_usd_support() {
        use my_country::Currency;
        let usd = Currency::USD;
        assert_eq!(usd.name(), "US Dollar");
    }
}

// 14. Locale Feature Tests

#[cfg(test)]
mod locale_tests {
    // These tests would verify that specific locales are properly supported
    // when their features are enabled

    #[cfg(feature = "locale_en")]
    #[test]
    fn test_en_locale() {
        // Test English locale functionality
        // Implementation would depend on how locales are used in the actual crate
    }

    // More specific locale tests would follow the same pattern
}

// 15. Edge Case and Validation Tests

#[cfg(test)]
mod edge_case_tests {
    use my_country::*;
    use std::str::FromStr;

    #[test]
    #[cfg(feature = "alpha2")]
    fn test_invalid_country_code() {
        let result = Country::from_str("XX");
        assert!(matches!(result, Err(ParseError::Unknown(_))));
    }

    #[test]
    #[cfg(any(
        all(feature = "us", feature = "currency_code"),
        feature = "currency_code_usd"
    ))]
    fn test_invalid_currency_code() {
        let result = Currency::from_str("YYY");
        assert!(matches!(result, Err(ParseError::Unknown(_))));
    }

    #[test]
    #[cfg(feature = "numeric_code")]
    fn test_invalid_numeric_code() {
        let result = Country::try_from(65535u16);
        assert!(matches!(result, Err(ParseError::Unknown(_))));
    }

    #[test]
    #[cfg(feature = "alpha2")]
    fn test_error_display() {
        let error = ParseError::Unknown("TEST".to_string());
        assert_eq!(error.to_string(), "unknown code: TEST");
    }
}

// 16. Struct Comparison Tests

#[cfg(test)]
#[cfg(any(feature = "geo", feature = "vat_rates"))]
mod struct_comparison_tests {
    use my_country::*;

    #[test]
    #[cfg(feature = "geo")]
    fn test_coordinates_equality() {
        let coord1 = Coordinates {
            lat: 41.0,
            lng: -74.0,
        };
        let coord2 = Coordinates {
            lat: 41.0,
            lng: -74.0,
        };
        let coord3 = Coordinates {
            lat: 42.0,
            lng: -74.0,
        };

        assert_eq!(coord1, coord2);
        assert_ne!(coord1, coord3);
    }

    #[test]
    #[cfg(feature = "geo")]
    fn test_bounds_equality() {
        let bounds1 = Bounds {
            northeast: Coordinates {
                lat: 41.0,
                lng: -74.0,
            },
            southwest: Coordinates {
                lat: 40.0,
                lng: -75.0,
            },
        };

        let bounds2 = Bounds {
            northeast: Coordinates {
                lat: 41.0,
                lng: -74.0,
            },
            southwest: Coordinates {
                lat: 40.0,
                lng: -75.0,
            },
        };

        let bounds3 = Bounds {
            northeast: Coordinates {
                lat: 42.0,
                lng: -74.0,
            },
            southwest: Coordinates {
                lat: 40.0,
                lng: -75.0,
            },
        };

        assert_eq!(bounds1, bounds2);
        assert_ne!(bounds1, bounds3);
    }

    #[test]
    #[cfg(feature = "geo")]
    fn test_geo_equality() {
        let geo1 = Geo {
            latitude: Some(41.0),
            longitude: Some(-74.0),
            min_latitude: None,
            min_longitude: None,
            max_latitude: None,
            max_longitude: None,
            bounds: None,
        };

        let geo2 = Geo {
            latitude: Some(41.0),
            longitude: Some(-74.0),
            min_latitude: None,
            min_longitude: None,
            max_latitude: None,
            max_longitude: None,
            bounds: None,
        };

        let geo3 = Geo {
            latitude: Some(42.0),
            longitude: Some(-74.0),
            min_latitude: None,
            min_longitude: None,
            max_latitude: None,
            max_longitude: None,
            bounds: None,
        };

        assert_eq!(geo1, geo2);
        assert_ne!(geo1, geo3);
    }

    #[test]
    #[cfg(feature = "vat_rates")]
    fn test_vat_rates_equality() {
        let vat1 = VatRates {
            standard: Some(20.0),
            reduced: Some(vec![5.0, 12.0]),
            super_reduced: None,
            parking: None,
        };

        let vat2 = VatRates {
            standard: Some(20.0),
            reduced: Some(vec![5.0, 12.0]),
            super_reduced: None,
            parking: None,
        };

        let vat3 = VatRates {
            standard: Some(21.0),
            reduced: Some(vec![5.0, 12.0]),
            super_reduced: None,
            parking: None,
        };

        assert_eq!(vat1, vat2);
        assert_ne!(vat1, vat3);
    }
}
