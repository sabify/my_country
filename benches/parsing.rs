use criterion::Criterion;
use my_country::{Country, Currency};
use std::{hint::black_box, str::FromStr};

pub fn benchmark_parsing(c: &mut Criterion) {
    let mut group = c.benchmark_group("Parsing");

    // Country parsing benchmarks
    group.bench_function("country_from_alpha2", |b| {
        b.iter(|| black_box(Country::from_str("US")));
    });

    group.bench_function("country_from_alpha3", |b| {
        b.iter(|| black_box(Country::try_from("USA")));
    });

    group.bench_function("country_from_numeric", |b| {
        b.iter(|| black_box(Country::try_from(840u16)));
    });

    // Currency parsing benchmarks
    group.bench_function("currency_from_str", |b| {
        b.iter(|| black_box(Currency::from_str("USD")));
    });

    group.bench_function("currency_from_numeric", |b| {
        b.iter(|| black_box(Currency::try_from(840u16)));
    });

    // Conversions
    group.bench_function("country_to_str", |b| {
        let country = Country::US;
        b.iter(|| {
            let code: &str = black_box(country).into();
            black_box(code)
        });
    });

    group.bench_function("country_to_string", |b| {
        let country = Country::US;
        b.iter(|| {
            let code: String = black_box(country).into();
            black_box(code)
        });
    });

    group.finish();
}
