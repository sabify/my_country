// benches/bench_main.rs
use criterion::{criterion_group, criterion_main};

mod parsing;

criterion_group!(benches, parsing::benchmark_parsing,);
criterion_main!(benches);
