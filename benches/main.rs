// benches/bench_main.rs
use criterion::{Criterion, criterion_group, criterion_main};

mod parsing;

criterion_group!(benches, parsing::benchmark_parsing,);
criterion_main!(benches);
