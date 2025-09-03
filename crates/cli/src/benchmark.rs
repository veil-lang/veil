use anyhow::{Result, anyhow};
use std::path::PathBuf;
use std::time::{Duration, Instant};

fn default_target_triple() -> String {
    let arch = std::env::consts::ARCH;
    let os = std::env::consts::OS;
    match os {
        "windows" => "x86_64-pc-windows-msvc".to_string(),
        "macos" => match arch {
            "aarch64" => "aarch64-apple-darwin".to_string(),
            _ => "x86_64-apple-darwin".to_string(),
        },
        "linux" => match arch {
            "aarch64" => "aarch64-unknown-linux-gnu".to_string(),
            _ => "x86_64-unknown-linux-gnu".to_string(),
        },
        _ => "x86_64-unknown-linux-gnu".to_string(),
    }
}

pub fn run_benchmark(input: PathBuf, iterations: usize, verbose: bool) -> Result<()> {
    let build_dir = input
        .parent()
        .ok_or_else(|| anyhow!("Invalid input file path"))?
        .join("build");
    std::fs::create_dir_all(&build_dir)?;

    let mut times = Vec::with_capacity(iterations);

    for i in 1..=iterations {
        if verbose {
            println!("Iteration {}/{}...", i, iterations);
        }
        let start = Instant::now();

        // Use the extracted CLI pipeline end-to-end for a realistic run.
        // We mark is_test=true so we don't execute the produced binary.
        let output = build_dir.join(format!("benchmark_run_{}.exe", i));
        let _built = crate::process_build(
            input.clone(),
            output,
            /* optimize */ false,
            /* target_triple */ default_target_triple(),
            /* verbose */ verbose,
            /* dump_norm_hir */ false,
            /* pass_timings */ false,
            /* cache_stats */ false,
            /* is_test */ true,
            /* skip_cc */ false,
        )?;

        times.push(start.elapsed());
    }

    if !times.is_empty() {
        let total: Duration = times.iter().copied().sum();
        let avg = total / (times.len() as u32);
        let min = *times.iter().min().expect("non-empty times has a min");
        let max = *times.iter().max().expect("non-empty times has a max");

        println!("Benchmark results over {} iterations:", iterations);
        println!("  avg: {:.2?}", avg);
        println!("  min: {:.2?}", min);
        println!("  max: {:.2?}", max);
    }

    Ok(())
}
