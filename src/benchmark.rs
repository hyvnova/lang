use std::{path::PathBuf, str::FromStr, time::Instant, fs};

use crate::lexer::Lexer;

pub const LEXER_BENCHMARK_FILES: [&str; 3] = [
    "./lexer_benchmark_data/bt.txt",
    "./lexer_benchmark_data/bt-50.txt",
    "./lexer_benchmark_data/bt-100.txt",
];

pub fn benchmark_lex(filename: &str, num_runs: u32) {
    let path = PathBuf::from_str(filename).expect("Couldn't find the file");

    // Get file size
    let file_size = fs::metadata(&path).expect("Couldn't get file metadata").len();
    println!("File size: {} bytes", file_size);

    let mut token_count = 0;

    let mut total_time = 0.0;
    let mut times = Vec::with_capacity(num_runs as usize);

    for i in 0..num_runs {
        let start_time = Instant::now();

        // Run your lexer
        let mut lexer = Lexer::from_path(path.clone());
        let mut count = 0;
        while let Some(token) = lexer.next() {
            println!("{}", token);
            count += 1;
        }

        let elapsed_time = start_time.elapsed();
        let elapsed_secs = elapsed_time.as_secs_f64();
        
        times.push(elapsed_secs);
        total_time += elapsed_secs;
        token_count += count;

        println!("Run {}: {:.4} seconds, {} tokens", i + 1, elapsed_secs, count);
    }

    let avg_time = total_time / num_runs as f64;
    println!("\nAverage time: {:.4} seconds", avg_time);
    println!("Best time: {:.4} seconds", times.iter().fold(f64::INFINITY, |a, &b| a.min(b)));
    println!("Worst time: {:.4} seconds", times.iter().fold(0.0f64, |a, &b| a.max(b)));

    // Calculate tokens per second based on the average time
    let tokens_per_second = token_count as f64 / avg_time;
    println!("\nEstimated performance: {:.2} tokens/second", tokens_per_second);
}
