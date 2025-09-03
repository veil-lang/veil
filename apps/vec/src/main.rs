use anyhow::Result;
use veil_cli as cli;

fn main() -> Result<()> {
    match cli::parse() {
        Ok(cli::CliCommand::Build {
            input,
            output,
            optimize,
            target_triple,
            verbose,
            dump_norm_hir,
            pass_timings,
            cache_stats,
            skip_cc,
        }) => {
            cli::process_build(
                input,
                output,
                optimize,
                target_triple,
                verbose,
                dump_norm_hir,
                pass_timings,
                cache_stats,
                false, // is_test
                skip_cc,
            )?;
            Ok(())
        }
        Ok(cli::CliCommand::Test { .. }) => {
            eprintln!("Command 'test' is not supported by this binary.");
            std::process::exit(1);
        }
        Ok(cli::CliCommand::Init { .. }) => {
            eprintln!("Command 'init' is not supported by this binary.");
            std::process::exit(1);
        }
        Ok(cli::CliCommand::Run { .. }) => {
            eprintln!("Command 'run' is not supported by this binary.");
            std::process::exit(1);
        }
        Ok(cli::CliCommand::Benchmark { .. }) => {
            eprintln!("Command 'benchmark' is not supported by this binary.");
            std::process::exit(1);
        }
        Ok(cli::CliCommand::Upgrade { .. }) => {
            eprintln!("Command 'upgrade' is not supported by this binary.");
            std::process::exit(1);
        }
        Err(e) => {
            eprintln!("Error: {}", e);
            std::process::exit(1);
        }
    }
}
