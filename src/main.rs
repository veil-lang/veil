use anyhow::Result;
use ve::cli;

fn main() -> Result<()> {
    let should_check_updates = std::env::args().nth(1).as_deref() != Some("upgrade");

    if should_check_updates {
        let _ = cli::upgrade::check_and_notify_updates();
    }

    match cli::parse() {
        Ok(cli::CliCommand::Build {
            input,
            output,
            optimize,
            target_triple,
            verbose,
            dump_norm_hir,
            skip_cc,
        }) => {
            cli::process_build(
                input,
                output,
                optimize,
                target_triple,
                verbose,
                dump_norm_hir,
                false,
                skip_cc,
            )?;
            Ok(())
        }
        Ok(cli::CliCommand::Test {
            input,
            test_name,
            verbose,
            list,
        }) => cli::test::run_test(input, test_name, verbose, list),
        Ok(cli::CliCommand::Init {
            directory,
            project_name,
        }) => cli::init::create_project(&directory, &project_name),
        Ok(cli::CliCommand::Run { input, verbose }) => cli::run::run_project(input, verbose),
        Ok(cli::CliCommand::Benchmark {
            input,
            iterations,
            verbose,
        }) => cli::benchmark::run_benchmark(input, iterations, verbose),
        Ok(cli::CliCommand::Upgrade {
            no_remind,
            force,
            verbose,
            channel,
        }) => cli::upgrade::run_upgrade(no_remind, force, verbose, channel),
        Err(e) => {
            eprintln!("Error: {}", e);
            std::process::exit(1);
        }
    }
}
