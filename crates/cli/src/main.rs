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
            recursive,
        }) => cli::test::run_test(input, test_name, verbose, list, recursive),
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
        Ok(cli::CliCommand::Update {
            verbose,
            force,
            channel,
        }) => {
            let update_channel = match channel {
                cli::Channel::Stable => cli::update::UpdateChannel::Stable,
                cli::Channel::Canary => cli::update::UpdateChannel::Nightly,
            };
            cli::update::run_update(verbose, force, update_channel)
        }
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
