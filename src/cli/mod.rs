pub mod benchmark;
pub mod init;
pub mod run;
pub mod test;
pub mod upgrade;

use crate::cli::upgrade::Channel;
use crate::compiler::incremental::IncrementalCompiler;
use crate::helpers::get_bundled_clang_path;
#[cfg(target_os = "windows")]
use crate::helpers::prepare_windows_clang_args;
use crate::helpers::validate_ve_file;

use anyhow::anyhow;
use clap::{Parser, Subcommand};
use codespan::Files;
use codespan_reporting::term;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use colored::*;
use std::path::PathBuf;

#[derive(Debug)]
#[allow(dead_code)]
pub struct CliError(pub String);

impl std::fmt::Display for CliError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl std::error::Error for CliError {}

#[derive(Debug)]
pub enum CliCommand {
    Build {
        input: PathBuf,
        output: PathBuf,
        optimize: bool,
        target_triple: String,
        verbose: bool,
        dump_norm_hir: bool,
        skip_cc: bool,
    },
    Init {
        directory: PathBuf,
        project_name: String,
    },
    Run {
        input: PathBuf,
        verbose: bool,
    },
    Benchmark {
        input: PathBuf,
        iterations: usize,
        verbose: bool,
    },
    Upgrade {
        no_remind: bool,
        force: bool,
        verbose: bool,
        channel: Channel,
    },
    Test {
        input: PathBuf,
        test_name: Option<String>,
        verbose: bool,
        list: bool,
    },
}

#[derive(Parser)]
#[command(
    version,
    about,
    args_conflicts_with_subcommands = true,
    subcommand_negates_reqs = true,
    disable_help_subcommand = true
)]
pub struct Args {
    #[command(subcommand)]
    command: Option<Command>,

    #[arg(required = true, value_parser = validate_ve_file, value_name = "FILE[.veil]")]
    input: Option<PathBuf>,

    #[arg(short, long, default_value = "program.exe")]
    output: PathBuf,

    #[arg(long, action = clap::ArgAction::SetFalse)]
    optimize: bool,

    #[arg(long)]
    target_triple: Option<String>,

    #[arg(short, long)]
    verbose: bool,

    #[arg(
        long,
        help = "Dump normalized HIR after typecheck (even without --verbose)"
    )]
    dump_norm_hir: bool,

    #[arg(long, help = "Skip C compilation and execution (generate C only)")]
    no_cc: bool,

    #[arg(long)]
    iterations: Option<usize>,
}

#[derive(Subcommand)]
enum Command {
    Build {
        #[arg(value_parser = validate_ve_file)]
        input: PathBuf,

        #[arg(short, long, default_value = "build/program.exe")]
        output: PathBuf,

        #[arg(long, action = clap::ArgAction::SetFalse)]
        optimize: bool,

        #[arg(long)]
        target_triple: Option<String>,

        #[arg(short, long)]
        verbose: bool,

        #[arg(
            long,
            help = "Dump normalized HIR after typecheck (even without --verbose)"
        )]
        dump_norm_hir: bool,

        #[arg(long, help = "Skip C compilation and execution (generate C only)")]
        no_cc: bool,
    },
    Init {
        project_name: String,
        #[arg(default_value = ".")]
        directory: PathBuf,
    },
    Run {
        #[arg(value_parser = validate_ve_file)]
        input: PathBuf,
        #[arg(short, long)]
        verbose: bool,
    },
    Benchmark {
        #[arg(value_parser = validate_ve_file)]
        input: PathBuf,
        #[arg(short, long, default_value_t = 10)]
        iterations: usize,
        #[arg(short, long)]
        verbose: bool,
    },
    Upgrade {
        #[arg(long, help = "Disable update reminder notifications")]
        no_remind: bool,
        #[arg(short, long, help = "Force upgrade without confirmation")]
        force: bool,
        #[arg(short, long, help = "Show verbose output during upgrade")]
        verbose: bool,
        #[arg(long, help = "Update channel: stable or canary", value_parser = parse_channel)]
        channel: Option<crate::cli::upgrade::Channel>,
    },
    Test {
        #[arg(value_parser = validate_ve_file)]
        input: PathBuf,
        #[arg(short, long)]
        test_name: Option<String>,
        #[arg(short, long)]
        verbose: bool,
        #[arg(long, help = "List available tests")]
        list: bool,
    },
}

fn parse_channel(s: &str) -> Result<Channel, String> {
    match s.to_lowercase().as_str() {
        "stable" => Ok(Channel::Stable),
        "canary" => Ok(Channel::Canary),
        _ => Err(format!(
            "Invalid channel '{}'. Valid options: stable, canary",
            s
        )),
    }
}

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

pub fn parse() -> anyhow::Result<CliCommand> {
    let args = Args::parse();

    match args.command {
        Some(Command::Build {
            input,
            output,
            optimize,
            target_triple,
            verbose,
            dump_norm_hir,
            no_cc,
        }) => Ok(CliCommand::Build {
            input,
            output,
            optimize,
            target_triple: target_triple.unwrap_or_else(default_target_triple),
            verbose,
            dump_norm_hir,
            skip_cc: no_cc,
        }),
        Some(Command::Init {
            directory,
            project_name,
        }) => Ok(CliCommand::Init {
            directory,
            project_name,
        }),
        Some(Command::Run { input, verbose }) => Ok(CliCommand::Run { input, verbose }),
        Some(Command::Benchmark {
            input,
            iterations,
            verbose,
        }) => Ok(CliCommand::Benchmark {
            input,
            iterations,
            verbose,
        }),
        Some(Command::Upgrade {
            no_remind,
            force,
            verbose,
            channel,
        }) => Ok(CliCommand::Upgrade {
            no_remind,
            force,
            verbose,
            channel: channel.unwrap_or_default(),
        }),
        Some(Command::Test {
            input,
            test_name,
            verbose,
            list,
        }) => Ok(CliCommand::Test {
            input,
            test_name,
            verbose,
            list,
        }),
        None => {
            let input = args
                .input
                .ok_or_else(|| anyhow!("Input file is required"))?;
            if let Some(iterations) = args.iterations {
                return Ok(CliCommand::Benchmark {
                    input,
                    iterations,
                    verbose: args.verbose,
                });
            }
            Ok(CliCommand::Build {
                input,
                output: args.output,
                optimize: args.optimize,
                target_triple: args.target_triple.unwrap_or_else(default_target_triple),
                verbose: args.verbose,
                dump_norm_hir: args.dump_norm_hir,
                skip_cc: args.no_cc,
            })
        }
    }
}

pub fn process_build(
    input: PathBuf,
    output: PathBuf,
    optimize: bool,
    target_triple: String,
    verbose: bool,
    dump_norm_hir: bool,
    is_test: bool,
    skip_cc: bool,
) -> anyhow::Result<PathBuf> {
    let build_dir = input
        .parent()
        .ok_or_else(|| anyhow!("Invalid input file path"))?
        .join("build");

    if build_dir.exists() {
        if verbose {
            println!(
                "{}",
                format!("Cleaning build directory: {}", build_dir.display()).yellow()
            );
        }

        for entry in std::fs::read_dir(&build_dir)? {
            let entry = entry?;
            let path = entry.path();
            if path.file_name() != Some(std::ffi::OsStr::new(".cache")) {
                if path.is_dir() {
                    std::fs::remove_dir_all(&path)?;
                } else {
                    std::fs::remove_file(&path)?;
                }
            }
        }

        if verbose {
            println!("{}", "Cache preserved".green());
        }
    } else {
        std::fs::create_dir_all(&build_dir)?;
    }

    let output = build_dir.join(output.file_name().unwrap());
    let c_file = build_dir.join("temp.c");

    let mut files = Files::<String>::new();
    let mut module_compiler = IncrementalCompiler::new(&build_dir);

    if verbose {
        println!(
            "{}",
            "Discovering modules and building dependency graph...".yellow()
        );
    }

    module_compiler.build_dependency_graph(&input)?;

    if verbose {
        println!("{}", "Compiling modules incrementally...".yellow());
    }

    let compiled_modules = module_compiler.compile_all_modules(&mut files, verbose)?;

    if verbose && !compiled_modules.is_empty() {
        println!("{} modules were recompiled", compiled_modules.len());
    }

    if verbose {
        println!("{}", "Creating merged program...".yellow());
    }

    let mut program = module_compiler.create_merged_program(&input)?;

    if verbose {
        println!("{}", format!("Input file: {}", input.display()).cyan());
        println!("{}", format!("Output file: {}", output.display()).cyan());
        println!(
            "{}",
            format!("Build directory: {}", build_dir.display()).cyan()
        );
    }

    if verbose {
        let ast_file = build_dir.join("parsed_ast.txt");
        let ast_content = format!("Parsed AST:\n{:#?}", program);
        std::fs::write(&ast_file, ast_content)?;
        println!(
            "{}",
            format!("Parsed AST saved to: {}", ast_file.display()).green()
        );

        // Lower AST to HIR and save HIR dump
        let module_id = crate::hir::ids::ModuleId::new(0);
        match crate::hir::lower_program(&program, module_id) {
            Ok(hir_program) => {
                let hir_file = build_dir.join("lowered_hir.txt");
                let hir_content = format!("HIR Program:\n{:#?}", hir_program);
                std::fs::write(&hir_file, hir_content)?;
                println!(
                    "{}",
                    format!("HIR saved to: {}", hir_file.display()).green()
                );
            }
            Err(err) => {
                println!("{}", format!("HIR lowering failed: {}", err).yellow());
            }
        }
    }

    let file_id = module_compiler.get_entry_file_id(&mut files, &input)?;

    // M4+M5: Lower AST → HIR, Resolve, then Type Check on HIR (replace legacy AST checker)
    let module_id = crate::hir::ids::ModuleId::new(0);
    let mut hir_program = match crate::hir::lower_program(&program, module_id) {
        Ok(h) => h,
        Err(err) => {
            println!("{}", format!("HIR lowering failed: {}", err).yellow());
            return Err(anyhow!("HIR lowering failed"));
        }
    };

    // Name/module/visibility resolution on HIR
    match veil_resolve::resolve_program(&mut hir_program) {
        Ok(resolver_ctx) => {
            // Type check resolved HIR
            let mut hir_type_checker = veil_typeck::TypeChecker::new(
                resolver_ctx.symbol_table.clone(),
                file_id,
                Some(module_id),
            );

            if let Err(errors) = hir_type_checker.check_program(&mut hir_program) {
                let writer = StandardStream::stderr(ColorChoice::Auto);
                let config = term::Config::default();
                println!("=== TYPE CHECKER ERRORS (HIR) ===");
                println!("Found {} errors", errors.len());
                for diag in &errors {
                    let _ = term::emit(&mut writer.lock(), &config, &files, diag);
                }
                return Err(anyhow!("Type check failed"));
            }

            // Optional: dump normalized HIR snapshot
            if verbose || dump_norm_hir {
                veil_normalize::normalize_program(&mut hir_program);
                let norm_file = build_dir.join("normalized_hir.txt");
                let norm_content = format!("Normalized HIR:\n{:#?}", hir_program);
                std::fs::write(&norm_file, norm_content)?;
                println!(
                    "{}",
                    format!("Normalized HIR saved to: {}", norm_file.display()).green()
                );
            }
        }
        Err(errors) => {
            let writer = StandardStream::stderr(ColorChoice::Auto);
            let config = term::Config::default();
            println!("=== RESOLUTION ERRORS ===");
            println!("Found {} errors", errors.len());
            for diag in &errors {
                let d = diag.to_diagnostic();
                let _ = term::emit(&mut writer.lock(), &config, &files, &d);
            }
            return Err(anyhow!("Resolution failed"));
        }
    }

    // M7: Monomorphize generic functions before codegen (with metadata)
    let mono = veil_mono::Monomorphizer::new(Default::default());
    let (program, mono_meta) = mono
        .monomorphize_with_metadata(&program)
        .map_err(|e| anyhow!("Monomorphization failed: {}", e))?;
    if verbose {
        let mono_file = build_dir.join("monomorphized_ast.txt");
        let mono_content = format!("Monomorphized Program (AST):\n{:#?}", program);
        std::fs::write(&mono_file, mono_content)?;
        println!(
            "{}",
            format!("Monomorphized AST saved to: {}", mono_file.display()).green()
        );

        let meta_file = build_dir.join("monomorphization_metadata.json");
        let meta_content =
            serde_json::to_string_pretty(&mono_meta).unwrap_or_else(|_| String::from("{}"));
        std::fs::write(&meta_file, meta_content)?;
        println!(
            "{}",
            format!(
                "Monomorphization metadata saved to: {}",
                meta_file.display()
            )
            .green()
        );

        // M8: Lower mono AST → HIR → normalize → IR and dump when verbose
        let module_id = crate::hir::ids::ModuleId::new(0);
        match crate::hir::lower_program(&program, module_id) {
            Ok(mut mono_hir) => {
                veil_normalize::normalize_program(&mut mono_hir);
                let ir = veil_ir::lower_from_hir(&mono_hir);
                let ir_file = build_dir.join("lowered_ir.txt");
                let ir_content = ir.to_pretty_string();
                std::fs::write(&ir_file, ir_content)?;
                println!("{}", format!("IR saved to: {}", ir_file.display()).green());
            }
            Err(err) => {
                println!("{}", format!("IR lowering (HIR) failed: {}", err).yellow());
            }
        }
    }

    {
        // Lower to IR from HIR and emit C via the dedicated IR→C backend
        let module_id = crate::hir::ids::ModuleId::new(0);
        let ir = match crate::hir::lower_program(&program, module_id) {
            Ok(mut mono_hir) => {
                veil_normalize::normalize_program(&mut mono_hir);
                veil_ir::lower_from_hir(&mono_hir)
            }
            Err(_err) => {
                // Fallback to temporary AST path to avoid breaking builds
                veil_ir::lower_from_ast(&program)
            }
        };

        // Use the extracted codegen-c crate to render a C translation unit
        let ir_backend = veil_codegen_c::IrCBackend::new(veil_codegen_c::CodegenConfig {
            target_triple: target_triple.clone(),
        });
        ir_backend.write_to_path(&ir, &c_file)?;

        if verbose {
            println!(
                "{}",
                format!("C emitted from IR to: {}", c_file.display()).green()
            );
        }
    }
    if skip_cc {
        if verbose {
            println!(
                "{}",
                "Skipping C compilation and execution (--no-cc)".yellow()
            );
            println!(
                "{}",
                format!("Generated C file at: {}", c_file.display()).green()
            );
        }
        return Ok(c_file);
    }

    if verbose {
        println!(
            "{}",
            format!("Compiling generated C code: {}", c_file.display()).yellow()
        );
    }

    #[cfg(target_os = "windows")]
    let mut clang_args: Vec<String> = prepare_windows_clang_args(&output, optimize, &c_file)?;

    #[cfg(not(target_os = "windows"))]
    let mut clang_args: Vec<String> = vec![
        if optimize { "-O3" } else { "-O0" }.to_string(),
        c_file.to_str().unwrap().into(),
        "-o".to_string(),
        output.to_str().unwrap().into(),
    ];
    // Link veil-runtime static library if provided via environment.
    // Expect VEIL_RUNTIME_LIB_DIR to point to a directory containing libveil_runtime.a
    if let Ok(libdir) = std::env::var("VEIL_RUNTIME_LIB_DIR") {
        clang_args.push(format!("-L{}", libdir));
        clang_args.push("-lveil_runtime".to_string());
    }

    // Suppress codegen-c iterator stubs; require runtime to provide hooks
    clang_args.push("-DVEIL_RUNTIME_PROVIDES_ITER".to_string());
    if verbose {
        clang_args.insert(0, "-v".to_string());
        clang_args.push("-DVE_DEBUG_MEMORY".to_string());
    }

    let clang_path = get_bundled_clang_path()?;
    let output_result = std::process::Command::new(clang_path)
        .args(&clang_args)
        .output()
        .map_err(|e| anyhow!("Failed to compile C code: {}", e))?;

    if !output_result.status.success() {
        let stdout = String::from_utf8_lossy(&output_result.stdout);
        let stderr = String::from_utf8_lossy(&output_result.stderr);
        if verbose {
            eprintln!("\n---- clang stdout ----\n{}", stdout);
            eprintln!("\n---- clang stderr ----\n{}", stderr);
            eprintln!("\nArgs: {:?}", clang_args);
        } else {
            for line in stderr.lines() {
                if line.contains("error:") || line.contains("fatal error:") {
                    eprintln!("{}", line);
                }
            }
        }
        return Err(anyhow!(
            "C compiler failed with status: {}",
            output_result.status
        ));
    }

    if verbose {
        println!(
            "{}",
            format!("Successfully compiled to: {}", output.display()).green()
        );
    }

    let artifacts = vec![output.clone(), c_file.clone()];
    let entry_dependencies =
        module_compiler.collect_module_dependencies(&program.imports, &input)?;
    module_compiler.cache_compilation_artifacts(&input, entry_dependencies, artifacts)?;

    if verbose {
        println!("{}", "Artifacts cached successfully".green());
    }

    if is_test {
        return Ok(output);
    }

    if verbose {
        println!("{}", "Running the compiled program...".bold().blue());
    }

    let status = std::process::Command::new(output.to_str().unwrap())
        .status()
        .map_err(|e| anyhow!("Failed to run program: {}", e))?;

    if verbose {
        println!(
            "{}",
            format!("Program exited with status: {}", status).magenta()
        );
    }

    Ok(output)
}
