#![forbid(unsafe_code)]
#![deny(rust_2018_idioms)]
#![deny(unused_must_use)]

use anyhow::{Context, Result, anyhow};
use clap::{Parser, Subcommand};
use codespan::Files;
use codespan_reporting::term;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use colored::*;
use std::fs;
use std::path::{Path, PathBuf};

use veil_ast as ast;
use veil_codegen_c as codegen_c;
#[cfg(target_os = "windows")]
use veil_helpers::prepare_windows_clang_args;
use veil_helpers::{get_bundled_clang_path, validate_ve_file};
use veil_hir as hir;
use veil_ir as ir;
use veil_normalize as normalize;
use veil_resolve as resolve;
use veil_typeck as typeck;

/// Re-exported CLI API: command enumeration and parsing/build orchestration.

#[derive(Debug)]
#[allow(dead_code)]
pub struct CliError(pub String);

impl std::fmt::Display for CliError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}
impl std::error::Error for CliError {}

/// Upgrade channel (extracted from original CLI upgrade module surface)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum Channel {
    #[default]
    Stable,
    Canary,
}

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
        channel: Option<Channel>,
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

/// Public: parse CLI args into a high-level command.
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
            no_remind: _,
            force: _,
            verbose: _,
            channel,
        }) => Ok(CliCommand::Upgrade {
            no_remind: false,
            force: false,
            verbose: false,
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

/// Lightweight import resolver result (extracted from original compiler path)
#[derive(Debug, Clone)]
pub struct ResolvedImport {
    pub path: PathBuf,
    #[allow(dead_code)]
    pub import_type: ImportType,
    #[allow(dead_code)]
    pub module_path: String,
}

#[derive(Debug, Clone)]
pub enum ImportType {
    All {
        #[allow(dead_code)]
        alias: Option<String>,
    },
    Specifiers {
        #[allow(dead_code)]
        specifiers: Vec<ast::ImportSpecifier>,
    },
}

/// Resolve standard library module path (std/...) or fallback to lib/src layout.
fn resolve_standard_library_path(module_path: &str) -> Result<PathBuf> {
    let base_path = if let Ok(veil_lib_path) = std::env::var("VEIL_LIB_PATH") {
        PathBuf::from(veil_lib_path)
    } else {
        let exe_path = std::env::current_exe()
            .map_err(|e| anyhow!("Cannot determine executable path: {}", e))?;
        let exe_dir = exe_path
            .parent()
            .ok_or_else(|| anyhow!("Cannot determine executable directory"))?;

        let mut candidate_paths = vec![
            exe_dir.join("lib"),
            exe_dir.join("..").join("lib"),
            exe_dir.join("..").join("..").join("lib"),
        ];

        if let Ok(cargo_manifest_dir) = std::env::var("CARGO_MANIFEST_DIR") {
            candidate_paths.push(PathBuf::from(cargo_manifest_dir).join("lib"));
        }

        candidate_paths
            .into_iter()
            .find(|path| path.exists())
            .unwrap_or_else(|| PathBuf::from("lib"))
    };

    let full_path = if let Some(module_name) = module_path.strip_prefix("std/") {
        base_path
            .join("std")
            .join("src")
            .join(format!("{}.veil", module_name))
    } else {
        base_path.join("src").join(format!("{}.veil", module_path))
    };

    if full_path.exists() {
        Ok(full_path)
    } else {
        Err(anyhow!(
            "Standard library module not found: {}",
            module_path
        ))
    }
}

/// Resolve imports to concrete file paths (std/local/external)
fn resolve_imports_only(
    imports: &[ast::ImportDeclaration],
    base_path: &Path,
) -> Result<Vec<ResolvedImport>> {
    let mut resolved = Vec::new();

    for import in imports {
        match import {
            ast::ImportDeclaration::ImportAll {
                module_path,
                module_type,
                alias,
            } => {
                let resolved_path = match module_type {
                    ast::ModuleType::Standard => resolve_standard_library_path(module_path)?,
                    ast::ModuleType::Local => {
                        let current_dir = base_path
                            .parent()
                            .ok_or_else(|| anyhow!("Base path has no parent"))?;
                        current_dir.join(format!("{}.veil", module_path))
                    }
                    ast::ModuleType::External => {
                        let external_libs_dir =
                            std::env::var("VEIL_LIBS_PATH").unwrap_or_else(|_| "libs".to_string());
                        PathBuf::from(external_libs_dir).join(format!("{}.veil", module_path))
                    }
                };

                resolved.push(ResolvedImport {
                    path: resolved_path,
                    import_type: ImportType::All {
                        alias: alias.clone(),
                    },
                    module_path: module_path.clone(),
                });
            }
            ast::ImportDeclaration::ImportSpecifiers {
                module_path,
                module_type,
                specifiers,
            } => {
                let resolved_path = match module_type {
                    ast::ModuleType::Standard => resolve_standard_library_path(module_path)?,
                    ast::ModuleType::Local => {
                        let current_dir = base_path
                            .parent()
                            .ok_or_else(|| anyhow!("Base path has no parent"))?;
                        current_dir.join(format!("{}.veil", module_path))
                    }
                    ast::ModuleType::External => {
                        let external_libs_dir =
                            std::env::var("VEIL_LIBS_PATH").unwrap_or_else(|_| "libs".to_string());
                        PathBuf::from(external_libs_dir).join(format!("{}.veil", module_path))
                    }
                };

                resolved.push(ResolvedImport {
                    path: resolved_path,
                    import_type: ImportType::Specifiers {
                        specifiers: specifiers.clone(),
                    },
                    module_path: module_path.clone(),
                });
            }
            ast::ImportDeclaration::ExportImportAll {
                module_path,
                module_type,
                alias,
            } => {
                let resolved_path = match module_type {
                    ast::ModuleType::Standard => resolve_standard_library_path(module_path)?,
                    ast::ModuleType::Local => {
                        let current_dir = base_path
                            .parent()
                            .ok_or_else(|| anyhow!("Base path has no parent"))?;
                        current_dir.join(format!("{}.veil", module_path))
                    }
                    ast::ModuleType::External => {
                        let external_libs_dir =
                            std::env::var("VEIL_LIBS_PATH").unwrap_or_else(|_| "libs".to_string());
                        PathBuf::from(external_libs_dir).join(format!("{}.veil", module_path))
                    }
                };

                resolved.push(ResolvedImport {
                    path: resolved_path,
                    import_type: ImportType::All {
                        alias: alias.clone(),
                    },
                    module_path: module_path.clone(),
                });
            }
            ast::ImportDeclaration::ExportImportSpecifiers {
                module_path,
                module_type,
                specifiers,
            } => {
                let resolved_path = match module_type {
                    ast::ModuleType::Standard => resolve_standard_library_path(module_path)?,
                    ast::ModuleType::Local => {
                        let current_dir = base_path
                            .parent()
                            .ok_or_else(|| anyhow!("Base path has no parent"))?;
                        current_dir.join(format!("{}.veil", module_path))
                    }
                    ast::ModuleType::External => {
                        let external_libs_dir =
                            std::env::var("VEIL_LIBS_PATH").unwrap_or_else(|_| "libs".to_string());
                        PathBuf::from(external_libs_dir).join(format!("{}.veil", module_path))
                    }
                };
                resolved.push(ResolvedImport {
                    path: resolved_path,
                    import_type: ImportType::Specifiers {
                        specifiers: specifiers.clone(),
                    },
                    module_path: module_path.clone(),
                });
            }
        }
    }

    Ok(resolved)
}

/// Merge imported public items into the root program (simple model).
fn merge_imports_into_program(
    root_program: &mut ast::Program,
    entry_path: &Path,
    files: &mut Files<String>,
) -> Result<()> {
    // Parse entry already done; now resolve imports and merge
    let resolved = resolve_imports_only(&root_program.imports, entry_path)?;

    for item in resolved {
        let content = fs::read_to_string(&item.path)
            .with_context(|| format!("Failed to read module {}", item.path.display()))?;
        let fid = files.add(item.path.to_string_lossy().to_string(), content);
        let imported = veil_syntax::parse_ast(files, fid).map_err(|diags| {
            let clean_path = item.path.to_string_lossy().replace("\\\\?\\", "");
            let msg = diags
                .first()
                .map(|_| "Parse error")
                .unwrap_or("Parse error");
            anyhow!("{}:{}: {}", clean_path, 0, msg)
        })?;

        // Public functions
        for f in imported.functions.iter() {
            if matches!(f.visibility, ast::Visibility::Public) {
                root_program.functions.push(f.clone());
            }
        }
        // Public structs
        for s in imported.structs.iter() {
            if matches!(s.visibility, ast::Visibility::Public) {
                root_program.structs.push(s.clone());
            }
        }
        // Impl blocks whose target is builtin or exported struct in the same module
        for ib in imported.impls.iter() {
            let is_builtin_type = ib.target_type.contains("[]")
                || matches!(
                    ib.target_type.as_str(),
                    "string"
                        | "i32"
                        | "i64"
                        | "f32"
                        | "f64"
                        | "bool"
                        | "i8"
                        | "i16"
                        | "u8"
                        | "u16"
                        | "u32"
                        | "u64"
                );

            let target_is_exported = is_builtin_type
                || imported.structs.iter().any(|s| {
                    let target_name: &str = match ib.target_type_parsed.as_ref() {
                        Some(ast::Type::Struct(n)) => n.as_str(),
                        Some(ast::Type::GenericInstance(n, _)) => n.as_str(),
                        _ => ib.target_type.as_str(),
                    };
                    s.name == target_name && matches!(s.visibility, ast::Visibility::Public)
                });

            if target_is_exported {
                root_program.impls.push(ib.clone());
            }
        }

        // FFI
        root_program
            .ffi_functions
            .extend(imported.ffi_functions.into_iter());
        root_program
            .ffi_variables
            .extend(imported.ffi_variables.into_iter());
    }

    Ok(())
}

/// Public: process a build end-to-end, returning the final output path (exe or C file when skip_cc).
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

        for entry in fs::read_dir(&build_dir)? {
            let entry = entry?;
            let path = entry.path();
            if path.file_name() != Some(std::ffi::OsStr::new(".cache")) {
                if path.is_dir() {
                    fs::remove_dir_all(&path)?;
                } else {
                    fs::remove_file(&path)?;
                }
            }
        }

        if verbose {
            println!("{}", "Cache preserved".green());
        }
    } else {
        fs::create_dir_all(&build_dir)?;
    }

    let output = build_dir.join(output.file_name().unwrap());
    let c_file = build_dir.join("temp.c");

    let mut files = Files::<String>::new();

    // Parse entry file
    let entry_content = fs::read_to_string(&input)
        .with_context(|| format!("Failed to read input file {}", input.display()))?;
    let file_id = files.add(input.to_string_lossy().to_string(), entry_content);

    let mut program = veil_syntax::parse_ast(&files, file_id).map_err(|diags| {
        let clean_path = input.to_string_lossy().replace("\\\\?\\", "");
        let msg = diags
            .first()
            .map(|_| "Parse error")
            .unwrap_or("Parse error");
        anyhow!("{}:{}: {}", clean_path, 0, msg)
    })?;

    // Ensure prelude import unless compiling prelude itself
    let is_prelude_module = input.to_string_lossy().ends_with("prelude.veil");
    let has_prelude_import = program.imports.iter().any(|import| match import {
        ast::ImportDeclaration::ImportAll { module_path, .. } => module_path == "std/prelude",
        ast::ImportDeclaration::ExportImportAll { module_path, .. } => module_path == "std/prelude",
        _ => false,
    });
    if !is_prelude_module && !has_prelude_import {
        let prelude_import = ast::ImportDeclaration::ImportAll {
            module_path: "std/prelude".to_string(),
            module_type: ast::ModuleType::Standard,
            alias: None,
        };
        program.imports.insert(0, prelude_import);
    }

    // Merge imported public items into the root program
    merge_imports_into_program(&mut program, &input, &mut files)?;

    if verbose {
        println!("{}", format!("Input file: {}", input.display()).cyan());
        println!("{}", format!("Output file: {}", output.display()).cyan());
        println!(
            "{}",
            format!("Build directory: {}", build_dir.display()).cyan()
        );

        let ast_file = build_dir.join("parsed_ast.txt");
        let ast_content = format!("Parsed AST:\n{:#?}", program);
        fs::write(&ast_file, ast_content)?;
        println!(
            "{}",
            format!("Parsed AST saved to: {}", ast_file.display()).green()
        );

        // Lower AST to HIR and save HIR dump
        let module_id = hir::ids::ModuleId::new(0);
        match hir::lower_program(&program, module_id) {
            Ok(hir_program) => {
                let hir_file = build_dir.join("lowered_hir.txt");
                let hir_content = format!("HIR Program:\n{:#?}", hir_program);
                fs::write(&hir_file, hir_content)?;
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

    // HIR lowering
    let module_id = hir::ids::ModuleId::new(0);
    let mut hir_program = match hir::lower_program(&program, module_id) {
        Ok(h) => h,
        Err(err) => {
            println!("{}", format!("HIR lowering failed: {}", err).yellow());
            return Err(anyhow!("HIR lowering failed"));
        }
    };

    // Name/module/visibility resolution on HIR
    match resolve::resolve_program(&mut hir_program) {
        Ok(resolver_ctx) => {
            // Type check resolved HIR
            let mut hir_type_checker = typeck::TypeChecker::new(
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

            if verbose || dump_norm_hir {
                normalize::normalize_program(&mut hir_program);
                let norm_file = build_dir.join("normalized_hir.txt");
                let norm_content = format!("Normalized HIR:\n{:#?}", hir_program);
                fs::write(&norm_file, norm_content)?;
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

    // M7: Monomorphize (AST-level, as in original pipeline)
    let mono = veil_mono::Monomorphizer::new(Default::default());
    let (program, mono_meta) = mono
        .monomorphize_with_metadata(&program)
        .map_err(|e| anyhow!("Monomorphization failed: {}", e))?;
    if verbose {
        let mono_file = build_dir.join("monomorphized_ast.txt");
        let mono_content = format!("Monomorphized Program (AST):\n{:#?}", program);
        fs::write(&mono_file, mono_content)?;
        println!(
            "{}",
            format!("Monomorphized AST saved to: {}", mono_file.display()).green()
        );

        let meta_file = build_dir.join("monomorphization_metadata.json");
        let meta_content =
            serde_json::to_string_pretty(&mono_meta).unwrap_or_else(|_| String::from("{}"));
        fs::write(&meta_file, meta_content)?;
        println!(
            "{}",
            format!(
                "Monomorphization metadata saved to: {}",
                meta_file.display()
            )
            .green()
        );

        // M8: Lower mono AST → HIR → normalize → IR and dump when verbose
        let module_id = hir::ids::ModuleId::new(0);
        match hir::lower_program(&program, module_id) {
            Ok(mut mono_hir) => {
                normalize::normalize_program(&mut mono_hir);
                let program_ir = ir::lower_from_hir(&mono_hir);
                let ir_file = build_dir.join("lowered_ir.txt");
                let ir_content = program_ir.to_pretty_string();
                fs::write(&ir_file, ir_content)?;
                println!("{}", format!("IR saved to: {}", ir_file.display()).green());
            }
            Err(err) => {
                println!("{}", format!("IR lowering (HIR) failed: {}", err).yellow());
            }
        }
    }

    // Lower to IR (non-verbose path)
    let module_id = hir::ids::ModuleId::new(0);
    let program_ir = match hir::lower_program(&program, module_id) {
        Ok(mut mono_hir) => {
            normalize::normalize_program(&mut mono_hir);
            ir::lower_from_hir(&mono_hir)
        }
        Err(_err) => {
            // Fallback to AST path to avoid breaking builds
            ir::lower_from_ast(&program)
        }
    };

    // Emit C via IR→C backend
    let ir_backend = codegen_c::IrCBackend::new(codegen_c::CodegenConfig {
        target_triple: target_triple.clone(),
    });
    ir_backend.write_to_path(&program_ir, &c_file)?;

    if verbose {
        println!(
            "{}",
            format!("C emitted from IR to: {}", c_file.display()).green()
        );
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
