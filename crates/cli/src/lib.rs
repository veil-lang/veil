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
use veil_compiler::{Pass, PassCx, PassManager, digest_bytes, digest_strs};
#[cfg(target_os = "windows")]
use veil_helpers::prepare_windows_clang_args;
use veil_helpers::{get_bundled_clang_path, validate_ve_file};
use veil_hir as hir;
use veil_ir as ir;
use veil_normalize as normalize;
use veil_resolve as resolve;
use veil_typeck as typeck;
pub mod benchmark;
pub mod init;
pub mod run;
pub mod test;
pub mod upgrade;

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
        pass_timings: bool,
        cache_stats: bool,
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

    #[arg(long, help = "Show per-pass timings in the build output")]
    pass_timings: bool,

    #[arg(long, help = "Show cache hit/miss statistics for each pass")]
    cache_stats: bool,

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

        #[arg(long, help = "Show per-pass timings in the build output")]
        pass_timings: bool,

        #[arg(long, help = "Show cache hit/miss statistics for each pass")]
        cache_stats: bool,
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
        #[arg(value_parser = validate_ve_file, value_name = "FILE|DIR", help = "Path to a .veil file or a directory to scan recursively for tests")]
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
            pass_timings,
            cache_stats,
        }) => {
            if pass_timings { /* pass_timings: no-op in CLI parse stage */ }
            if cache_stats { /* cache_stats: no-op in CLI parse stage */ }
            Ok(CliCommand::Build {
                input,
                output,
                optimize,
                target_triple: target_triple.unwrap_or_else(default_target_triple),
                verbose,
                dump_norm_hir,
                pass_timings,
                cache_stats,
                skip_cc: no_cc,
            })
        }
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
            if args.pass_timings { /* pass_timings: no-op in CLI parse stage */ }
            if args.cache_stats { /* cache_stats: no-op in CLI parse stage */ }
            Ok(CliCommand::Build {
                input,
                output: args.output,
                optimize: args.optimize,
                target_triple: args.target_triple.unwrap_or_else(default_target_triple),
                verbose: args.verbose,
                dump_norm_hir: args.dump_norm_hir,
                pass_timings: args.pass_timings,
                cache_stats: args.cache_stats,
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
            exe_dir.join("..").join("..").join("..").join("lib"), // For tests in target/debug/deps
        ];

        if let Ok(cargo_manifest_dir) = std::env::var("CARGO_MANIFEST_DIR") {
            candidate_paths.push(PathBuf::from(cargo_manifest_dir).join("lib"));
        }

        // Add current working directory as fallback for development/testing
        if let Ok(cwd) = std::env::current_dir() {
            candidate_paths.push(cwd.join("lib"));
            // Also try going up from current directory to find project root
            if let Some(parent) = cwd.parent() {
                candidate_paths.push(parent.join("lib"));
                if let Some(grandparent) = parent.parent() {
                    candidate_paths.push(grandparent.join("lib"));
                    if let Some(great_grandparent) = grandparent.parent() {
                        candidate_paths.push(great_grandparent.join("lib"));
                    }
                }
            }
        }

        candidate_paths
            .into_iter()
            .find(|path| path.exists())
            .unwrap_or_else(|| PathBuf::from("lib"))
    };

    let full_path = if let Some(module_name) = module_path
        .strip_prefix("std::")
        .or_else(|| module_path.strip_prefix("std/"))
    {
        base_path
            .join("std")
            .join("src")
            .join(format!("{}.veil", module_name))
    } else {
        base_path
            .join("src")
            .join(format!("{}.veil", module_path.replace("::", "/")))
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
                        current_dir.join(format!("{}.veil", module_path.replace("::", "/")))
                    }
                    ast::ModuleType::External => {
                        let external_libs_dir =
                            std::env::var("VEIL_LIBS_PATH").unwrap_or_else(|_| "libs".to_string());
                        PathBuf::from(external_libs_dir)
                            .join(format!("{}.veil", module_path.replace("::", "/")))
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
                        current_dir.join(format!("{}.veil", module_path.replace("::", "/")))
                    }
                    ast::ModuleType::External => {
                        let external_libs_dir =
                            std::env::var("VEIL_LIBS_PATH").unwrap_or_else(|_| "libs".to_string());
                        PathBuf::from(external_libs_dir)
                            .join(format!("{}.veil", module_path.replace("::", "/")))
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
                        current_dir.join(format!("{}.veil", module_path.replace("::", "/")))
                    }
                    ast::ModuleType::External => {
                        let external_libs_dir =
                            std::env::var("VEIL_LIBS_PATH").unwrap_or_else(|_| "libs".to_string());
                        PathBuf::from(external_libs_dir)
                            .join(format!("{}.veil", module_path.replace("::", "/")))
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
                        current_dir.join(format!("{}.veil", module_path.replace("::", "/")))
                    }
                    ast::ModuleType::External => {
                        let external_libs_dir =
                            std::env::var("VEIL_LIBS_PATH").unwrap_or_else(|_| "libs".to_string());
                        PathBuf::from(external_libs_dir)
                            .join(format!("{}.veil", module_path.replace("::", "/")))
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
    _files: &mut Files<String>,
) -> Result<()> {
    // Parse entry already done; now resolve imports and merge
    let resolved = resolve_imports_only(&root_program.imports, entry_path)?;

    // Recursively process imports and re-exports (export import ...)
    let mut queue: std::collections::VecDeque<ResolvedImport> = resolved.into();
    let mut visited: std::collections::HashSet<std::path::PathBuf> =
        std::collections::HashSet::new();

    let mut iteration: usize = 0;

    while let Some(item) = queue.pop_front() {
        iteration += 1;

        let canonical = std::fs::canonicalize(&item.path).unwrap_or(item.path.clone());
        #[cfg(windows)]
        let canonical_key = {
            use std::path::PathBuf;
            PathBuf::from(
                canonical
                    .to_string_lossy()
                    .to_lowercase()
                    .replace('\\', "/"),
            )
        };
        #[cfg(not(windows))]
        let canonical_key = canonical.clone();

        if !visited.insert(canonical_key) {
            continue;
        }

        if iteration > 20_000 {
            return Err(anyhow!(
                "Aborting import processing after {} iterations (probable cyclic or exploding import graph)",
                iteration
            ));
        }

        let content = fs::read_to_string(&item.path)
            .with_context(|| format!("Failed to read module {}", item.path.display()))?;

        // Parse the imported module on a dedicated thread with a larger stack to avoid
        // overflowing the main thread stack on Windows.
        let path_string = item.path.to_string_lossy().to_string();
        let content_clone = content.clone();
        let parse_join_handle = std::thread::Builder::new()
            .name("veil-parse".into())
            .stack_size(16 * 1024 * 1024)
            .spawn(move || {
                // Use a temporary Files buffer isolated to this thread.
                let mut temp_files = Files::<String>::new();
                let tfid = temp_files.add(path_string.clone(), content_clone);
                veil_syntax::parse_ast(&temp_files, tfid)
            })
            .map_err(|e| anyhow!("Failed to spawn parser thread: {}", e))?;

        let parse_result = parse_join_handle
            .join()
            .map_err(|_| anyhow!("Parser thread panicked"))?;

        let imported = parse_result.map_err(|diags| {
            let clean_path = item.path.to_string_lossy().replace("\\\\?\\", "");

            let msg = diags
                .first()
                .map(|_| "Parse error")
                .unwrap_or("Parse error");

            anyhow!("{}:{}: {}", clean_path, 0, msg)
        })?;

        // Merge exports from this module into the root based on how it was imported.
        match &item.import_type {
            // Re-export specific specifiers
            ImportType::Specifiers { specifiers } => {
                // Functions
                for spec in specifiers {
                    if let Some(func) = imported.functions.iter().find(|f| {
                        matches!(f.visibility, ast::Visibility::Public) && f.name == spec.name
                    }) {
                        let mut fcl = func.clone();
                        if let Some(alias) = &spec.alias {
                            fcl.name = alias.clone();
                        }
                        root_program.functions.push(fcl);
                    }
                }
                // Structs
                for spec in specifiers {
                    if let Some(st) = imported.structs.iter().find(|s| {
                        matches!(s.visibility, ast::Visibility::Public) && s.name == spec.name
                    }) {
                        let scl = st.clone();
                        root_program.structs.push(scl);
                    }
                }
                // Note: enums and other items can be added here similarly if needed.
            }
            // Import all public items
            ImportType::All { .. } => {
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
            }
        }

        // Follow re-exports (export import ...) from this imported module
        let export_imports: Vec<ast::ImportDeclaration> = imported
            .imports
            .iter()
            .filter_map(|imp| match imp {
                ast::ImportDeclaration::ExportImportAll { .. }
                | ast::ImportDeclaration::ExportImportSpecifiers { .. } => Some(imp.clone()),
                _ => None,
            })
            .collect();

        if !export_imports.is_empty() {
            // Resolve re-exported imports relative to this module's path
            let next = resolve_imports_only(&export_imports, &item.path)?;
            for n in next {
                queue.push_back(n);
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

fn compute_transitive_import_digests(
    root_imports: &[ast::ImportDeclaration],
    base_path: &Path,
    _files: &mut Files<String>,
) -> Vec<String> {
    // Breadth-first traversal of imports to collect transitive module digests.
    // Errors are non-fatal; missing/unparseable modules are skipped for robustness.
    let mut digests: Vec<(String, String)> = Vec::new(); // (path_str, digest)
    let mut visited: std::collections::HashSet<String> = std::collections::HashSet::new();

    let mut queue: std::collections::VecDeque<ResolvedImport> =
        resolve_imports_only(root_imports, base_path)
            .unwrap_or_default()
            .into();

    let mut iteration: usize = 0;
    let safety_cap: usize = 20_000;

    while let Some(item) = queue.pop_front() {
        iteration += 1;

        // Normalize/canonicalize path for robust deduplication across platforms.
        let canonical = std::fs::canonicalize(&item.path).unwrap_or(item.path.clone());
        #[cfg(windows)]
        let path_str = canonical
            .to_string_lossy()
            .to_lowercase()
            .replace('\\', "/");
        #[cfg(not(windows))]
        let path_str = canonical.to_string_lossy().to_string();

        if !visited.insert(path_str.clone()) {
            continue;
        }

        // Safety cap to avoid runaway processing and possible stack/exhaustion scenarios
        if iteration > safety_cap {
            break;
        }

        // Digest file contents if available
        if let Ok(bytes) = fs::read(&item.path) {
            let d = digest_bytes(&bytes);
            digests.push((path_str.clone(), d));

            // Parse to discover further imports
            if let Ok(content_str) = String::from_utf8(bytes) {
                let path_for_thread = item.path.clone();
                let content_for_thread = content_str.clone();
                let parse_spawn = std::thread::Builder::new()
                    .name("veil-digest-parse".into())
                    .stack_size(16 * 1024 * 1024)
                    .spawn(move || {
                        let mut tmp_files = Files::<String>::new();
                        let tfid = tmp_files.add(
                            path_for_thread.to_string_lossy().to_string(),
                            content_for_thread,
                        );
                        veil_syntax::parse_ast(&tmp_files, tfid)
                    });

                match parse_spawn {
                    Ok(handle) => {
                        match handle.join() {
                            Ok(parse_result) => {
                                if let Ok(parsed) = parse_result {
                                    // Resolve imports from this module, using this module's path as base
                                    if let Ok(next_level) =
                                        resolve_imports_only(&parsed.imports, &item.path)
                                    {
                                        for n in next_level {
                                            queue.push_back(n);
                                        }
                                    }
                                } else {
                                    // Parsing returned errors; skip transitive resolution for this module.
                                }
                            }
                            Err(_) => {
                                // Thread panicked while parsing; log and continue.
                            }
                        }
                    }
                    Err(_e) => {
                        // Failed to spawn parser thread; log and continue.
                    }
                }
            }
        }
    }

    // Stable ordering by path for deterministic key composition
    digests.sort_by(|a, b| a.0.cmp(&b.0));
    digests.into_iter().map(|(_, d)| d).collect()
}

/// M9: Cached IR build pass (used by process_build)
#[derive(Debug, Clone)]
struct BuildIrInput {
    entry: PathBuf,
    dump_norm_hir: bool,
    verbose: bool,
}

#[derive(Debug, Default)]
struct BuildIrPass;

impl Pass for BuildIrPass {
    const NAME: &'static str = "build-ir";

    type Input = BuildIrInput;
    type Output = ir::ProgramIR;

    fn run(&self, input: &Self::Input, _cx: &mut PassCx) -> Result<Self::Output> {
        let BuildIrInput {
            entry,
            dump_norm_hir,
            verbose,
        } = input.clone();

        // Local codespan files for parsing and diagnostics mapping
        let mut files = Files::<String>::new();

        // Parse entry file
        let entry_content = fs::read_to_string(&entry)
            .with_context(|| format!("Failed to read input file {}", entry.display()))?;
        let file_id = files.add(entry.to_string_lossy().to_string(), entry_content);

        // Parse AST
        let (mut program, parse_warnings) = veil_syntax::parse_ast_with_warnings(&files, file_id)
            .map_err(|_diags| {
            anyhow::anyhow!(
                "{}:{}: {}",
                entry.to_string_lossy().replace("\\\\?\\", ""),
                0,
                "Parse error"
            )
        })?;
        if verbose && !parse_warnings.is_empty() {
            let writer = StandardStream::stderr(ColorChoice::Auto);
            let config = term::Config::default();
            for diag in &parse_warnings {
                let _ = term::emit(&mut writer.lock(), &config, &files, diag);
            }
        }

        // Ensure prelude import unless compiling prelude itself
        let is_prelude_module = entry.to_string_lossy().ends_with("prelude.veil");
        let has_prelude_import = program.imports.iter().any(|import| match import {
            ast::ImportDeclaration::ImportAll { module_path, .. } => {
                module_path == "std::prelude" || module_path == "std/prelude"
            }
            ast::ImportDeclaration::ExportImportAll { module_path, .. } => {
                module_path == "std::prelude" || module_path == "std/prelude"
            }
            _ => false,
        });
        if !is_prelude_module && !has_prelude_import {
            let prelude_import = ast::ImportDeclaration::ImportAll {
                module_path: "std::prelude".to_string(),
                module_type: ast::ModuleType::Standard,
                alias: None,
            };
            program.imports.insert(0, prelude_import);
        }

        // Merge imported public items into root program
        merge_imports_into_program(&mut program, &entry, &mut files)?;

        // HIR: lower, resolve, typecheck, normalize
        let module_id = hir::ids::ModuleId::new(0);
        let mut hir_program = hir::lower_program(&program, module_id)
            .map_err(|e| anyhow::anyhow!("HIR lowering failed: {}", e))?;

        // Name/module/visibility resolution on HIR
        let resolver_ctx = resolve::resolve_program(&mut hir_program)
            .map_err(|_errs| anyhow::anyhow!("Resolution failed"))?;

        // Typecheck
        let mut hir_type_checker =
            typeck::TypeChecker::new(resolver_ctx.symbol_table.clone(), file_id, Some(module_id));
        if let Err(_errors) = hir_type_checker.check_program(&mut hir_program) {
            return Err(anyhow::anyhow!("Type check failed"));
        }

        // Optional normalized HIR dump (side-effect only controlled by flags)
        if verbose || dump_norm_hir {
            normalize::normalize_program(&mut hir_program);
        } else {
            normalize::normalize_program(&mut hir_program);
        }

        // M7 (current pipeline kept): monomorphize at AST level (compat)
        let mono = veil_mono::Monomorphizer::new(Default::default());
        let (program_mono, _meta) = mono
            .monomorphize_with_metadata(&program)
            .map_err(|e| anyhow::anyhow!("Monomorphization failed: {}", e))?;

        // Lower mono AST → HIR → normalize → IR
        let module_id = hir::ids::ModuleId::new(0);
        let mut mono_hir = hir::lower_program(&program_mono, module_id)
            .map_err(|e| anyhow::anyhow!("IR lowering (HIR) failed: {}", e))?;
        normalize::normalize_program(&mut mono_hir);
        let program_ir = ir::lower_from_hir(&mono_hir);

        Ok(program_ir)
    }
}

/// Public: process a build end-to-end, returning the final output path (exe or C file when skip_cc).
pub fn process_build(
    input: PathBuf,
    output: PathBuf,
    optimize: bool,
    target_triple: String,
    verbose: bool,
    dump_norm_hir: bool,
    pass_timings: bool,
    cache_stats: bool,
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
    let file_id = files.add(input.to_string_lossy().to_string(), entry_content.clone());

    let (mut program, parse_warnings) = match veil_syntax::parse_ast_with_warnings(&files, file_id)
    {
        Ok(x) => x,
        Err(diags) => {
            let writer = StandardStream::stderr(ColorChoice::Auto);
            let config = term::Config::default();
            eprintln!("=== PARSE ERRORS ===");
            eprintln!("Found {} errors", diags.len());
            for d in &diags {
                let _ = term::emit(&mut writer.lock(), &config, &files, d);
            }
            let clean_path = input.to_string_lossy().replace("\\\\?\\", "");
            return Err(anyhow!("Parse failed for {}", clean_path));
        }
    };
    if verbose && !parse_warnings.is_empty() {
        let writer = StandardStream::stderr(ColorChoice::Auto);
        let config = term::Config::default();
        for diag in &parse_warnings {
            let _ = term::emit(&mut writer.lock(), &config, &files, diag);
        }
    }

    // Ensure prelude import unless compiling prelude itself
    let is_prelude_module = input.to_string_lossy().ends_with("prelude.veil");
    let has_prelude_import = program.imports.iter().any(|import| match import {
        ast::ImportDeclaration::ImportAll { module_path, .. } => {
            module_path == "std::prelude" || module_path == "std/prelude"
        }
        ast::ImportDeclaration::ExportImportAll { module_path, .. } => {
            module_path == "std::prelude" || module_path == "std/prelude"
        }
        _ => false,
    });
    if !is_prelude_module && !has_prelude_import {
        let prelude_import = ast::ImportDeclaration::ImportAll {
            module_path: "std::prelude".to_string(),
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

    let mut pm =
        PassManager::with_fs_cache(&build_dir, veil_compiler::default_fingerprint(), verbose);
    let mut pcx = PassCx::new(&build_dir, verbose);

    // Derive cache key from entry content, resolved imports, and build knobs
    let entry_digest = digest_bytes(entry_content.as_bytes());
    let dep_digests: Vec<String> =
        compute_transitive_import_digests(&program.imports, &input, &mut files);
    let mut parts: Vec<String> = Vec::new();
    parts.push(entry_digest);
    parts.extend(dep_digests);
    parts.push(format!("opt:{}", optimize));
    parts.push(format!("triple:{}", target_triple));
    let parts_refs: Vec<&str> = parts.iter().map(|s| s.as_str()).collect();
    let key_seed = digest_strs(&parts_refs);

    let build_input = BuildIrInput {
        entry: input.clone(),
        dump_norm_hir,
        verbose,
    };

    // Run the cached pass inside a dedicated thread with an increased stack size.
    // We move ownership of the PassManager and PassCx into the thread and retrieve
    // them back after the run so the outer scope can continue using them.
    let build_input_clone = build_input.clone();
    let key_seed_clone = key_seed.as_bytes().to_vec();
    let spawn_res = std::thread::Builder::new()
        .name("veil-pass-run".into())
        .stack_size(16 * 1024 * 1024)
        .spawn(move || {
            let run_result = pm.run_cached(
                &BuildIrPass,
                key_seed_clone.as_slice(),
                &build_input_clone,
                &mut pcx,
            );
            (run_result, pm, pcx)
        });

    let (run_result, _pm, mut pcx) = match spawn_res {
        Ok(handle) => match handle.join() {
            Ok(tuple) => tuple,
            Err(_) => {
                // Thread panicked; fall back to computing IR from AST in this thread.
                // pass thread panicked, falling back
                let fallback = ir::lower_from_ast(&program);
                // reconstruct variables for continued use (we cannot recover pm/pcx here)
                // Create a noop PassManager and PassCx for subsequent code to use conservatively.
                let pm = PassManager::with_fs_cache(
                    &build_dir,
                    veil_compiler::default_fingerprint(),
                    verbose,
                );
                let pcx = PassCx::new(&build_dir, verbose);
                (Ok(fallback), pm, pcx)
            }
        },
        Err(_e) => {
            // failed to spawn pass thread, falling back
            // Spawn failed: fall back synchronously
            let fallback = ir::lower_from_ast(&program);
            let pm = PassManager::with_fs_cache(
                &build_dir,
                veil_compiler::default_fingerprint(),
                verbose,
            );
            let pcx = PassCx::new(&build_dir, verbose);
            (Ok(fallback), pm, pcx)
        }
    };

    let program_ir = match run_result {
        Ok(ir) => ir,
        Err(_) => ir::lower_from_ast(&program),
    };

    let stats = pcx.take_stats();
    if verbose || pass_timings || cache_stats {
        for s in &stats {
            if pass_timings && cache_stats {
                println!("[{}] {:?} ({} ms)", s.pass, s.cache, s.duration.as_millis());
            } else if pass_timings {
                println!("[{}] ({} ms)", s.pass, s.duration.as_millis());
            } else if cache_stats {
                println!("[{}] {:?}", s.pass, s.cache);
            } else {
                println!("[{}]", s.pass);
            }
        }

        // Emit JSON for CI diffing
        let stats_path = build_dir.join("pass-stats.json");
        let payload = serde_json::json!({
            "fingerprint": veil_compiler::default_fingerprint(),
            "input": input.to_string_lossy(),
            "optimize": optimize,
            "target_triple": target_triple,
            "timestamp": chrono::Utc::now().to_rfc3339(),
            "stats": stats,
        });
        match serde_json::to_vec_pretty(&payload) {
            Ok(bytes) => {
                if let Err(e) = std::fs::write(&stats_path, bytes) {
                    if verbose {
                        eprintln!("Failed to write pass stats: {}", e);
                    }
                } else if verbose {
                    println!(
                        "{}",
                        format!("Pass stats saved to: {}", stats_path.display()).green()
                    );
                }
            }
            Err(e) => {
                if verbose {
                    eprintln!("Failed to serialize pass stats: {}", e);
                }
            }
        }
    }

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
