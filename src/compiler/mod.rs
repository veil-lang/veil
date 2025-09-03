pub mod cache;
pub mod incremental;

use crate::ast;
use anyhow::{Result, anyhow};
use std::path::{Path, PathBuf};

#[derive(Debug, Clone)]
#[cfg_attr(test, allow(dead_code))]
pub struct ResolvedImport {
    pub path: PathBuf,
    #[allow(dead_code)]
    pub import_type: ImportType,
    #[allow(dead_code)]
    pub module_path: String,
}

#[derive(Debug, Clone)]
#[cfg_attr(test, allow(dead_code))]
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

pub fn resolve_standard_library_path(module_path: &str) -> Result<PathBuf> {
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

pub fn resolve_imports_only(
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

#[cfg(test)]
mod __debug_use_fields {
    use super::*;
    use std::path::PathBuf;

    #[test]
    fn touch_resolved_import_all_variant() {
        // Construct and read fields to silence “never read” warnings under tests
        let ri = ResolvedImport {
            path: PathBuf::from("dummy/path.veil"),
            import_type: ImportType::All {
                alias: Some("x".to_string()),
            },
            module_path: "std/dummy".to_string(),
        };
        // Read each field
        let _p = ri.path.to_string_lossy();
        let _m = &ri.module_path;
        if let ImportType::All { alias } = &ri.import_type {
            let _ = alias.as_deref();
        }
    }

    #[test]
    fn touch_resolved_import_specifiers_variant() {
        // Use Specifiers variant and read contained specifiers length
        let ri = ResolvedImport {
            path: PathBuf::from("dummy/other.veil"),
            import_type: ImportType::Specifiers {
                specifiers: Vec::new(),
            },
            module_path: "local/other".to_string(),
        };
        let _p = &ri.path;
        let _m = &ri.module_path;
        if let ImportType::Specifiers { specifiers } = &ri.import_type {
            let _len = specifiers.len();
        }
    }
}
