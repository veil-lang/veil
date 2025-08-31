use anyhow::Result;
use codespan::{FileId, Files};
use std::path::{Path, PathBuf};

pub fn extract_line_col_from_error(
    files: &Files<String>,
    file_id: FileId,
    error: &codespan_reporting::diagnostic::Diagnostic<FileId>,
) -> String {
    if let Some(label) = error.labels.first() {
        let source = files.source(file_id);
        let mut line_num = 1u32;
        let mut col_num = 1u32;

        for (idx, ch) in source.char_indices() {
            if idx >= label.range.start {
                break;
            }
            if ch == '\n' {
                line_num += 1;
                col_num = 1;
            } else {
                col_num += 1;
            }
        }
        format!("{}:{}", line_num, col_num)
    } else {
        "1:1".to_string()
    }
}

pub fn format_parse_error(
    files: &Files<String>,
    file_id: FileId,
    error: &codespan_reporting::diagnostic::Diagnostic<FileId>,
    file_path: &Path,
) -> String {
    let mut output = String::new();

    let file_name = file_path
        .file_name()
        .and_then(|n| n.to_str())
        .unwrap_or("unknown");

    output.push_str(&format!("\x1b[1;31merror\x1b[0m: {}\n", error.message));

    if let Some(label) = error.labels.first() {
        let range = label.range.clone();

        let source = files.source(file_id);
        let lines: Vec<&str> = source.lines().collect();

        let mut line_num = 1u32;
        let mut col_num = 1u32;

        for (idx, ch) in source.char_indices() {
            if idx >= range.start {
                break;
            }
            if ch == '\n' {
                line_num += 1;
                col_num = 1;
            } else {
                col_num += 1;
            }
        }

        output.push_str(&format!(
            " \x1b[1;34m-->\x1b[0m {}:{}:{}\n",
            file_name, line_num, col_num
        ));

        let line_idx = (line_num.saturating_sub(1)) as usize;

        if line_idx < lines.len() {
            let line_content = lines[line_idx];
            let col_start = (col_num.saturating_sub(1)) as usize;

            let mut end_col = col_start + 1;
            if range.end > range.start {
                let error_length = (range.end - range.start).min(line_content.len() - col_start);
                end_col = col_start + error_length;
            }

            output.push_str(&format!(" \x1b[1;34m{:4} |\x1b[0m\n", ""));
            output.push_str(&format!(
                " \x1b[1;34m{:4} |\x1b[0m {}\n",
                line_num, line_content
            ));

            output.push_str(&format!(" \x1b[1;34m{:4} |\x1b[0m ", ""));

            for _ in 0..col_start {
                output.push(' ');
            }

            let indicator_len = if end_col > col_start {
                end_col - col_start
            } else {
                1
            };

            output.push_str("\x1b[1;31m");
            for i in 0..indicator_len {
                if i == 0 {
                    output.push('^');
                } else {
                    output.push('~');
                }
            }
            output.push_str("\x1b[0m");

            if !label.message.is_empty() {
                output.push_str(&format!(" \x1b[1;31m{}\x1b[0m", label.message));
            }

            output.push('\n');
        }
    }

    for note in &error.notes {
        output.push_str(&format!(" \x1b[1;36m= note:\x1b[0m {}\n", note));
    }

    output
}

pub fn print_parse_error(
    files: &Files<String>,
    file_id: FileId,
    error: &codespan_reporting::diagnostic::Diagnostic<FileId>,
    file_path: &Path,
) {
    let formatted = format_parse_error(files, file_id, error, file_path);
    eprint!("{}", formatted);
}

pub fn validate_ve_file(path: &str) -> std::result::Result<PathBuf, String> {
    let path = Path::new(path);
    let path = if path.extension().is_none() {
        path.with_extension("veil")
    } else {
        path.to_path_buf()
    };

    if !path.exists() {
        let suggestions = suggest_similar_files(&path.clone())
            .map(|s| format!("\nDid you mean:\n{}", s))
            .unwrap_or_default();

        return Err(format!(
            "File '{}' not found.{}",
            path.display(),
            suggestions
        ));
    }
    Ok(path)
}

fn suggest_similar_files(missing_path: &Path) -> Option<String> {
    let dir = missing_path.parent()?;
    let target_name = missing_path.file_stem()?.to_string_lossy();
    let target_name = target_name.as_ref();

    let matches: Vec<_> = dir
        .read_dir()
        .ok()?
        .filter_map(|entry| {
            let path = entry.ok()?.path();
            let name = path.file_stem()?.to_string_lossy();
            (name.contains(target_name) && path.extension() == Some("veil".as_ref()))
                .then_some(format!("  • {}", path.display()))
        })
        .collect();
    (!matches.is_empty()).then(|| matches.join("\n"))
}
#[cfg(target_os = "windows")]
pub fn prepare_windows_clang_args(
    output: &Path,
    optimize: bool,
    c_file: &Path,
) -> Result<Vec<String>> {
    let opt_flag = if optimize { "-O3" } else { "-O0" }.to_string();

    let exe_dir = std::env::current_exe()?.parent().unwrap().to_path_buf();
    let sysroot = exe_dir.join("tools").join("windows-x64").join("llvm-mingw");
    let force_llvm_mingw = std::env::var("VEIL_FORCE_LLVM_MINGW").ok().as_deref() == Some("1");

    if force_llvm_mingw || sysroot.exists() {
        let mut clang_args = vec![
            opt_flag.clone(),
            "-pipe".to_string(),
            "-fno-exceptions".to_string(),
            "-fuse-ld=lld".to_string(),
            "-target".to_string(),
            "x86_64-w64-windows-gnu".to_string(),
            c_file.to_str().unwrap().into(),
            "-o".to_string(),
            output.to_str().unwrap().into(),
            "--sysroot".to_string(),
            sysroot.to_string_lossy().to_string(),
        ];
        if std::env::var("VEIL_CLANG_VERBOSE").ok().as_deref() == Some("1") {
            clang_args.insert(0, "-v".to_string());
        }
        if let Ok(extra) = std::env::var("VEIL_EXTRA_CLANG_ARGS") {
            let mut split: Vec<String> = shell_words::split(&extra)
                .unwrap_or_default()
                .into_iter()
                .collect();
            clang_args.splice(1..1, split.drain(..));
        }
        return Ok(clang_args);
    }

    let prefer = std::env::var("VEIL_WINDOWS_TOOLCHAIN").unwrap_or_default();
    let have_link = which::which("link.exe").is_ok();
    let have_lld_link = which::which("lld-link").is_ok()
        || std::path::Path::new(r"C:\Program Files\LLVM\bin\lld-link.exe").exists();
    let have_mingw = which::which("x86_64-w64-mingw32-clang").is_ok()
        || which::which("x86_64-w64-mingw32-gcc").is_ok();

    let msvc_libs = discover_msvc_lib_paths();

    let mut args = vec![
        opt_flag,
        c_file.to_str().unwrap().into(),
        "-o".to_string(),
        output.to_str().unwrap().into(),
    ];

    let use_msvc = match prefer.as_str() {
        "msvc" => true,
        "gnu" => false,
        _ => !msvc_libs.is_empty() || have_link || have_lld_link,
    };
    let use_gnu = match prefer.as_str() {
        "gnu" => true,
        "msvc" => false,
        _ => !use_msvc && have_mingw,
    };

    if use_msvc {
        args.insert(1, "-target".to_string());
        args.insert(2, "x86_64-pc-windows-msvc".to_string());
        args.insert(
            1,
            if have_lld_link {
                "-fuse-ld=lld-link".to_string()
            } else {
                "-fuse-ld=link".to_string()
            },
        );
        for lib_path in msvc_libs {
            args.push("-Xlinker".to_string());
            args.push(format!("/LIBPATH:{}", lib_path.to_string_lossy()));
        }
    } else {
        args.insert(1, "-target".to_string());
        args.insert(2, "x86_64-w64-windows-gnu".to_string());
        if which::which("ld.lld").is_ok() || which::which("lld").is_ok() {
            args.insert(1, "-fuse-ld=lld".to_string());
        }
    }

    if std::env::var("VEIL_CLANG_VERBOSE").ok().as_deref() == Some("1") {
        args.insert(1, "-v".to_string());
    }
    if let Ok(extra) = std::env::var("VEIL_EXTRA_CLANG_ARGS") {
        let mut split: Vec<String> = shell_words::split(&extra)
            .unwrap_or_default()
            .into_iter()
            .collect();
        args.splice(1..1, split.drain(..));
    }

    Ok(args)
}

pub fn get_bundled_clang_path() -> Result<PathBuf> {
    if let Ok(custom) = std::env::var("VEIL_CLANG_PATH") {
        let p = PathBuf::from(custom);
        if p.exists() {
            return Ok(p);
        }
    }
    if std::env::var("VEIL_USE_SYSTEM_CLANG").ok().as_deref() == Some("1")
        && let Ok(p) = which::which("clang") {
            return Ok(p);
        }

    let exe_dir = std::env::current_exe()?.parent().unwrap().to_path_buf();

    let platform = if cfg!(target_os = "windows") {
        "windows-x64"
    } else if cfg!(target_os = "macos") {
        "macos-x64"
    } else {
        "linux-x64"
    };

    let clang_name = if cfg!(windows) { "clang.exe" } else { "clang" };

    if cfg!(target_os = "windows") {
        let bin_dir = exe_dir
            .join("tools")
            .join(platform)
            .join("llvm-mingw")
            .join("bin");
        let wrapper = bin_dir.join("x86_64-w64-mingw32-clang.exe");
        if wrapper.exists() {
            return Ok(wrapper);
        }
        let mingw_path = bin_dir.join(clang_name);
        if mingw_path.exists() {
            return Ok(mingw_path);
        }
    }

    let bundled = exe_dir.join("tools").join(platform).join(clang_name);
    if bundled.exists() {
        return Ok(bundled);
    }

    which::which("clang").map_err(|_| anyhow::anyhow!("No C compiler found"))
}

#[cfg(target_os = "windows")]
fn discover_msvc_lib_paths() -> Vec<PathBuf> {
    let mut paths = Vec::new();

    if let Ok(vctools) = std::env::var("VCToolsInstallDir") {
        let base = PathBuf::from(vctools);
        let host = base.join("lib").join("x64");
        if host.exists() {
            paths.push(host);
        }
        let atlmfc = base.join("atlmfc").join("lib").join("x64");
        if atlmfc.exists() {
            paths.push(atlmfc);
        }
    }
    if let (Ok(sdkdir), Ok(sdkver)) = (
        std::env::var("WindowsSdkDir"),
        std::env::var("WindowsSDKLibVersion"),
    ) {
        let base = PathBuf::from(sdkdir).join("Lib").join(sdkver);
        for sub in ["ucrt", "um"] {
            let p = base.join(sub).join("x64");
            if p.exists() {
                paths.push(p);
            }
        }
    }

    if paths.is_empty() {
        let vs_base =
            PathBuf::from(r"C:\Program Files\Microsoft Visual Studio\2022\Community\VC\Tools\MSVC");
        if let Ok(read_dir) = fs::read_dir(&vs_base) {
            let mut versions: Vec<PathBuf> =
                read_dir.filter_map(|e| e.ok().map(|e| e.path())).collect();
            versions.sort();
            if let Some(latest) = versions.into_iter().last() {
                let lib_host = latest.join("lib").join("x64");
                if lib_host.exists() {
                    paths.push(lib_host);
                }
                let atlmfc = latest.join("atlmfc").join("lib").join("x64");
                if atlmfc.exists() {
                    paths.push(atlmfc);
                }
            }
        }
        let sdk_base = PathBuf::from(r"C:\Program Files (x86)\Windows Kits\10\Lib");
        if let Ok(read_dir) = fs::read_dir(&sdk_base) {
            let mut versions: Vec<PathBuf> =
                read_dir.filter_map(|e| e.ok().map(|e| e.path())).collect();
            versions.sort();
            if let Some(latest) = versions.into_iter().last() {
                for sub in ["ucrt", "um"] {
                    let p = latest.join(sub).join("x64");
                    if p.exists() {
                        paths.push(p);
                    }
                }
            }
        }
    }

    paths.sort();
    paths.dedup();
    paths
}
