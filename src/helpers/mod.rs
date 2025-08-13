use anyhow::Result;
use codespan::{FileId, Files};
use codespan_reporting;
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
    

    let file_name = file_path.file_name()
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
            file_name,
            line_num,
            col_num
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
            output.push_str(&format!(" \x1b[1;34m{:4} |\x1b[0m {}\n", line_num, line_content));
            

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
        path.with_extension("ve")
    } else {
        path.to_path_buf()
    };

    if !path.exists() {
        let suggestions = suggest_similar_files(&*path.clone())
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
            (name.contains(target_name) && path.extension() == Some("ve".as_ref()))
                .then_some(format!("  • {}", path.display()))
        })
        .collect();    (!matches.is_empty()).then(|| matches.join("\n"))
}

#[cfg(target_os = "windows")]
pub fn prepare_windows_clang_args(
    output: &Path,
    optimize: bool,
    c_file: &Path,
) -> Result<Vec<String>> {
    // Use a fully self-contained llvm-mingw toolchain. No MSVC/Build Tools.
    let mut clang_args = vec![
        if optimize { "-O3" } else { "-O0" }.to_string(),
        "-pipe".to_string(),
        "-fno-exceptions".to_string(),
        "-fuse-ld=lld".to_string(),
        "-target".to_string(),
        "x86_64-w64-windows-gnu".to_string(),
        c_file.to_str().unwrap().into(),
        "-o".to_string(),
        output.to_str().unwrap().into(),
    ];

    // Point clang at the bundled sysroot explicitly to avoid depending on system state
    let exe_dir = std::env::current_exe()?.parent().unwrap().to_path_buf();
    let sysroot = exe_dir
        .join("tools")
        .join("windows-x64")
        .join("llvm-mingw");
    if sysroot.exists() {
        clang_args.push("--sysroot".to_string());
        clang_args.push(sysroot.to_string_lossy().to_string());
    }

    Ok(clang_args)
}

pub fn get_bundled_clang_path() -> Result<PathBuf> {
    let exe_dir = std::env::current_exe()?.parent().unwrap().to_path_buf();

    let platform = if cfg!(target_os = "windows") {
        "windows-x64"
    } else if cfg!(target_os = "macos") {
        "macos-x64"
    } else {
        "linux-x64"
    };

    let clang_name = if cfg!(windows) { "clang.exe" } else { "clang" };

    // Prefer llvm-mingw layout first (self-contained sysroot)
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

    // Fallback to flat layout
    let bundled = exe_dir.join("tools").join(platform).join(clang_name);
    if bundled.exists() {
        return Ok(bundled);
    }

    which::which("clang").map_err(|_| anyhow::anyhow!("No C compiler found"))
}


// Removed legacy MSVC discovery: we fully rely on bundled llvm-mingw toolchain
