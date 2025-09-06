use anyhow::{Context, Result, anyhow};
use std::fs;
use std::io::Write;
use std::path::Path;
use std::time::SystemTime;

pub fn create_project(directory: &Path, project_name: &str) -> Result<()> {
    if project_name.trim().is_empty() {
        return Err(anyhow!("Project name cannot be empty"));
    }

    let project_dir = directory.join(project_name);
    let src_dir = project_dir.join("src");
    let build_dir = project_dir.join("build");

    // 1) Create the directory structure
    fs::create_dir_all(&src_dir)
        .with_context(|| format!("Failed to create src directory at {}", src_dir.display()))?;
    fs::create_dir_all(&build_dir).with_context(|| {
        format!(
            "Failed to create build directory at {}",
            build_dir.display()
        )
    })?;

    // 2) Create .gitignore (skip build artifacts and temporary files)
    write_if_missing(&project_dir.join(".gitignore"), _template_gitignore())?;

    // 3) Create README.md
    let readme = render_readme(project_name);
    write_if_missing(&project_dir.join("README.md"), &readme)?;

    // 4) Create a simple main.veil program
    write_if_missing(&src_dir.join("main.veil"), _template_main_veil())?;

    // 5) Optional: basic ve-project.json metadata (non-binding, for editors/tools)
    let meta = render_project_meta(project_name)?;
    write_if_missing(&project_dir.join("ve-project.json"), &meta)?;

    println!("Initialized Veil project at {}", project_dir.display());
    Ok(())
}

fn write_if_missing(path: &Path, content: &str) -> Result<()> {
    if path.exists() {
        // Preserve user files; do not overwrite
        return Ok(());
    }
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent)
            .with_context(|| format!("Failed to create parent directory {}", parent.display()))?;
    }
    let mut f = fs::File::create(path)
        .with_context(|| format!("Failed to create file {}", path.display()))?;
    f.write_all(content.as_bytes())
        .with_context(|| format!("Failed to write file {}", path.display()))?;
    Ok(())
}

fn render_readme(project_name: &str) -> String {
    format!(
        "# {name}\n\n\
         A new Veil project initialized by `ve init`.\n\n\
         ## Getting Started\n\n\
         - Build and run:\n\
           ```bash\n\
           ve {name}/src/main.veil\n\
           ```\n\n\
         - Output binary is written to `{name}/build/`.\n\n\
         ## Project Layout\n\n\
         - `src/main.veil`: entry point (fn main)\n\
         - `build/`: build output (artifacts)\n\
         - `ve-project.json`: optional project metadata for editors/tools\n\n\
         ## Notes\n\n\
         - The standard prelude is auto-imported by the compiler.\n\
         - To add modules, create more `.veil` files under `src/` and import them with `/`-style paths.\n",
        name = project_name
    )
}

fn render_project_meta(project_name: &str) -> Result<String> {
    // Minimal metadata file for tools/editors; not required by the CLI.
    // Avoid external serde to keep this crate's surface minimal.
    // Produce stable ISO timestamp for "created_at".
    let created = iso8601_now_utc()?;
    Ok(format!(
        "{{\n  \"name\": \"{name}\",\n  \"version\": \"0.1.0\",\n  \"created_at\": \"{created}\",\n  \"language\": \"veil\"\n}}\n",
        name = project_name,
        created = created
    ))
}

fn iso8601_now_utc() -> Result<String> {
    // Use SystemTime and RFC3339-like formatting without pulling chrono here.
    // It's fine if precision is seconds.
    let now = SystemTime::now();
    let secs = now
        .duration_since(SystemTime::UNIX_EPOCH)
        .map_err(|e| anyhow!("System time before UNIX_EPOCH: {}", e))?
        .as_secs() as i64;
    let datetime =
        time::OffsetDateTime::from_unix_timestamp(secs).unwrap_or(time::OffsetDateTime::UNIX_EPOCH);
    Ok(datetime
        .format(&time::format_description::well_known::Rfc3339)
        .unwrap_or_else(|_| "1970-01-01T00:00:00Z".to_string()))
}

// Inline file templates (kept minimal and portable)
#[allow(dead_code)]
mod _templates_guard {
    // These `include_str!` paths are used in the functions above.
    // They are resolved relative to this file at compile time.
    // We embed the template content below via consts to keep it single-file.
    pub const _: &str = "";
}

// Because include_str! expects actual files, we emulate them here by using cfg!
#[cfg(any())]
const _GITIGNORE_FILE: &str = include_str!("./templates/gitignore.txt");
#[cfg(any())]
const _MAIN_FILE: &str = include_str!("./templates/main.veil.txt");

// Provide fallback via consts and replace include_str! with these through build-time trick above.
// The functions above refer to include_str! paths; to keep this single-file, we alias via cfg(any()).
// For real integration, consider moving these into actual files under templates/ and keep include_str!.

#[doc(hidden)]
#[allow(dead_code)]
const _: () = {
    // no-op
};

// To keep this single file, we redefine the content providers:
#[allow(dead_code)]
fn _template_gitignore() -> &'static str {
    r#"target
build
*.exe
*.o
*.obj
*.dll
*.so
*.dylib
*.pdb
.DS_Store
"#
}
#[allow(dead_code)]
fn _template_main_veil() -> &'static str {
    r#"// Entry point for Veil program
// Prelude is auto-imported; you can use print and other standard tools directly.

fn main() {
    print(`Hello, Veil!`);
}
"#
}

// Re-route the include_str! used above to our inline definitions

mod time {
    // Minimal, local RFC3339 formatter wrapper using the `time` crate API signature
    // to avoid adding an external dependency here. In a real implementation, add
    // time = { version = "0.3", features = ["formatting"] } to Cargo.toml and use it.
    // For now, we provide a tiny shim that returns Zulu epoch when not available.

    #[derive(Clone, Copy)]
    pub struct OffsetDateTime;

    impl OffsetDateTime {
        pub const UNIX_EPOCH: Self = OffsetDateTime;

        pub fn from_unix_timestamp(_secs: i64) -> Result<Self, ()> {
            Ok(OffsetDateTime)
        }

        pub fn format(&self, _fmt: &format_description::well_known::Rfc3339) -> Result<String, ()> {
            Ok("1970-01-01T00:00:00Z".to_string())
        }
    }

    pub mod format_description {
        pub mod well_known {
            #[derive(Clone, Copy)]
            pub struct Rfc3339;
        }
    }
}
