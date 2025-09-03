//! Veil Compiler - Pass Manager and Cache Scaffold (M9)
//!
//! This crate provides the initial pass manager abstraction and pluggable cache
//! interfaces for the pass-based pipeline:
//!
//!   Source → Parse(AST) → Lower(HIR) → Resolve → TypeCheck → Normalize
//!   → Monomorphize → IR → Codegen
//!
//! Design goals:
//! - Strongly-typed passes with explicit Input/Output
//! - Deterministic caching keyed by strong hashes and a version fingerprint
//! - Minimal runtime deps; future integration with existing crates lives in the
//!   binary crate or dedicated drivers
//!
//! Notes:
//! - This is an initial scaffold: no concrete passes are implemented here.
//! - Pass outputs can be cached when they implement serde Serialize/Deserialize.
//! - The cache interface is generic; a filesystem-backed cache is provided.

#![forbid(unsafe_code)]
#![deny(rust_2018_idioms)]
#![deny(unused_must_use)]
#![allow(clippy::needless_return)]

use anyhow::{Context, Result};
use codespan::Files;
use serde::de::DeserializeOwned;
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::fmt::Debug;
use std::fs;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::time::{Duration, Instant};

/// Execution context shared by passes.
///
/// Keep this intentionally small and clonable/portable. Larger state should be
/// maintained in higher-level orchestration code.
#[derive(Debug)]
pub struct PassCx {
    pub files: Files<String>,
    pub build_dir: PathBuf,
    pub verbose: bool,
    stats: Vec<PassStat>,
}

impl PassCx {
    pub fn new(build_dir: impl Into<PathBuf>, verbose: bool) -> Self {
        Self {
            files: Files::new(),
            build_dir: build_dir.into(),
            verbose,
            stats: Vec::new(),
        }
    }

    pub fn log(&self, msg: impl AsRef<str>) {
        if self.verbose {
            println!("{}", msg.as_ref());
        }
    }

    fn push_stat(&mut self, stat: PassStat) {
        self.stats.push(stat);
    }

    /// Returns a snapshot of accumulated stats and clears the internal buffer.
    pub fn take_stats(&mut self) -> Vec<PassStat> {
        std::mem::take(&mut self.stats)
    }
}

/// Basic statistics gathered per pass invocation.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PassStat {
    pub pass: String,
    pub duration: Duration,
    pub cache: CacheStatus,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
pub enum CacheStatus {
    Hit,
    Miss,
    Disabled,
}

/// Trait implemented by each compiler pass.
pub trait Pass: Send + Sync + 'static {
    /// Human-readable stable name of the pass (used by cache namespaces).
    const NAME: &'static str;

    /// Input type consumed by this pass.
    type Input: Debug;

    /// Output type produced by this pass.
    type Output: Debug;
    /// Run the pass on the given input producing an output.
    fn run(&self, input: &Self::Input, cx: &mut PassCx) -> Result<Self::Output>;
}

/// Pluggable cache interface for pass artifacts.
pub trait PassCache: Send + Sync {
    /// Retrieve a cached artifact (raw bytes) for pass + key.
    fn get(&self, pass: &str, key: &str) -> Option<Vec<u8>>;

    /// Store a cached artifact.
    fn put(&self, pass: &str, key: &str, bytes: &[u8]) -> Result<()>;

    /// Invalidate a cached artifact.
    fn invalidate(&self, pass: &str, key: &str) -> Result<()>;
}

/// Filesystem-backed cache implementation.
///
/// Layout:
///   <root>/.cache/passes/<pass>/<key>.bin
#[derive(Debug, Clone)]
pub struct FsPassCache {
    root: PathBuf,
}

impl FsPassCache {
    pub fn new(root_dir: impl Into<PathBuf>) -> Self {
        Self {
            root: root_dir.into(),
        }
    }

    fn bucket_dir(&self, pass: &str) -> PathBuf {
        self.root.join(".cache").join("passes").join(pass)
    }

    fn entry_path(&self, pass: &str, key: &str) -> PathBuf {
        self.bucket_dir(pass).join(format!("{key}.bin"))
    }
}

impl PassCache for FsPassCache {
    fn get(&self, pass: &str, key: &str) -> Option<Vec<u8>> {
        let path = self.entry_path(pass, key);
        match fs::read(&path) {
            Ok(bytes) => Some(bytes),
            Err(_) => None,
        }
    }

    fn put(&self, pass: &str, key: &str, bytes: &[u8]) -> Result<()> {
        let dir = self.bucket_dir(pass);
        fs::create_dir_all(&dir)
            .with_context(|| format!("Failed to create cache dir: {}", dir.display()))?;
        let path = self.entry_path(pass, key);
        let mut f = fs::File::create(&path)
            .with_context(|| format!("Failed to create cache file: {}", path.display()))?;
        f.write_all(bytes)?;
        Ok(())
    }

    fn invalidate(&self, pass: &str, key: &str) -> Result<()> {
        let path = self.entry_path(pass, key);
        let _ = fs::remove_file(&path);
        Ok(())
    }
}

/// A null cache implementation that always misses.
#[derive(Debug, Default, Clone, Copy)]
pub struct NoopCache;

impl PassCache for NoopCache {
    fn get(&self, _pass: &str, _key: &str) -> Option<Vec<u8>> {
        None
    }
    fn put(&self, _pass: &str, _key: &str, _bytes: &[u8]) -> Result<()> {
        Ok(())
    }
    fn invalidate(&self, _pass: &str, _key: &str) -> Result<()> {
        Ok(())
    }
}

/// PassManager orchestrates pass execution and caching.
pub struct PassManager {
    cache: Box<dyn PassCache>,
    fingerprint: String,
    verbose: bool,
}

impl Debug for PassManager {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("PassManager")
            .field("cache", &"<dyn PassCache>")
            .field("fingerprint", &self.fingerprint)
            .field("verbose", &self.verbose)
            .finish()
    }
}

impl PassManager {
    /// Create a new PassManager with the given cache and version fingerprint.
    ///
    /// The fingerprint should change when:
    /// - The grammar or parser semantics change
    /// - Pass algorithms change in a way that affects outputs
    /// - Toolchain or target ABI changes require recomputation
    pub fn new(cache: Box<dyn PassCache>, fingerprint: impl Into<String>, verbose: bool) -> Self {
        Self {
            cache,
            fingerprint: fingerprint.into(),
            verbose,
        }
    }

    /// Convenience constructor for FS cache rooted at `build_dir`.
    pub fn with_fs_cache(
        build_dir: impl Into<PathBuf>,
        fingerprint: impl Into<String>,
        verbose: bool,
    ) -> Self {
        let cache = Box::new(FsPassCache::new(build_dir.into()));
        Self::new(cache, fingerprint, verbose)
    }

    /// Run a pass with caching. The `key_seed` should be stable and derived from
    /// the pass inputs (e.g., content hashes, module paths, and config flags).
    ///
    /// Requirements for caching:
    /// - Output must be Serialize + DeserializeOwned
    pub fn run_cached<P>(
        &mut self,
        pass: &P,
        key_seed: impl AsRef<[u8]>,
        input: &P::Input,
        cx: &mut PassCx,
    ) -> Result<P::Output>
    where
        P: Pass,
        P::Output: Serialize + DeserializeOwned,
    {
        let pass_name = P::NAME;
        let key = self.derive_key(pass_name, key_seed.as_ref());

        // Cache probe
        let started = Instant::now();
        if let Some(bytes) = self.cache.get(pass_name, &key) {
            match bincode::deserialize::<P::Output>(&bytes) {
                Ok(artifact) => {
                    let dur = started.elapsed();
                    cx.push_stat(PassStat {
                        pass: pass_name.to_string(),
                        duration: dur,
                        cache: CacheStatus::Hit,
                    });
                    if self.verbose || cx.verbose {
                        println!("[{pass_name}] cache hit ({} ms)", dur.as_millis());
                    }
                    return Ok(artifact);
                }
                Err(_) => {
                    // Fall through and recompute; also invalidate corrupt entry
                    let _ = self.cache.invalidate(pass_name, &key);
                }
            }
        }

        // Miss: compute
        let compute_start = Instant::now();
        let output = pass.run(input, cx)?;
        let artifact_bytes = bincode::serialize(&output)
            .with_context(|| format!("Failed to serialize output for pass {pass_name}"))?;

        self.cache
            .put(pass_name, &key, &artifact_bytes)
            .with_context(|| format!("Failed to store cache for pass {pass_name}"))?;

        let dur = compute_start.elapsed();
        cx.push_stat(PassStat {
            pass: pass_name.to_string(),
            duration: dur,
            cache: CacheStatus::Miss,
        });
        if self.verbose || cx.verbose {
            println!("[{pass_name}] computed ({} ms)", dur.as_millis());
        }
        Ok(output)
    }

    /// Run a pass without caching (useful during initial bring-up).
    pub fn run_uncached<P>(
        &mut self,
        pass: &P,
        input: &P::Input,
        cx: &mut PassCx,
    ) -> Result<P::Output>
    where
        P: Pass,
    {
        let pass_name = P::NAME;
        let started = Instant::now();
        let out = pass.run(input, cx)?;
        let dur = started.elapsed();
        cx.push_stat(PassStat {
            pass: pass_name.to_string(),
            duration: dur,
            cache: CacheStatus::Disabled,
        });
        if self.verbose || cx.verbose {
            println!("[{pass_name}] executed ({} ms)", dur.as_millis());
        }
        Ok(out)
    }

    fn derive_key(&self, pass: &str, seed: &[u8]) -> String {
        // Include pass name and manager fingerprint in the hash domain separation.
        let mut hasher = Sha256::new();
        hasher.update(pass.as_bytes());
        hasher.update(0xFFu8.to_le_bytes());
        hasher.update(self.fingerprint.as_bytes());
        hasher.update(0xA5u8.to_le_bytes());
        hasher.update(seed);
        hex_encode(hasher.finalize())
    }

    /// Returns a stable, human-readable build fingerprint suitable for cache keys.
    ///
    /// Composition (when available via build-time env):
    ///   <pkg>@<ver>|<rustc>|<target>|<profile>|sha=<git_sha>,tag=<git_tag>|ts=<unix_ts>
    ///
    /// Notes:
    /// - Values are sourced from compile-time env variables populated by a build script
    ///   when present, and fall back to Cargo-provided values or "unknown".
    /// - This string is intended for inclusion in PassManager cache fingerprints to
    ///   force recomputation across toolchain or source tree changes.
    pub fn default_fingerprint() -> String {
        let name = option_env!("VEIL_COMPILER_PKG_NAME").unwrap_or(env!("CARGO_PKG_NAME"));
        let ver = option_env!("VEIL_COMPILER_PKG_VERSION").unwrap_or(env!("CARGO_PKG_VERSION"));
        let rustc = option_env!("VEIL_COMPILER_RUSTC_VERSION").unwrap_or("rustc-unknown");
        let target = option_env!("VEIL_COMPILER_TARGET")
            .or(option_env!("TARGET"))
            .unwrap_or("unknown");
        let profile = option_env!("VEIL_COMPILER_PROFILE").unwrap_or(env!("PROFILE"));
        let git_sha = option_env!("VEIL_COMPILER_GIT_SHA").unwrap_or("unknown");
        let git_tag = option_env!("VEIL_COMPILER_GIT_TAG").unwrap_or("-");
        let ts = option_env!("VEIL_COMPILER_BUILD_UNIX_TS").unwrap_or("0");
        format!(
            "{}@{}|{}|{}|{}|sha={},tag={}|ts={}",
            name, ver, rustc, target, profile, git_sha, git_tag, ts
        )
    }
}
/// Public re-export for ergonomic access without referencing PassManager directly.
/// Equivalent to PassManager::default_fingerprint().
pub fn default_fingerprint() -> String {
    PassManager::default_fingerprint()
}

/// Helper to compute a stable SHA-256 hex digest of a file on disk.
pub fn digest_file(path: &Path) -> Result<String> {
    let data = fs::read(path)
        .with_context(|| format!("Failed to read file for digest: {}", path.display()))?;
    Ok(digest_bytes(&data))
}

/// Helper to compute a stable SHA-256 hex digest for a byte slice.
pub fn digest_bytes(data: &[u8]) -> String {
    let mut hasher = Sha256::new();
    hasher.update(data);
    hex_encode(hasher.finalize())
}

/// Helper to compute a stable SHA-256 hex digest for a list of ordered strings.
pub fn digest_strs(parts: &[&str]) -> String {
    let mut hasher = Sha256::new();
    for p in parts {
        hasher.update((*p).as_bytes());
        hasher.update(0x1Fu8.to_le_bytes()); // delimiter
    }
    hex_encode(hasher.finalize())
}

fn hex_encode(bytes: impl AsRef<[u8]>) -> String {
    let b = bytes.as_ref();
    let mut s = String::with_capacity(b.len() * 2);
    const HEX: &[u8; 16] = b"0123456789abcdef";
    for &v in b {
        s.push(HEX[(v >> 4) as usize] as char);
        s.push(HEX[(v & 0x0F) as usize] as char);
    }
    s
}

/// Well-known pass names used by the pipeline orchestrator.
/// These are provided as constants to promote consistent cache namespaces.
pub mod passes {
    pub const PARSE_AST: &str = "parse-ast";
    pub const LOWER_HIR: &str = "lower-hir";
    pub const RESOLVE: &str = "resolve";
    pub const TYPECHECK: &str = "typecheck";
    pub const NORMALIZE: &str = "normalize";
    pub const MONOMORPHIZE: &str = "monomorphize";
    pub const LOWER_IR: &str = "lower-ir";
    pub const CODEGEN: &str = "codegen";
}

/// A tiny example pass used for sanity tests and initial wiring.
#[derive(Debug, Default)]
pub struct IdentityPass;

impl Pass for IdentityPass {
    const NAME: &'static str = "identity";

    type Input = Vec<u8>;
    type Output = Vec<u8>;

    fn run(&self, input: &Self::Input, _cx: &mut PassCx) -> Result<Self::Output> {
        Ok(input.clone())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn hex_encode_works() {
        assert_eq!(hex_encode([0x00u8, 0xFFu8]), "00ff");
    }

    #[test]
    fn digest_is_stable() {
        let d1 = digest_strs(&["a", "b", "c"]);
        let d2 = digest_strs(&["a", "b", "c"]);
        assert_eq!(d1, d2);
        assert_ne!(d1, digest_strs(&["a", "b", "d"]));
    }

    #[test]
    fn pass_manager_runs_and_caches() {
        let temp = tempfile::tempdir().unwrap();
        let mut cx = PassCx::new(temp.path(), true);
        let mut pm = PassManager::with_fs_cache(temp.path(), "fp-test-1", true);
        let pass = IdentityPass::default();

        let key_seed = b"hello";
        let input = b"payload".to_vec();

        // First run: miss
        let out1 = pm.run_cached(&pass, key_seed, &input, &mut cx).unwrap();
        assert_eq!(out1, input);

        // Second run: hit
        let out2 = pm.run_cached(&pass, key_seed, &input, &mut cx).unwrap();
        assert_eq!(out2, input);

        let stats = cx.take_stats();
        assert!(stats.iter().any(|s| s.cache == CacheStatus::Miss));
        assert!(stats.iter().any(|s| s.cache == CacheStatus::Hit));
    }
}
