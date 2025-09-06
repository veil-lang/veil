#![forbid(unsafe_code)]
#![deny(rust_2018_idioms)]
#![deny(unused_must_use)]

//! Upgrade subcommand for package management (placeholder)
//!
//! This module will handle upgrading ve packages and dependencies in the future.
//! Currently it provides a clear message about the difference between `update` and `upgrade`.

use crate::Channel;
use anyhow::{Result, anyhow};

/// Execute the upgrade flow for package management.
///
/// This command is intended for upgrading ve packages and dependencies,
/// not the toolchain itself. For toolchain updates, use `ve update`.
///
/// Arguments:
/// - `no_remind`: Disable future upgrade reminders (planned feature).
/// - `force`: Proceed without confirmation (planned feature).
/// - `verbose`: Print extra information (planned feature).
/// - `channel`: Selected update channel for packages (planned feature).
pub fn run_upgrade(_no_remind: bool, _force: bool, verbose: bool, _channel: Channel) -> Result<()> {
    if verbose {
        println!("Package upgrade system not yet implemented.");
    }

    Err(anyhow!(
        "The 'upgrade' command for package management is not yet implemented.\n\
         \n\
         Command usage clarification:\n\
         • 've update'   - Update the ve toolchain itself\n\
         • 've upgrade'  - Upgrade ve packages (this command, coming soon)\n\
         \n\
         For now, to update the ve toolchain:\n\
         1. Use 've update' to automatically update the toolchain\n\
         2. Or manually download from https://github.com/veil-lang/veil/releases\n\
         \n\
         Package management features will be available in a future release."
    ))
}

// Package management functions will be added in future releases
// when the package system specification is complete.
