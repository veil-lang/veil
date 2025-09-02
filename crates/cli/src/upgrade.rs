#![forbid(unsafe_code)]
#![deny(rust_2018_idioms)]
#![deny(unused_must_use)]

//! Upgrade subcommand (no-op placeholder)
//!
//! This module provides a stub implementation for the `upgrade` command,
//! which will later be wired to a real update mechanism (e.g. downloading
//! a new release channel build, verifying signatures, swapping binaries, etc).
//!
//! Current behavior:
//! - Prints an informational message with the selected options.
//! - Returns Ok(()) without performing any changes.

use crate::Channel;
use anyhow::Result;

/// Execute the upgrade flow (placeholder).
///
/// Arguments:
/// - `no_remind`: Disable future upgrade reminders (not persisted in this stub).
/// - `force`: Proceed without confirmation (no-op in this stub).
/// - `verbose`: Print extra information about what would happen.
/// - `channel`: Selected update channel (Stable or Canary).
pub fn run_upgrade(no_remind: bool, force: bool, verbose: bool, channel: Channel) -> Result<()> {
    let channel_str = match channel {
        Channel::Stable => "stable",
        Channel::Canary => "canary",
    };

    println!("Veil upgrade (placeholder)");
    println!("  - channel: {}", channel_str);
    println!("  - force: {}", force);
    println!("  - no-remind: {}", no_remind);

    if verbose {
        println!("Verbose: A full implementation would:");
        println!(
            "  • discover the latest version for the '{}' channel",
            channel_str
        );
        println!("  • download the artifact for the current platform");
        println!("  • verify integrity/signature");
        println!("  • replace the current binary with the new version");
        println!("  • persist 'no reminders' preference if requested");
    }

    println!("No changes were made. Upgrade handling is not yet implemented.");
    Ok(())
}
