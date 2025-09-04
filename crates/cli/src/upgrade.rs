#![forbid(unsafe_code)]
#![deny(rust_2018_idioms)]
#![deny(unused_must_use)]

//! Upgrade subcommand (not implemented)
//!
//! This module provides a clear error message for the `upgrade` command,
//! which is not yet implemented. Instead of misleading users with a placeholder
//! that pretends to work, this gives them clear information about the status.

use crate::Channel;
use anyhow::{Result, anyhow};

/// Execute the upgrade flow.
///
/// Currently returns an error indicating the feature is not implemented.
/// This prevents user confusion about whether the upgrade actually worked.
///
/// Arguments:
/// - `no_remind`: Disable future upgrade reminders (ignored - not implemented).
/// - `force`: Proceed without confirmation (ignored - not implemented).
/// - `verbose`: Print extra information (ignored - not implemented).
/// - `channel`: Selected update channel (ignored - not implemented).
pub fn run_upgrade(
    _no_remind: bool,
    _force: bool,
    _verbose: bool,
    _channel: Channel,
) -> Result<()> {
    Err(anyhow!(
        "The 'upgrade' command is not yet implemented.\n\
         \n\
         To update Veil, please:\n\
         1. Download the latest release from the official repository\n\
         2. Replace your current installation manually\n\
         3. Or use your system package manager if available\n\
         \n\
         This feature will be implemented in a future release."
    ))
}
