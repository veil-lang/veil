#!/bin/bash
# Veil Installation Script for Linux (binary release, July 2025)
# Does NOT require Git/Rust for install from released binaries
# Install: bash install.sh

INSTALL_DIR="$HOME/.veil"
TEMP_DIR=$(mktemp -d -t veil-install-XXXXXX)

# Text formatting
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
CYAN='\033[0;36m'
NC='\033[0m'

function print_msg() {
    case "$1" in
        "INFO") echo -e "${CYAN}[INFO]${NC} $2" ;;
        "SUCCESS") echo -e "${GREEN}[SUCCESS]${NC} $2" ;;
        "WARNING") echo -e "${YELLOW}[WARNING]${NC} $2" ;;
        "ERROR") echo -e "${RED}[ERROR]${NC} $2" ;;
    esac
}

function exit_with_pause() {
    echo "Press Enter to exit..."
    read -r
    exit 1
}

print_msg 'INFO' 'Checking dependencies...'

# Check for required tools
if ! command -v curl &> /dev/null; then
    print_msg 'ERROR' "curl is required but not installed."
    echo "Please install curl and try again"
    exit_with_pause
fi

if ! command -v tar &> /dev/null; then
    print_msg 'ERROR' "tar is required but not installed."
    echo "Please install tar and try again"
    exit_with_pause
fi

print_msg 'SUCCESS' "Dependencies check completed"

# Create temporary directory
print_msg "INFO" "Creating temporary directory..."
mkdir -p "$TEMP_DIR"

# Detect architecture
arch=$(uname -m)
case "$arch" in
    x86_64|amd64) asset_arch="x86_64" ;;
    aarch64|arm64) asset_arch="aarch64" ;;
    *)
        print_msg "ERROR" "Unsupported architecture: $arch"
        exit_with_pause
        ;;
esac

asset_pattern="veil-${asset_arch}-unknown-linux-gnu.tar.gz"
download_url=""

# Try to get latest release first
print_msg "INFO" "Checking latest Veil release..."
api_url="https://api.github.com/repos/veil-lang/veil/releases/latest"

if release_data=$(curl -s -H "User-Agent: VeilInstaller" "$api_url" 2>/dev/null); then
    # Better JSON parsing - try multiple methods
    if command -v jq &> /dev/null; then
        # Use jq if available
        download_url=$(echo "$release_data" | jq -r ".assets[] | select(.name | test(\"$asset_pattern\")) | .browser_download_url" 2>/dev/null | head -1)
    else
        # Fallback to grep/sed parsing
        download_url=$(echo "$release_data" | grep -o "\"browser_download_url\":\"[^\"]*$asset_pattern\"" | sed 's/.*"\([^"]*\)".*/\1/' | head -1)
    fi
fi

# If latest release doesn't have Linux binary, try all releases
if [ -z "$download_url" ]; then
    print_msg "WARNING" "No Linux binary in latest release, checking all releases..."
    api_url="https://api.github.com/repos/veil-lang/veil/releases"

    if releases_data=$(curl -s -H "User-Agent: VeilInstaller" "$api_url" 2>/dev/null); then
        if command -v jq &> /dev/null; then
            download_url=$(echo "$releases_data" | jq -r ".[].assets[] | select(.name | test(\"$asset_pattern\")) | .browser_download_url" 2>/dev/null | head -1)
        else
            download_url=$(echo "$releases_data" | grep -o "\"browser_download_url\":\"[^\"]*$asset_pattern\"" | sed 's/.*"\([^"]*\)".*/\1/' | head -1)
        fi
    fi
fi

# Final fallback to known v0.2-beta URL
if [ -z "$download_url" ]; then
    print_msg "WARNING" "API search failed, trying known v0.2-beta release..."
    download_url="https://github.com/veil-lang/veil/releases/download/v0.2-beta/veil-${asset_arch}-unknown-linux-gnu.tar.gz"
fi

if [ -z "$download_url" ]; then
    print_msg "ERROR" "No prebuilt Veil binary for Linux ($asset_arch) found."
    print_msg "INFO" "You may need to build from source or check https://github.com/veil-lang/veil/releases"
    exit_with_pause
fi

# Test if download URL is accessible
print_msg "INFO" "Testing download URL..."
if ! curl -s -I "$download_url" | grep -q "200 OK"; then
    print_msg "ERROR" "Download URL is not accessible: $download_url"
    exit_with_pause
fi

# Download archive
asset_name=$(basename "$download_url")
print_msg "INFO" "Downloading $asset_name..."
archive_path="$TEMP_DIR/$asset_name"

if ! curl -L -H "User-Agent: VeilInstaller" "$download_url" -o "$archive_path"; then
    print_msg "ERROR" "Failed to download Veil archive."
    exit_with_pause
fi

print_msg "SUCCESS" "Archive downloaded."

# Extract to installation directory
print_msg "INFO" "Extracting to $INSTALL_DIR..."
if [ -d "$INSTALL_DIR" ]; then
    rm -rf "$INSTALL_DIR"
fi
mkdir -p "$INSTALL_DIR"

if ! tar -xzf "$archive_path" -C "$INSTALL_DIR"; then
    print_msg "ERROR" "Failed to extract archive."
    exit_with_pause
fi

print_msg "SUCCESS" "Files extracted."

# Add to PATH
print_msg "INFO" "Adding Veil to system PATH..."

# Determine shell profile file
shell_profile=""
case "$SHELL" in
    */zsh) shell_profile="$HOME/.zshrc" ;;
    */bash) shell_profile="$HOME/.bashrc" ;;
    */fish) shell_profile="$HOME/.config/fish/config.fish" ;;
    *) shell_profile="$HOME/.profile" ;;
esac

# Check if already in PATH
if ! grep -q "$INSTALL_DIR" "$shell_profile" 2>/dev/null; then
    echo "export PATH=\"\$PATH:$INSTALL_DIR\"" >> "$shell_profile"
    print_msg 'SUCCESS' "Added $INSTALL_DIR to $shell_profile"
else
    print_msg 'INFO' 'Veil directory already in PATH, skipping.'
fi

# Check for success & verify
print_msg 'INFO' 'Verifying installation...'
ve_binary="$INSTALL_DIR/ve"

if [ -f "$ve_binary" ]; then
    if "$ve_binary" --version >/dev/null 2>&1; then
        print_msg 'SUCCESS' 'Veil is working correctly'
    else
        print_msg 'WARNING' 'Veil binary exists but may not be working correctly'
    fi
else
    print_msg 'ERROR' "Veil binary 've' not found in $INSTALL_DIR"
    exit_with_pause
fi

# Cleanup
print_msg 'INFO' 'Cleaning up...'
rm -rf "$TEMP_DIR"

# Final messages
echo ''
print_msg 'SUCCESS' "Veil installation completed! ($INSTALL_DIR)"
echo ''
echo 'To get started:'
echo '  source '"$shell_profile"'      # Reload shell configuration'
echo '  ve --help                 # Show help'
echo '  ve init my_project        # Create a new project'
echo '  ve example.veil             # Compile and run a file'
echo ''
echo 'Join our Discord: https://dsc.gg/velang'
echo ''
print_msg 'INFO' 'Please restart your terminal or run: source '"$shell_profile"
echo ''
