# VeLang Installation Script for Windows PowerShell (binary release, July 2025)
# Does NOT require Git/Rust for install from released binaries
# Install: powershell -ExecutionPolicy Bypass -File install.ps1

$INSTALL_DIR = Join-Path $env:USERPROFILE ".veil"
$TEMP_DIR = Join-Path $env:TEMP "veil-install"

function Write-Message {
    param(
        [string]$Type,
        [string]$Message
    )
    switch ($Type) {
        "INFO"    { Write-Host "[INFO] $Message"    -ForegroundColor Cyan }
        "SUCCESS" { Write-Host "[SUCCESS] $Message" -ForegroundColor Green }
        "WARNING" { Write-Host "[WARNING] $Message" -ForegroundColor Yellow }
        "ERROR"   { Write-Host "[ERROR] $Message"   -ForegroundColor Red }
    }
}

function Exit-WithPause {
    Write-Host "Press any key to exit..."
    $null = $Host.UI.RawUI.ReadKey("NoEcho,IncludeKeyDown")
    exit 1
}

Write-Message 'INFO' 'Checking PowerShell version and tar/curl...'

# Powershell 5+ and tar/curl required
if ($PSVersionTable.PSVersion.Major -lt 5) {
    Write-Message 'ERROR' "PowerShell 5.0 or newer required."
    Exit-WithPause
}

if (-not (Get-Command "tar" -ErrorAction SilentlyContinue)) {
    Write-Message 'ERROR' "The 'tar' utility (Windows 10+) is required."
    Exit-WithPause
}

if (-not (Get-Command "curl" -ErrorAction SilentlyContinue) -and -not (Get-Command "Invoke-WebRequest" -ErrorAction SilentlyContinue)) {
    Write-Message 'ERROR' "curl or Invoke-WebRequest required (should be present by default)."
    Exit-WithPause
}

Write-Message 'SUCCESS' "Dependencies check completed"

# Create temporary directory
Write-Message "INFO" "Creating temporary directory..."
if (Test-Path $TEMP_DIR) {
    Remove-Item $TEMP_DIR -Recurse -Force
}
New-Item -ItemType Directory -Path $TEMP_DIR -Force | Out-Null

# Download latest VeLang release metadata
Write-Message "INFO" "Checking latest VeLang release..."
$apiUrl = "https://api.github.com/repos/veil-lang/veil/releases/latest"
try {
    $release = Invoke-RestMethod -UseBasicParsing -Headers @{ "User-Agent" = "VeLangInstaller" } -Uri $apiUrl
} catch {
    Write-Message "ERROR" "Could not fetch release info. Check your internet connection/GitHub status."
    Exit-WithPause
}

# Locate Windows asset
$asset = $release.assets | Where-Object { $_.name -like "veil-x86_64-pc-windows-msvc.tar.gz" } | Select-Object -First 1
if (-not $asset) {
    Write-Message "ERROR" "No prebuilt VeLang binary for Windows found in latest release."
    Exit-WithPause
}

# Download archive
Write-Message "INFO" "Downloading $($asset.name)..."
$zipPath = Join-Path $TEMP_DIR $asset.name
try {
    if (Get-Command "curl" -ErrorAction SilentlyContinue) {
        curl.exe -L $asset.browser_download_url -o $zipPath
    } else {
        Invoke-WebRequest -Headers @{ "User-Agent" = "VeLangInstaller" } -Uri $asset.browser_download_url -OutFile $zipPath
    }
} catch {
    Write-Message "ERROR" "Failed to download VeLang archive."
    Exit-WithPause
}

Write-Message "SUCCESS" "Archive downloaded."

# Extract to installation directory
Write-Message "INFO" "Extracting to $INSTALL_DIR..."
if (Test-Path $INSTALL_DIR) { Remove-Item $INSTALL_DIR -Recurse -Force }
New-Item -ItemType Directory -Path $INSTALL_DIR | Out-Null

try {
    tar -xzf $zipPath -C $INSTALL_DIR
} catch {
    Write-Message "ERROR" "Failed to extract archive. Make sure Windows tar is available."
    Exit-WithPause
}

Write-Message "SUCCESS" "Files extracted."

# Add to PATH
Write-Message "INFO" "Adding VeLang to system PATH..."
try {
    $userPath = [Environment]::GetEnvironmentVariable('Path', 'User')
    if ($userPath -notlike "*$($INSTALL_DIR)*") {
        $newPath = ($userPath ? ($userPath + ";") : "") + $INSTALL_DIR
        [Environment]::SetEnvironmentVariable('Path', $newPath, 'User')
        Write-Message 'SUCCESS' "Added $INSTALL_DIR to user PATH."
    } else {
        Write-Message 'INFO' 'VeLang directory already in PATH, skipping.'
    }
} catch {
    Write-Message 'WARNING' 'Failed to add VeLang to PATH. You may need to add it manually.'
}

# Check for success & verify
Write-Message 'INFO' 'Verifying installation...'
$veExe = Join-Path $INSTALL_DIR 've.exe'
if (Test-Path $veExe) {
    try {
        & $veExe --version 2>$null | Out-Null
        if ($LASTEXITCODE -eq 0) {
            Write-Message 'SUCCESS' 'VeLang is working correctly'
        } else {
            Write-Message 'WARNING' 'VeLang binary exists but may not be working correctly'
        }
    } catch {
        Write-Message 'WARNING' 'VeLang binary exists but may not be working correctly'
    }
} else {
    Write-Message 'ERROR' "VeLang binary ve.exe not found in $INSTALL_DIR"
    Exit-WithPause
}

# Cleanup
Write-Message 'INFO' 'Cleaning up...'
Remove-Item $TEMP_DIR -Recurse -Force

# Final messages
Write-Host ''
Write-Message 'SUCCESS' "VeLang installation completed! ($INSTALL_DIR)"
Write-Host ''
Write-Host 'To get started:'
Write-Host '  ve --help                 # Show help'
Write-Host '  ve init my_project        # Create a new project'
Write-Host '  ve example.ve             # Compile and run a file'
Write-Host ''
Write-Host 'Join our Discord: https://dsc.gg/velang'
Write-Host ''
Write-Message 'INFO' 'Please restart your command prompt to use ''ve'' command'
Write-Host ''

if ($Host.Name -eq 'ConsoleHost') {
    Write-Host 'Press any key to exit...'
    $null = $Host.UI.RawUI.ReadKey('NoEcho,IncludeKeyDown')
}
