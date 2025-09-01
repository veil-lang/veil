#!/usr/bin/env bash
#
# capture_all_baselines.sh
#
# Batch-capture baseline artifacts (AST, emitted C, timings) for:
#   - tests/**/*.veil (including tests/neg)
#   - lib/std/src/*.veil
# and produce a consolidated manifest JSON.
#
# This script invokes scripts/capture_baselines.sh for each file and aggregates
# each run's summary.json into a single manifest at:
#   build/baselines/<timestamp>/manifest.json
#
# Usage:
#   ./scripts/capture_all_baselines.sh [--ve <path_to_ve>] [--out <dir>] [--keep-shim]
#                                      [--only-tests] [--only-stdlib]
#                                      [--tests-glob "<glob>"] [--stdlib-glob "<glob>"]
#                                      [--dry-run]
#
# Examples:
#   ./scripts/capture_all_baselines.sh
#   ./scripts/capture_all_baselines.sh --ve ./target/release/ve --only-tests
#   ./scripts/capture_all_baselines.sh --out build/baselines/myrun --keep-shim
#
# Notes:
# - Requires bash, find, date. Optional: sha256sum/shasum (used by capture_baselines.sh).
# - Designed for Unix-like environments (Linux/macOS).
# - The per-file script avoids C compilation via --no-cc (if supported) or a clang shim.

set -u

print_usage() {
  sed -n '1,80p' "$0" | sed 's/^# \{0,1\}//'
}

info()  { echo "[info] $*"; }
warn()  { echo "[warn] $*" >&2; }
error() { echo "[error] $*" >&2; }

# Resolve script/repo paths
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"

CAPTURE_ONE="${SCRIPT_DIR}/capture_baselines.sh"
if [[ ! -x "${CAPTURE_ONE}" ]]; then
  error "Missing or non-executable ${CAPTURE_ONE}. Ensure it exists and is executable."
  exit 1
fi

# Defaults
VE_BIN="${VE_BIN:-}"
OUT_BASE=""                         # computed later as build/baselines/<timestamp>
KEEP_SHIM=0
ONLY_TESTS=0
ONLY_STDLIB=0
TESTS_GLOB_DEFAULT="tests/**/*.veil"
STDLIB_GLOB_DEFAULT="lib/std/src/*.veil"
TESTS_GLOB="${TESTS_GLOB_DEFAULT}"
STDLIB_GLOB="${STDLIB_GLOB_DEFAULT}"
DRY_RUN=0

# Parse args
while [[ $# -gt 0 ]]; do
  case "$1" in
    -h|--help) print_usage; exit 0 ;;
    --ve) shift; VE_BIN="${1:-}";;
    --out) shift; OUT_BASE="${1:-}";;
    --keep-shim) KEEP_SHIM=1;;
    --only-tests) ONLY_TESTS=1;;
    --only-stdlib) ONLY_STDLIB=1;;
    --tests-glob) shift; TESTS_GLOB="${1:-}";;
    --stdlib-glob) shift; STDLIB_GLOB="${1:-}";;
    --dry-run) DRY_RUN=1;;
    *) error "Unknown arg: $1"; print_usage; exit 1 ;;
  esac
  shift
done

timestamp() { date -u +%Y%m%dT%H%M%SZ; }

# Compute OUT_BASE if not provided
if [[ -z "${OUT_BASE}" ]]; then
  OUT_BASE="${REPO_ROOT}/build/baselines/$(timestamp)"
fi
mkdir -p "${OUT_BASE}" || { error "Failed to create OUT_BASE: ${OUT_BASE}"; exit 1; }

# Discover ve binary if not provided
if [[ -z "${VE_BIN}" ]]; then
  if command -v ve >/dev/null 2>&1; then
    VE_BIN="$(command -v ve)"
  else
    warn "ve binary not found in PATH and --ve not provided. capture_baselines.sh may fail."
    VE_BIN=""
  fi
fi

# Utility: portable absolute path
abs_path() {
  # Use Python for portability across OS
  python3 - "$1" <<'PY'
import os, sys
p = sys.argv[1]
print(os.path.abspath(p))
PY
}

# Utility: repo-relative path (for nice reporting)
rel_to_repo() {
  python3 - "$1" "$2" <<'PY'
import os, sys
target = os.path.abspath(sys.argv[1])
root = os.path.abspath(sys.argv[2])
try:
    print(os.path.relpath(target, root))
except ValueError:
    print(target)
PY
}

# Utility: sanitize path into a filesystem-friendly dir name
sanitize_path() {
  # Replace path separators, spaces, and other non-alnum chars
  echo "$1" | sed -e 's/[\/\\]/__/g' -e 's/[^A-Za-z0-9_.-]/_/g'
}

# Collect files
find_files_with_glob() {
  local pattern="$1"
  # Expand glob relative to repo root using bash -O globstar if needed
  ( shopt -s nullglob globstar; cd "${REPO_ROOT}" && printf '%s\n' ${pattern} )
}

FILES=()

if [[ ${ONLY_STDLIB} -eq 0 ]]; then
  mapfile -t TEST_FILES < <(find_files_with_glob "${TESTS_GLOB}")
  # Include tests/neg by default if using default glob patterns
  if [[ "${TESTS_GLOB}" == "${TESTS_GLOB_DEFAULT}" ]]; then
    mapfile -t TEST_NEG_FILES < <(find_files_with_glob "tests/neg/**/*.veil")
    TEST_FILES+=( "${TEST_NEG_FILES[@]}" )
  fi
  FILES+=( "${TEST_FILES[@]}" )
fi

if [[ ${ONLY_TESTS} -eq 0 ]]; then
  mapfile -t STDLIB_FILES < <(find_files_with_glob "${STDLIB_GLOB}")
  FILES+=( "${STDLIB_FILES[@]}" )
fi

# De-duplicate
if [[ ${#FILES[@]} -eq 0 ]]; then
  warn "No input .veil files found with provided globs."
  exit 0
fi

# Make consolidated directories
FILES_DIR="${OUT_BASE}/files"
mkdir -p "${FILES_DIR}"

# Prepare consolidated manifest
MANIFEST="${OUT_BASE}/manifest.json"
TMP_MANIFEST="${MANIFEST}.tmp"
echo "[" > "${TMP_MANIFEST}"
FIRST=1

# Status counters
TOTAL=0
SUCCEEDED=0
FAILED=0

# Iterate
for f in "${FILES[@]}"; do
  # Guard for empty expansion
  [[ -z "${f}" ]] && continue

  TOTAL=$((TOTAL + 1))
  F_ABS="$(abs_path "${f}")"
  REL="$(rel_to_repo "${F_ABS}" "${REPO_ROOT}")"
  SAN="$(sanitize_path "${REL}")"
  OUT_DIR="${FILES_DIR}/${SAN}"

  info "Capturing baseline for: ${REL}"
  if [[ ${DRY_RUN} -eq 1 ]]; then
    info "DRY-RUN: would invoke: ${CAPTURE_ONE} '${F_ABS}' --out '${OUT_DIR}' ${VE_BIN:+--ve '${VE_BIN}'} ${KEEP_SHIM:+--keep-shim}"
    # Add a minimal manifest entry
    [[ ${FIRST} -eq 0 ]] && echo "," >> "${TMP_MANIFEST}"
    FIRST=0
    cat >> "${TMP_MANIFEST}" <<JSON
{"input":"${REL}","out_dir":"$(rel_to_repo "${OUT_DIR}" "${REPO_ROOT}")","dry_run":true}
JSON
    continue
  fi

  mkdir -p "${OUT_DIR}" || { warn "Failed to create ${OUT_DIR}, skipping."; FAILED=$((FAILED + 1)); continue; }

  # Build command
  CMD=( "${CAPTURE_ONE}" "${F_ABS}" --out "${OUT_DIR}" )
  [[ -n "${VE_BIN}" ]] && CMD+=( --ve "${VE_BIN}" )
  [[ ${KEEP_SHIM} -eq 1 ]] && CMD+=( --keep-shim )

  # Execute
  if "${CMD[@]}"; then
    SUCCEEDED=$((SUCCEEDED + 1))
  else
    warn "Capture failed for ${REL}"
    FAILED=$((FAILED + 1))
  fi

  # Consume summary.json if present
  SUMMARY="${OUT_DIR}/summary.json"
  if [[ -f "${SUMMARY}" ]]; then
    # Append with proper comma
    [[ ${FIRST} -eq 0 ]] && echo "," >> "${TMP_MANIFEST}"
    FIRST=0
    # Indent summary content for readability
    sed 's/^/  /' "${SUMMARY}" >> "${TMP_MANIFEST}"
  else
    # Insert an error entry
    [[ ${FIRST} -eq 0 ]] && echo "," >> "${TMP_MANIFEST}"
    FIRST=0
    cat >> "${TMP_MANIFEST}" <<JSON
  {
    "input": "${REL}",
    "out_dir": "$(rel_to_repo "${OUT_DIR}" "${REPO_ROOT}")",
    "error": "summary.json missing (capture may have failed)",
    "timestamp": "$(timestamp)"
  }
JSON
  fi
done

echo "]" >> "${TMP_MANIFEST}"
mv -f "${TMP_MANIFEST}" "${MANIFEST}"

# Write a short index file with paths (for quick grep)
INDEX="${OUT_BASE}/files.txt"
: > "${INDEX}"
for f in "${FILES[@]}"; do
  [[ -z "${f}" ]] && continue
  REL="$(rel_to_repo "$(abs_path "${f}")" "${REPO_ROOT}")"
  echo "${REL}" >> "${INDEX}"
done

# Summary
echo
info "Baseline batch completed."
info "Total: ${TOTAL} | Succeeded: ${SUCCEEDED} | Failed: ${FAILED}"
info "Output base: $(rel_to_repo "${OUT_BASE}" "${REPO_ROOT}")"
info "Manifest:    $(rel_to_repo "${MANIFEST}" "${REPO_ROOT}")"
info "Index:       $(rel_to_repo "${INDEX}" "${REPO_ROOT}")"

# Exit with non-zero if any failures (useful for CI)
if [[ ${FAILED} -gt 0 ]]; then
  exit 2
fi

exit 0
