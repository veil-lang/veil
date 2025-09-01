#!/usr/bin/env bash
#
# capture_baselines.sh
#
# Capture baseline artifacts for a Veil source file:
# - Parsed AST dump (from build/parsed_ast.txt, via verbose mode)
# - Generated C source (build/temp.c)
# - Wall-clock timings
#
# This script attempts to avoid compiling C by shimming clang to a no-op.
# If your ve CLI supports --no-cc, the script will use it automatically.
#
# Usage:
#   ./scripts/capture_baselines.sh <input.veil> [--out <dir>] [--ve <path_to_ve>] [--keep-shim]
#
# Examples:
#   ./scripts/capture_baselines.sh examples/hello.veil
#   ./scripts/capture_baselines.sh ./main.veil --out build/baselines --ve ./target/release/ve
#
# Notes:
# - Requires: bash, mktemp, date, sha256sum (or shasum), awk, sed
# - Designed for Unix-like environments (Linux/macOS)

set -u

print_usage() {
  sed -n '1,40p' "$0" | sed 's/^# \{0,1\}//'
}

err() {
  echo "Error: $*" >&2
}

info() {
  echo "[info] $*"
}

# --- args ---
INPUT=""
OUT_DIR=""
VE_BIN="${VE_BIN:-}"
KEEP_SHIM=0

while [[ $# -gt 0 ]]; do
  case "$1" in
    -h|--help)
      print_usage
      exit 0
      ;;
    --out)
      shift
      OUT_DIR="${1:-}"
      ;;
    --ve)
      shift
      VE_BIN="${1:-}"
      ;;
    --keep-shim)
      KEEP_SHIM=1
      ;;
    -*)
      err "Unknown flag: $1"
      exit 1
      ;;
    *)
      if [[ -z "${INPUT}" ]]; then
        INPUT="$1"
      else
        err "Unexpected positional argument: $1"
        exit 1
      fi
      ;;
  esac
  shift
done

if [[ -z "${INPUT}" ]]; then
  err "Input .veil file is required. Use -h for help."
  exit 1
fi

if [[ ! -f "${INPUT}" ]]; then
  err "Input file not found: ${INPUT}"
  exit 1
fi

# Resolve ve binary
if [[ -z "${VE_BIN}" ]]; then
  if command -v ve >/dev/null 2>&1; then
    VE_BIN="$(command -v ve)"
  else
    err "ve binary not found in PATH. Provide with --ve <path_to_ve>."
    exit 1
  fi
fi

# Normalize paths
abs_path() {
  # Portable realpath alternative
  python3 - "$1" <<'PY'
import os, sys
p = sys.argv[1]
print(os.path.abspath(p))
PY
}

INPUT_ABS="$(abs_path "${INPUT}")"

# Compute default OUT_DIR if not provided
TIMESTAMP="$(date -u +%Y%m%dT%H%M%SZ)"
if [[ -z "${OUT_DIR}" ]]; then
  # Place baselines next to the input file's build directory
  INPUT_DIR="$(dirname "${INPUT_ABS}")"
  OUT_DIR="${INPUT_DIR}/build/baselines/${TIMESTAMP}"
fi
mkdir -p "${OUT_DIR}" || { err "Failed to create out dir: ${OUT_DIR}"; exit 1; }

# Baseline targets
AST_OUT="${OUT_DIR}/ast.txt"
C_OUT="${OUT_DIR}/emitted.c"
SUMMARY_JSON="${OUT_DIR}/summary.json"
LOG_FILE="${OUT_DIR}/capture.log"

# --- clang shim (to avoid compiling C) ---
SHIM_DIR="$(mktemp -d 2>/dev/null || mktemp -d -t veil-baseline)"
cleanup() {
  if [[ ${KEEP_SHIM} -eq 0 ]]; then
    rm -rf "${SHIM_DIR}" >/dev/null 2>&1 || true
  fi
}
trap cleanup EXIT

# Create a no-op clang shim
cat > "${SHIM_DIR}/clang" <<'SH'
#!/usr/bin/env bash
# No-op clang shim for baseline capture; exit success without compiling.
# Writes minimal message to stderr to make it obvious in logs.
echo "[shim-clang] Skipping C compilation." >&2
exit 0
SH
chmod +x "${SHIM_DIR}/clang"

# Prepend shim to PATH; this often takes precedence if the tool resolves clang via PATH.
export PATH="${SHIM_DIR}:${PATH}"

# --- run ve to produce AST (verbose) and emitted C ---
# Prefer --no-cc if supported; otherwise fall back to normal invocation
# Note: ve (verbose) writes build/parsed_ast.txt before invoking clang.
VE_ARGS_VERBOSE="-v"
VE_OUT_DEFAULT="program.exe"  # ve CLI defaults
VE_BUILD_OUT_DIR="$(dirname "${INPUT_ABS}")/build"
mkdir -p "${VE_BUILD_OUT_DIR}" || true

supports_no_cc=0
if "${VE_BIN}" --help 2>&1 | grep -q -- "--no-cc"; then
  supports_no_cc=1
fi

start_ns="$(date +%s%N)"
if [[ ${supports_no_cc} -eq 1 ]]; then
  info "Running ve with --no-cc to avoid compiling C"
  "${VE_BIN}" "${INPUT_ABS}" ${VE_ARGS_VERBOSE} --no-cc >"${LOG_FILE}" 2>&1
  ve_rc=$?
else
  info "ve does not support --no-cc; using clang shim to avoid heavy compilation"
  "${VE_BIN}" "${INPUT_ABS}" ${VE_ARGS_VERBOSE} >"${LOG_FILE}" 2>&1
  ve_rc=$?
fi
end_ns="$(date +%s%N)"
elapsed_ms=$(( (end_ns - start_ns) / 1000000 ))
info "ve invocation completed in ${elapsed_ms} ms (exit=${ve_rc}). See ${LOG_FILE}."

# Even if ve fails later (e.g., linking), AST and temp.c may already exist.
AST_SRC="${VE_BUILD_OUT_DIR}/parsed_ast.txt"
C_SRC="${VE_BUILD_OUT_DIR}/temp.c"

# Copy AST
if [[ -f "${AST_SRC}" ]]; then
  cp -f "${AST_SRC}" "${AST_OUT}"
  info "Captured AST: ${AST_OUT}"
else
  err "AST dump not found at ${AST_SRC}. Ensure ve ran with verbose output."
fi

# Copy emitted C
if [[ -f "${C_SRC}" ]]; then
  cp -f "${C_SRC}" "${C_OUT}"
  info "Captured emitted C: ${C_OUT}"
else
  err "Emitted C not found at ${C_SRC}. Codegen may have failed."
fi

# --- digests and sizes ---
hash_cmd=""
if command -v sha256sum >/dev/null 2>&1; then
  hash_cmd="sha256sum"
elif command -v shasum >/dev/null 2>&1; then
  hash_cmd="shasum -a 256"
fi

ast_hash=""
ast_size=0
if [[ -f "${AST_OUT}" ]]; then
  ast_size=$(wc -c < "${AST_OUT}" | awk '{print $1}')
  if [[ -n "${hash_cmd}" ]]; then
    ast_hash=$(${hash_cmd} "${AST_OUT}" | awk '{print $1}')
  fi
fi

c_hash=""
c_size=0
if [[ -f "${C_OUT}" ]]; then
  c_size=$(wc -c < "${C_OUT}" | awk '{print $1}')
  if [[ -n "${hash_cmd}" ]]; then
    c_hash=$(${hash_cmd} "${C_OUT}" | awk '{print $1}')
  fi
fi

# --- summary JSON ---
cat > "${SUMMARY_JSON}" <<JSON
{
  "input": "$(printf '%s' "${INPUT_ABS}" | sed 's/\\/\\\\/g')",
  "timestamp": "${TIMESTAMP}",
  "ve_path": "$(printf '%s' "${VE_BIN}" | sed 's/\\/\\\\/g')",
  "elapsed_ms": ${elapsed_ms},
  "exit_code": ${ve_rc},
  "artifacts": {
    "ast": {
      "path": "$(printf '%s' "${AST_OUT}" | sed 's/\\/\\\\/g')",
      "exists": $( [[ -f "${AST_OUT}" ]] && echo true || echo false ),
      "size_bytes": ${ast_size},
      "sha256": "$(printf '%s' "${ast_hash}")"
    },
    "c": {
      "path": "$(printf '%s' "${C_OUT}" | sed 's/\\/\\\\/g')",
      "exists": $( [[ -f "${C_OUT}" ]] && echo true || echo false ),
      "size_bytes": ${c_size},
      "sha256": "$(printf '%s' "${c_hash}")"
    },
    "log": {
      "path": "$(printf '%s' "${LOG_FILE}" | sed 's/\\/\\\\/g')",
      "exists": $( [[ -f "${LOG_FILE}" ]] && echo true || echo false )
    }
  },
  "notes": [
    "AST is captured by running ve with verbose mode, which emits build/parsed_ast.txt.",
    "C output is captured from build/temp.c prior to C compilation.",
    "$( [[ ${supports_no_cc} -eq 1 ]] && echo "Used --no-cc to avoid compiling C." || echo "Used a no-op clang shim to avoid heavy compilation." )"
  ]
}
JSON

info "Summary: ${SUMMARY_JSON}"
info "Baseline capture complete."
