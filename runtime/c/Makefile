# Veil Runtime (C) - Makefile
# Builds static and shared libraries for the portable iterator runtime
#
# Targets:
#   make            - build static and shared libraries into $(BUILD_DIR)
#   make static     - build libveil_runtime.a
#   make shared     - build libveil_runtime.{so,dylib,dll}
#   make install    - install headers and libraries into $(PREFIX)
#   make uninstall  - remove installed headers and libraries from $(PREFIX)
#   make clean      - remove build artifacts
#   make help       - show this help
#
# Configurable variables:
#   CC, AR, RANLIB, CFLAGS, LDFLAGS, PREFIX, INCLUDEDIR, LIBDIR, BUILD_DIR, DESTDIR
#
# Example:
#   make
#   sudo make install PREFIX=/usr/local
#   make clean

# ---- Toolchain ----------------------------------------------------------------

CC      ?= cc
AR      ?= ar
RANLIB  ?= ranlib

# ---- Paths & Install -----------------------------------------------------------

PREFIX      ?= /usr/local
INCLUDEDIR  ?= $(PREFIX)/include
LIBDIR      ?= $(PREFIX)/lib
BUILD_DIR   ?= build

# ---- Platform Detection --------------------------------------------------------

UNAME_S := $(shell uname -s 2>/dev/null || echo Unknown)

# Defaults (Linux / other POSIX)
SHARED_EXT     := so
SHARED_LDFLAGS := -shared -fPIC
SHARED_NAME    := libveil_runtime.$(SHARED_EXT)
STATIC_NAME    := libveil_runtime.a
DLL_DEFS       :=

ifeq ($(UNAME_S),Darwin)
    SHARED_EXT     := dylib
    SHARED_LDFLAGS := -dynamiclib -fPIC
    SHARED_NAME    := libveil_runtime.$(SHARED_EXT)
endif

# Windows (MSYS/MinGW environments often expose Windows via env var)
ifeq ($(OS),Windows_NT)
    SHARED_EXT     := dll
    SHARED_LDFLAGS := -shared
    SHARED_NAME    := veil_runtime.$(SHARED_EXT)
    DLL_DEFS       := -DVEIL_RUNTIME_DLL
endif

# ---- Flags ---------------------------------------------------------------------

# Caller may append to CFLAGS/LDFLAGS via environment.
CFLAGS  ?= -O2 -std=c99 -Wall -Wextra -pedantic
CFLAGS  += -DVEIL_RUNTIME_BUILD $(DLL_DEFS)
CPPFLAGS?=
LDFLAGS ?=

# Always compile PIC for shared, harmless for static on most platforms
PICFLAGS := -fPIC

# ---- Sources/Objects -----------------------------------------------------------

SRCS := veil_runtime.c
HDRS := veil_runtime.h
OBJS := $(BUILD_DIR)/veil_runtime.o

# ---- Phony Targets -------------------------------------------------------------

.PHONY: all static shared install uninstall clean distclean help

all: $(BUILD_DIR)/$(STATIC_NAME) $(BUILD_DIR)/$(SHARED_NAME)

static: $(BUILD_DIR)/$(STATIC_NAME)

shared: $(BUILD_DIR)/$(SHARED_NAME)

help:
	@echo "Veil Runtime (C) - Makefile"
	@echo "Usage:"
	@echo "  make [all]         Build static and shared libraries into $(BUILD_DIR)"
	@echo "  make static        Build static library ($(STATIC_NAME))"
	@echo "  make shared        Build shared library ($(SHARED_NAME))"
	@echo "  make install       Install header and libraries into $(PREFIX)"
	@echo "  make uninstall     Remove installed header and libraries from $(PREFIX)"
	@echo "  make clean         Remove build artifacts"
	@echo "Variables:"
	@echo "  PREFIX=$(PREFIX)"
	@echo "  INCLUDEDIR=$(INCLUDEDIR)"
	@echo "  LIBDIR=$(LIBDIR)"
	@echo "  BUILD_DIR=$(BUILD_DIR)"
	@echo "  CC=$(CC) AR=$(AR) RANLIB=$(RANLIB)"

# ---- Build Rules ---------------------------------------------------------------

$(BUILD_DIR):
	@mkdir -p $(BUILD_DIR)

$(BUILD_DIR)/veil_runtime.o: veil_runtime.c $(HDRS) | $(BUILD_DIR)
	$(CC) $(CPPFLAGS) $(CFLAGS) $(PICFLAGS) -c $< -o $@

$(BUILD_DIR)/$(STATIC_NAME): $(OBJS) | $(BUILD_DIR)
	$(AR) rcs $@ $(OBJS)
	-@$(RANLIB) $@ >/dev/null 2>&1 || true
	@echo "Built static: $@"

$(BUILD_DIR)/$(SHARED_NAME): $(OBJS) | $(BUILD_DIR)
	$(CC) $(LDFLAGS) $(SHARED_LDFLAGS) -o $@ $(OBJS)
	@echo "Built shared: $@"

# ---- Install/Uninstall ---------------------------------------------------------

install: all
	@echo "Installing headers to $(DESTDIR)$(INCLUDEDIR)"
	@install -d "$(DESTDIR)$(INCLUDEDIR)"
	@install -m 0644 veil_runtime.h "$(DESTDIR)$(INCLUDEDIR)/veil_runtime.h"
	@echo "Installing libraries to $(DESTDIR)$(LIBDIR)"
	@install -d "$(DESTDIR)$(LIBDIR)"
	@install -m 0644 "$(BUILD_DIR)/$(STATIC_NAME)" "$(DESTDIR)$(LIBDIR)/$(STATIC_NAME)"
	@install -m 0755 "$(BUILD_DIR)/$(SHARED_NAME)" "$(DESTDIR)$(LIBDIR)/$(SHARED_NAME)"
	@echo "Install complete."

uninstall:
	@echo "Uninstalling from $(DESTDIR)$(PREFIX)"
	@rm -f "$(DESTDIR)$(INCLUDEDIR)/veil_runtime.h"
	@rm -f "$(DESTDIR)$(LIBDIR)/$(STATIC_NAME)"
	@rm -f "$(DESTDIR)$(LIBDIR)/$(SHARED_NAME)"
	@echo "Uninstall complete."

# ---- Cleaning ------------------------------------------------------------------

clean:
	@rm -rf "$(BUILD_DIR)"

distclean: clean

# ---- Misc ----------------------------------------------------------------------

# Prevent built-in implicit rules that could interfere
.SUFFIXES:
