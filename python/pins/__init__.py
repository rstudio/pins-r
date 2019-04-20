"""
The ``pins`` module provides an API for tracking, discovering and sharing datasets.
"""

import os
import yaml
from _pins_cffi import ffi
import subprocess
import platform
import sys

def _get_rhome():
    r_home = os.environ.get("R_HOME")
    if r_home:
        return r_home
    tmp = subprocess.check_output(("R", "RHOME"), universal_newlines=True)
    r_home = tmp.split(os.linesep)
    if r_home[0].startswith("WARNING"):
        r_home = r_home[1]
    else:
        r_home = r_home[0].strip()
    return r_home

def _get_rlib():
    r_home = _get_rhome()
    system = platform.system()
    if system == "Linux":
        lib_path = os.path.join(r_home, "lib", "libR.so")
    elif system == "Darwin":
        lib_path = os.path.join(r_home, "lib", "libR.dylib")
    else:
        raise ValueError("System '%s' is unsupported.")
    return lib_path

def _open_rlib():
    return ffi.dlopen(_get_rlib())

def _print(message):
    sys.stdout.write(message)
    sys.stdout.flush()

@ffi.callback("void(char *, int, int)")
def _console_write(buffer, size, otype):
    _print(ffi.string(buffer, size).decode("utf-8"))

@ffi.callback("void(char *)")
def _showmessage(buffer):
    _print(ffi.string(buffer).decode("utf-8"))

def _start():
    os.environ["R_HOME"] = _get_rhome()
    rlib = _open_rlib()

    options = ("pins", "--quiet", "--vanilla", "--no-save")
    options_raw = [ffi.new("char[]", o.encode("ASCII")) for o in options]
    status = rlib.Rf_initialize_R(ffi.cast("int", len(options_raw)), options_raw)

    rlib.ptr_R_WriteConsoleEx = _console_write
    rlib.ptr_R_WriteConsole = ffi.NULL

    rlib.setup_Rmainloop()
    return rlib

rlib = _start()

def eval(code):
    cmdSexp = rlib.Rf_allocVector(rlib.STRSXP, 1)
    rlib.Rf_protect(cmdSexp)
    rlib.SET_STRING_ELT(cmdSexp, 0, rlib.Rf_mkChar(code));
    
    status = ffi.new("ParseStatus *")
    cmdexpr = rlib.Rf_protect(rlib.R_ParseVector(cmdSexp, -1, status, rlib.R_NilValue));

    rlib.Rf_unprotect(2)
    if status[0] <> rlib.PARSE_OK:
        raise RuntimeError("Failed to parse: " + code)

    environment = rlib.R_GlobalEnv
    error = ffi.new("int *")

    for idx in range(0, rlib.Rf_length(cmdexpr)):
        rlib.R_tryEval(rlib.VECTOR_ELT(cmdexpr, idx), environment, error);
        if (error[0]):
            break;

    if (error[0]):
        raise RuntimeError("Error (" + str(error[0]) + ") evaluating: " + code)

def find_pin():
    """
    Find Pin.
    """
    eval("print(pins::find_pin())")

def get_pin(name, board = None):
    """
    Retrieve Pin.
    """
    eval("pins::as_arrow(pins::get_pin(\"" + name + "\"))")