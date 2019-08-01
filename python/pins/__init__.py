"""
The ``pins`` module provides an API to pin, discover and share files.
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

@ffi.callback('void(SA_TYPE, int, int)')
def _cleanup(saveact, status, runlast):
    pass

@ffi.callback('void(void)')
def _processevents():
    pass

@ffi.callback('void(int)')
def _busy(which):
    pass
    
def _main_loop_started():
    return rlib.ptr_R_WriteConsoleEx != ffi.NULL or rlib.R_GlobalEnv != ffi.NULL

rlib = None
def r_start():
    global rlib
    if (rlib != None):
        return rlib
        
    os.environ["R_HOME"] = _get_rhome()
    rlib = _open_rlib()

    if (_main_loop_started()):
        return rlib
        
    import atexit
    atexit.register(r_end, 0)
        
    options = ("pins", "--quiet", "--vanilla", "--no-save")
    options_raw = [ffi.new("char[]", o.encode("ASCII")) for o in options]
    status = rlib.Rf_initialize_R(ffi.cast("int", len(options_raw)), options_raw)

    rlib.ptr_R_WriteConsoleEx = _console_write
    rlib.ptr_R_WriteConsole = ffi.NULL
    rlib.ptr_R_CleanUp = _cleanup
    rlib.ptr_R_ProcessEvents = _processevents
    rlib.ptr_R_Busy = _busy

    rlib.setup_Rmainloop()

    return rlib

def r_end(fatal):
    rlib.R_dot_Last()
    rlib.R_RunExitFinalizers()
    rlib.Rf_KillAllDevices()
    rlib.R_CleanTempDir()
    rlib.R_gc()
    rlib.Rf_endEmbeddedR(fatal)

def r_eval(code, environment = None):
    r_start()
    
    cmdSexp = rlib.Rf_allocVector(rlib.STRSXP, 1)
    rlib.Rf_protect(cmdSexp)
    rlib.SET_STRING_ELT(cmdSexp, 0, rlib.Rf_mkChar(code));
    
    status = ffi.new("ParseStatus *")
    cmdexpr = rlib.Rf_protect(rlib.R_ParseVector(cmdSexp, -1, status, rlib.R_NilValue))

    rlib.Rf_unprotect(2)
    if status[0] != rlib.PARSE_OK:
        raise RuntimeError("Failed to parse: " + code)

    if environment == None:
        environment = rlib.R_GlobalEnv
        
    error = ffi.new("int *")

    result = rlib.Rf_protect(rlib.R_tryEval(rlib.VECTOR_ELT(cmdexpr, 0), environment, error))

    if (error[0]):
        message = r_eval("gsub('\\\n', '', geterrmessage())")
        raise RuntimeError(message + " at " + code)

    rtype = result.sxpinfo.type
    if (rtype == rlib.CHARSXP):
        result = ffi.string(rlib.R_CHAR(result))
    elif (rtype == rlib.STRSXP):
        result = ffi.string(rlib.R_CHAR(rlib.STRING_ELT(result, 0)))
    elif (rtype == rlib.RAWSXP):
        n = rlib.Rf_xlength(result)
        result = ffi.buffer(rlib.RAW(result), n)

    rlib.Rf_unprotect(1)
    return result

def _init_pins():
    r_start()
    r_eval("""
        if (length(find.package("pins", quiet = TRUE)) == 0) {
            if (length(find.package("remotes", quiet = TRUE)) == 0) {
              pin_log("Installing 'remotes' package.")
              install.packages("remotes")
            }

            pin_log("Installing 'pins' package.")
            remotes::install_github("rstudio/pins")
        }
        
        if (length(find.package("feather", quiet = TRUE)) == 0) {
          pin_log("Installing 'feather' package.")
          install.packages("feather")
        }
    """)
    r_eval("library('pins')")
    
def _from_arrow(buffer):
    import pyarrow as pa
    return pa.ipc.open_stream(buffer).read_pandas()

def _to_arrow(x):
    import pyarrow as pa
    return pa.ipc.open_stream(buffer).read_pandas()

def _to_feather(x):
    import feather
    feather_path = r_eval('tempfile(fileext = ".feather")')
    feather.write_dataframe(x, feather_path)
    return feather_path
    
def _from_feather(path):
    import feather
    return feather.read_dataframe(path)
    
def _eval_deserialize(operation, serializer):
    feather_path = r_eval('tempfile(fileext = ".feather")')
    r_eval("feather::write_feather(" + operation + ", \"" + feather_path + "\")")
    result = _from_feather(feather_path)
    os.remove(feather_path)
    return result

def pin_find(text = "", serializer = "feather"):
    """
    Find Pin.
    """
    _init_pins()
    return _eval_deserialize("pins::pin_find(\"" + text + "\")", serializer)

def pin_get(name, board = None, serializer = "feather"):
    """
    Retrieve Pin.
    """
    _init_pins()
    return _eval_deserialize("pins::pin_get(\"" + name + "\")", serializer)

def pin(x, name, description = "", board = None, serializer = "feather"):
    """
    Create Pin.
    """
    _init_pins()
    result = None
    
    if serializer == "arrow":
      raise RuntimeError("Serializing pin() with 'arrow' currently unsupported, use 'feather' instead.")
    elif type(x) == "str":
      result = r_eval("pins::pin(\"" + x + "\"), \"" + name + "\")")
    else:
      path = _to_feather(x)
      r_eval("feather::write_feather(pins::pin(feather::read_feather(\"" + path + "\"), \"" + name + "\"), \"" + path + "\")")
      result = _from_feather(path)
      os.remove(path)
      
    return result
    
def use_board(name):
    """
    Use Board.
    """
    _init_pins()
    return r_eval("pins::use_board(\"" + name + "\")")
