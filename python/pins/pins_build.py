from cffi import FFI
ffibuilder = FFI()

# include/Rembedded.h
ffibuilder.cdef("""
int Rf_initialize_R(int ac, char **av);
void R_RunExitFinalizers(void);
void Rf_KillAllDevices(void);
void R_CleanTempDir(void);
void Rf_endEmbeddedR(int fatal);
""")

ffibuilder.set_source("_pins_cffi", None)

if __name__ == "__main__":
    ffibuilder.compile(verbose=True)
