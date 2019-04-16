from cffi import FFI
ffibuilder = FFI()

ffibuilder.cdef("""
""")

ffibuilder.set_source("_pins_cffi", None)

if __name__ == "__main__":
    ffibuilder.compile(verbose=True)