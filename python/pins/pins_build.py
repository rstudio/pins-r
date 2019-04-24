from cffi import FFI
ffibuilder = FFI()

ffibuilder.cdef(
    """
typedef unsigned int SEXPTYPE;
const unsigned int NILSXP     =  0;
const unsigned int SYMSXP     =  1;
const unsigned int LISTSXP    =  2;
const unsigned int CLOSXP     =  3;
const unsigned int ENVSXP     =  4;
const unsigned int PROMSXP    =  5;
const unsigned int LANGSXP    =  6;
const unsigned int SPECIALSXP =  7;
const unsigned int BUILTINSXP =  8;
const unsigned int CHARSXP    =  9;
const unsigned int LGLSXP     = 10;
const unsigned int INTSXP     = 13;
const unsigned int REALSXP    = 14;
const unsigned int CPLXSXP    = 15;
const unsigned int STRSXP     = 16;
const unsigned int DOTSXP     = 17;
const unsigned int ANYSXP     = 18;
const unsigned int VECSXP     = 19;
const unsigned int EXPRSXP    = 20;
const unsigned int BCODESXP   = 21;
const unsigned int EXTPTRSXP  = 22;
const unsigned int WEAKREFSXP = 23;
const unsigned int RAWSXP     = 24;
const unsigned int S4SXP      = 25;
const unsigned int NEWSXP     = 30;
const unsigned int FREESXP    = 31;
const unsigned int FUNSXP     = 99;
    """)

# include/R_exts/Complex.h
ffibuilder.cdef("""
typedef struct {
    double r;
    double i;
} Rcomplex;
""")

# include/Rinternals.h
ffibuilder.cdef("""
typedef int R_len_t;
""")

if ffibuilder.sizeof('size_t') > 4:
    LONG_VECTOR_SUPPORT = True
    R_XLEN_T_MAX = 4503599627370496
    R_SHORT_LEN_MAX = 2147483647
    ffibuilder.cdef("""
typedef ptrdiff_t R_xlen_t;
    """)
else:
    ffibuilder.cdef("""
typedef int R_xlen_t;
    """)

ffibuilder.cdef("""
double R_NaN;       /* IEEE NaN */
double R_NaReal;    /* NA_REAL: IEEE */
int    R_NaInt;
""")

ffibuilder.cdef("""
typedef unsigned char Rbyte;
""")

ffibuilder.cdef(
    """
struct symsxp_struct {
    struct SEXPREC *pname;
    struct SEXPREC *value;
    struct SEXPREC *internal;
};
struct listsxp_struct {
    struct SEXPREC *carval;
    struct SEXPREC *cdrval;
    struct SEXPREC *tagval;
};
struct envsxp_struct {
    struct SEXPREC *frame;
    struct SEXPREC *enclos;
    struct SEXPREC *hashtab;
};
struct closxp_struct {
    struct SEXPREC *formals;
    struct SEXPREC *body;
    struct SEXPREC *env;
};
struct promsxp_struct {
    struct SEXPREC *value;
    struct SEXPREC *expr;
    struct SEXPREC *env;
};
typedef struct SEXPREC *SEXP;
struct sxpinfo_struct {
    SEXPTYPE type      : 5;
    unsigned int scalar: 1;
    unsigned int alt   : 1;
    unsigned int obj   : 1;
    unsigned int gp    : 16;
    unsigned int mark  : 1;
    unsigned int debug : 1;
    unsigned int trace : 1;
    unsigned int spare : 1;
    unsigned int gcgen : 1;
    unsigned int gccls : 3;
    unsigned int named : 16;
    unsigned int extra : 32;
};
struct primsxp_struct {
int offset;
};
    """)

ffibuilder.cdef(
    """
struct vecsxp_struct {
    R_xlen_t length;
    R_xlen_t truelength;
};
    """)

SEXPREC_HEADER = """
    struct sxpinfo_struct sxpinfo;
    struct SEXPREC *attrib;
    struct SEXPREC *gengc_next_node, *gengc_prev_node;
"""

ffibuilder.cdef("""
typedef struct SEXPREC {
%(SEXPREC_HEADER)s
    union {
        struct primsxp_struct primsxp;
        struct symsxp_struct symsxp;
        struct listsxp_struct listsxp;
        struct envsxp_struct envsxp;
        struct closxp_struct closxp;
        struct promsxp_struct promsxp;
    } u;
} SEXPREC;
""" % {'SEXPREC_HEADER': SEXPREC_HEADER}
)

ffibuilder.cdef("""
typedef struct {
%(SEXPREC_HEADER)s
    struct vecsxp_struct vecsxp;
} VECTOR_SEXPREC, *VECSEXP;
""" % {'SEXPREC_HEADER': SEXPREC_HEADER}
)

ffibuilder.cdef("""
typedef union {
    VECTOR_SEXPREC s;
    double align;
} SEXPREC_ALIGN;
""")

ffibuilder.cdef("""
const char *(R_CHAR)(SEXP x);
""")

# include/R_ext/Boolean.h
ffibuilder.cdef("""
typedef enum { FALSE = 0, TRUE } Rboolean;
""")

# include/Rinternals.h
ffibuilder.cdef("""
SEXP Rf_allocVector(SEXPTYPE, R_xlen_t);
SEXP Rf_protect(SEXP);
void Rf_unprotect(int);

SEXP R_NilValue;
""")

# include/Rembedded.h
ffibuilder.cdef("""
int Rf_initialize_R(int ac, char **av);
void R_RunExitFinalizers(void);
void Rf_KillAllDevices(void);
void R_CleanTempDir(void);
void Rf_endEmbeddedR(int fatal);
""")

# include/Parse.h
ffibuilder.cdef("""
typedef enum {
    PARSE_NULL,
    PARSE_OK,
    PARSE_INCOMPLETE,
    PARSE_ERROR,
    PARSE_EOF
} ParseStatus;

SEXP R_ParseVector(SEXP, int, ParseStatus *, SEXP);
""")

ffibuilder.cdef("""
SEXP Rf_install(const char *);
SEXP Rf_installChar(SEXP x);
SEXP Rf_mkChar(const char *);
SEXP Rf_mkString(const char *);
""")

ffibuilder.cdef("""
SEXP (STRING_ELT)(SEXP x, R_xlen_t i);
void SET_STRING_ELT(SEXP x, R_xlen_t i, SEXP v);
""")

ffibuilder.cdef("""
void setup_Rmainloop(void);
""")

ffibuilder.cdef("""
R_len_t Rf_length(SEXP x);
SEXP (VECTOR_ELT)(SEXP x, R_xlen_t i);
SEXP Rf_eval(SEXP, SEXP);
SEXP R_tryEval(SEXP, SEXP, int*);
SEXP R_GlobalEnv;
""")

ffibuilder.cdef("""
typedef enum {
    SA_NORESTORE,/* = 0 */
    SA_RESTORE,
    SA_DEFAULT,/* was === SA_RESTORE */
    SA_NOSAVE,
    SA_SAVE,
    SA_SAVEASK,
    SA_SUICIDE
} SA_TYPE;
""")

ffibuilder.cdef("""
extern FILE *R_Consolefile;
extern FILE *R_Outputfile;
extern void (*ptr_R_Suicide)(const char *);
extern void (*ptr_R_ShowMessage)(const char *);
extern int  (*ptr_R_ReadConsole)(const char *, unsigned char *, int, int);
extern void (*ptr_R_WriteConsole)(const char *, int);
extern void (*ptr_R_WriteConsoleEx)(const char *, int, int);
extern void (*ptr_R_ResetConsole)(void);
extern void (*ptr_R_FlushConsole)(void);
extern void (*ptr_R_ClearerrConsole)(void);
extern void (*ptr_R_Busy)(int);
extern void (*ptr_R_CleanUp)(SA_TYPE, int, int);
""")

ffibuilder.set_source("_pins_cffi", None)

if __name__ == "__main__":
    ffibuilder.compile(verbose=True)
