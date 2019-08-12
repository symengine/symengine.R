
#define R_NO_REMAP
#include <R.h>
#include <Rcpp.h>
#include <symengine/cwrapper.h>

#include "cwrapper_utils.h"
#include "rbinding.h"


#include <symengine/basic.h>

// Copied from symengine/cwrapper.cpp
struct CRCPBasic {
    SymEngine::RCP<const SymEngine::Basic> m;
};

static_assert(sizeof(CRCPBasic) == sizeof(CRCPBasic_C),
              "Size of 'basic' is not correct");
static_assert(std::alignment_of<CRCPBasic>::value
                  == std::alignment_of<CRCPBasic_C>::value,
                      "Alignment of 'basic' is not correct");

using namespace Rcpp;


//// Global variables  ////////

// These are convenient global variables to hold
// any temporary basic object inside a function.
static basic global_bholder;

int hook_lib_onload() {
    basic_new_stack(global_bholder);
    return 0;
}

// Run the hook here
static int dummy = hook_lib_onload();

//// R functions       ////////

static SEXP robj_as_list(SEXP x) {
    SEXP as_list_symbol = PROTECT(Rf_install("as.list.default"));
    SEXP call = PROTECT(Rf_lang2(as_list_symbol, x));
    SEXP ans = Rf_eval(call, R_BaseEnv);
    UNPROTECT(2);
    return ans;
}

// Related: https://github.com/RcppCore/Rcpp/issues/983
static SEXP robj_quote_lang(SEXP x) {
    if (TYPEOF(x) != SYMSXP && TYPEOF(x) != LANGSXP)
        return x;
    SEXP quot = Environment::base_env()["quote"];
    return Rf_lang2(quot, x);
}

//// SymEngine Infomation ////////////

// [[Rcpp::export()]]
SEXP cwrapper_symengine_ascii_art() {
    SEXP out = PROTECT(Rf_allocVector(STRSXP, 1));
    char* s = ascii_art_str();
    out = Rf_mkString(s);
    basic_str_free(s);
    UNPROTECT(1);
    return out;
}

// [[Rcpp::export()]]
SEXP cwrapper_symengine_version() {
    return Rf_mkString(symengine_version());
}

// [[Rcpp::export()]]
SEXP cwrapper_symengine_have_component(SEXP s) {
    const char* str = CHAR(Rf_asChar(s));
    return Rf_ScalarLogical(symengine_have_component(str));
}

// [[Rcpp::export()]]
SEXP compilation_notes() {
    return Rcpp::List::create(
        Rcpp::Named("CompilationDate") = __DATE__
    );
}


//// Rcpp types holding the external pointer. Finalizers will be called at GC.
//// Constructor should set the tag value for type identification

typedef Rcpp::XPtr<basic_struct, Rcpp::PreserveStorage, basic_free_heap,   true> XPtrBasic;
typedef Rcpp::XPtr<CVecBasic,    Rcpp::PreserveStorage, vecbasic_free,     true> XPtrVecBasic;
typedef Rcpp::XPtr<CDenseMatrix, Rcpp::PreserveStorage, dense_matrix_free, true> XPtrDenseMatrix;

//// Determine the pointer type by check the tag value

inline s4binding_t s4binding_typeof(SEXP x) {
    if (IS_S4_OBJECT(x) && R_has_slot(x, Rf_install("ptr"))) {
        SEXP p = R_do_slot(x, Rf_install("ptr"));
        s4binding_t type = (s4binding_t) RAW(R_ExternalPtrTag(p))[0];
        return type;
    }
    return S4UNKNOWN;
}

// SEXP s4binding_dim(RObject x) {
//     // Real dim
//     s4binding_t type = s4binding_typeof(x);
//     if (type == S4BASIC)
//         return R_NilValue;
//     if (type == S4VECBASIC)
//         return IntegerVector::create(s4vecbasic_size(x));
//     if (type == S4DENSEMATRIX)
//         return s4DenseMat_dim(x);
//     if (type == S3SYMARRAY) {
//         RObject d = x.attr("dim");
//         if (d == R_NilValue)
//             return ;
//         
//     }
// }

// [[Rcpp::export()]]
bool s4basic_check(SEXP x) {
    return s4binding_typeof(x) == S4BASIC;
}
// [[Rcpp::export()]]
bool s4vecbasic_check(SEXP x) {
    return s4binding_typeof(x) == S4VECBASIC;
}
// [[Rcpp::export()]]
bool s4DenseMat_check(SEXP x) {
    return s4binding_typeof(x) == S4DENSEMATRIX;
}

//// Functions to wrap the external pointer into S4 class.

// Cache and return a shallow copy of Basic object
SEXP BasicClassPrototype() {
    static SEXP BasicClassPrototype_val = NULL;
    if (BasicClassPrototype_val == NULL) {
        BasicClassPrototype_val = R_do_new_object(R_getClassDef("Basic"));
        R_PreserveObject(BasicClassPrototype_val);
    }
    return Rf_shallow_duplicate(BasicClassPrototype_val);
}

inline SEXP s4basic(basic_struct* s) {
    SEXP ans = PROTECT(BasicClassPrototype());
    ans = R_do_slot_assign(
        ans, PROTECT(Rf_install("ptr")),
        XPtrBasic(s, true, PROTECT(Rf_ScalarRaw(S4BASIC)), R_NilValue)
    );
    UNPROTECT(3);
    return ans;
}
inline S4 s4vecbasic(CVecBasic* v) {
    S4 out = S4("VecBasic");
    out.slot("ptr") = XPtrVecBasic(v, true, Rf_ScalarRaw(S4VECBASIC), R_NilValue);
    return out;
}
inline S4 s4DenseMat(CDenseMatrix* mat) {
    XPtrDenseMatrix ptr = XPtrDenseMatrix(mat, true, Rf_ScalarRaw(S4DENSEMATRIX), R_NilValue);
    S4 out = S4("DenseMatrix");
    out.slot("ptr") = ptr;
    return out;
}

//// Empty Basic and VecBasic

// [[Rcpp::export()]]
S4 s4basic() {
    basic_struct* s = basic_new_heap();
    return s4basic(s);
}
// [[Rcpp::export()]]
S4 s4vecbasic() {
    CVecBasic* vec = vecbasic_new();
    return s4vecbasic(vec);
}
// [[Rcpp::export()]]
S4 s4DenseMat() {
    CDenseMatrix* mat = dense_matrix_new();
    return s4DenseMat(mat);
}

//// Functions to extract the pointer from S4

inline
basic_struct* s4basic_elt(SEXP robj) {
    basic_struct* p = (basic_struct*) R_ExternalPtrAddr(R_do_slot(robj, Rf_install("ptr")));
    if (p == NULL) Rf_error("Invalid pointer\n");
    return p;
}
inline
CVecBasic* s4vecbasic_elt(SEXP robj) {
    CVecBasic* p = (CVecBasic*) R_ExternalPtrAddr(R_do_slot(robj, Rf_install("ptr")));
    if (p == NULL) Rf_error("Invalid pointer\n");
    return p;
}
inline
CDenseMatrix* s4DenseMat_elt(SEXP robj) {
    CDenseMatrix* p = (CDenseMatrix*) R_ExternalPtrAddr(R_do_slot(robj, Rf_install("ptr")));
    if (p == NULL) Rf_error("Invalid pointer\n");
    return p;
}

//// R level accessor functions

// [[Rcpp::export()]]
SEXP s4basic_get_type(SEXP robj) {
    TypeID type_id = basic_get_type(s4basic_elt(robj));
    char* classname = basic_get_class_from_id(type_id);
    SEXP ans = Rf_mkString(classname);
    basic_str_free(classname);
    return ans;
}
// [[Rcpp::export()]]
String s4basic_str(SEXP robj) {
    char* str = basic_str_julia(s4basic_elt(robj));
    String out = String(str);
    basic_str_free(str);
    return out;
}
// [[Rcpp::export()]]
String s4basic_hash(SEXP robj) {
    size_t hash = basic_hash(s4basic_elt(robj));
    // Convert to string
    char str[256] = "";
    snprintf(str, sizeof(str), "%zu", hash);
    return str;
}
// [[Rcpp::export()]]
bool s4basic_eq(S4 a, S4 b) {
    return basic_eq(s4basic_elt(a), s4basic_elt(b));
}
// [[Rcpp::export()]]
bool s4basic_neq(S4 a, S4 b) {
    return basic_neq(s4basic_elt(a), s4basic_elt(b));
}

// [[Rcpp::export()]]
S4 s4basic_get_args(S4 s) {
    CVecBasic* vec = vecbasic_new();
    cwrapper_hold(basic_get_args(s4basic_elt(s), vec));
    return s4vecbasic(vec);
}

static inline
S4 wrap_basic_to_setbasic_function(CWRAPPER_OUTPUT_TYPE (* func)(const basic_struct*, CSetBasic*),
                                   S4 s) {
    CSetBasic* set = setbasic_new();
    CVecBasic* vec = vecbasic_new(); 
    CWRAPPER_OUTPUT_TYPE status1 = func(s4basic_elt(s), set);
    // Convert setbasic to vecbasic
    CWRAPPER_OUTPUT_TYPE status2 = cwrapper_set2vec(set, vec);
    
    // Handle the exception until freeing "set" and wrapping "vec"
    setbasic_free(set);
    S4 out = s4vecbasic(vec);
    cwrapper_hold(status1);
    cwrapper_hold(status2);
    return out;
}

// [[Rcpp::export()]]
S4 s4basic_free_symbols(S4 s) {
    return wrap_basic_to_setbasic_function(basic_free_symbols, s);
}

// [[Rcpp::export()]]
S4 s4basic_function_symbols(S4 s) {
    auto basic_function_symbols_fix = [](const basic_struct* a, CSetBasic* b) {
        return basic_function_symbols(b, a);
    };
    return wrap_basic_to_setbasic_function(basic_function_symbols_fix, s);
}

// [[Rcpp::export()]]
SEXP s4basic_function_getname(S4 s) {
    basic_struct* b = s4basic_elt(s);
    if (basic_get_type(b) != SYMENGINE_FUNCTIONSYMBOL)
        Rf_error("Not a function symbol");
    char* str = function_symbol_get_name(b);
    SEXP ans = Rf_mkString(str);
    basic_str_free(str);
    return ans;
}


// [[Rcpp::export()]]
int s4basic_realmpfr_get_prec(S4 robj) {
#ifdef HAVE_SYMENGINE_MPFR
    basic_struct* s = s4basic_elt(robj);
    mpfr_prec_t prec = real_mpfr_get_prec(s);
    return prec;
#endif
    Rf_error("The library is not compiled with MPFR support\n");
}

// [[Rcpp::export()]]
bool s4basic_is_Number(S4 robj) {
    return is_a_Number(s4basic_elt(robj));
}

// [[Rcpp::export()]]
bool s4basic_number_is_negative(RObject robj) {
    return number_is_negative(s4basic_elt(robj));
}
// [[Rcpp::export()]]
bool s4basic_number_is_positive(RObject robj) {
    return number_is_positive(s4basic_elt(robj));
}
// [[Rcpp::export()]]
bool s4basic_number_is_complex(RObject robj) {
    return number_is_complex(s4basic_elt(robj));
}
// [[Rcpp::export()]]
bool s4basic_number_is_zero(RObject robj) {
    return number_is_zero(s4basic_elt(robj));
}

//// Convert R object to Basic

CWRAPPER_OUTPUT_TYPE
cwrapper_basic_parse(basic_struct* s, RObject robj, bool check_whole_number) {
    if (robj.sexp_type() == STRSXP) {
        CharacterVector rstr = as<CharacterVector>(robj);
        
        if (rstr.size() != 1)
            Rf_error("Can only parse scalar data\n");
        
        // We should not accept string NA?
        if (CharacterVector::is_na(rstr[0]))
            Rf_error("Can not parse NA_character_\n");
        const char* cstr = String(rstr).get_cstring();
        return basic_parse2(s, cstr, 1);
    }
    if (robj.sexp_type() == REALSXP) {
        NumericVector rnum = as<NumericVector>(robj);
        
        if (rnum.size() != 1)
            Rf_error("Can only parse scalar data\n");
        // NA
        if (R_IsNA(rnum[0])) {
            Rf_error("Can not parse NA_real_");
        }
        // NaN
        if (R_IsNaN(rnum[0])) {
            basic_const_nan(s);
            return SYMENGINE_NO_EXCEPTION;
        }
        if (rnum[0] == R_PosInf) {
            basic_const_infinity(s);
            return SYMENGINE_NO_EXCEPTION;
        }
        if (rnum[0] == R_NegInf) {
            basic_const_neginfinity(s);
            return SYMENGINE_NO_EXCEPTION;
        }
        if (check_whole_number) {
            // Check if double is an integer, then assign the basic with string
            if (floor(rnum[0]) == ceil(rnum[0])) {
                char cnum_str[256] = "";
                int cnum_str_size = snprintf(cnum_str, sizeof(cnum_str), "%0.0f", rnum[0]);
                if (cnum_str_size >= sizeof(cnum_str))
                    Rf_error("Can not handle string size of %d\n", cnum_str_size);
                return integer_set_str(s, cnum_str);
            }
        }
        return real_double_set_d(s, rnum[0]);
    }
    // Factor is also INTSXP, Rf_isInteger can distinguish factor and integer
    if (Rf_isInteger(robj)) {
        IntegerVector rint = as<IntegerVector>(robj);
        
        if (rint.size() != 1)
            Rf_error("Can only parse scalar data\n");
        
        if (rint[0] == NA_INTEGER) {
            Rf_error("Can not parse NA_integer_");
            //basic_const_nan(s);
            //return SYMENGINE_NO_EXCEPTION;
        }
        int cint = rint[0];
        return integer_set_si(s, cint);
    }
    
    if (s4basic_check(robj)) {
        return basic_assign(s, s4basic_elt(robj));
    }
    
    if (s4vecbasic_check(robj)) {
        if (s4vecbasic_size(robj) != 1)
            Rf_error("Can only accept length-one VecBasic\n");
        return basic_assign(s, s4basic_elt(s4vecbasic_get(robj, 1)));
    }
    
    if (TYPEOF(robj) == EXPRSXP || TYPEOF(robj) == LANGSXP || TYPEOF(robj) == SYMSXP) {
        Function s4basic_parse_language =
            Environment::namespace_env("symengine")["s4basic_parse_language"];
        // Refer: https://github.com/RcppCore/Rcpp/issues/983
        RObject ans = s4basic_parse_language(robj_quote_lang(robj));
        return basic_assign(s, s4basic_elt(ans));
    }
    
    if (robj.isNULL())
        Rf_error("Can not parse NULL\n");
    
    Rf_error("Can not convert to Basic (SEXP type: %d)\n", robj.sexp_type());
}

// [[Rcpp::export()]]
SEXP s4basic_parse(RObject robj, bool check_whole_number = false) {
    
    // Return directly if it is already a Basic
    if (s4basic_check(robj)) return robj;
    
    if (s4vecbasic_check(robj) && s4vecbasic_size(robj) == 1)
        return s4vecbasic_get(robj, 1);
    
    basic_struct*  s = basic_new_heap();
    S4 out = s4basic(s);
    
    cwrapper_hold(cwrapper_basic_parse(s, robj, check_whole_number));
    return out;
}

// [[Rcpp::export()]]
S4 s4basic_symbol(SEXP robj) {
    s4binding_t type = s4binding_typeof(robj);
    if (type == S4BASIC) {
        if (basic_get_type(s4basic_elt(robj)) != SYMENGINE_SYMBOL)
            Rf_error("Input is not a SYMBOL\n");
        return robj;
    }
    // If input is a scalar character vector, construct as Symbol,
    // otherwise parser may recognize it as a Constant.
    if (IS_SCALAR(robj, STRSXP)) {
        if (Rf_asChar(robj) == NA_STRING)
            Rf_error("Can not accept NA_character_\n");
        const char* str = CHAR(Rf_asChar(robj));
        S4 ans = s4basic();
        cwrapper_hold(
            symbol_set(s4basic_elt(ans), str)
        );
        return ans;
    }
    S4 ans = s4basic_parse(robj, false);
    if (basic_get_type(s4basic_elt(ans)) != SYMENGINE_SYMBOL)
        Rf_error("Unable to parse input as a SYMBOL\n");
    return ans;
}


// [[Rcpp::export()]]
S4 s4basic_const(CharacterVector robj) {
    basic_struct* s = basic_new_heap();
    S4 out = s4basic(s);
    if (robj.size() != 1)
        Rf_error("Input must be length-one character vector\n");
    if (CharacterVector::is_na(robj[0]))
        Rf_error("Can not accept NA_character_\n");
    const char* cstr = String(robj).get_cstring();
    // void function
    basic_const_set(s, cstr);
    return out;
}

// [[Rcpp::export()]]
S4 s4basic_function(String name, SEXP args) {
    CVecBasic* args_elt = s4vecbasic_elt(args);
    S4 ans = s4basic();
    cwrapper_hold(
        function_symbol_set(s4basic_elt(ans), name.get_cstring(), args_elt)
    );
    return ans;
}

CWRAPPER_OUTPUT_TYPE cwrapper_real_mpfr_set_d(basic s, double d, int prec) {
#ifdef HAVE_SYMENGINE_MPFR
    return real_mpfr_set_d(s, d, prec);
#endif
    Rf_error("The library is not compiled with MPFR support\n");
};

CWRAPPER_OUTPUT_TYPE cwrapper_real_mpfr_set_str(basic s, const char *c, int prec) {
#ifdef HAVE_SYMENGINE_MPFR
    return real_mpfr_set_str(s, c, prec);
#endif
    Rf_error("The library is not compiled with MPFR support\n");
};

// [[Rcpp::export()]]
S4 s4basic_real(RObject robj, RObject prec = R_NilValue) {
    basic_struct*  s = basic_new_heap();
    S4 out = s4basic(s);
    
    // robj can be either integer, double or string
    // prec should be NULL or integer
    if (is<NumericVector>(robj)) {
        NumericVector rnum = as<NumericVector>(robj);
        if (rnum.size() != 1)
            Rf_error("Length of input must be one\n");
        double rnum_double = rnum[0];
        if (prec == R_NilValue) {
            cwrapper_hold(real_double_set_d(s, rnum_double));
            return out;
        }
        else {
            cwrapper_hold(cwrapper_real_mpfr_set_d(s, rnum_double, as<int>(prec)));
            return out;
        }
    }
    if (is<CharacterVector>(robj)) {
        if (prec == R_NilValue) {
            // FIXME: check the parsed value is indeed a RealDouble or RealMPFR
            // What to do if the result is a Integer??
            return s4basic_parse(robj); // Let the parser determine precision
        }
        else {
            CharacterVector rstr = as<CharacterVector>(robj);
            if (rstr.size() != 1)
                Rf_error("Length of input must be one\n");
            const char* cstr = String(rstr).get_cstring();
            cwrapper_hold(cwrapper_real_mpfr_set_str(s, cstr, as<int>(prec)));
            return out;
        }
    }
    if (is<IntegerVector>(robj)) {
        // Convert them into Numeric Vector and call this function
        NumericVector rnum_castint = as<NumericVector>(robj);
        if (rnum_castint.size() != 1)
            Rf_error("Length of input must be one\n");
        return s4basic_real(rnum_castint, prec);
    }
    if (s4basic_check(robj)) {
        basic_struct* b = s4basic_elt(robj);
        // Return directly if input is Real and prec is not specified
        if (prec == R_NilValue)
            if (is_a_RealDouble(b) || is_a_RealMPFR(b))
                return as<S4>(robj);
        S4 out = s4basic();
        cwrapper_hold(basic_evalf(s4basic_elt(out), b, as<int>(prec), true));
        return out;
    }
    Rf_error("Not implemented for SEXP type %d\n", robj.sexp_type());
}

// [[Rcpp::export()]]
S4 s4basic_subs(SEXP expr, SEXP a, SEXP b) {
    // TODO: support VecBasic and DenseMatrix
    S4 expr2 = s4basic_parse(expr, false);
    S4 a2    = s4basic_parse(a, false);
    S4 b2    = s4basic_parse(b, false);
    S4 ans   = s4basic();
    cwrapper_hold(
        basic_subs2(s4basic_elt(ans), s4basic_elt(expr2), s4basic_elt(a2), s4basic_elt(b2))
    );
    return ans;
}

//// Convert Basic class to R object (i.e. double)

// [[Rcpp::export()]]
SEXP s4basic_as_sexp(S4 robj) {
    const basic_struct* s = s4basic_elt(robj);
    
    if (is_a_RealDouble(s)) {
        double cdouble = real_double_get_d(s);
        return Rf_ScalarReal(cdouble);
    }
    if (is_a_Integer(s)) {
        signed long cint = integer_get_si(s);
        // Note that INT_MIN is used as NA_integer_ in R
        if (cint >= (INT_MIN + 1) && cint <= INT_MAX)
            return Rf_ScalarInteger(cint);
        else
            Rf_error("Number %ld can not be coerced to integer range\n", cint);
    }
    if (is_a_RealMPFR(s)) {
#ifdef HAVE_SYMENGINE_MPFR
        // Round to double precision
        double cdouble = real_mpfr_get_d(s);
        return Rf_ScalarReal(cdouble);
#endif
        Rf_error("Should not happen\n");
    }
    if (is_a_Rational(s)) {
        // TODO (use evalf?)
    }
    if (basic_get_type(s) == SYMENGINE_CONSTANT) {
        // TODO (use evalf?)
        Rprintf("not implemented for constant\n");
    }
    if (is_a_Complex(s) || is_a_ComplexDouble(s) || is_a_ComplexMPC(s)) {
        // TODO
    }
    
    const char* cstr = String(s4basic_get_type(robj)).get_cstring();
    Rf_error("Not implemented for %s\n", cstr);
}


//// Functions for wrapping VecBasic =====================================

// [[Rcpp::export()]]
SEXP s4vecbasic_get(RObject robj, int idx) {
    CVecBasic* c_vec = s4vecbasic_elt(robj);
    size_t idx_bound = vecbasic_size(c_vec);
    int c_idx = idx - 1;
    if (c_idx >= idx_bound)
        Rf_error("Index out of bounds\n");
    S4 ans = s4basic();
    basic_struct* s = s4basic_elt(ans);
    cwrapper_hold(vecbasic_get(c_vec, c_idx, s));
    return ans;
}

static inline bool robj_is_simple(SEXP x) {
    // "simple" object should be able to be parsed as Basic
    
    switch(TYPEOF(x)) {
    // EXPRSXP/LANGSXP is a "Vector" with length >= 1,
    // but it should be treated as a scalar.
    case EXPRSXP:
    case LANGSXP:
    case SYMSXP:
        return true;
    case LGLSXP:
    case INTSXP:
    case REALSXP:
    case CPLXSXP:
    case STRSXP:
        if (Rf_length(x) == 1)
            return true;
        else
            return false;
    case VECSXP:
        return false;
    }
    return false;
}

// This will modify it in place. Be careful when using it at R level functions.
// [[Rcpp::export()]]
void s4vecbasic_mut_append(S4 vec, RObject robj) {
    CVecBasic* self = s4vecbasic_elt(vec);
    s4binding_t type = s4binding_typeof(robj);
    if (type == S4BASIC) {
        cwrapper_hold(vecbasic_push_back(self, s4basic_elt(robj)));
        return;
    }
    if (type == S4VECBASIC) {
        cwrapper_hold(cwrapper_vec_append_vec(self, s4vecbasic_elt(robj), -1));
        return;
    }
    if (type == S4DENSEMATRIX) {
        Rf_error("DenseMatrix is not supported\n");
    }
    if (robj_is_simple(robj)) {
        cwrapper_hold(cwrapper_basic_parse(global_bholder, robj, false));
        cwrapper_hold(vecbasic_push_back(self, global_bholder));
        return;
    }
    
    // Convert it to a list and parse each
    switch(TYPEOF(robj)) {
    case LGLSXP:
    case INTSXP:
    case REALSXP:
    case CPLXSXP:
    case STRSXP:
    case VECSXP:
        if (Rf_length(robj) == 0)
            return;
        break;
    default:
        Rf_error("Unrecognized type\n");
    }

    List robj_list = robj_as_list(robj);
    for (int i = 0; i < robj_list.size(); i++) {
        RObject el = robj_list[i];
        // s4basic_parse will check the length of each element to be one
        cwrapper_hold(cwrapper_basic_parse(global_bholder, el, false));
        cwrapper_hold(vecbasic_push_back(self, global_bholder));
    }
    return;
}

// [[Rcpp::export()]]
void s4vecbasic_mut_set(S4 self, int idx, S4 rval) {
    CVecBasic*    vec = s4vecbasic_elt(self);
    basic_struct* val = s4basic_elt(rval);
    cwrapper_hold(vecbasic_set(vec, idx - 1, val));
    return;
}

// [[Rcpp::export()]]
size_t s4vecbasic_size(SEXP robj) {
    CVecBasic* vec = s4vecbasic_elt(robj);
    size_t sz = vecbasic_size(vec);
    if (sz <= INT_MAX)
        return sz;
    else
        Rf_error("Exceeding INTMAX\n");
}

// [[Rcpp::export()]]
S4 s4vecbasic_unique(SEXP robj) {
    CVecBasic* vec  = s4vecbasic_elt(robj);
    size_t     len  = vecbasic_size(vec);
    CSetBasic*    set_holder    = setbasic_new();
    // TODO: Move this to cwrapper_vec2set?
    basic basic_holder;
    basic_new_stack(basic_holder);
    for (size_t i = 0; i < len; i++) {
        CWRAPPER_OUTPUT_TYPE status = vecbasic_get(vec, i, basic_holder);
        setbasic_insert(set_holder, basic_holder);
        if (status) {
            setbasic_free(set_holder);
            basic_free_stack(basic_holder);
            cwrapper_hold(status);
        }
    }
    basic_free_stack(basic_holder);
    
    S4 ans = s4vecbasic();
    CWRAPPER_OUTPUT_TYPE status2 = cwrapper_set2vec(set_holder, s4vecbasic_elt(ans));
    setbasic_free(set_holder);
    cwrapper_hold(status2);
    return ans;
}

//// Functions for wrapping DenseMatrix ==================================

// [[Rcpp::export()]]
S4 s4DenseMat_byrow(RObject robj, unsigned nrow, unsigned ncol) {
    // TODO: add byrow argument?
    // if (!byrow)
    //     Rf_error("not implemented");
    
    // TODO: support input of a list of VecBasic or a list of Basic
    // TODO: support matrix input
    if (robj.isNULL()) {
        // Do we need to fill with default value?
        CDenseMatrix* mat = dense_matrix_new_rows_cols(nrow, ncol);
        return s4DenseMat(mat);
    }
    if (s4vecbasic_check(robj)) {
        CVecBasic* vec = s4vecbasic_elt(robj);
        if (vecbasic_size(vec) != nrow * ncol)
            Rf_error("Length of vector (%zu) does not match with matrix size (%d x %d)\n",
                     vecbasic_size(vec), nrow, ncol);
        CDenseMatrix* mat = dense_matrix_new_vec(nrow, ncol, vec);
        return s4DenseMat(mat);
    }
    if (s4basic_check(robj)) {
        basic_struct* val = s4basic_elt(robj);
        S4 out = s4DenseMat(dense_matrix_new_rows_cols(nrow, ncol)); // Empty matrix
        CDenseMatrix* mat = s4DenseMat_elt(out);
        for (size_t row = 0; row < nrow; row++)
            for (size_t col = 0; col < ncol; col++)
                cwrapper_hold(dense_matrix_set_basic(mat, row, col, val));
        return out;
    }
    Rf_error("Not implemented\n");
}

// [[Rcpp::export()]]
S4 s4DenseMat_transpose(SEXP robj) {
    CDenseMatrix* mat_ori = s4DenseMat_elt(robj);
    CDenseMatrix* mat_new = dense_matrix_new();
    S4 out = s4DenseMat(mat_new);
    cwrapper_hold(dense_matrix_transpose(mat_new, mat_ori));
    return out;
}

// [[Rcpp::export()]]
SEXP s4DenseMat_str(S4 robj) {
    CDenseMatrix* mat = s4DenseMat_elt(robj);
    char* str = dense_matrix_str(mat);
    SEXP ans = Rf_mkString(str);
    basic_str_free(str);
    return ans;
}

// [[Rcpp::export()]]
S4 s4DenseMat_copy(S4 robj) {
    CDenseMatrix* mat = dense_matrix_new();;
    S4 out = s4DenseMat(mat);
    // Copy the matrix
    cwrapper_hold(dense_matrix_set(mat, s4DenseMat_elt(robj)));
    return out;
}

// [[Rcpp::export()]]
IntegerVector s4DenseMat_dim(SEXP robj) {
    CDenseMatrix* mat  = s4DenseMat_elt(robj);
    size_t        nrow = dense_matrix_rows(mat);
    size_t        ncol = dense_matrix_cols(mat);
    if (nrow > INT_MAX || ncol > INT_MAX)
        Rf_error("Exceeding INT_MAX\n");
    IntegerVector ans(2);
    ans[0] = nrow;
    ans[1] = ncol;
    return ans;
}

// [[Rcpp::export()]]
S4 s4DenseMat_get(S4 robj, IntegerVector rows, IntegerVector cols, bool get_basic = false) {
    /* Default: get_basic=false */
    CDenseMatrix* mat = s4DenseMat_elt(robj);
    
    if (get_basic) {
        // TODO: check NA, check length
        if (rows.size() != 1 || cols.size() != 1)
            Rf_error("Expecting size to be 1\n");
        int row = rows[0];
        int col = cols[0];
        basic_struct* s = basic_new_heap();
        S4 out = s4basic(s);
        cwrapper_hold(dense_matrix_get_basic(s, mat, row - 1, col - 1));
        return out;
    }
    
    size_t len = rows.size();
    if (len != cols.size())
        Rf_error("Index sizes do not match\n");
    
    // TODO: check bounds of rows and cols
    
    S4         out  = s4vecbasic();
    CVecBasic* outv = s4vecbasic_elt(out);
    for (int i = 0; i < len; i++) {
        cwrapper_hold(dense_matrix_get_basic(global_bholder, mat, rows[i] - 1, cols[i] -1));
        cwrapper_hold(vecbasic_push_back(outv, global_bholder));
    }
    return out;
}

// [[Rcpp::export()]]
void s4DenseMat_mut_setbasic(S4 rmat, int row, int col, RObject value) {
    // TODO: Implement s4DenseMat_mut_set that sets a list of values at once
    // TODO: check index is not out of bounds
    if (row <= 0 || col <= 0)
        Rf_error("Index can not be negative or zero\n");
    CDenseMatrix* mat = s4DenseMat_elt(rmat);
    S4 rbasic;
    if (!s4basic_check(value))
        rbasic = s4basic_parse(value, false);
    else
        rbasic = as<S4>(value);
    basic_struct* cbasic = s4basic_elt(rbasic);
    cwrapper_hold(dense_matrix_set_basic(mat, row - 1, col - 1, cbasic));
    return;
}

// [[Rcpp::export()]]
void s4DenseMat_mut_addcols(RObject A, RObject B) {
    CDenseMatrix* self = s4DenseMat_elt(A);
    CDenseMatrix* value = s4DenseMat_elt(B);
    size_t nrow_self  = dense_matrix_rows(self);
    size_t nrow_value = dense_matrix_rows(value);
    if (nrow_self != nrow_value)
        Rf_error("Number of rows not equal (%d != %d)\n", nrow_self, nrow_value);
    cwrapper_hold(dense_matrix_row_join(self, value));
    return;
}
// [[Rcpp::export()]]
void s4DenseMat_mut_addrows(RObject A, RObject B) {
    CDenseMatrix* self = s4DenseMat_elt(A);
    CDenseMatrix* value = s4DenseMat_elt(B);
    size_t ncol_self  = dense_matrix_cols(self);
    size_t ncol_value = dense_matrix_cols(value);
    if (ncol_self != ncol_value)
        Rf_error("Number of cols not equal (%d != %d)\n", ncol_self, ncol_value);
    cwrapper_hold(dense_matrix_col_join(self, value));
    return;
}

// [[Rcpp::export()]]
S4 s4DenseMat_mul_matrix(RObject a, RObject b) {
    S4 ans = s4DenseMat();
    CDenseMatrix* a_elt = s4DenseMat_elt(a);
    CDenseMatrix* b_elt = s4DenseMat_elt(b);
    if (dense_matrix_cols(a_elt) != dense_matrix_rows(b_elt))
        Rf_error("Matrixs are non-comformable\n");
    cwrapper_hold(
        dense_matrix_mul_matrix(s4DenseMat_elt(ans), a_elt, b_elt)
    );
    return ans;
}

// [[Rcpp::export()]]
S4 s4DenseMat_det(RObject m) {
    S4 ans = s4basic();
    cwrapper_hold(
        dense_matrix_det(s4basic_elt(ans), s4DenseMat_elt(m))
    );
    return ans;
}

// [[Rcpp::export()]]
S4 s4DenseMat_inv(RObject m) {
    S4 ans = s4DenseMat();
    cwrapper_hold(
        dense_matrix_inv(s4DenseMat_elt(ans), s4DenseMat_elt(m))
    );
    return ans;
}
// [[Rcpp::export()]]
S4 s4DenseMat_LU_solve(RObject a, RObject b) {
    S4 ans = s4DenseMat();
    cwrapper_hold(
        dense_matrix_LU_solve(s4DenseMat_elt(ans), s4DenseMat_elt(a), s4DenseMat_elt(b))
    );
    return ans;
}

//// Generic methods for Basic, VecBasic, DenseMatrix

SEXP s4binding_wrap(void* p, s4binding_t type) {
    switch(type) {
    case S4BASIC:
        return s4basic((basic_struct*) p);
    case S4VECBASIC:
        return s4vecbasic((CVecBasic*) p);
    case S4DENSEMATRIX:
        return s4DenseMat((CDenseMatrix*) p);
    default:
        Rf_error("Unrecognized type\n");
    }
}

void* s4binding_elt(SEXP robj) {
    // TODO: maybe construct a struct to add type information to the pointer
    s4binding_t type = s4binding_typeof(robj);
    void* p = R_ExternalPtrAddr(R_do_slot(robj, Rf_mkString("ptr")));
    if (p == NULL) Rf_error("Invalid pointer\n");
    return p;
}

// [[Rcpp::export()]]
int s4binding_size(SEXP robj) {
    s4binding_t type = s4binding_typeof(robj);
    if (type == S4BASIC)
        return 1;
    if (type == S4VECBASIC)
        return s4vecbasic_size(robj);
    if (type == S4DENSEMATRIX) {
        IntegerVector dim = s4DenseMat_dim(robj);
        size_t ans = dim[0] * dim[1];
        if (ans > INT_MAX)
            Rf_error("Exceeding INTMAX: %zu\n", ans);
        return ((int)ans);
    }
    // In this case, assume it is a atomic vector or a list
    if (Rf_isVector(robj))
        return Rf_length(robj);
    
    Rf_error("Unrecognized type\n");
}


// TODO:: Move the actual implementation of s4binding_subset to here.
// CWRAPPER_OUTPUT_TYPE cwrapper_binding_getbasic(void* ptr, s4binding_t type, int idx,
//                                                basic_struct* val);

// Returns a VecBasic or extract a Basic
// [[Rcpp::export()]]
S4 s4binding_subset(SEXP robj, IntegerVector idx, bool get_basic) {
    // This idx should only accept positive integer
    
    if (s4DenseMat_check(robj)) {
        // TODO: add test case for this
        // Treat DenseMatrix as if `as(mat, "VecBasic")`
        size_t nrow = dense_matrix_rows(s4DenseMat_elt(robj));
        IntegerVector rows(idx.size());
        IntegerVector cols(idx.size());
        for (int i = 0; i < idx.size(); i++) {
            cols[i] = (idx[i] - 1)/nrow + 1;
            rows[i] = (idx[i] - 1)%nrow + 1;
        }
        return s4DenseMat_get(robj, rows, cols, get_basic);
    }
    
    // Treat Basic as if it is a length-one VecBasic
    if (s4basic_check(robj)) {
        if (get_basic) {
            if (idx.size() != 1 || idx[0] != 1) {
                Rf_error("Invalid getting for Basic\n");
            }
            return robj;  
        }
        S4 ans = s4vecbasic();
        CVecBasic* vec_ans = s4vecbasic_elt(ans);
        basic_struct* b = s4basic_elt(robj);
        for (int i = 0; i < idx.size(); i++) {
            if (idx[i] != 1) Rf_error("Index out of bounds\n");
            cwrapper_hold(vecbasic_push_back(vec_ans, b));
        }
        return ans;
    }
    if (s4vecbasic_check(robj)) {
        if (get_basic) {
            if (idx.size() != 1)
                Rf_error("Invalid getting for VecBasic");
            return s4vecbasic_get(robj, idx[0]);
        }
        CVecBasic* cvec = s4vecbasic_elt(robj);
        size_t idx_bound = vecbasic_size(cvec);
        S4 ans = s4vecbasic();
        CVecBasic* vec_ans = s4vecbasic_elt(ans);
        for (int i = 0; i < idx.size(); i++) {
            int c_idx = idx[i] - 1;
            if (c_idx >= idx_bound)
                Rf_error("Index out of bound\n");
            cwrapper_hold(cwrapper_vec_append_vec(vec_ans, cvec, c_idx));
        }
        return ans;
    }
    
    // TODO: if input is a R vector, parse it as a VecBasic or a Basic
    
    Rf_error("Unrecognized type\n");
}

//// Parse as Basic or VecBasic, depending on the length
//// Matrix or array will be ignored
// [[Rcpp::export()]]
SEXP s4binding_parse(RObject robj) {
    s4binding_t type = s4binding_typeof(robj);
    if (type == S4BASIC || type == S4VECBASIC || type == S4DENSEMATRIX)
        return robj;
    
    if (robj_is_simple(robj))
        return s4basic_parse(robj, false);
    
    // Convert to VecBasic
    S4 ans = s4vecbasic();
    s4vecbasic_mut_append(ans, robj);
    return ans;
}

////========  Two argument functions ===============

// [[Rcpp::export()]]
SEXP s4binding_dummy(int level) {
    if (level == 1)
        return R_NilValue;
    if (level == 2) {
        basic_struct* bl[100];
        for (int i = 0; i < 100; i++) {
            bl[i] = basic_new_heap();
        }
        for (int n = 0; n < 100; n++) {
            basic_free_heap(bl[n]);
        }
        return R_NilValue;
    }
    if (level == 3) {
        for (int m = 0; m < 100; m++) {
            XPtrBasic(basic_new_heap(), true, Rf_ScalarRaw(S4BASIC), R_NilValue);
        }
        return R_NilValue;
    }
    if (level == 4) {
        SEXP sexp_list[100];
        for (int i = 0; i < 100; i++) {
            sexp_list[i] = PROTECT(s4basic());
        }
        UNPROTECT(100);
        return R_NilValue;
    }
    Rf_error("emmm\n");
}

typedef CWRAPPER_OUTPUT_TYPE cwrapper_op_t(basic, const basic, const basic);

// // util functions
CWRAPPER_OUTPUT_TYPE cwrapper_binding_ntheory_binomial(basic s,
                                                       const basic a, const basic b) {
    // TODO: check Integer type
    unsigned long b_val = integer_get_ui(b);
    return ntheory_binomial(s, a, b_val);
}

cwrapper_op_t* op_lookup(const char* key) {
    
    typedef struct {
        const char* key;
        cwrapper_op_t* val;
    } cwrapper_op_mapping_t;
    
    static const cwrapper_op_mapping_t op_lookup_table[] = {
        //========= Members of Arith Group Generics =======
        {"+"    , basic_add       },
        {"-"    , basic_sub       },
        {"*"    , basic_mul       },
        {"/"    , basic_div       },
        {"^"    , basic_pow       },
        {"%%"   , ntheory_mod     },
        {"%/%"  , ntheory_quotient},
        
        //========= Others ================================
        {"diff" , basic_diff       },
        {"gcd"  , ntheory_gcd      },  // Greatest Common Divisor
        {"lcm"  , ntheory_lcm      },  // Least Common Multiple
        
        {"binomial", cwrapper_binding_ntheory_binomial},
    };
    
    const int table_len = sizeof(op_lookup_table) / sizeof(cwrapper_op_mapping_t);
    for (int i = 0; i < table_len; i++) {
        cwrapper_op_mapping_t entry = op_lookup_table[i];
        if (strcmp(key, entry.key) == 0)
            return entry.val;
    }
    Rf_error("op_lookup failed to find '%s'\n", key);
}

// [[Rcpp::export()]]
S4 s4binding_op(SEXP robj1, SEXP robj2, const char* op_key) {
    // How to implement:
    // e1 and e2 can be: Basic, VecBasic, DenseMatrix, R vector (including list).
    // However, vectors with dim, i.e. matrix or array, are not supported.
    // 1. Check if e1 are e2 are both scalar or Basic, return Basic
    // 2. If DenseMatrix is not involved, return VecBasic
    // 3. If DenseMatrix is involved, convert the result in step 2 to DenseMatrix
    
    //const char* op_key = String(op).get_cstring();
    
    cwrapper_op_t* op_wrapped = op_lookup(op_key);
    
    RObject e1 = s4binding_parse(robj1);
    RObject e2 = s4binding_parse(robj2);
    s4binding_t t1 = s4binding_typeof(e1);
    s4binding_t t2 = s4binding_typeof(e2);
    
    if (t1 == S4BASIC && t2 == S4BASIC) {
        S4 ans = s4basic();
        cwrapper_hold(
            op_wrapped(s4basic_elt(ans), s4basic_elt(e1), s4basic_elt(e2))
        );
        return ans;
    }
    
    int sz1 = s4binding_size(e1);
    int sz2 = s4binding_size(e2);
    int sz_ans = sz1 >= sz2 ? sz1 : sz2;
    
    S4 ans = s4vecbasic();
    if (sz_ans != 0) {
        if (sz_ans%sz1 != 0 || sz_ans%sz2 != 0)
            Rf_warning("Length of answer is not a multiple of length of input\n");
        IntegerVector idx1 = rep_len(seq_len(sz1), sz_ans);
        IntegerVector idx2 = rep_len(seq_len(sz2), sz_ans);
        
        CVecBasic* ans_ptr = s4vecbasic_elt(ans);
        for (int i = 0; i < sz_ans; i++) {
            S4 e1_basic = s4binding_subset(e1, Rf_ScalarInteger(idx1[i]), true);
            S4 e2_basic = s4binding_subset(e2, Rf_ScalarInteger(idx2[i]), true);
            cwrapper_hold(op_wrapped(global_bholder, s4basic_elt(e1_basic), s4basic_elt(e2_basic)));
            cwrapper_hold(vecbasic_push_back(ans_ptr, global_bholder));
        }
    }
    
    // Convert ans back to DenseMatrix if necessary
    if (t1 == S4DENSEMATRIX || t2 == S4DENSEMATRIX) {
        int ans_nrow, ans_ncol;
        if (t1 == S4DENSEMATRIX && t2 == S4DENSEMATRIX) {
            IntegerVector dim1 = s4DenseMat_dim(e1);
            IntegerVector dim2 = s4DenseMat_dim(e2);
            if (!((dim1[0] == dim2[0]) && (dim1[1] == dim2[1])))
                Rf_warning("Dimensions of Matrix input are not identical\n");
        }
        IntegerVector ans_dim;
        if (t1 == S4DENSEMATRIX)
            ans_dim = s4DenseMat_dim(e1);
        else
            ans_dim = s4DenseMat_dim(e2);
        ans_nrow = ans_dim[0];
        ans_ncol = ans_dim[1];
        // Fill by row and then transpose
        ans = s4DenseMat_byrow(as<RObject>(ans), ans_ncol, ans_nrow);
        ans = s4DenseMat_transpose(as<RObject>(ans));
        return ans;
    }
    
    return ans;
}


////========  One argument functions ===============

// util functions
CWRAPPER_OUTPUT_TYPE cwrapper_binding_ntheory_factorial(basic s, const basic n) {
    // TODO: check Integer type
    unsigned long n_val = integer_get_ui(n);
    return ntheory_factorial(s, n_val);
}

typedef CWRAPPER_OUTPUT_TYPE cwrapper_math_t(basic, const basic);

cwrapper_math_t* math_lookup(const char* key) {
    
    typedef struct {
        const char* key;
        cwrapper_math_t* val;
    } cwrapper_math_mapping_t;
    
    static const cwrapper_math_mapping_t math_lookup_table[] = {
        // ======== Memebers of Math groupGenerics ==========
        // Missing ones:
        //     "sign", "ceiling", "floor", "trunc", "cummax", "cummin",
        // TODO: implement "cumprod" and "cumsum" in another function
        {"abs",   basic_abs},
        {"sqrt",  basic_sqrt},
        {"exp",   basic_exp},
        {"log",   basic_log},
        // {"log10", } TODO: log(x)/log(10)
        // {"log2", }  TODO: log(x)/log(2)
        // {"log1p", } TODO: log(1 + x)
        // {"expm1", } TODO: exp(x) - 1
        {"cos",   basic_cos},
        {"cosh",  basic_cosh},
        {"sin",   basic_sin},
        {"sinh",  basic_sinh},
        {"tan",   basic_tan},
        {"tanh",  basic_tanh},
        {"acos",  basic_acos},
        {"acosh", basic_acosh},
        {"asin",  basic_asin},
        {"asinh", basic_asinh},
        {"atan",  basic_atan},
        {"atanh", basic_atanh},
        // {"cospi", }
        // {"sinpi", }
        // {"tanpi", }
        {"gamma", basic_gamma},
        {"lgamma", basic_loggamma},
        // {"digamma", }  TODO
        // {"trigamma", } TODO
        
        
        // ======== Other Functions =========================
        {"nextprime", ntheory_nextprime},
        {"neg",  basic_neg},
        {"expand", basic_expand},
        
        {"zeta", basic_zeta},
        {"lambertw", basic_lambertw},
        {"dirichlet_eta", basic_dirichlet_eta},
        
        {"erf",  basic_erf},
        {"erfc", basic_erfc},
        
        {"factorial", cwrapper_binding_ntheory_factorial},
        
        {"csc"   ,  basic_csc   },
        {"sec"   ,  basic_sec   },
        {"cot"   ,  basic_cot   },
        {"acsc"  ,  basic_acsc  },
        {"asec"  ,  basic_asec  },
        {"acot"  ,  basic_acot  },
        {"csch"  ,  basic_csch  },
        {"sech"  ,  basic_sech  },
        {"coth"  ,  basic_coth  },
        {"acsch" ,  basic_acsch },
        {"asech" ,  basic_asech },
        {"acoth" ,  basic_acoth },
    };
    
    const int table_len = sizeof(math_lookup_table) / sizeof(cwrapper_math_mapping_t);
    for (int i = 0; i < table_len; i++) {
        cwrapper_math_mapping_t entry = math_lookup_table[i];
        if (strcmp(key, entry.key) == 0)
            return entry.val;
    }
    Rf_error("math_lookup failed to find '%s'\n", key);
}

// [[Rcpp::export()]]
S4 s4binding_math(SEXP robj, const char* math_key) {
    // Rules:
    //   1. Basic -> Basic; VecBasic -> VecBasic; DenseMatrix -> DenseMatrix
    //   2. R length-one vector and formula -> Basic
    //   3. R vector (including list) -> VecBasic
    //   4. Dimension of R matrix and array will be ignored and return VecBasic
    cwrapper_math_t* math_wrapped = math_lookup(math_key);
    S4          x      = s4binding_parse(robj);
    s4binding_t x_type = s4binding_typeof(x);
    if (x_type == S4BASIC) {
        S4 ans = s4basic();
        cwrapper_hold(
            math_wrapped(s4basic_elt(ans), s4basic_elt(x))
        );
        return ans;
    }
    // x is a VecBasic or a DenseMatrix
    S4            ans     = s4vecbasic();
    CVecBasic*    ans_elt = s4vecbasic_elt(ans);
    int           sz      = s4binding_size(x);
    for (int i = 0; i < sz; i++) {
        S4 val = s4binding_subset(x, Rf_ScalarInteger(i + 1), true);
        cwrapper_hold(math_wrapped(global_bholder, s4basic_elt(val)));
        cwrapper_hold(vecbasic_push_back(ans_elt, global_bholder));
    }
    
    // If input is a DenseMatrix, convert vecbasic back to the same dimension
    if (x_type == S4DENSEMATRIX) {
        IntegerVector dim = s4DenseMat_dim(x);
        ans = s4DenseMat_byrow(as<RObject>(ans), dim[1], dim[0]);
        ans = s4DenseMat_transpose(ans);
    }
    return ans;
}

////========  Evalf  ===============================

// [[Rcpp::export()]]
S4 s4binding_evalf(RObject expr, int bits, bool complex) {
    S4          x      = s4binding_parse(expr);
    s4binding_t x_type = s4binding_typeof(x);
    if (x_type == S4BASIC) {
        S4 ans = s4basic();
        cwrapper_hold(
            basic_evalf(s4basic_elt(ans), s4basic_elt(x), bits, !complex)
        );
        return ans;
    }
    
    // x is a VecBasic or a DenseMatrix
    S4 ans = s4vecbasic();
    CVecBasic* ans_elt = s4vecbasic_elt(ans);
    int sz = s4binding_size(x);
    
    
    for (int i = 0; i < sz; i++) {
        S4 val = s4binding_subset(x, Rf_ScalarInteger(i + 1), true);
        cwrapper_hold(basic_evalf(global_bholder, s4basic_elt(val), bits, !complex));
        cwrapper_hold(vecbasic_push_back(ans_elt, global_bholder));
    }
    
    if (x_type == S4DENSEMATRIX) {
        IntegerVector dim = s4DenseMat_dim(x);
        ans = s4DenseMat_byrow(as<RObject>(ans), dim[1], dim[0]);
        ans = s4DenseMat_transpose(ans);
    }
    return ans;
}

////========  linsovle and solve_poly ===============

// [[Rcpp::export()]]
S4 s4binding_solve_lin(RObject sys, RObject sym) {
    S4 sys2;
    S4 sym2;
    
    if (s4vecbasic_check(sys))
        sys2 = sys;
    else {
        sys2 = s4vecbasic();
        s4vecbasic_mut_append(sys2, sys);
    }
    
    if (s4vecbasic_check(sym))
        sym2 = sym;
    else {
        sym2 = s4vecbasic();
        s4vecbasic_mut_append(sym2, sym);
    }
    
    S4 ans = s4vecbasic();
    cwrapper_hold(
        vecbasic_linsolve(s4vecbasic_elt(ans), s4vecbasic_elt(sys2), s4vecbasic_elt(sym2))
    );
    return ans;
}

// [[Rcpp::export()]]
S4 s4binding_solve_poly(RObject f, RObject s) {
    S4 f2 = s4basic_parse(f, false);
    S4 s2 = s4basic_parse(s, false);
    S4 ans = s4vecbasic();
    CSetBasic* set = setbasic_new();
    CVecBasic* vec = s4vecbasic_elt(ans); 
    CWRAPPER_OUTPUT_TYPE status1 = basic_solve_poly(
        set, s4basic_elt(f2), s4basic_elt(s2)
    );
    CWRAPPER_OUTPUT_TYPE status2 = cwrapper_set2vec(set, vec);
    if (status1 || status2){
        setbasic_free(set);
        cwrapper_hold(status1);
        cwrapper_hold(status2);
    }
    setbasic_free(set);
    return ans;
}


