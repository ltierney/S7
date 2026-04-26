#include "compat.h"

#if (R_VERSION >= R_Version(4, 5, 0))
#define getClosureFormals R_ClosureFormals
#else
#define getClosureFormals FORMALS
#endif

extern SEXP parent_sym;
extern SEXP sym_ANY;
extern SEXP ns_S7;
extern SEXP sym_obj_dispatch;
extern SEXP sym_dispatch_args;
extern SEXP sym_methods;
extern SEXP sym_S7_dispatch;
extern SEXP sym_name;

extern SEXP fn_base_quote;
extern SEXP fn_base_missing;

extern SEXP R_TRUE;
extern SEXP s7_proto_object;


static inline
void APPEND_NODE(SEXP node, SEXP tag, SEXP val) {
  SEXP new_node = Rf_cons(val, R_NilValue);
  SETCDR(node, new_node);
  SET_TAG(new_node, tag);
}

// extern Rboolean is_S7_object(SEXP);
// extern Rboolean is_s7_class(SEXP);
// extern void check_is_S7(SEXP object);


static inline
SEXP maybe_enquote(SEXP x) {
  switch (TYPEOF(x)) {
    case SYMSXP:
    case LANGSXP:
      return Rf_lang2(fn_base_quote, x);
    default:
      return x;
  }
}

// Recursively walk through method table to perform iterated dispatch
SEXP method_rec(SEXP table, SEXP signature, R_xlen_t signature_itr) {
  if (signature_itr >= Rf_xlength(signature)) {
    return R_NilValue;
  }

  SEXP classes = VECTOR_ELT(signature, signature_itr);

  for (R_xlen_t i = 0; i < Rf_xlength(classes); ++i) {
    SEXP klass = Rf_install(CHAR(STRING_ELT(classes, i)));
    SEXP val = s7_get_var_in_frame(table, klass, R_NilValue);
    if (TYPEOF(val) == ENVSXP) {
      PROTECT(val); // no really necessary, but rchk flags spuriously
      val = method_rec(val, signature, signature_itr + 1);
      UNPROTECT(1);
    }
    if (TYPEOF(val) == CLOSXP) {
      return val;
    }
  }

  // ANY fallback
  SEXP val = s7_get_var_in_frame(table, sym_ANY, R_NilValue);
  if (TYPEOF(val) == ENVSXP) {
    PROTECT(val);
    val = method_rec(val, signature, signature_itr + 1);
    UNPROTECT(1);
  }
  if (TYPEOF(val) == CLOSXP) {
    return val;
  }

  return R_NilValue;
}

SEXP generic_args(SEXP generic, SEXP envir) {
  // This function is only used to generate an informative message when
  // signalling an S7_method_lookup_error, so it doesn't need to be maximally efficient.

  // How many arguments are used for dispatch?
  SEXP dispatch_args = Rf_getAttrib(generic, sym_dispatch_args);
  R_xlen_t n_dispatch = Rf_xlength(dispatch_args);

  // Allocate a list to store the arguments
  SEXP args = PROTECT(Rf_allocVector(VECSXP, n_dispatch));

  SEXP missing_call = PROTECT(Rf_lang2(fn_base_missing, R_NilValue));
  PROTECT_INDEX pi;
  PROTECT_WITH_INDEX(R_NilValue, &pi);

  // Find the value of each argument.
  SEXP formals = getClosureFormals(generic);
  for (R_xlen_t i = 0; i < n_dispatch; ++i) {
    SEXP name = TAG(formals);

    SETCADR(missing_call, name);
    SEXP is_missing = Rf_eval(missing_call, envir);
    REPROTECT(is_missing, pi);

    if (Rf_asLogical(is_missing))  {
      SET_VECTOR_ELT(args, i, R_MissingArg);
    } else {
      // method_call_() has already done the necessary computation
      SET_VECTOR_ELT(args, i, Rf_eval(name, envir));
    }

    formals = CDR(formals);
  }
  Rf_setAttrib(args, R_NamesSymbol, dispatch_args);

  UNPROTECT(3);

  return args;
}

__attribute__ ((noreturn))
void S7_method_lookup_error(SEXP generic, SEXP envir) {

  SEXP name = Rf_getAttrib(generic, R_NameSymbol);
  SEXP args = generic_args(generic, envir);

  SEXP S7_method_lookup_error_call = PROTECT(Rf_lang3(Rf_install("method_lookup_error"), name, args));
  Rf_eval(S7_method_lookup_error_call, ns_S7);

  while(1);
}

SEXP method_(SEXP generic, SEXP signature, SEXP envir, SEXP error_) {
  if (!Rf_inherits(generic, "S7_generic")) {
    return R_NilValue;
  }

  SEXP table = Rf_getAttrib(generic, sym_methods);
  if (TYPEOF(table) != ENVSXP) {
    Rf_error("Corrupt S7_generic: @methods isn't an environment");
  }

  SEXP m = method_rec(table, signature, 0);

  if (m == R_NilValue && Rf_asLogical(error_)) {
    S7_method_lookup_error(generic, envir);
  }

  return m;
}


SEXP S7_obj_dispatch(SEXP object) {

  SEXP obj_dispatch_call = PROTECT(Rf_lang2(sym_obj_dispatch, maybe_enquote(object)));
  SEXP res = Rf_eval(obj_dispatch_call, ns_S7);
  UNPROTECT(1);

  return res;
}

SEXP S7_object_(void) {
  return Rf_duplicate(s7_proto_object);
}

SEXP method_call_(SEXP call_, SEXP op_, SEXP args_, SEXP env_) {
  args_ = CDR(args_);
  SEXP generic = CAR(args_); args_ = CDR(args_);
  SEXP envir = CAR(args_); args_ = CDR(args_);

  // Get the formals and the number of arguments used for dispatch
  SEXP formals = getClosureFormals(generic);
  SEXP dispatch_args = Rf_getAttrib(generic, sym_dispatch_args);
  R_xlen_t n_dispatch = Rf_xlength(dispatch_args);

  // Allocate a vector to store the classes for the arguments
  SEXP dispatch_classes = PROTECT(Rf_allocVector(VECSXP, n_dispatch));

  PROTECT_INDEX val_pi;
  PROTECT_WITH_INDEX(R_NilValue, &val_pi); // unnecessary, for rchk only

  // For each of the dispatch arguments
  for (R_xlen_t i = 0; i < n_dispatch; ++i, formals = CDR(formals)) {

    SEXP name = TAG(formals);

    if (R_GetBindingType(name, envir) == R_BindingTypeMissing)
      SET_VECTOR_ELT(dispatch_classes, i, Rf_mkString("MISSING"));
    else {

      SEXP val = R_getVar(name, envir, FALSE);
      REPROTECT(val, val_pi); // unnecessary, for rchk only

      if (Rf_inherits(val, "S7_super")) {

        // Put the super() stored class dispatch vector into dispatch_classes
        SET_VECTOR_ELT(dispatch_classes, i, VECTOR_ELT(val, 1));

        // Replace the super() value by the true_val used for dispatch
        Rf_defineVar(name, VECTOR_ELT(val, 0), envir);

      } else { // val is not a S7_super, a regular value

        // Determine class string to use for method look up
        SET_VECTOR_ELT(dispatch_classes, i, S7_obj_dispatch(val));

      }
    }
  }

  // Now that we have all the classes, we can look up what method to call
  SEXP m = method_(generic, dispatch_classes, envir, R_TRUE);
  REPROTECT(m, val_pi); // unnecessary, for rchk only

  /// Inlining the method closure in the call like `SETCAR(mcall, m);`
  /// leads to extremely verbose (unreadable) traceback()s. So,
  /// for nicer tracebacks, we set a SYMSXP at the head.
  SEXP method_name = Rf_getAttrib(m, sym_name);
  if (TYPEOF(method_name) != SYMSXP) {
    // if name is missing, fallback to masking the `S7_dispatch` symbol.
    // we could alternatively fallback to inlining m: SETCAR(mcall, m)
    method_name = sym_S7_dispatch;
  }

  Rf_defineVar(method_name, m, envir);

  SEXP mcall = PROTECT(LCONS(method_name, R_NilValue));
  for (SEXP f = getClosureFormals(generic), a = mcall;
       f != R_NilValue;
       f = CDR(f)) {
    SEXP sym = TAG(f);
    if (sym == R_DotsSymbol) {
      if (R_DotsExist(envir)) {
	SETCDR(a, CONS(sym, R_NilValue));
	a = CDR(a);
      }
    }
    else {
      SETCDR(a, CONS(sym, R_NilValue));
      SET_TAG(CDR(a), sym);
      a = CDR(a);
    }
  }

  // need this declaration until it is added to the installed headers
  SEXP R_TailCall(SEXP call, SEXP fun, SEXP rho, SEXP callrho);

  SEXP out = R_TailCall(mcall, m, envir, envir);
  UNPROTECT(3);
  return out;
}
