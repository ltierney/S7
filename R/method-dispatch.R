# Called from C
method_lookup_error <- function(name, args) {
  types <- vcapply(args, obj_desc)
  msg <- method_lookup_error_message(name, types)
  cnd <- errorCondition(msg, class = "S7_error_method_not_found")
  stop(cnd)
}

method_lookup_error_message <- function(name, types) {
  if (length(types) == 1) {
    sprintf("Can't find method for `%s(%s)`.", name, types)
  } else {
    arg_names <- paste0(names(types), collapse = ", ")
    types <- paste0("- ", format(names(types)), ": ", types, collapse = "\n")
    sprintf("Can't find method for generic `%s(%s)`:\n%s", name, arg_names, types)
  }
}

#' @rdname new_generic
#' @order 2
#' @export
S7_dispatch <- function() {
  .External2(method_call_, sys.function(-1L), sys.frame(-1L))
}

method_from_dispatch <- function(generic, dispatch) {
    check_is_S7(generic, S7_generic)
    method <- .Call(method_, generic, dispatch, environment(), 
        FALSE)
    if (is.null(method)) {
        types <- lapply(dispatch, `[[`, 1)
        names(types) <- generic@dispatch_args
        method_lookup_error(generic@name, types)
    }
    else method
}

is_super <- function(x)
    ! missing(x) && inherits(x, "S7_super")

obj_disp <- function(x) {
    if (missing(x))
        "MISSING"
    else if (is_super(x))
        x$dispatch
    else
        obj_dispatch(x)
}

call_args <- function(gen) {
    args <- names(formals(gen))
    nargs <- length(args)
    dots_pos <- match("...", args, nargs)
    paste(ifelse(seq_len(nargs) > dots_pos,
                 sprintf("%s = %s", args, args),
                 args),
          collapse = ", ")
}

S7_dispatch_call <- function(gen) {
    call <- gen@dispatch_call
    if (! is.null(call))
        return(call)
    dispatch_args <- gen@dispatch_args

    ## build the source code
    disp <- paste(sprintf("S7:::obj_disp(%s)", dispatch_args), collapse = ", ")
    displine <- sprintf("dispatch <- list(%s)", disp)

    supers <- sprintf("if (S7:::is_super(%s)) %s <- %s$object",
                      dispatch_args, dispatch_args, dispatch_args)

    args <- call_args(gen)
    mline <-
        sprintf("S7:::method_from_dispatch(generic, dispatch)(%s)", args)
    mline <- "S7:::S3_dispatch(S7:::method_from_dispatch(generic, dispatch), environment())"
    mline <- "S7:::method_from_dispatch(generic, dispatch)"
    text <- c("generic <- sys.function(-3L)", displine, supers, mline)

    parse(text = c("{", paste("    ", text), "}"))[[1]]
}

S7_dispatch <- function() {
    gen <- sys.function(-1L)
    call <- S7_dispatch_call(gen)
    method <- eval(call, parent.frame())
    mname <- as.name("<method>")
    rho <- sys.frame(-1L)
    callrho <- sys.frame(-2L)
    .External2(dispatchClosure_, gen, mname, method, rho, callrho)
}
