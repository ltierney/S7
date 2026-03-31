# Called from C
method_lookup_error <- function(name, args) {
  types <- vcapply(args, obj_desc)
  msg <- method_lookup_error_message(name, types)
  cnd <- errorCondition(msg, class = c("S7_error_method_not_found", "error"))
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

S7_dispatch_call <- function(gen) {
    dispatch_args <- gen@dispatch_args

    ## build the source code
    args <- paste(dispatch_args, collapse = ", ")
    if (length(dispatch_args) == 1)
        obform <- dispatch_args
    else
        obform <- sprintf("list(%s)", args)
    template <- "S7::method(sys.function(-3L), object = %s)(%s, ...)"
    text <- sprintf(template, obform, args)

    parse(text = text)[[1]]
}

S7_dispatch <- function() {
    gen <- sys.function(-1L)
    call <- S7_dispatch_call(gen)
    eval(call, parent.frame())
}
