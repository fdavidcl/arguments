#
# resolve_defaults <- function(formal, actual, envir = parent.frame()) {
#   set_formals <- names(formal) %in% names(actual)
#
#   result <- actual
#
#   for (param in which(!set_formals)) {
#     if (!is.null(formal[[param]]$defaults_to)) {
#       cat("Setting ", names(formal)[param], "to", formal[[param]]$defaults_to)
#       eval(call("<-", names(formal)[param], formal[[param]]$defaults_to), envir, envir)
#     }
#   }
# }

check_list <- function(x, value) {
  ps <- as_predicate(x)
  errs <- list()

  for (i in seq_along(ps)) {
    a <- x[[i]]
    p <- ps[[i]]

    # cat("Checking", as.character(a), " with ", as.character(value))

    if (!p(value)) {
      errs[[length(errs) + 1]] <- as.character(a)
    }
  }

  errs
}

check_args <- function(named_args, actual_args) {
  errors <- list()

  for (n in names(named_args)) {
    result <- check_list(named_args[[n]], actual_args[[n]])
    if (length(result) > 0) {
      errors[[length(errors) + 1]] <- paste0("  ", n, " ->", paste0(result, collapse = " & "))
    }
  }

  errors
}
