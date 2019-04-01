#' Define a function
#'
#' @param .f Function
#' @param ... Named arguments for the function
#'
#' @importFrom rlang enexpr
#' @export
def <- function(.f, ...) {
  if (missing(.f)) {
    stop("It is necessary to pass a code block in order to define a function. For example:\n  f <- def(x = \"numeric\", { x + 1 })\n                          ^^^^^^^^^\n")
  }

  oldf <- enexpr(.f)
  arg_checks <- structure(lapply(list(...), as_argument), class = "arg_description")

  # TODO: Check arg checks

  f_args <- vector("list", length(arg_checks))
  names(f_args) <- names(arg_checks)

  checked_f <- function() {
    if (exists(".__man__.")) {
      return(arg_checks)
    }

    # Do something with args
    .myargs <- environment()

    actual_args <- resolve_defaults(arg_checks, .myargs)
    errors <- check_args(arg_checks, actual_args)

    if (length(errors) > 0) {
      errors <- paste0("Errors were found when checking validity of arguments:\n", paste(errors, collapse = "\n"))
      stop(errors)
    }

    # Cleanup
    rm(.myargs)

    result <- withVisible(eval(oldf))
    if (result$visible) {
      result$value
    } else {
      invisible(result$value)
    }
  }

  formals(checked_f) <- f_args
  checked_f
}
