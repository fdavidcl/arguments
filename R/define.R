#' Define a function
#'
#' @param .f Function
#' @param ... Named arguments for the function
#'
#' @importFrom rlang enexpr
#' @export
def <- function(.f, ...) {
  if (missing(.f)) {
    stop("It is necessary to pass a code block in order to define a function. For example:\n  f <- def(x = numeric(), { x + 1 })\n                          ^^^^^^^^^\n")
  }

  oldf <- enexpr(.f)
  arg_checks <- list(...)

  f_args <- vector("list", length(arg_checks))
  names(f_args) <- names(arg_checks)

  checked_f <- function() {
    if (exists(".__man__.")) {
      return(arg_checks)
    }

    # Do something with args
    .myargs <- environment()

    resolve_defaults()
    check_args()

    # Cleanup
    rm(.myargs)

    result <- withVisible(eval(oldf))
    if (result$visible) {
      result$value
    } else {
      invisible(result$value)
    }
  }

  describe_f <- function() {
    structure(arg_checks, class = "arg_description")
  }

  formals(checked_f) <- f_args
  checked_f
}
