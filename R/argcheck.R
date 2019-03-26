ARGS <- list()

arg <- function(..., .required = TRUE, .default, .condition) {
  if (missing(.default)) {
    .default = NULL
  }
  if (missing(.condition)) {
    .condition = function(...) TRUE
  }

  function(get = F)
    structure(list(
      ...
    ), class = "arg")
}

arg_numeric <- arg(numeric = NULL)

arg_character <- arg(character = NULL)

resolve_defaults <- function() {}

#' @import rlang
#' @export
def <- function(., .., ...) {
  parent <- parent.frame()
  name <- enexpr(.)
  oldf <- enexpr(..)
  arg_checks <- list(...)

  f_args <- vector("list", length(arg_checks))
  names(f_args) <- names(arg_checks)

  checked_f <- function() {
    # Do something with args
    myargs <- environment()
    # resolve_defaults()
    # check_args()

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

  eval(call("<-", paste0(".describe.", name), describe_f), parent, parent)
  # eval(call("<-", name, checked_f), parent, parent)

  checked_f
}

#' Example function
#'
#'
#' @param number A number
#' @param string A string
#' @return dunno
#'
#' @export
example_argcheck <- def("example_argcheck",
  number = arg_numeric(),
  string = arg_character(),
  {
    print(myargs)
    print("alright")
    print(number)
  }
)


arg_numeric <- arg(numeric = NULL)

check_args <- function(formal_args, arguments) {
  checks <- ARGS[]
  # validate_call(as.list(arguments), checks)
}
