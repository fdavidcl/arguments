# arg <- function(..., .default, .required = missing(.default)) {
#   function()
#     structure(list(
#       ...
#     ), class = "arg")
# }

#' @export
as_arg <- function(x) UseMethod("as_arg", x)

#' @export
as_arg.arg <- function(x) x

#' @export
as_arg.formula <- function(x) {
  # TODO: check if formula is predicate, or at least unary
  structure(
    rlang::as_function(x),
    description = paste0("Verifies: ", as.character(enexpr(x))[2]),
    class = "arg"
  )
}

#' @import rlang
#' @export
as_arg.function <- function(x) {
  bd <- body(x)
  if (is.null(bd)) {
    fn <- enexpr(x)
    text <- as.character(substitute(x))
  } else {
    text <- as.character(enexpr(bd))
    text <- text[length(text)]
  }

  structure(
    # Using only 'x' causes some problems with primitive functions
    function(.) x(.),
    description = paste0("Verifies: ", text),
    class = c("arg", "function")
  )
}

#' @export
as_arg.character <- function(x) {
  structure(
    function(obj) { any(x %in% class(obj)) },
    description = paste0("Is", if (length(x) == 1) "" else " one of", ": ", paste0(x, collapse = ", ")),
    class = "arg"
  )
}

#' @import rlang
#' @export
as_arg.call <- function(x) {
  x <- enquo(x)
  structure(
    function(.) {
      eval(as.call(c(list(sym(call_name(x)), .), call_args(x))))
    },
    description = as.character(x),
    class = "arg"
  )
}

is_numeric <- as_arg("numeric")

is_character <- as_arg("character")
