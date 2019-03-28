# arg <- function(..., .default, .required = missing(.default)) {
#   function()
#     structure(list(
#       ...
#     ), class = "arg")
# }

#' @export
as_arg <- function(x) UseMethod("as_arg")

as_arg.arg <- function(x) x

as_arg.formula <- function(x) {
  # TODO: check if formula is predicate, or at least unary
  structure(
    rlang::as_function(x),
    class = "arg"
  )
}

as_arg.function <- function(x) {
  structure(
    x,
    class = "arg"
  )
}

as_arg.character <- function(x) {
  as.arg(
    function(obj) { any(x %in% class(obj)) }
  )
}

as_arg.call <- function(x) {
  as_arg(
    function(.) {
      eval(as.call(c(list(sym(call_name(x)), .), call_args(x))))
    }
  )
}

is_numeric <- as_arg("numeric")

is_character <- as_arg("character")
