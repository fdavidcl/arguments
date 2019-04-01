# arg <- function(..., .default, .required = missing(.default)) {
#   function()
#     structure(list(
#       ...
#     ), class = "arg")
# }


#' @export
as_predicate <- function(x) UseMethod("as_predicate", x)

#' @export
as_predicate.predicate <- function(x) x


#' @export
as_predicate.argument_class <- function(x) function(a) {
  any(class(a) %in% x$is_a)
}

#' @export
as_predicate.argument_set <- function(x) function(a) {
  a %in% x$is_in
}

#' @export
as_predicate.argument_condition <- function(x) function(a) {
  x$verifies(a)
}

#' @export
as_predicate.argument_list <- function(x) function(a) {
  for (check in x) {
    p <- as_predicate(check)
    if (!p(a)) return(FALSE)
  }

  return(TRUE)
}

#' #' @export
#' as_predicate.formula <- function(x) {
#'   # TODO: check if formula is predicate, or at least unary
#'   structure(
#'     rlang::as_function(x),
#'     description = paste0("Verifies: ", paste0(as.character(enexpr(x))[-1], collapse = " ")),
#'     class = c("predicate", "function")
#'   )
#' }


#' #' @import rlang
#' #' @export
#' as_predicate.function <- function(x) {
#'   structure(
#'     # Using only 'x' causes some problems with primitive functions
#'     function(.) x(.),
#'     description = paste0("Verifies: ", func_to_string(x)),
#'     class = c("predicate", "function")
#'   )
#' }

#' #' @export
#' as_predicate.character <- function(x) {
#'   structure(
#'     function(obj) { any(x %in% class(obj)) },
#'     description = paste0("Is", if (length(x) == 1) "" else " one of", ": ", paste0(x, collapse = ", ")),
#'     class = c("predicate", "function")
#'   )
#' }

#' #' @import rlang
#' #' @export
#' as_predicate.call <- function(x) {
#'   x <- enquo(x)
#'   structure(
#'     function(.) {
#'       eval(as.call(c(list(sym(call_name(x)), .), call_args(x))))
#'     },
#'     description = as.character(x),
#'     class = c("predicate", "function")
#'   )
#' }

# is_numeric <- as_predicate("numeric")
#
# is_character <- as_predicate("character")
