argument <- function(
  is_a = NULL,
  is_in = NULL,
  verifies = NULL,
  defaults_to = NULL,
  is_required = NULL
) {
  if (!is.null(is_a)) stopifnot(is(is_a, "character"))
  if (!is.null(verifies)) stopifnot(is(verifies, "function") || is(verifies, "rlang_lambda_function"))

  as_argument(
    structure(list(
      is_a = is_a,
      is_in = is_in,
      verifies = verifies,
      defaults_to = defaults_to,
      is_required = is_required
    ), class = "argument")
  )
}

argument_class <- function(is_a) {
  r <- argument(is_a = is_a)
  attr(r[[1]], "class") <-  c("argument_class", class(r[[1]]))
  r
}

argument_set <- function(is_in) {
  r <- argument(is_in = is_in)
  attr(r[[1]], "class") <-  c("argument_set", class(r[[1]]))
  r
}

argument_condition <- function(verifies) {
  r <- argument(verifies = verifies)
  attr(r[[1]], "class") <-  c("argument_condition", class(r[[1]]))
  r
}

argument_default <- function(defaults_to) {
  r <- argument(defaults_to = defaults_to)
  attr(r[[1]], "class") <-  c("argument_default", class(r[[1]]))
  r
}

# required <- rlang::sym("required")
# argument_required <- function() {
#   r <- argument(is_required = TRUE)
#   attr(r[[1]], "class") <-  c("argument_required", class(r[[1]]))
#   r
# }

#' Chain multiple argument conditions
#'
#' @param a1 First condition
#' @param a2 Second condition
#'
#' @return The concatenation of both conditions
#'
#' @examples
#'
#' checklist <-
#'   "numeric" %&%
#'   (~ . != 0) %&%
#'   (~ abs(.) < 10)
#'
#' f <- def(a = "numeric", b = checklist, { a / b })
#'
#' @export
`%&%` <- function(a1, a2) {
  p1 <- as_argument(a1)
  p2 <- as_argument(a2)

  c(p1, p2)
}

#' @importFrom methods is
#' @export
c.argument_list <- function(...) {
  reduce(list(...), function(p1, p2) {
    stopifnot(is(p1, "argument_list"))
    stopifnot(is(p2, "argument_list"))

    class(p1) <- "list"
    class(p2) <- "list"

    structure(c(p1, p2), class = "argument_list")
  })
}

#' Coercion to an argument type
#'
#' @param x value to be coerced
#' @return An argument list object
#'
#' @examples
#'
#' a <- as_argument("numeric")
#' b <- as_argument(~ is.logical(.) && .)
#' c <- as_argument(list(1, 2, 3, TRUE, FALSE))
#'
#' @export
as_argument <- function(x) UseMethod("as_argument", x)

#' @export
as_argument.character <- function(x) {
  argument_class(x)
}

#' @export
as_argument.formula <- function(x) {
  # TODO: check if formula is predicate, or at least unary
  argument_condition(rlang::as_function(x))
}

#' @export
as_argument.function <- function(x) {
  # Using only 'x' may cause some problems with primitive functions
  # argument_condition(function(.) x(.))
  argument_condition(x)
}

#' @export
as_argument.list <- function(x) {
  argument_set(x)
}

# as_argument.name <- function(x) {
#   if (identical(x, required)) {
#     argument_required()
#   } else {
#     stop("Unknown symbol")
#   }
# }

#' @export
as_argument.argument <- function (x) {
  structure(list(x), class = "argument_list")
}

#' @export
as_argument.argument_list <- function (x) x


# Default value management ------------------------------------------------

#' Set a default value for an argument
#'
#' @param value Default value
#'
#' @examples
#'
#' f1 <- def(x = defaults_to(0), { x + 1 })
#' f2 <- def(x = "numeric" %=% 0, { x + 1 })
#'
#' f1() # => 1
#' f2() # => 1
#'
#' @export
defaults_to <- function(value) {
  argument_default(value)
}

#' @rdname defaults_to
#' @param alist Left-hand side, argument definitions such as classes or conditions
#'
#' @export
`%=%` <- function(alist, value) {
  alist %&% defaults_to(value)
}

#' @import purrr
get_default <- function(arglist) {
  def_values <- arglist %>% map("defaults_to")

  for (v in def_values) {
    if (!is.null(v)) return(v)
  }

  NULL
}
