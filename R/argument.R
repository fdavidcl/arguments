argument <- function(
  is_a = NULL,
  is_in = NULL,
  verifies = NULL,
  defaults_to = NULL,
  is_required = NULL
) {
  if (!is.null(is_a)) stopifnot(is(is_a, "character"))
  if (!is.null(verifies)) stopifnot(is(verifies, "function") || is(verifies, "rlang_lambda_function"))

  structure(list(list(
    is_a = is_a,
    is_in = is_in,
    verifies = verifies,
    defaults_to = defaults_to,
    is_required = is_required
  )), class = c("argument_list", "argument"))
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

#' @export
required <- rlang::sym("required")
argument_required <- function() {
  r <- argument(is_required = TRUE)
  attr(r[[1]], "class") <-  c("argument_required", class(r[[1]]))
  r
}

`%and%` <- function(a1, a2) {
  p1 <- as_argument(a1)
  p2 <- as_argument(a2)

  c(p1, p2)
}

#' @export
c.argument_list <- function(p1, p2) {
  stopifnot(is(p1, "argument_list"))
  # if (is(p2, "argument") && !is(p2, "argument_list")) {
  #   p2 <- structure(list(p2), class = c("argument_list", "argument"))
  # }
  stopifnot(is(p2, "argument_list"))

  class(p1) <- "list"
  class(p2) <- "list"

  structure(c(p1, p2), class = c("argument_list", "argument"))
}

func_to_string <- function(x) {
  bd <- body(x)
  if (is.null(bd)) {
    fn <- enexpr(x)
    text <- as.character(substitute(x))
  } else {
    # TODO: Improve this a lot
    text <- as.character(enexpr(bd))
    text <- paste0(text[-1], collapse = " ")
  }

  text
}

#' @export

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
  argument_condition(function(.) x(.))
}

#' @export
as_argument.list <- function(x) {
  argument_set(x)
}

as_argument.name <- function(x) {
  if (identical(x, required)) {
    argument_required()
  } else {
    stop("Unknown symbol")
  }
}

#' @export
as_argument.argument <- function (x) x

defaults_to <- function(value) {
  argument_default(value)
}

`%=%` <- function(alist, value) {
  alist %and% defaults_to(value)
}
