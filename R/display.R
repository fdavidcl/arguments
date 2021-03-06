print.argument <- function(x, ...) {
  cat(as.character(x))
}

print.argument_list <- function(x, ...) {
  cat(as.character(x))
}

INDENT <- "  "
NEWLINE <- "\n"
mycat <- function(...) {
  paste0(INDENT, paste(...), NEWLINE)
}

as.character.argument_class <- function(x) {
  mycat("Is a:", paste0(x$is_a, collapse = " or "))
}

as.character.argument_set <- function(x) {
  mycat("Is in:", paste0(x$is_in, collapse = ", "))
}

func_to_string <- function(x) {
  bd <- body(x)
  if (is.null(bd)) {
    fn <- enexpr(x)
    text <- rlang::expr_deparse(fn)
  } else {
    # TODO: Improve this a lot
    text <- rlang::expr_deparse(enexpr(bd))
    text <- paste0(text, collapse = " ")
  }

  text
}

as.character.argument_condition <- function(x) {
  mycat("Verifies:", func_to_string(x$verifies))
}

# as.character.argument_required <- function(x) {
#   mycat("Is required")
# }

as.character.argument_default <- function(x) {
  mycat("Defaults to:", as.character(x$defaults_to))
}

as.character.argument_list <- function(x) {
  do.call(paste0, x)
}
