print.argument_list <- function(x, ...) {
  cat(as.character(x))
}

as.character.argument_list <- function(x) {
  indent <- "  "
  newline <- "\n"
  output <- ""
  mycat <- function(...) {
    output <<- paste0(output, indent, paste(...), newline)
  }

  for (check in x) {
    if (!is.null(check$is_a)) {
      mycat("Is a:", paste0(check$is_a, collapse = " or "))
    }
    if (!is.null(check$is_in)) {
      mycat("Is in:", paste0(check$is_in, collapse = ", "))
    }
    if (!is.null(check$verifies)) {
      mycat("Verifies:", func_to_string(check$verifies))
    }
    if (!is.null(check$defaults_to)) {
      mycat("Defaults to:", as.character(check$defaults_to))
    }
    if (!is.null(check$is_required)) {
      mycat("Is required")
    }
  }

  output
}
