#' Retrieve the manual of a function
#'
#' `man` receives a function and obtains the conditions that its arguments must
#' verify. It returns them in a list that prints as a list of instructions.
#'
#' @param f A function created with `def`
#' @return A list of argument checks
#'
#' @export
man <- function(f) {
  formals(f) <- list(.__man__. = TRUE)
  f()
}

as.character.def_function_description <- function(x) {
  paste0(paste0(names(x), " ->\n"), sapply(x, as.character), collapse = "\n")
}

print.def_function_description <- function(x) {
  cat(as.character(x))
}
