
resolve_defaults <- function(formal, actual) {
  parent <- parent.frame()

  set_formals <- names(formal) %in% names(actual)
  # extra_params <- !(names(actual) %in% names(formal))

  result <- actual

  for (param in which(!set_formals)) {
    result[[names(formal)[param]]] <- eval(formal[[param]])
  }

  result
}
