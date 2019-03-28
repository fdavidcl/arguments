check_args <- function(named_args, actual_args) {
  # checks <- ARGS[]
  # validate_call(as.list(arguments), checks)

  errors <- list()

  for (n in names(named_args)) {
    result <- named_args[n](actual_args[n])
    if (!result) {
      errors[[length(errors) + 1]] <- n
    }
  }

  errors
}
