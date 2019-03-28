arg <- function(..., .required = TRUE, .default) {
  if (missing(.default)) {
    .default = NULL
  }

  function(get = F)
    structure(list(
      ...
    ), class = "arg")
}
