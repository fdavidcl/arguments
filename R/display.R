print.arg_descriptions <- function(x) {

}

#' @import rlang
print.arg <- function(x) {
  cat(attr(x, "description"))
  cat("\n")
}
