
man <- function(f) {
  formals(f) <- list(.__man__. = TRUE)
  f()
}

print.arglist <- function(x, ...) {

}
