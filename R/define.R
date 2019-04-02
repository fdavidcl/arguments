#' Define a function with argument checking
#'
#' `def` is the main utility of this package. It allows users to define
#' functions that perform argument checking when called.
#'
#' @param .f Function
#' @param ... Named arguments for the function
#' @return A function that will check its arguments when called
#'
#' @examples
#'
#' # Check argument type
#' succ <- def(x = "numeric", {
#'   x + 1
#' })
#'
#' succ(1) # => 2
#' \donttest{
#' succ('a') # => error
#' }
#'
#'
#' # Check if argument is in a set of possible values
#' f1 <- def(x = list(TRUE, FALSE, NA, NULL), {
#'   x && !x
#' })
#'
#' # Check any condition on an argument
#' f2 <- def(x = (~ . > 0 && . < 10), {
#'   x + 1
#' })
#'
#' # Chain multiple checks
#' f3 <- def(x = "character" %&% (~ length(grep("hi", .)) > 0), {
#'   paste(x, "world!")
#' })
#'
#' # Set default values
#' f4 <- def(x = "numeric" %=% 0, {
#'   rep(TRUE, x)
#' })
#'
#' @importFrom rlang enexpr
#' @export
def <- function(.f, ...) {
  if (missing(.f)) {
    stop("It is necessary to pass a code block in order to define a function. For example:\n  f <- def(x = \"numeric\", { x + 1 })\n                          ^^^^^^^^^\n")
  }

  oldf <- enexpr(.f)
  arg_checks <- structure(lapply(list(...), as_argument), class = "arg_description")

  # TODO: Check arg checks

  f_args <- structure(
    vector("list", length(arg_checks)),
    names = names(arg_checks)
  )

  for (n in names(f_args)) {
    def_value <- get_default(arg_checks[[n]])
    f_args[[n]] <- if (is.null(def_value))
      rlang::sym("") # or NULL?
    else
      def_value
  }

  checked_f <- function() {
    if (exists(".__man__.")) {
      return(arg_checks)
    }

    parent <- parent.frame()

    # Store actual arguments as environment (can extract with as.list)
    .myargs <- environment()
#     print(arg_checks)
#     print(as.list(.myargs))
    actual_args <- as.list(.myargs)
    # required_args <- check_required(arg_checks, actual_args)
    errors <- check_args(arg_checks, actual_args)

    if (length(errors) > 0) {
      errors <- paste0("Errors were found when checking validity of arguments:\n", paste(errors, collapse = "\n"))
      stop(errors)
    }

    # Cleanup
    # rm(.myargs)

    # Create a simulated call environment for the function code to safely run
    # without a cluttered environment
    eval_env <- rlang::new_environment(actual_args, parent)

    # Evaluate, checking for return visibility
    result <- withVisible(eval(oldf, eval_env))
    if (result$visible) {
      result$value
    } else {
      invisible(result$value)
    }
  }

  formals(checked_f) <- f_args
  checked_f
}
