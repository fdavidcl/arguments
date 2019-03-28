#' Example function
#'
#' @param number A number
#' @param string A string
#' @return dunno
#'
#' @export
example_argcheck <- def(number = arg_numeric(), string = arg_character(), {
  if (exists("myargs")) {
    stop("Enviroment is leaking or block was executed prematurely!!!")
  }
  print("alright")
  print(number)
})
