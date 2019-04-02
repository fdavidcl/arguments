
# example_argcheck <- def(number = is_numeric, string = is_character, {
#   if (exists("myargs")) {
#     stop("Enviroment is leaking or block was executed prematurely!!!")
#   }
#   print("alright")
#   print(number)
# })
