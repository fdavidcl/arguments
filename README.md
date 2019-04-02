# arguments
R package for defining functions which provide descriptions, automatic argument checking, and more

## Usage

Instead of:

~~~r
my_function <- function(x, y) {
  stopifnot(is(x, "numeric"))
  stopifnot(x != 0)
  stopifnot(y %in% c(TRUE, FALSE))
  
  ## Do stuff with x and y
}
~~~

do this:

~~~r
my_function <- def(x = "numeric" %&% (~ . != 0), y = list(TRUE, FALSE), {
  ## Do stuff with x and y
})
~~~

As a bonus, you get this:

~~~r
man(my_function)
~~~
~~~
x ->
  Is a: numeric
  Verifies: . != 0

y ->
  Is in: TRUE, FALSE
~~~
