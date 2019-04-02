context("test-define")

test_that("can define simple functions", {
  expect_silent(f1 <- def({ NULL }))
  expect_silent(f2 <- def(x = "numeric", { x }))
  expect_silent(f3 <- def(x = "numeric", y = "numeric", { x + y }))
})

test_that("defined functions mimic standard functions", {
  f1 <- def({ NULL })
  f2 <- def(x = "integer", { x })
  f3 <- def(x = "integer", y = "integer", { x + y })
  g1 <- function(){ NULL }
  g2 <- function(x){ x }
  g3 <- function(x, y){ x + y }

  expect_equal(f1(), g1())
  expect_equal(f2(1L), g2(1L))
  expect_equal(f3(4L, 1L), g3(4L, 1L))
})

test_that("default values work", {
  val <- "works"
  f1 <- def(x = "character" %=% val, { x })
  expect_equal(f1(), val)
})

test_that("can't define empty functions", {
  expect_error(def())
})

test_that("args can be required", {
  f1 <- def(x = "numeric", { x })
  expect_error(f1())
})

test_that("debug output does not show up", {
  expect_silent(def(x = "numeric", {})(1))
})
