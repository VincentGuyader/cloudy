context("comble_posee")

test_that("comble_posee works", {
  posee <- condition(iris[,-3])
  expect_equal_to_reference(comble_posee(iris,posee),"comble_posee.test")
})
