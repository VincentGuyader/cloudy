context("test-base.R")

insert_na <- function(x, n = 40) {
  x[sample(seq_along(x), n)] <- NA
  x
}

test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})
test_that("quali", {
  data(iris)
  set.seed(123456)
  iris <- iris %>% dplyr::mutate(
    plop = sample(c("a","b"),150,replace = TRUE) %>% as.factor(),
    plop2 = forcats::lvls_expand(plop,c("a","b","c"))        ,
    plop3 = forcats::lvls_expand(plop,c("b","c","a"))
                   )
  z <- quali_desc_all(iris)
  expect_equal_to_reference(z,"quali_all_iris.test")
})
test_that("quali", {
  data(iris)
  set.seed(123456)
  iris <- iris %>% dplyr::mutate(
    plop = sample(c("a","b"),150,replace = TRUE) %>% as.factor(),
    plop2 = forcats::lvls_expand(plop,c("a","b","c"))        ,
    plop3 = forcats::lvls_expand(plop,c("b","c","a"))
                   ) %>%
    dplyr::mutate_all(insert_na)

  z <- quali_desc_all(iris)
  expect_equal_to_reference(z,"quali_all_iris_na.test")
})

