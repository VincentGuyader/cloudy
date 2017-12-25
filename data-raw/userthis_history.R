usethis::use_package("arsenal")
usethis::use_package("dplyr")
usethis::use_package("glue")
usethis::use_package("tibble")
usethis::use_package("stringr")
usethis::use_package("skim")
usethis::use_package("forcats")
usethis::use_package("purrr")
usethis::use_tidy_eval()
usethis::use_pipe()
usethis::use_data_raw()
usethis::use_tidy_description()
usethis::use_vignette("demo")
usethis::use_vignette("demo2")

usethis::use_git()
usethis::use_github()
usethis::use_test("base")



dataset <- structure(list(id = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), age = c(12,
                                                                         43, NA, 21, 2, 56, 7, 8, 9, 3), sexe = c("F", "F", "M", NA, "F",
                                                                                                                  "M", "M", "F", "F", "M"), `tu fume ?` = c("non", "non", "oui",
                                                                                                                                                            "oui", "non", "oui", NA, "non", "non", "oui"), `tu fume combien ?` = c(NA,
                                                                                                                                                                                                                                   NA, 2, 3, NA, NA, NA, NA, NA, 5), `date des derniere regles` = structure(c(1496361600,
                                                                                                                                                                                                                                                                                                              1493942400, NA, 1493942400, 1485907200, 1496448000, NA, 1496448000,
                                                                                                                                                                                                                                                                                                              NA, NA), class = c("POSIXct", "POSIXt"), tzone = "UTC"), `ca va ?` = c("oui",
                                                                                                                                                                                                                                                                                                                                                                                     "non", "oui", "non", "non", "non", "non", "oui", "oui", "oui"
                                                                                                                                                                                                                                                                                                              )), .Names = c("id", "age", "sexe", "tu fumes ?", "tu fumes combien ?",
                                                                                                                                                                                                                                                                                                                             "date des derniere regles", "ca va ?"), class = c("tbl_df", "tbl",
                                                                                                                                                                                                                                                                                                                                                                               "data.frame"), row.names = c(NA, -10L))
demo_cloudy <- dataset
usethis::use_data(demo_cloudy)
