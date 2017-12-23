library(tidyverse)
library(glue)
library(tibble)
library(dplyr)
library(arsenal)
library(rlang)

#' Title
#'
#' @param x
#' @param ...
#' @importFrom  tibble column_to_rownames
#' @import dplyr
#' @import forcats
#' @return
#' @export
#'

count_with_na <- function(x,...){
  table(x,useNA = "always") %>%
    data.frame() %>%
    rename(count=Freq) %>%
    mutate(pct=count/sum(count)*100) %>%
    mutate_all(funs(case_when(
      is.na(.) ~ "manquant",
      TRUE ~ as.character(.)
    ))) %>%
    mutate(x = forcats::as_factor(x),
           x = forcats::fct_relevel(x,"manquant")
    ) %>%
    arrange(x) %>%
    mutate_at(vars(-x),as.numeric) %>%
    # mutate(pct= glue("{round(pct,2)} %")) %>% # trouver dans arsenal::
    tibble::column_to_rownames("x")
}




#' Title
#'
#' @param dataset
#' @param var
#' @import rlang
#' @import dplyr
#' @importFrom  arsenal tableby tableby.control
#' @importFrom  glue glue
#' @import stringr
#' @importFrom stats as.formula
#' @return
#' @export
#'
#' @examples
#'
#' quali_desc(iris,Species)
#'
quali_desc <- function(dataset,var){
  var <- enquo(var)

  dataset2 <- rbind(dataset %>%
                      mutate(grpXXX = "Complet"),
                    dataset %>%
                      mutate(grpXXX = "Sans données manquantes") %>%
                      filter(!is.na(!! var))
  )

  summary(
    tableby(
      as.formula(glue("grpXXX~ {quo_text(var)}")),
      data=dataset2,
      control=tableby.control(cat.stats = c("N","count_with_na"),total=FALSE,test = FALSE)
    ))


}
