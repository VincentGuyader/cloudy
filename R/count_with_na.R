
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
