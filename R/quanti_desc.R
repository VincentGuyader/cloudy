
#' Title
#'
#' @param dataset
#' @param var
#' @param posee
#'
#' @return
#' @export
#'
#' @examples
quanti_desc   <- function(dataset,var,posee = dataset %>% condition()){
  var <- enquo(var)

  # browser()
  dataset <-  dataset %>%
    select(!! var) %>%
    cbind(
      posee %>%
        select(!! var)%>% rename_all(function(...){"xx"}))  %>%
    filter(xx == 1) %>%
    select(-xx)

  sk <- skim(dataset)
  tableau_quanti <-  sk %>%
    filter(type %in% c("integer","numeric")) %>%
    dplyr::do(., skimr:::skim_render(., NULL, skimr:::print_impl) %>% invisible())

  tableau_quanti
}


#' Title
#'
#' @param dataset
#' @param posee
#'
#' @return
#' @export
#'
#' @examples
quanti_desc_all <- function(dataset,posee = dataset %>% condition()){
  quanti <- dataset %>%
    map_chr(~class(.x)[[1]]) %>%
    .[.%in% c("numeric","integer","double")] %>%
    names()

  quanti %>%
    map_df(~ quanti_desc(dataset,var = !! sym(.x),posee = posee))

}
