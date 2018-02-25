
#' Title
#'
#' @param dataset
#' @param var
#' @param posee
#' @import skimr
#' @return
#' @export
#'
#' @examples
quanti_desc   <- function(dataset,var,posee = dataset %>% condition(), tidy=TRUE){
  if (tidy) {
    var <- quo(!!quo_text(enquo(var)) %>%
                 str_replace_all("_", " "))
    posee <- posee %>% rename_all(~str_replace_all(.,"_"," "))
    dataset <- dataset %>% rename_all(~str_replace_all(.,"_"," "))

  } else{
    var <- enquo(var)

  }
  posee <- comble_posee(dataset,posee)

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
#' @importFrom purrr map_chr
#' @importFrom purrr map_df
#' @return
#' @export
#'
#' @examples
quanti_desc_all <- function(dataset,
                            posee = dataset %>% condition(),tidy=TRUE){

  if (tidy) {
    posee <- posee %>% rename_all(~str_replace_all(.,"_"," "))
    dataset <- dataset %>% rename_all(~str_replace_all(.,"_"," "))

  }
   quanti <- dataset %>%
    map_chr(~class(.x)[[1]]) %>%
    .[.%in% c("numeric","integer","double")] %>%
    names()
if (length(quanti) == 0){return(NULL)}
  quanti %>%
    map_df(~ quanti_desc(dataset,var = !! sym(.x),posee = posee,tidy=FALSE))

}
