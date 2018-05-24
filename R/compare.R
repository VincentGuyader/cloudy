#' Title
#'
#' @param dataset
#' @param var
#' @param tidy
#' @param ... additional arguments to be passed to internal tableby functions or tableby.control.
#'
#' @return
#' @export
#' @examples
#'
#'library(dplyr)
#' compare(iris,Species)
#' compare(iris %>% rename(Spec_ies=Species),Spec_ies)
#' compare(iris %>% rename(Spec_ies=Species),Spec_ies,tidy=FALSE)
#'
#'
compare <- function(dataset,var,tidy=TRUE,...) {

  if ( tidy ){
    var <- sym(quo_text(enquo(var)) %>% str_replace_all("_"," "))
    dataset <- dataset%>%
      rename_all(~str_replace_all(.,"_"," "))
  }else{
    var <- enquo(var)
  }
  formule <- rlang::new_formula(get_expr(var),quote(.))
  summary(arsenal::tableby(  formule,data=dataset, ... ))
}


#' compare mais retourne les tableau
#'
#' @param dataset
#' @param var
#' @param tidy
#'
#' @export
#'
compare_inverse <- function(dataset,var,tidy=TRUE) {

  if ( tidy ){
    var <- sym(quo_text(enquo(var)) %>% str_replace_all("_"," "))
    dataset <- dataset%>%
      rename_all(~str_replace_all(.,"_"," "))
  }else{
    var <- enquo(var)
  }

  # on récupere toutes les varialbe quali

a_parcourir <-   dataset %>%
    mutate_if(is.character,as.factor) %>%
    select_if(is.factor) %>%
    names() %>%
    setdiff(quo_text(var))


a_parcourir %>%
    map(~rlang::new_formula(as.name(.x),get_expr(var))) %>%
    # map(~tableby(formula = .x,data=dataset))
    map(tableby,data=dataset) %>%
    map(summary) %>% set_names(a_parcourir)

}
