#' Title
#'
#' @param dataset
#' @param var
#' @param tidy
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
compare <- function(dataset,var,tidy=TRUE) {

  if ( tidy ){
    var <- sym(quo_text(enquo(var)) %>% str_replace_all("_"," "))
    dataset <- dataset%>%
      rename_all(~str_replace_all(.,"_"," "))
  }else{
    var <- enquo(var)
  }
  formule <- rlang::new_formula(get_expr(var),quote(.))
  summary(arsenal::tableby(  formule,data=dataset ))
}
