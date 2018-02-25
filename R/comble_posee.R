#' comble posee avec les eventuelle colonne rajouté, comme si elles avaient été posée a tout le monde
#'
#' @param dataset
#' @param posee
#' @import purrr
#' @import rlang
#' @return
#' @export
#'
comble_posee <- function(dataset,posee){
  colonnes_a_rajouter <- setdiff(names(dataset),names(posee))

  posee %>%
    mutate( !!! map( colonnes_a_rajouter, ~quo(1) ) %>%
              set_names(colonnes_a_rajouter) )



}
