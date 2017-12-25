#' genrere une table de
#'
#' @param dataset  un jeu de données
#' @param ... des conditions
#'
#' @return un data.frame avec des 0 et des 1
#' @export
#'
#' @examples
#'
#'
#' data("demo_cloudy")
#' demo_cloudy %>%
#'  condition(
#'    "tu fumes combien ?" = list(`tu fumes ?` == 'oui' & age > 18)
#'    , "date des derniere regles" = list(sexe == 'F')
#'  )
#'
#'

condition <- function(dataset, ...){
  dots <- quos(...)

  init <- base <- dataset
  base[]<-NA
  base

#
# dataset %>%
#   mutate(x = case_when(
#     unlist(eval_tidy(!! dots[[1]] )) ~ 0,
#     TRUE ~ 1
#     ))
#
# quo(dataset %>%
#   mutate(x = case_when(
#     unlist(eval_tidy(!! dots[[1]] )) ~ 0,
#     TRUE ~ 1
#     )))
#
#
# quo(dataset %>%
#       mutate(!!!paste0(names( dots[1]),"_cond") := case_when(
#     unlist(eval_tidy(!!dots[[1]] )) ~ 1,
#     TRUE ~ 0
#     )))

# on va le faire avec plusieurs dots d un coup

# TODO PAS FAIRE DE BOUCLE
# TODO GERER LES liste multiple de parametres
  # browser()
for (d in seq_along(dots)){
  dataset <-  dataset %>%
        mutate(!!!paste0(names( dots[d]),"_cond") := case_when(
          unlist(eval_tidy(!!dots[[d]] )) ~ 1,
          TRUE ~ 0
        )
)




# attr(dataset[[names(dots[d])]],"posee") <- ff[[paste0(names( dots[d]),"_cond")]]

  }
# browser()
  out3 <- dataset %>%
    select(ends_with("_cond"))%>%
    rename_all(str_replace_all,"_cond$","")


  for ( n in names(out3)){
    base[[n]] <- out3[[n]]
  }
  base[is.na(base)] <- 1
# names(dataset)
  # attr(init,"posee") <- base
  # class(init)<-c("prout",class(init))
  # init
  base
}




# dataset %>%
#   conditon(`tu fume combien ?`, `tu fume ? == 'oui'`) %>%
#   conditon(`date des derniere regles`, `sexe == 'F'`)


# out2
