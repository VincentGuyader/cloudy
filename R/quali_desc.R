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
quali_desc <- function(dataset,var,posee = dataset %>% condition()){
  var <- enquo(var)

# browser()
 dataset <-  dataset %>%
    select(!!var) %>%
    cbind(
     posee %>%
      select(!!var)%>% rename_all(function(...){"xx"}))  %>%
    filter(xx == 1) %>%
    select(-xx)


  dataset2 <- rbind(dataset %>%
                      mutate(grpXXX = "Complet"),
                    dataset %>%
                      mutate(grpXXX = "Sans données manquantes") %>%
                      filter(!is.na(!! var))
  )


if (nrow(dataset2) == 0) {return(NULL)}

  summary(
    tableby(
      as.formula(glue("grpXXX~ `{quo_text(var)}`")),
      data=dataset2,
      control=tableby.control(cat.stats = c("N","count_with_na"),total=FALSE,test = FALSE)
    ))


}




#' @noRd
#' @import dplyr
#' @export
quali_desc_ <- function(dataset,var){
  # var <- enquo(var)

  dataset2 <- rbind(dataset %>%
                      mutate(grpXXX = "Complet"),
                    dataset %>%
                      mutate(grpXXX = "Sans données manquantes") %>%
                      # filter(!is.na(var)) #marche pas
                      # filter(!is.na(dataset[[var]])) # la honte
                    # filter_at(vars(var),any_vars(!is.na(.)))
                    filter(!is.na(!!sym(var)))
  )

  summary(
    tableby(
      as.formula(glue("grpXXX ~ `{var}`")),
      data=dataset2,
      control=tableby.control(cat.stats = c("N","count_with_na"),
                              total=FALSE,test = FALSE)
    ))


}


#' Title
#'
#' @param dataset
#' @param posee
#' @importFrom purrr map_chr
#' @import rlang
#'
#' @return
#' @export
#'
quali_desc_all <- function(dataset,posee = dataset %>% condition()){


  quali <- dataset %>%
    map_chr(~class(.x)[[1]]) %>%
    .[.%in% c("character","factor")] %>%
    names()

# on supprime de quali les question jamias posée

  quali <- setdiff(quali,names(posee)[posee %>% colSums() == 0])



  for (var in quali){
    cat("###   ", var, "\n   ")
    cat("\n")
    cat("\n")
    # demo_cloudy[[var]]
    # demo_cloudy %>% quali_desc( !! sym(var))
    # demo_cloudy %>% quali_desc_(var)
    dataset %>% quali_desc(!! sym(var),posee = posee )

  }

}
