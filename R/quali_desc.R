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




#' @noRd
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
