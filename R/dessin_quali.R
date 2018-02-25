#' Title
#'
#' @param dataset
#' @param var
#'
#' @return
#' @export
#' @import rlang
#' @import ggplot2
#' @import dplyr
#' @examples
#' dessin_quali_var(iris,Species)
#' dessin_quali(iris$Species)
dessin_quali_var <- function(dataset,var,groupe){
  var <- enquo(var)
  dataset %>% pull(!!var) %>%
  table() %>%
  as.data.frame() %>%
  setNames(c("Var","Freq")) %>%
ggplot(aes(x = Var, y = Freq)) +
  geom_bar(stat = "identity", color = "black", fill = "grey") +
  labs(title = "", x = paste("\n",quo_text(var)), y = "Frequence\n") +
  theme_classic()
}


dessin_quali <- function(var,nom){
  if (missing(nom)){nom <- deparse(substitute(var))}

  var %>%
  table() %>%
  as.data.frame() %>%
  setNames(c("Var","Freq")) %>%
ggplot(aes(x = Var, y = Freq)) +
  geom_bar(stat = "identity", color = "black", fill = "grey") +
  labs(title = "", x = paste("\n",nom), y = "Frequence\n") +
  theme_classic()
}


#' Title
#'
#' @param dataset un data.frame
#'
#' @return list of ggplot
#' @export
#'
#' @examples
#' dessin_quali_all(iris)
#'
dessin_quali_all <- function(dataset,groupe){

  if ( is_missing(groupe)){

out <-  dataset %>%
    select_if(is.factor) %>%
    map2(.,names(.),dessin_quali)
}
  if ( !is_missing(groupe)){

out <-  dataset %>%
  select_if(is.factor) %>%
  dessin_quali_var(groupe=groupe)
}

out
}
