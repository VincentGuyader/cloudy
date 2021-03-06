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
#'
#' dessin_quali(iris$Species,"coucou",camembert = TRUE)
#' dessin_quali(iris$Species,"coucou",camembert = FALSE)
#' dessin_quali_var(dataset = iris,var = Species,camembert = TRUE)
#' dessin_quali_var(dataset = iris,var = Species,camembert = FALSE)
#' dessin_quali_all(iris)
#' dessin_quali_all(iris,camembert = TRUE)
#'
dessin_quali_var <- function(dataset,var,groupe,camembert = FALSE){
#   var <- enquo(var)
#   dataset %>% pull(!!var) %>%
#   table() %>%
#   as.data.frame() %>%
#   setNames(c("Var","Freq")) %>%
# ggplot(aes(x = Var, y = Freq)) +
#   geom_bar(stat = "identity", color = "black", fill = "grey") +
#   labs(title = "", x = paste("\n",quo_text(var)), y = "Frequence\n") +
#   theme_classic()
  var <- enquo(var)
  dessin_quali(var = dataset %>% pull( !! var) ,nom = quo_text(var),camembert = camembert)

}



dessin_quali <- function(var,nom,camembert = FALSE){
  if (missing(nom)){nom <- deparse(substitute(var))}

  if ( !camembert ){
  out <- var %>%
  table() %>%
  as.data.frame() %>%
  setNames(c("Var","Freq")) %>%
ggplot(aes(x = Var, y = Freq)) +
  geom_bar(stat = "identity", color = "black", fill = "grey") +
  labs(title = "", x = paste("\n",nom), y = "Frequence\n") +
  theme_classic()
}
  if ( camembert ){

    out <-
      var %>%
      table() %>%
      as.data.frame() %>%
      setNames(c("Var","Freq")) %>%
      ggplot( aes(x="", y=Freq, fill=Var))+
      geom_bar(width = 1, stat = "identity") +
      coord_polar("y", start=0) +
      # theme_classic()+
      theme(axis.text.x=element_blank()) +
      theme(plot.background = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(),
        panel.grid=element_blank(),
        axis.ticks = element_blank(),
        plot.title=element_text(size=14, face="bold")
      )+labs(Var="coucou")+
      theme(rect = element_rect(fill = NA,
                               size = 12, colour = NA, linetype = 0), panel.background = element_rect(fill = NA))+
      viridis::scale_fill_viridis(name = "",discrete = TRUE)+
      ggtitle(nom)
  }
  out
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
dessin_quali_all <- function(dataset,groupe,tidy=TRUE,camembert=FALSE){

  if ( tidy ){
    dataset <- dataset %>% rename_all(~str_replace_all(.,"_"," "))
  }

  if ( is_missing(groupe)){

out <-  dataset %>%
    select_if(is.factor) %>%
    map2(.,names(.),dessin_quali,camembert=camembert)
}
  if ( !is_missing(groupe)){

out <-  dataset %>%
  select_if(is.factor) %>%
  dessin_quali_var(groupe=groupe)
}

out
}
