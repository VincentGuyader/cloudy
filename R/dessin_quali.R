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
      ggplot( aes(x=factor(1), fill=Var))+
      geom_bar(width = 1)+
      coord_polar("y") +
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
        scale_fill_discrete(name = "")+ggtitle(nom)
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
