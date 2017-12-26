
#' Title
#'
#' @param x
#' @param ...
#' @importFrom  tibble column_to_rownames
#' @import dplyr
#' @import forcats
#' @return
#' @export
#'
count_with_na <- function(x,...){
  table(x,useNA = "always") %>%
    data.frame() %>%
    rename(count=Freq) %>%
    mutate(pct=count/sum(count)*100) %>%
    mutate_all(funs(case_when(
      is.na(.) ~ "manquant",
      TRUE ~ as.character(.)
    ))) %>%
    mutate(x = forcats::as_factor(x),
           x = forcats::fct_relevel(x,"manquant")
    ) %>%
    arrange(x) %>%
    mutate_at(vars(-x),as.numeric) %>%
    # mutate(pct= glue("{round(pct,2)} %")) %>% # trouver dans arsenal::
    tibble::column_to_rownames("x")
}



#' Title
#'
#' @param x
#' @param digits
#' @param pct
#'
#' @return
#' @export
#'
#' @examples
format.countpct <- function(x,digits=5, pct='') {
  if(!is.null(ncol(x))) {
    ## multiple rows
    xformat <- cbind.data.frame(format(x[,1], digits=digits), format(x[,2], digits=digits))
    row.names(xformat) <- row.names(x)
    digits <- digits - 2
    return (apply (xformat, 1, function(xrow) paste (xrow[1], " (", format (round (as.numeric (xrow[2]), digits), nsmall = digits),
                                                     pct, ")", sep = "")))
  } else {
    ## just one row
    return(paste(signif(x[1],digits=digits), "(",signif(x[2],digits=digits), ")",sep=""))
  }
}


#' Title
#'
#' @param x
#' @param digits
#' @param pct
#'
#' @return
#' @export
#'
#' @examples
format.count_with_na <- function(x,digits=5, pct='') {
  if(!is.null(ncol(x))) {
    ## multiple rows
    xformat <- cbind.data.frame(format(x[,1], digits=digits), format(x[,2], digits=digits))
    row.names(xformat) <- row.names(x)
    digits <- digits - 2
    return (apply (xformat, 1, function(xrow) paste (xrow[1], " (", format (round (as.numeric (xrow[2]), digits), nsmall = digits),
                                                     pct, ")", sep = "")))
  } else {
    ## just one row
    return(paste(signif(x[1],digits=digits), "(",signif(x[2],digits=digits), ")",sep=""))
  }
}
