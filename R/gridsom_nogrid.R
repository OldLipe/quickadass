#' gridsom_line
#'
#' Visualização dos neurônios do SOMs utilizando ggplot2
#'
#' @author Felipe
#'
#' @param redshift data.frame tratado pela função ggsom
#'
#' @import tidyverse
#'
#' @import hrbrthemes
#'
#' @import ggthemes
#'
#' @return Visualização dos grids da rede SOMs
#'
#' @export
#'
no_gridsom_line <- function(redshift){

  redshift_gather <- no_grid_helper(redshift)

  ggplot(redshift_gather, aes(x=var,y= values, group=id)) +
    geom_line(alpha=0.3, colour = "grey50") +
    geom_point(alpha=0.7) +
    labs(title="Redshift between 0 and 7",
         caption="Source: Amita Muralikrishna",
         y="Values",
         x="Filter") +
    scale_x_discrete(limits = c("u", "g", "r", "i", "z"),
                     expand = c(0.009,0)) +
    scale_y_continuous(limits  = c(0,30),
                       breaks = seq(0,30,5),
                       expand = c(0,0)) +
    theme_calc() +
    theme(plot.title = element_text(size=12),
          strip.background = element_blank())
}

#' no_grid_helper
#'
#' Necessário para a visualização de dados
#'
#' @param redshift_data data.frame tratado pela função ggsom
#'
#' @import tidyverse
#'
#' @return conjunto de dados wide
#'
no_grid_helper <- function(redshift_data) {

  redshift_data$id <- c(1:nrow(redshift_data))

  redshift_gather <-
    gather(redshift_data, var, values, -redshift, -id)
}

