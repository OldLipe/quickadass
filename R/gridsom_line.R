#' gridsom_line
#'
#' Visualização dos neurônios do SOMs utilizando ggplot2
#'
#' @author Felipe
#'
#' @param redshift_ggsom data.frame tratado pela função ggsom
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
gridsom_line <- function(redshift_ggsom) {
  redshift_ggsom_gather <- junta_conjunto(redshift_ggsom)

  ggplot(redshift_ggsom_gather, aes(x = var, y = values, group = id)) +
    geom_line(colour = "grey50") +
    geom_point() +
    labs(
      title = "Redshift between 0 and 1",
      caption = "Source: Amita",
      y = "Values",
      x = "Filter"
    ) +
    geom_text(aes(
      y = 7.5,
      x = 4.1,
      label = sum
    ), check_overlap = TRUE) +
    facet_grid(redshift_ggsom_gather$y ~ redshift_ggsom_gather$x) +
    scale_x_discrete(limits = c("u", "g", "r", "i", "z"),
                     expand = c(0.009, 0.3)) +
    scale_y_continuous(
      limits  = c(0, 30),
      breaks = seq(0, 30, 5),
      expand = c(0, 0)
    ) +
    theme_calc() +
    theme(
      plot.title = element_text(size = 12),
      strip.background = element_blank(),
      panel.spacing = unit(0.6, "lines")
    )
}

#' junta_conjunto
#'
#' Necessário para a visualização de dados
#'
#' @param redshift_ggsom data.frame tratado pela função ggsom
#'
#' @import tidyverse
#'
#' @return conjunto de dados unido
#'
junta_conjunto <- function(redshift_ggsom) {
  redshift_ggsom_gather <-
    gather(redshift_ggsom,
           var,
           values,-unit.class,-redshift,-id,-x,-y,-sum)
}
