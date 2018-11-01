#' gridsom_boxplot
#'
#' Visualização de dados da saida da MLP
#'
#' @param conjunto conjunto respectivo a predição da MLP
#'
#' @import tidyverse
#'
#' @import ggthemes
#'
#' @return Visualização de dados
gridsom_boxplot <- function(conjunto){
  redshift_result <- gather_data(conjunto)

  ggplot(redshift_result, aes(x = as.factor(neurons), y = values, group=X)) +
    geom_boxplot()+
    stat_summary(fun.y=mean,
                 geom="point",
                 shape=20,
                 size=1,
                 color="red",
                 fill="red") +
    labs(
      title = "Redshift between 0 and 1",
      caption = "Source: Amita",
      y = "Sigma NMAD",
      x = "# de Neuronios ocultos"
    ) +
    scale_x_discrete(limits=c("5", "10", "30", "50", "70", "90")) +
    facet_grid(redshift_result$y ~ redshift_result$x) +
    theme_calc() +
    theme(
      plot.title = element_text(size = 12),
      strip.background = element_blank(),
      panel.spacing = unit(0.6, "lines")
    )
}

# scale_y_continuous(
#   breaks = seq(0, 0.06, 0.005)
# ) +
#   scale_x_continuous(breaks = c(5, 10, 30, 50, 70, 90)) +

#' gather_data
#'
#' junta o conjunto para a visualização de boxplot no grid
#'
#' @param conjunto saida do treino da MLP
#'
#' @import tidyverse
#'
#' @return Retorna o dado tratado para a visualização
gather_data <- function(conjunto){
  conjunto <- conjunto %>%
    gather(var, values, -X, -unit.class, -x, -y, -neurons)
  return(conjunto)
}
