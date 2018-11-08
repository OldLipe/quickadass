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
gridsom_boxplot <- function(conjunto, redshift_title){
  redshift_result <- gather_data(conjunto)

  ggplot(redshift_result, aes(x = as.factor(neurons), y = values, group=X)) +
    geom_boxplot(outlier.colour="black",
                 outlier.shape=20,
                 outlier.size=2) +
    stat_summary(fun.y=mean,
                 geom="point",
                 shape=20,
                 size=1,
                 color="red",
                 fill="red") +
    labs(
      title = paste("Redshift between 0 and", redshift_title),
      caption = "Source: Amita Muralikrishna",
      y = expression(sigma~"NMAD"),
      x = "Hidden Neurons"
    ) +
    scale_x_discrete(limits=c("5", "10", "30", "50", "70", "90")) +
    scale_y_continuous(
      breaks = seq(0.00, 0.36, 0.06)
    ) +
    facet_grid(redshift_result$y ~ redshift_result$x) +
    theme_calc() +
    theme(
      plot.title = element_text(size = 12),
      strip.background = element_blank(),
      panel.spacing = unit(0.6, "lines")
    )
}

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
