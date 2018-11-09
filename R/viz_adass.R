#'Preguiça de escrever aqui
data_tratment <- function(redshift) {
  redshift_result <- gather(redshift,
                            var,
                            values, -X, -neurons, -unit.class, -x, -y)

  single_value <-
    redshift_result_5_5_gather %>% select(everything()) %>%
    group_by(unit.class, neurons) %>%
    summarise(minimo = min(values))

  unique_select <- redshift_result %>% select(x, y, unit.class) %>% unique()


  valor_final <- left_join(valor_teste_1, unique_select, by = "unit.class")


}

#'dado de entrada
gridsom_min <- function(redshift_data) {

  redshift <- data_tratment(redshift_data)

  ggplot(redshift, aes(x = as.factor(neurons), y = minimo)) +
    geom_line(aes(group = unit.class),
              colour = "grey50") +
    geom_point(alpha = 0.7) +
    labs(
      title = expression("Redshift between 0 and 1 - Minimum " ~ sigma ~ "NMAD"),
      caption = "Source: Amita Muralikrishna",
      y = expression(sigma ~ "NMAD"),
      x = "Hidden Neurons"
    ) +
    scale_y_continuous(breaks = seq(0.010, 0.052, 0.007)) +
    scale_x_discrete(limits = c("5", "10", "30", "50", "70", "90")) +
    facet_grid(redshift$y ~ redshift$x) +
    theme_calc() +
    theme(
      plot.title = element_text(size = 12),
      strip.background = element_blank(),
      panel.spacing = unit(0.6, "lines")
    )
}
