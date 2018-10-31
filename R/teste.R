ggplot(teste_gather, aes(x = neurons, y = values, group = neurons)) +
  geom_boxplot()+
  stat_summary(fun.y=mean, geom="point", shape=23, size=4) +
  labs(
    title = "Redshift between 0 and 1",
    caption = "Source: Amita",
    y = "Sigma NMAD",
    x = "# de Neuronios ocultos"
  ) +
  scale_y_continuous(
    breaks = seq(0, 0.06, 0.005)
  ) +
  scale_x_continuous(breaks = c(5, 10, 30, 50, 70, 90)) +

   facet_grid(teste_gather$y ~ teste_gather$x) +
  theme_calc() +
  theme(
    plot.title = element_text(size = 12),
    strip.background = element_blank(),
    panel.spacing = unit(0.6, "lines")
  )
