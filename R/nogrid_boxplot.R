nogrid_boxplot <- function(redshift, title){
  redshift_gather <- gather(redshift,
                            var,
                            values,
                            -X,
                            -neurons)

  ggplot(redshift_gather, aes(x = as.factor(neurons), y = values, group=X)) +
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
      title = paste("Redshift between 0 and", title),
      caption = "Source: Amita Muralikrishna",
      y = expression(sigma~"NMAD"),
      x = "Hidden Neurons"
    ) +
    scale_x_discrete(limits=c("5", "10", "30", "50", "70", "90")) +
    theme_calc() +
    theme(
      plot.title = element_text(size = 12),
      strip.background = element_blank(),
      panel.spacing = unit(0.6, "lines")
    )



}
