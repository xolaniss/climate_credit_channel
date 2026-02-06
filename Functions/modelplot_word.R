

modelplot_word <- function(
    data = data, 
    conf_level = 0.99,
    facet = FALSE,
    vcov = "NeweyWest",
    coef_map = NULL,
    variables_colors = 6
    ) {
  modelplot(
    data,
    conf_level = conf_level,
    facet = facet,
    vcov = vcov,
    coef_map = coef_map,
    background = list(geom_vline(
      xintercept = 0, color = "#81a9ad"
    ))
  ) +
    theme_bw() +
    theme(
      legend.position = "right",
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    ) +
    theme(
      text = element_text(size = 7),
      strip.background = element_rect(colour = "white", fill = "white"),
      axis.text.x = element_text(angle = 90)
    ) +
    
    scale_color_manual(values = pnw_palette("Winter", variables_colors))
  
  
}
