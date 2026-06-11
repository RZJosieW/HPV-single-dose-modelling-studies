library(ggplot2)
library(dplyr)
library(patchwork)
theme_colors <- c("Global" = "#EBF2F6", "HICs" = "#F5DED9", "LMICs" = "#98BCCF")
theme_edges  <- c("Global" = "#C6D8E4", "HICs" = "#D4A494", "LMICs" = "#6D8EA6")
apply_journal_theme <- function(p) {
  p + theme(
    legend.position = "right",
    legend.box = "vertical",
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 9, face = "bold"),
    legend.key.size = unit(0.35, "cm"),
    plot.margin = margin(5, 5, 5, 5)
  )
}
p_map_final   <- apply_journal_theme(p_map)
p_b_final     <- apply_journal_theme(p_panel_b)
p_c_final     <- apply_journal_theme(p_lollipop)
final_layout <- (p_map_final) / (p_b_final + p_c_final) + 
  plot_layout(
    heights = c(2, 1),
    widths = c(1, 1)
  ) + 
  plot_annotation(
    tag_levels = 'A',
    theme = theme(plot.tag = element_text(face = 'bold', size = 18))
  )
print(final_layout)

final_layout <- (p_map_final) / (p_b_final + p_c_final) + 
  plot_layout(
    heights = c(1.5, 1),  
    widths = c(1, 1)    
  ) + 
  plot_annotation(
    tag_levels = 'A',
    theme = theme(
      plot.tag = element_text(face = 'bold', size = 18),
      plot.margin = margin(10, 10, 10, 10)
    )
  )
print(final_layout)
