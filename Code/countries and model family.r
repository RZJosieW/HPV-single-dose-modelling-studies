library(readxl)
library(dplyr)
library(tidyr)  
library(ggplot2)
library(scales)
dat <- read_excel("~/Desktop/all the data points.xlsx")
dat_plot_raw <- dat 
dat_plot_raw <- dat_plot_raw %>%
  mutate(
   
    scenario = factor(Scanories, levels = c("1","2")),
    model_family = factor(trimws(Modelfamily)),
    ICER2023 = as.numeric(ICER2023),
    GDP2023 = as.numeric(gsub(",", "", GDP2023))
  )
dat_plot_raw <- dat_plot_raw %>%
  filter(
    !is.na(ICER2023),
    !is.na(GDP2023),
    ICER2023 > 0,
    GDP2023 > 0,
    scenario %in% c("1", "2")
  )

if(!"country" %in% colnames(dat_plot_raw)){
  dat_plot_raw <- dat_plot_raw %>%
    mutate(country = paste0("Study_", row_number()))
} else {
  dat_plot_raw <- dat_plot_raw %>%
    mutate(country = ifelse(is.na(country),
                            paste0("Study_", row_number()),
                            country))
}
dat_plot_raw <- dat_plot_raw %>%
  mutate(ICER_pct = (ICER2023 / GDP2023) * 100)
dat_plot_balanced <- dat_plot_raw %>%
  complete(country, scenario, fill = list(ICER_pct = NA)) %>%
  group_by(country, scenario) %>%
  mutate(point_id = row_number()) %>%
  ungroup()
order_df <- dat_plot_raw %>%
  group_by(country) %>%
  summarise(mean_ICER = mean(ICER_pct, na.rm = TRUE)) %>%
  arrange(mean_ICER)

dat_plot_balanced$country <- factor(dat_plot_balanced$country, levels = order_df$country)

dat_bar_data <- dat_plot_raw %>%
  group_by(country, scenario) %>%
  summarise(ICER_pct = mean(ICER_pct, na.rm = TRUE), .groups = "drop") %>%
  complete(country, scenario, fill = list(ICER_pct = NA))

dat_bar_data$country <- factor(dat_bar_data$country, levels = order_df$country)

lancet_colors <- c(
  "1" = "#6C83B5", 
  "2" = "#C06C6C" 
)
families <- levels(dat_plot_raw$model_family)
shape_pool <- c(16, 17, 15, 18, 3, 4, 8, 7, 9, 10)
shape_values <- shape_pool[1:length(families)]
names(shape_values) <- families

dodge_width <- 0.75 
jitter_width <- 0.1

p_bar_beauty <- ggplot() +
geom_col(
  data = dat_bar_data,
  aes(x = country, y = ICER_pct, color = scenario),
  position = position_dodge(width = dodge_width), 
  width = 0.6,   
  fill = NA,
  linewidth = 0.7,
  na.rm = TRUE
) +

geom_point(
  data = dat_plot_balanced,
  aes(x = country, y = ICER_pct,
      color = scenario,
      shape = model_family,
      group = scenario    
  ),

  position = position_jitterdodge(dodge.width = dodge_width, jitter.width = jitter_width),
  size = 2.0,   
  alpha = 0.8   
) +
  geom_hline(
  yintercept = 100,
  linetype = "dashed",
  linewidth = 0.5,
  color = "grey40"
) +
  
scale_y_continuous(
  trans = scales::pseudo_log_trans(base = 10),
  breaks = c(0, 10, 50, 100, 300, 1000, 2500),
  labels = comma
) +
  coord_flip() +
  scale_color_manual(
    values = c("1" = "#6C83B5", "2" = "#C06C6C"),
    labels = c(
      "1" = "1-dose vs no vaccination",
      "2" = "2 doses vs 1 dose"
    )
  ) + 
  
  scale_fill_manual(
    values = c("1" = "#6C83B5", "2" = "#C06C6C"),
    labels = c(
      "1" = "1-dose vs no vaccination",
      "2" = "2 doses vs 1 dose"
    )
  ) +
  labs(
    x = NULL,
    y = "ICER (% of GDP/ per capita)",
    color = "Comparison",
    shape = "ModelType"
  ) +
theme_classic(base_family = "Helvetica") +
  theme(
    axis.line = element_line(linewidth = 0.4),
    axis.ticks = element_line(linewidth = 0.3),
    axis.text = element_text(size = 9.5),
    
    legend.position = "top",
    legend.box = "vertical",
    legend.title = element_text(size = 10.5),
    legend.text  = element_text(size = 9.5),
    panel.grid.major.x = element_line(color = "grey96"),
    aspect.ratio = 0.8
  )
print(p_bar_beauty)

dodge_width <- 0.75 
jitter_width <- 0.1 

p_bar_beauty <- ggplot() +
geom_col(
  data = dat_bar_data,
  aes(x = country, y = ICER_pct, color = scenario, fill = scenario), 
  position = position_dodge(width = dodge_width), 
  width = 0.6,
  alpha = 0,  
  linewidth = 0.7, 
  na.rm = TRUE
) +

geom_point(
  data = dat_plot_balanced,
  aes(x = country, y = ICER_pct,
      color = scenario,
      shape = model_family,
      group = scenario    
  ),
  position = position_jitterdodge(dodge.width = dodge_width, jitter.width = jitter_width),
  size = 2.0,    
  alpha = 0.8,
  na.rm = TRUE   
) +

geom_hline(
  yintercept = 100,
  linetype = "dashed",
  linewidth = 0.5,
  color = "grey40"
) +

scale_y_continuous(
  trans = scales::pseudo_log_trans(base = 10),
  breaks = c(0, 10, 50, 100, 300, 1000, 2500),
  labels = comma
) +
  
  coord_flip() +

scale_color_manual(values = lancet_colors) +
  scale_fill_manual(values = lancet_colors) + 
  scale_shape_manual(
    values = shape_values,
    na.translate = FALSE
  ) +

labs(
  x = NULL,
  y = "ICER (% of GDP/WTP per capita)",
  color = "Comparison",   
  fill = "Comparison",  
  shape = "Model Type"
) +

theme_classic(base_family = "Helvetica") +
  theme(
    axis.line = element_line(linewidth = 0.4),
    axis.ticks = element_line(linewidth = 0.3),
    axis.text = element_text(size = 9.5),
    
    legend.position = "top",
    legend.box = "vertical", 
    legend.title = element_text(size = 10.5, face = "bold"),
    legend.text  = element_text(size = 9.5),
    
    panel.grid.major.x = element_line(color = "grey96"),
    aspect.ratio = 0.8
  ) +
  guides(
    color = guide_legend(title = "Comparison"),
    fill = guide_legend(title = "Comparison")
  )
print(p_bar_beauty)









# second version
dodge_width <- 0.75

p_final <- ggplot() +
geom_vline(xintercept = seq_along(levels(dat_bar_data$country)), 
           color = "grey92", linewidth = 0.4) +

geom_col(
  data = dat_bar_data,
  aes(x = country, y = ICER_pct, color = scenario, fill = scenario),
  position = position_dodge(width = dodge_width), 
  width = 0.6,
  alpha = 0.1,      
  linewidth = 0.8,  
  na.rm = TRUE
) +

geom_point(
  data = dat_plot_balanced,
  aes(x = country, y = ICER_pct,
      color = scenario,
      shape = model_family,
      group = scenario
  ),
  position = position_jitterdodge(dodge.width = dodge_width, jitter.width = 0.15),
  size = 2.5,       
  stroke = 0.4,     
  alpha = 0.7,
  na.rm = TRUE 
) +

geom_hline(yintercept = 100, linetype = "dashed", color = "grey30", linewidth = 0.6) +

scale_y_continuous(
  trans = scales::pseudo_log_trans(base = 10),
  breaks = c(0, 10, 50, 100, 300, 1000, 2500),
  labels = comma,
  expand = expansion(mult = c(0, 0.08)) 
) +
  
  coord_flip() +
scale_color_manual(
  values = c("1" = "#6C83B5", "2" = "#C06C6C"),
  labels = c(
    "1" = "1-dose vs no vaccination",
    "2" = "2 doses vs 1 dose"
  )
) + 
  
  scale_fill_manual(
    values = c("1" = "#6C83B5", "2" = "#C06C6C"),
    labels = c(
      "1" = "1-dose vs No vaccination",
      "2" = "2 doses vs 1 dose"
    )
  ) +

  scale_shape_manual(
    values = shape_values,
    na.translate = FALSE 
  ) +

labs(
  x = NULL, 
  y = "ICER (% GDP per capita)", 
  color = "Comparison", 
  fill = "Comparison", 
  shape = "Model Type"
) +

theme_minimal(base_family = "Helvetica") +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(), 
    panel.grid.major.x = element_line(color = "grey95"),
    
    axis.line.x = element_line(color = "black", linewidth = 0.5),
    axis.line.y = element_line(color = "black", linewidth = 0.5),
    
    axis.text = element_text(size = 11, color = "black"),
    axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 10)),
    
    legend.position = "top",
    legend.box = "vertical",
    legend.margin = margin(b = 10),
    legend.title = element_text(size = 10, face = "bold"),
    
    aspect.ratio = 0.7
  ) +
  guides(
    fill = "none",
    color = guide_legend(title = "Comparison"),
    shape = guide_legend(title = "Model Type")
  )

print(p_final)
dodge_width <- 0.75

p_final <- ggplot() +
geom_vline(xintercept = seq_along(levels(dat_bar_data$country)), 
           color = "grey92", linewidth = 0.4) +

geom_col(
  data = dat_bar_data,
  aes(x = country, y = ICER_pct, color = scenario, fill = scenario),
  position = position_dodge(width = dodge_width), 
  width = 0.6,
  alpha = 0.1,      
  linewidth = 0.8,  
  na.rm = TRUE
) +

geom_point(
  data = dat_plot_balanced,
  aes(x = country, y = ICER_pct,
      color = scenario,
      shape = model_family,
      group = scenario
  ),
  position = position_jitterdodge(dodge.width = dodge_width, jitter.width = 0.15),
  size = 2.5,       
  stroke = 0.4,     
  alpha = 0.7,
  na.rm = TRUE 
) +

geom_hline(yintercept = 100, linetype = "dashed", color = "grey30", linewidth = 0.6) +

scale_y_continuous(
  trans = scales::pseudo_log_trans(base = 10),
  breaks = c(0, 10, 50, 100, 300, 1000, 2500),
  labels = comma,
  expand = expansion(mult = c(0, 0.08)) 
) +
  
  coord_flip() +
scale_color_manual(
  values = c("1" = "#6C83B5", "2" = "#C06C6C"),
  labels = c(
    "1" = "1-dose vs no vaccination",
    "2" = "2 doses vs 1 dose"
  )
) + 
  
  scale_fill_manual(
    values = c("1" = "#6C83B5", "2" = "#C06C6C"),
    labels = c(
      "1" = "1-dose vs no vaccination",
      "2" = "2 doses vs 1 dose"
    )
  ) +

  scale_shape_manual(
    values = shape_values,
    na.translate = FALSE 
  ) +
labs(
  x = NULL, 
  y = "ICER as % of GDP per capita", 
  color = "Comparison", 
  fill = "Comparison", 
  shape = "Model Type"
) +
theme_minimal(base_family = "Helvetica") +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(), 
    panel.grid.major.x = element_line(color = "grey95"),
    
    axis.line.x = element_line(color = "black", linewidth = 0.5),
    axis.line.y = element_line(color = "black", linewidth = 0.5),
    
    axis.text = element_text(size = 11, color = "black"),
    axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 10)),
    
    legend.position = "top",
    legend.box = "vertical",      
    legend.margin = margin(b = 10),
    legend.title = element_text(size = 10, face = "bold"),
    
    aspect.ratio = 0.7
  ) +
guides(
  fill = "none",                                         
  shape = guide_legend(title = "Model Type", order = 1), 
  color = guide_legend(title = "Comparison", order = 2)  
)
print(p_final)
