library(readxl)
library(dplyr)
library(ggplot2)
library(scales)
dat <- read_excel("~/Desktop/all the data points.xlsx")

dat <- dat %>%
  mutate(
    scenario = factor(
      Scanories,
      levels = c("1", "2")
    ),
    
    model_type = as.factor(type),
    model_family = as.factor(Modelfamily),
    
    vaccine_efficacy = as.numeric(`vaccine efficacy`),
    
    ICER2023 = as.numeric(ICER2023),
    
    GDP2023 = as.numeric(
      gsub(",", "", GDP2023)
    ),
    
    ICER_pctGDP = (ICER2023 / GDP2023) * 100,
    
    duration_num = suppressWarnings(
      as.numeric(duration)
    ),
    
    duration_clean = case_when(
      grepl("lifetime", duration, ignore.case = TRUE) ~ "lifetime",
      !is.na(duration_num) & duration_num > 20 ~ ">20y",
      !is.na(duration_num) & duration_num == 20 ~ "=20y",
      !is.na(duration_num) & duration_num < 20 ~ "<20y",
      TRUE ~ NA_character_
    ),
    
    duration_clean = factor(
      duration_clean,
      levels = c("lifetime", ">20y", "=20y", "<20y")
    )
  ) %>%
  filter(
    scenario == "2",
    !is.na(ICER_pctGDP),
    ICER_pctGDP > 0
  )

dominant_dots <- data.frame(
  GDP2023 = c(100, 200, 400, 800),
  ICER_pctGDP = c(-2, -2, -2, -2),
  vaccine_efficacy = NA,
  scenario = factor(c("2", "2", "2", "2"), levels = levels(dat$scenario)),
  model_type = factor(c("Hybrid", "Hybrid", "Hybrid", "Hybrid"), levels = levels(dat$model_type)),
  duration_clean = factor(c("lifetime", "<20y", "<20y", "<20y"), levels = c("lifetime", ">20y", "=20y", "<20y"))
)

dat_full <- bind_rows(dat, dominant_dots)
duration_colors <- c(
  "lifetime" = "#B76E6E",
  ">20y" = "#D98C8C",
  "=20y" = "#E6A6A6",
  "<20y" = "#F3CFCF"
)

shape_values <- c(
  "Dynamic" = 16,
  "Hybrid"  = 17,
  "Static"  = 4
)

size_values <- c(
  "Dynamic" = 4.6,  
  "Hybrid"  = 3.6,  
  "Static"  = 4.25  
)

ve1_dots <- dat_full %>% filter(vaccine_efficacy == 1)
first_triangle_in_grey <- dat_full %>% filter(model_type == "Hybrid", ICER_pctGDP < 0) %>% head(1)
highlight_data <- bind_rows(ve1_dots, first_triangle_in_grey)

p_LGH2 <- ggplot(
  dat_full,
  aes(x = GDP2023, y = ICER_pctGDP)
) +

annotate("rect", xmin = -Inf, xmax = Inf, ymin = -5, ymax = 0, fill = "grey92", alpha = 0.8) +
  annotate("text", x = 12, y = -1, label = "", hjust = 0, size = 3.5, fontface = "italic", color = "grey40") +

geom_hline(yintercept = 100, linetype = "dashed", linewidth = 0.5, color = "grey50") +
geom_point(
  data = highlight_data,
  aes(
    x = GDP2023,
    y = ICER_pctGDP,
    fill = "1 dose vaccine efficacy less than 95%" 
  ),
  shape = 22,
  colour = "grey70",
  size = 6.56,
  stroke = 0.8,
  position = position_nudge(y = 0.03)  
) +
geom_point(
  aes(color = duration_clean, shape = model_type, size = model_type),
  stroke = 1.2,
  alpha = 0.95
) +

scale_x_continuous(
  trans = "pseudo_log",
  breaks = c(10, 100, 1000, 10000, 100000),
  labels = comma,
  limits = c(10, 100000)
) +
  scale_y_continuous(
    trans = "pseudo_log",
    breaks = c(1, 10, 100, 1000),
    labels = function(x) { paste0(x, "%") }
  ) +
    coord_cartesian(ylim = c(-5, 1800)) + 

scale_color_manual(values = duration_colors, name = "Duration of protection") +
  scale_shape_manual(values = shape_values, name = "Model type") +
  scale_size_manual(values = size_values, name = "Model type") +
  
  scale_fill_manual(
    name = NULL, 
    values = c("1 dose vaccine efficacy less than 95%" = NA) 
  ) +

labs(tag = "B)", x = "GDP per capita (2023 USD)", y = "ICER as % of GDP per capita") +
  theme_classic(base_family = "Helvetica") +
  theme(
    axis.line = element_line(linewidth = 0.5),
    axis.ticks = element_line(linewidth = 0.4),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 10.5),
    legend.position = "right",
  
    legend.spacing.y = unit(0.15, "cm"),
    legend.text = element_text(
      size = 9,       
      color = "grey20",      
      lineheight = 0.95      
    ),
    
    aspect.ratio = 1,
    plot.tag = element_text(size = 13),
    plot.tag.position = c(0, 1)
  ) +
guides(
  color = guide_legend(order = 1, override.aes = list(size = 3.5)),
  shape = guide_legend(order = 2, override.aes = list(color = "grey40", size = 3.5)),
  
  fill = guide_legend(
    order = 3,
    label.vjust = 0.5,      
    override.aes = list(
      shape = 22,        
      color = "grey70",  
      fill = NA,         
      size = 5.0,        
      stroke = 0.8
    )
  ),
  
  size = "none" 
)
print(p_LGH2)