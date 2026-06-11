
library(readxl)
library(dplyr)
library(ggplot2)
library(scales)
library(patchwork)
library(grid)
dat <- read_excel("~/Desktop/all the data points.xlsx")
dat <- dat %>%
  
  mutate(
    scenario = as.factor(Scanories),
    
    scenario = recode(
      scenario,
      "1" = "1 dose vs no vaccination",
      "2" = "2 doses vs 1 dose"
    ),
    model_type = factor(
      type,
      levels = c(
        "Dynamic",
        "Hybrid",
        "Static"
      )
    ),
    model_family = as.factor(Modelfamily),
    ICER2023 = as.numeric(ICER2023),
    
    GDP2023 = as.numeric(
      gsub(",", "", GDP2023)
    ),

    ICER_pct_GDP = (
      ICER2023 / GDP2023
    ) * 100,
    duration_num =
      suppressWarnings(
        as.numeric(duration)
      ),
    
    duration_clean = case_when(
      
      grepl(
        "lifetime",
        duration,
        ignore.case = TRUE
      ) ~ "lifetime",
      
      !is.na(duration_num) &
        duration_num > 20 ~ ">20y",
      
      !is.na(duration_num) &
        duration_num == 20 ~ "=20y",
      
      !is.na(duration_num) &
        duration_num < 20 ~ "<20y",
      
      TRUE ~ NA_character_
    ),
    
    duration_clean = factor(
      duration_clean,
      levels = c(
        "lifetime",
        ">20y",
        "=20y",
        "<20y"
      )
    )
  )
model_map <- data.frame(
  
  model_family = c(
    "Harvard",
    "Song et al.",
    "Barnabas group model",
    "UNIVAC decision support model",
    "Laval",
    "Termrungruanglert et al.",
    "EpiMetHeos",
    "Zou et al.",
    "Merck"
  ),
  
  model_id = 1:9
)

dat <- dat %>%
  
  left_join(
    model_map,
    by = "model_family"
  )
lancet_colors <- c(
  
  "1 dose vs no vaccination" = "#6C83B5",
  
  "2 doses vs 1 dose" = "#C06C6C"
)

shape_values <- c(
  
  "Dynamic" = 16,
  
  "Hybrid" = 17,
  
  "Static" = 4
)
plot_protection <- function(
    vtype,
    title_text,
    is_first = FALSE,
    add_sep = FALSE
) {
  
  dat_sub <- dat %>%
    
    filter(
      !is.na(ICER_pct_GDP),
      !is.na(GDP2023),
      ICER_pct_GDP > 0,
      GDP2023 > 0,
      `protection type` == vtype
    )
  
  p <- ggplot(
    
    dat_sub,
    
    aes(
      x = GDP2023,
      y = ICER_pct_GDP
    )
    
  ) +
  geom_hline(
    
    yintercept = 100,
    
    linetype = "dashed",
    
    colour = "grey40",
    
    linewidth = 0.6
  ) +
  geom_point(
    
    aes(
      color = scenario,
      shape = model_type
    ),
    
    size = 4,
    
    stroke = 1.1,
    
    alpha = 0.8
  ) +

  geom_text(
    
    aes(
      label = model_id
    ),
    
    size = 2.8,
    
    color = "black",
    
    vjust = -0.8
  ) +
  scale_x_log10(
    
    breaks = c(
      100,
      1000,
      10000
    ),
    
    labels = comma,
    
    expand = c(0,0)
  ) +
  scale_y_log10(
    
    breaks = c(
      0.1,
      1,
      10,
      100,
      1000,
      3000
    ),
    
    labels = function(x){
      paste0(x, "%")
    },
    
    expand = c(0,0)
  ) +

  scale_color_manual(
    values = lancet_colors
  ) +

  scale_shape_manual(
    
    values = shape_values,
    
    drop = FALSE
  ) +

  guides(
    
    color = guide_legend(
      
      title = "Comparison",
      
      order = 1,
      
      override.aes = list(
        size = 4,
        alpha = 1
      )
    ),
    
    shape = guide_legend(
      
      title = "Model type",
      
      order = 2,
      
      override.aes = list(
        
        colour = "grey40",
        
        fill = "grey40",
        
        size = 4,
        
        alpha = 1
      )
    )
  ) +
  
  labs(
    
    title = title_text,
    
    x = NULL,
    
    y = "ICER (% of GDP per capita)"
  ) +
  theme_classic(
    base_family = "Helvetica"
  ) +
    
    theme(
      
      axis.line = element_line(
        linewidth = 0.5,
        color = "black"
      ),
      
      axis.ticks = element_line(
        linewidth = 0.4,
        color = "black"
      ),
      
      axis.text = element_text(
        size = 10.5,
        color = "black"
      ),
      
      axis.title.y = element_text(
        size = 11,
        margin = margin(r = 10)
      ),
      
      plot.title = element_text(
        
        size = 12,
        
        hjust = 0.5,
        
        vjust = -1.5
      ),
      
      aspect.ratio = 0.8,
      
      legend.position = "right",
      
      legend.box = "vertical",
      
      legend.margin = margin(l = 20),
      
      legend.spacing.y = unit(
        0.4,
        "cm"
      ),
      
      legend.title = element_text(
        size = 11
      ),
      
      legend.text = element_text(
        size = 10
      ),
      
      plot.margin = margin(
        t = 20,
        r = 0,
        b = 10,
        l = 0
      )
    ) +
    
    coord_cartesian(
      
      xlim = c(60, 80000),
      
      ylim = c(0.1, 5000),
      
      clip = "off"
    )

  if(add_sep){
    
    p <- p +
      
      annotate(
        
        "segment",
        
        x = 80000,
        xend = 80000,
        
        y = 0.1,
        yend = 5000,
        
        linetype = "dashed",
        
        colour = "grey35",
        
        linewidth = 0.6
      )
  }
  if(!is_first){
    
    p <- p +
      
      theme(
        
        axis.title.y = element_blank(),
        
        axis.text.y  = element_blank(),
        
        axis.ticks.y = element_blank(),
        
        axis.line.y  = element_blank()
      )
  }
  
  return(p)
}
p1 <- plot_protection(
  "9vHPV",
  "9vHPV protection",
  TRUE,
  TRUE
)

p2 <- plot_protection(
  "4vHPV",
  "4vHPV protection",
  FALSE,
  TRUE
)

p3 <- plot_protection(
  "2vHPV",
  "2vHPV protection",
  FALSE,
  FALSE
)
combined_plot <- p1 + p2 + p3 +
  
  plot_layout(
    
    ncol = 3,
    
    guides = "collect",
    
    widths = c(1,1,1)
  ) +
  
  plot_annotation(
    
    caption =
      "Model family: 1 = Harvard; 2 = Song et al.; 3 = Barnabas group model; 4 = UNIVAC decision support model; 5 = Laval; 6 = Termrungruanglert et al.; 7 = EpiMetHeos; 8 = Zou et al.; 9 = Merck.",
    
    theme = theme(
      
      plot.caption = element_text(
        
        size = 10,
        
        hjust = 0,
        
        lineheight = 1.2,
        
        margin = margin(t = 18)
      ),
      
      plot.margin = margin(
        10, 10, 10, 10
      )
    )
  ) &
  
  theme(
    legend.position = "right"
  )
print(combined_plot)

# version 2
library(readxl)
library(dplyr)
library(ggplot2)
library(scales)
library(patchwork)
library(grid)
dat <- read_excel("~/Desktop/all the data points.xlsx")
dat <- dat %>%
  mutate(
    scenario = as.factor(Scanories),
    
    scenario = recode(
      scenario,
      "1" = "1 dose vs no vaccination",
      "2" = "2 doses vs 1 dose"
    ),
    model_type = factor(
      type,
      levels = c(
        "Dynamic",
        "Hybrid",
        "Static"
      )
    ),
    ICER2023 = as.numeric(ICER2023),
    
    GDP2023 = as.numeric(
      gsub(",", "", GDP2023)
    ),
    ICER_pct_GDP = (
      ICER2023 / GDP2023
    ) * 100
  )
lancet_colors <- c(
  "1 dose vs no vaccination" = "#6C83B5",
  "2 doses vs 1 dose"        = "#C06C6C"
)

shape_values <- c(
  "Dynamic" = 16,
  "Hybrid"  = 17,
  "Static"  = 4
)
plot_protection <- function(
    vtype,
    title_text,
    is_first = FALSE,
    add_sep = FALSE
) {
  
  dat_sub <- dat %>%
    filter(
      !is.na(ICER_pct_GDP),
      !is.na(GDP2023),
      ICER_pct_GDP > 0,
      GDP2023 > 0,
      `protection type` == vtype
    )
  
  p <- ggplot(
    dat_sub,
    aes(
      x = GDP2023,
      y = ICER_pct_GDP
    )
  ) +

  geom_hline(
    yintercept = 100,
    linetype = "dashed",
    colour = "grey40",
    linewidth = 0.6
  ) +

  geom_point(
    aes(
      color = scenario,
      shape = model_type
    ),
    size = 4,
    stroke = 1.1,
    alpha = 0.8,
    show.legend = TRUE
  ) +

  scale_x_log10(
    breaks = c(
      100,
      1000,
      10000
    ),
    labels = comma,
    expand = c(0,0)
  ) +

  scale_y_log10(
    breaks = c(
      0.1,
      1,
      10,
      100,
      1000
  
    ),
    
    labels = function(x){
      paste0(x, "%")
    },
    
    expand = c(0,0)
  ) +

  scale_color_manual(
    values = lancet_colors
  ) +
  scale_shape_manual(
    values = shape_values,
    drop = FALSE,
    limits = c(
      "Dynamic",
      "Hybrid",
      "Static"
    )
  ) +

  guides(
    color = guide_legend(
      title = "Comparison",
      order = 1, 
      override.aes = list(
        size = 4,
        alpha = 1
      )
    ),
    
    shape = guide_legend(
      title = "Model type",
      order = 2, 
      override.aes = list(
        colour = "grey40",
        fill   = "grey40",
        size   = 4,
        alpha  = 1
      )
    )
  ) +
    

  labs(
    title = title_text,
    x = NULL,
    y = "ICER as % of GDP per capita"
  ) +

  theme_classic(
    base_family = "Helvetica"
  ) +
    
    theme(

      axis.line = element_line(
        linewidth = 0.5,
        color = "black"
      ),
      
      axis.ticks = element_line(
        linewidth = 0.4,
        color = "black"
      ),
      
      axis.title.x = element_text(
        size = 11,
        margin = margin(t = 12)
      ),
      
      axis.title.y = element_text(
        size = 11,
        margin = margin(r = 10)
      ),
      
      axis.text = element_text(
        size = 10.5,
        color = "black"
      ),

      aspect.ratio = 1.1,

      plot.title = element_text(
        size = 12,
        hjust = 0.5,
        margin = margin(
          t = 0,
          b = 2
        )
      ),

      legend.position = "right",
      
      legend.box = "vertical",
      
      legend.margin = margin(l = 20),
      
      legend.spacing.y = unit(
        0.4,
        "cm"
      ),
      
      legend.title = element_text(
        size = 11
      ),
      
      legend.text = element_text(
        size = 10
      ),

      plot.margin = margin(
        t = 5,
        r = 0,
        b = 5,
        l = 0
      )
    ) +

  coord_cartesian(
    xlim = c(60, 80000),
    ylim = c(0.1, 5000)
  )

  if(add_sep){
    
    p <- p +
      annotate(
        "segment",
        x = 80000,
        xend = 80000,
        y = 0.1,
        yend = 5000,
        linetype = "dashed",
        colour = "grey35",
        linewidth = 0.6
      )
  }

  if(!is_first){
    
    p <- p +
      theme(
        axis.title.y = element_blank(),
        axis.text.y  = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y  = element_blank()
      )
  }
  
  return(p)
}

p1 <- plot_protection(
  "9vHPV",
  "9vHPV protection",
  TRUE,
  TRUE
)

p2 <- plot_protection(
  "4vHPV",
  "4vHPV protection",
  FALSE,
  TRUE
)

p3 <- plot_protection(
  "2vHPV",
  "2vHPV protection",
  FALSE,
  FALSE
)

combined_plot <- p1 + p2 + p3 +
  
  plot_layout(
    ncol = 3,
    guides = "collect",
    widths = c(1,1,1)
  ) +
  
  plot_annotation(
    
    caption = "ICER expressed as percentage of GDP per capita",
    
    theme = theme(
      
      plot.caption = element_text(
        size = 12,
        hjust = 0.5,
        margin = margin(t = 15)
      ),
      
      plot.margin = margin(
        t = 10,
        r = 10,
        b = 10,
        l = 10
      )
    )
  ) &
  
  theme(
    legend.position = "right"
  )

print(combined_plot)

plot_protection <- function(
    vtype, 
    title_text, 
    is_first = FALSE, 
    add_sep = FALSE
) {
  
  dat_sub <- dat %>%
    filter(
      !is.na(ICER_pct_GDP),
      !is.na(GDP2023),
      ICER_pct_GDP > 0,
      GDP2023 > 0,
      `protection type` == vtype
    )
  
  p <- ggplot(dat_sub, aes(x = GDP2023, y = ICER_pct_GDP)) +
    geom_hline(yintercept = 100, linetype = "dashed", colour = "grey40", linewidth = 0.6) +
    geom_point(aes(color = scenario, shape = model_type), size = 4, stroke = 1.1, alpha = 0.8) +
    scale_x_log10(breaks = c(100, 1000, 10000), labels = comma, expand = c(0,0)) +
    scale_y_log10(
      breaks = c(0.1, 1, 10, 100, 1000, 3000),
      labels = function(x){paste0(x, "%")},
      expand = c(0,0)
    ) +
    scale_color_manual(values = lancet_colors) +
    scale_shape_manual(values = shape_values, drop = FALSE) +
  
    labs(title = title_text, x = NULL, y = "ICER as % of GDP per capita") +
    theme_classic(base_family = "Helvetica") +
    theme(
      axis.line = element_line(linewidth = 0.5, color = "black"),
      axis.ticks = element_line(linewidth = 0.4, color = "black"),
      axis.text = element_text(size = 10.5, color = "black"),
      axis.title.y = element_text(size = 11, margin = margin(r = 10)),
      plot.title = element_text(
        size = 12, 
        hjust = 0.5, 
        vjust = -1.5, 
        face = "plain"
      ),
      
      aspect.ratio = 0.8,
      legend.position = "right",
      plot.margin = margin(t = 20, r = 0, b = 10, l = 0) 
    ) +
    coord_cartesian(xlim = c(60, 80000), ylim = c(0.1, 5000), clip = "off")
  

  if(add_sep){
    p <- p + annotate("segment", x = 80000, xend = 80000, y = 0.1, yend = 5000,
                      linetype = "dashed", colour = "grey35", linewidth = 0.6)
  }
  
  if(!is_first){
    p <- p + theme(
      axis.title.y = element_blank(),
      axis.text.y  = element_blank(),
      axis.ticks.y = element_blank(),
      axis.line.y  = element_blank()
    )
  }
  
  return(p)
}

combined_plot <- p1 + p2 + p3 + 
  plot_layout(ncol = 3, guides = "collect") +
  plot_annotation(
   
    caption = "GDP per capita (2023 USD)",
    theme = theme(
      plot.caption = element_text(size = 12, hjust = 0.5, margin = margin(t = 20)),
      plot.margin = margin(10, 10, 10, 10)
    )
  )

print(combined_plot)







