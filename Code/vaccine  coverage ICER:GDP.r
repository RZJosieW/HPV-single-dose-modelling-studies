library(readxl)
library(dplyr)
library(ggplot2)
library(scales)
library(stringr)

dat <- read_excel("~/Desktop/all the data points.xlsx")
dat <- dat %>%
  
  mutate(
    scenario = as.factor(Scanories),
    
    scenario = recode(
      scenario,
      "1" = "1 dose vs no vaccination",
      "2" = "2 doses vs 1 dose"
    ),

    ICER2023 = as.numeric(ICER2023),
    
    GDP2023 = as.numeric(
      gsub(",", "", GDP2023)
    ),

    ICER_pctGDP =
      (ICER2023 / GDP2023) * 100,

    duration_num = as.numeric(
      str_extract(duration, "\\d+")
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
    ),

    coverage = as.numeric(coverage),
    
    coverage_group = factor(
      coverage,
      levels = c(
        60,
        70,
        80,
        90,
        100
      )
    ),

    color_group =
      paste0(
        scenario,
        "_",
        duration_clean
      )
  )

color_map <- c(
  
  # Scenario 1 (blue)
  "1 dose vs no vaccination_lifetime" = "#2B4581",
  "1 dose vs no vaccination_>20y"     = "#5373B4",
  "1 dose vs no vaccination_=20y"     = "#89A1D2",
  "1 dose vs no vaccination_<20y"     = "#BDC9E1",
  
  # Scenario 2 (red)
  "2 doses vs 1 dose_lifetime" = "#990000",
  "2 doses vs 1 dose_>20y"     = "#D73027",
  "2 doses vs 1 dose_=20y"     = "#F4A582",
  "2 doses vs 1 dose_<20y"     = "#FDDBC7"
)

p_final <- ggplot(
  
  dat,
  
  aes(
    x = GDP2023,
    y = ICER_pctGDP
  )
  
) +

geom_hline(
  
  yintercept = 100,
  
  linetype = "dashed",
  
  color = "grey50",
  
  linewidth = 0.6
) +

geom_point(
  
  aes(
    fill = color_group,
    size = coverage_group
  ),
  
  shape = 21,
  
  color = "white",
  
  stroke = 0.35,
  
  alpha = 0.9
) +

scale_x_log10(
  
  breaks = c(
    1000,
    10000,
    100000
  ),
  
  labels = comma,
  
  expand = expansion(
    mult = c(0.05, 0.05)
  )
) +

scale_y_continuous(
  
  trans = "pseudo_log",
  
  breaks = c(
    1,
    10,
    100,
    1000
  ),
  
  labels = function(x){
    paste0(x, "%")
  },
  
  expand = expansion(
    mult = c(0.02, 0.05)
  )
) +

coord_cartesian(
  ylim = c(0.8, 2000)
) +

scale_fill_manual(
  
  values = color_map,
  
  name = "Scenario & Duration",
  
  labels = c(
    
    "1 dose vs no vaccination_lifetime" =
      "1 dose vs no vaccination · lifetime",
    
    "1 dose vs no vaccination_>20y" =
      "1 dose vs no vaccination · >20y",
    
    "1 dose vs no vaccination_=20y" =
      "1 dose vs no vaccination · =20y",
    
    "1 dose vs no vaccination_<20y" =
      "1 dose vs no vaccination · <20y",
    
    "2 doses vs 1 dose_lifetime" =
      "2 doses vs 1 dose · lifetime",
    
    "2 doses vs 1 dose_>20y" =
      "2 doses vs 1 dose · >20y",
    
    "2 doses vs 1 dose_=20y" =
      "2 doses vs 1 dose · =20y",
    
    "2 doses vs 1 dose_<20y" =
      "2 doses vs 1 dose · <20y"
  )
) +

scale_size_manual(
  
  values = c(
    "60"  = 2.5,
    "70"  = 3.5,
    "80"  = 4.5,
    "90"  = 5.5,
    "100" = 6.5
  ),
  
  name = "Vaccine coverage (%)"
) +

labs(
  
  x = "GDP per capita (2023 USD)",
  
  y = "ICER as % of GDP per capita"
) +
theme_bw(
  base_family = "Helvetica"
) +
  
  theme(
    
    panel.grid.minor =
      element_blank(),
    
    panel.grid.major =
      element_line(
        color = "grey96",
        linewidth = 0.25
      ),
    
    panel.border =
      element_rect(
        colour = "black",
        fill = NA,
        linewidth = 0.8
      ),
    
    axis.text =
      element_text(
        size = 10,
        color = "black"
      ),
    
    axis.title =
      element_text(
        size = 11,
        face = "bold"
      ),
    
    legend.title =
      element_text(
        size = 10,
        face = "bold"
      ),
    
    legend.text =
      element_text(
        size = 9
      ),
    
    legend.spacing.y =
      unit(0.25, "cm"),
    
    aspect.ratio = 1
  ) +
guides(
  
  fill = guide_legend(
    
    order = 1,
    
    override.aes = list(
      size = 4,
      alpha = 1
    )
  ),
  
  size = guide_legend(
    
    order = 2,
    
    override.aes = list(
      
      shape = 21,
      
      fill = "grey50",
      
      color = "white",
      
      stroke = 0.5
    )
  )
)
print(p_final)