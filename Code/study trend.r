library(ggplot2)
library(dplyr)
library(tidyr)
library(tibble)
setting_cols <- c(
  "Global" = "#EBF2F6", 
  "HICs"   = "#F5DED9", 
  "LMICs"  = "#98BCCF" 
)
article_data <- tribble(
  ~Year, ~Objective,          ~Setting,
  2018, "Both",              "LMICs",
  2018, "Health impact",     "LMICs",
  2021, "Both",              "LMICs",
  2022, "Health impact",     "LMICs",
  2022, "Health impact",     "LMICs",
  2022, "Both",              "HICs",
  2023, "Both",              "LMICs",
  2023, "Both",              "LMICs",
  2023, "Health impact",     "Global",
  2023, "Both",              "LMICs",
  2023, "Health impact",     "LMICs",
  2023, "Both",              "LMICs",
  2023, "Both",              "LMICs",
  2023, "Both",              "LMICs",
  2024, "Health impact",     "LMICs",
  2024, "Both",              "HICs",
  2024, "Both",              "LMICs",
  2024, "Health impact",     "LMICs",
  2024, "Health impact",     "Global",
  2024, "Health impact",     "HICs",
  2024, "Health impact",     "HICs",
  2024, "Both",              "LMICs",
  2024, "Both",              "LMICs",
  2024, "Both",              "LMICs",
  2024, "Both",              "LMICs"
) %>%
  mutate(
    Objective = recode(Objective, "Both" = "Health impact and cost-effectiveness analysis", "Health impact" = "Health impact"),
    Setting = factor(Setting, levels = c("Global", "HICs", "LMICs")),
    Objective = factor(Objective, levels = c("Health impact", "Health impact and cost-effectiveness analysis"))
  )

bar_data <- article_data %>% count(Year, Setting, name = "n") %>%
  complete(Year = 2018:2024, Setting = factor(c("Global", "HICs", "LMICs"), levels = c("Global", "HICs", "LMICs")), fill = list(n = 0))

line_data <- article_data %>% count(Year, Objective, name = "n") %>%
  complete(Year = 2018:2024, Objective = factor(c("Health impact", "Health impact and cost-effectiveness analysis"), levels = c("Health impact", "Health impact and cost-effectiveness analysis")), fill = list(n = 0))
p_panel_b <- ggplot() +
  # bar 
  geom_col(data = bar_data, aes(x = Year, y = n, fill = Setting), width = 0.62, color = "white", linewidth = 0.35, alpha = 0.95) +
  # line
  geom_line(data = line_data, aes(x = Year, y = n, group = Objective, linetype = Objective), color = "#4A4A4A", linewidth = 0.52) +

  geom_point(data = line_data, aes(x = Year, y = n), shape = 21, size = 2.8, stroke = 0.6, color = "#2C4F6C", fill = "#FFFFFF") +

  scale_fill_manual(values = setting_cols, name = "Study setting") +
  scale_linetype_manual(values = c("solid", "dashed"), name = "Study objective", labels = c("Health impact", "Health impact and\ncost-effectiveness analysis")) +

  scale_x_continuous(breaks = 2018:2024) +
  scale_y_continuous(breaks = seq(0, 12, 2), limits = c(0, 12), expand = expansion(mult = c(0, 0.02))) +
  labs(x = "Publication year", y = "Number of studies") +
  theme_classic(base_family = "Helvetica") +
  theme(
    axis.title = element_text(size = 10.5),
    axis.text = element_text(size = 9.5, color = "#2A2A2A"),
    axis.line = element_line(linewidth = 0.35, color = "#4A4A4A"),
    panel.grid = element_blank(),
    legend.position = "right",
    legend.title = element_text(size = 9.2, face = "bold"),
    legend.text = element_text(size = 8.4)
  ) +
  guides(fill = guide_legend(order = 1), linetype = guide_legend(order = 2, override.aes = list(color = "#4A4A4A", linewidth = 0.6)))

print(p_panel_b)