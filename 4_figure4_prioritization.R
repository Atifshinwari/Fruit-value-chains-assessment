
#----------------------------------------------------------------------------------------------------#
                                            # Figure 4: Prioritization and gaps
#----------------------------------------------------------------------------------------------------#

library(dplyr)
library(ggplot2)
aggregate_quant <- read.csv("aggregate_quantitative_combined.csv")

# Remove missing median values before plotting
aggregate_quant <- aggregate_quant %>%
  filter(!is.na(median_val))


# Compute the median response across crops for each indicator
severity_summary <- aggregate_quant %>%
  group_by(new_questions, `Sub.areas`) %>%
  summarise(median_of_median = median(median_val, na.rm = TRUE)) %>%
  arrange(median_of_median)

# i will reorder the sub-areas on the plot legend
severity_summary <- severity_summary %>%
  mutate(`Sub.areas` = factor(`Sub.areas`, levels = c("Orchard establishment", "Soil and irrigation", "Orchard management practices", "Government support",
                                                      "On-farm handling practices", "Off-farm processing",  "Transportation and market access")
  ))

threshold <- 20  # Critical severity threshold

# Identify crops in the critical severity level
problem_crops <- aggregate_quant %>%
  filter(median_val <= threshold) %>%
  select(new_questions, Crops, median_val)

# Summarise priority crops by indicator and label all 13 crops as "All fruits"
problem_crops_labels <- problem_crops %>%
  group_by(new_questions) %>%
  summarise(
    n_crops = n_distinct(Crops),
    Crops = ifelse(n_crops == 13, "All fruits",
                   paste(unique(Crops), collapse = ", ")),
    .groups = "drop"
  )

# Plot
priority_and_commonness <- ggplot(severity_summary, aes(y = reorder(new_questions, median_of_median), x = median_of_median, fill = `Sub.areas`)) +
  annotate("rect", xmin = 0, xmax = 20, ymin = -Inf, ymax = Inf,
           fill = "red", alpha = 0.05) +
  
  annotate("rect", xmin = 20, xmax = 40, ymin = -Inf, ymax = Inf,
           fill = "orange", alpha = 0.05) +
  
  annotate("rect", xmin = 40, xmax = Inf, ymin = -Inf, ymax = Inf,
           fill = "green", alpha = 0.05) +
  
  # Band labels (placed near the top of the plot)
  annotate("text", x = 10, y = -Inf, label = "Critical (0–20)",
           vjust = 1.4, fontface = "bold", size = 7, color = "red4") +
  annotate("text", x = 30, y = -Inf, label = "Warning (21–40)",
           vjust = 1.4, fontface = "bold", size = 7, color = "orange3") +
  annotate("text", x = 50, y = -Inf, label = "Moderate (≥41)",
           vjust = 1.4, fontface = "bold", size = 7, color = "darkgreen") +
  
  geom_col(alpha = 0.7) +
  scale_fill_manual(values = c(
    "Orchard establishment" = "#005824",
    "Soil and irrigation" =  "#66C2A5",
    "Orchard management practices" = "#33a02c",
    "Government support" = "pink",
    "On-farm handling practices" = "#A8E6A1",
    "Off-farm processing" = "#6a3d9a",
    "Transportation and market access" =  "#ff7f00"
  )) +

  geom_text(
    data = problem_crops_labels,
    aes(y = new_questions, x = 0, label = Crops), # here the value moves the names (threshold -20)
    inherit.aes = FALSE,
    size = 5.7,
    color = "firebrick",
    hjust = 0
  ) +
  geom_vline(xintercept = threshold, linetype = "dashed", color = "red") +
  labs(
    x = "Average median response (%)",
    y = "Questions / Indicators",
    #title = "Severity and Commonness of Problems Across Crops"
    fill = "Value chain\nsub-sectors"
  ) +
  
  coord_cartesian(clip = "off") +
  
  guides(
    fill = guide_legend(
      nrow = 3,
      byrow = TRUE
    )
  ) +
  

  theme_bw() +
  theme(
    axis.text.y = element_text(size = 19),
    axis.text.x = element_text(size = 21),
    axis.title.x = element_text(size = 25,
                              margin = margin(t = 15)),
    axis.title.y =  element_text(size = 25),
    legend.position = 'bottom',
    legend.direction = "horizontal",
    legend.justification = c(1.5, 0.2),
    legend.text = element_text(size = 19),
    legend.title = element_text(size = 23)
    #plot.title = element_text(size = 15, face = "bold")
  )

#priority_and_commonness

ggsave("plot/questions/261012_prioritized_&_common_quant_var.png", width = 19, height = 18, dpi = 600)

