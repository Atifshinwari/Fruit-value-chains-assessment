
# =============================================================================
# Supplementary Figure S1: Quality standards and certification schemes
# =============================================================================

library(dplyr)
library(ggplot2)
library(scales)

# Read aggregated certification-response data
certificates_q <- read.csv(
  "data/certification_responses_aggregated.csv",
  check.names = FALSE
)

# Restore certification response-category order
answer_order <- certificates_q %>%
  distinct(Answers, Answer_order) %>%
  arrange(Answer_order) %>%
  pull(Answers) %>%
  unique()

certificates_q <- certificates_q %>%
  mutate(Answers = factor(Answers, levels = answer_order))

# Put crops in consistent alphabetical order
certificates_q <- certificates_q %>%
  mutate(Crops = factor(Crops, levels = rev(sort(unique(Crops)))))

max_labels_cert <- certificates_q %>%
  group_by(Questions, Crops) %>%
  filter(perc == max(perc)) %>%
  ungroup()


# Cleaner facet labels for the final figure
certificate_labels <- c(
  "1. Fumigation certificate" = "1. Fumigation certification",
  "2. Sanitary and Phytosanitary" = "2. Sanitary and phytosanitary (SPS)",
  "3. GlobalG.A.P." = "3. GLOBALG.A.P.",
  "4. HACCP" = "4. HACCP",
  "5. ISO 22000 / FSSC 22000" = "5. ISO 22000 / FSSC 22000",
  "6. Fairtrade" = "6. Fairtrade",
  "7. IFS" = "7. IFS",
  "8. BRCGS / BRC" = "8. BRCGS",
  "9. Organic certification" = "9. Organic certification"
)

# plot
certificates <- ggplot(
  certificates_q,
  aes(x = Crops, y = perc, fill = Answers)
) +
  geom_col(position = "fill") +
  coord_flip() +
  
  facet_wrap(
    ~ Questions,
    labeller = as_labeller(certificate_labels),
    ncol = 3
  ) +
  
  labs(
    x = "Fruit crops",
    y = "Share of respondents (%)",
    fill = NULL
  ) +
  
  scale_y_continuous(labels = scales::percent) +
  
  scale_fill_discrete(
    breaks = c(
      "Not needed",
      "Needed/But not available",
      "Needed/Available/But very limited",
      "Needed/Available/Sufficient coverage",
      "I don't know"
    )
  ) +
  
  geom_text(
    data = max_labels_cert,
    aes(label = paste0(round(perc), "%")),
    position = position_fill(vjust = 0.5),
    inherit.aes = TRUE,
    size = 6.5,
    fontface = "bold",
    color = "black"
  ) +
  
  guides(
    fill = guide_legend(
      nrow = 2,
      byrow = TRUE
    )
  ) +
  
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    panel.spacing.x = grid::unit(0.4, "cm"),
    
    axis.title = element_text(size = 30),
    axis.text = element_text(size = 23),
    
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_text(size = 28),
    legend.text = element_text(size = 23),
    
    strip.text = element_text(
      size = 23,
      face = "bold"
    )
  )

ggsave(
  "plot/questions/qualitative/certificates_2.png",
  plot = certificates,
  width = 20,
  height = 16,
  dpi = 600
)













