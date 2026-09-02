
#----------------------------------------------------------------------------------------------------#
                                            # QUANTITATIVE 
#----------------------------------------------------------------------------------------------------#
library(dplyr)
library(ggplot2)
aggregate_quant <- read.csv("aggregate_quantitative_combined.csv")

# there are some NA for one question, b/c not all respondent chose Yes in the question b4, so i remove them
# although I checked does not make a difference in below plot
aggregate_quant <- aggregate_quant %>%
  filter(!is.na(median_val))


# Compute mean severity for each variable/question
severity_summary <- aggregate_quant %>%
  group_by(new_questions, `Sub.areas`) %>%
  summarise(median_of_median = median(median_val, na.rm = TRUE)) %>%
  arrange(median_of_median)

# i will reorder the sub-areas on the plot legend
severity_summary <- severity_summary %>%
  mutate(`Sub.areas` = factor(`Sub.areas`, levels = c("Orchard establishment", "Soil and irrigation", "Orchard management practices", "Government support",
                                                      "On-farm handling practices", "Off-farm processing",  "Transportation and market access")
  ))

threshold <- 20  # define your cutoff for "problem"

#the commented out part was previously used for 20% threshold only (old plot)
# Identify crops that fall below the threshold
problem_crops <- aggregate_quant %>%
  filter(median_val < threshold) %>%
  select(new_questions, Crops, median_val)

# Summarise crops per question, and label "All fruits" when all 13 crops are affected
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
           vjust = 1.4, fontface = "bold", size = 5, color = "red4") +
  annotate("text", x = 30, y = -Inf, label = "Warning (21–40)",
           vjust = 1.4, fontface = "bold", size = 5, color = "orange3") +
  annotate("text", x = 50, y = -Inf, label = "Moderate (≥41)",
           vjust = 1.4, fontface = "bold", size = 5, color = "darkgreen") +
  
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
    size = 4,
    color = "firebrick",
    hjust = 0
  ) +
  geom_vline(xintercept = threshold, linetype = "dashed", color = "red") +
  labs(
    x = "Average median response (%)",
    y = "Questions / Variables",
    #title = "Severity and Commonness of Problems Across Crops"
    fill = "Value chain\nsub-sectors"
  ) +
  
  coord_cartesian(clip = "off") +

  theme_bw() +
  theme(
    axis.text.y = element_text(size = 16),
    axis.text.x = element_text(size = 16),
    axis.title = element_text(size = 22),
    legend.position = 'right',
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 15)
    #plot.title = element_text(size = 15, face = "bold")
  )

#priority_and_commonness

ggsave("plot/questions/260605_prioritized_&_common_quant_var.png", width = 18, height = 15, dpi = 600)
# 
# # In this plot, we put all the median of all median_responses for a prioritization based on the variables for all quantiative data
# # the ranking is based from top (good) to bottom (worse). In addition,
# # we defined a threshold of 40% to identify crops that falls behind it and to see the crops that are common/share the problem
# # with other crops for different variables.


# # library(dplyr)
# # library(ggplot2)
# # 
# # ─────────────────────────────────────────────
# # 1) Load & clean
# # ─────────────────────────────────────────────
# aggregate_quant <- read.csv("aggregate_quantitative_combined.csv") %>%
#   filter(!is.na(median_val))
# 
# questions_to_remove <- c(
#   "How developed is the PRODUCTION sector?",
#   "How developed is the PROCESSING sector?",
#   "How developed is the MARKETING sector?"
# )
# 
# aggregate_quant <- read.csv("aggregate_quantitative_combined.csv") %>%
#   filter(!is.na(median_val)) %>%
#   filter(!new_questions %in% questions_to_remove)
# 
# # ─────────────────────────────────────────────
# # 2) Shared constants
# # ─────────────────────────────────────────────
# threshold1 <- 20
# #threshold2 <- 40
# 
# severity_levels_ordered <- rev(c(
#   "Critical (0–20%)",
#   "Warning (21–40%)",
#   "Moderate (>40%)"
# ))
# 
# subareas_levels <- c(
#   "Orchards establishment",
#   "Soil and irrigation",
#   "Orchard management practices",
#   "Government support",
#   "On-farm handling practices",
#   "Off-farm processing",
#   "Transportation and markets"
# )
# 
# subareas_colors <- c(
#   "Orchards establishment"       = "#005824",
#   "Soil and irrigation"          = "#66C2A5",
#   "Orchard management practices" = "#33a02c",
#   "Government support"           = "pink",
#   "On-farm handling practices"   = "#A8E6A1",
#   "Off-farm processing"          = "#6a3d9a",
#   "Transportation and markets"   = "#ff7f00"
# )
# 
# # Facet strip colors by severity
# severity_strip_colors <- c(
#   "Critical (0–20%)" = "#f4b6b6",  # light red
#   "Warning (21–40%)" = "#ffe0a3",  # light orange
#   "Moderate (>40%)"= "#cdeccd"  # light green
# )
# 
# 
# # ─────────────────────────────────────────────
# # 3) Question-level summary
# #    ONE row per question × Sub.area
# #    Severity derived from question_median so each question goes to one facet
# # ─────────────────────────────────────────────
# question_severity <- aggregate_quant %>%
#   group_by(new_questions, Sub.areas) %>%
#   summarise(median_of_median = median(median_val, na.rm = TRUE), .groups = "drop") %>%
#   group_by(new_questions) %>%
#   mutate(question_median = median(median_of_median, na.rm = TRUE)) %>%
#   ungroup() %>%
#   mutate(
#     SeverityLevel = case_when(
#       question_median <= 20 ~ "Critical (0–20%)",
#       question_median > 20 & question_median <= 40 ~ "Warning (21–40%)",
#       question_median > 40 ~ "Moderate (>40%)"
#     ),
#     SeverityLevel = factor(SeverityLevel, levels = severity_levels_ordered),
#     Sub.areas     = factor(Sub.areas, levels = subareas_levels)
#   )
# 
# # ─────────────────────────────────────────────
# # 4) Crop labels (only for critical questions based on crop-level median_val < 20)
# #    Join SeverityLevel from question_severity for correct faceting
# # ─────────────────────────────────────────────
# problem_crops_labels <- aggregate_quant %>%
#   filter(median_val < threshold1) %>%
#   left_join(
#     question_severity %>% distinct(new_questions, SeverityLevel, question_median),
#     by = "new_questions"
#   ) %>%
#   group_by(new_questions, SeverityLevel) %>%
#   summarise(
#     n_crops = n_distinct(Crops),
#     Crops = ifelse(
#       n_crops == 13,
#       "All fruits",
#       paste(sort(unique(Crops)), collapse = ", ")
#     ),
#     .groups = "drop"
#   ) %>%
#   mutate(SeverityLevel = factor(SeverityLevel, levels = severity_levels_ordered))
# 
# # problem_crops_labels <- aggregate_quant %>%
# #   # 1️⃣ Classify crops by their own median_val
# #   mutate(
# #     CropSeverity = case_when(
# #       median_val <= 20 ~ "Critical (0–20%)"
# #       #median_val > 20 & median_val <= 40 ~ "Warning (21–40%)",
# #       #median_val > 40 ~ "Moderate (>40%)"
# #     )
# #   ) %>%
# #
# #   # 2️⃣ Join question-level severity so crops go into correct facet
# #   left_join(
# #     question_severity %>%
# #       distinct(new_questions, QuestionSeverity = SeverityLevel),
# #     by = "new_questions"
# #   ) %>%
# #
# #   # 3️⃣ Keep crops only if crop severity matches question severity
# #   filter(CropSeverity == QuestionSeverity) %>%
# #
# #   # 4️⃣ Summarise crop names per question × severity
# #   group_by(new_questions, CropSeverity) %>%
# #   summarise(
# #     n_crops = n_distinct(Crops),
# #     Crops = ifelse(
# #       n_crops == 13,
# #       "All fruits",
# #       paste(sort(unique(Crops)), collapse = ", ")
# #     ),
# #     .groups = "drop"
# #   ) %>%
# #
# #   # 5️⃣ Rename for faceting consistency
# #   rename(SeverityLevel = CropSeverity) %>%
# #
# #   mutate(
# #     SeverityLevel = factor(SeverityLevel, levels = severity_levels_ordered)
# #   )
# # ─────────────────────────────────────────────
# # 5) Plot
# # ─────────────────────────────────────────────
# priority_and_commonness <- ggplot(
#   question_severity,
#   aes(
#     y    = reorder(new_questions, question_median),
#     x    = median_of_median,
#     fill = Sub.areas
#   )
# ) +
#   geom_col(alpha = 0.7) +
#   scale_fill_manual(values = subareas_colors) +
# 
#   # Crop annotations (critical only)
#   geom_text(
#     data = problem_crops_labels,
#     aes(y = new_questions, x = 0.5, label = Crops),
#     inherit.aes = FALSE,
#     hjust = 0,
#     color = "firebrick",
#     size = 3.5
#   ) +
# 
#   # Two thresholds
#   geom_vline(xintercept = threshold1, linetype = "dashed", color = "red") +
#   #geom_vline(xintercept = threshold2, linetype = "dashed", color = "red") +
# 
#   facet_grid(SeverityLevel ~ ., scales = "free_y", space = "free_y") +
# 
#   scale_x_continuous(breaks = c(0, 20, 40, 60, 80, 100)) +
# 
#   labs(
#     x    = "Average median response (%)",
#     y    = "Questions / Variables",
#     fill = "Value chain\nsub-sectors"
#   ) +
# 
#   theme_bw() +
#   theme(
#     strip.text.y     = element_text(size = 14, face = "bold"),
#     axis.text.y      = element_text(size = 16),
#     axis.text.x      = element_text(size = 16),
#     axis.title       = element_text(size = 22),
#     legend.position  = "right",
#     legend.text      = element_text(size = 14),
#     legend.title     = element_text(size = 15)
#   )
# 
# ggsave(
#   "plot/questions/260222_prioritized_&_common_quant_var.png",
#   priority_and_commonness,
#   width = 18, height = 15, dpi = 600
# )
#
# # ggsave("plot/questions/251208_prioritized_&_common_quant_var.png", width = 18, height = 15, dpi = 600)



#--------------------------------------------------------------------------------#
# For the value chain mapping plot, I summarized different thresholds below:
#--------------------------------------------------------------------------------#

# Define thresholds of interest
thresholds <- c(20, 40, 60)

# Create crop groupings for each threshold
problem_crops_levels <- aggregate_quant %>%
  group_by(new_questions) %>%
  summarise(
    below_20 = {
      crops <- unique(Crops[median_val < 20])
      if (length(crops) == 13) "All fruits" else paste(crops, collapse = ", ")
    },
    below_40_above_20 = {
      crops <- unique(Crops[median_val < 40 & median_val > 20])
      if (length(crops) == 13) "All fruits" else paste(crops, collapse = ", ")
    },
    below_60_above_40 = {
      crops <- unique(Crops[median_val < 60 & median_val > 40])
      if (length(crops) == 13) "All fruits" else paste(crops, collapse = ", ")
    },
    n_below_20 = sum(median_val < 20, na.rm = TRUE),
    n_below_40_above_20 = sum(median_val < 40 & median_val > 20, na.rm = TRUE),
    n_below_60_above_40 = sum(median_val < 60 & median_val > 40, na.rm = TRUE),
    .groups = "drop"
  )


severity_with_levels <- severity_summary %>%
  left_join(problem_crops_levels, by = "new_questions")

# # i want to check if data b/w two the dfs and columns matches or not
# identical(severity_summary$new_questions, problem_crops_levels$new_questions)
# which(severity_summary$new_questions != problem_crops_levels$new_questions)
# setdiff(severity_summary$new_questions, problem_crops_levels$new_questions)  # In df1 but not in df2
# setdiff(problem_crops_levels$new_questions, severity_summary$new_questions)  # In df2 but not in df1

# define classes of crops near the critical bottleneck section
severity_with_levels <- severity_with_levels %>%
  mutate(
    severity_class = case_when(
      n_below_20 == 13 ~ "Critical (<20, All fruits)",
      n_below_40_above_20 == 13 ~ "Warning (<40, All fruits)",
      n_below_60_above_40 == 13 ~ "Moderate (<60, All fruits)",
      TRUE ~ "Mixed"
    )
  )

write.csv(severity_with_levels, 'data/results/severity_with_levels_quant_QUESTIONS ORDERERED.csv', row.names = FALSE)

# Arrange data as previous plots
# Extract sub-area order from aggregate_quantitative
subarea_order <- aggregate_quant %>%
  distinct(`Sub.areas`, No) %>%
  arrange(No) %>%
  pull(`Sub.areas`) %>%
  unique()

# Apply the same order 
severity_with_levels <- severity_with_levels %>%
  mutate(`Sub.areas` = as.character(`Sub.areas`)) %>%  # reset any old factor
  mutate(`Sub.areas` = factor(`Sub.areas`, levels = subarea_order)) %>%
  arrange(`Sub.areas`)   # make visible in correct order

write.csv(severity_with_levels, 'data/results/severity_with_levels_quant_SUBAREAS ORDERED.csv', row.names = FALSE)


# Below may not be necessary 
plot_data <- severity_with_levels %>%
  select(new_questions, `Sub.areas`, n_below_20, n_below_40, n_below_60) %>%
  pivot_longer(
    cols = starts_with("n_below"),
    names_to = "Threshold",
    values_to = "n_crops"
  ) %>%
  mutate(
    Bottleneck = case_when(
      Threshold == "n_below_20" ~ "Critical (<20%)",
      Threshold == "n_below_40" ~ "Severe (<40%)",
      Threshold == "n_below_60" ~ "Moderate (<60%)"
    ))

# ques_subarea_1
ques_subarea_1 <- severity_with_levels %>% 
  filter(Sub.areas == 'Orchards establishment')

ggplot(ques_subarea_1, aes(x = new_questions, y = median_of_median)) +
       geom_col() +
         geom_text(aes(label = below_20), y = 20, color = "red") +
         geom_text(aes(label = below_40_above_20),  y = 40, color = "blue") +
         geom_text(aes(label = below_60_above_40),  y = 60, color = "green")



# Prioritize crops based on sub-areas
library(dplyr)

#Step 1 – Define the severity levels/bands on the long data
aggregate_banded <- aggregate_quant %>%
  mutate(
    band = case_when(
      median_val < 20                      ~ "below_20",
      median_val >= 20 & median_val < 40   ~ "between_20_40",
      median_val >= 40 & median_val < 100   ~ "between_40_60",
      TRUE ~ NA_character_
    )
  )

#Step 2 – Count how many questions each crop appears in, per subarea & band
crop_priority <- aggregate_banded %>%
  filter(!is.na(band)) %>%                     # only bands of interest
  group_by(Sub.areas, band, Crops) %>%
  summarise(
    n_questions = n_distinct(new_questions),       # how many questions this crop is “problematic” in
    .groups = "drop"
  ) %>%
  arrange(Sub.areas, band, desc(n_questions), Crops)

# For each threshold level, the high n_questions represents the higher priority crops under specific severity level of specific sub-area

#Step 3 – Optional: get a prioritized list string per subarea × band
crop_priority_summary <- crop_priority %>%
  group_by(Sub.areas, band) %>%
  summarise(
    crops_prioritized = paste0(Crops, " (", n_questions, ")", collapse = ", "),
    .groups = "drop"
  )


ggplot(crop_priority_summary, aes(x = Sub.areas, y = band)) +
  geom_text(aes(label = crops_prioritized), hjust = 0) +
  theme_minimal() +
  coord_flip()

library(tidytext)  # for reorder_within

# get the order for sub areas
subarea_order <- unique(aggregate_quant$Sub.areas)

# apply the order to our plot
crop_priority$Sub.areas <- factor(crop_priority$Sub.areas,
                                  levels = subarea_order)

crop_priority$band <- factor(
  crop_priority$band,
  levels = c("below_20", "between_20_40", "between_40_60")
)

# plot
crops_severity_subareas <- ggplot(crop_priority, 
       aes(x = n_questions, 
           y = reorder_within(Crops, n_questions, interaction(band, Sub.areas)), 
           fill = band)) +
  geom_col() +
  #facet_wrap(~ band + Sub.areas, scales = "free_y", ncol = 3, nrow = 7) +
  facet_grid(rows = vars(Sub.areas),
             cols  = vars(band),
             scales = "free_y") +      # each small plot gets its own y-scale
  scale_y_reordered() +
  labs(x = "Number of questions", y = "Crops") +
  theme_bw()

ggsave("plot/questions/251120_crops_severity_subareas.png", width = 18, height = 15, dpi = 600)  

# filter each level
below_20 <- crop_priority %>% 
  filter(band == 'below_20')

below_20_plot <- ggplot(below_20, 
                                  aes(x = n_questions, 
                                      y = reorder_within(Crops, n_questions, interaction(band, Sub.areas)), 
                                      fill = band)) +
  geom_col() +
  #facet_wrap(~ band + Sub.areas, scales = "free_y", ncol = 3, nrow = 7) +
  facet_grid(rows = vars(Sub.areas),
             cols  = vars(band),
             scales = "free_y") +      # each small plot gets its own y-scale
  scale_y_reordered() +
  scale_x_continuous(breaks = seq(0, max(below_20$n_questions, na.rm = TRUE), by = 1)) +   
  labs(x = "Number of questions", y = "Crops") +
  theme_bw()

ggsave("plot/questions/251120_subareas_critical_crops.png", width = 8, height = 12, dpi = 600)  

  # [1] Active breeding program for developing new high-yielding varieties? + combine two options and turn around
      #Active breeding program (although limited in scope & fund)

  # [2] Which irrigation system is mainly used?
      # Modern vs. conventional irrigation (flood, basin, furrow) 

  # [3] Are there available and accessible subsidy programs for farmers?
      # Available & accessible subsidy programs: none mentioned fully available and accessible

  # [4] Are there reliable and accessible credit facilities for farmers?
  #     Availability of credit facilities     : none mentioned fully available and accessible                                

  # [5] To what extent do the extension officers have knowledge and skills in fruit production practices?
  #     Expertise of extension officers on production practices   : Expert-level & good vs. intermediate & basic knowledge   : no knowledge was not mentioned

  # [6] Is there an active and timely early warning for farmers on frost, winds, pests, diseases, etc.?
  #     Active (although not punctual) early warning for farmers vs. non-availability     

  # [7] Is there a crop-specific active growers association?

  # [8] Is there an efficient local transport system available for supply from the farm to local markets?
  #     Standardized local transport system (farm to local market), standard vs. inefficient & costly

  # [9] Depending on the fruit species, which type of private sector is mainly involved in fruit processing?
  #     Involvement of SMEs vs. middlemen & exporters
  #     large processing firms are 0

