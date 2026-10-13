# =====================================================================================
# Figure 3 generation: Value chain performance: median expert response across all crops
# =====================================================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(stringr)
library(ggtext)
library(ggh4x)
library(grid)

# Read analysis-ready aggregated data
quant_merge <- read.csv(
  "data/aggregate_quantitative_combined.csv",
  check.names = FALSE
)
 
# =========================
# 1) Crop order
# =========================
crop_order <- quant_merge %>%
  filter(!is.na(median_val)) %>%
  group_by(Crops) %>%
  summarise(
    med_overall = median(median_val, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(med_overall)) %>%
  pull(Crops)

quant_merge <- quant_merge %>%
  mutate(
    Crops_reorder = factor(Crops, levels = crop_order)
  )

# =========================
# 2) Base plotting data
# =========================
plot_df <- quant_merge %>%
  filter(!is.na(median_val)) %>%
  mutate(
    y_q = reorder(new_questions, -No),
    Sub_area_facet = factor(
      `Sub-areas`,
      levels = c(
        "Orchard establishment",
        "Soil and irrigation",
        "Orchard management practices",
        "Government support",
        "On-farm handling practices",
        "Off-farm processing",
        "Transportation and market access",
        "Global median"
      )
    ),
    Stage_group = case_when(
      `Sub-areas` %in% c(
        "Orchard establishment",
        "Soil and irrigation",
        "Orchard management practices",
        "Government support"
      ) ~ "Production",
      `Sub-areas` %in% c(
        "On-farm handling practices",
        "Off-farm processing"
      ) ~ "Processing",
      `Sub-areas` %in% c(
        "Transportation and market access"
      ) ~ "Marketing",
      TRUE ~ NA_character_
    ),
    Stage_group = factor(
      Stage_group,
      levels = c("Production", "Processing", "Marketing", "Overall")
    )
  )

# =========================
# 3) Row medians per question within facet
# =========================
row_meds <- plot_df %>%
  group_by(Stage_group, Sub_area_facet, y_q) %>%
  summarise(
    row_median = median(median_val, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Crops_reorder = "Indicator median"
  )

# =========================
# 4) Column medians per crop within facet
# =========================
col_meds <- plot_df %>%
  group_by(Stage_group, Sub_area_facet, Crops_reorder) %>%
  summarise(
    col_median = median(median_val, na.rm = TRUE),
    .groups = "drop"
  )

# =========================
# 5) Extend factor levels
# =========================
plot_df2 <- plot_df %>%
  mutate(
    Crops_reorder = factor(
      as.character(Crops_reorder),
      levels = c(levels(quant_merge$Crops_reorder), "Indicator median")
    ),
    y_q = factor(
      as.character(y_q),
      levels = c("Crop median", levels(y_q), "Global median")
    )
  )

row_meds2 <- row_meds %>%
  mutate(
    Crops_reorder = factor(
      as.character(Crops_reorder),
      levels = levels(plot_df2$Crops_reorder)
    ),
    y_q = factor(
      as.character(y_q),
      levels = levels(plot_df2$y_q)
    ),
    Stage_group = factor(
      as.character(Stage_group),
      levels = c("Production", "Processing", "Marketing", "Overall")
    ),
    flag = case_when(
      row_median > 40 ~ "max",
      row_median <= 20 ~ "min",
      TRUE ~ "mid"
    )
  )

col_meds2 <- col_meds %>%
  mutate(
    Crops_reorder = factor(
      as.character(Crops_reorder),
      levels = levels(plot_df2$Crops_reorder)
    ),
    y_q = factor(
      "Crop median",
      levels = levels(plot_df2$y_q)
    ),
    Stage_group = factor(
      as.character(Stage_group),
      levels = c("Production", "Processing", "Marketing", "Overall")
    ),
    flag = case_when(
      col_median > 40 ~ "max",
      col_median <= 20 ~ "min",
      TRUE ~ "mid"
    )
  )

# =========================
# 6) Global medians per crop from long data
# =========================
global_meds <- plot_df2 %>%
  filter(as.character(Crops_reorder) != "Indicator median") %>%
  group_by(Crops_reorder) %>%
  summarise(
    median_val = median(median_val, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Stage_group = factor(
      "Overall",
      levels = c("Production", "Processing", "Marketing", "Overall")
    ),
    Sub_area_facet = factor(
      "Global median",
      levels = c(
        "Orchard establishment",
        "Soil and irrigation",
        "Orchard management practices",
        "Government support",
        "On-farm handling practices",
        "Off-farm processing",
        "Transportation and market access",
        "Global median"
      )
    ),
    y_q = factor(
      "Global median",
      levels = levels(plot_df2$y_q)
    ),
    flag = case_when(
      median_val > 40 ~ "max",
      median_val <= 20 ~ "min",
      TRUE ~ "mid"
    )
  )

# =========================
# 7) Stage labels for merged strips
# =========================
stage_levels_markdown <- c(
  "<b>Production</b>",
  "<b>Processing</b>",
  "<b>Marketing</b>",
  ""
)

plot_df2 <- plot_df2 %>%
  mutate(
    Stage_group_lab = factor(
      case_when(
        as.character(Stage_group) == "Production" ~ "<b>Production</b>",
        as.character(Stage_group) == "Processing" ~ "<b>Processing</b>",
        as.character(Stage_group) == "Marketing"  ~ "<b>Marketing</b>",
        as.character(Stage_group) == "Overall"    ~ ""
      ),
      levels = stage_levels_markdown
    ))

row_meds2 <- row_meds2 %>%
  mutate(
    Stage_group_lab = factor(
      case_when(
        as.character(Stage_group) == "Production" ~ "<b>Production</b>",
        as.character(Stage_group) == "Processing" ~ "<b>Processing</b>",
        as.character(Stage_group) == "Marketing"  ~ "<b>Marketing</b>",
        as.character(Stage_group) == "Overall"    ~ ""
      ),
      levels = stage_levels_markdown
    ))

col_meds2 <- col_meds2 %>%
  mutate(
    Stage_group_lab = factor(
      case_when(
        as.character(Stage_group) == "Production" ~ "<b>Production</b>",
        as.character(Stage_group) == "Processing" ~ "<b>Processing</b>",
        as.character(Stage_group) == "Marketing"  ~ "<b>Marketing</b>",
        as.character(Stage_group) == "Overall"    ~ ""
      ),
      levels = stage_levels_markdown
    )
  )

global_meds <- global_meds %>%
  mutate(
    Stage_group_lab = factor(
      case_when(
        as.character(Stage_group) == "Production" ~ "<b>Production</b>",
        as.character(Stage_group) == "Processing" ~ "<b>Processing</b>",
        as.character(Stage_group) == "Marketing"  ~ "<b>Marketing</b>",
        as.character(Stage_group) == "Overall"    ~ ""
      ),
      levels = stage_levels_markdown
    )
  )

# =========================
# 8) Final plotting data
# =========================
plot_df2    <- plot_df2    %>% mutate(across(where(is.numeric), round))
row_meds2   <- row_meds2   %>% mutate(across(where(is.numeric), round))
col_meds2   <- col_meds2   %>% mutate(across(where(is.numeric), round))
global_meds <- global_meds %>% mutate(across(where(is.numeric), round))

plot_df_final <- bind_rows(plot_df2, global_meds)



# =========================
# 9) Plot
# =========================
quantitative_variables <- ggplot(
  plot_df_final,
  aes(x = Crops_reorder, y = y_q, fill = median_val)
) +
  geom_tile(color = "white") +
  
  ggh4x::facet_nested(
    Stage_group_lab + Sub_area_facet ~ .,
    scales = "free_y",
    space = "free_y",
    labeller = labeller(
      Sub_area_facet = function(x) {
        x[x == "Global median"] <- ""
        x <- stringr::str_wrap(x, width = 16)
        x <- gsub("\n", "<br>", x)
        x
      }
    ),
    strip = ggh4x::strip_nested(
      bleed = FALSE,
      background_y = list(
        element_rect(fill = "white", colour = "black"),
        element_rect(fill = "grey90", colour = "black")
      ),
      by_layer_y = TRUE
    )
  ) +
  
  scale_x_discrete(
    labels = function(x) {
      x[x == "Indicator median"] <- "<b>Indicator median</b>"
      x
    }
  ) +
  
  scale_y_discrete(
    labels = function(x) {
      x[x == "Crop median"] <- "<b>Crop median</b>"
      x[x == "Global median"] <- "<b>Global median</b>"
      x
    }
  ) +
  
  scale_fill_gradientn(
    colors = c("orange", "white", "steelblue"),
    limits = c(0, 100),
    breaks = seq(0, 100, 20),
    oob = scales::squish
  ) +
  
  geom_text(
    data = row_meds2,
    aes(
      x = Crops_reorder,
      y = y_q,
      label = row_median,
      color = flag
    ),
    inherit.aes = FALSE,
    size = 5.5,
    fontface = "bold"
  ) +

geom_text(
  data = col_meds2,
  aes(
    x = Crops_reorder,
    y = y_q,
    label = col_median,
    color = flag
  ),
  inherit.aes = FALSE,
  size = 5.5,
  fontface = "bold"
) +

geom_text(
  data = global_meds,
  aes(
    x = Crops_reorder,
    y = y_q,
    label = median_val,
    color = flag
  ),
  inherit.aes = FALSE,
  size = 5.5,
  fontface = "bold"
) +
  
  scale_color_manual(
    values = c(
      max = "darkgreen",
      min = "red3",
      mid = "black"
    ),
    guide = "none"
  ) +
  
  labs(
    x = "Fruit crops",
    y = "Questions / Indicators",
    fill = "Median response (%)"
    #fill = "Median\nresponse (%)"
  ) +
  
  theme_bw() +
  theme(
    panel.background = element_rect(fill = "grey95", colour = NA),
    axis.text.x = ggtext::element_markdown(size = 18, angle = 45, hjust = 1),
    axis.text.y = ggtext::element_markdown(size = 18),
    axis.title = element_text(size = 25),
    legend.title = element_text(size = 22),
    legend.text = element_text(size = 18),
    #legend.key.size = unit(0.6, "cm"),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box.margin = margin(t = 12),
    strip.text.y = element_text(size = 15, face = "bold", colour = 'black', margin = margin(t = 10, b = 10)),
    strip.text.y.right = ggtext::element_markdown(
      size = 15,
      hjust = 0.5,
      vjust = 0.5,
      colour = 'black',
      face = 'bold',
      margin = margin(t = 10, b = 10)
    )
    ) +
      guides(
        fill = guide_colorbar(
          direction = "horizontal",
          title.position = "top",
          title.hjust = 0.5,
          barwidth = unit(8, "cm"),
          barheight = unit(0.5, "cm")
        )
      ) +
  coord_cartesian(clip = "off")

ggsave(
  "plot/questions/261007_quant_vars_crops_reordered_Combined_with_rowcol_medians_heighted.png",
  quantitative_variables,
  width = 15,   # old 15.7
  height = 18.5,   # old value 15.9
  dpi = 600
)






#------------------------------------ Table for calculations---------------------------------#


library(dplyr)
library(tidyr)

# -----------------------------
# 1) Filter valid data
# -----------------------------
df <- quant_merge %>%
  filter(!is.na(median_val))

# -----------------------------
# 2) Compute crop order (overall median across variables)
# -----------------------------
crop_order <- df %>%
  group_by(Crops) %>%
  summarise(med_overall = median(median_val, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(med_overall)) %>%   # Order crops from higher to lower median performance
  pull(Crops)

# -----------------------------
# 3) Stable question order (same as plot: by No)
# -----------------------------
question_levels <- df %>%
  arrange(No) %>%
  distinct(No, new_questions) %>%
  pull(new_questions)

# -----------------------------
# 4) Prepare ordered data
# -----------------------------
df <- df %>%
  mutate(
    Crops = factor(Crops, levels = crop_order),
    new_questions = factor(new_questions, levels = rev(question_levels))
  )

# -----------------------------
# 5) Create wide table (heatmap shape)
# -----------------------------
wide_tbl <- df %>%
  select(new_questions, Crops, median_val) %>%
  pivot_wider(names_from = Crops, values_from = median_val)

# -----------------------------
# 6) Add row medians (across crops)
# -----------------------------
row_meds <- df %>%
  group_by(new_questions) %>%
  summarise(IndicatorMedian = median(median_val, na.rm = TRUE), .groups = "drop")

wide_tbl2 <- wide_tbl %>%
  left_join(row_meds, by = "new_questions")

# Ensure crop columns follow correct order
wide_tbl2 <- wide_tbl2 %>%
  select(new_questions, all_of(crop_order), IndicatorMedian)

# -----------------------------
# 7) Add bottom row (crop medians across variables)
# -----------------------------
col_meds <- df %>%
  group_by(Crops) %>%
  summarise(col_median = median(median_val, na.rm = TRUE), .groups = "drop")

bottom_vals <- setNames(col_meds$col_median, as.character(col_meds$Crops))

bottom_row <- tibble(
  new_questions = "Crop median",
  !!!as.list(bottom_vals),
  RowMedian = NA_real_
)

final_table <- bind_rows(wide_tbl2, bottom_row)

# -----------------------------
# 8) Save table
# -----------------------------
final_table    <- final_table    %>% mutate(across(where(is.numeric), round))

write.csv(final_table,
          "data/results/260224_quant_vars_median_table.csv",
          row.names = FALSE)




# ===========================================================
# STAGE-LEVEL MEDIANS AND INTERQUARTILE RANGES
# Stage summaries are calculated from indicator-level medians
# ===========================================================

# 1) Question-level medians WITH thematic area (and facet id)
row_meds_ok <- quant_merge %>%
  filter(!is.na(median_val)) %>%
  group_by(`Sub-areas`, No, new_questions) %>%
  summarise(
    row_median = median(median_val, na.rm = TRUE),
    .groups = "drop"
  )

# 2) Thematic medians (7 values) = median of question medians
thematic_medians <- row_meds_ok %>%
  group_by(`Sub-areas`) %>%
  summarise(
    thematic_median = median(row_median, na.rm = TRUE),
    Q1 = quantile(row_median, 0.25, na.rm = TRUE),
    Q3 = quantile(row_median, 0.75, na.rm = TRUE),
    n_questions     = n_distinct(No),
    .groups = "drop"
  ) %>%
  arrange(desc(thematic_median))

# 3) Join them
theme_medians <- row_meds_ok %>%
  left_join(thematic_medians, by = "Sub-areas") %>%
  arrange(No)

theme_medians    <- theme_medians    %>% mutate(across(where(is.numeric), round))
write.csv(theme_medians,
          "data/results/260224_thematic_medians.csv",
          row.names = FALSE)


# Overall Median (inter-quartile range IQR)
Overall_median <- quant_merge %>% 
  summarise(
    overall_median = median(median_val, na.rm = TRUE),
    Q1 = quantile(median_val, 0.25, na.rm = TRUE),
    Q3 = quantile(median_val, 0.75, na.rm = TRUE)
  )


overall_median <- final_table %>%
  left_join(theme_medians, by = 'new_questions') %>%
  arrange(No)

overall_median    <- overall_median    %>% mutate(across(where(is.numeric), round))

write.csv(overall_median,
          "data/results/260224_overall_medians.csv",
          row.names = FALSE)

global_median <- quant_merge %>%
  filter(!is.na(median_val)) %>%
  summarise(overall_median = median(median_val, na.rm = TRUE),
            Q1 = quantile(median_val, 0.25, na.rm = TRUE),
            Q3 = quantile(median_val, 0.75, na.rm = TRUE))

global_median    <- global_median    %>% mutate(across(where(is.numeric), round))
write.csv(global_median,
          "data/results/260224_global_medians.csv",
          row.names = FALSE)


