
library(dplyr)

# inventory of crops in provinces
crop_inventory <- read.csv('data/data.csv')

#The goal is to identify the fewest provinces that contribute the majority of the production, to reduce the number of provinces

#almond <- crop_inventory %>%
  #select(Province, Almond_p) %>%
 # mutate(Almond_p = as.numeric(gsub(",", "", Almond_p))) %>% # make numeric and remove comma style
 # arrange(desc(Almond_p)) %>% # arrange in descending order
#  mutate(cumulative_production = cumsum(Almond_p),# cumulative production
      #   total_sum = sum(Almond_p), # total sum
      #   cumulative_percentage = (cumulative_production/total_sum * 100)) # cumulative percentage

#Cum_percen_prov <- almond %>%
  #filter(cumulative_percentage <=80)

# I define a function to consider other crops as well
crops_cumulative <- function(data, crop_column, threshold = 80) {
  data %>%
    select(Province, !!sym(crop_column)) %>% # !! tells R to evaluate sym = Almond_p as a column name, not text
    mutate(!!sym(crop_column) := as.numeric(gsub(",", "", !!sym(crop_column)))) %>%
    arrange(desc(!!sym(crop_column))) %>%
    filter(!is.na(!!sym(crop_column))) %>%
    mutate(cumulative_production = cumsum(!!sym(crop_column)),
           total_sum = sum(!!sym(crop_column)),
           cumulative_percentage = (cumulative_production / total_sum) * 100) %>%
    filter(cumulative_percentage <= threshold)
}


# apply this to all crops in a loop

crops <- c("Almond",   "Walnut",  "Apple", "Apricot", "Peach", "Plum" , "Pomegranate", "Grape", "Cherry", "Citrus", "Fig")

list_provinces <- c()

for (crop in crops) {

  top_province <- crops_cumulative(data = crop_inventory, crop_column = crop)
  top_province$Crop <- crop  # Add crop name as a column
  names(top_province)[2] <- 'Production_MT' # rename second column to ease bind rows lateron
  list_provinces[[crop]] <- top_province # append data to list
  

  
} 

# put in one dataframe
province_list <- bind_rows(list_provinces)

# add pistachio and pinenuts (wild and organic)
pistachio <- data.frame(Province = c('Badghis', 'Samangan', 'Hirat', 'Faryab', 'Balkh', 'Jawzjan', 'Kunduz'),
                       Crop = 'Pistachio')

pinenut <- data.frame(Province = c('Paktya', 'Paktika', 'Khost', 'Logar', 'Nuristan', 'Kunar', 'Nangarhar', 'Kapisa'),
                      Crop = 'Pinenut')

province_list <- bind_rows(province_list, pistachio, pinenut)

write.csv(province_list, 'data/crop_province_share.csv', row.names = FALSE)

# Need to take care of Pomegranate separately for Kandahar, it shows only Kandahar

require(sf)
require(ggplot2)
# 1: Get the shape file of Afghanistan
library(geodata)
# Load the outline for Afghanistan # Get Afghanistan shape file
sf_afghanistan <- geodata::gadm(country ="AF", level =1, path = tempdir()) 

# Convert the sf object to an sp object
sf_afghanistan <- sf::st_as_sf(sf_afghanistan) 

# Calculate centroids (for circle placement)
province_centroids <- sf::st_centroid(sf_afghanistan)

# Extract coordinates and province names
province_coords <- province_centroids %>%
  mutate(long = sf::st_coordinates(.)[,1],
         lat = sf::st_coordinates(.)[,2]) %>%
  select(NAME_1, long, lat) %>%
  rename(Province = NAME_1)

# Merge to get lat-long for each crop in province
province_crop_coords <- province_list %>%
  left_join(province_coords, by = "Province")

# Unique crops and colors
#crop_colors <- RColorBrewer::brewer.pal(length(unique(province_crop_coords$Crop)), "Set3")
crop_colors <- c(
  "Almond_p" = "#8B4513",      # Dark Brown
  "Walnut_p" =  "#D2B48C",       # Light Brown (Tan)
  "Apple_p" = "#FF0000",        # Red
  "Apricot_p" = "#FF8C00",      # Soft Apricot Orange
  "Peach_p" = "lightpink",        # Pink-Orange
  "Plum_p" = "#800080",         # Purple
  "Pomegranate_p" = "firebrick",  # Dark Red
  "Grape_p" = "lightgreen",        # Dark Purple
  "Cherry_p" = "#DC143C"    ,    # Crimson
  "Pistachio_p" = "darkgreen", 
  "Pinenut_p" = "antiquewhite",
  "Citrus_p" = "#FFB300",       # Bright Orange
  "Fig_p" = "#4B0082"     # Indigo (dark fig skin tone)"
)

names(crop_colors) <- unique(province_crop_coords$Crop)


# Extract centroids from the sf_afghanistan object for labeling
sf_afghanistan_centroids <- sf_afghanistan %>%
  st_centroid() %>%  # Calculate centroids
  st_coordinates()    # Extract coordinates

# Add the coordinates to the sf_afghanistan data
sf_afghanistan <- sf_afghanistan %>%
  mutate(long = sf_afghanistan_centroids[,1],
         lat = sf_afghanistan_centroids[,2])

sf_afghanistan$HASC_1 <- gsub('AF.', '', sf_afghanistan$HASC_1) # remove AF.

# Get bounding box
bbox <- sf::st_bbox(sf_afghanistan)


require(ggspatial)
# Plot map with crop circles
major_crop_distr <- ggplot() +
  geom_sf(data = sf_afghanistan, fill = "white", color = "black") +  # Map of Afghanistan
  geom_jitter(data = province_crop_coords,
             aes(x = long, y = lat, color = Crop),
             size = 4, shape = 19, width = 0.2, height = 0.25) +  # Circles for crops
  
  # Add province names from HASC_1 column
  geom_text(data = sf_afghanistan,
            aes(x = long, y = lat, label = HASC_1),
            size = 4.5, color = "black", fontface = "bold") +
           # nudge_y = 0.1, nudge_x = 0.1) +  # Adjust position to avoid overlapping
  
  scale_color_manual(values = crop_colors) +
  
  # Add scale bar and north arrow
  annotation_scale(location = "bl", width_hint = 0.2, 
                   text_col = "grey20", line_col = "grey20", text_cex = 0.8) +
  annotation_north_arrow(location = "tl", which_north = "true",
                         style = north_arrow_fancy_orienteering(text_col = "black"),
                         height = unit(1.2, "cm"), width = unit(1.2, "cm")) +
  
  
  # add graticules
  coord_sf() +  # datum = NA to avoid showing projection info
  
  theme_bw() +
  labs(title = "",
       x = "Longitude", y = "Latitude", color = "Major Crops\nDistribution") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_text(size = 18),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 14))

ggsave('plot/crop_distribution/250430major_crop_distr.png', width = 12, height = 10, dpi = 300)


#-------------------------------------------------------------------------------------
# limit the symbols position within the boundaries of respective province
#-------------------------------------------------------------------------------------

library(sf)
library(dplyr)

# Empty list for new jittered points
jittered_points <- list()

# Loop through each province
for (prov in unique(province_crop_sf$Province)) {
  
  # Subset province polygon and crop points
  prov_poly <- sf_afghanistan %>% filter(NAME_1 == prov)
  prov_points <- province_crop_sf %>% filter(Province == prov)
  
  # Get province centroid
  prov_centroid <- st_centroid(prov_poly)
  
  # Create a buffer around centroid (e.g., 0.2 degrees; adjust as needed)
  centroid_buffer <- st_buffer(prov_centroid, dist = 0.2)
  
  # Intersect buffer with province polygon to constrain area
  constrained_area <- st_intersection(centroid_buffer, prov_poly)
  
  # Generate random points inside the constrained area
  new_geoms <- st_sample(constrained_area, size = nrow(prov_points), type = "random")
  
  # Convert to sf object with original crop data
  new_points_sf <- st_sf(prov_points, geometry = new_geoms)
  
  # Save
  jittered_points[[prov]] <- new_points_sf
}

# Combine all provinces' jittered points
province_crop_sf_jittered <- do.call(rbind, jittered_points)
#-------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------
# Applying jitter
#-------------------------------------------------------------------------------------

# Convert province_crop_coords to sf object
province_crop_sf <- st_as_sf(province_crop_coords, coords = c("long", "lat"), crs = 4326)


# apply jitter to all provinces 
province_crop_sf_jittered <- st_jitter(province_crop_sf, amount = 0.32)  # Adjust amount as needed

# Define the provinces you want to apply smaller jitter to
specific_provinces <- c("Kapisa", 'Kabul',"Parwan",  "Panjshir", "Kunar", "Samangan", "Nangarhar", "Badakhshan")  # Adjust these to your specific provinces
specific_provinces_data <- subset(province_crop_sf, Province %in% specific_provinces) # Subset the data for these provinces
specific_provinces_data_jittered <- st_jitter(specific_provinces_data, amount = 0.19)  # apply smaller jitter

# Combine the data back together
province_crop_sf_jittered <- rbind(
  subset(province_crop_sf_jittered, !Province %in% specific_provinces),
  specific_provinces_data_jittered
)



# # Create a scaling factor based on the crop type
# province_crop_sf_jittered$size_factor <- with(province_crop_sf_jittered, 
#                                               ifelse(Crop == "Crop_Type_1", Production_MT / 1000, 
#                                                      ifelse(Crop == "Crop_Type_2", Production_MT / 5000, 
#                                                             Production_MT / 10000))
# )
# 
# province_almond <- subset(province_crop_sf_jittered, Crop == "Almond")
# province_apple <- subset(province_crop_sf_jittered, Crop == "Apple")
# province_apricot <- subset(province_crop_sf_jittered, Crop == "Apricot")
# province_cherry <- subset(province_crop_sf_jittered, Crop == "Cherry")
# province_grapes <- subset(province_crop_sf_jittered, Crop == "Grape")
# province_peach <- subset(province_crop_sf_jittered, Crop == "Peach")
# #province_pinenut <- subset(province_crop_sf_jittered, Crop == "Pinenut")
# #province_pistachio <- subset(province_crop_sf_jittered, Crop == "Pistachio")
# province_plum <- subset(province_crop_sf_jittered, Crop == "Plum")
# province_pomegranate <- subset(province_crop_sf_jittered, Crop == "Pomegranate")
# province_walnut <- subset(province_crop_sf_jittered, Crop == "Walnut")

#-------------------------------------------------------------------------------------
# Plotting
#-------------------------------------------------------------------------------------
# get outer country outer boundary 
#afghanistan_outer <- sf::st_union(sf_afghanistan)

library(tmap)
crop_distr_tmap <- tm_shape(sf_afghanistan) +
  tm_borders(lwd = 1, lty = 'solid', alpha = 0.7,  col = "black") +  # Province borders

  # tm_shape(afghanistan_outer) +
  # tm_borders(col='black', lwd = 1.5, lty = "solid", alpha = 0.7) +
  
  #Plot crop points with jitter (small offset per crop type)
  tm_shape(province_crop_sf_jittered) +
  tm_symbols(col = "Crop", title.col = 'Major Crops\nDistribution',  palette = crop_colors,  shape = 16,
             border.col = "black", alpha = 1, size = 0.25) +
  
  
  # Plot crop points with jitter (small offset per crop type)
  # tm_shape(province_almond) +
  # tm_symbols(size = 'size_factor', 
  #            col = "Crop", palette = crop_colors,  shape = 21, 
  #            border.col = "black", alpha = 1,
  #            scale = 0.7) +
  # 
  # # Plot crop points with jitter (small offset per crop type)
  # tm_shape(province_apple) +
  # tm_symbols(size = 'size_factor', 
  #            col = "Crop", palette = crop_colors,  shape = 21, 
  #            border.col = "black", alpha = 1,
  #            scale = 0.7) +
  # 
  # # Plot crop points with jitter (small offset per crop type)
  # tm_shape(province_apricot) +
  # tm_symbols(size = 'size_factor', 
  #            col = "Crop", palette = crop_colors,  shape = 21, 
  #            border.col = "black", alpha = 1,
  #            scale = 0.7) +
  # 
  # # Plot crop points with jitter (small offset per crop type)
  # tm_shape(province_cherry) +
  # tm_symbols(size = 'size_factor', 
  #            col = "Crop", palette = crop_colors,  shape = 21, 
  #            border.col = "black", alpha = 1,
  #            scale = 0.7) +
  # 
  # # Plot crop points with jitter (small offset per crop type)
  # tm_shape(province_grapes) +
  # tm_symbols(size = 'size_factor', 
  #            col = "Crop", palette = crop_colors,  shape = 21, 
  #            border.col = "black", alpha = 1,
  #            scale = 0.7) +
  # 
  # # Plot crop points with jitter (small offset per crop type)
  # tm_shape(province_peach) +
  # tm_symbols(size = 'size_factor', 
  #            col = "Crop", palette = crop_colors,  shape = 21, 
  #            border.col = "black", alpha = 1,
  #            scale = 0.7) +
  # 
  # # Plot crop points with jitter (small offset per crop type)
  # tm_shape(province_plum) +
  # tm_symbols(size = 'size_factor', 
  #            col = "Crop", palette = crop_colors,  shape = 21, 
  #            border.col = "black", alpha = 1,
  #            scale = 0.7) +
  # 
  # # Plot crop points with jitter (small offset per crop type)
  # tm_shape(province_pomegranate) +
  # tm_symbols(size = 'size_factor', 
  #            col = "Crop", palette = crop_colors,  shape = 21, 
  #            border.col = "black", alpha = 1,
  #            scale = 0.7) +
  # 
  # Plot crop points with jitter (small offset per crop type)
  # tm_shape(province_walnut) +
  # tm_symbols(size = 'size_factor', 
  #            col = "Crop", palette = crop_colors,  shape = 21, 
  #            border.col = "black", alpha = 1,
  #            scale = 0.7) +
  
  
  # Province labels
  tm_shape(sf_afghanistan) + # Add a shape layer with point symbols
  tm_text('HASC_1', size=0.40, ymod= 0, xmod = 0, auto.placement = FALSE) + # # Add stations abbreviations from each column
  
  tm_legend(legend.outside=F) +
  tm_scale_bar(position = c(0.41, 0.029),bg.color = 'transparent', text.color = 1.7, color.dark = "grey20", lwd = 2, text.size = 0.8) +  # Add a scale bar on the left side
  tm_compass(position = c(0.04, 0.72), text.size = 0.9) +
  tm_graticules(lines = TRUE, labels.size = 0.8, labels.col = "black", labels.inside.frame = FALSE,
                alpha=0.3, n.y=5, n.x = 5, labels.margin.y=0.1,) +
  
  # Title & layout
  tm_layout(
            legend.show = TRUE,
            legend.title.size = 1.1,
            legend.text.size = 0.8,
            legend.position = c(1.02, 0.15), # old second value was 0.28
            legend.width = 1.2,
            legend.height = 1.5,
            bg.color = "white",
            outer.bg.color = "white",
            frame = FALSE)


tmap_save(crop_distr_tmap, filename = 'plot/crop_distribution/250430_crop_distr_tmap.png', height = 10, width = 19, units = 'cm')  

#------------------------------------------------------------------------------------#

#------------------------------------------------------------------------------------#
# Plotting crops relative to its respective province for production potential--------#
#------------------------------------------------------------------------------------#

# I do it based on size factor
# 1. Crop count for each province
province_crop_count <- province_crop_sf_jittered %>%
  group_by(Province) %>%
  summarise(crop_count = n()) %>%
  st_drop_geometry(province_crop_count) # this line drops the geometry column
              

# 2. join crop count with original data
province_crop_sf_jittered <- province_crop_sf_jittered %>%
  left_join(province_crop_count, by = "Province") 
  

library(forcats)  # ensure forcats is loaded

# Reorder province factor levels globally
province_crop_sf_jittered$Province <- fct_reorder(province_crop_sf_jittered$Province,
                                                  province_crop_sf_jittered$crop_count,
                                                  .desc = TRUE)


crop_contribution <- province_crop_sf_jittered %>%
  #filter(!is.na(Production_MT)) %>% # rows of missing value for pinenut and pistachio
  group_by(Crop) %>%
  #mutate(size_factor = scales::rescale(Production_MT, to = c(1, 10))) %>%  # rescale set circle size from 1 to 10 includ pist and pinenut
  mutate(size_factor = if (all(is.na(Production_MT))) NA_real_ # this excludes rescale for pistachio and pinenut b/c missing v
         else scales::rescale(Production_MT, to = c(1, 10)),
         percent_cont_to_total = Production_MT * 100/ total_sum) %>%
  ungroup()

# I also apply order to crops manually
crop_order <- c("Apricot", "Peach", "Walnut", "Cherry", "Apple", "Pinenut", "Pistachio",  "Plum",  "Almond", "Grape", "Fig", "Citrus", "Pomegranate")  # Replace with your preferred order

crop_contribution$Crop <- factor(crop_contribution$Crop, levels = crop_order)


# apply separate geompoints to pistachio and pinenut

# Split data into NA and non-NA for percent_cont_to_total
crop_with_data <- crop_contribution %>% filter(!is.na(percent_cont_to_total))
crop_with_data$Crop <- factor(crop_with_data$Crop, levels = crop_order)

crop_no_data <- crop_contribution %>% filter(is.na(percent_cont_to_total))
crop_no_data$Crop <- factor(crop_no_data$Crop, levels = crop_order)


# apply annotations:
# Example crop-level numbers (e.g., total production in MT)
crop_labels <- data.frame(
  Crop = crop_order,
  crop_value = c(12, 11, 10, 8, 8, 8, 7, 7, 6, 5, 3, 1, 1)  # the number belong to in how many provinces each crop grows
)

# Example province-level numbers (e.g., number of crop types or total plots)
province_labels <- data.frame(
  Province = c("Hirat", "Parwan",  "Baghlan", "Kandahar", "Balkh", "Ghazni", "Kabul",     
              "Kapisa", "Nangarhar","Hilmand", "Paktika",  "Paktya",  "Sari Pul", "Takhar", "Wardak",
              "Badakhshan", "Bamyan",   "Daykundi", "Faryab", "Kunar", "Panjshir", "Samangan",  "Badghis",
              "Jawzjan",  "Khost",  "Kunduz",   "Laghman",   "Logar", "Nuristan", "Uruzgan", "Zabul"),
  prov_value = c(7, 6, 5, 6, 4, 4, 4, 
                 4, 5, 3, 3, 3, 3, 3, 3, 
                 2, 2, 2, 3, 2, 2, 2, 1, 
                 1, 1, 1, 1, 1, 1, 1, 1))


# Combine plot with two patterns
crop_dist_2 <- ggplot(crop_contribution, aes(x = Crop, y = Province)) +
  geom_point(aes(size = percent_cont_to_total, fill = Crop),
             shape = 21, color = "black", alpha = 0.8) +  # shape=21 for circle with border
  
  # 2. Plot NA data with a different shape (e.g., triangle, shape=24) and small fixed size
  geom_point(data = crop_no_data, aes(x = Crop, y = Province, fill = Crop),
             shape = 21, color = "black", size = 4, alpha = 0.8) +
  
  
  # Annotations below x-axis (use lowest y value - 0.5 for bottom space)
  geom_text(data = crop_labels,
            aes(x = Crop, y = 0, label = crop_value),
            vjust = 1.2,
            inherit.aes = FALSE, size = 5) +
  
  # Annotations on right y-axis (use Inf for x, aligns right)
  geom_text(data = province_labels,
            aes(x = Inf, y = Province, label = prov_value),
            hjust = -0.2,  # slight offset from right edge
            inherit.aes = FALSE, size = 5) + # fontface = "bold"
  
  

  scale_size_continuous(range = c(3, 13),   # Adjust the circle size range
                        name = "Percent contribution\nto total production",
                        breaks = c(10, 30, 50, 70))  +


  scale_fill_manual(values = crop_colors) +  # If you have specific crop colors

  #facet_grid(~ Crop) +  # Show each crop in a separate panel

  scale_y_discrete(limits = rev) + # reverse the order
  scale_x_discrete(position = 'top') + # takes x axis text to top
  
  coord_cartesian(clip = "off") +  # allow annotating text outside plot area
  

  theme_bw() +
  labs(x = NULL, fill = 'Crops provincial\ndistribution') +
  theme(strip.text = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 18),
       # axis.text.x = element_blank(),
       axis.text.x.top = element_text(size = 14, face = 'bold'),
       axis.text.x.bottom = element_blank(),
       axis.title.x.bottom = element_blank(),
        axis.text = element_text(size = 14),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 14),
       plot.margin = margin(10, 50, 30, 10)) +  # bottom, right, top, left) +

guides(fill = guide_legend(override.aes = list(size = 5)))  # Increase the size of the legend symbols

ggsave('plot/crop_distribution/250430_crop_dist_2.png', width = 17, height = 10, dpi = 300)



























# Create a list of provinces for each crop
province_groups <- province_list %>%
  group_by(Crop) %>%
  summarise(provinces = list(unique(Province)))

# Now, create a matrix of shared provinces
crop_matrix <- sapply(province_groups$provinces, function(x) {
  sapply(province_groups$provinces, function(y) {
    # Calculate the proportion of shared provinces
    length(intersect(x, y)) / length(union(x, y))  # Overlap divided by union
  })
})


# Threshold for overlap
threshold <- 0.20

# Create a list of groups
crop_groups <- list()

# Loop through matrix to find overlapping crops above threshold
for (i in 1:nrow(crop_matrix)) {
  for (j in i:ncol(crop_matrix)) {
    if (crop_matrix[i, j] >= threshold) {
      # Record the pair (or group of crops) that meet the threshold
      group_name <- paste(province_groups$Crop[i], province_groups$Crop[j], sep = ", ")
      crop_groups[[group_name]] <- c(province_groups$Crop[i], province_groups$Crop[j])
    }
  }
}

# Now `crop_groups` contains all crop pairs (or sets) with 80% or more shared provinces.
# View crop groups
crop_groups
















# Load necessary libraries
library(ggplot2)
library(dplyr)

# Assuming `province_list` is your data with 'Province' and 'Crop' columns
heatmap_data <- province_list %>%
  group_by(Province, Crop) %>%
  summarise(count = n()) %>%
  ungroup()

# Create heatmap with ggplot2
ggplot(heatmap_data, aes(x = Province, y = Crop, fill = count)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  theme_minimal() +
  labs(title = "Crop Distribution by Province", x = "Province", y = "Crop", fill = "Count")



wide_data <- heatmap_data %>%
  pivot_wider(names_from = Crop, values_from = count, values_fill = 0)


#Extract province names
provinces <- wide_data$Province

# Convert to matrix for clustering (remove Province column)
data_matrix <- as.matrix(wide_data[, -1])

# Compute distance matrix (using Euclidean distance)
dist_matrix <- dist(data_matrix, method = "euclidean")

# Perform hierarchical clustering
hc <- hclust(dist_matrix, method = "ward.D2")

# Get the order of provinces from clustering
province_order <- provinces[hc$order]

# Reorder the Province factor in the original heatmap_data
heatmap_data$Province <- factor(heatmap_data$Province, levels = province_order)

# Create the heatmap with reordered provinces
ggplot(heatmap_data, aes(x = Province, y = Crop, fill = count)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  theme_minimal() +
  labs(title = "Crop Distribution by Province", x = "Province", y = "Crop", fill = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels for readability


# Cluster crops
crop_matrix <- t(data_matrix) # Transpose the matrix (rows are crops, columns are provinces)
dist_matrix_crops <- dist(crop_matrix, method = "euclidean")
hc_crops <- hclust(dist_matrix_crops, method = "ward.D2")
crop_order <- rownames(crop_matrix)[hc_crops$order]

# Reorder the Crop factor
heatmap_data$Crop <- factor(heatmap_data$Crop, levels = crop_order)

# Plot the heatmap with both provinces and crops reordered
ggplot(heatmap_data, aes(x = Province, y = Crop, fill = count)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  theme_minimal() +
  labs(title = "Crop Distribution by Province", x = "Province", y = "Crop", fill = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


crop_totals <- table(province_long$Crop)

crop_province_counts <- province_long %>%
  group_by(Crop, Province) %>%
  summarise(Count = n(), .groups = "drop")

# Step 3: Calculate percentages for each crop
results <- crop_province_counts %>%
  group_by(Crop) %>%
  summarise(
    Total = sum(Count),
    MaxProvinceCount = max(Count),
    NumProvinces = n_distinct(Province)
  ) %>%
  mutate(
    SameProvincePct = (MaxProvinceCount / Total) * 100,
    DifferentProvincesPct = 100 - SameProvincePct
  ) %>%
  select(Crop, SameProvincePct, DifferentProvincesPct)

# Step 4: Print results
for (i in 1:nrow(results)) {
  crop <- results$Crop[i]
  same_pct <- results$SameProvincePct[i]
  diff_pct <- results$DifferentProvincesPct[i]
  cat(sprintf("%s:\n", crop))
  cat(sprintf("  Same Province: %.2f%%\n", same_pct))
  cat(sprintf("  Different Provinces: %.2f%%\n", diff_pct))
}


library(tidyr)
library(dplyr)
library(tibble)
# If provinces are stored as lists/vectors in the column:
province_long <- province_list


# Step 1: Summarize the data to ensure no duplicates for crops
province_long <- province_long %>%
  group_by(Crop, Province) %>%
  summarise(Presence = 1) %>%  # Assuming 1 means presence
  ungroup()

# Step 2: Pivot the data to create the matrix, with Provinces as columns
binary_matrix <- province_long %>%
  pivot_wider(names_from = Province, values_from = Presence, values_fill = list(Presence = 0)) %>%
  column_to_rownames("Crop") %>%  # Set Crop as row names
  as.matrix()  # Convert to matrix

# Step 3: Create the heatmap
heatmap(binary_matrix, main = "Crop Distribution Heatmap", col = heat.colors(256), scale = "none")

