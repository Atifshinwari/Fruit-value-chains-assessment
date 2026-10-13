# ====================================================================================
#                               Afghanistan Elevation Map
# ====================================================================================

library(sf)
library(terra)
library(geodata)
library(ggplot2)
library(ggspatial)
library(dplyr)

# 1) Afghanistan boundaries (ADM1)
af_adm1 <- geodata::gadm(country = "AF", level = 1, path = tempdir())
af_sf   <- st_as_sf(af_adm1) |> st_make_valid()

# Use union ONLY for masking the raster
af_union <- st_union(af_sf)

# 2) Elevation 
elev <- geodata::elevation_30s(country = "AF", path = tempdir())

# 3) Crop + mask to Afghanistan
af_v <- terra::vect(af_union)
elev_af <- terra::mask(terra::crop(elev, af_v), af_v)

# 4) Hillshade
slope  <- terra::terrain(elev_af, v = "slope", unit = "radians")
aspect <- terra::terrain(elev_af, v = "aspect", unit = "radians")
shade  <- terra::shade(slope, aspect)

# 5) Raster -> data frame
elev_df <- as.data.frame(elev_af, xy = TRUE, na.rm = TRUE)
names(elev_df)[3] <- "elev"

shade_df <- as.data.frame(shade, xy = TRUE, na.rm = TRUE)
names(shade_df)[3] <- "shade"

# 6) CRS alignment (important)
crs_r <- terra::crs(elev_af)
af_union2 <- st_transform(af_union, crs_r)
af_sf2    <- st_transform(af_sf,    crs_r)


color_comb <- c("#e65b43","#f18b5a","#fbc17c","#fde6a4",
                "#f7f7f7",
                "#d1e5f0","#92c5de","#4393c3","#2166ac")

# Example: create abbreviations (edit names to match data exactly)
prov_abbr <- c(
  "Badakhshan" = "BDK", "Badghis" = "BDG", "Baghlan" = "BGL",
  "Balkh" = "BLH", "Bamyan" = "BMY", "Daykundi" = "DYK",
  "Farah" = "FRH", "Faryab" = "FYB", "Ghazni" = "GHZ",
  "Ghor" = "GHR", "Hilmand" = "HLM", "Hirat" = "HRT",
  "Jawzjan" = "JWJ", "Kabul" = "KBL", "Kandahar" = "KDR",
  "Kapisa" = "KPS", "Khost" = "KST", "Kunar" = "KNR",
  "Kunduz" = "KDZ", "Laghman" = "LGM", "Logar" = "LGR",
  "Nangarhar" = "NGR", "Nimroz" = "NMZ", "Nuristan" = "NST",
  "Paktya" = "PKT", "Paktika" = "PKA", "Panjshir" = "PNJ",
  "Parwan" = "PRW", "Samangan" = "SMG", "Sari Pul" = "SRP",
  "Takhar" = "TKR", "Uruzgan" = "URZ", "Wardak" = "WRD",
  "Zabul" = "ZBL"
)

af_sf2$abbr <- prov_abbr[af_sf2$NAME_1]   # change NAME_1 if needed

# get labels positions
af_labels <- st_point_on_surface(af_sf2)


# Reading provincial production data to show on plot
crops_provinces <- read.csv('data/crop_province_share.csv')

# i) Prep a “coverage only” crop layer (no production)
# library(dplyr)

crops_cov <- crops_provinces %>%
  distinct(Province, Crop)   # coverage only

# ii) Joining crop coverage to province sf
af_crops <- af_sf2 %>%
  left_join(crops_cov, by = c("NAME_1" = "Province"))

crop_colors <- c(
  "Almond" = "#8B4513",      # Dark Brown
  "Walnut" =  "#D2B48C",       # Light Brown (Tan)
  "Apple" = "#FF0000",        # Red
  "Apricot" = "#FF8C00",      # Soft Apricot Orange
  "Peach" = "lightpink",        # Pink-Orange
  "Plum" = "#800080",         # Purple
  "Pomegranate" = "firebrick",  # Dark Red
  "Grape" = "lightgreen",        # Dark Purple
  "Cherry" = "#DC143C"    ,    # Crimson
  "Pistachio" = "darkgreen", 
  "Pinenut" = "antiquewhite",
  "Citrus" = "#FFB300",       # Bright Orange
  "Fig" = "#4B0082"     # Indigo (dark fig skin tone)"
)

# Put this ONCE before plotting (so jitter is repeatable)
set.seed(46)
# Build jittered crop points safely (meters)
crop_pts <- af_crops %>%
  dplyr::filter(!is.na(Crop)) %>%
  sf::st_transform(3857) %>%        # project first (meters)
  sf::st_point_on_surface() %>%     # now safe
  sf::st_jitter(amount = 35000) %>% # 35 km
  sf::st_transform(sf::st_crs(af_sf2))


# 7) Plot
p <- ggplot() +
  # Hillshade underlay
  geom_raster(data = shade_df, aes(x = x, y = y, alpha = shade)) +
  
  # Elevation overlay
  geom_raster(data = elev_df, aes(x = x, y = y, fill = elev), alpha = 0.70) +
  
  # Country outline (slightly thicker)
  geom_sf(data = af_union2, fill = NA, linewidth = 0.45, color = "black",
          inherit.aes = FALSE) +
  
  # ADM1 (province) borders (thin grey)
  geom_sf(data = af_sf2, fill = NA, linewidth = 0.40, color = "white",
          inherit.aes = FALSE) +
  
  # Crop points for Jitter - start
geom_sf(
  data = crop_pts,
  aes(color = Crop),
  size = 2.3,
  alpha = 0.95,
  inherit.aes = FALSE
) +
  scale_color_manual(values = crop_colors, name = "Crop coverage",
                     guide = guide_legend(order = 2,
                     keyheight = unit(0.35, "cm"),   # controls spacing between crop items in legend
                     keywidth  = unit(0.45, "cm") # control key spacing
  )
) +
  # Crop points for jitter - ends


  
  # Elevation scale (FIRST fill scale)
  scale_fill_gradientn(
    colours = color_comb,
    name = "Elevation (m a.s.l.)",
    limits = c(245, 7775),
    breaks = c(245, 1000, 2000, 3000, 4000, 5000, 6000, 7000, 7775),
    
    oob = scales::squish,
    guide = guide_colorbar(order = 1)
  ) +
  
  scale_alpha(range = c(0.25, 0.9), guide = "none") +
  

  coord_sf(expand = FALSE) +
  
  geom_sf_text(
    data = af_labels,
    aes(label = abbr),
    size = 2.6,
    fontface = "bold",
    color = "black",
    check_overlap = TRUE
  ) +
  
  # North arrow (top-left)
  annotation_north_arrow(
    location = "tl", which_north = "true",
    style = north_arrow_fancy_orienteering,
    pad_x = unit(0.25, "cm"), pad_y = unit(0.25, "cm")
  ) +
  
  # Scale bar (bottom-left) WITH padding + background so it stays readable
  annotation_scale(
    location = "br",
    width_hint = 0.22,
    pad_x = unit(5.2, "cm"),
    pad_y = unit(0.45, "cm"),
    text_cex = 0.8,
    line_width = 0.7
  ) +
  
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "grey85", linewidth = 0.25),
    panel.grid.minor = element_blank(),
    legend.position = "right",
    legend.title = element_text(size = 11),
    legend.text  = element_text(size = 10)
  )

# ---- 8) Save ----
ggsave("Afghanistan_Elevation_ADM1_Hillshade_260219.png", p, width = 8, height = 6, dpi = 300)


