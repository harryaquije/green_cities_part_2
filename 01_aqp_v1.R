#Green areas analysis - v2
#Date: December, 2025
#Objective: Calculate green areas in Arequipa using INEI and OMS

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Parameters
library(osmdata)
library(sf)
library(dplyr)
library(tidyverse)
library(units)
library(ggplot2)
library(ggspatial)

rm(list = ls())
setwd(here::here())
  
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# -----------------------------
# 1. Load Peru boundary from INEI
# -----------------------------
aqp_lad <- st_read("./00_raw_data/geo/per/DISTRITO.gpkg")
  
aqp_lad |> map_dbl(~length(unique(.))) #1891 districts
aqp_lad |> map_dbl(~sum(is.na(.)))
table(aqp_lad$NOMBPROV)
#table(aqp_prov$NOMBDIST)
#aqp_prov |> map_dbl(~length(unique(.)))
#Arequipa province has 29 districts and we consider 13 for Arequipa metropolitan
aqp_metropolitan <- c(
  "ALTO SELVA ALEGRE", "AREQUIPA", "CAYMA", "CERRO COLORADO", "JACOBO HUNTER", "JOSE LUIS BUSTAMANTE Y RIVERO",
  "MARIANO MELGAR", "MIRAFLORES", "PAUCARPATA", "SACHACA", "SOCABAYA", "TIABAYA", "YANAHUARA"
  )
  
# 2. Filter Arequipa province
aqp_prov <- aqp_lad |>
  filter(NOMBPROV %in% c("AREQUIPA")) |>
  filter(NOMBDIST %in% aqp_metropolitan) |>
  st_union() %>%
  st_make_valid() %>%
  st_as_sf() %>%
  st_transform(4326)
  
# 3. Save boundary
st_write(
  st_as_sf(aqp_prov),
  "./02_intermediate_data/green/per/arequipa_v1.gpkg",
  delete_dsn = TRUE
)
  
#opq(bbox = "Provincia de Arequipa, Peru")
  
# 4. CORRECT bounding box name
bbox_aqp_prov <- getbb("Provincia de Arequipa, Peru")
  
# 5. Fetch function
fetch_green_bbox <- function(key, values, bbox, boundary) {
    
  q <- opq(bbox = bbox) |>
    add_osm_feature(key = key, value = values)
    
  dat <- osmdata_sf(q)
  polys <- dat$osm_polygons
    
  if (!is.null(polys) && nrow(polys) > 0) {
      
    polys <- polys |>
      st_transform(st_crs(boundary)) |>
      st_make_valid()
      
    polys$key <- key
    polys$value <- polys[[key]]
      
    polys_clipped <- st_intersection(polys, boundary)
    return(polys_clipped)
  }
    
  return(NULL)
}
  
# 6. Fetch green features
green_list <- list(
  fetch_green_bbox(
    "leisure",
    c("park","garden","recreation_ground","pitch","nature_reserve","golf_course"),
    bbox = bbox_aqp_prov, boundary = aqp_prov
  ),
  fetch_green_bbox(
    "landuse",
    c("forest","grass","meadow",
      "farmland","orchard","vineyard","plant_nursery","agricultural"),
    bbox = bbox_aqp_prov, boundary = aqp_prov
  ),
  fetch_green_bbox(
    "natural",
    c("wood","heath","scrub"),
    bbox = bbox_aqp_prov, boundary = aqp_prov
  ),
  fetch_green_bbox(
    "amenity",
    c("community_garden"),
    bbox = bbox_aqp_prov, boundary = aqp_prov
  )
)
  

green_list_clean <- lapply(green_list, function(x) {
  if (!is.null(x)) select(x, osm_id, name, key, value, geometry)
})
  
green_areas_aqp <- do.call(rbind, green_list_clean)
  
green_areas_aqp
  
#=============================================================
#Stats
nrow(green_areas_aqp)
table(green_areas_aqp$key)
table(green_areas_aqp$value)
  
# ============================================================
# 4. Calculate total green coverage
# ============================================================
#Reproject to equal-area CRS for Peru (UTM 19S)
aqp_proj   <- st_transform(aqp_prov, 32719)
green_proj <- st_transform(green_areas_aqp, 32719)
  
aqp_area <- st_area(aqp_proj)
green_area   <- st_area(green_proj)
  
green_share <- as.numeric(sum(green_area) / aqp_area * 100)
  
cat("Total green area share of Arequipa (%):", round(green_share, 2), "%\n")
  
#Total green area share of Arequipa (%): 10.16 %
  
# ---------------------------------------------------------------------
# 4.1 Compute individual areas
aqp_area_m2 <- as.numeric(aqp_area)
green_osm_m2    <- sum(as.numeric(green_area))
  
cat("Arequipa area (km²):", round(set_units(aqp_area_m2 / 1e6, km^2), 1), "\n") #683.3
cat("OSM green area (km²):", round(set_units(green_osm_m2 / 1e6, km^2), 1), "\n") #69.4

#===============================================================================
#Distinction between agriculture and recreational green
#===============================================================================

#A) Agricultural green
agri_values <- c(
  "farmland",
  "orchard",
  "vineyard",
  "plant_nursery",
  "farmyard",
  "agricultural"
)

#B) Urban / non-agricultural green
urban_green_values <- c(
  "park",
  "garden",
  "recreation_ground",
  "pitch",
  "nature_reserve",
  "golf_course",
  "grass",
  "meadow",
  "wood",
  "scrub",
  "heath"
)

green_proj_b <- st_transform(green_areas_aqp, 32719)
green_proj_b$area_m2 <- as.numeric(st_area(green_proj_b))

#Tag polygons as agricultural vs other
green_proj_b$type <- dplyr::case_when(
  green_proj_b$value %in% agri_values ~ "agriculture",
  TRUE                              ~ "other_green"
)

#Aggregate areas
area_summary <- green_proj_b |>
  st_drop_geometry() |>
  group_by(type) |>
  summarise(area_m2 = sum(area_m2), .groups = "drop")

#Convert to km2 and shares
area_summary <- area_summary |>
  mutate(
    area_km2 = area_m2 / 1e6,
    share_of_green = area_m2 / sum(area_m2) * 100,
    share_of_city  = area_m2 / as.numeric(aqp_area) * 100
  )

area_summary

# ============================================================
# 5. Static map
# ============================================================
paa <- ggplot() +
  geom_sf(data = aqp_prov, fill = "white", color = "black", size = 0.6) +
  geom_sf(data = green_areas_aqp, fill = "darkgreen", color = NA, alpha = 0.5) +
  theme_minimal() +
  labs(
    title = "Green Areas within Arequipa Metropolitan",
    subtitle = "White = INEI Aqrequipa Metropolitan boundary | Green = OSM green features",
    caption = "Source: INEI 2023 + OSM"
  ) +
  theme(
    plot.title = element_text(color = "white"),
    plot.subtitle = element_text(color = "white"),
    plot.caption = element_text(color = "white"),
    axis.text = element_text(color = "white"),
    panel.grid = element_line(color = "grey30"),
    plot.background = element_rect(fill = "black", color = NA),
    panel.background = element_rect(fill = "black", color = NA)
  )

# ============================================================
# Killer map: Green but not green for people
# ============================================================

# ============================================================
# Fetch Chili River (OSM)
# ============================================================

# Reproject to plotting CRS (keep UTM for accurate shapes)
aqp_plot   <- st_transform(aqp_prov, 32719)
green_plot <- st_transform(green_areas_aqp, 32719)

# Tag agriculture vs urban green
green_plot$type <- dplyr::case_when(
  green_plot$value %in% agri_values ~ "Agricultural green",
  TRUE                              ~ "Urban green (parks & recreation)"
)

# Build map
pab <- ggplot() +
  # --- Agricultural green (background)
  geom_sf(
    data  = dplyr::filter(green_plot, type == "Agricultural green"),
    fill  = "#9BD770",   # soft light green
    color = NA,
    alpha = 0.85
  ) +
  # --- Urban green spaces (foreground)
  geom_sf(
    data  = dplyr::filter(green_plot, type == "Urban green (parks & recreation)"),
    fill  = "#1F8A5B",   # dark green
    color = NA,
    alpha = 0.95
  ) +
  # --- Metropolitan boundary
  geom_sf(
    data  = aqp_plot,
    fill  = NA,
    color = "white",
    linewidth = 0.6
  ) +
  # --- Labels
  labs(
    title    = "Green space in Arequipa: agriculture vs public green areas",
    subtitle = "Most green land is agricultural; parks and recreational spaces are limited",
    caption  = "Source: INEI 2023 + OSM"
  ) +
  # --- Dark theme
  theme_minimal(base_size = 12) +
  theme(
    panel.background   = element_rect(fill = "black", color = NA),
    plot.background    = element_rect(fill = "black", color = NA),
    panel.grid         = element_blank(),
    axis.text          = element_blank(),
    axis.title         = element_blank(),
    plot.title         = element_text(color = "white", face = "bold"),
    plot.subtitle      = element_text(color = "grey80", size = 10),
    plot.caption       = element_text(color = "grey70", size = 8),
    legend.position    = "none"
  )

#=================
# Save static map
ggsave(
  filename = "./02_intermediate_data/green/map/static/per/garequipa_v1.png",
  plot = paa,
  width = 8, height = 6, dpi = 300
)

ggsave(
  filename = "./02_intermediate_data/green/map/static/per/garequipa_v2.png",
  plot = pab,
  width = 8, height = 6, dpi = 300
)

# ============================================================
# 6. Save cleaned vector
# ============================================================
st_write(green_areas_aqp, "./01_clean_data/green/per/green_areas_arequipa.gpkg", delete_dsn = TRUE)
