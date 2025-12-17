#Green areas analysis - v1d - final version for Seville
#Date: December, 2025
#Objective: Calculate green areas in Seville using CNIG and OMS

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
#Source:
#https://centrodedescargas.cnig.es/CentroDescargas/informacion-geografica-referencia
#https://centrodedescargas.cnig.es/CentroDescargas/limites-municipales-provinciales-autonomicos

# ============================================================
# 1. Load municipal boundaries (CNIG - Spain)
# ============================================================
esp_path <- "./00_raw_data/geo/esp/lineas_limite/SHP_ETRS89/recintos_municipales_inspire_peninbal_etrs89/recintos_municipales_inspire_peninbal_etrs89.shp"
esp_muni <- st_read(esp_path)

esp_muni |> map_dbl(~length(unique(.)))
#Spain has 8132 municipalities

# 2. Filter Sevilla municipality (CODNUT3 = ES618)
sevilla_city <- esp_muni %>%
  filter(NAMEUNIT == "Sevilla") %>%
  st_union() %>%
  st_make_valid() %>%
  st_as_sf() %>%
  st_transform(4326)

#plot(sevilla_city)

# 3. Save boundary
st_write(sevilla_city,
         "./02_intermediate_data/green/esp/sevilla_city_v1.gpkg",
         delete_dsn = TRUE)

# 4. CORRECT bounding box name
bbox_sevilla_city <- getbb("Sevilla, España")

# 5. Fetch function
fetch_green_bbox <- function(key, values, bbox, boundary) {
  
  q <- opq(bbox = bbox) |>
    add_osm_feature(key = key, value = values)
  
  dat <- osmdata_sf(q)
  
  # ✅ SAFE binding of polygons + multipolygons
  polys <- dplyr::bind_rows(
    dat$osm_polygons,
    dat$osm_multipolygons
  )
  
  if (!is.null(polys) && nrow(polys) > 0) {
    
    polys <- polys |>
      st_transform(st_crs(boundary)) |>
      st_make_valid()
    
    polys$key   <- key
    polys$value <- polys[[key]]
    
    polys_clipped <- st_intersection(polys, boundary)
    return(polys_clipped)
  }
  
  return(NULL)
}

# 6. Fetch green features
green_list <- list(
  fetch_green_bbox("leisure", c("park","garden","recreation_ground","pitch","nature_reserve", "golf_course"),
                   bbox = bbox_sevilla_city, boundary = sevilla_city),
  fetch_green_bbox("landuse", c("forest","grass","meadow", "allotments"),
                   bbox = bbox_sevilla_city, boundary = sevilla_city),
  fetch_green_bbox("natural", c("wood","heath","scrub"),
                   bbox = bbox_sevilla_city, boundary = sevilla_city),
  fetch_green_bbox("amenity", c("community_garden"),
                   bbox = bbox_sevilla_city, boundary = sevilla_city)
)

green_list_clean <- lapply(green_list, function(x) {
  if (!is.null(x)) select(x, osm_id, name, key, value, geometry)
})

green_areas_sevilla <- do.call(rbind, green_list_clean)

green_areas_sevilla

#=============================================================
#Stats
class(green_areas_sevilla)
table(green_areas_sevilla$key)
table(green_areas_sevilla$value)

# ============================================================
# 4. Calculate total green coverage
# ============================================================
sev_city_proj <- st_transform(sevilla_city, 25830)
green_proj    <- st_transform(green_areas_sevilla, 25830)

sevilla_area <- st_area(sev_city_proj)
green_area   <- st_area(green_proj)

green_share <- as.numeric(sum(green_area) / sevilla_area * 100)

cat("Total green area share of Sevilla (%):", round(green_share, 2), "%\n")

#Total green area share of Sevilla (%): 17.98  %

# ---------------------------------------------------------------------
# 4.1 Compute individual areas
sevilla_area_m2 <- as.numeric(sevilla_area)
green_osm_m2    <- sum(as.numeric(green_area))

cat("Sevilla area (km²):", round(set_units(sevilla_area_m2 / 1e6, km^2), 1), "\n") #142.1
cat("OSM green area (km²):", round(set_units(green_osm_m2 / 1e6, km^2), 1), "\n") #25.6

# ============================================================
# 5. Static map
# ============================================================
paa <- ggplot() +
  geom_sf(data = sevilla_city, fill = "white", color = "black", size = 0.6) +
  geom_sf(data = green_areas_sevilla, fill = "darkgreen", color = NA, alpha = 0.5) +
  theme_minimal() +
  labs(
    title = "Green Areas within Sevilla City",
    subtitle = "White = CNIG Sevilla City boundary | Green = OSM green features",
    caption = "Source: CNIG 2024 + OSM"
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

#----------------------------------------
#New map style for article
# Reproject for consistency (if not already)
sev_plot   <- st_transform(sevilla_city, 25830)
green_plot <- st_transform(green_areas_sevilla, 25830)

# Build map
pab <- ggplot() +
  # --- Green areas (OSM)
  geom_sf(
    data  = green_plot,
    fill  = "#1F8A5B",   # same dark green as Arequipa urban green
    color = NA,
    alpha = 0.95
  ) +
  # --- Sevilla boundary
  geom_sf(
    data  = sev_plot,
    fill  = NA,
    color = "white",
    linewidth = 0.6
  ) +
  # --- Labels
  labs(
    title    = "Urban green space in Sevilla",
    subtitle = "Public parks and recreational green areas within the city boundary",
    caption  = "Source: CNIG 2024 + OSM"
  ) +
  # --- Dark theme (identical to Arequipa)
  theme_minimal(base_size = 12) +
  theme(
    panel.background = element_rect(fill = "black", color = NA),
    plot.background  = element_rect(fill = "black", color = NA),
    panel.grid       = element_blank(),
    
    axis.text        = element_blank(),
    axis.title       = element_blank(),
    
    plot.title       = element_text(color = "white", face = "bold"),
    plot.subtitle    = element_text(color = "grey80", size = 10),
    plot.caption     = element_text(color = "grey70", size = 8),
    
    legend.position  = "none"
  )

# Save static map
ggsave(
  filename = "./02_intermediate_data/green/map/static/esp/gsevilla_v1.png",
  plot = paa,
  width = 8, height = 6, dpi = 300
)

ggsave(
  filename = "./02_intermediate_data/green/map/static/esp/gsevilla_v2.png",
  plot = pab,
  width = 8, height = 6, dpi = 300
)

# ============================================================
# 6. Save cleaned vector
# ============================================================
st_write(green_areas_sevilla, "./01_clean_data/green/esp/green_areas_sevilla.gpkg", delete_dsn = TRUE)
