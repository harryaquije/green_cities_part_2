#Green areas analysis
#Date: December, 2025
#Objective: Calculate the share of green areas in Sevilla municipality

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Parameters
library(sf)
library(dplyr)
library (tidyverse)
library(units)
library(rmapshaper)
library(leaflet)

rm(list = ls())
setwd(here::here())

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------
# 1. Paths
# ---------------------------------------------------------------------
#Peru districts
mun_path <- "./00_raw_data/geo/esp/lineas_limite/SHP_ETRS89/recintos_municipales_inspire_peninbal_etrs89/recintos_municipales_inspire_peninbal_etrs89.shp"
#Sevilla city polygon
aqp_v1 <- "./02_intermediate_data/green/esp/sevilla_v1.gpkg"
#Green areas in Sevilla
green_v1 <- "./01_clean_data/green/esp/green_areas_sevilla.gpkg"

outdir <- "./02_intermediate_data/green/esp/"

# ---------------------------------------------------------------------
# 2. Load data
# ---------------------------------------------------------------------
mun_all <- st_read(mun_path, quiet = TRUE)
sevilla_prov <- st_read(aqp_v1, quiet = TRUE)
green_lp <- st_read(green_v1, quiet = TRUE)

summary(mun_all)
summary(sevilla_prov)
summary(green_lp)

#Transform to suitable projection for Sevilla to ensure meters for area calculation
proj_crs <- 25830 
mun_all <- st_transform(mun_all, proj_crs)
sevilla_prov <- st_transform(sevilla_prov, proj_crs)
green_lp <- st_transform(green_lp, proj_crs)

mun_all |> map_dbl(~length(unique(.)))

# ---------------------------------------------------------------------
# 3. Keep only Lima province districts
# ---------------------------------------------------------------------
# Intersect with our polygon for Lima
mun_all <- mun_all |> 
  st_transform(st_crs(sevilla_prov)) |>
  st_make_valid() |>
  st_intersection(sevilla_prov)

# Filter Sevilla city
mun_sevilla <- mun_all |>
  filter(NAMEUNIT == "Sevilla")

table(mun_sevilla$NAMEUNIT)

cat("Number of muncipalities in Sevilla:", nrow(mun_sevilla), "\n")

# ---------------------------------------------------------------------
# 4. Compute total area of each LSOA
# ---------------------------------------------------------------------
mun_sevilla$area_total_m2 <- as.numeric(st_area(mun_sevilla))

#Calculate total area in Sevilla to check figures are correct
area_gl <- mun_sevilla |>
  summarise(
    across(c(
      area_total_m2),
      ~sum(., na.rm = TRUE)
    )) |>
  as.data.frame() |>
  select(-geometry)

# ---------------------------------------------------------------------
# 5. Intersect green areas with districts
# ---------------------------------------------------------------------
cat("Computing intersection of municipalities with green areas in Sevilla \n")

district_green <- st_intersection(st_make_valid(mun_sevilla), st_make_valid(green_lp))
#Compute green areas by LSOA
district_green$area_green_m2 <- as.numeric(st_area(district_green))

green_gl <- district_green |>
  summarise(
    across(c(
      area_green_m2),
      ~sum(., na.rm = TRUE)
    )) |>
  as.data.frame() |>
  select(-geometry)

district_green |> map_dbl(~length(unique(.)))
district_green |> map_dbl(~sum(is.na(.)))

table(district_green$NAMEUNIT)

# Aggregate green area per district
green_summary <- district_green |>
  st_drop_geometry() |>
  group_by(NAMEUNIT) |>
  summarise(area_green_m2 = sum(area_green_m2, na.rm = TRUE))

# ---------------------------------------------------------------------
# 6. Join back to municipality polygons and compute share
# ---------------------------------------------------------------------
mun_sevilla <- mun_sevilla |>
  left_join(green_summary, by = "NAMEUNIT") |>
  mutate(
    area_green_m2 = replace_na(area_green_m2, 0),
    share_green = 100 * area_green_m2 / area_total_m2
  )

summary(mun_sevilla$share_green)

# ---------------------------------------------------------------------
# 7. Save output
# ---------------------------------------------------------------------

outpath <- paste0(outdir, "sevilla_mun_green_share.gpkg")

st_write(mun_sevilla, outpath,
         delete_dsn = TRUE)

st_write(
  mun_sevilla,
  "./02_intermediate_data/green/per/sevilla_mun_green_share.gpkg",
  delete_dsn = TRUE
)
