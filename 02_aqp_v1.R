#Green areas analysis
#Date: December, 2025
#Objective: Calculate the share of green areas in Arequipa metropolitan by district

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
district_path <- "./00_raw_data/geo/per/DISTRITO.gpkg"
#Arequipa province polygon
aqp_v1 <- "./02_intermediate_data/green/per/arequipa_v1.gpkg"
#Green areas in Arequipa
green_v1 <- "./01_clean_data/green/per/green_areas_arequipa.gpkg"

outdir <- "./02_intermediate_data/green/per/"

# ---------------------------------------------------------------------
# 2. Load data
# ---------------------------------------------------------------------
district_all <- st_read(district_path, quiet = TRUE)
aqp_prov <- st_read(aqp_v1, quiet = TRUE)
green_lp <- st_read(green_v1, quiet = TRUE)

summary(district_all)
summary(aqp_prov)
summary(green_lp)

#Transform to suitable projection for Lima (UTM Zone 18S) to ensure meters for area calculation
proj_crs <- 32718  # WGS 84 / UTM zone 18S
district_all <- st_transform(district_all, proj_crs)
aqp_prov <- st_transform(aqp_prov, proj_crs)
green_lp <- st_transform(green_lp, proj_crs)

district_all |> map_dbl(~length(unique(.)))

# ---------------------------------------------------------------------
# 3. Keep only Lima province districts
# ---------------------------------------------------------------------
# Intersect with our polygon for Lima
district_aqp <- district_all |> 
  st_transform(st_crs(aqp_prov)) |>
  st_make_valid() |>
  st_intersection(aqp_prov)

#Arequipa province has 29 districts and we consider 13 for Arequipa metropolitan
aqp_metropolitan <- c(
  "ALTO SELVA ALEGRE", "AREQUIPA", "CAYMA", "CERRO COLORADO", "JACOBO HUNTER", "JOSE LUIS BUSTAMANTE Y RIVERO",
  "MARIANO MELGAR", "MIRAFLORES", "PAUCARPATA", "SACHACA", "SOCABAYA", "TIABAYA", "YANAHUARA"
)

# Filter Arequipa metropolitan
district_aqp <- district_all |>
  filter(NOMBPROV %in% c("AREQUIPA")) |>
  filter(NOMBDIST %in% aqp_metropolitan)

table(district_aqp$NOMBPROV)

cat("Number of districts in Arequipa Metroolitan:", nrow(district_aqp), "\n")

# ---------------------------------------------------------------------
# 4. Compute total area of each LSOA
# ---------------------------------------------------------------------
district_aqp$area_total_m2 <- as.numeric(st_area(district_aqp))

#Calculate total area in Arequipa to check figures are correct
area_gl <- district_aqp |>
  summarise(
    across(c(
      area_total_m2),
      ~sum(., na.rm = TRUE)
    )) |>
  as.data.frame() |>
  select(-geom)

# ---------------------------------------------------------------------
# 5. Intersect green areas with districts
# ---------------------------------------------------------------------
cat("Computing intersection of districts with green areas in Arequipa metropolitan \n")

district_green <- st_intersection(st_make_valid(district_aqp), st_make_valid(green_lp))
#Compute green areas by LSOA
district_green$area_green_m2 <- as.numeric(st_area(district_green))

green_gl <- district_green |>
  summarise(
    across(c(
      area_green_m2),
      ~sum(., na.rm = TRUE)
    )) |>
  as.data.frame() |>
  select(-geom)

district_green |> map_dbl(~length(unique(.)))
district_green |> map_dbl(~sum(is.na(.)))

table(district_green$CCDI)
table(district_green$NOMBDIST)

# Aggregate green area per district
green_summary <- district_green |>
  st_drop_geometry() |>
  group_by(NOMBDIST) |>
  summarise(area_green_m2 = sum(area_green_m2, na.rm = TRUE))

# ---------------------------------------------------------------------
# 6. Join back to province polygons and compute share
# ---------------------------------------------------------------------
district_aqp <- district_aqp |>
  left_join(green_summary, by = "NOMBDIST") |>
  mutate(
    area_green_m2 = replace_na(area_green_m2, 0),
    share_green = 100 * area_green_m2 / area_total_m2
  )

summary(district_aqp$share_green)

# ---------------------------------------------------------------------
# 7. Save output
# ---------------------------------------------------------------------

outpath <- paste0(outdir, "aqp_district_green_share.gpkg")

st_write(district_aqp, outpath,
         delete_dsn = TRUE)

st_write(
  district_aqp,
  "./02_intermediate_data/green/per/aqp_district_green_share.gpkg",
  delete_dsn = TRUE
)
