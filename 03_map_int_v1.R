#Green areas analysis
#Date: December, 2025
#Objective: Create interactive maps

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Parameters
library(sf)
library(dplyr)
library(leaflet)
library(leaflet.extras)
library(RColorBrewer)
library(htmlwidgets)
library(htmltools)
library(mapview)
#webshot::install_phantomjs()
library(webshot)

rm(list = ls())
setwd(here::here())

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Paths
#Green share Greater London
lsoa_green_path <- "./02_intermediate_data/green/gbr/london_lsoa_green_share.gpkg"

#Green share Lima
district_green_path <- "./02_intermediate_data/green/per/lima_district_green_share.gpkg"

#Green share Sevilla
sevilla_green_path <- "./02_intermediate_data/green/esp/sevilla_mun_green_share.gpkg"

#Green share Arequipa
aqp_green_path <- "./02_intermediate_data/green/per/aqp_district_green_share.gpkg"

#Load data
lsoa_london <- st_read(lsoa_green_path, quiet = TRUE)
district_lima <- st_read(district_green_path, quiet = TRUE)
mun_sevilla <- st_read(sevilla_green_path, quiet = TRUE)
district_aqp <- st_read(aqp_green_path, quiet = TRUE)

st_crs(lsoa_london)$epsg #EPSG 27700 (British National Grid) - correct for UK data
st_crs(district_lima)$epsg #EPSG 32718 (WGS 84 / UTM zone 18S) - correct for Peru
st_crs(mun_sevilla)$epsg #EPSG 25830
st_crs(district_aqp)$epsg #EPSG 32718 (WGS 84 / UTM zone 18S) - correct for Peru

#Fix the area variable for Lima
district_lima <- district_lima %>%
  rename(total_area_m2 = area_total_m2)

mun_sevilla <- mun_sevilla %>%
  rename(total_area_m2 = area_total_m2)

district_aqp <- district_aqp %>%
  rename(total_area_m2 = area_total_m2)

# ---------------------------------------------------------------------
# Function to create and optionally save an interactive map
# ---------------------------------------------------------------------
make_green_map <- function(
    data_sf,
    id_field,           
    share_field,        
    area_field = NULL,  
    title = "Green share",
    
    # 🔧 UPDATED: New default bins (better balance for London–Lima)
    bins = c(0, 5, 10, 20, 30, 50, 70, 100),
    
    palette = "Greens",
    basemaps = c("CartoDB.Positron", "Esri.WorldStreetMap", "Esri.WorldImagery", "CartoDB.DarkMatter"),
    map_height = 650,
    zoom_bounds = c(6, 15),
    save_html = FALSE,
    html_path = "map.html",
    selfcontained = TRUE,
    save_png = FALSE,
    png_path = "map.png",
    png_vwidth = 1400,
    png_vheight = 900,
    add_search = TRUE
) {
  # ---- Input checks ----
  if(!inherits(data_sf, "sf")) stop("data_sf must be an sf object.")
  if(!(id_field %in% names(data_sf))) stop("id_field not found in data.")
  if(!(share_field %in% names(data_sf))) stop("share_field not found in data.")
  
  data_sf[[share_field]] <- as.numeric(data_sf[[share_field]])
  data_leaf <- st_transform(data_sf, 4326)
  
  # ---- FIX 2: flexible color palette (no RColorBrewer limit) ----
  pal <- colorBin(
    palette = colorRampPalette(RColorBrewer::brewer.pal(9, palette))(length(bins) - 1),
    domain = data_leaf[[share_field]],
    bins = bins,
    na.color = "transparent",
    right = FALSE
  )
  
  # ---- Labels ----
  if(!is.null(area_field) && area_field %in% names(data_leaf)) {
    area_km <- round(as.numeric(data_leaf[[area_field]]) / 1e6, 1)
    labels_html <- sprintf(
      "<strong>%s</strong><br/>Green share: <strong>%.1f%%</strong><br/>Area: <strong>%.1f km²</strong>",
      data_leaf[[id_field]],
      data_leaf[[share_field]],
      area_km
    )
  } else {
    labels_html <- sprintf(
      "<strong>%s</strong><br/>Green share: <strong>%.1f%%</strong>",
      data_leaf[[id_field]],
      data_leaf[[share_field]]
    )
  }
  labels <- lapply(labels_html, HTML)
  
  # ---- Build leaflet map ----
  m <- leaflet(data_leaf, options = leafletOptions(minZoom = zoom_bounds[1], maxZoom = zoom_bounds[2])) %>%
    # Basemaps
    {tmp <- .
    for(b in basemaps) {
      if(b == "CartoDB.Positron") tmp <- tmp %>% addProviderTiles(providers$CartoDB.Positron, group = "Light (Grey)")
      if(b == "Esri.WorldStreetMap") tmp <- tmp %>% addProviderTiles(providers$Esri.WorldStreetMap, group = "Street Map")
      if(b == "Esri.WorldImagery") tmp <- tmp %>% addProviderTiles(providers$Esri.WorldImagery, group = "Aerial (Satellite)")
      if(b == "CartoDB.DarkMatter") tmp <- tmp %>% addProviderTiles(providers$CartoDB.DarkMatter, group = "Dark Theme")
    }
    tmp
    } %>%
    # Direct palette mapping (no .data masking)
    addPolygons(
      fillColor = pal(data_leaf[[share_field]]),
      color = "#444444",
      weight = 0.5,
      opacity = 1,
      fillOpacity = 0.75,
      highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
      label = labels,
      labelOptions = labelOptions(direction = "auto"),
      group = title
    ) %>%
    
    # Custom legend labels for readability (e.g. "0–5%", "5–10%")
    addLegend(
      pal = pal,
      values = data_leaf[[share_field]],
      title = "Green area (%)",
      position = "bottomright",
      labFormat = function(type, cuts, p) {
        paste0(cuts[-length(cuts)], "–", cuts[-1], "%")
      }
    ) %>%
    
    addLayersControl(
      baseGroups = c("Light (Grey)", "Street Map", "Aerial (Satellite)", "Dark Theme"),
      overlayGroups = c(title),
      options = layersControlOptions(collapsed = FALSE)
    )
  
  if(add_search) {
    m <- m %>% addSearchFeatures(targetGroups = title, options = searchFeaturesOptions(zoom = 12, openPopup = TRUE))
  }
  
  # ---- Optional: Save HTML ----
  if(isTRUE(save_html)) {
    saveWidget(m, file = html_path, selfcontained = selfcontained)
    message("✅ Saved HTML to: ", html_path)
  }
  
  # ---- Optional: Save PNG ----
  if(isTRUE(save_png)) {
    tryCatch({
      mapshot(m, file = png_path, vwidth = png_vwidth, vheight = png_vheight, delay = 0.5)
      message("✅ Saved PNG to: ", png_path)
    }, error = function(e) {
      warning("⚠️ Could not save PNG. Ensure webshot::install_phantomjs() is available.\nError: ", e$message)
    })
  }
  
  return(m)
}

# ================================================================================================================================
# 🔧 UPDATED: Consistent bins for both maps
my_bins <- c(0, 5, 10, 20, 30, 50, 70, 100)

# London map
m_london <- make_green_map(
  data_sf = lsoa_london,
  id_field = "LSOA21NM",
  share_field = "share_green",
  area_field = "area_total_m2",
  title = "Green share by LSOA",
  bins = my_bins,
  save_html = TRUE,
  html_path = "./03_output/green/maps/interactive/gbr/london_green_share_lsoa.html",
  selfcontained = TRUE,
  save_png = TRUE,
  png_path = "./03_output/green/maps/interactive/gbr/london_green_share_lsoa.png"
)

# Lima map
m_lima <- make_green_map(
  data_sf = district_lima,
  id_field = "NOMBDIST",
  share_field = "share_green",
  area_field = "total_area_m2",
  title = "Green share by district",
  bins = my_bins,
  save_html = TRUE,
  html_path = "./03_output/green/maps/interactive/per/lima_green_share_district.html",
  selfcontained = TRUE,
  save_png = TRUE,
  png_path = "./03_output/green/maps/interactive/per/lima_green_share_district.png"
)

# Sevilla map
m_sevilla <- make_green_map(
  data_sf = mun_sevilla,
  id_field = "NAMEUNIT",
  share_field = "share_green",
  area_field = "total_area_m2",
  title = "Green share by municipality",
  bins = my_bins,
  save_html = TRUE,
  html_path = "./03_output/green/maps/interactive/esp/sevilla_green_share_mun.html",
  selfcontained = TRUE,
  save_png = TRUE,
  png_path = "./03_output/green/maps/interactive/esp/sevilla_green_share_mun.png"
)

# Arequipa map
m_aqp <- make_green_map(
  data_sf = district_aqp,
  id_field = "NOMBDIST",
  share_field = "share_green",
  area_field = "total_area_m2",
  title = "Green share by district",
  bins = my_bins,
  save_html = TRUE,
  html_path = "./03_output/green/maps/interactive/per/arequipa_green_share_district.html",
  selfcontained = TRUE,
  save_png = TRUE,
  png_path = "./03_output/green/maps/interactive/per/arequipa_green_share_district.png"
)
