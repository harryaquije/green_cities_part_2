#Green areas analysis
#Date: December, 2025
#Objective: Create statics combined maps

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Parameters
library(magick)

rm(list = ls())
setwd(here::here())

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Read both images
sevilla <- image_read("./02_intermediate_data/green/map/static/esp/gsevilla_v2.png")
aqp   <- image_read("./02_intermediate_data/green/map/static/per/garequipa_v2.png")

# --- 2. Resize both to same height (important for alignment) ---
target_height <- min(image_info(sevilla)$height, image_info(aqp)$height)
sevilla <- image_scale(sevilla, paste0("x", target_height))
aqp   <- image_scale(aqp,   paste0("x", target_height))

# Option 1 — Side-by-side (landscape)
combined_side <- image_append(c(sevilla, aqp))

# Option 2 — One on top of the other (portrait)
combined_stack <- image_append(c(sevilla, aqp), stack = TRUE)

# Save both versions to compare
image_write(combined_side, path = "./02_intermediate_data/green/map/static/gsevilla_aqp_v1.png", format = "png")
image_write(combined_stack, path = "./02_intermediate_data/green/map/static/gsevilla_aqp_v2.png", format = "png")

# Optional: resize for LinkedIn post (width ~1600px)
linkedin_img <- image_scale(combined_side, "1600")
image_write(linkedin_img, path = "./02_intermediate_data/green/map/static/gsevilla_aqp_v1_in.png", format = "png")
