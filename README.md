# Green Areas Analysis - part 2: Seville & Arequipa

This repository contains the code and resources for the geospatial analysis of urban green areas in Seville and Arequipa. The analysis supports the article *“What Does Green Access Look Like in your City? - Part 2”*.

---

## Project Structure

scripts/ <- R scripts for analysis
│ ├── 01_aqp_v1.R
│ ├── 01_sevilla_v1.R
│ ├── 02_aqp_v1.R
│ ├── 02_sevilla_v1.R
│ ├── 03_map_int_v1.R
│ ├── 01_map_sta_v1.R

## Data Sources

- **CNIG**: Instituto Geográfico Nacional. Límites municipales, provinciales y autonómicos
- **INEI**: Instituto Nacional de Estadística e Informática. Límites Distritales del Perú
- **OMS**: Open Map Services / OpenStreetMap, for geospatial mapping  
- **Note**: Only non-sensitive and shareable data is included in this repository. Any personal or confidential information has been removed.

---
## Getting Started

### Requirements

- R (version ≥ 4.2)
- R packages: `osmdata`, `sf`, `dplyr`, `units`, `ggplot2`, `ggspatial`, `here`

Install required packages using:

```r
install.packages(c("osmdata", "sf", "dplyr", "units", "ggplot2", "ggspatial", "here"))
