
# Can we use OpenBus data to support work around travel barriers?

packages <- c('easypackages', 'tidyr', 'ggplot2', 'dplyr', 'scales', 'readxl', 'readr', 'purrr', 'spdplyr', 'geojsonio', 'jsonlite', 'sf', 'leaflet', 'htmlwidgets', 'PostcodesioR', 'osrm', 'viridis', 'osmdata', 'bodsr', 'tidytransit', 'naptanr', 'keyring')
install.packages(setdiff(packages, rownames(installed.packages())))
easypackages::libraries(packages)

areas <- c('Adur', 'Arun', 'Chichester', 'Crawley', 'Horsham', 'Mid Sussex', 'Worthing')

# base directory
base_directory <- '~/Repositories/Older_people_and_ASC/'

data_directory <- paste0(base_directory, 'Data')
output_directory <- paste0(base_directory, 'Outputs')

# visualising outputs
map_theme = function(){
  theme( 
    legend.position = "bottom", 
    legend.key.size = unit(.75,"line"),
    legend.title = element_text(size = 8, face = 'bold'),
    plot.background = element_blank(), 
    plot.title.position = "plot",
    panel.background = element_blank(),  
    panel.border = element_blank(),
    axis.text = element_blank(), 
    plot.title = element_text(colour = "#000000", face = "bold", size = 11), 
    plot.subtitle = element_text(colour = "#000000", size = 10), 
    axis.title = element_blank(),     
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.x = element_blank(), 
    panel.grid.major.y = element_blank(), 
    panel.grid.minor.y = element_blank(), 
    strip.text = element_text(colour = "white"), 
    strip.background = element_rect(fill = "#ffffff"), 
    axis.ticks = element_blank()
  ) 
}

# Set up a wsx bounding box - if this exists in the data directory then it will skip this code
if(file.exists(paste0(output_directory, '/west_sussex_LTLAs.geojson'))!= TRUE){

lad_boundaries_clipped_sf <- read_sf('https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Local_Authority_Districts_May_2022_UK_BFC_V3_2022/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson') %>%
  filter(LAD22NM %in% c('Chichester'))

lad_boundaries_full_extent_sf <- read_sf('https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Local_Authority_Districts_May_2022_UK_BFE_V3_2022/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson') %>%
  filter(LAD22NM %in% areas & LAD22NM != 'Chichester')

lad_boundaries_sf <- rbind(lad_boundaries_clipped_sf, lad_boundaries_full_extent_sf)
lad_boundaries_spdf <- as_Spatial(lad_boundaries_sf, IDs = lad_boundaries_sf$LAD22NM)

geojson_write(lad_boundaries_spdf, file = paste0(output_directory, '/west_sussex_LTLAs.geojson')) 
}

lad_boundaries_spdf <- read_sf(paste0(output_directory, '/west_sussex_LTLAs.geojson')) %>% 
  as_Spatial()

area_bb <- bbox(lad_boundaries_spdf)

# Whilst we are not sharing data, we may share R scripts and these contain the database key.
# Use a package called keyring to set passwords and other credentials to access the database outside of R scripts. 
#keyring::key_set('database_name')
#keyring::key_set('dobs_key')

# NaPTAN ####

# NaPTAN is an open dataset held by DfT containing bus stop, train station, tram, metro, ferry terminal and airport in England, Wales and Scotland 

# You can download NaPTAN data as a csv directly from the website https://beta-naptan.dft.gov.uk/Download/National

NaPTAN_df <- read_csv(paste0(data_directory, '/Stops.csv'))

NaPTAN_df %>% names()

# Using the API for live (more live?) data

## Returns area code for West Sussex only
lookup_atco_codes(area_name = 'west sussex') 
# This search operates via regular expression and is not case sensitive.
# This will return additional hits if you search for part tags (e.g. sussex will return east and west)
# lookup_atco_codes('sussex')








# BODSR - Open Bus Data ####

get_location_gtfs(route_id = "45",
                  api_key = key_get('dobs_key'))

#Return data within a specified bounding box
area_location_df <- get_location_gtfs(bounding_box = area_bb,
                  key_get('dobs_key'))
