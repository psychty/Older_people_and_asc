# components of change 

# Loading some packages 
packages <- c('easypackages', 'tidyr', 'ggplot2', 'plyr', 'dplyr', 'scales', 'readxl', 'readr', 'purrr', 'stringr', 'PHEindicatormethods', 'rgdal', 'spdplyr', 'geojsonio', 'rmapshaper', 'jsonlite', 'rgeos', 'sp', 'sf', 'maptools', 'ggpol', 'magick', 'officer', 'leaflet', 'leaflet.extras', 'zoo', 'fingertipsR', 'nomisr', 'showtext', 'waffle', 'lemon', 'ggstream')
install.packages(setdiff(packages, rownames(installed.packages())))
easypackages::libraries(packages)

local_directory <- 'C:/Users/asus/OneDrive/Documents/Repositories/projecting_older_adult_assumptions/Data'

output_directory <- 'C:/Users/asus/OneDrive/Documents/Repositories/projecting_older_adult_assumptions/Outputs'

if(dir.exists(local_directory) != TRUE){
  dir.create(local_directory)
}

if(dir.exists(output_directory) != TRUE){
  dir.create(output_directory)
}

# Custom fonts ####

# https://cran.rstudio.com/web/packages/showtext/vignettes/introduction.html
library(showtext)
## Loading Google fonts (https://fonts.google.com/)
font_add_google('Poppins', 'poppins')

font_add(family = "poppins", regular = "C:/Users/asus/AppData/Local/Microsoft/Windows/Fonts/Poppins-Regular.ttf")
font_add(family = "poppinsb", regular = "C:/Users/asus/AppData/Local/Microsoft/Windows/Fonts/Poppins-Bold.ttf")

showtext_auto(TRUE)

ph_theme = function(){
  theme(
    plot.title = element_text(colour = "#000000", face = "bold", size = 12, family = 'poppinsb'),
    plot.subtitle = element_text(colour = "#000000", size = 12),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.background = element_rect(fill = "#FFFFFF"),
    panel.grid.major.y = element_line(colour = "#E7E7E7", size = .3),
    panel.grid.minor.y = element_blank(),
    strip.text = element_text(colour = "#000000", size = 11),
    strip.background = element_blank(),
    axis.ticks = element_line(colour = "#dbdbdb"),
    legend.title = element_text(colour = "#000000", size = 11, face = "bold"),
    legend.background = element_rect(fill = "#ffffff"),
    legend.key = element_rect(fill = "#ffffff", colour = "#ffffff"),
    legend.key.size = unit(.5,"line"),
    legend.text = element_text(colour = "#000000", size = 11),
    axis.text = element_text(colour = "#000000", size = 11),
    axis.title =  element_text(colour = "#000000", size = 11, face = "bold"),
    axis.line = element_line(colour = "#dbdbdb"),
    legend.position = 'none',
    # plot.margin = margin(t = 1, r = 1.5, b = 0.2,l = 0, unit = 'cm'),
    text = element_text(family = 'poppins'))}

if(file.exists( paste0(local_directory, '/detailed_pop_2020.xlsx') == FALSE)){
download.file('https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationprojections/datasets/componentsofchangebirthsdeathsandmigrationforregionsandlocalauthoritiesinenglandtable5/2018based/table53.xls',
              paste0(local_directory, '/pop_projections.xls'),
              mode = 'wb')
}

pop_projections <- read_excel("~/Repositories/projecting_older_adult_assumptions/Data/pop_projections.xls", 
                              sheet = "Persons", skip = 5) %>% 
  pivot_longer(cols = `2018`:`2043`,
               names_to = 'Year',
               values_to = 'Value') %>% 
  select(Area_name = AREA, Component = COMPONENT, Year, Value) %>% 
  filter(Component %in% c('Births', 'Deaths', 'All Migration Net', 'Internal Migration In', 'Internal Migration Out', 'International Migration In', 'International Migration Out', 'Cross-border Migration In', 'Cross-border Migration Out' , 'Other'))



Wsx_coc <- pop_projections %>% 
  filter(Area_name == 'West Sussex') %>% 
  filter(Component %in% c('Births', 'Deaths'))


ggplot(data = Wsx_coc,
       aes(x = Year,
           y = Value, 
           color = Component, 
           fill = Component)) +
  geom_stream(
    geom = "contour",
    color = "#000000",
    size = 1.25,
    bw = .45) +
  geom_stream(
    geom = "polygon",
    bw = .45,
    size = 0)#+
  scale_color_manual(
    expand = c(0, 0),
    values = pal,
    guide = "none"
  ) +
  scale_fill_manual(
    values = pal,
    name = NULL
  ) +
  facet_grid( ## needs facet_grid for space argument
    parameter ~ ., 
    scales = "free_y", 
    space = "free"
  )
