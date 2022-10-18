# Loading some packages 
packages <- c('easypackages', 'tidyr', 'ggplot2', 'plyr', 'dplyr', 'scales', 'readxl', 'readr', 'purrr', 'stringr', 'PHEindicatormethods', 'rgdal', 'spdplyr', 'geojsonio', 'rmapshaper', 'jsonlite', 'rgeos', 'sp', 'sf', 'maptools', 'ggpol', 'magick', 'officer', 'leaflet', 'leaflet.extras', 'zoo', 'fingertipsR', 'nomisr', 'showtext', 'waffle', 'lemon')
install.packages(setdiff(packages, rownames(installed.packages())))
easypackages::libraries(packages)

local_directory <- '//chi_nas_prod2.corporate.westsussex.gov.uk/groups2.bu/Public Health Directorate/PH Research Unit/Adult Social Care/Data'

output_directory <- '//chi_nas_prod2.corporate.westsussex.gov.uk/groups2.bu/Public Health Directorate/PH Research Unit/Adult Social Care/Outputs'

if(dir.exists(local_directory) != TRUE){
  dir.create(local_directory)
}

if(dir.exists(output_directory) != TRUE){
  dir.create(output_directory)
}

# Custom fonts ####
# library(extrafont) 
#library(showtext)
showtext_auto(TRUE)

# Load up Poppins font
font_paths("\\\\chi_nas_prod2.corporate.westsussex.gov.uk/groups2.bu/Public Health Directorate/PH Research Unit/Rich Tyler/Fonts")
# font_add(family = "roboto", "Roboto-Regular.ttf")
font_add(family = "poppins", "Poppins-Regular.ttf")
font_add(family = "poppinsb", "Poppins-Bold.ttf")

# general themes for ggplots
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

# Styles for our pyramid plots
pyramid_theme <- function(){
  theme(plot.background = element_rect(fill = "white", colour = "#ffffff"),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(colour = "#000000", family = "poppinsb", size = 12, vjust = 1),
        axis.text = element_text(colour = "#000000", size = 11),
        plot.subtitle = element_text(colour = "#000000", size = 12, vjust = 1),
        axis.title = element_text(colour = "#000000", face = "bold", size = 10),
        strip.text = element_text(colour = "#000000", size = 12),
        text = element_text(family = 'poppins'),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        strip.background = element_rect(fill = "#ffffff"),
        axis.ticks = element_line(colour = "#E2E2E3"),
        legend.position = 'none')}

waffle_theme <- function(){
  theme(plot.title = element_text(colour = "#000000", face = "bold", size = 12, family = 'poppinsb'),
        plot.subtitle = element_text(colour = "#000000", size = 12),
        strip.text = element_text(colour = "#000000", size = 11),
        legend.title = element_text(colour = "#000000", size = 11, face = "bold"),
        legend.text = element_text(colour = "#000000", size = 11),
        text = element_text(family = 'poppins'), 
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "#FFFFFF"),
        strip.background = element_blank(),
        axis.ticks = element_blank(),
        legend.key.size = unit(.5,"line"),
        legend.background = element_rect(fill = "#ffffff"),
        legend.key = element_rect(fill = "#ffffff", colour = "#ffffff"),
        axis.text = element_blank(),
        axis.line = element_blank(),
        legend.position = 'bottom',
        plot.caption = element_text(lineheight = 0.5))}

# Overall Population numbers and projections ####
wsx_areas <- c('Adur', 'Arun', 'Chichester', 'Crawley', 'Horsham', 'Mid Sussex', 'Worthing', 'West Sussex')

# Current population estimates - using Mid Year Estimates and Census 2021 early release information ####
if(file.exists(paste0(local_directory, '/First_results_EW_28_June_22.xlsx')) != TRUE){
  download.file('https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/populationandhouseholdestimatesenglandandwalescensus2021/census2021/census2021firstresultsenglandwales1.xlsx', paste0(local_directory, '/First_results_EW_28_June_22.xlsx'), mode = 'wb')
}

# This is rounded to nearest 100
First_results_EW_raw_df_1 <- read_excel(paste0(local_directory, '/First_results_EW_28_June_22.xlsx'), 
                                        sheet = "P03", skip = 7) %>% 
  pivot_longer(cols = `All persons`:`Males:\r\nAged 90 years and over\r\n[note 12]`,
               names_to = 'Age',
               values_to = 'Population') %>% 
  filter(Age != 'All persons') %>% 
  mutate(Sex = ifelse(substr(Age, 1,1) == 'F', 'Females', ifelse(substr(Age, 1,1) == 'M', 'Males', ifelse(substr(Age, 1,1) == 'A', 'Persons', NA)))) %>% 
  mutate(Age_group = ifelse(str_detect(Age, '4 years and under'), '0-4 years', ifelse(str_detect(Age, '5 to 9 years'), '5-9 years', ifelse(str_detect(Age, '10 to 14'), '10-14 years', ifelse(str_detect(Age, '15 to 19 years'), '15-19 years', ifelse(str_detect(Age, '20 to 24'), '20-24 years', ifelse(str_detect(Age, '25 to 29 years'), '25-29 years', ifelse(str_detect(Age, '30 to 34'), '30-34 years', ifelse(str_detect(Age, '35 to 39 years'), '35-39 years', ifelse(str_detect(Age, '40 to 44'), '40-44 years', ifelse(str_detect(Age, '45 to 49 years'), '45-49 years', ifelse(str_detect(Age, '50 to 54'), '50-54 years', ifelse(str_detect(Age, '55 to 59 years'), '55-59 years', ifelse(str_detect(Age, '60 to 64'), '60-64 years',  ifelse(str_detect(Age, '65 to 69 years'), '65-69 years', ifelse(str_detect(Age, '70 to 74'), '70-74 years', ifelse(str_detect(Age, '75 to 79 years'), '75-79 years', ifelse(str_detect(Age, '80 to 84'), '80-84 years', ifelse(str_detect(Age, '85 to 89 years'), '85-89 years', ifelse(str_detect(Age, '90 years and over'), '90+ years', NA)))))))))))))))))))) %>% 
  mutate(Age_group = factor(Age_group, 
                            levels = c('0-4 years', '5-9 years', '10-14 years', '15-19 years', '20-24 years', '25-29 years', '30-34 years', '35-39 years', '40-44 years', '45-49 years', '50-54 years', '55-59 years', '60-64 years', '65-69 years', '70-74 years', '75-79 years', '80+ years', '80-84 years', '85-89 years', '90+ years'))) %>% 
  select(Area = 'Area name', Sex, Age_group, Population) %>% 
  filter(Area %in% c('Adur', 'Arun', 'Chichester', 'Crawley', 'Horsham', 'Mid Sussex', 'Worthing', 'West Sussex')) %>% 
  mutate(Data_source = 'Census 2021')

First_results_EW_raw_df_2 <- read_excel(paste0(local_directory, '/First_results_EW_28_June_22.xlsx'), 
                                        sheet = "P02", skip = 7) %>% 
  pivot_longer(cols = `All persons`:`Aged 90 years and over\r\n[note 12]`,
               names_to = 'Age',
               values_to = 'Population') %>% 
  filter(Age != 'All persons') %>% 
  mutate(Sex = 'Persons') %>% 
  mutate(Age_group = ifelse(str_detect(Age, '4 years and under'), '0-4 years', ifelse(str_detect(Age, '5 to 9 years'), '5-9 years', ifelse(str_detect(Age, '10 to 14'), '10-14 years', ifelse(str_detect(Age, '15 to 19 years'), '15-19 years', ifelse(str_detect(Age, '20 to 24'), '20-24 years', ifelse(str_detect(Age, '25 to 29 years'), '25-29 years', ifelse(str_detect(Age, '30 to 34'), '30-34 years', ifelse(str_detect(Age, '35 to 39 years'), '35-39 years', ifelse(str_detect(Age, '40 to 44'), '40-44 years', ifelse(str_detect(Age, '45 to 49 years'), '45-49 years', ifelse(str_detect(Age, '50 to 54'), '50-54 years', ifelse(str_detect(Age, '55 to 59 years'), '55-59 years', ifelse(str_detect(Age, '60 to 64'), '60-64 years',  ifelse(str_detect(Age, '65 to 69 years'), '65-69 years', ifelse(str_detect(Age, '70 to 74'), '70-74 years', ifelse(str_detect(Age, '75 to 79 years'), '75-79 years', ifelse(str_detect(Age, '80 to 84'), '80-84 years', ifelse(str_detect(Age, '85 to 89 years'), '85-89 years', ifelse(str_detect(Age, '90 years and over'), '90+ years', NA)))))))))))))))))))) %>% 
  mutate(Age_group = factor(Age_group, 
                            levels = c('0-4 years', '5-9 years', '10-14 years', '15-19 years', '20-24 years', '25-29 years', '30-34 years', '35-39 years', '40-44 years', '45-49 years', '50-54 years', '55-59 years', '60-64 years', '65-69 years', '70-74 years', '75-79 years', '80+ years', '80-84 years', '85-89 years', '90+ years'))) %>%
  select(Area = 'Area name', Sex, Age_group, Population) %>% 
  filter(Area %in% c('Adur', 'Arun', 'Chichester', 'Crawley', 'Horsham', 'Mid Sussex', 'Worthing', 'West Sussex')) %>% 
  mutate(Data_source = 'Census 2021')

First_results_EW_raw_df <- First_results_EW_raw_df_1 %>% 
  bind_rows(First_results_EW_raw_df_2) %>% 
  mutate(Sex = factor(Sex, levels = c('Females', 'Males', 'Persons')))

# Visualising as a pyramid ####

# We use ggplot to create population pyramids by creating two facets (one for male and one for female) and control the direction of bars by making the female values negative and male values positive. So we need to create a function that creates an absolute (removes the sign) value and adds a comma separator.
abs_comma <- function (x, ...) {
  format(abs(x), ..., big.mark = ",", scientific = FALSE, trim = TRUE)}

Area_x <- 'West Sussex'

pyramid_x_df <- First_results_EW_raw_df %>% 
  filter(Area == Area_x)

pyramid_x_df_limit <- pyramid_x_df %>% 
  filter(Sex != 'Persons') %>% 
  mutate(Population = abs(Population)) %>% 
  select(Population) %>% 
  unique() %>% 
  max()

pyramid_x_df_limit <- ifelse(pyramid_x_df_limit <= 1000, round_any(pyramid_x_df_limit, 500, ceiling), ifelse(pyramid_x_df_limit <= 5000, round_any(pyramid_x_df_limit, 1000, ceiling), ifelse(pyramid_x_df_limit <= 10000, round_any(pyramid_x_df_limit, 2000, ceiling), ifelse(pyramid_x_df_limit < 40000, round_any(pyramid_x_df_limit, 5000, ceiling), round_any(pyramid_x_df_limit, 10000, ceiling)))))

pyramid_x_breaks <- ifelse(pyramid_x_df_limit <= 1000, 100,  ifelse(pyramid_x_df_limit <= 5000, 1000,  ifelse(pyramid_x_df_limit <= 10000, 2000, ifelse(pyramid_x_df_limit < 40000, 5000, 10000))))

area_x_dummy <- pyramid_x_df %>% 
  mutate(Denominator_dummy = ifelse(Sex == 'Females', 0 - pyramid_x_df_limit, pyramid_x_df_limit)) %>% 
  filter(Sex != 'Persons')

pyramid_plot_a <- pyramid_x_df %>%
  filter(Sex != 'Persons') %>% 
  ggplot() +
  geom_bar(data = area_x_dummy, aes(x = Age_group,
                                    y = Denominator_dummy),
           stat = "identity",
           fill = NA) +
  geom_bar(aes(x = Age_group, 
               y = ifelse(Sex == 'Females', -Population, Population),
               fill = Sex), 
           colour = '#ffffff',
           width = .95,
           stat = "identity",
           position = position_dodge()) +
  scale_fill_manual(values =  c("#934c93", "#ff9966"),
                    name = 'Sex') +
  labs(x = '',
       y = '',
       title = paste0('Population pyramid; ', Area_x),
       subtitle = 'Census 2021;',
       caption = 'Figures are rounded to the nearest 100') +
  facet_share(~Sex,
              dir = "h",
              scales = "free",
              switch = 'both',
              reverse_num = FALSE) +
  scale_y_continuous(labels = abs_comma,
                     breaks = seq(-pyramid_x_df_limit, pyramid_x_df_limit, pyramid_x_breaks)) +
  coord_flip() +
  pyramid_theme() 

png(paste0(output_directory,'/West Sussex Census 2021 Population structure.png'),
    width = 1080,
    height = 880,
    res = 200)
print(pyramid_plot_a)
dev.off()

# Mid year estimates 2020

# https://www.nomisweb.co.uk/api/v01/dataset/NM_2002_1.data.csv?geography=1807745163,1811939621...1811939627&date=latest&gender=1,2&c_age=200,0,101...191&measures=20100

mye_df <- nomis_get_data(id = 'NM_2002_1',
               time = 'latestMINUS7-latest',
               sex = '1,2',
               measures = '20100',
               c_age = '101...191',
               geography = "1807745163,1811939621...1811939627") %>%
  select(Area_code = GEOGRAPHY_CODE, Area = GEOGRAPHY_NAME, Year = DATE, Sex = GENDER_NAME, Age = C_AGE_NAME, Population = OBS_VALUE) %>% 
  mutate(Age = as.numeric(ifelse(Age == 'Aged 90+' , 90, gsub('Age ', '', Age)))) %>% 
  mutate(Age_group = factor(ifelse(Age <= 4, "0-4 years", ifelse(Age <= 9, "5-9 years", ifelse(Age <= 14, "10-14 years", ifelse(Age <= 19, "15-19 years", ifelse(Age <= 24, "20-24 years", ifelse(Age <= 29, "25-29 years",ifelse(Age <= 34, "30-34 years", ifelse(Age <= 39, "35-39 years",ifelse(Age <= 44, "40-44 years", ifelse(Age <= 49, "45-49 years",ifelse(Age <= 54, "50-54 years", ifelse(Age <= 59, "55-59 years",ifelse(Age <= 64, "60-64 years", ifelse(Age <= 69, "65-69 years",ifelse(Age <= 74, "70-74 years", ifelse(Age <= 79, "75-79 years",ifelse(Age <= 84, "80-84 years",ifelse(Age <= 89, "85-89 years", "90+ years")))))))))))))))))), levels = c('0-4 years', '5-9 years','10-14 years','15-19 years','20-24 years','25-29 years','30-34 years','35-39 years','40-44 years','45-49 years','50-54 years','55-59 years','60-64 years','65-69 years','70-74 years','75-79 years','80-84 years','85-89 years', '90+ years'))) %>% 
  group_by(Area_code, Area, Year, Sex, Age_group) %>% 
  summarise(Population = sum(Population, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(Population_rounded = round(Population, -2)) %>% 
  mutate(Sex = paste0(Sex, 's'))

pyramid_df_x2 <- mye_df %>% 
  filter(Area == Area_x) %>% 
  filter(Year == 2020)

pyramid_plot_b <- pyramid_df_x2 %>%
  ggplot() +
  geom_bar(data = area_x_dummy, aes(x = Age_group,
                                    y = Denominator_dummy),
           stat = "identity",
           fill = NA) +
  geom_bar(aes(x = Age_group, 
               y = ifelse(Sex == 'Females', -Population, Population),
               fill = Sex), 
           colour = '#ffffff',
           width = .95,
           stat = "identity",
           position = position_dodge()) +
  scale_fill_manual(values =  c("#934c93", "#ff9966"),
                    name = 'Sex') +
  labs(x = '',
       y = '',
       title = paste0('Population pyramid; ', Area_x),
       subtitle = 'Mid 2020 estimates;',
       caption = paste0('These figures are based on unrounded values.')) +
  facet_share(~Sex,
              dir = "h",
              scales = "free",
              switch = 'both',
              reverse_num = FALSE) +
  scale_y_continuous(labels = abs_comma,
                     breaks = seq(-pyramid_x_df_limit, pyramid_x_df_limit, pyramid_x_breaks)) +
  coord_flip() +
  pyramid_theme() +
  theme(legend.position = 'none')

png(paste0(output_directory,'/West Sussex Mid 2020 Population structure.png'),
    width = 1080,
    height = 880,
    res = 200)
print(pyramid_plot_b)
dev.off()

pyramid_plot_ab <- pyramid_df_x2 %>%
  ggplot() +
  geom_bar(data = area_x_dummy, aes(x = Age_group,
                                    y = Denominator_dummy),
           stat = "identity",
           fill = NA) +
  geom_bar(aes(x = Age_group, 
               y = ifelse(Sex == 'Females', -Population, Population),
               fill = Sex), 
           colour = '#ffffff',
           width = .95,
           stat = "identity",
           position = position_dodge()) +
  scale_fill_manual(values =  c("#934c93", "#ff9966"),
                    name = 'Sex') +
  geom_bar(data = subset(pyramid_x_df, Sex != 'Persons'),
           aes(x = Age_group, 
               y = ifelse(Sex == 'Females', -Population, Population)),
           fill = NA, 
           colour = '#999999',
           lty = 'longdash',
           width = .95,
           stat = "identity",
           position = position_dodge()) +
  labs(x = '',
       y = '',
       title = 'Population pyramid; West Sussex',
       subtitle = '2020 mid year estimates (bars) with 2021 census (dashed lines) overlaid',
       caption = 'This figure shows that the census estimates show greater numbers of people in some age groups (particularly 15-49 years).\nHowever, note that the census estimates are rounded to the nearest 100 whilst ordinary mid year estimates are unrounded.') +
  facet_share(~Sex,
              dir = "h",
              scales = "free",
              switch = 'both',
              reverse_num = FALSE) +
  scale_y_continuous(labels = abs_comma,
                     breaks = seq(-pyramid_x_df_limit, pyramid_x_df_limit, pyramid_x_breaks)) +
  coord_flip() +
  pyramid_theme() 

png(paste0(output_directory,'/West Sussex Mid 2020 vs 2021 Population structure.png'),
    width = 1080,
    height = 880,
    res = 200)
print(pyramid_plot_ab)
dev.off()

# https://www.nomisweb.co.uk/api/v01/dataset/NM_2006_1.data.csv?geography=1816133768,1820328217...1820328223&projected_year=2018...2043&gender=1,2&c_age=200,101...191&measures=20100

projections_df <- nomis_get_data(id = 'NM_2006_1',
               c_age = '101...191',
               sex = '1,2',
               measures = '20100',
               projected_year = '2018...2043',
               geography = "1816133768,1820328217...1820328223") %>%
  select(Area_code = GEOGRAPHY_CODE, Area = GEOGRAPHY_NAME, Year = PROJECTED_YEAR, Sex = GENDER_NAME, Age = C_AGE_NAME, Population = OBS_VALUE) %>% 
  mutate(Age = as.numeric(ifelse(Age == 'Aged 90+' , 90, gsub('Age ', '', Age)))) %>% 
  mutate(Age_group = factor(ifelse(Age <= 4, "0-4 years", ifelse(Age <= 9, "5-9 years", ifelse(Age <= 14, "10-14 years", ifelse(Age <= 19, "15-19 years", ifelse(Age <= 24, "20-24 years", ifelse(Age <= 29, "25-29 years",ifelse(Age <= 34, "30-34 years", ifelse(Age <= 39, "35-39 years",ifelse(Age <= 44, "40-44 years", ifelse(Age <= 49, "45-49 years",ifelse(Age <= 54, "50-54 years", ifelse(Age <= 59, "55-59 years",ifelse(Age <= 64, "60-64 years", ifelse(Age <= 69, "65-69 years",ifelse(Age <= 74, "70-74 years", ifelse(Age <= 79, "75-79 years",ifelse(Age <= 84, "80-84 years",ifelse(Age <= 89, "85-89 years", "90+ years")))))))))))))))))), levels = c('0-4 years', '5-9 years','10-14 years','15-19 years','20-24 years','25-29 years','30-34 years','35-39 years','40-44 years','45-49 years','50-54 years','55-59 years','60-64 years','65-69 years','70-74 years','75-79 years','80-84 years','85-89 years', '90+ years'))) %>% 
  group_by(Area_code, Area, Year, Sex, Age_group) %>% 
  summarise(Population = sum(Population, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(Population_rounded = round(Population, -2)) %>% 
  mutate(Sex = paste0(Sex, 's'))

pyramid_df_x3 <- projections_df %>% 
  filter(Area == Area_x) %>% 
  filter(Year == 2030) 
  
pyramid_plot_c <- pyramid_df_x3 %>%
  ggplot() +
  geom_bar(data = area_x_dummy, aes(x = Age_group,
                                    y = Denominator_dummy /2),
           stat = "identity",
           fill = NA) +
  geom_bar(aes(x = Age_group, 
               y = ifelse(Sex == 'Females', -Population_rounded, Population_rounded),
               fill = Sex), 
           colour = '#ffffff',
           width = .95,
           stat = "identity",
           position = position_dodge()) +
  scale_fill_manual(values =  c("#934c93", "#ff9966"),
                    name = 'Sex') +
  labs(x = '',
       y = '',
       title = paste0('Population pyramid; ', Area_x),
       subtitle = 'Projected mid 2030 estimates; based on 2018 estimates',
       caption =  'Note: projections based on mid 2018 population estimates.') +
  facet_share(~Sex,
              dir = "h",
              scales = "free",
              switch = 'both',
              reverse_num = FALSE) +
  scale_y_continuous(labels = abs_comma,
                     breaks = seq(-pyramid_x_df_limit, pyramid_x_df_limit, pyramid_x_breaks)) +
  coord_flip() +
  pyramid_theme() +
  theme(legend.position = 'none')

png(paste0(output_directory,'/West Sussex Projected Mid 2030 Population structure (based on mid 2018 estimates).png'),
    width = 1080,
    height = 880,
    res = 200)
print(pyramid_plot_c)
dev.off()

pyramid_plot_bc <- pyramid_df_x2 %>%
  ggplot() +
  geom_bar(data = area_x_dummy, aes(x = Age_group,
                                    y = Denominator_dummy),
           stat = "identity",
           fill = NA) +
  geom_bar(aes(x = Age_group, 
               y = ifelse(Sex == 'Females', -Population, Population),
               fill = Sex), 
           colour = '#ffffff',
           width = .95,
           stat = "identity",
           position = position_dodge()) +
  scale_fill_manual(values =  c("#934c93", "#ff9966"),
                    name = 'Sex') +
  geom_bar(data = subset(pyramid_df_x3, Sex != 'Persons'),
           aes(x = Age_group, 
               y = ifelse(Sex == 'Females', -Population, Population)),
           fill = NA, 
           colour = '#999999',
           lty = 'longdash',
           width = .95,
           stat = "identity",
           position = position_dodge()) +
  labs(x = '',
       y = '',
       title = 'Population pyramid; West Sussex' ,
       subtitle = '2020 mid year estimates (bars) with 2030 projected estimates (dashed lines) overlaid',
       caption = 'By 2030, West Sussex could have an even older population structure with greater numbers of people in older age groups\nfor both men and women whilst estimates for those aged 40-59 are broadly similar. Note: projections based on mid 2018 population estimates.') +
  facet_share(~Sex,
              dir = "h",
              scales = "free",
              switch = 'both',
              reverse_num = FALSE) +
  scale_y_continuous(labels = abs_comma,
                     breaks = seq(-pyramid_x_df_limit, pyramid_x_df_limit, pyramid_x_breaks)) +
  coord_flip() +
  pyramid_theme() +
  theme(legend.position = 'none')

png(paste0(output_directory,'/West Sussex Projected Mid 2030 Population structure vs 2020 estimates.png'),
    width = 1080,
    height = 880,
    res = 200)
print(pyramid_plot_bc)
dev.off()

# Projections - overs 65s plot
wsx_projections <- projections_df %>% 
  filter(!Year %in% c(2018,2019,2020)) %>% 
  mutate(Type = 'Projections')

wsx_population_ts <- mye_df %>% 
  mutate(Type = 'Estimates') %>% 
  bind_rows(wsx_projections) %>% 
  filter(Age_group %in% c('65-69 years', '70-74 years', '75-79 years', '80-84 years', '85-89 years',
'90+ years')) %>% 
  mutate(Age_group = factor(ifelse(Age_group %in% c('65-69 years', '70-74 years'), '65-74 years', ifelse(Age_group %in% c('75-79 years', '80-84 years'), '75-84 years', ifelse(Age_group %in% c('85-89 years', '90+ years'), '85+ years', NA))), levels = c("65-74 years", "75-84 years", "85+ years"))) %>% 
  group_by(Area_code, Area, Age_group, Sex, Year, Type) %>% 
  summarise(Population = sum(Population, na.rm = TRUE)) %>% 
  arrange(Area_code, Area, Age_group, Sex, Year)

area_x_stacked_bars <- wsx_population_ts %>% 
  filter(Area == Area_x) %>% 
  mutate(Age_group = factor(Age_group, levels = rev(c("65-74 years", "75-84 years", "85+ years"))))

over_65_plot <- area_x_stacked_bars %>% 
  ggplot(aes(x = Year,
             y = Population,
             fill = Age_group)) +
  geom_bar(stat = 'identity') +
  labs(title = "West Sussex resident population aged 65+",
       subtitle = "Estimates 2013 to 2020; Projected to 2043",
       caption = 'Note: projections based on mid 2018 population estimates.',
       x= "Year",
       y = "Population") +
  scale_x_continuous(breaks = seq(min(area_x_stacked_bars$Year), max(area_x_stacked_bars$Year), by = 1),
                     expand = c(0,0.01)) +
  scale_y_continuous(limits = c(0,300000), 
                     breaks = seq(0,300000,25000), 
                     labels = comma,
                     expand = c(0, 0.01)) +
  scale_fill_manual(values = c("#A6A6A6", "#404040", "#FD0000"), 
                    breaks = c("65-74 years", "75-84 years", "85+ years"), 
                    name = "Age") +
  geom_vline(xintercept = 2020.5, lty = "dotted", colour = "#000000", lwd = 1) +
  annotate(geom = "rect", xmin = 2020.5, xmax = Inf, ymin = 0, ymax = Inf, fill = "#e7e7e7", alpha = 0.35) +
  annotate(geom = "text", x = 2017, y = 275000, label = "Estimated", fontface = "bold", size = 5) +
  annotate(geom = "text", x = 2030, y = 275000, label = "Projected", fontface = "bold", size = 5) +
  ph_theme() +
  theme(axis.text.x = element_text(angle = 90),
        legend.position = 'top')

png(paste0(output_directory,'/West Sussex resident population over 65 estimates to 2020 projected to 2030.png'),
    width = 1080,
    height = 580,
    res = 200)
print(over_65_plot)
dev.off()

over_65_plot_b <- over_65_plot +
  scale_y_continuous(limits = c(0,175000), 
                     breaks = seq(0,175000,25000), 
                     labels = comma,
                     expand = c(0, 0.01)) +
  annotate(geom = "text", x = 2017, y = 160000, label = "Estimated", fontface = "bold", size = 5) +
  annotate(geom = "text", x = 2030, y = 160000, label = "Projected", fontface = "bold", size = 5) +
  facet_rep_wrap(~ Sex, nrow = 2, repeat.tick.labels = TRUE)

png(paste0(output_directory,'/West Sussex resident population over 65 estimates to 2020 projected to 2030 by sex.png'),
    width = 1080,
    height = 880,
    res = 200)
print(over_65_plot_b)
dev.off()

# Overall population change table ####
# TODO - Area, 2018 Population, 2020 Population (measured), 2025 Population (projected), 2030 Population (projected), Percentage population change

# Tabled starting point of estimates (2018), latest available unrounded estimates (2020), projected to current year (2022), then every five years for 20 years

wsx_projected_table_a <- wsx_population_ts %>% 
  filter(Year %in% c(2018, 2020, 2022, 2027, 2032, 2037, 2042),
         Area == Area_x) %>% 
  group_by(Area, Sex, Year) %>% 
  summarise(Population = sum(Population, na.rm = TRUE)) 

wsx_projected_table_b <- wsx_population_ts %>% 
  filter(Year %in% c(2018, 2020, 2022, 2027, 2032, 2037, 2042),
         Area == Area_x) %>% 
  group_by(Area, Year) %>% 
  summarise(Population = sum(Population, na.rm = TRUE),
            Sex = 'Persons') 

wsx_projected_table <- wsx_projected_table_b %>% 
  bind_rows(wsx_projected_table_a) %>%
  mutate(Age_group = '65+ years') %>% 
  select(Area, Sex, Age_group, Year, Population) %>% 
  mutate(Year = ifelse(Year %in% c(2018, 2020), paste0(Year, ' (estimated)'), paste0(Year, ' (projected)'))) %>% 
  pivot_wider(names_from = Year,
              values_from = Population) %>% 
  mutate(Change_2022_2032 = paste0('+', format(`2032 (projected)` - `2022 (projected)`, big.mark = ','), ' (+', round(((`2032 (projected)` - `2022 (projected)`) / `2022 (projected)`)* 100, 1), '%)'),
         Change_2022_2042 = paste0('+', format(`2042 (projected)` - `2022 (projected)`, big.mark = ','), ' (+', round(((`2042 (projected)` - `2022 (projected)`) / `2022 (projected)`)* 100, 1), '%)')) %>% 
  mutate_at(vars("2018 (estimated)", "2020 (estimated)", "2022 (projected)", "2027 (projected)", "2032 (projected)", "2037 (projected)", "2042 (projected)"), ~format(., big.mark = ','))

# High level projecting number of people of State Pension age, Working age, and the old age dependency ratios

# Long-term subnational population projections are an indication of the future trends in population by age and sex over the next 25 years. They are trend-based projections, which means assumptions for future levels of births, deaths and migration are based on observed levels mainly over the previous five years. They show what the population would be if recent trends continue.

# Subnational population projections do not attempt to predict the impact of political circumstances such as the UK's withdrawal from the European Union.

# These projections account for the planned changes to State Pension age under existing legislation. Otherwise, the projections do not take into account any policy changes that have not yet occurred, nor those that have not yet had an impact on observed trends. The projections published on 24 March 2020 are based on the mid 2018 estimates

# These data are based on administrative geographic boundaries in place on the reference date of 30 June 2018.
# Old age dependency ratio (OADR) is the number of people of State Pension age per 1,000 people of working age.
# The number of people of State Pension age is calculated by multiplying State Pension age factors against corresponding ages. The State Pension age factors are available in the Table of State Pension Age factors
# Being over the state pension age does not necessarily mean someone is retired nor are all working age people in employment

if(file.exists(paste0(local_directory, '/2018_based_high_level_SPA_WA_Dependency_Ratios_Projected.xlsx')) != TRUE){
download.file('https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationprojections/datasets/populationofstatepensionageandworkingageandoldagedependencyratiosforlocalauthoritiesandregionsinengland/2018based/2018snppprincipaloadr.xlsx', paste0(local_directory, '/2018_based_high_level_SPA_WA_Dependency_Ratios_Projected.xlsx'),
              mode = 'wb')
}

wsx_high_level_projections <- read_excel("//chi_nas_prod2.corporate.westsussex.gov.uk/groups2.bu/Public Health Directorate/PH Research Unit/Adult Social Care/Data/2018_based_high_level_SPA_WA_Dependency_Ratios_Projected.xlsx",
                                         sheet = "Local authorities", 
                                         skip = 3) %>% 
  filter(area_name %in% wsx_areas) %>% 
  pivot_longer(cols = `2018`:ncol(.),
               names_to = 'Year',
               values_to = 'Population')

# We should use this rather than our own calculation, as the file takes into account the gradual changing of SPA

 # Old Age Dependency Ratio ####

# A common measure of ageing is the proportion of people aged 65 years and over. In England as a whole, this is projected to increase from 18.2% to 20.7% of the total population between mid-2018 and mid-2028. This is the continuation of a trend seen in the population estimates. The proportion is also projected to increase for all regions and local authorities, with the exception of Coventry where there is a slight reduction.

# An alternative measure of ageing is the old age dependency ratio (OADR), defined as the number of people of State Pension age (SPA) per 1,000 people of working age. Working age covers all people aged from 16 years up to State Pension age. Note that being over SPA does not necessarily mean someone is retired, nor are all working age people in employment.

# By 2028, the State Pension age will rise to age 67. As a result, the OADR in England is projected to fall from 293 in mid-2018 to 287 in mid-2028. However, at local authority level around a third of areas see a rise in OADR over this period.

# After mid-2028, almost all areas are projected to have an increasing OADR up to the end of the projection in mid-2043. This reflects the continued ageing of the population during a period in which no more rises in State Pension age are scheduled.

OADR_df <- wsx_high_level_projections %>% 
  filter(age_group == 'Old age dependency ratio') %>% 
  select(Area = area_name, Year, `State pension age` = Population) %>% 
  mutate(`Working age` = 1000) %>% 
  pivot_longer(cols = c('State pension age', 'Working age'),
               names_to = 'Group',
               values_to = 'Population') %>% 
  mutate(Group = factor(Group, levels = rev(c('State pension age', 'Working age'))))

Area_y = 'Adur'

attempt_1_waffle <- OADR_df %>% 
  # filter(Area == Area_y) %>%
  filter(Year %in% c(2018, 2020, 2022, 2027, 2032, 2037, 2042)) %>% 
  ggplot(aes(fill = Group, 
             values = Population/10)) +
  geom_waffle(color = "white",
              n_rows = 10, 
              flip = TRUE) +
  labs(title = paste0('Old age dependency ratio; West Sussex Districts & Boroughs'),
       subtitle = "Number of people of state pension age per 1,000 working age population",
       caption = 'Every area is standardised to have the same number of working age people (1,000).',
       x = "Year",
       y = "" ) +
  scale_x_discrete() + 
  scale_y_continuous(labels = NULL,
                     expand = c(0,0)) +
  scale_fill_manual(values = c('orange', 'maroon'),
                    name = 'Age') +
  facet_grid(Area ~ Year) +
  coord_equal() +
  waffle_theme() +
  guides(fill = guide_legend(nrow = 1))

png(paste0(output_directory,'/West Sussex OADR attempt 1.png'),
    width = 880,
    height = 1080,
    res = 200)
print(attempt_1_waffle)
dev.off()

attempt_2_waffle <- OADR_df %>% 
  filter(Year %in% c(2018, 2020, 2022, 2027, 2032, 2037, 2042)) %>%
  filter(Group != 'Working age') %>% 
  ggplot(aes(fill = Group, 
             values = Population/10)) +
  geom_waffle(color = "white",
              n_rows = 10, 
              flip = TRUE) +
  labs(title = paste0('Old age dependency ratio; West Sussex Districts & Boroughs'),
       subtitle = "Each square represents 10 people.",
       caption = 'The old age dependency ratio is the number of people of State Pension age per 1,000 people of working age.\nEvery area and year is standardised to have the same number of working age people (1,000).\nThis allows us to compare across areas as if they had the same number of people.\nIn Arun, in 2018 there were estimated to be 400 people of State Pension age for every 1,000 population of working age.\nThis is roughly two older people for every five working age people. However, in Crawley, there are far fewer older people,\nrelative to the overall working age population, and the ratio there was 200 per 1,000;\nor one state pension age person for every five working age people.\n\nIn all areas, the ratio is set to increase over the next two decades with Arun and Chichester\nexpected to have one State Pension aged person for every two working aged population.',
       x = '',
       y = '' ) +
  scale_x_discrete() + 
  scale_y_continuous(labels = NULL,
                     expand = c(0,0)) +
  scale_fill_manual(values = c('maroon'),
                    name = 'Age') +
  facet_grid(Area ~ Year,
             space = 'free') +
  coord_equal(ylim = c(0,9)) +
  waffle_theme() +
  guides(fill = guide_legend(nrow = 1))

png(paste0(output_directory,'/West Sussex OADR attempt 2.png'),
    width = 880,
    height = 1080,
    res = 200)
print(attempt_2_waffle)
dev.off()

OADR_wsx <- wsx_high_level_projections %>% 
  filter(age_group != 'Old age dependency ratio') %>% 
  group_by(Year, age_group) %>%
  summarise(Population = sum(Population, na.rm = TRUE),
            Area = 'West Sussex') %>% 
  pivot_wider(names_from = age_group,
              values_from = Population) %>% 
  mutate(OADR = `State Pension age` / `Working age` * 1000)

attempt_3_waffle <- OADR_wsx %>% 
  # filter(Year %in% c(2018, 2020, 2022, 2027, 2032, 2037, 2042)) %>%
  ggplot(aes(fill = 'State Pension age', 
             values = OADR/10)) +
  geom_waffle(color = "white",
              n_rows = 10, 
              flip = TRUE) +
  labs(title = paste0('Old age dependency ratio; West Sussex'),
       subtitle = "Each square represents 10 people.",
       caption = 'The old age dependency ratio is the number of people of State Pension age per 1,000 people of working age.\nEvery area and year is standardised to have the same number of working age people (1,000).\nThis allows us to compare across areas as if they had the same number of people.\nIn Arun, in 2018 there were estimated to be 400 people of State Pension age for every 1,000 population of working age.\nThis is roughly two older people for every five working age people. However, in Crawley, there are far fewer older people,\nrelative to the overall working age population, and the ratio there was 200 per 1,000;\nor one state pension age person for every five working age people.\n\nIn all areas, the ratio is set to increase over the next two decades with Arun and Chichester\nexpected to have one State Pension aged person for every two working aged population.',
       x = '',
       y = '' ) +
  scale_x_discrete() + 
  scale_y_continuous(labels = NULL,
                     expand = c(0,0)) +
  scale_fill_manual(values = c('maroon'),
                    name = 'Age') +
  facet_wrap(~ Year) +
  coord_equal(ylim = c(0,9)) +
  waffle_theme() 

png(paste0(output_directory,'/West Sussex OADR attempt 3.png'),
    width = 880,
    height = 1080,
    res = 200)
print(attempt_3_waffle)
dev.off()

# Alternative display accounting for population size (iceberg plot)

alt_df <- wsx_high_level_projections %>% 
  filter(age_group != 'Old age dependency ratio') %>% 
  select(Area = area_name, Year, Group = age_group, Population)

summary(alt_df$Population/1000)

# Every square would need to represent 1,000 people





# Projection variants ####
# All statistics in this bulletin are from our main (principal) subnational projection. However, we have also published a range of variant projections. These include: a high international migration variant, a low international migration variant, an alternative internal migration variant, a 10-year migration variant

# The high and low international migration variants assume either higher or lower levels of net international migration to England as a whole, but the proportional distribution at local authority level remains the same. The result is that all areas see correspondingly higher or lower population totals, with areas that have high levels of international migration in the principal projection (especially parts of London) seeing the greatest difference.

# There is often debate around how many years of data should be used to inform the projected population change at local level. In general we use five years of data, but we have used just two years of data for internal migration in the 2018-based projections. This is because we only have two years of data for internal migration available using our current method.

# We have produced the alternative internal migration variant, which uses five years of data for internal migration: two using the new method and three using the old method. We have also produced a 10-year migration variant where all migration trends (internal, cross-border and international) are based on 10 years of data.
 
# The pros and cons of using different numbers of years of input data are complex. More information and a comparison of the results of the principal projection, the alternative internal migration variant and the 10-year migration variant are discussed in our article on the Impact of different migration trend lengths. 

# sub ltla ####
# Sub local authority projections do not exist in the public domain, there is an application called POPGROUP (Which I think WSCC has subscribed to in the past (we've got some documentation from a legacy version on the shared drives)) but I dont have the latest version. so we should stick to LTLA for now

# Adult Social Care Outcomes Framework - current ####

ASCOF_open <- read_csv('https://files.digital.nhs.uk/65/E7A20E/meas-from-asc-of-eng-1920-open-data-csv.csv') %>% 
  select(Area = `Geographical Description`, ASCOF_ID = `ASCOF Measure Code`, Type = `Measure Type`, Cohort = `Disaggregation Level`, Value = `Measure Value`, Indicator_name = `Measure Group Description`) %>% 
  filter(Area %in% c('West Sussex')) %>% 
  pivot_wider(names_from = Type,
              values_from = Value)

unique(ASCOF_open$Indicator_name)

# Load in assumptions ####
Assumptions_df <- read_csv('https://raw.githubusercontent.com/psychty/projecting_older_adult_assumptions/main/Data/Assumptions_lookup_2022.csv')

Local_assumptions <- Assumptions_df %>% 
  filter(Application == 'Local prevalence applied')

# We have two to get from the census.
unique(Local_assumptions$Indicator)
unique(Local_assumptions$Source)

# https://www.nomisweb.co.uk/api/v01/dataset/NM_674_1.data.csv?date=latest&geography=1941962888&c_sex=1,2&c_age=6...8&c_disability=1...3&c_health=0&measures=20100

# https://www.nomisweb.co.uk/api/v01/dataset/NM_674_1.data.csv?date=latest&geography=1941962888,1946157339...1946157345&c_sex=0&c_age=0&c_disability=0&c_health=0&measures=20100

WSx_DC3302 <- nomis_get_data(id = 'NM_674_1',
               time = 'latest',
               sex = '1,2',
               measures = '20100',
               c_disability = '1...3',
               c_health = '0',
               c_age = '6...8',
               geography = "1941962888,1946157339...1946157345") %>% 
  select(Year = DATE, Area = GEOGRAPHY_NAME, Area_code = GEOGRAPHY_CODE, Age_group = C_AGE_NAME, Sex = C_SEX_NAME, Limitation = C_DISABILITY_NAME, Population = OBS_VALUE) %>% 
  mutate(Age_group = ifelse(Age_group == 'Age 65 to 74', '65-74 years', ifelse(Age_group == 'Age 75 to 84', '75-84 years', ifelse(Age_group == 'Age 85 and over', '85+ years', NA)))) %>% 
  group_by(Area_code, Area, Age_group, Sex) %>% 
  mutate(Proportion = Population / sum(Population))

# WSx_DC4210EWL<- nomis_get_data(id = 'NM_784_1',
#                                time = 'latest',
#                                c_sex = '0',
#                                measures = '20100',
#                                c_cectmcews11 = '8,9,15,16',
#                                position_age = '7...9',
#                                geography = "1107296282,1132462324...1132462330") %>% 
#   select(Year = DATE, Area = GEOGRAPHY_NAME, Area_code = GEOGRAPHY_CODE, Age_group = POSITION_AGE_NAME, Living_status = C_CECTMCEWS11_NAME, Sex = C_SEX_NAME, Population = OBS_VALUE) %>%
#   mutate(Living_status = ifelse(Living_status %in% c('Medical and care establishment: Local Authority: Care home with nursing', 'Medical and care establishment: Local Authority: Care home without nursing'), 'Local authority Care home', ifelse(Living_status %in% c('Medical and care establishment: Other: Care home with nursing', 'Medical and care establishment: Other: Care home without nursing'), 'Non-LA Care home', NA))) %>% 
#   group_by(Year, Area_code, Area, Age_group, Sex, Living_status) %>%
#   summarise(Population = sum(Population, na.rm = TRUE)) %>% 
#   pivot_wider(names_from = 'Living_status',
#               values_from = 'Population') 
# # This is not going to tell us the proportion of the population in care homes is it? by itself? do we need the census population for these ages as a denominator? It suggests there were only six local authority care home residents in Adur in 2011.

# https://www.nomisweb.co.uk/api/v01/dataset/NM_784_1.data.csv?date=latest&geography=1107296282,1132462324...1132462330&c_sex=0&c_cectmcews11=0,8,9,15,16&position_age=0,7...9&measures=20100


# Applying assumptions to population estimates 

# create a dataframe for each assumption #

unique(Assumptions_df$Indicator)

Assumptions_df %>% 
  write.csv(., paste0(local_directory, '/Assumptions_lookup_2022.csv'),
            row.names = FALSE)

Assumptions_df %>% 
  select(Indicator, Source, Source_year) %>% 
  unique() %>% 
  write.csv(., paste0(local_directory, '/summary_indicators.csv'))

assumption_01 <- Assumptions_df %>% 
  filter(Indicator == 'Living alone')

applied_df_01 <- mye_df %>% 
  mutate(Type = 'Estimates') %>% 
  bind_rows(wsx_projections) %>% 
  filter(Age_group %in% c('65-69 years', '70-74 years', '75-79 years', '80-84 years', '85-89 years',
                          '90+ years')) %>% 
  left_join(assumption_01, by = c('Sex', 'Age_group')) %>% 
  mutate(Estimated_population = Population * Assumption)

assumption_02 <- Assumptions_df %>% 
  filter(Indicator == 'Needing support with domestic tasks')

applied_df_02 <- mye_df %>% 
  mutate(Type = 'Estimates') %>% 
  bind_rows(wsx_projections) %>% 
  filter(Age_group %in% c('65-69 years', '70-74 years', '75-79 years', '80-84 years', '85-89 years',
                          '90+ years')) %>% 
  left_join(assumption_02, by = c('Sex', 'Age_group')) %>% 
  mutate(Estimated_population = Population * Assumption)

assumption_03 <- Assumptions_df %>% 
  filter(Indicator == 'Needing support with self-care')

applied_df_03 <- mye_df %>% 
  mutate(Type = 'Estimates') %>% 
  bind_rows(wsx_projections) %>% 
  filter(Age_group %in% c('65-69 years', '70-74 years', '75-79 years', '80-84 years', '85-89 years',
                          '90+ years')) %>% 
  left_join(assumption_03, by = c('Sex', 'Age_group')) %>% 
  mutate(Estimated_population = Population * Assumption)

assumption_04_info <- Assumptions_df %>% 
  filter(Indicator == 'Limiting long term illness') %>% 
  select(`Indicator origin`, Application, Source, Source_year, Notes) %>% 
  unique()

assumption_04 <- WSx_DC3302 %>% 
  ungroup() %>% 
  select(Area, Age_group, Sex, Indicator = Limitation, Assumption = Proportion) %>%
  filter(Indicator != 'Day-to-day activities not limited') %>% 
  bind_cols(assumption_04_info)
  
unique(assumption_04$Age_group)

applied_df_04 <- mye_df %>% 
  mutate(Type = 'Estimates') %>% 
  bind_rows(wsx_projections) %>% 
  filter(Age_group %in% c('65-69 years', '70-74 years', '75-79 years', '80-84 years', '85-89 years',
                          '90+ years')) %>% 
  mutate(Age_group = ifelse(Age_group %in% c('65-69 years', '70-74 years'), '65-74 years', ifelse(Age_group %in% c('75-79 years', '80-84 years'), '75-84 years', ifelse(Age_group %in% c('85-89 years', '90+ years'), '85+ years', NA)))) %>% 
  group_by(Area, Year, Sex, Age_group, Type) %>% 
  summarise(Population = sum(Population, na.rm = TRUE)) %>% 
  left_join(assumption_04, by = c('Area','Sex', 'Age_group')) %>% 
  mutate(Estimated_population = Population * Assumption)

unique(Assumptions_df$Indicator)

# TODO deal with the 90 and 95+ assumptions

assumption_05 <- Assumptions_df %>% 
  filter(Indicator == 'Dementia')

applied_df_05 <- mye_df %>% 
  mutate(Type = 'Estimates') %>% 
  bind_rows(wsx_projections) %>% 
  filter(Age_group %in% c('65-69 years', '70-74 years', '75-79 years', '80-84 years', '85-89 years',
                          '90+ years')) %>% 
  left_join(assumption_03, by = c('Sex', 'Age_group')) %>% 
  mutate(Estimated_population = Population * Assumption)