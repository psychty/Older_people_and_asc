
# Older people tend to move out of urban cities to rural areas.

# However adults moving to new areas to grow older often have to start anew with social support networks; which older adults who have lived there for longer will already have established. 

# So do we need to consider more than just those areas with more older people. Do some areas of West Sussex (or wider) have more older people flowing in. 

# Of course some people might move to a new area to be closer to family so this isn't fool proof and its also not particularly granular (only down to LTLA).

# this says census data but i cant find it
# https://www.solihull.gov.uk/sites/default/files/2023-12/Older-People-Internal-Migration.pdf

# Census 2021 is the latest available data, though is it representative of typical changes due to the covid lockdowns? maybe we need to look at the previous year too.

# two parts zipped folders 
# https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/migrationwithintheuk/datasets/internalmigrationbyoriginanddestinationlocalauthoritiessexandsingleyearofagedetailedestimatesdataset/yearendingjune2020part1/detailedestimates2020on2021laspt1.zip

#https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/migrationwithintheuk/datasets/internalmigrationbyoriginanddestinationlocalauthoritiessexandsingleyearofagedetailedestimatesdataset/yearendingjune2020part2/detailedestimates2020on2021laspt2.zip

# Loading some packages
packages <- c('easypackages','plyr', 'tidyr', 'ggplot2', 'dplyr', 'scales', 'readxl', 'readr', 'purrr', 'stringr', 'rgdal',  'geojsonio', 'jsonlite', 'viridis', 'nomisr', 'lemon', 'spdplyr', 'rgeos', "tmaptools", 'sp', 'sf', 'maptools', 'flextable', 'ggmap', 'grid', 'lemon', 'ggpol', 'httr', 'rvest','circlize', 'directlabels')

install.packages(setdiff(packages, rownames(installed.packages())))
easypackages::libraries(packages)

local_store <- '~/Repositories/Older_people_and_asc/Data'
output_store <- '~/Repositories/Older_people_and_asc/Outputs'

local_areas <- c('Adur', 'Arun', 'Chichester', 'Crawley', 'Horsham', 'Mid Sussex', 'Worthing')

# Lookup

if(file.exists(paste0(output_store, '/Area_to_region_lookup.csv')) != TRUE){
MSOA_region <- read_sf('https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/MSOA21_BUA22_LAD22_RGN22_EW_LU_v2/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson') %>% 
  st_drop_geometry() %>% 
  select(MSOA21CD, MSOA21NM, Area_code = LAD22CD, Area_name = LAD22NM, RGN22CD, RGN22NM) %>% 
  unique()

LAD_UTLA <- read_sf('https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/LTLA21_UTLA21_EW_LU_9bbac05558b74a88bda913ad5bf66917/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson') %>% 
  st_drop_geometry() %>% 
  select(Area_code = LTLA21CD, Area_name = LTLA21NM, UTLA_code = UTLA21CD, UTLA_name = UTLA21NM)

LAD_region <- MSOA_region %>% 
  select(Area_code, Area_name, RGN22CD, RGN22NM) %>% 
  left_join(LAD_UTLA, by = c('Area_code', 'Area_name')) %>% 
  unique() %>% 
  mutate(UTLA_code = ifelse(Area_name == 'Herefordshire', 'E06000019', ifelse(Area_name == 'Bristol', 'E06000023',ifelse(Area_name == 'Kingston upon Hull', 'E06000010', UTLA_code)))) %>% 
  mutate(UTLA_name = ifelse(Area_name == 'Herefordshire', 'Herefordshire',ifelse(Area_name == 'Bristol', 'Bristol',ifelse(Area_name == 'Kingston upon Hull', 'Kingston upon Hull', UTLA_name)))) %>% 
  select(Area_code, Area_name, UTLA_code, UTLA_name, RGN_code = RGN22CD, RGN_name = RGN22NM)

# We have some LTLA and UTLA here in the same dataframe, so lets make one long area_code to region lookup
area_lookup <- LAD_region %>% 
  select(Area_code, Area_name, RGN_code, RGN_name) %>% 
  unique() %>% 
  bind_rows(
    LAD_region %>% 
      select(Area_code = UTLA_code, Area_name = UTLA_name, RGN_code, RGN_name) %>% 
      unique()
  ) %>% 
  unique()

area_lookup %>% 
  write.csv(., paste0(output_store, '/Area_to_region_lookup.csv'), row.names = FALSE)
}

area_lookup <- read_csv(paste0(output_store, '/Area_to_region_lookup.csv'))


# Custom fonts ####
showtext::showtext_auto(TRUE)


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


# https://www.nomisweb.co.uk/sources/census_2021_od

# Overall migration ####
# download.file('https://www.nomisweb.co.uk/output/census/2021/odmg01ew.zip',
#               paste0(local_store, '/Census_origin_destination_migration.zip'),
#               mode = 'wb')
# unzip(paste0(local_store, '/Census_origin_destination_migration.zip'),
#       exdir = local_store)
# 
# msoa_origin_dest_df <- read_csv(paste0(local_store, '/ODMG01EW_MSOA.csv'))

# Rough stable LTLA population by age group Census 2021 ####

# This is from table TS009 - sex by single year of age
census_LTLA_raw_df <- nomis_get_data(id = 'NM_2029_1',
                                     time = 'latest', 
                                     c_sex = '0,1,2', # '1,2' would return males and females
                                     measures = '20100',
                                     c2021_age_92 = '1...91',
                                     geography = 'TYPE154') %>% 
  select(Area_code = GEOGRAPHY_CODE, Area_name = GEOGRAPHY_NAME, Year = DATE, Sex = C_SEX_NAME, Age = C2021_AGE_92_NAME, Population = OBS_VALUE) %>% 
  mutate(Sex = gsub('All persons', 'Persons', Sex)) 

census_UTLA_raw_df <- nomis_get_data(id = 'NM_2029_1',
                                     time = 'latest', 
                                     c_sex = '0,1,2', # '1,2' would return males and females
                                     measures = '20100',
                                     c2021_age_92 = '1...91',
                                     geography = 'TYPE155') %>% 
  select(Area_code = GEOGRAPHY_CODE, Area_name = GEOGRAPHY_NAME, Year = DATE, Sex = C_SEX_NAME, Age = C2021_AGE_92_NAME, Population = OBS_VALUE) %>% 
  mutate(Sex = gsub('All persons', 'Persons', Sex)) 

census_region_raw_df <- nomis_get_data(id = 'NM_2029_1',
                                        time = 'latest', 
                                        c_sex = '0,1,2', # '1,2' would return males and females
                                        measures = '20100',
                                        c2021_age_92 = '1...91',
                                        geography = 'TYPE480') %>% 
  select(Area_code = GEOGRAPHY_CODE, Area_name = GEOGRAPHY_NAME, Year = DATE, Sex = C_SEX_NAME, Age = C2021_AGE_92_NAME, Population = OBS_VALUE) %>% 
  mutate(Sex = gsub('All persons', 'Persons', Sex)) 

census_England_raw_df <- nomis_get_data(id = 'NM_2029_1',
                                     time = 'latest', 
                                     c_sex = '0,1,2', # '1,2' would return males and females
                                     measures = '20100',
                                     c2021_age_92 = '1...91',
                                     geography = 'TYPE499') %>% 
  select(Area_code = GEOGRAPHY_CODE, Area_name = GEOGRAPHY_NAME, Year = DATE, Sex = C_SEX_NAME, Age = C2021_AGE_92_NAME, Population = OBS_VALUE) %>% 
  mutate(Sex = gsub('All persons', 'Persons', Sex)) 

census_population_raw_df <- census_England_raw_df %>% 
  bind_rows(census_LTLA_raw_df) %>% 
  bind_rows(census_UTLA_raw_df) %>% 
  bind_rows(census_region_raw_df) %>%  
  unique() %>% # This is important because the Unitary authorities will be downloaded twice (once from the LTLA, and once from the UTLA query)
  mutate(Age = as.numeric(ifelse(Age == 'Aged under 1 year', 0, ifelse(Age == 'Aged 90 years and over', 90, gsub(' year','', gsub(' years', '', gsub('Aged ', '', Age))))))) %>% 
  mutate(Age_group = factor(ifelse(Age < 1, 'Under 12 months', ifelse(Age <= 4, "1-4 years", ifelse(Age <= 9, "5-9 years", ifelse(Age <= 14, "10-14 years", ifelse(Age <= 19, "15-19 years", ifelse(Age <= 24, "20-24 years", ifelse(Age <= 29, "25-29 years",ifelse(Age <= 34, "30-34 years", ifelse(Age <= 39, "35-39 years",ifelse(Age <= 44, "40-44 years", ifelse(Age <= 49, "45-49 years",ifelse(Age <= 54, "50-54 years", ifelse(Age <= 59, "55-59 years",ifelse(Age <= 64, "60-64 years", ifelse(Age <= 69, "65-69 years",ifelse(Age <= 74, "70-74 years", ifelse(Age <= 79, "75-79 years",ifelse(Age <= 84, "80-84 years", "85+ years")))))))))))))))))), levels = c('Under 12 months','1-4 years', '5-9 years','10-14 years','15-19 years','20-24 years','25-29 years','30-34 years','35-39 years','40-44 years','45-49 years','50-54 years','55-59 years','60-64 years','65-69 years','70-74 years','75-79 years','80-84 years','85+ years'))) 

rm(census_England_raw_df, census_LTLA_raw_df, census_UTLA_raw_df, census_region_raw_df)

census_population_five_years <- census_population_raw_df %>% 
  group_by(Area_code, Area_name, Year, Sex, Age_group) %>% 
  summarise(Population = sum(Population))

census_total_population <- census_population_raw_df %>% 
  group_by(Area_code, Area_name, Sex, Year) %>% 
  summarise(Population = sum(Population),
            Age = 'All ages')

census_over_65_population <- census_population_five_years %>% 
  # filter(Sex == 'Persons') %>% 
  filter(Age_group %in% c('65-69 years','70-74 years','75-79 years','80-84 years','85+ years')) %>% 
  group_by(Area_code, Area_name, Sex, Year) %>% 
  summarise(Population = sum(Population),
            Age = '65+ years')

census_over_85_population <- census_population_five_years %>% 
  # filter(Sex == 'Persons') %>% 
  filter(Age_group %in% c('85+ years')) %>% 
  group_by(Area_code, Area_name, Sex, Year) %>% 
  summarise(Population = sum(Population),
            Age = '85+ years')

total_table <- census_total_population %>% 
  bind_rows(census_over_65_population) %>% 
  bind_rows(census_over_85_population) %>% 
  pivot_wider(names_from = 'Age',
              values_from = 'Population'
              ) %>% 
  mutate(Proportion_65 = `65+ years` / `All ages`,
         Proportion_85 = `85+ years` / `All ages`) %>% 
  ungroup()

table_1 <- total_table %>% 
  filter(Area_name %in% c(local_areas, 'West Sussex', 'South East', 'England')) %>% 
  mutate(Area_name = factor(Area_name, levels = c('West Sussex', local_areas, 'South East', 'England'))) %>% 
  arrange(Area_name) %>% 
  filter(Sex == 'Persons') %>%
  mutate(`All ages` = paste0(format(`All ages`, big.mark = ',', trim = TRUE))) %>% 
  mutate(`65+ years` = paste0(format(`65+ years`, big.mark = ',', trim = TRUE), ' (', round(Proportion_65 *100, 1), '%)')) %>% 
  mutate(`85+ years` = paste0(format(`85+ years`, big.mark = ',', trim = TRUE), ' (', round(Proportion_85 *100, 1), '%)')) %>% 
  select(Area = Area_name, `All ages`, `65+ years`, `85+ years`) %>% 
  flextable()

table_1

# by age address moves Census 2021 ####

# This dataset provides Census 2021 estimates on all usual residents aged 1 year and over in England and Wales who were living at a different address one year before the Census. The estimates classify people currently resident in each Middle layer Super Output Area (MSOA), or higher area by the MSOA, or higher area in which they were resident in one year before the Census. The estimates are as at Census Day, 21 March 2021. People resident outside of the UK one year before the Census are counted in the category "Outside UK".     

# If the LTLA OD file does not exist then download the zipped file and unzip it
if(file.exists(paste0(local_store, '/ODMG02EW_LTLA.csv')) != TRUE){
download.file('https://www.nomisweb.co.uk/output/census/2021/odmg02ew.zip',
              paste0(local_store, '/Census_origin_destination_migration_age.zip'),
              mode = 'wb')
unzip(paste0(local_store, '/Census_origin_destination_migration_age.zip'),
      exdir = local_store)
}

# Read in origin_destination df
origin_dest_df <- read_csv(paste0(local_store, '/ODMG02EW_LTLA.csv')) %>% 
  select(Origin_area_code = "Migrant LTLA one year ago code", Origin_area_name = "Migrant LTLA one year ago label", Area_code = "Lower tier local authorities code", Area_name = "Lower tier local authorities label", Age =  "Age (23 categories) label", Moves = Count) %>% 
    bind_rows(read_csv(paste0(local_store, '/ODMG02EW_UTLA.csv')) %>% 
                select(Origin_area_code = "Migrant UTLA one year ago code", Origin_area_name = "Migrant UTLA one year ago label", Area_code = "Upper tier local authorities code", Area_name = "Upper tier local authorities label", Age =  "Age (23 categories) label", Moves = Count)) %>% 
  unique() %>% # The unitary authorities will be counted twice, so we can remove duplicated rows. 
  mutate(Age_group = factor(ifelse(Age %in% c("Aged 1 to 2 years", "Aged 3 to 4 years"), '1-4 years', ifelse(Age %in% c("Aged 5 to 7 years", "Aged 8 to 9 years"), '5-9 years', ifelse(Age %in% c("Aged 10 to 14 years"), '10-14 years', ifelse(Age %in% c('Aged 15 years', 'Aged 16 to 17 years', 'Aged 18 to 19 years'), '15-19 years', ifelse(Age %in% c('Aged 20 to 24 years'), '20-24 years', ifelse(Age %in% c('Aged 25 to 29 years'), '25-29 years', ifelse(Age %in% c('Aged 30 to 34 years'), '30-34 years', ifelse(Age %in% c('Aged 35 to 39 years'), '35-39 years', ifelse(Age %in% c('Aged 40 to 44 years'), '40-44 years', ifelse(Age %in% c('Aged 45 to 49 years'), '45-49 years', ifelse(Age %in% c('Aged 50 to 54 years'), '50-54 years', ifelse( Age %in% c('Aged 55 to 59 years'), '55-59 years', ifelse(Age %in% c('Aged 60 to 64 years'), '60-64 years', ifelse(Age %in% c('Aged 65 to 69 years'), '65-69 years', ifelse(Age %in% c('Aged 70 to 74 years'), '70-74 years', ifelse(Age %in% c('Aged 75 to 79 years'), '75-79 years', ifelse(Age %in% c('Aged 80 to 84 years'),'80-84 years', ifelse(Age %in% c('Aged 85 years and over'), '85+ years', NA)))))))))))))))))), levels = c('1-4 years', '5-9 years', '10-14 years', '15-19 years', '20-24 years', '25-29 years', '30-34 years', '35-39 years', '40-44 years', '45-49 years', '50-54 years', '55-59 years', '60-64 years', '65-69 years', '70-74 years', '75-79 years', '80-84 years', '85+ years'))) %>% 
  mutate(Age_broad =ifelse(Age %in% c("Aged 1 to 2 years", "Aged 3 to 4 years","Aged 5 to 7 years","Aged 8 to 9 years", "Aged 10 to 14 years",  "Aged 15 years", "Aged 16 to 17 years", "Aged 18 to 19 years", "Aged 20 to 24 years", "Aged 25 to 29 years", "Aged 30 to 34 years", "Aged 35 to 39 years", "Aged 40 to 44 years", "Aged 45 to 49 years", "Aged 50 to 54 years", "Aged 55 to 59 years", "Aged 60 to 64 years"), '1-64 years', ifelse(Age %in% c('Aged 65 to 69 years', 'Aged 70 to 74 years', 'Aged 75 to 79 years', 'Aged 80 to 84 years', 'Aged 85 years and over'), '65+ years',  NA))) %>% 
  mutate(Broad_origin = ifelse(Origin_area_name == 'Does not apply', 'Not moved', ifelse(Origin_area_name == Area_name, 'Moved within local authority', ifelse(Origin_area_name %in% local_areas, 'Moved from another West Sussex area', 'Moved from elsewhere')))) %>% 
  left_join(area_lookup[c('Area_code','Area_name', 'RGN_name')], by = c('Origin_area_name' = 'Area_name', 'Origin_area_code' = 'Area_code')) %>% 
  rename(Origin_region_name = 'RGN_name') %>% 
  mutate(Origin_region_name = ifelse(str_detect(Origin_area_code, '^S'), 'Scotland', ifelse(str_detect(Origin_area_code, '^N'), 'Northern Ireland', ifelse(Origin_area_code == '999999999', 'Outside of UK', ifelse(Origin_area_code == '-8', 'Not moved', Origin_region_name))))) %>% 
  left_join(area_lookup[c('Area_code','Area_name', 'RGN_name')], by = c('Area_name', 'Area_code'))

moves_age <- origin_dest_df %>% 
  group_by(Origin_area_code, Origin_area_name, Origin_region_name, Area_code, Area_name, RGN_name, Age_group, Broad_origin) %>% 
  summarise(Moves = sum(Moves)) %>% 
  ungroup()

moves_broad_age <- origin_dest_df %>% 
  group_by(Origin_area_code, Origin_area_name, Origin_region_name, Area_code, Area_name, RGN_name, Age_broad, Broad_origin) %>% 
  summarise(Moves = sum(Moves)) %>% 
  ungroup()

i = 1
Area_x = local_areas[i]

area_focus_x <- origin_dest_df %>% 
  filter(Area_name == Area_x)

area_focus_x %>% 
  group_by(Area_name, Broad_origin) %>% 
  summarise(Moves = sum(Moves))

area_focus_x %>% 
  group_by(Area_name) %>% 
  summarise(Moves = sum(Moves))

census_population_raw_df %>% 
  filter(Area_name == Area_x) %>% 
  filter(Sex == 'Persons') %>% 
  filter(Age != 0) %>% 
  summarise(Population = sum(Population))

# I think this is close enough, the moves counts people aged 1+
area_focus_x %>% 
  filter(Age %in% c('Aged 65 to 69 years', 'Aged 70 to 74 years', 'Aged 75 to 79 years', 'Aged 80 to 84 years', 'Aged 85 years and over')) %>% 
  group_by(Area_name) %>% 
  summarise(Moves = sum(Moves))
  
census_population_raw_df %>% 
  filter(Area_name == Area_x) %>% 
  filter(Sex == 'Persons') %>% 
  filter(Age >= 65) %>% 
  group_by(Area_name) %>% 
  summarise(Population = sum(Population))

# Similarly with the 65+ the census population and number of moves is close enough.

# Of the people who moved into the area in the last year, how many were aged 65+
area_focus_x %>% 
  filter(Broad_origin != 'Not moved') %>% 
  group_by(Area_name, Age_broad) %>% 
  summarise(Moves = sum(Moves)) %>% 
  mutate(Proportion = Moves/sum(Moves))

# Which Local Authority has the highest number of 65+ moving into the area, from non-neighbouring areas

# generalise this to capture WSx areas
table_2a <- origin_dest_df %>% 
  filter(Area_name %in% c(local_areas, 'West Sussex')) %>% 
  mutate(Area_name = factor(Area_name, levels = c('West Sussex', local_areas))) %>% 
  filter(Broad_origin != 'Not moved') %>% 
  filter(Broad_origin != 'Moved within local authority') %>% 
  # filter(Broad_origin != 'Moved from another West Sussex area') %>% 
  group_by(Area_name, Age_broad) %>% 
  summarise(Moves = sum(Moves)) %>% 
  pivot_wider(names_from = 'Age_broad',
              values_from = 'Moves') %>%
  mutate(`Total moves into area` = `1-64 years` + `65+ years`) %>% 
  mutate(Proportion_1_64 = `1-64 years` / `Total moves into area`) %>% 
  mutate(Proportion_65 = `65+ years` / `Total moves into area`)  
  
table_2a_flex <- table_2a %>% 
  mutate(`Total moves into area` = paste0(format(`Total moves into area`, big.mark = ',', trim = TRUE))) %>% 
  mutate(`1-64 years` = paste0(format(`1-64 years`, big.mark = ',', trim = TRUE), ' (', round(Proportion_1_64 *100, 1), '%)')) %>% 
  mutate(`65+ years` = paste0(format(`65+ years`, big.mark = ',', trim = TRUE), ' (', round(Proportion_65 *100, 1), '%)')) %>% 
  select(Area = Area_name, `Total moves into area`,`1-64 years`, `65+ years`) %>% 
  flextable()


# This excludes those who moved in the last year but from another address in the same local authority (e.g. inflows capture those becoming a new resident of responsibility for the local authority).As you can see, the sum of the number of people moving into each of the lower tier local authority is greater than the number of people moving into West Sussex from outside of the county suggesting that a large number of moves between the local authorities of West Sussex rather than new people into the county.

table_2b <- origin_dest_df %>% 
  filter(Area_name %in% c(local_areas, 'West Sussex')) %>% 
  mutate(Area_name = factor(Area_name, levels = c('West Sussex', local_areas))) %>% 
  filter(Broad_origin != 'Not moved') %>% 
  filter(Broad_origin != 'Moved within local authority') %>% 
  filter(Broad_origin != 'Moved from another West Sussex area') %>% 
  group_by(Area_name, Age_broad) %>% 
  summarise(Moves = sum(Moves)) %>% 
  pivot_wider(names_from = 'Age_broad',
              values_from = 'Moves') %>%
  mutate(`Total moves into area` = `1-64 years` + `65+ years`) %>% 
  mutate(Proportion_1_64 = `1-64 years` / `Total moves into area`) %>% 
  mutate(Proportion_65 = `65+ years` / `Total moves into area`)

table_2b_flex <- table_2b %>% 
  mutate(`Total moves into area` = paste0(format(`Total moves into area`, big.mark = ',', trim = TRUE))) %>% 
  mutate(`1-64 years` = paste0(format(`1-64 years`, big.mark = ',', trim = TRUE), ' (', round(Proportion_1_64 *100, 1), '%)')) %>% 
  mutate(`65+ years` = paste0(format(`65+ years`, big.mark = ',', trim = TRUE), ' (', round(Proportion_65 *100, 1), '%)')) %>% 
  select(Area = Area_name, `Total moves into area`,`1-64 years`, `65+ years`) %>% 
  flextable()

table_2a %>% 
  select(Area_name, Moves_2a = 'Total moves into area', Moves_2a_65 = '65+ years') %>% 
  left_join(table_2b %>% 
              select(Area_name, Moves_2b = 'Total moves into area', Moves_2b_65 = '65+ years'), by = 'Area_name') %>% 
  mutate(Proportion_in = (Moves_2a - Moves_2b) / Moves_2a) %>% 
  mutate(New_to_county = Moves_2a - Moves_2b) %>% 
  mutate(New_to_county_65 = Moves_2a_65 - Moves_2b_65) %>% 
  mutate(Proportion_in_65 = (Moves_2a_65 - Moves_2b_65) / Moves_2a_65) %>% 
  select(Area = Area_name, Moves_2a, Moves_2b, New_to_county, Proportion_in, Moves_2a_65, Moves_2b_65, New_to_county_65, Proportion_in_65)

# Table 2b also excludes moves at lower tier local authority level from other local authorities within West Sussex. Comparing the two tables, you can see that Arun has almost 8,000 moves into the local authority area, with 3,000 (38%) of these coming from outside of West Sussex county. Similarly, Worthing has a total of 5,700 moves into the area (from outside Worthing) in the year leading to Census day 2021; and around 2,000 (34%) of these were from other local authorities in West Sussex. However, for Chichester moves into the local authority from other parts of West Sussex account for just 16% of moves (1,500 of 9,300 moves) and for Crawley it is slightly higher at 17% (980 new to county of 9,300 new to Crawley). Some of the longer distance moves for Chichester might be explained by the university campus and incoming students staying in residential halls or other student accomodation.

# However, for those aged 65 and over, the pattern across the county is the same, and more pronounced. For Worthing, 44% (370 of 660) over 65s moving to the local authority originated outside of West Sussex compared to 18% of over 65s new to Chichester who are also new to West Sussex. 

# Of course, we do not know the history of moves for individuals (some may have lived in West Sussex for most of their lives, gone away and then returned), and we don't know the reasons for moving recently. Nor do we know if family and friends are in place where movers are going to. It is not unreasonable, however, to suggest that for a large number of these movers, they will be going to an area where they do not have established social contacts and crucially informal support for activities of daily living, such as shopping or social support. 

# inflow and outflow by age group
# Again this excludes people who moved within a local authority, and at West Sussex level
area_inflow <- origin_dest_df %>% 
  filter(Broad_origin != 'Not moved') %>%
  filter(Broad_origin != 'Moved within local authority') %>% 
  filter(Area_name %in% c(local_areas, 'West Sussex')) %>% 
  group_by(Area_name, Age_group) %>% 
  summarise(Inflow = sum(Moves))

area_outflow <- origin_dest_df %>% 
  filter(Broad_origin != 'Not moved') %>%
  filter(Broad_origin != 'Moved within local authority') %>% 
  filter(Origin_area_name %in% c(local_areas, 'West Sussex')) %>% 
  group_by(Origin_area_name, Age_group) %>% 
  summarise(Outflow = sum(Moves)) %>% 
  select(Area_name = Origin_area_name, Age_group, Outflow)

area_flow <- area_inflow %>% 
  left_join(area_outflow, by = c('Area_name', 'Age_group')) %>% 
  mutate(Net_migration = Inflow - Outflow,
         Migration_turnover = Inflow + Outflow) %>% 
  mutate(Sex = 'Persons') %>% 
  left_join(census_population_five_years[c('Area_name', 'Sex', 'Age_group', 'Population')], by = c('Area_name', 'Sex', 'Age_group')) %>% 
  mutate(Turnover_proportion = Inflow / Population)

# This is absolute numbers, are the same areas experiencing a higher migration turnover as a proportion of its older population

area_flow %>%
  filter(Age_group %in% c('65-69 years', '70-74 years', '75-79 years', '80-84 years', '85+ years')) %>% 
  group_by(Area_name, Sex) %>% 
  summarise(Inflow = sum(Inflow),
            Outflow = sum(Outflow),
            Population = sum(Population)) %>% 
  mutate(Age_group = '65+ years') %>% 
  mutate(Net_migration = Inflow - Outflow,
         Migration_turnover = Inflow + Outflow) %>% 
  mutate(Turnover_proportion = paste0(round(Inflow / Population *100, 1), '%')) %>% 
  select(Area_name, Age_group, Inflow, Outflow, Net_migration, Migration_turnover, Sex, Population, Turnover_proportion) %>%   flextable()

# Gives an idea of the scale we're talking about, new people aged 65+ into the county represent 1.8% of the population of older people in West Sussex; ranging from 1.3% in Crawley to 3% in Chichester.

# Chordplot ####

if(file.exists(paste0(output_store, '/Age_65_plus_migration_flow_raw.svg')) != TRUE){


Local_flow_total <- origin_dest_df %>% 
  filter(Area_name == 'West Sussex' | Origin_area_name == 'West Sussex') %>%
  filter(Broad_origin != 'Not moved') %>% 
  filter(Broad_origin != 'Moved within local authority') %>% 
  mutate(InRegion_name = ifelse(Area_name == 'West Sussex', 'West Sussex', RGN_name)) %>% 
  mutate(OutRegion_name = ifelse(Origin_area_name == 'West Sussex', 'West Sussex', Origin_region_name)) %>% 
  group_by(InRegion_name, OutRegion_name) %>% 
  summarise(Moves = sum(Moves)) %>% 
  filter(OutRegion_name != InRegion_name) # Remove moves within area

Local_flow_total %>% 
  group_by(Area = OutRegion_name) %>% 
  summarise(Moves = sum(Moves)) %>% 
  bind_rows(Local_flow_total %>% 
              group_by(Area = InRegion_name) %>% 
              summarise(Moves = sum(Moves))) %>% 
  group_by(Area) %>% 
  summarise(Moves = sum(Moves))

# In the year to Census day, there were 16,655 moves from the South East region (excluding West Sussex itself), into the county. There were slightly fewer people who left West Sussex (12,468) to live elsewhere in the South East region.
Local_flow_total %>% 
  filter(OutRegion_name == 'South East')

Local_flow_total %>% 
  filter(InRegion_name == 'South East')

Local_flow_total <- Local_flow_total %>% 
  select(OutRegion_name, InRegion_name, Moves)


# Transparency (or highlighting areas) is controlled by adding a number between 0 and 99 to the end of the colour (adding 30 to every area except the one were interested in makes others 30% transparent).


Area_meta <- data.frame(Region = c("North East","North West","Yorkshire and The Humber", "East Midlands","West Midlands","East of England", "London", "South East", "South West", "Wales", "Scotland", "Northern Ireland", 'Outside of UK', 'West Sussex'), 
                        Order = seq(1,14,1), 
                        Colour = c("#bb5f70","#d3434b","#c96d34","#b58f48","#a8b23b","#588347","#5dbb61","#4db6c2","#6776c8","#a259c7","#c782bf","#cf4597", '#d3434b', '#1e4b7a'),
                        Label_l1 = c("North East","North West","Yorkshire and", "East Midlands","West Midlands","East of", "London", "South East", "South West", "Wales", "Scotland", "Northern", 'Outside of', 'West Sussex'), 
                        Label_l2 = c(NA,NA,"the Humber",NA,NA,"England",NA,NA,NA,NA,NA, "Ireland",'UK', '')) %>% 
  mutate(Region = as.character(Region)) %>% 
  mutate(Colour =  as.character(Colour)) %>% 
  mutate(Colour = ifelse(Region == "West Sussex", paste0(Colour, 30), paste0(Colour)))

svg(paste0(output_store, '/All_age_migration_flow_raw.svg'), width = 12)

circos.clear()
par(mar = rep(0, 4), 
    cex=1)

circos.par(start.degree = 90, 
           track.margin=c(-0.2, 0.2),
           gap.degree = 6, 
           points.overflow.warning = FALSE)

# Create the plot itself
chordDiagram(Local_flow_total, 
             directional = 1, 
             order = Area_meta$Region, 
             grid.col = Area_meta$Colour, 
             annotationTrack = "grid", 
             transparency = 0.25,  
             annotationTrackHeight = c(0.05, 0.1), 
             direction.type = c("diffHeight", "arrows"), 
             link.arr.type = "big.arrow",
             diffHeight  = -0.02, 
             link.sort = TRUE, 
             link.largest.ontop = TRUE)

circos.track(
  track.index = 1, 
  bg.border = NA, # no borders are plotted on the track.
  panel.fun = function(x, y) {
    xlim = get.cell.meta.data("xlim") 
    sector.index = get.cell.meta.data("sector.index")
    
    Label_l1 = Area_meta %>% # collect matching name information from plot data frame (df1).
      filter(Region == sector.index) %>% 
      pull(Label_l1)
    
    Label_l2 = Area_meta %>% 
      filter(Region == sector.index) %>% 
      pull(Label_l2)
    
    # adds text from (reg1) either at y = 4 (if there is a second part of the name in reg2) or 3.
    circos.text(
      x = mean(xlim), # add text in the middle of the arch
      y = ifelse(is.na(Label_l2), 3, 4), 
      labels = Label_l1,
      facing = "bending", 
      cex = 0.5
    )
    
    circos.text(
      x = mean(xlim), 
      y = 2.75, 
      labels = Label_l2, # adds text (reg2).
      facing = "bending", 
      cex = 0.5
    )
    
    #add axis with major and minor ticks, without flipping the axis labels in the bottom half.
    circos.axis(h = "top",
                labels.cex = 0.5, 
                labels.niceFacing = FALSE, 
                labels.pos.adjust = FALSE,
                major.at = seq(0,70000,5000),
                minor.ticks = 4)
  })

text(x = .7,
     y = .9,
     pos = 4,
     cex = 0.6,
     labels = paste0("Units = moves;\nEach tick represents\n1,000 moves."))

text(x = -1, 
     y = .9, 
     pos = 4, 
     cex = 1.1, 
     labels = "The flow of the population in West Sussex\n(all ages)")

text(x = -1, 
     y = .75, 
     pos = 4, 
     cex = 1.1, 
     col = "red",
     labels = paste0('2020-2021'))

text(x = .6, 
     y = -.9, 
     pos = 4, 
     cex = 0.6, 
     labels = paste0('Data source = Census 2021'))

dev.off()

Local_flow_total_2 <- origin_dest_df %>% 
  filter(Age_broad == '65+ years') %>% 
  filter(Area_name == 'West Sussex' | Origin_area_name == 'West Sussex') %>%
  filter(Broad_origin != 'Not moved') %>% 
  filter(Broad_origin != 'Moved within local authority') %>% 
  mutate(InRegion_name = ifelse(Area_name == 'West Sussex', 'West Sussex', RGN_name)) %>% 
  mutate(OutRegion_name = ifelse(Origin_area_name == 'West Sussex', 'West Sussex', Origin_region_name)) %>% 
  group_by(InRegion_name, OutRegion_name) %>% 
  summarise(Moves = sum(Moves)) %>% 
  filter(OutRegion_name != InRegion_name) %>%  # Remove moves within area
  select(OutRegion_name, InRegion_name, Moves)

# Transparency (or highlighting areas) is controlled by adding a number between 0 and 99 to the end of the colour (adding 30 to every area except the one were interested in makes others 30% transparent).
Area_meta <- data.frame(Region = c("North East","North West","Yorkshire and The Humber", "East Midlands","West Midlands","East of England", "London", "South East", "South West", "Wales", "Scotland", "Northern Ireland", 'Outside of UK', 'West Sussex'), 
                        Order = seq(1,14,1), 
                        Colour = c("#bb5f70","#d3434b","#c96d34","#b58f48","#a8b23b","#588347","#5dbb61","#4db6c2","#6776c8","#a259c7","#c782bf","#cf4597", '#d3434b', '#1e4b7a'),
                        Label_l1 = c("North East","North West","Yorkshire and", "East Midlands","West Midlands","East of", "London", "South East", "South West", "Wales", "Scotland", "Northern", 'Outside of', 'West Sussex'), 
                        Label_l2 = c(NA,NA,"the Humber",NA,NA,"England",NA,NA,NA,NA,NA, "Ireland",'UK', '')) %>% 
  mutate(Region = as.character(Region)) %>% 
  mutate(Colour =  as.character(Colour)) %>% 
  mutate(Colour = ifelse(Region == "West Sussex", paste0(Colour, 30), paste0(Colour)))

# This just needs to be origin, destination, and moves

# What if we did WSX to regions (South East not including West Sussex)

# The next plot is saved as a png file
svg(paste0(output_store, '/Age_65_plus_migration_flow_raw.svg'), width = 12)

circos.clear()
par(mar = rep(0, 4), 
    cex=1)
circos.par(start.degree = 90, 
           track.margin=c(-0.2, 0.2),
           gap.degree = 6, 
           points.overflow.warning = FALSE)

# Create the plot itself
chordDiagram(Local_flow_total_2, 
             directional = 1, 
             order = Area_meta$Region, 
             grid.col = Area_meta$Colour, 
             annotationTrack = "grid", 
             transparency = 0.25,  
             annotationTrackHeight = c(0.05, 0.1), 
             direction.type = c("diffHeight", "arrows"), 
             link.arr.type = "big.arrow",
             diffHeight  = -0.02, 
             link.sort = TRUE, 
             link.largest.ontop = TRUE)

circos.track(track.index = 1, 
             bg.border = NA, # no borders are plotted on the track.
             panel.fun = function(x, y) {
               xlim = get.cell.meta.data("xlim") 
               sector.index = get.cell.meta.data("sector.index")
               
               Label_l1 = Area_meta %>% # collect matching name information from plot data frame (df1).
                 filter(Region == sector.index) %>% 
                 pull(Label_l1)
               Label_l2 = Area_meta %>% 
                 filter(Region == sector.index) %>% 
                 pull(Label_l2)
               
               # adds text from (reg1) either at y = 4 (if there is a second part of the name in reg2) or 3.
               circos.text(x = mean(xlim), # add text in the middle of the arch
                           y = ifelse(is.na(Label_l2), 3, 4), 
                           labels = Label_l1, 
                           facing = "bending", 
                           cex = 0.5)
               
               circos.text(x = mean(xlim), 
                           y = 2.75, 
                           labels = Label_l2, # adds text (reg2).
                           facing = "bending", 
                           cex = 0.5)
               
               #add axis with major and minor ticks, without flipping the axis labels in the bottom half.
               circos.axis(h = "top",
                           labels.cex = 0.5, 
                           labels.niceFacing = FALSE, 
                           labels.pos.adjust = FALSE,
                           major.at = seq(0,6000,1000),
                           minor.ticks = 9)
             })

text(x = .7,
     y = .9,
     pos = 4,
     cex = 0.6,
     labels = paste0("Units = moves;\nEach tick represents\n100 moves."))

text(x = -1, 
     y = .9, 
     pos = 4, 
     cex = 1.1, 
     labels = "The flow of the population in West Sussex\n(aged 65+ years)")

text(x = -1, 
     y = .75, 
     pos = 4, 
     cex = 1.1, 
     col = "red",
     labels = paste0('2020-2021'))

text(x = .6, 
     y = -.9, 
     pos = 4, 
     cex = 0.6, 
     labels = paste0('Data source = Census 2021'))

dev.off()

}

# Note for Outside of UK, we wont know the outflow to, as they would not have completed the Census.

# West Sussex picture ####
# Median age of areas (MSOA and LTLA)
# 2011 - 2021 map of over 65s and over 85s

# small area ####
census_21_LSOA_raw_df <- nomis_get_data(id = 'NM_2020_1',
                                     time = 'latest',
                                     measures = '20100',
                                     c2021_age_19 = '0,14...18',
                                     geography = 'TYPE151') %>%
  select(Area_code = GEOGRAPHY_CODE, Area_name = GEOGRAPHY_NAME, Year = DATE, Age = C2021_AGE_19_NAME, Population = OBS_VALUE)
# 
# census_11_LSOA_raw_df <- nomis_get_data(id = 'NM_1414_1',
#                                         time = 'latest', 
#                                         measures = '20100',
#                                         c_age = '0,34...39',
#                                         geography = 'TYPE298') #%>% 
#   select(Area_code = GEOGRAPHY_CODE, Area_name = GEOGRAPHY_NAME, Year = DATE, Age = C2021_AGE_19_NAME, Population = OBS_VALUE) 
# 
# https://www.nomisweb.co.uk/api/v01/dataset/NM_1414_1.data.csv?date=latest&geography=1249902593...1249937345&c_sex=0&c_age=0,34...39&measures=20100

# Describe population change at LTLA levels

if(file.exists(paste0(local_store, '/ltla_censusareachanges.xlsx'))!= TRUE){
download.file('https://www.ons.gov.uk/visualisations/censusareachanges/data/datadownload.xlsx',
              paste0(local_store, '/ltla_censusareachanges.xlsx'),
              mode = 'wb')
}

# Map CQC inspected residences 

cqc_raw <- read_csv('https://www.cqc.org.uk/sites/default/files/2024-01/24_January_2024_CQC_directory.csv',
                    skip = 4)

cqc_wsx <- cqc_raw %>% 
  filter(`Local authority` == 'West Sussex') %>% 
  filter(str_detect(`Service types`, 	'Nursing homes|Residential homes')) %>% 
  filter(str_detect(`Specialisms/services`, 'Caring for adults over 65 yrs'))


# The future ####



if(file.exists(paste0(local_store, '/ltla_subnational_oadr.xlsx'))!= TRUE){
  download.file('https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationprojections/datasets/populationofstatepensionageandworkingageandoldagedependencyratiosforlocalauthoritiesandregionsinengland/2018based/2018snppprincipaloadr.xlsx',
                paste0(local_store, '/ltla_subnational_oadr.xlsx'),
                mode = 'wb')
}



oadr_ons <- read_excel("Older_people_and_asc/Data/ltla_subnational_oadr.xlsx", 
                                           sheet = "Counties", skip = 3) %>% 
  bind_rows(read_excel("Older_people_and_asc/Data/ltla_subnational_oadr.xlsx", 
                            sheet = "Local authorities", skip = 3)) %>% 
  bind_rows(read_excel("Older_people_and_asc/Data/ltla_subnational_oadr.xlsx", 
                       sheet = "Regions", skip = 3)) %>% 
  pivot_longer(cols = '2018':'2043',
               names_to = 'Year') %>% 
  filter(area_name %in% c(local_areas, 'West Sussex', 'South East')) %>% 
  filter(age_group == 'Old age dependency ratio') %>% 
  select(Area = area_name, Year, OADR = value) %>% 
  mutate(Year = as.numeric(Year))


oadr_eng <- read_csv('https://download.ons.gov.uk/downloads/datasets/ageing-population-projections/editions/time-series/versions/1.csv') %>%
  filter(AgeGroups == 'Old age dependency ratio') %>%
  filter(Sex == 'All') %>%
  filter(UnitOfMeasure == 'Number') %>%
  select(Area = Geography, Year = Time, OADR = v4_1) %>% 
  filter(Area == 'England')


# need to get 1991 - 2022 population estimates then 2018 based projections to 2043
# oadr of 16-64 year olds over 65+
# This does not take into account the different SPA over time.

oadr_raw_estimated <- nomis_get_data(id = 'NM_2002_1',
                                   #  time = 'latest', 
                                     gender = '0', # '1,2' would return males and females
                                     measures = '20100',
                                     c_age = '203,209',
                                     geography = '1807745163,1811939621...1811939627,2013265928,2092957699') %>% 
  select(Area_code = GEOGRAPHY_CODE, Area_name = GEOGRAPHY_NAME, Year = DATE, Age = C_AGE_NAME, Population = OBS_VALUE) %>% 
  mutate(Type = 'Estimated')

oadr_raw_projected <- nomis_get_data(id = 'NM_2006_1',
               #  time = 'latest', 
               gender = '0', # '1,2' would return males and females
               measures = '20100',
               c_age = '203,209',
               geography = '1816133768,1820328217...1820328223,2092957699,2013265928') %>% 
  select(Area_code = GEOGRAPHY_CODE, Area_name = GEOGRAPHY_NAME, Year = PROJECTED_YEAR, Age = C_AGE_NAME, Population = OBS_VALUE) %>% 
  filter(!Year %in% c(2018,2019,2020,2021,2022)) %>% 
  mutate(Type = 'Projected')



oadr_df <- oadr_raw_estimated %>%
  bind_rows(oadr_raw_projected) %>% 
  pivot_wider(names_from = 'Age',
              values_from = 'Population') %>% 
  mutate(OADR = `Aged 65+` / `Aged 16 to 64` * 1000) %>% 
  mutate(Area_type = factor(ifelse(Area_name %in% c('Adur', 'Arun', 'Chichester', 'Crawley', 'Horsham', 'Mid Sussex', 'Worthing'), 'West Sussex District and Borough', ifelse(Area_name == 'South East', 'South East region', Area_name)), levels = c('West Sussex District and Borough', 'West Sussex', 'South East region', 'England')))

oadr_fig <- oadr_df %>% 
  filter(Year %in% c(1991, 1995, 2000, 2005, 2010, 2015, 2020, 2025, 2030, 2035, 2040, 2043)) %>% 
  arrange(Year) %>% 
  ggplot(aes(x = Year,
             y = OADR,
             group = Area_name)) +
  annotate(geom = "rect", xmin = 2022.5, xmax = Inf, ymin = 0, ymax = Inf, fill = "#e7e7e7", alpha = 0.35) +
  annotate(geom = "text", x = 1992, y = 750, hjust = 0, label = "Estimated", fontface = "bold", size = 4) +
  annotate(geom = "text", x = 2023, y = 750, hjust = 0, label = "Projected", fontface = "bold", size = 4) +
  geom_line(aes(colour = Area_type),
            linewidth = 1) +
  geom_point(shape = 21,
             size = 4,
             aes(colour = Area_type),
             fill = '#ffffff') +
  scale_colour_manual(values = c('#dbdbdb', '#1e4b7a', '#4db6c2', 'maroon')) +
  scale_y_continuous(limits = c(0,800),
                     breaks = seq(0,800, 100),
                     expand = c(0,0.01)) +
  scale_x_continuous(breaks = c(1991, 1995, 2000, 2005, 2010, 2015, 2020, 2025, 2030, 2035, 2040, 2043),
                     limits = c(1990, 2055),
                     expand = c(0,0.01)) +
  labs(x = 'Year',
       y = 'Number of 65+ year olds per 1,000 people aged 16-64',
       title = paste0('Old age dependency ratio: West Sussex areas; 1991-2043'),
       subtitle = 'Age 65+ per 1,000 aged 16-64*',
       caption =  '*Note: This does not take into account changes in state pension age over time.') +
  geom_vline(xintercept = 2022.5, lty = "dotted", colour = "#000000", lwd = 1) +
  geom_dl(data = subset(oadr_df, Year == 2043),
          aes(y = ifelse(Area_name %in% c('Chichester'), OADR + 15, ifelse(Area_name == 'Adur', OADR - 5, ifelse(Area_name == 'Horsham', OADR + 20, ifelse(Area_name %in% c('Arun', 'Worthing'), OADR - 15, OADR)))),
              x = 2044,
              label = paste0(' ', format(round(OADR,0),big.mark = ','), ' - ', Area_name)),
          method = list(cex = .65, hjust = 0)) + 
  ph_theme()

svg(paste0(output_store, '/OADR_timeseries.svg'),
    width = 10,
    height = 4.5,
    pointsize = 12)
print(oadr_fig)
dev.off()
