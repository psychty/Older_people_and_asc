
# Older people tend to move out of urban cities to rural areas.

# However adults moving to new areas to grow older often have to start anew with social support networks; which older adults who have lived there for longer will already have established. 

# So do we need to consider more than just those areas with more older people. Do some areas of West Sussex (or wider) have more older people flowing in. 

# Of course some people might move to a new area to be closer to family so this isn't fool proof and its also not particularly granular (only down to LTLA).

# this says census data but i cant find it
# https://www.solihull.gov.uk/sites/default/files/2023-12/Older-People-Internal-Migration.pdf

# year ending 2020 is the latest available data, though is it representative of typical changes due to the covid lockdowns? maybe we need to look at the previous year too.
# two parts zipped folders 
# https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/migrationwithintheuk/datasets/internalmigrationbyoriginanddestinationlocalauthoritiessexandsingleyearofagedetailedestimatesdataset/yearendingjune2020part1/detailedestimates2020on2021laspt1.zip

#https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/migrationwithintheuk/datasets/internalmigrationbyoriginanddestinationlocalauthoritiessexandsingleyearofagedetailedestimatesdataset/yearendingjune2020part2/detailedestimates2020on2021laspt2.zip

# Loading some packages 
packages <- c('easypackages','plyr', 'tidyr', 'ggplot2', 'dplyr', 'scales', 'readxl', 'readr', 'purrr', 'stringr', 'rgdal',  'geojsonio', 'jsonlite', 'viridis', 'nomisr', 'lemon', 'spdplyr', 'rgeos', "tmaptools", 'sp', 'sf', 'maptools', 'flextable', 'ggmap', 'grid', 'lemon', 'ggpol', 'httr', 'rvest')

install.packages(setdiff(packages, rownames(installed.packages())))
easypackages::libraries(packages)

local_store <- '~/Repositories/Older_people_and_asc/Data'

local_areas <- c('Adur', 'Arun', 'Chichester', 'Crawley', 'Horsham', 'Mid Sussex', 'Worthing')

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
# by age Census 2021 ####

# This dataset provides Census 2021 estimates on all usual residents aged 1 year and over in England and Wales who were living at a different address one year before the Census. The estimates classify people currently resident in each Middle layer Super Output Area (MSOA), or higher area by the MSOA, or higher area in which they were resident in one year before the Census. The estimates are as at Census Day, 21 March 2021. People resident outside of the UK one year before the Census are counted in the category "Outside UK".     

# If the LTLA OD file does not exist then download the zipped file and unzip it
if(file.exists(paste0(local_store, '/ODMG02EW_LTLA.csv')) != TRUE){
download.file('https://www.nomisweb.co.uk/output/census/2021/odmg02ew.zip',
              paste0(local_store, '/Census_origin_destination_migration_age.zip'),
              mode = 'wb')
unzip(paste0(local_store, '/Census_origin_destination_migration_age.zip'),
      exdir = local_store)
}

# Read in ltla_origin_destination df
ltla_origin_dest_df <- read_csv(paste0(local_store, '/ODMG02EW_LTLA.csv')) %>% 
  select(Origin_area_code = "Migrant LTLA one year ago code", Origin_area_name = "Migrant LTLA one year ago label", Area_code = "Lower tier local authorities code", Area_name = "Lower tier local authorities label", Age =  "Age (23 categories) label", Moves = Count) %>% 
  mutate(Age_group = factor(ifelse(Age %in% c("Aged 1 to 2 years", "Aged 3 to 4 years"), '1-4 years', ifelse(Age %in% c("Aged 5 to 7 years", "Aged 8 to 9 years"), '5-9 years', ifelse(Age %in% c("Aged 10 to 14 years"), '10-14 years', ifelse(Age %in% c('Aged 15 years', 'Aged 16 to 17 years', 'Aged 18 to 19 years'), '15-19 years', ifelse(Age %in% c('Aged 20 to 24 years'), '20-24 years', ifelse(Age %in% c('Aged 25 to 29 years'), '25-29 years', ifelse(Age %in% c('Aged 30 to 34 years'), '30-34 years', ifelse(Age %in% c('Aged 35 to 39 years'), '35-39 years', ifelse(Age %in% c('Aged 40 to 44 years'), '40-44 years', ifelse(Age %in% c('Aged 45 to 49 years'), '45-49 years', ifelse(Age %in% c('Aged 50 to 54 years'), '50-54 years', ifelse( Age %in% c('Aged 55 to 59 years'), '55-59 years', ifelse(Age %in% c('Aged 60 to 64 years'), '60-64 years', ifelse(Age %in% c('Aged 65 to 69 years'), '65-69 years', ifelse(Age %in% c('Aged 70 to 74 years'), '70-74 years', ifelse(Age %in% c('Aged 75 to 79 years'), '75-79 years', ifelse(Age %in% c('Aged 80 to 84 years'),'80-84 years', ifelse(Age %in% c('Aged 85 years and over'), '85+ years', NA)))))))))))))))))), levels = c('1-4 years', '5-9 years', '10-14 years', '15-19 years', '20-24 years', '25-29 years', '30-34 years', '35-39 years', '40-44 years', '45-49 years', '50-54 years', '55-59 years', '60-64 years', '65-69 years', '70-74 years', '75-79 years', '80-84 years', '85+ years'))) %>% 
  mutate(Age_broad =ifelse(Age %in% c("Aged 1 to 2 years", "Aged 3 to 4 years","Aged 5 to 7 years","Aged 8 to 9 years", "Aged 10 to 14 years",  "Aged 15 years", "Aged 16 to 17 years", "Aged 18 to 19 years", "Aged 20 to 24 years", "Aged 25 to 29 years", "Aged 30 to 34 years", "Aged 35 to 39 years", "Aged 40 to 44 years", "Aged 45 to 49 years", "Aged 50 to 54 years", "Aged 55 to 59 years", "Aged 60 to 64 years"), '1-64 years', ifelse(Age %in% c('Aged 65 to 69 years', 'Aged 70 to 74 years', 'Aged 75 to 79 years', 'Aged 80 to 84 years', 'Aged 85 years and over'), '65+ years',  NA))) %>% 
  mutate(Broad_origin = ifelse(Origin_area_name == 'Does not apply', 'Not moved', ifelse(Origin_area_name == Area_name, 'Moved within LTLA', ifelse(Origin_area_name %in% local_areas, 'Moved from another West Sussex area', 'Moved from elsewhere'))))

ltla_age <- ltla_origin_dest_df %>% 
  group_by(Origin_area_code, Origin_area_name, Area_code, Area_name, Age_group, Broad_origin) %>% 
  summarise(Moves = sum(Moves)) %>% 
  ungroup()

ltla_broad_age <- ltla_origin_dest_df %>% 
  group_by(Origin_area_code, Origin_area_name, Area_code, Area_name, Age_broad, Broad_origin) %>% 
  summarise(Moves = sum(Moves)) %>% 
  ungroup()

i = 1
Area_x = local_areas[i]

area_focus_x <- ltla_origin_dest_df %>% 
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

# generalise this to capture WSx areas
ltla_origin_dest_df %>% 
  filter(Area_name %in% local_areas) %>% 
  filter(Broad_origin != 'Not moved') %>% 
  filter(Broad_origin != 'Moved within LTLA') %>% 
  group_by(Area_name, Age_broad) %>% 
  summarise(Moves = sum(Moves)) %>% 
  pivot_wider(names_from = 'Age_broad',
              values_from = 'Moves') %>%
  mutate(`Total moves into area` = `1-64 years` + `65+ years`) %>% 
  mutate(Proportion_1_64 = `1-64 years` / `Total moves into area`) %>% 
  mutate(Proportion_65 = `65+ years` / `Total moves into area`) %>% 
  mutate(`Total moves into area` = paste0(format(`Total moves into area`, big.mark = ',', trim = TRUE))) %>% 
  mutate(`1-64 years` = paste0(format(`1-64 years`, big.mark = ',', trim = TRUE), ' (', round(Proportion_1_64 *100, 1), '%)')) %>% 
  mutate(`65+ years` = paste0(format(`65+ years`, big.mark = ',', trim = TRUE), ' (', round(Proportion_65 *100, 1), '%)')) %>% 
  select(Area = Area_name, `Total moves into area`,`1-64 years`, `65+ years`) %>% 
  flextable()

# This excludes those who moved in the last year but from another address in the same local authority (e.g. inflows capture those becoming a new resident of responsibility for the local authority).

# inflow and outflow by age group
area_inflow <- ltla_origin_dest_df %>% 
  filter(Broad_origin != 'Not moved') %>%
  filter(Area_name %in% local_areas) %>% 
  group_by(Area_name, Age_group) %>% 
  summarise(Inflow = sum(Moves))

area_x_outflow <- ltla_origin_dest_df %>% 
  filter(Broad_origin != 'Not moved') %>%
  filter(Origin_area_name %in% local_areas) %>% 
  group_by(Origin_area_name, Age_group) %>% 
  summarise(Outflow = sum(Moves)) %>% 
  select(Area_name = Origin_area_name, Age_group, Outflow)

area_flow <- area_inflow %>% 
  left_join(area_x_outflow, by = c('Area_name', 'Age_group')) %>% 
  mutate(Net_migration = Inflow - Outflow,
         Migration_turnover = Inflow + Outflow) %>% 
  mutate(Sex = 'Persons') %>% 
  left_join(census_population_five_years[c('Area_name', 'Sex', 'Age_group', 'Population')], by = c('Area_name', 'Sex', 'Age_group'))

# This is absolute numbers, are the same areas experiencing a higher migration turnover as a proportion of its older population

area_flow

# Which Local Authority has the highest number of 65+ moving into the area, from non-neighbouring areas


# West Sussex picture ####
# 2011 - 2021 map of over 65s and over 85s

# Map CQC inspected residences ####

