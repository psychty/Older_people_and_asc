
# Loading some packages 
packages <- c('easypackages', 'tidyr', 'ggplot2', 'plyr', 'dplyr', 'scales', 'readxl', 'readr', 'purrr', 'stringr', 'PHEindicatormethods', 'rgdal', 'spdplyr', 'geojsonio', 'rmapshaper', 'jsonlite', 'rgeos', 'sp', 'sf', 'maptools', 'ggpol', 'magick', 'officer', 'leaflet', 'leaflet.extras', 'zoo', 'fingertipsR', 'nomisr')
install.packages(setdiff(packages, rownames(installed.packages())))
easypackages::libraries(packages)

local_directory <- '//chi_nas_prod2.corporate.westsussex.gov.uk/groups2.bu/Public Health Directorate/PH Research Unit/Adult Social Care/Data'

if(dir.exists(local_directory) != TRUE){
  dir.create(local_directory)
}

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

# Styles for our pyramid plots
pyramid_theme <- function(){
  theme(plot.background = element_rect(fill = "white", colour = "#ffffff"),
        panel.background = element_rect(fill = "white"),
        axis.text = element_text(colour = "#000000", size = 7),
        plot.title = element_text(colour = "#000000", face = "bold", size = 9, vjust = 1),
        plot.subtitle = element_text(colour = "#000000", size = 8, vjust = 1),
        axis.title = element_text(colour = "#000000", face = "bold", size = 8),
        panel.grid.major.x = element_line(colour = "#E2E2E3", linetype = "solid", size = 0.1),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        strip.text = element_text(colour = "#000000", size = 8),
        strip.background = element_rect(fill = "#ffffff"),
        axis.ticks = element_line(colour = "#E2E2E3"),
        legend.key.size = unit(.5, 'cm'),
        legend.title = element_text(size= 8), #change legend title font size
        legend.text = element_text(size=8),
        legend.position = 'none')}

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
       title = paste0('Population pyramid; ', Area_x, ';\nCensus 2021;'),
       caption = 'Figures are rounded to the nearest 100') +
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

pyramid_plot_a

# Mid year estimates 2020

# https://www.nomisweb.co.uk/api/v01/dataset/NM_2002_1.data.csv?geography=1807745163,1811939621...1811939627&date=latest&gender=1,2&c_age=200,0,101...191&measures=20100

mye_2020 <- nomis_get_data(id = 'NM_2002_1',
               time = 'latest',
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

pyramid_df_x2 <- mye_2020 %>% 
  filter(Area == Area_x) 

pyramid_plot_b <- pyramid_df_x2 %>%
  ggplot() +
  geom_bar(data = area_x_dummy, aes(x = Age_group,
                                    y = Denominator_dummy /2),
           stat = "identity",
           fill = NA) +
  geom_bar(aes(x = Age_group, 
               y = ifelse(Sex == 'Females', -Population, Population),
               fill = Sex), 
           # colour = '#ffffff',
           width = 1,
           stat = "identity",
           position = position_dodge()) +
  scale_fill_manual(values =  c("#934c93", "#ff9966"),
                    name = 'Sex') +
  labs(x = '',
       y = '',
       title = paste0('Population pyramid; ', Area_x, ';\nMid 2020 estimates;'),
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

pyramid_plot_b

pyramid_plot_b +
  geom_bar(data = subset(pyramid_x_df, Sex != 'Persons'),
           aes(x = Age_group, 
               y = ifelse(Sex == 'Females', -Population, Population)),
           fill = NA, 
           colour = '#999999',
           lty = 'longdash',
           width = .95,
           stat = "identity",
           position = position_dodge()) +
  labs(title = 'Population pyramid; West Sussex\n2020 mid year estimates (bars) with 2021 census (dashed lines) overlaid',
       caption = 'This figure shows that the census estimates show greater numbers\nof people in some age groups (particularly 15-49 years)')

pyramid_plot_b +
  geom_line(data = subset(pyramid_x_df, Sex != 'Persons'),
            aes(x = Age_group, 
                y = ifelse(Sex == 'Females', -Population, Population),
                group = Sex),
                colour = '#000000') +
  labs(title = 'Population pyramid; West Sussex\n2020 mid year estimates (bars) with 2021 census (lines) overlaid',
       caption = 'This figure shows that the census estimates show greater numbers\nof people in some age groups (particularly 15-49 years).\nHowever, note that the census estimates are rounded to the nearest 100\nwhilst ordinary mid year estimates are unrounded.')

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
       title = paste0('Population pyramid; ', Area_x, ';\nProjected mid 2030 estimates; based on 2018 estimates')) +
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

pyramid_plot_c

pyramid_plot_b +
  geom_bar(data = subset(pyramid_df_x3, Sex != 'Persons'),
           aes(x = Age_group, 
               y = ifelse(Sex == 'Females', -Population, Population)),
           fill = NA, 
           colour = '#999999',
           lty = 'longdash',
           width = .95,
           stat = "identity",
           position = position_dodge()) +
  labs(title = 'Population pyramid; West Sussex\n2020 mid year estimates (bars) with 2030 projected estimates (dashed lines) overlaid',
       caption = 'This figure shows that by 2030 projected estimates show greater numbers\nof people in older age groups for both men and women\nwhilst estimates for those aged 40-59 are broadly similar.')



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
  filter(area_name %in% wsx_areas)

# Overall population change table 
# TODO - Area, 2018 Population, 2020 Population (measured), 2025 Population (projected), 2030 Population (projected), Percentage population change

# Old Age Dependency Ratio ####

# A common measure of ageing is the proportion of people aged 65 years and over. In England as a whole, this is projected to increase from 18.2% to 20.7% of the total population between mid-2018 and mid-2028. This is the continuation of a trend seen in the population estimates. The proportion is also projected to increase for all regions and local authorities, with the exception of Coventry where there is a slight reduction.

# An alternative measure of ageing is the old age dependency ratio (OADR), defined as the number of people of State Pension age (SPA) per 1,000 people of working age. Working age covers all people aged from 16 years up to State Pension age. Note that being over SPA does not necessarily mean someone is retired, nor are all working age people in employment.

# By 2028, the State Pension age will rise to age 67. As a result, the OADR in England is projected to fall from 293 in mid-2018 to 287 in mid-2028. However, at local authority level around a third of areas see a rise in OADR over this period.

# After mid-2028, almost all areas are projected to have an increasing OADR up to the end of the projection in mid-2043. This reflects the continued ageing of the population during a period in which no more rises in State Pension age are scheduled.

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

WSx_DC3302 <- nomis_get_data(id = 'NM_674_1',
               time = 'latest',
               sex = '1,2',
               measures = '20100',
               c_disability = '1...3',
               c_health = '0',
               c_age = '6...8',
               geography = "1941962888")

MSOA_DC3302 <- nomis_get_data(id = 'NM_674_1',
                                  time = 'latest',
                                  sex = '1,2',
                                  measures = '20100',
                                  c_disability = '1...3',
                                  c_health = '0',
                                  c_age = '6...8',
                                  geography = "TYPE297") %>% 
  select(Year = DATE, MSOA11NM = GEOGRAPHY_NAME, MSOA11CD = GEOGRAPHY_CODE, Age_group = C_AGE_NAME, Sex = GENDER_NAME, Limitation = C_DISABILITY_NAME, Health_status = C_HEALTH_NAME, Population = OBS_VALUE) 




