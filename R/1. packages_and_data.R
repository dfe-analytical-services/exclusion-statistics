# Load packages
# Import and create data


#### 
# 1. Load packages ----

library(leaflet)
library(geojsonio)
library(rgdal)
library(sp)
library(data.table)
library(RColorBrewer)
library(raster)
library(pander)
library(tidyverse)
library(shinycssloaders)
library(plotly)
library(shiny)
library(DT)
library(ggalt)
library(magrittr)
library(scales)

####
# 2. Load the data required ----

# load main_ud file
# includes main measures at school, la, region and national level for 2006/07 to 2015/16

main_ud <- read_csv('data/SFR35_2017_national_region_la_school_data.csv', col_types = cols(.default = "c"))

#glimpse(main_ud)

# load reason_ud file
# includes la, region and national level for 2006/07 to 2015/16

reason_ud <- read_csv("data/SFR35_2017_reason_for_exclusion.csv", col_types = cols(.default = "c"))

# head(reason_ud)

# characteristics UD

char_ud <- read_csv('data/SFR35_2017_National_characteristics.csv', col_types = cols(.default = "c"))

# school names data from get schools information full data

school_names_raw <- read_csv('data/get_schools_information.csv')

####
# 2. Create the data required ----

# create a national summary table (used for plots):

nat_summary <-
  dplyr::select(
    filter(main_ud, level == 'National'),
    year,
    school_type,
    num_schools,
    headcount,
    perm_excl,
    perm_excl_rate,
    fixed_excl,
    fixed_excl_rate,
    one_plus_fixed,
    one_or_more_fixed_excl_rate
  ) %>%
  arrange(year)

# Create LA plot data 

la_plot_data <-
  dplyr::select(
    filter(main_ud, level == 'Local authority', school_type != 'dummy'),
    year,
    la_name,
    school_type,
    num_schools,
    headcount,
    perm_excl,
    perm_excl_rate,
    fixed_excl,
    fixed_excl_rate,
    one_plus_fixed,
    one_or_more_fixed_excl_rate) %>%
  mutate(school_type = ifelse(
    school_type == "state-funded primary","Primary",
    ifelse(school_type == "state-funded secondary","Secondary",
           ifelse(school_type == "special", "Special", 
                  ifelse(school_type == "total", "Total", "NA")))))

# cleaning data for characteristics tab  
nat_char_prep <- filter(char_ud, ! characteristic_1 %in% c("SEN_provision_Unclassified","FSM_Unclassified","Age_unclassified")) %>%
  mutate(school_type = ifelse(
    school_type == "State-funded primary","Primary",
    ifelse(school_type == "State-funded secondary","Secondary",
           ifelse(school_type == "Special", "Special", 
                  ifelse(school_type == "Total", "Total", "NA"))))) 

nat_char_prep$characteristic_1 <- recode(nat_char_prep$characteristic_1,
       Gender_male="Boys",
       Gender_female= "Girls",
       SEN_Provision_No_SEN="No SEN",
       SEN_provision_SEN_with_statement_EHC= "SEN with statement or EHC",
       SEN_provision_SEN_without_statement="SEN without a statement or EHC",
       FSM_Eligible="FSM eligible",
       FSM_NotEligible="FSM not eligible",
       Age_4_and_under = "Age 4 and under",                                              
       Age_5 = "Age 5",                                                         
       Age_6 = "Age 6",                                                         
       Age_7 = "Age 7",                                                           
       Age_8 = "Age 8",                                                           
       Age_9 = "Age 9",   
       Age_10 = "Age 10",                                                          
       Age_11 = "Age 11",                                                           
       Age_12 = "Age 12",                                                         
       Age_13 = "Age 13",                                                         
       Age_14 = "Age 14",                                                         
       Age_15 = "Age 15",                                                         
       Age_16 = "Age 16",                                                         
       Age_17 = "Age 17",                                                         
       Age_18 = "Age 18",                                                         
       Age_19_and_over = "Age 19 and over"                                                 

)
