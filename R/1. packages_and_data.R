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


# cleaning data for characteristics tab  
nat_char_prep <- filter(char_ud, ! characteristic_1 %in% c("SEN_provision_Unclassified","FSM_Unclassified")) %>%
  mutate(characteristic_1 = ifelse(
    characteristic_1 == "Gender_male","Boys",
    ifelse(characteristic_1 == "Gender_female", "Girls",
           ifelse(characteristic_1 == "Total", "Total",
                  ifelse(characteristic_1 == "SEN_Provision_No_SEN","No SEN",
                         ifelse(characteristic_1 == "SEN_provision_SEN_with_statement_EHC", "SEN with statement or EHC",
                                ifelse(characteristic_1 == "SEN_provision_SEN_without_statement", "SEN without a statement or EHC",
                                       ifelse(characteristic_1 == "FSM_Eligible","FSM eligible",
                                              ifelse(characteristic_1 == "FSM_NotEligible", "FSM not eligible","NA"))))))))) %>%
  mutate(school_type = ifelse(
    school_type == "State-funded primary","Primary",
    ifelse(school_type == "State-funded secondary","Secondary",
           ifelse(school_type == "Special", "Special", 
                  ifelse(school_type == "Total", "Total", "NA"))))) 