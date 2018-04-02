#---------------------------------------------------------------------
#Load libraries

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
library(sparkline)

#---------------------------------------------------------------------
#Load the data required

# main_ud file
main_ud <- read_csv('data/SFR35_2017_national_region_la_school_data.csv', col_types = cols(.default = "c"))

# reason_ud file
reason_ud <- read_csv("data/SFR35_2017_reason_for_exclusion.csv", col_types = cols(.default = "c"))

# characteristics UD
char_ud <- read_csv('data/SFR35_2017_National_characteristics.csv', col_types = cols(.default = "c"))


#---------------------------------------------------------------------
#General functions - 

# Change the year variable into xxxx/xx format

formatyr <- function(refyear) {
  
  sub("(.{4})(.*)", "\\1/\\2", refyear)
  
}

# example
# formatyr(201213)
# = 2012/13