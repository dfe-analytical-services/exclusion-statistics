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
library(extrafont)

#---------------------------------------------------------------------
#Load the data required

# main_ud file
main_ud <- read_csv('data/SFR35_2017_national_region_la_school_data.csv', col_types = cols(.default = "c"))

# reason_ud file
reason_ud <- read_csv("data/SFR35_2017_reason_for_exclusion.csv", col_types = cols(.default = "c"))

# characteristics UD
char_ud <- read_csv('data/SFR35_2017_National_characteristics.csv', col_types = cols(.default = "c"))

# school names data from get schools information full data

school_names_raw <- read_csv('data/get_schools_information.csv', col_types = cols(.default = "c"))

#---------------------------------------------------------------------
#General functions - 

# Change the year variable into xxxx/xx format

formatyr <- function(refyear) {
  
  sub("(.{4})(.*)", "\\1/\\2", refyear)
  
}

# example
# formatyr(201213)
# = 2012/13

# test change to be deleted 


has_warning <- function (expr) 
{
  warn = FALSE
  op = options(warn = -1)
  on.exit(options(op))
  withCallingHandlers(expr, warning = function(w) {
    warn <<- TRUE
    invokeRestart("muffleWarning")
  })
  warn
}

round_with_supressed_values <- function(x) {
  
  if(has_warning(as.numeric(x))) {
    return(x)
  } 
  
  if(!has_warning(as.numeric(x))) {
    x <- round(as.numeric(x), 0)
    return(x)
  } 
  
  else(
    return(x))
  
}

format_with_supressed_numbers <- function(x) {
  
  if(has_warning(as.numeric(x))) {
    return(x)
  } 
  
  if(!has_warning(as.numeric(x))) {
    x <- format(as.numeric(x), nsmall=0, big.mark=",")
    return(x)
  } 
  
  else(
    return(x))
  
}