# sourceDir <- function(path, trace = TRUE, ...) {
#   for (nm in list.files(path, pattern = "\\.[RrSsQq]$")) {
#     if(trace) cat(nm,":")           
#     source(file.path(path, nm),...)
#     if(trace) cat("\n")
#   }
# }
# 
# sourceDir("R/")


source("R/1. packages_and_data.R")
source("R/2. overview_tab.R")
source("R/characteristics_tab.R")
source("R/general.R")
source("R/gov_colours.R")
source("R/la_trends_tab.R")
source("R/map_tab.R")
source("R/reason_tab.R")
source("R/school_tab.R")
