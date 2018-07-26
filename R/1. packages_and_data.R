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
library(shinyjs)

#---------------------------------------------------------------------
#Load the data required

# main_ud file
main_ud <- read_csv('data/main0617excl_ud_r_df.csv', col_types = cols(.default = "c"))

# reason_ud file
reason_ud <- read_csv("data/reasons.csv", col_types = cols(.default = "c"))

# characteristics UD
char_ud <- read_csv('data/nat_char (201112 to 201617).csv', col_types = cols(.default = "c"))

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


change_ed <- function(numA, numB) {
  
  if(numA == 'x' | numB == 'x') {return ('been suppressed')}
  
  if(numA < numB) {return ('increased')}
  
  if(numA > numB) {return ('decreased')}
  
  else {return('stayed the same')}
  
}



numeric_ifelse <- function(x) {
  
  if(has_warning(as.numeric(x))) {
    return(" one or two pupils per 10,000 pupils, however this is not exact due to supression of small numbers.")
  } 
  
  if(!has_warning(as.numeric(x))) {
    return(paste(as.numeric(x)*100, " pupils per 10,000 pupils."))
  } 
  
  else(
    return(""))
  
}


change_ed_x <- function(numA, numB) {
  
  if(has_warning(as.numeric(numA)) | has_warning(as.numeric(numB))) {return('has been supressed')}
  
  if(is.na(numA) | is.na(numB)) {return ('is not available')}
  
  if(as.numeric(numA) < as.numeric(numB)) {return ('increased')}
  
  if(as.numeric(numA) > as.numeric(numB)) {return ('decreased')}
  
  else {return('stayed the same')}
  
}



numeric_supress <- function(x) {
  
  if(has_warning(as.numeric(x))) {
    return(paste("has been supressed"))
  } 
  
  if(!has_warning(as.numeric(x))) {
    return(paste(as.numeric(x)))
  } 
  
  else(
    return(""))
  
}


numeric_round_warning <- function(x) {
  
  if(has_warning(as.numeric(x))) {
    return(x)
  } 
  
  if(!has_warning(as.numeric(x))) {
    return(format(round(as.numeric(x),2), nsmall=2))
  } 
  
  else(
    return(""))
  
}


dyExtraHead =
  ## Collection of tags you would usually want to add to
  ##  `tags$head` when using dygraph-extra in shiny.
  ## e.g. tags$head(dyExtraHead())
  function() tagList(
    ## Load the JS library, this should be located within
    ##  the /www/ subfolder of the shiny app.
    tags$script(src = "dygraph-extra.js"),
    ## Ensure dygraphs resize correctly on tab switch
    tags$script('Dygraph.Export.ShinyTabResize();'),
    ## Ensure dygraphs clears selection correctly on mouseleave
    tags$script('Dygraph.Export.AutoClear();')
  )

dyRegister =
  ## Pass to drawCallback when creating a dygraph
  ## e.g. dyCallbacks(drawCallback = dyRegister())
  function() "Dygraph.Export.Register"

dyDownload =
  ## Create a link with given label
  ## When clicked, will prompt user to download:
  ##  - a png image of the dygraph with given id, if a single id
  ##  - a zip containing pngs of all dygraphs with given ids, for multiple ids
  ## elid (id of the <a> link) can be optionally specified
  ## If usetitle is TRUE, the filename will use the dygraph's main-title
  ##             if FALSE, the filename will be the id
  ## If asbutton is TRUE, the link will be styled as a nice looking button
  function(id, label, elid = NULL, usetitle = TRUE, asbutton = FALSE){
    enc = function(x){
      if(is.logical(x)) ifelse(x, "true", "false")
      else encodeString(x, quote = '"')
    }
    encid = if(length(id) > 1)
      paste0("[", paste(enc(id), collapse = ","), "]")
    else enc(id)
    
    if(is.null(elid)) elid = paste0(id[1], "-dyDownload")
    out = tags$a(id = elid, class = "download-link",
                 href = "#", label, onclick =
                   paste0("Dygraph.Export.DownloadByID(",
                          encid, ", ", enc(usetitle), "); return false;"))
    if(asbutton){
      out$attribs$class = "btn btn-default download-link"
      out$children = tagList(icon("download"), out$children)
    }
    out
  }

dyDownloadGroup =
  ## Wrapper for a nice list of multiple dyDownload
  ## downIDs can be a named vector, in which case the names are
  ##  used for the labels for dyDownload
  ## If downAll is TRUE, also adds a dyDownload for all plots at the top
  ##  with the label "Download All Plots"
  function(groupid, label, downIDs, downAll = TRUE,
           usetitle = TRUE, asbutton = FALSE){
    if(is.null(names(downIDs))) names(downIDs) = downIDs
    downlist = list()
    for(i in 1:length(downIDs))
      downlist[[i]] = tags$li(
        dyDownload(downIDs[i], names(downIDs)[i],
                   usetitle = usetitle, asbutton = asbutton))
    
    if(downAll)
      downlist = c(list(tags$li(
        dyDownload(downIDs, "Download All Plots",
                   paste0(groupid, "-dyDownloadAll"),
                   usetitle = usetitle, asbutton = asbutton)
      )), downlist)
    
    tags$div(id = groupid, class = "form-group dydownload-group",
             tags$label(label),
             tags$ul(do.call(tagList, downlist))
    )
  }

mbie_header =
  ## Adds a basic MBIE header, that will only display
  ##  if the page is not embedded as an iframe.
  ## Requires: www/mbie-logo.png
  function() div(id = "mbie-header",
                 div(class = "mbie-topbar"),
                 div(class = "mbie-brand",
                     tags$a(class = "mbie-brand", href = "https://www.gov.uk/government/statistics?keywords=&topics%5B%5D=all&departments%5B%5D=department-for-education&from_date=&to_date=",
                            title = "Department for Education",
                            tags$img(src = "Department_for_Education.png",
                                     height = 85.2,
                                     width = 144,
                                     style = "margin:10px 10px",
                                     alt = "Department for Education"))
                 )
  )





frontp = function() div(class = "frontp",
                        div(class = "front-banner",
                            div(class = "imgcon"),
                            div(class = "hcon", h1("Understanding School"), h1("Exclusion Statistics"))
                        ),

                        #tags$p(tags$span(class = "warning", "This app is still in active development and has not been officially launched.")),
                        tags$p(class = "intro", "The Department for Education's school exclusion statistics dashboard is a one-stop shop for all information about school exclusions. 
                               It allows users to more easily access school exclusion statistics produced by the department into one easy-to-use tool. Information is presented using data tables, graphs and interactive maps."),
                        div(class = "intro-divider"),
                        tags$p("Main subject-area groupings of exclusion statistics are shown on the toolbar above. To navigate around this site, left-click one of the subject areas and then select one of the related categories in the drop down list."),
                        tags$p("Data tables can be downloaded to a csv file. Information on the data used in this dashboard is available in the",
                               tags$a("Data and methods", title = "Help Tab", href = "#", id = "HelpTabLink"), "tab.",
                               tags$script("$('#HelpTabLink').click(function(){$('a[data-value=\"Help\"]')[0].click();});")
                        ),
                        div(class = "box-con",
                            tags$a(target = "_blank",
                                   href = "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/642577/Guide-to-exclusion-statistics-05092017.pdf",
                                   div(class = "float box box-more",
                                       tags$p(class = "intro", "Find out more"),
                                       tags$p("Click here for more information on the collection of exclusion statistics in England.")
                                   )),
                            tags$a(target = "_blank",
                                   href = "https://www.gov.uk/government/statistics/permanent-and-fixed-period-exclusions-in-england-2016-to-2017",
                                   div(class = "float box box-rear",
                                       tags$p(
                                         tags$img(class = "rear-preview", src = "dfe_report.png"),
                                         "The", span(class = "bold", "Permanent and fixed-period exclusions in England: 2016 to 2017"),
                                         "report produced by Department for Education presents further information and trends that supplements the information in this dashboard. Click here to access the online report."
                                       )
                                   )
                            )
                        ),
                        div(class = "box box-timeout",
                            tags$p(tags$span(class = "bold", "PLEASE NOTE:"),
                                   "This app may time-out if left idle too long, which will cause the screen to grey-out.",
                                   "To use the app again, refresh the page. This will reset all previously-selected input options.")
                        )
)


