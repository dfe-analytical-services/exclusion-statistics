# Map

#---------------------------------------------------------------------
#Load spatial points dataframe

englishLocalAuthorityData <- shapefile("data/englishLocalAuthorityData/la_map.shp")

#---------------------------------------------------------------------
#Create colour bins and palette labels

# Create bins
perm_excl_rate_Pal = colorQuantile(map_gov_colours, englishLocalAuthorityData$prm_xc_, n = 5)

fixed_excl_rate_Pal = colorQuantile(map_gov_colours, englishLocalAuthorityData$fxd_xc_, n = 5)

# Add a labels for tooltips 
perm_excl_rate_Labels <-
  sprintf(
    "<div style='width:230px'>
    <font size='4.5'><strong>%s</strong></font>
    <br/>Headcount                   <span style='color:black;float:right'><strong>%s</strong></span>
    <br/>Permanent exclusions        <span style='color:black;float:right'><strong>%s</strong></span>
    <br/>Permanent exclusion rate    <span style='color:black;float:right'><strong>%s</strong></span>
    </div>",
    englishLocalAuthorityData$LA15NM,
    format(englishLocalAuthorityData$headcnt,big.mark = ",",trim = TRUE),
    format(englishLocalAuthorityData$prm_xcl,big.mark = ",",trim = TRUE),
    paste(as.character(englishLocalAuthorityData$prm_xc_), "%", sep = "")
  ) %>%
  lapply(htmltools::HTML)

fixed_excl_rate_Labels <-
  sprintf(
    "<div style='width:230px'>
    <font size='4.5'><strong>%s</strong></font>
    <br/>Headcount                   <span style='color:black;float:right'><strong>%s</strong></span>
    <br/>Fixed period exclusions     <span style='color:black;float:right'><strong>%s</strong></span>
    <br/>Fixed period exclusion rate <span style='color:black;float:right'><strong>%s</strong></span>
    </div>",
    englishLocalAuthorityData$LA15NM,
    format(englishLocalAuthorityData$headcnt,big.mark = ",",trim = TRUE),
    format(englishLocalAuthorityData$fxd_xcl,big.mark = ",",trim = TRUE),
    paste(as.character(englishLocalAuthorityData$fxd_xc_), "%", sep = "")
  ) %>%
  lapply(htmltools::HTML)

#---------------------------------------------------------------------
#Create map function

excmap <- function(measure) {
  
  if(measure == 'perm') {
    
    map <- leaflet(englishLocalAuthorityData) %>%
        addProviderTiles(providers$CartoDB.Positron,
                         options = providerTileOptions(minZoom = 6, maxZoom = 10)) %>%
        addPolygons(fillColor = ~perm_excl_rate_Pal(englishLocalAuthorityData$prm_xc_),
                    weight = 1,
                    opacity = 1,
                    color = "black",
                    dashArray = "0",
                    fillOpacity = 0.7,
                    highlight = highlightOptions(
                      weight = 5,
                      color = "#666",
                      dashArray = "",
                      fillOpacity = 0.7,
                      bringToFront = TRUE),
                    label = perm_excl_rate_Labels,
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", 
                                   padding = "3px 8px",
                                   "background-color" = "white"),
                      textsize = "15px",
                      direction = "auto")) 
  }
  
  if(measure == 'fixed') {
    
    map <- leaflet(englishLocalAuthorityData) %>%
        addProviderTiles(providers$CartoDB.Positron,
                         options = providerTileOptions(minZoom = 6, maxZoom = 10)) %>%
        addPolygons(fillColor = ~fixed_excl_rate_Pal(englishLocalAuthorityData$fxd_xc_),
                    weight = 1,
                    opacity = 1,
                    color = "black",
                    dashArray = "0",
                    fillOpacity = 0.7,
                    highlight = highlightOptions(
                      weight = 5,
                      color = "#666",
                      dashArray = "",
                      fillOpacity = 0.7,
                      bringToFront = TRUE),
                    label = fixed_excl_rate_Labels,
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", 
                                   padding = "3px 8px",
                                   "background-color" = "white"),
                      textsize = "15px",
                      direction = "auto")) 
  }
  
  return(map %>%
           addLegend(colors = c("#FFBF47", "#EC933D", "#D86733", "#C53A28", "#B10E1E", "#808080"), 
                     opacity = 1, 
                     title = NULL,
                     position = "topright",
                     labels= c("Lowest exclusion rates", "","","","Highest exclusion rates", "Supressed data")) %>%
           setMaxBounds(lat1 = 55.5, lng1 = -6.8, lat2 = 49.99, lng2 = 1.95))
}

