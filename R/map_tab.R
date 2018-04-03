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
    "<strong>%s</strong><br/>Headcount <strong>%s</strong><br/>Permanent exclusions <strong>%s</strong><br/>Permanent exclusion rate <strong>%s</strong>",
    englishLocalAuthorityData$LA15NM,
    format(englishLocalAuthorityData$headcnt,big.mark = ",",trim = TRUE),
    format(englishLocalAuthorityData$prm_xcl,big.mark = ",",trim = TRUE),
    paste(as.character(englishLocalAuthorityData$prm_xc_), "%")
  ) %>%
  lapply(htmltools::HTML)

fixed_excl_rate_Labels <-
  sprintf(
    "<strong>%s</strong><br/>Headcount <strong>%s</strong><br/>Fixed period exclusions <strong>%s</strong><br/>Fixed period exclusion rate <strong>%s</strong>",
    englishLocalAuthorityData$LA15NM,
    format(englishLocalAuthorityData$headcnt,big.mark = ",",trim = TRUE),
    format(englishLocalAuthorityData$fxd_xcl,big.mark = ",",trim = TRUE),
    paste(as.character(englishLocalAuthorityData$fxd_xc_), "%")
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
                    opacity = 0.7,
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
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto",
                      opacity = 1)) 
  }
  
  if(measure == 'fixed') {
    
    map <- leaflet(englishLocalAuthorityData) %>%
        addProviderTiles(providers$CartoDB.Positron,
                         options = providerTileOptions(minZoom = 6, maxZoom = 10)) %>%
        addPolygons(fillColor = ~fixed_excl_rate_Pal(englishLocalAuthorityData$fxd_xc_),
                    weight = 1,
                    opacity = 0.7,
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
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto")) 
  }
  
  return(map %>%
           addLegend(colors = c("#FFBF47", "#EC933D", "#D86733", "#C53A28", "#B10E1E", "#808080"), 
                     opacity = 0.7, 
                     title = NULL,
                     position = "topright",
                     labels= c("Lowest exclusion rates", "","","","Highest exclusion rates", "Supressed data")) %>%
           setMaxBounds(lat1 = 55.5, lng1 = -6.8, lat2 = 49.99, lng2 = 1.95))
}

