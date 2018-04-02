# MAP tab

# ukLocalAuthoritises <- shapefile("data/England_LA_2016.shp")
# 
# exc_data <- filter(main_ud, level == 'Local authority', school_type == 'total', year ==201516) %>%
#   select(old_la_code,perm_excl_rate, fixed_excl_rate, headcount, perm_excl, fixed_excl)
# 
# exc_data$perm_excl_rate <- as.numeric(exc_data$perm_excl_rate)
# exc_data$fixed_excl_rate <- as.numeric(exc_data$fixed_excl_rate)
# exc_data$headcount <- as.numeric(exc_data$headcount)
# exc_data$perm_excl <- as.numeric(exc_data$perm_excl)
# exc_data$fixed_excl <- as.numeric(exc_data$fixed_excl)
# 
# ukLocalAuthoritises <- spTransform(ukLocalAuthoritises, CRS("+proj=longlat +ellps=GRS80"))
# englishLocalAuthorities = subset(ukLocalAuthoritises, LA15CD %like% "E") # Code begins with E
# 
# englishLocalAuthorityData <- merge(englishLocalAuthorities,
#                                    exc_data,
#                                    by.x = 'LA_Code',
#                                    by.y = 'old_la_code',
#                                    all.y = TRUE)
# 
# writeOGR(obj=englishLocalAuthorityData, dsn="data/englishLocalAuthorityData", layer="la_map", driver="ESRI Shapefile")
# 

englishLocalAuthorityData <- shapefile("data/englishLocalAuthorityData/la_map.shp")

#permanent exc

# Create bins for colour plotting
perm_excl_rate_Pal = colorQuantile(map_gov_colours, englishLocalAuthorityData$prm_xcl, n = 5)

# Add a label for tooltip (bit of html)
perm_excl_rate_Labels <- sprintf("<strong>%s</strong><br/>Headcount <strong>%s</strong><br/>Permanent exclusions <strong>%s</strong><br/>Permanent exclusion rate <strong>%s</strong>",
                                 englishLocalAuthorityData$LA15NM, format(englishLocalAuthorityData$headcnt,big.mark=",", trim=TRUE), format(englishLocalAuthorityData$prm_xc_,big.mark=",", trim=TRUE), paste(as.character(englishLocalAuthorityData$prm_xcl), "%")) %>%
  lapply(htmltools::HTML)

#fixed exc

# Create bins for colour plotting
fixed_excl_rate_Pal = colorQuantile(map_gov_colours, englishLocalAuthorityData$fxd_xcl, n = 5)

# Add a label for tooltip (more html...)
fixed_excl_rate_Labels <- sprintf("<strong>%s</strong><br/>Headcount <strong>%s</strong><br/>Fixed period exclusions <strong>%s</strong><br/>Fixed period exclusion rate <strong>%s</strong>",
                                  englishLocalAuthorityData$LA15NM, format(englishLocalAuthorityData$headcnt,big.mark=",", trim=TRUE), format(englishLocalAuthorityData$fxd_xc_,big.mark=",", trim=TRUE), paste(as.character(englishLocalAuthorityData$fxd_xcl), "%")) %>%
  lapply(htmltools::HTML)


excmap <- function(measure) {
  
  if(measure == 'perm') {
    
    return(
      leaflet(englishLocalAuthorityData) %>%
        addProviderTiles(providers$CartoDB.Positron,
                         options = providerTileOptions(minZoom = 7, maxZoom = 10)) %>%
        addPolygons(fillColor = ~perm_excl_rate_Pal(englishLocalAuthorityData$prm_xcl),
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
                      opacity = 1)) %>%
        addLegend(colors = c("#FFBF47", "#EC933D", "#D86733", "#C53A28", "#B10E1E", "#808080"), 
                  opacity = 0.7, 
                  title = NULL,
                  position = "topright",
                  labels= c("Lowest exclusion rates", "","","","Highest exclusion rates", "Supressed data")) %>%
        setMaxBounds(lat1 = 55.5, lng1 = -6.8, lat2 = 49.99, lng2 = 1.95)
    )
  }
  
  if(measure == 'fixed') {
    
    return(
      leaflet(englishLocalAuthorityData) %>%
        addProviderTiles(providers$CartoDB.Positron,
                         options = providerTileOptions(minZoom = 7, maxZoom = 10)) %>%
        addPolygons(fillColor = ~fixed_excl_rate_Pal(englishLocalAuthorityData$fxd_xcl),
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
                      direction = "auto")) %>%
        addLegend(colors = c("#FFBF47", "#EC933D", "#D86733", "#C53A28", "#B10E1E", "#808080"), 
                  opacity = 0.7, 
                  title = NULL,
                  position = "topright",
                  labels= c("Lowest exclusion rates", "","","","Highest exclusion rates", "Supressed data")) %>%
        setMaxBounds(lat1 = 55.5, lng1 = -6.8, lat2 = 49.99, lng2 = 1.95)
    )
  }
}

