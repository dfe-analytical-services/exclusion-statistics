# MAP tab

ukLocalAuthoritises <- shapefile("data/England_LA_2016.shp")

exc_data <- filter(main_ud, level == 'Local authority', school_type == 'total', year ==201516) %>%
  select(old_la_code,perm_excl_rate, fixed_excl_rate)

exc_data$perm_excl_rate <- as.numeric(exc_data$perm_excl_rate)
exc_data$fixed_excl_rate <- as.numeric(exc_data$fixed_excl_rate)

ukLocalAuthoritises <- spTransform(ukLocalAuthoritises, CRS("+proj=longlat +ellps=GRS80"))
englishLocalAuthorities = subset(ukLocalAuthoritises, LA15CD %like% "E") # Code begins with E

englishLocalAuthorityData <- merge(englishLocalAuthorities, 
                                   exc_data, 
                                   by.x = 'LA_Code', 
                                   by.y = 'old_la_code',
                                   all.y = TRUE)

#permanent exc

# Create bins for colour plotting
perm_excl_rate_Pal = colorQuantile('YlOrRd', englishLocalAuthorityData$perm_excl_rate, n = 5)

# Add a label for tooltip (bit of html)
perm_excl_rate_Labels <- sprintf("<strong>%s</strong><br/>Permanent exclusion rate <strong>%g</strong> <sup></sup>",
                                 englishLocalAuthorityData$LA15NM, englishLocalAuthorityData$perm_excl_rate) %>%
  lapply(htmltools::HTML)

#fixed exc

# Create bins for colour plotting
fixed_excl_rate_Pal = colorQuantile('YlOrRd', englishLocalAuthorityData$fixed_excl_rate, n = 5)

# Add a label for tooltip (more html...)
fixed_excl_rate_Labels <- sprintf("<strong>%s</strong><br/>Fixed period exclusion rate <strong>%g</strong> <sup></sup>",
                                  englishLocalAuthorityData$LA15NM, englishLocalAuthorityData$fixed_excl_rate) %>%
  lapply(htmltools::HTML)


excmap <- function(measure) {
  
  if(measure == 'perm') {
    
    return(
      leaflet(englishLocalAuthorityData) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~perm_excl_rate_Pal(englishLocalAuthorityData$perm_excl_rate),
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    dashArray = "3",
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
                      direction = "auto")) %>%
        addLegend(colors = c("#FFFFB2", "#FECC5C", "#FD8D3C", "#F03B20", "#BD0026", "#808080"), 
                  opacity = 0.7, 
                  title = NULL,
                  position = "topright",
                  labels= c("Lowest exclusion rates", "","","","Highest exclusion rates", "Supressed data"))
    )
  }
  
  if(measure == 'fixed') {
    
    return(
      leaflet(englishLocalAuthorityData) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~fixed_excl_rate_Pal(englishLocalAuthorityData$fixed_excl_rate),
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    dashArray = "3",
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
        addLegend(colors = c("#FFFFB2", "#FECC5C", "#FD8D3C", "#F03B20", "#BD0026", "#808080"), 
                  opacity = 0.7, 
                  title = NULL,
                  position = "topright",
                  labels= c("Lowest exclusion rates", "","","","Highest exclusion rates", "Supressed data"))
    )
  }
}

