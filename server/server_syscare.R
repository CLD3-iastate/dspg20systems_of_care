server <- function(input, output){
  
  filtered_data <- reactive({
    filter(health_clinics, COUNTY == input$county2)
  })
  
  #--------------------------------------------------------------------------------------------------#
  
  
  
  
  
  
  #--------------------------------------------------------------------------------------------------#
  output$hospital_leafy <- renderLeaflet({
    
    if(input$value == "Point"){
      data_hospitals %>%
        leaflet() %>%
        addTiles() %>%
        addPolygons(data = st_transform(counties, crs='+proj=longlat +datum=WGS84'),
                    weight = 1, color="#333333") %>%
        addCircleMarkers(radius = 1, stroke = 0.1, label = data_hospitals$BEDS)
      
    }
    else if(input$value == "choropleth_bed_cap" ) {
      scale_range = c(0, max(county_counts$beds_percap))
      pal <- colorNumeric("Blues", scale_range, na.color = "#aaff56", reverse=FALSE)
      county_counts %>%
        leaflet() %>%
        addTiles() %>%
        addPolygons(data = county_counts$geometry, weight = 1, color="#333333", fillColor = pal(county_counts$beds_percap), fillOpacity = 0.9, label = county_counts$CO_FIPS) %>%
        addLegend(pal = pal,
                  values = scale_range,
                  position = "topright",
                  title =  "Number of<br>Hospital Beds<br>Per Capita", opacity = 0.9)
    }
    else {
      bins = c(0, 0.5, 1, 2, 4, 8, 16, 32, 64)
      bins = exp(c(-Inf, -7, -6.5, -6, -5.5, -5, -4.5, -4, -3.5, -3, log(max(county_counts$beds_percap + 0.01))))*1000
      bins = round(bins, digits = 1)
      pal = colorBin("Blues", domain = county_counts$beds_per1000, bins = bins)
      county_counts %>%
        leaflet() %>%
        addTiles() %>%
        addPolygons(data = county_counts$geometry, weight = 1, color="#333333", fillColor = pal(county_counts$beds_per1000), fillOpacity = 0.9, label = county_counts$beds_per1000) %>%
        addCircleMarkers(data = data_hospitals$geometry, radius = 1, stroke = 0.1, color = "#000000") %>%
        addLegend(pal = pal,
                  values = bins,
                  position = "topright",
                  title = "Number of<br>Hospital Beds<br>Per 1000 People", opacity = 0.9)
    }
  })
  
  #--------------------------------------------------------------------------------------------------#
  
  output$health_leafy <- renderLeaflet({
    
    filtered_data() %>%
      leaflet() %>%
      addTiles() %>%
      addPolygons(data = st_transform(ia_counties, crs='+proj=longlat +datum=WGS84'),
                  weight = 1, color="#333333") %>%
      addCircleMarkers(lng = ~Longitude, lat = ~Latitude,
                       radius = 1, stroke = 0.1,
                       label = ~health_clinics)
    
  })
  
  
  mymap_proxy <- leafletProxy("health_leafy")
  
  observe({
    fdata <- filtered_data()
    mymap_proxy %>%
      clearMarkers() %>%
      addMarkers(lng = fdata$Longitude, lat = fdata$Latitude) %>%
      flyTo(lng = fdata$Longitude, lat = fdata$Latitude, zoom = 10)
  })
  
  
  #--------------------------------------------------------------------------------------------------#
  
  output$iaworks_leafy <- renderLeaflet({
    
    iowaworks %>%
      leaflet() %>%
      addTiles() %>%
      addPolygons(data = st_transform(ia_counties, crs='+proj=longlat +datum=WGS84'),
                  weight = 1, color="#333333") %>%
      addCircleMarkers(lng = ~Longitude, lat = ~Latitude,
                       radius = 1, stroke = 0.1,
                       label = ~iowaworks)
    
  })
  
  
  output$iaworks_table = DT::renderDataTable({
    iowaworks
  })
  
  
  #--------------------------------------------------------------------------------------------------#
  
  
  output$multiple_leafy <- renderLeaflet({
    colors <- RColorBrewer::brewer.pal(n = length(datasets), name="Dark2")
    
    pal <- colorFactor(
      palette = colors,
      levels = datasets)
    
    # Create point map with the regional datasets colored differently
    leaflet() %>%
      addTiles() %>%
      addPolygons(data = st_transform(ia_counties, crs='+proj=longlat +datum=WGS84'),
                  weight = 1, color="#333333") %>%
      addCircleMarkers(lng = ~lon, lat = ~lat, label = ~Name,
                       radius = 1, color = ~pal("Area Substance Abuse Council"), fillOpacity = 0.7,
                       data = asac_locations,
                       group = "Area Substance Abuse Council") %>%
      addCircleMarkers(lng = ~lon, lat = ~lat, label = ~Name,
                       radius = 1, color = ~pal("Community and Family Resources"), fillOpacity = 0.5,
                       data = cf_resources,
                       group = "Community and Family Resources") %>%
      addCircleMarkers(lng = ~lon, lat = ~lat, label = ~`Service Title`,
                       radius = 1, color = ~pal("Southwest MHDS"), fillOpacity = 0.5,
                       data = southwest_mhds,
                       group = "Southwest MHDS") %>%
      addLayersControl(
        overlayGroups = datasets,
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      setView(lng = -93.645733, lat = 42.026234, zoom = 7)
  })
  
  
}

shinyApp(ui = ui, server = server)
