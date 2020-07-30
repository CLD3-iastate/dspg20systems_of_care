#-------------------------------------------------------------------------------------------------------------------------------------------------------------------#


server <- function(input, output){
  
  filtered_data <- reactive({
    filter(health_clinics, county == input$county2)
  })
  
  
  #--------------------------------------------------------------------------------------------------#
  output$hospital_leafy <- renderLeaflet({
    
    if(input$value == "Point"){
      filter_checkbox <- c("Hospital buildings", "Cross Mental Health", "Health Clinics", "hospitals", "mat locations", "Rx Locations")
      
      
      
      colors <- RColorBrewer::brewer.pal(n = length(filter_checkbox), name="Dark2")
      
      pal <- colorFactor(
        palette = colors,
        levels = filter_checkbox)
      
      # Create point map with the regional datasets colored differently
      leaflet() %>%
        addTiles() %>%
        addPolygons(data = st_transform(ia_counties, crs='+proj=longlat +datum=WGS84'),
                    weight = 1, color = "#333333") %>%
        addCircleMarkers(label = ~name,
                         radius = 1, color = ~pal("Hospital buildings"), fillOpacity = 0.7,
                         data = hospital_buildings,
                         group = "Hospital buildings") %>%
        
        addCircleMarkers(lng = ~longitude, lat = ~latitude, label = ~name,
                         radius = 1, color = ~pal("Health Clinics"), fillOpacity = 0.5,
                         data = health.clinics,
                         group = "Health Clinics") %>%
        
        addCircleMarkers(lng = ~longitude, lat = ~latitude, label = ~name,
                         radius = 1, color = ~pal("hospitals"), fillOpacity = 0.5,
                         data = hospitals,
                         group = "hospitals") %>%
        
        addCircleMarkers(lng = ~longitude, lat = ~latitude, label = ~center,
                         radius = 1, color = ~pal("mat locations"), fillOpacity = 0.5,
                         data = mat_locations,
                         group = "mat locations") %>%
        
        addCircleMarkers(lng = ~longitude, lat = ~latitude, label = ~`name`,
                         radius = 1, color = ~pal("Rx Locations"), fillOpacity = 0.5,
                         data = Rx_Drop_Off_Locations,
                         group = "Rx Locations") %>%
        addLayersControl(
          overlayGroups = filter_checkbox,
          options = layersControlOptions(collapsed = TRUE)
        ) %>%
        addLegend(
          position = c("bottomright"), pal = pal, values = filter_checkbox
        ) %>%
        setView(lng = -93.645733, lat = 42.026234, zoom = 7)
      
    }
    else if(input$value == "choropleth_bed_cap" ) {
      scale_range = c(0, max(county_counts$beds_percap))
      pal <- colorNumeric("Blues", scale_range, na.color = "#aaff56", reverse=FALSE)
      county_counts %>%
        leaflet() %>%
        addTiles() %>%
        addPolygons(data = county_counts$geometry, weight = 1, color="#333333", fillColor = pal(county_counts$beds_percap), fillOpacity = 0.9, label = county_counts$co_fips) %>%
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
      addCircleMarkers(lng = ~longitude, lat = ~latitude,
                       radius = 1, stroke = 0.1,
                       label = ~health_clinics)
    
  })
  
  
  mymap_proxy <- leafletProxy("health_leafy")
  
  observe({
    fdata <- filtered_data()
    mymap_proxy %>%
      clearMarkers() %>%
      addMarkers(lng = fdata$longitude, lat = fdata$latitude) %>%
      flyTo(lng = fdata$longitude, lat = fdata$latitude, zoom = 10)
  })
  
  
  #--------------------------------------------------------------------------------------------------#
  
  output$iaworks_leafy <- renderLeaflet({
    
    datasets = c("Colleges", "Churches", "Iowa Works Offices", "Parks")
    colors <- RColorBrewer::brewer.pal(n = length(datasets), name="Dark2")
    
    pal <- colorFactor(
      palette = colors,
      levels = datasets)
    
    # Create point map with the regional datasets colored differently
    leaflet() %>%
      addTiles() %>%
      addPolygons(data = st_transform(ia_counties, crs='+proj=longlat +datum=WGS84'),
                  weight = 1, color="#333333") %>%
      addCircleMarkers(lng = ~longitude, lat = ~latitude, label = ~name,
                       radius = 1, color = ~pal(datasets[1]), fillOpacity = 1,
                       data = colleges,
                       group = datasets[1]) %>%
      addCircleMarkers(label = ~name,
                       radius = 1, color = ~pal(datasets[2]), fillOpacity = 0.25,
                       data = churches,
                       group = datasets[2]) %>%
      addCircleMarkers(lng = ~Longitude, lat = ~Latitude, label = ~formatted_,
                       radius = 1, color = ~pal(datasets[3]), fillOpacity = 0.25,
                       data = iowaworks,
                       group = datasets[3]) %>%
      addCircleMarkers(lng = ~longitude, lat = ~latitude, label = ~name,
                       radius = 1, color = ~pal(datasets[4]), fillOpacity = 0.25,
                       data = parks,
                       group = datasets[4]) %>%
      addLayersControl(
        overlayGroups = datasets,
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      addLegend(
        position = c("bottomleft"), pal = pal, values = datasets
      ) %>%
      setView(lng = -93.645733, lat = 42.026234, zoom = 7)
    
  })
  
  
  output$iaworks_table = DT::renderDataTable({
    iowaworks
  })
  
  
  output$change <- renderLeaflet({
    datasets = c("MHDS Regional Access Points", "Southwest Iowa Resource Directory Locations", "South Central Iowa Resource Directory Locations")
    colors <- RColorBrewer::brewer.pal(n = length(datasets), name="Dark2")
    
    pal <- colorFactor(
      palette = colors,
      levels = datasets)
    
    
    # Create point map with the regional datasets colored differently
    leaflet() %>%
      addTiles() %>%
      addPolygons(data = st_transform(ia_counties, crs='+proj=longlat +datum=WGS84'),
                  weight = 1, color="#333333") %>%
      addCircleMarkers(lng = ~longitude, lat = ~latitude, label = ~name,
                       radius = 1, stroke = 1, color = ~pal("MHDS Regional Access Points"), fillOpacity = 1,
                       data = regional_MHDS,
                       group = "MHDS Regional Access Points") %>%
      addCircleMarkers(lng = ~longitude, lat = ~latitude, label = ~`service title`,
                       radius = 1, stroke = 1, color = ~pal("Southwest Iowa Resource Directory Locations"), fillOpacity = 0.25,
                       data = southwest_mhds,
                       group = "Southwest Iowa Resource Directory Locations") %>%
      addCircleMarkers(lng = ~longitude, lat = ~latitude, label = ~agency,
                       radius = 1, stroke = 1, color = ~pal("South Central Iowa Resource Directory Locations"), fillOpacity = 0.25,
                       data = scbhr_mhds,
                       group = "South Central Iowa Resource Directory Locations") %>%
      addLayersControl(
        overlayGroups = datasets,
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      addLegend(
        position = c("bottomleft"), pal = pal, values = datasets
      ) %>%
      setView(lng = -93.645733, lat = 42.026234, zoom = 7)
    
    
  })
  
  
  output$change2 <- renderLeaflet({
    
    filter_checkbox <- c("Substance Use Treatment", "Recovery Housing", "Shelters")
    
    # Color palette
    colors <- RColorBrewer::brewer.pal(n = length(filter_checkbox), name="Dark2")
    
    pal <- colorFactor(
      palette = colors,
      levels = filter_checkbox)
    
    # Edit starting dataframe and input for lng and lat inside addCircleMarkers to change data used
    df %>%
      leaflet() %>%
      addTiles() %>%
      # Use county border polygons from DSPG package
      addPolygons(data = st_transform(ia_counties, crs='+proj=longlat +datum=WGS84'),
                  weight = 1, color="#333333") %>%
      addCircleMarkers(lng = ~lon, lat = ~lat,
                       radius = 1, stroke = 1, fillOpacity = 0.7,color=~pal("Substance Use Treatment"),
                       label = ~Name, data = Recovery_SU,
                       group = "Substance Use Treatment") %>%
      addCircleMarkers(lng = ~lon, lat = ~lat,
                       radius = 1, stroke = 1, fillOpacity = 0.7,color=~pal("Recovery Housing"),
                       label = ~Name, data = Recovery_RH,
                       group = "Recovery Housing") %>%
      addCircleMarkers(lng = ~lon, lat = ~lat,
                       radius = 1, stroke = 1, fillOpacity = 0.7,color=~pal("Shelters"),
                       label = ~Name, data = Recovery_S,
                       group = "Shelters") %>%
      addLayersControl(
        overlayGroups = filter_checkbox,
        options = layersControlOptions(collapsed = TRUE)
      ) %>%
      addLegend(
        position = c("bottomright"), pal = pal, values = filter_checkbox
      ) %>%
      setView(lng = -93.645733, lat = 42.026234, zoom = 7)
    
    
  })
  
  output$change3 <- renderLeaflet({
    filter_checkbox <- c("Alcoholics Anonymous", "Adult children of Alcoholic", "Celebrate", "CRUSH", "IDRA", "Narcotics Anonymous", "Pills Anonymous", "Refuge Recovery", "SMART")
    
    # Color palette
    colors <- RColorBrewer::brewer.pal(n = length(filter_checkbox), name="Spectral")
    
    pal <- colorFactor(
      palette = colors,
      levels = filter_checkbox)
    
    # Edit starting dataframe and input for lng and lat inside addCircleMarkers to change data used
    df2 %>%
      leaflet() %>%
      addTiles() %>%
      addPolygons(data = st_transform(ia_counties, crs='+proj=longlat +datum=WGS84'),
                  weight = 1, color="#333333") %>%
      addCircleMarkers(lng = ~lon, lat = ~lat,
                       radius = 1, stroke =1, fillOpacity = 0.7, color=~pal("Alcoholics Anonymous"),
                       label = ~Meeting, data= Meetings_AA,
                       group = "Alcoholics Anonymous") %>%
      addCircleMarkers(lng = ~lon, lat = ~lat,
                       radius = 1, stroke =1, fillOpacity = 0.7, color=~pal("Adult children of Alcoholic"),
                       label = ~Meeting, data= Meetings_AdultChildA,
                       group = "Adult children of Alcoholic") %>%
      addCircleMarkers(lng = ~lon, lat = ~lat,
                       radius = 1, stroke =1, fillOpacity = 0.7, color=~pal("Celebrate"),
                       label = ~Meeting, data= Meetings_Celebrate,
                       group = "Celebrate") %>%
      addCircleMarkers(lng = ~lon, lat = ~lat,
                       radius = 1, stroke =1, fillOpacity = 0.7, color=~pal("CRUSH"),
                       label = ~Meeting, data= Meetings_Crush,
                       group = "CRUSH") %>%
      addCircleMarkers(lng = ~lon, lat = ~lat,
                       radius = 1, stroke =1, fillOpacity = 0.7, color=~pal("IDRA"),
                       label = ~Meeting, data= Meetings_IDRA,
                       group = "IDRA") %>%
      addCircleMarkers(lng = ~lon, lat = ~lat,
                       radius = 1, stroke =1, fillOpacity = 0.7, color=~pal("Narcotics Anonymous"),
                       label = ~Meeting, data= Meetings_NarAnon,
                       group = "Narcotics Anonymous") %>%
      addCircleMarkers(lng = ~lon, lat = ~lat,
                       radius = 1, stroke =1, fillOpacity = 0.7, color=~pal("Pills Anonymous"),
                       label = ~Meeting, data= Meetings_PA,
                       group = "Pills Anonymous") %>%
      addCircleMarkers(lng = ~lon, lat = ~lat,
                       radius = 1, stroke =1, fillOpacity = 0.7, color=~pal("Refuge Recovery"),
                       label = ~Meeting, data= Meetings_RR,
                       group = "Refuge Recovery") %>%
      addCircleMarkers(lng = ~lon, lat = ~lat,
                       radius = 1, stroke =1, fillOpacity = 0.7, color=~pal("SMART"),
                       label = ~Meeting, data= Meetings_Smart,
                       group = "SMART") %>%
      addLayersControl(
        overlayGroups = filter_checkbox,
        options = layersControlOptions(collapsed = TRUE)
      ) %>%
      addLegend(
        position = c("bottomright"), pal = pal, values = filter_checkbox
      ) %>%
      setView(lng = -93.645733, lat = 42.026234, zoom = 7)
  })
  
}


