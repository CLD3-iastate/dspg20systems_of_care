#Load required Packages

library("dplyr")
library("purrr")
library("readr")
library("stringr")
#library("readxl")
library("magrittr")
library("ggplot2")
library("shiny")
#library("sentimentr")
library("shinythemes")
library("shindashboard")
library("shinydashboardPlus")
library("plotly")
library("leaflet")
library("sf")
 packages <- c("dplyr", "purrr", "stringr", "readxl",
               "magrittr", "stringr", "ggplot2", "shiny", "sentimentr",
               "shinythemes", "shinydashboard", "shinydashboardPlus", 
               "plotly", "leaflet", "sf")
 
 for (pkgs in packages){
  if(!require(pkgs, character.only = TRUE)){ # Condition 
     install.packages(pkgs) # Install if not
     library(pkgs) # Load if installed
   }
 }

library(DT)

library(shiny)
library(shinydashboardPlus)
library(shinydashboard)

library(DSPG)
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------#




data_hospitals <- hospitals #Df Change Name

counties <- ia_counties #Df Change Name


data_hospitals <-  data_hospitals %>% st_as_sf(coords = c("longitude", "latitude"), crs = 4326, agr = "identity") #create object sf

data_hospitals <- data_hospitals %>% filter(!is.na(beds)) #filter out NAs

combined <- st_join(data_hospitals, st_transform(counties, crs='+proj=longlat +datum=WGS84')) #Choose correct datum the user wants to use



county_counts <- combined %>% group_by(co_fips) %>% summarise(bed_count = sum(beds)) #Create bed count column

county_counts <- county_counts %>% st_set_geometry(NULL) 

county_counts = left_join(st_transform(ia_counties, crs='+proj=longlat +datum=WGS84'), county_counts, by = "co_fips") #Join dataframes

county_counts[is.na(county_counts$bed_count), "bed_count"] = 0 # Set all NAs to 0


county_counts <- county_counts %>% mutate(beds_percap = bed_count/popestimate2019) #Create beds_percap column


county_counts <- county_counts %>% mutate(beds_per1000 = beds_percap*1000) #Create beds_per1000 column
county_counts = county_counts %>% mutate(ln_beds_percap = log(beds_percap)) #Create beds_percap column


health_clinics <- health.clinics #Df change Name


datasets = c("Area Substance Abuse Council", "Community and Family Resources", "Southwest MHDS") #For Multi Layer Plot





df <- recovery #read.csv('Reco_Data.csv')

df = df[-c(144),]


# Grouping Recovery centers data by type
Recovery_SU <- df[df$ServiceType == "SubstanceUseTreatment", ]
Recovery_RH <- df[df$ServiceType == "RecoveryHousing", ]
Recovery_S <- df[df$ServiceType == "Shelters", ]


df2 <- meetings #read.csv('All_Meetings_Geocoded.csv')


# Grouping Meetings data by type
Meetings_AA <- filter(df2, df2$type %in% c("Alcoholics Anonymous", "Al-anon"))
Meetings_AdultChildA <- df2[df2$type == "Adult children of alcoholic", ]
Meetings_Celebrate <- df2[df2$type == "Celebrate", ]
Meetings_Crush <- df2[df2$type == "CRUSH", ]
Meetings_IDRA <- df2[df2$type == "IDRA", ]
Meetings_NarAnon <- filter(df2, df2$type %in% c("Narcotics Anonymous", "Nar-Anon"))
Meetings_PA <- df2[df2$type == "Pills Anonymous", ]
Meetings_RR <- df2[df2$type == "Refuge Recovery", ]
Meetings_Smart <- df2[df2$type == "SMART", ]



#---------------------------------------------------------------------------------------------------------------------------------------------------------------------#
sidebar <- dashboardSidebar(
  
  sidebarMenu(
    
    menuItem("Project Descriptions", icon = icon(" fa-file-text "), tabName = "description"),
    
    
    menuItem("Syscare", icon = icon("leaf"),
             menuSubItem("Panel 1", tabName = "syscare_1", icon = icon("angle-right")),
             menuSubItem("Panel 2", tabName = "syscare_2", icon = icon("angle-right")),
             menuSubItem("Panel 3", tabName = "syscare_3", icon = icon("angle-right")),
             menuSubItem("Panel 4", tabName = "syscare_4", icon = icon("angle-right")),
             menuSubItem("abstinence-from-substance", tabName = "syscare_5", icon = icon("angle-right")),
             menuSubItem("community-services", tabName = "syscare_6", icon = icon("angle-right")),
             menuSubItem("improved-health", tabName = "syscare_7", icon = icon("angle-right")),
             menuSubItem("quality-of-life", tabName = "syscare_8", icon = icon("angle-right")),
             menuSubItem("social-connectedness", tabName = "syscare_9", icon = icon("angle-right")),
             menuSubItem("employment-education", tabName = "syscare_10", icon = icon("angle-right"))),
    
    menuItem("Recovery", icon = icon("leaf"),
             menuSubItem("Panel 1", tabName = "recovery_1", icon = icon("angle-right")),
             menuSubItem("Panel 2", tabName = "recovery_2", icon = icon("angle-right")),
             menuSubItem("Panel 3", tabName = "recovery_3", icon = icon("angle-right")),
             menuSubItem("Panel 4", tabName = "recovery_4", icon = icon("angle-right"))),
    
    menuItem("Acknowledgements", icon = icon("star"), tabName = "credit")
  )
  
)

#------------------------------------------------------------------------------------------------------------------------------------------------------------#

body <- dashboardBody(
  tabItems(
    
    tabItem(tabName = "description",
            fluidRow(
              boxPlus(
                title = "Project Overview",
                closable = FALSE,
                width = NULL,
                status = "warning",
                solidHeader = TRUE,
                collapsible = TRUE,
                h1("2020 DSPG Project Name"),
                h2("Project Description"),
                p("Example text: Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante. Fusee justo nisi, suscipit a lacus et, posuere sagittis ex."),
                h2("Project Goals"),
                p("Example text: Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante. Fusee justo nisi, suscipit a lacus et, posuere sagittis ex."),
                h2("Our Approach"),
                p("Example text: Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante. Fusee justo nisi, suscipit a lacus et, posuere sagittis ex."),
                h2("Ethical Considerations"),
                p("Example text: Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante. Fusee justo nisi, suscipit a lacus et, posuere sagittis ex.")
              )
            )),
    
    
    
    tabItem(tabName = 'syscare_1', #Panel 1
            
            
            fluidRow(
              
              box(
                title = "(I)owa (H)ospital (I)nformation (S)ystem",
                status = "warning",
                width = NULL,
                fluidRow(
                  column(
                    width = 3,
                    boxPad(
                      color = "grey",
                      
                      
                      
                      selectInput(inputId = "value", label = strong("Select Graph Type"),
                                  choices = c("Point", "choropleth_bed_cap", "choropleth_log_1000"),
                                  selected = NULL, multiple = FALSE, selectize = TRUE)
                      
                    )
                  ),
                  column(
                    width = 9,
                    leafletOutput("hospital_leafy", height = "500px")
                  )
                )
              )
            )),
    
    tabItem(tabName = 'syscare_2',  #Panel 2
            
            fluidRow(
              
              box(
                title = "Health Clinic Information System",
                status = "warning",
                width = NULL,
                fluidRow(
                  column(
                    width = 3,
                    boxPad(
                      color = "grey",
                      
                      selectInput(inputId = "county2", label = strong("Select a County"),
                                  choices = unique(health_clinics$county),
                                  selected = NULL)
                    )
                  ),
                  column(
                    width = 9,
                    leafletOutput("health_leafy", height = "500px")
                  )
                )
              )
            )),
    
    tabItem(tabName = 'syscare_3',  #Panel 3#######################################################
            
            fluidRow(
              
              box(
                title = "General Well Being",
                status = "warning",
                width = NULL,
                fluidRow(
                  column(
                    width = 3,
                    boxPad(
                      color = "grey"
                    )
                  ),
                  column(
                    width = 9,
                    leafletOutput("iaworks_leafy", height = "500px")
                  )
                )
              )
            ),
            
            fluidRow(
              dataTableOutput("iaworks_table"), style = "overflow-x: scroll;"
            )),
    
    tabItem(tabName = 'syscare_4',  #Panel 4#######################################################
            
            fluidRow(
              
              box(
                title = "Mental Heath and Disability Services",
                status = "warning",
                width = NULL,
                fluidRow(
                  column(
                    width = 3,
                    boxPad(
                      color = "grey"
                      
                      
                    )
                  ),
                  column(
                    width = 9,
                    leafletOutput("change", height = "500px")
                  )
                )
              )
            )
            

          ),
    tabItem(tabName = 'syscare_5',  #Panel 4#######################################################
            
            fluidRow(
              
              box(
                title = "Abstinence from Substance Locations",
                status = "warning",
                width = NULL,
                fluidRow(
                  column(
                    width = 3,
                    boxPad(
                      color = "grey"
                      
                      
                    )
                  ),
                  column(
                    width = 9,
                    leafletOutput("abstinence", height = "500px")
                  )
                )
              )
            )
            
            
    ),
    
    tabItem(tabName = 'syscare_6',  #Panel 4#######################################################
            
            fluidRow(
              
              box(
                title = "Community Service locations",
                status = "warning",
                width = NULL,
                fluidRow(
                  column(
                    width = 3,
                    boxPad(
                      color = "grey"
                      
                      
                    )
                  ),
                  column(
                    width = 9,
                    leafletOutput("community", height = "500px")
                  )
                )
              )
            )
            
            
    ),
    
    tabItem(tabName = 'syscare_7',  #Panel 4#######################################################
            
            fluidRow(
              
              box(
                title = "Improved Health Locations",
                status = "warning",
                width = NULL,
                fluidRow(
                  column(
                    width = 3,
                    boxPad(
                      color = "grey"
                      
                      
                    )
                  ),
                  column(
                    width = 9,
                    leafletOutput("improve", height = "500px")
                  )
                )
              )
            )
            
            
    ),
    
    tabItem(tabName = 'syscare_8',  #Panel 4#######################################################
            
            fluidRow(
              
              box(
                title = "Quality of Life locations",
                status = "warning",
                width = NULL,
                fluidRow(
                  column(
                    width = 3,
                    boxPad(
                      color = "grey"
                      
                      
                    )
                  ),
                  column(
                    width = 9,
                    leafletOutput("quality", height = "500px")
                  )
                )
              )
            )
            
            
    ),
    
    tabItem(tabName = 'syscare_9',  #Panel 4#######################################################
            
            fluidRow(
              
              box(
                title = "Social Connectedness Locations",
                status = "warning",
                width = NULL,
                fluidRow(
                  column(
                    width = 3,
                    boxPad(
                      color = "grey"
                      
                      
                    )
                  ),
                  column(
                    width = 9,
                    leafletOutput("social", height = "500px")
                  )
                )
              )
            )
            
            
    ),
    
    tabItem(tabName = 'syscare_10',  #Panel 4#######################################################
            
            fluidRow(
              
              box(
                title = "Employment Education Locations",
                status = "warning",
                width = NULL,
                fluidRow(
                  column(
                    width = 3,
                    boxPad(
                      color = "grey"
                      
                      
                    )
                  ),
                  column(
                    width = 9,
                    leafletOutput("employment", height = "500px")
                  )
                )
              )
            )
            
            
    ),
    
    tabItem(tabName = 'recovery_1',  
            
            fluidRow(
              
              box(
                title = "Recovery data",
                status = "warning",
                width = NULL,
                fluidRow(
                  column(
                    width = 3,
                    boxPad(
                      color = "grey"
                      
                      
                    )
                  ),
                  column(
                    width = 9,
                    leafletOutput("change2", height = "500px")
                  )
                )
              )
            )
            
            
    ),
    
    tabItem(tabName = 'recovery_2',  
            
            fluidRow(
              
              box(
                title = "Recovery meeting data",
                status = "warning",
                width = NULL,
                fluidRow(
                  column(
                    width = 3,
                    boxPad(
                      color = "grey"
                      
                      
                    )
                  ),
                  column(
                    width = 9,
                    leafletOutput("change3", height = "500px")
                  )
                )
              )
            )
            
            
    )
    )
  
)

















#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------#

ui <- dashboardPage(
  dashboardHeader(title = "Syscare+Recovery Shiny App"),
  sidebar = sidebar,
  body = body,
  skin = "black"
)

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
        addEasyButton(easyButton(
          icon="fa-globe", title="Zoom to Level 1",
          onClick=JS("function(btn, map){ map.setZoom(1); }"))) %>%
        addEasyButton(easyButton(
          icon="fa-crosshairs", title="Locate Me",
          onClick=JS("function(btn, map){ map.locate({setView: true}); }"))) %>%
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
      addEasyButton(easyButton(
        icon="fa-globe", title="Zoom to Level 1",
        onClick=JS("function(btn, map){ map.setZoom(1); }"))) %>%
      addEasyButton(easyButton(
        icon="fa-crosshairs", title="Locate Me",
        onClick=JS("function(btn, map){ map.locate({setView: true}); }"))) %>%
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
      addEasyButton(easyButton(
        icon="fa-globe", title="Zoom to Level 1",
        onClick=JS("function(btn, map){ map.setZoom(1); }"))) %>%
      addEasyButton(easyButton(
        icon="fa-crosshairs", title="Locate Me",
        onClick=JS("function(btn, map){ map.locate({setView: true}); }"))) %>%
      setView(lng = -93.645733, lat = 42.026234, zoom = 7)
    
  
  })
  
  
  #-----------------------------------------------------------------------------------------------------#
  
  output$abstinence <- renderLeaflet({
    
    datasets = c("Regional Substance Use Treatment Facilities", "Crug Drop Off Sites", "MAT Locations", "Substance Use/Problem Gambling Recovery")
    colors <- RColorBrewer::brewer.pal(n = length(datasets), name="Dark2")
    
    pal <- colorFactor(
      palette = colors,
      levels = datasets)
    
    
    # Create point map with the regional datasets colored differently
    leaflet() %>%
      addTiles() %>%
      addPolygons(data = st_transform(ia_counties, crs='+proj=longlat +datum=WGS84'),
                  weight = 1, color="#333333") %>%
      addCircleMarkers(lng = ~longitude, lat = ~latitude, label = ~location_name,
                       radius = 1, stroke = 1, color = ~pal(datasets[1]), fillOpacity = 0.75,
                       data = regional_substance_treatment,
                       group = datasets[1]) %>%
      addCircleMarkers(lng = ~longitude, lat = ~latitude, label = ~name,
                       radius = 1, stroke = 1, color = ~pal(datasets[2]), fillOpacity = 0.25,
                       data = Rx_Drop_Off_Locations,
                       group = datasets[2]) %>%
      addCircleMarkers(lng = ~longitude, lat = ~latitude, label = ~center,
                       radius = 1, stroke = 1, color = ~pal(datasets[3]), fillOpacity = 0.25,
                       data = mat_locations,
                       group = datasets[3]) %>%
      addCircleMarkers(lng = ~longitude, lat = ~latitude, label = ~name,
                       radius = 1, stroke = 1, color = ~pal(datasets[4]), fillOpacity = 0.25,
                       data = gambling,
                       group = datasets[4]) %>%
      addLayersControl(
        overlayGroups = datasets,
        options = layersControlOptions(collapsed = TRUE)
      ) %>%
      addLegend(
        position = c("bottomleft"), pal = pal, values = datasets, opacity = 0.9
      ) %>%
      setView(lng = -93.645733, lat = 42.026234, zoom = 7)
    
    
    
  })
  
  
  output$community <- renderLeaflet({
    
    
    leaflet() %>%
      addTiles() %>%
      addPolygons(data = st_transform(ia_counties, crs='+proj=longlat +datum=WGS84'),
                  weight = 1, color="#333333") %>%
      addCircleMarkers(lng = ~longitude, lat = ~latitude, label = ~name,
                       radius = 1, stroke = 1, fillOpacity = 0.75,
                       data = iowa_211)
    
    
  })
  
  
  output$improve <- renderLeaflet({
    
    datasets = c("Rural Health Clinics", "Iowa Hospitals", "Iowa Hospitals - OfficialUSA", "Iowa Hospitals and Medical Centers", "General Health Clinics", "VA Clinics", "Childcare Providers", "Medical Facilities")
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
                       radius = 1, stroke = 1, color = ~pal(datasets[1]), fillOpacity = 0.75,
                       data = health.clinics,
                       group = datasets[1]) %>%
      addCircleMarkers(lng = ~longitude, lat = ~latitude, label = ~name,
                       radius = 1, stroke = 1, color = ~pal(datasets[2]), fillOpacity = 0.25,
                       data = hospitals,
                       group = datasets[2]) %>%
      addCircleMarkers(lng = ~longitude, lat = ~latitude, label = ~hospital_name,
                       radius = 1, stroke = 1, color = ~pal(datasets[3]), fillOpacity = 0.25,
                       data = official_usa,
                       group = datasets[3]) %>%
      #addCircleMarkers(lng = ~longitude, lat = ~latitude, label = ~name,
      #                radius = 1, stroke = 1, color = ~pal(datasets[4]), fillOpacity = 0.25,
      #               data = wiki_hospitals,
      #              group = datasets[4]) %>%
      #addCircleMarkers(lng = ~longitude, lat = ~latitude, label = ~location_name,
      #                radius = 1, stroke = 1, color = ~pal(datasets[5]), fillOpacity = 0.75,
      #               data = regional_substance_treatment,
      #              group = datasets[5]) %>%
      addCircleMarkers(lng = ~longitude, lat = ~latitude, label = ~facility,
                       radius = 1, stroke = 1, color = ~pal(datasets[6]), fillOpacity = 0.25,
                       data = va_medical_centers,
                       group = datasets[6]) %>%
      #addCircleMarkers(lng = ~longitude, lat = ~latitude, label = ~center,
      #                radius = 1, stroke = 1, color = ~pal(datasets[7]), fillOpacity = 0.25,
      #               data = mat_locations,
      #              group = datasets[7]) %>%
      addCircleMarkers(label = ~name,
                       radius = 1, stroke = 1, color = ~pal(datasets[8]), fillOpacity = 0.001,
                       data = hospital_buildings,
                       group = datasets[8]) %>%
      addLayersControl(
        overlayGroups = datasets,
        options = layersControlOptions(collapsed = TRUE)
      ) %>%
      addLegend(
        position = c("bottomleft"), pal = pal, values = datasets, opacity = 0.9
      ) %>%
      setView(lng = -93.645733, lat = 42.026234, zoom = 7)
    
  })
  
  output$quality <- renderLeaflet({
    
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
        options = layersControlOptions(collapsed = TRUE)
      ) %>%
      addLegend(
        position = c("bottomleft"), pal = pal, values = datasets, opacity = 0.9
      ) %>%
      setView(lng = -93.645733, lat = 42.026234, zoom = 7)
    
    
    
  })
  
  
  output$social <- renderLeaflet({
    
    leaflet() %>%
      addTiles() %>%
      addPolygons(data = st_transform(ia_counties, crs='+proj=longlat +datum=WGS84'),
                  weight = 1, color="#333333") %>%
      addCircleMarkers(label = ~name,
                       radius = 1, stroke = 1, fillOpacity = 0.75,
                       data = churches)
    
    
  })
  
  
  output$employment <- renderLeaflet({
    
    
    datasets = c("Iowa Works Offices", "Iowa Colleges and Universities")
    colors <- RColorBrewer::brewer.pal(n = length(datasets), name="Dark2")
    
    pal <- colorFactor(
      palette = colors,
      levels = datasets)
    
    
    # Create point map with the regional datasets colored differently
    leaflet() %>%
      addTiles() %>%
      addPolygons(data = st_transform(ia_counties, crs='+proj=longlat +datum=WGS84'),
                  weight = 1, color="#333333") %>%
      addCircleMarkers(lng = ~Longitude, lat = ~Latitude, label = ~NAME,
                       radius = 1, stroke = 1, color = ~pal(datasets[1]), fillOpacity = 1,
                       data = iowaworks,
                       group = datasets[1]) %>%
      addCircleMarkers(lng = ~longitude, lat = ~latitude, label = ~name,
                       radius = 1, stroke = 1, color = ~pal(datasets[2]), fillOpacity = 1,
                       data = colleges,
                       group = datasets[2]) %>%
      addLayersControl(
        overlayGroups = datasets,
        options = layersControlOptions(collapsed = TRUE)
      ) %>%
      addLegend(
        position = c("bottomleft"), pal = pal, values = datasets, opacity = 0.9
      ) %>%
      setView(lng = -93.645733, lat = 42.026234, zoom = 7)
 
    
  })
  
  
  
  
  
  
  
  
  
  
  #-----------------------------------------------------------------------------------------------------#
  
  
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
      addCircleMarkers(lng = ~longitude, lat = ~latitude,
                       radius = 1, stroke = 1, fillOpacity = 0.7,color=~pal("Substance Use Treatment"),
                       label = ~Name, data = Recovery_SU,
                       group = "Substance Use Treatment") %>%
      addCircleMarkers(lng = ~longitude, lat = ~latitude,
                       radius = 1, stroke = 1, fillOpacity = 0.7,color=~pal("Recovery Housing"),
                       label = ~Name, data = Recovery_RH,
                       group = "Recovery Housing") %>%
      addCircleMarkers(lng = ~longitude, lat = ~latitude,
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
      addEasyButton(easyButton(
        icon="fa-globe", title="Zoom to Level 1",
        onClick=JS("function(btn, map){ map.setZoom(1); }"))) %>%
      addEasyButton(easyButton(
        icon="fa-crosshairs", title="Locate Me",
        onClick=JS("function(btn, map){ map.locate({setView: true}); }"))) %>%
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
      addCircleMarkers(lng = ~longitude, lat = ~latitude,
                       radius = 1, stroke =1, fillOpacity = 0.7, color=~pal("Alcoholics Anonymous"),
                       label = ~Meeting, data= Meetings_AA,
                       group = "Alcoholics Anonymous") %>%
      addCircleMarkers(lng = ~longitude, lat = ~latitude,
                       radius = 1, stroke =1, fillOpacity = 0.7, color=~pal("Adult children of Alcoholic"),
                       label = ~Meeting, data= Meetings_AdultChildA,
                       group = "Adult children of Alcoholic") %>%
      addCircleMarkers(lng = ~longitude, lat = ~latitude,
                       radius = 1, stroke =1, fillOpacity = 0.7, color=~pal("Celebrate"),
                       label = ~Meeting, data= Meetings_Celebrate,
                       group = "Celebrate") %>%
      addCircleMarkers(lng = ~longitude, lat = ~latitude,
                       radius = 1, stroke =1, fillOpacity = 0.7, color=~pal("CRUSH"),
                       label = ~Meeting, data= Meetings_Crush,
                       group = "CRUSH") %>%
      addCircleMarkers(lng = ~longitude, lat = ~latitude,
                       radius = 1, stroke =1, fillOpacity = 0.7, color=~pal("IDRA"),
                       label = ~Meeting, data= Meetings_IDRA,
                       group = "IDRA") %>%
      addCircleMarkers(lng = ~longitude, lat = ~latitude,
                       radius = 1, stroke =1, fillOpacity = 0.7, color=~pal("Narcotics Anonymous"),
                       label = ~Meeting, data= Meetings_NarAnon,
                       group = "Narcotics Anonymous") %>%
      addCircleMarkers(lng = ~longitude, lat = ~latitude,
                       radius = 1, stroke =1, fillOpacity = 0.7, color=~pal("Pills Anonymous"),
                       label = ~Meeting, data= Meetings_PA,
                       group = "Pills Anonymous") %>%
      addCircleMarkers(lng = ~longitude, lat = ~latitude,
                       radius = 1, stroke =1, fillOpacity = 0.7, color=~pal("Refuge Recovery"),
                       label = ~Meeting, data= Meetings_RR,
                       group = "Refuge Recovery") %>%
      addCircleMarkers(lng = ~longitude, lat = ~latitude,
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
      addEasyButton(easyButton(
        icon="fa-globe", title="Zoom to Level 1",
        onClick=JS("function(btn, map){ map.setZoom(1); }"))) %>%
      addEasyButton(easyButton(
        icon="fa-crosshairs", title="Locate Me",
        onClick=JS("function(btn, map){ map.locate({setView: true}); }"))) %>%
      setView(lng = -93.645733, lat = 42.026234, zoom = 7)
  })

}

#----------------------------------------------#
shinyApp(ui = ui, server = server)
