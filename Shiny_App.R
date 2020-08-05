#Load required Packages

library(dplyr)
library(purrr)
library(stringr)
library(magrittr)
library(ggplot2)
library(shiny)

library(shinythemes)
library(shinydashboardPlus)
library(plotly)
library(leaflet)
library(sf)
 #packages <- c("dplyr", "purrr", "stringr", "readxl",
              # "magrittr", "stringr", "ggplot2", "shiny", "sentimentr",
             #  "shinythemes", "shinydashboard", "shinydashboardPlus", 
             #  "plotly", "leaflet", "sf")
 
 #for (pkgs in packages){
 # if(!require(pkgs, character.only = TRUE)){ # Condition 
    # install.packages(pkgs) # Install if not
   # library(pkgs) # Load if installed
  # }
# }

library(DT)
library(shinydashboardPlus)
library(shinydashboard)
library(mapproj)
library(DSPG)
library(tidyr)


#----------------------------------------------------------------------------------------------------------------------------------------------------------------------#

load("Syscare_Recovery_data.RData")


#data_hospitals <- hospitals #Df Change Name

#counties <- ia_counties #Df Change Name


#data_hospitals <-  data_hospitals %>% st_as_sf(coords = c("longitude", "latitude"), crs = 4326, agr = "identity") #create object sf

#data_hospitals <- data_hospitals %>% filter(!is.na(beds)) #filter out NAs

#combined <- st_join(data_hospitals, st_transform(counties, crs='+proj=longlat +datum=WGS84')) #Choose correct datum the user wants to use



#county_counts <- combined %>% group_by(co_fips) %>% summarise(bed_count = sum(beds)) #Create bed count column

#county_counts <- county_counts %>% st_set_geometry(NULL) 

#county_counts = left_join(st_transform(ia_counties, crs='+proj=longlat +datum=WGS84'), county_counts, by = "co_fips") #Join dataframes

#county_counts[is.na(county_counts$bed_count), "bed_count"] = 0 # Set all NAs to 0


#county_counts <- county_counts %>% mutate(beds_percap = bed_count/popestimate2019) #Create beds_percap column


#county_counts <- county_counts %>% mutate(beds_per1000 = beds_percap*1000) #Create beds_per1000 column
#county_counts = county_counts %>% mutate(ln_beds_percap = log(beds_percap)) #Create beds_percap column


#health_clinics <- health.clinics #Df change Name


#datasets = c("Area Substance Abuse Council", "Community and Family Resources", "Southwest MHDS") #For Multi Layer Plot





#df <- recovery



# Grouping Recovery centers data by type
#Recovery_Reco <- df[df$RecoveryHousing == "yes", ]
#Recovery_In <- df[df$InpatientTreatment == "yes", ]
#Recovery_Out <- df[df$OutpatientTreatment == "yes", ]


#df2 <- meetings #read.csv('All_Meetings_Geocoded.csv')


# Grouping Meetings data by type
#Meetings_AA <- df2[df2$type == "Alcoholics Anonymous", ]
#Meetings_AAnon <- df2[df2$type == "Al-anon", ]
#Meetings_AdultChildA <- df2[df2$type == "Adult children of alcoholic", ]
#Meetings_Celebrate <- df2[df2$type == "Celebrate", ]
#Meetings_Crush <- df2[df2$type == "CRUSH", ]
#Meetings_IDRA <- df2[df2$type == "Iowa Dual Recovery Anonymous (IDRA)", ]
#Meetings_NA <- df2[df2$type == "Narcotics Anonymous", ]
#Meetings_NAnon <- df2[df2$type == "Nar-Anon", ]
#Meetings_PA <- df2[df2$type == "Pills Anonymous", ]
#Meetings_RR <- df2[df2$type == "Refuge Recovery", ]
#Meetings_Smart <- df2[df2$type == "SMART", ]



#meetings_county <- meetings %>%
  #group_by(county) %>%
  #summarise(count = n())

#ia_counties_2 <- ia_counties %>%
  #select(county, popestimate2019)

#counties <- map_data('county', region = "Iowa") 

#counties <- counties %>% 
  #rename(county = subregion)

#join county map data with employment data
#meetings_county <- left_join(ia_counties_2, meetings_county)

#Set NAs to 0 (really are 0)
#meetings_county[is.na(meetings_county)] <- 0

#Figure Per 10,000 People Rate
#meetings_county <- meetings_county %>%
  #mutate(meeting_rate = count / popestimate2019 * 10000) %>%
  #mutate(county = tolower(county))

#Merge With counties shapes
#meetings_county <- left_join(meetings_county, counties)


#topCities  <- meetings %>%
  #filter(city %in% c("Des Moines", "Sioux City", "Cedar Rapids", "Davenport",
                     #"Iowa City", "Ames", "Council Bluffs", "Dubuque"))

#topCities$city <- ordered(topCities$city, c("Ames", "Council Bluffs", "Dubuque", "Iowa City",
                                          #  "Davenport", "Cedar Rapids", "Sioux City", "Des Moines"))

#topCities_pop <- left_join(topCities,ia_cities %>% select(-geometry),by="city")


#head(df2)
#df2$county <- as.factor(df2$county)
#df2$time <- as.factor(df2$time)
#df2$ampm <- as.factor(df2$ampm)
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------#
sidebar <- dashboardSidebar(
  
  sidebarMenu(
    
    menuItem("Project Descriptions", icon = icon(" fa-file-text "), tabName = "description"),
    
    
    menuItem("Syscare", icon = icon("leaf"),
             menuSubItem("Work and Community Resources", tabName = "syscare_1", icon = icon("angle-right")),
             menuSubItem("Clinical Health", tabName = "syscare_2", icon = icon("angle-right")),
             menuSubItem("Mental Health Resources", tabName = "syscare_3", icon = icon("angle-right")),
             menuSubItem("Substance Use Resources", tabName = "syscare_4", icon = icon("angle-right"))),
    
    menuItem("Recovery", icon = icon("leaf"),
             menuSubItem("About", tabName = "recovery_1", icon = icon("angle-right")),
             menuSubItem("Recovery Centers", tabName = "recovery_2", icon = icon("angle-right")),
             menuSubItem("Recovery Meetings", tabName = "recovery_3", icon = icon("angle-right"))),
    
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
    
    
    tabItem(tabName = 'syscare_1',  #Panel 4#######################################################
            
            fluidRow(
              
              box(
                title = "Community Resources",
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
                    leafletOutput("resource", height = "500px")
                  )
                )
              )
            )
            
            
    ),
    
    tabItem(tabName = 'syscare_2',  #Panel 4#######################################################
            
            fluidRow(
              
              box(
                title = "Medical Facility locations",
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
                    leafletOutput("health", height = "500px")
                  )
                )
              )
            ),
            
            fluidRow(
              
              box(
                title = "Medical Facility locations",
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
                    leafletOutput("health_2", height = "500px")
                  )
                )
              )
            ),
            
            fluidRow(
              
              box(
                title = "Medical Facility locations",
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
                    leafletOutput("health_3", height = "500px")
                  )
                )
              )
            )
            
            
    ),
    
    tabItem(tabName = 'syscare_3',  #Panel 4#######################################################
            
            fluidRow(
              
              box(
                title = "Mental Health services Locations",
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
                    leafletOutput("mental", height = "500px")
                  )
                )
              )
            )
            
            
    ),
    
    tabItem(tabName = 'syscare_4',  #Panel 4#######################################################
            
            fluidRow(
              
              box(
                title = "Substance Abuse Management locations",
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
                    leafletOutput("substance", height = "500px")
                  )
                )
              )
            )
            
            
    ),
    
    tabItem(tabName = "recovery_1",
            fluidRow(
              boxPlus(
                title = "Project Overview",
                closable = FALSE,
                width = NULL,
                status = "warning",
                solidHeader = TRUE,
                collapsible = TRUE,
                h1("About Recovery"),
                h2("Project Description"),
                p("Information about team members and their photos")

              )
            )),
    
  
    
    tabItem(tabName = 'recovery_2',  
            
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
    
    tabItem(tabName = 'recovery_3',  
            
            fluidRow(
              
              box(
                title = "Recovery meeting data",
                status = "warning",
                width = NULL,
                fluidRow(
                  column(
                    width = 3,
                    boxPad(
                      color = "grey",
                      
                      # selectInput(inputId = "county_meetings", label = strong("Select a County"),
                      #             choices = unique(df2$county),
                      #             selected = NULL),
                      
                      # selectInput(inputId = "dayz", label = strong("Select a Day"),
                      #             choices = unique(df2$day),
                      #             selected = NULL),
                      
                      #selectInput(inputId = "time1", label = strong("Select a Time"),
                                  #choices = unique(df2$time),
                                  #selected = NULL),
                      
                      #selectInput(inputId = "time2", label = strong("Select Morning or Evening"),
                                  #choices = unique(df2$ampm),
                                  #selected = NULL),
                      
                      sliderInput(inputId = 'time', label = strong("Select Meeting Times"),
                                  min = now(), max = now()+days(7), value = c(now(), now()+hours(24)))
                    )
                  ),
                  column(
                    width = 9,
                    leafletOutput("change3", height = "500px")
                  )
                )
              )
            ),
            
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
                    plotlyOutput("change4", height = "500px")
                  )
                )
              )
            ),
            
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
                    plotlyOutput("change5", height = "500px")
                  )
                )
              )
            ),
            
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
                    plotlyOutput("change6", height = "500px")
                  )
                )
              )
            ),
            
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
                    plotlyOutput("change7", height = "500px")
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
  
  
  #---------------------------------------------------------------------------------------------------#
  filtered_data_meetings <- reactive({
    
    
    df2 <- get_meetings(from = input$time[1], to =  input$time[2])
    
    df2
    

  })
  
  

  
  
  
  
  Meetings_AA_data <- reactive({
    filtered_data_meetings() %>% 
      filter(type == "Alcoholics Anonymous")
    
  })
  
  
 
  
   Meetings_AAnon_data <- reactive({
     filtered_data_meetings() %>%
       filter(type == "Al-anon")
   })
   
  
   Meetings_AdultChildA_data <- reactive({
     filtered_data_meetings() %>%
      filter(type == "Adult children of alcoholic")
   })
  
  
   Meetings_Celebrate_data <- reactive({
     filtered_data_meetings() %>%
       filter(type == "Celebrate")
   })
  
   Meetings_Crush_data <- reactive({
     filtered_data_meetings() %>%
      filter(type == "CRUSH")
     
   })
   
    Meetings_IDRA_data <- reactive({
      filtered_data_meetings() %>%
        filter(type == "Iowa Dual Recovery Anonymous (IDRA)")
    })
    
   
    Meetings_NA_data <- reactive({
      filtered_data_meetings() %>%
        filter(type == "Narcotics Anonymous")
    })
   
   
    Meetings_NAnon_data <- reactive({
      filtered_data_meetings() %>%
        filter(type == "Nar-Anon")
    })
   
   Meetings_PA_data <- reactive({
     filtered_data_meetings() %>%
      filter(type == "Pills Anonymous")
   })
   
  
   Meetings_RR_data <- reactive({
     filtered_data_meetings() %>%
       filter(type == "Refuge Recovery")
     
  }) 
  
  
   Meetings_Smart_data <- reactive({
     filtered_data_meetings() %>%
      filter(type == "SMART")
  })


  
  #-----------------------------------------------------------------------------------------------------#
  
  output$resource <- renderLeaflet({
    
    one = makeIcon(
      iconUrl = "stuff/Icons/1.png",
      iconWidth = 10, iconHeight = 10
    )
    
    two = makeIcon(
      iconUrl = "stuff/Icons/2.png",
      iconWidth = 10, iconHeight = 10
    )
    
    three = makeIcon(
      iconUrl = "stuff/Icons/3.png",
      iconWidth = 10, iconHeight = 10
    )
    
    four = makeIcon(
      iconUrl = "stuff/Icons/4.png",
      iconWidth = 10, iconHeight = 10
    )
    
    five = makeIcon(
      iconUrl = "stuff/Icons/5.png",
      iconWidth = 10, iconHeight = 10
    )
    
    six = makeIcon(
      iconUrl = "stuff/Icons/6.png",
      iconWidth = 10, iconHeight = 10
    )
    
    seven = makeIcon(
      iconUrl = "stuff/Icons/7.png",
      iconWidth = 10, iconHeight = 10
    )
    
    
    
    
    datasets = c("Iowa 211", "Churches", "Childcare Providers", "Colleges and Universities", "Iowa Works Offices")
    colors <- RColorBrewer::brewer.pal(n = length(datasets), name="Dark2")
    
    pal <- colorFactor(
      palette = colors,
      levels = datasets)
    
    
    
    
    # Create point map with the regional datasets colored differently
    leaflet() %>%
      addTiles() %>%
      addPolygons(data = st_transform(ia_counties, crs='+proj=longlat +datum=WGS84'),
                  weight = 1, color="#333333") %>%
      addMarkers(lng = ~longitude, lat = ~latitude, label = ~name, icon = one,
                 popup = paste0(iowa_211$name, "<br>", iowa_211$search_address, "<br>", iowa_211$phone),
                 data = iowa_211,
                 group = datasets[1]) %>%
      addMarkers(label = ~name, icon = two,
                 popup = paste0(churches$name, "<br>", churches$description),
                 data = churches,
                 group = datasets[2]) %>%
      addMarkers(lng = ~longitude, lat = ~latitude, label = ~provider_business_name, icon = three,
                 popup = paste0(childcare$provider_business_name, "<br>", childcare$search_address, "<br>", childcare$phone),
                 data = childcare,
                 group = datasets[3]) %>%
      addMarkers(lng = ~longitude, lat = ~latitude, label = ~name, icon = four,
                 popup = paste0(colleges$name, "<br>", colleges$city, "<br>", colleges$type),
                 data = colleges,
                 group = datasets[4]) %>%
      addMarkers(lng = ~Longitude, lat = ~Latitude, label = ~NAME, icon = five,
                 popup = paste0(iowaworks$NAME, "<br>", iowaworks$formatted_, "<br>", iowaworks$PHONE, "<br>", iowaworks$LINK),
                 data = iowaworks,
                 group = datasets[5]) %>%
      addLayersControl(
        overlayGroups = datasets,
        options = layersControlOptions(collapsed = TRUE)
      ) %>%
      addLegend(
        position = c("bottomleft"), pal = pal, values = datasets, opacity = 0.9
      ) %>%
      setView(lng = -93.645733, lat = 42.026234, zoom = 7)
    
  })
  
#---------------------------------------------------------------------#
#---------------------------------------------------------------------#
  
  output$health <- renderLeaflet({
    
    one = makeIcon(
      iconUrl = "stuff/Icons/1.png",
      iconWidth = 10, iconHeight = 10
    )
    
    two = makeIcon(
      iconUrl = "stuff/Icons/2.png",
      iconWidth = 10, iconHeight = 10
    )
    
    three = makeIcon(
      iconUrl = "stuff/Icons/3.png",
      iconWidth = 10, iconHeight = 10
    )
    
    four = makeIcon(
      iconUrl = "stuff/Icons/4.png",
      iconWidth = 10, iconHeight = 10
    )
    
    five = makeIcon(
      iconUrl = "stuff/Icons/5.png",
      iconWidth = 10, iconHeight = 10
    )
    
    six = makeIcon(
      iconUrl = "stuff/Icons/6.png",
      iconWidth = 10, iconHeight = 10
    )
    
    seven = makeIcon(
      iconUrl = "stuff/Icons/7.png",
      iconWidth = 10, iconHeight = 10
    )
    
    
    
    
    datasets = c("Rural Health Clinics", "Iowa Hospitals", "VA Clinics", "Medical Facilities")
    colors <- RColorBrewer::brewer.pal(n = length(datasets), name="Dark2")
    
    pal <- colorFactor(
      palette = colors,
      levels = datasets)
    
    
    
    
    # Create point map with the regional datasets colored differently
    leaflet() %>%
      addTiles() %>%
      addPolygons(data = st_transform(ia_counties, crs='+proj=longlat +datum=WGS84'),
                  weight = 1, color="#333333") %>%
      addMarkers(label = ~name, icon = four,
                 popup = paste0(hospital_buildings$name),
                 data = hospital_buildings,
                 group = datasets[4]) %>%
      addMarkers(lng = ~longitude, lat = ~latitude, label = ~name, icon = one,
                 popup = paste0(health.clinics$name, "<br>", health.clinics$address),
                 data = health.clinics,
                 group = datasets[1]) %>%
      addMarkers(lng = ~longitude, lat = ~latitude, label = ~name, icon = two,
                 popup = paste0(hospitals$name, "<br>", hospitals$type, "<br>", hospitals$address, "<br>", hospitals$website),
                 data = hospitals,
                 group = datasets[2]) %>%
      addMarkers(lng = ~longitude, lat = ~latitude, label = ~facility, icon = three,
                 popup = paste0(va_medical_centers$facility, "<br>", va_medical_centers$address, "<br>", va_medical_centers$phone),
                 data = va_medical_centers,
                 group = datasets[3]) %>%
      addLayersControl(
        overlayGroups = datasets,
        options = layersControlOptions(collapsed = TRUE)
      ) %>%
      addLegend(
        position = c("bottomleft"), pal = pal, values = datasets, opacity = 0.9
      ) %>%
      setView(lng = -93.645733, lat = 42.026234, zoom = 7)
    

  })
  
  output$health_2 <- renderLeaflet({
    
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
   
    
    
  })
  
  
  output$health_3 <- renderLeaflet({
  
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
    
    
  })
  
#--------------------------------------------------------------#
#--------------------------------------------------------------#
  
  output$mental <- renderLeaflet({
    
    one = makeIcon(
      iconUrl = "stuff/Icons/1.png",
      iconWidth = 10, iconHeight = 10
    )
    
    two = makeIcon(
      iconUrl = "stuff/Icons/2.png",
      iconWidth = 10, iconHeight = 10
    )
    
    three = makeIcon(
      iconUrl = "stuff/Icons/3.png",
      iconWidth = 10, iconHeight = 10
    )
    
    four = makeIcon(
      iconUrl = "stuff/Icons/4.png",
      iconWidth = 10, iconHeight = 10
    )
    
    five = makeIcon(
      iconUrl = "stuff/Icons/5.png",
      iconWidth = 10, iconHeight = 10
    )
    
    six = makeIcon(
      iconUrl = "stuff/Icons/6.png",
      iconWidth = 10, iconHeight = 10
    )
    
    seven = makeIcon(
      iconUrl = "stuff/Icons/7.png",
      iconWidth = 10, iconHeight = 10
    )
    
    
    
    
    datasets = c("MHDS Regional Access Points", "South Central Iowa Resource Directory Locations", "Southwest Iowa Resource Directory Locations")
    colors <- RColorBrewer::brewer.pal(n = length(datasets), name="Dark2")
    
    pal <- colorFactor(
      palette = colors,
      levels = datasets)
    
    
    
    
    # Create point map with the regional datasets colored differently
    leaflet() %>%
      addTiles() %>%
      addPolygons(data = st_transform(ia_counties, crs='+proj=longlat +datum=WGS84'),
                  weight = 1, color="#333333") %>%
      addMarkers(lng = ~longitude, lat = ~latitude, label = ~name, icon = one,
                 popup = paste0(regional_MHDS$name, "<br>", regional_MHDS$regional_org, "<br>", regional_MHDS$street_address, "<br>", regional_MHDS$phone, "<br>", regional_MHDS$source),
                 data = regional_MHDS,
                 group = datasets[1]) %>%
      addMarkers(lng = ~longitude, lat = ~latitude, label = ~agency, icon = two,
                 popup = paste0(scbhr_mhds$agency, "<br>", scbhr_mhds$category, "<br>", scbhr_mhds$address, "<br>", scbhr_mhds$phone, "<br>", scbhr_mhds$website),
                 data = scbhr_mhds,
                 group = datasets[2]) %>%
      addMarkers(lng = ~longitude, lat = ~latitude, label = ~`service title`, icon = three,
                 popup = paste0(southwest_mhds$`service title`, "<br>", southwest_mhds$category, "<br>", southwest_mhds$address, "<br>", southwest_mhds$phone, "<br>", southwest_mhds$website),
                 data = southwest_mhds,
                 group = datasets[3]) %>%
      addLayersControl(
        overlayGroups = datasets,
        options = layersControlOptions(collapsed = TRUE)
      ) %>%
      addLegend(
        position = c("bottomleft"), pal = pal, values = datasets, opacity = 0.9
      ) %>%
      setView(lng = -93.645733, lat = 42.026234, zoom = 7)
 
  })
  
  output$substance <- renderLeaflet({
    
    one = makeIcon(
      iconUrl = "stuff/Icons/1.png",
      iconWidth = 10, iconHeight = 10
    )
    
    two = makeIcon(
      iconUrl = "stuff/Icons/2.png",
      iconWidth = 10, iconHeight = 10
    )
    
    three = makeIcon(
      iconUrl = "stuff/Icons/3.png",
      iconWidth = 10, iconHeight = 10
    )
    
    four = makeIcon(
      iconUrl = "stuff/Icons/4.png",
      iconWidth = 10, iconHeight = 10
    )
    
    five = makeIcon(
      iconUrl = "stuff/Icons/5.png",
      iconWidth = 10, iconHeight = 10
    )
    
    six = makeIcon(
      iconUrl = "stuff/Icons/6.png",
      iconWidth = 10, iconHeight = 10
    )
    
    seven = makeIcon(
      iconUrl = "stuff/Icons/7.png",
      iconWidth = 10, iconHeight = 10
    )
    
    
    
    
    datasets = c("Drug Drop Off Sites", "Medicated Assisted Treatment (MAT) Locations", "Regional Substance Use Treatment Facilities", "Substance Use/Problem Gambling Recovery")
    colors <- RColorBrewer::brewer.pal(n = length(datasets), name="Dark2")
    
    pal <- colorFactor(
      palette = colors,
      levels = datasets)
    
    
    
    
    # Create point map with the regional datasets colored differently
    leaflet() %>%
      addTiles() %>%
      addPolygons(data = st_transform(ia_counties, crs='+proj=longlat +datum=WGS84'),
                  weight = 1, color="#333333") %>%
      addMarkers(lng = ~longitude, lat = ~latitude, label = ~name, icon = one,
                 popup = paste0(Rx_Drop_Off_Locations$name, "<br>", Rx_Drop_Off_Locations$type, "<br>", Rx_Drop_Off_Locations$search_address, "<br>", Rx_Drop_Off_Locations$phone),
                 data = Rx_Drop_Off_Locations,
                 group = datasets[1]) %>%
      addMarkers(lng = ~longitude, lat = ~latitude, label = ~doctor, icon = two,
                 popup = paste0(mat_locations$center, "<br>", mat_locations$doctor, "<br>", mat_locations$search_address, "<br>", mat_locations$phone),
                 data = mat_locations,
                 group = datasets[2]) %>%
      addMarkers(lng = ~longitude, lat = ~latitude, label = ~location_name, icon = three,
                 popup = paste0(regional_substance_treatment$location_name, "<br>", regional_substance_treatment$regional_org, "<br>", regional_substance_treatment$search_address, "<br>", regional_substance_treatment$phone, "<br>", regional_substance_treatment$source),
                 data = regional_substance_treatment,
                 group = datasets[3]) %>%
      addMarkers(lng = ~longitude, lat = ~latitude, label = ~name, icon = two,
                 popup = paste0(sud$name, "<br>", sud$address, "<br>", sud$phone),
                 data = sud,
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
  
  
  

  #-----------------------------------------------------------------------------------------------------#
  
  
  output$change2 <- renderLeaflet({
    
    filter_checkbox <- c("Recovery Housing", "Inpatient Treatment", "Outpatient Treatment")
    
    # Color palette
    colors <- RColorBrewer::brewer.pal(n = length(filter_checkbox), name="Dark2")
    
    label <- paste(sep="<br>",
                   df$Name,
                   df$Address,
                   df$Phone)
    
    
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
                       radius = 1, color = ~pal("Recovery Housing"), fillOpacity = 0.5,
                       popup = label, label= ~Name, data = Recovery_Reco,
                       group = "Recovery Housing") %>%
      addCircleMarkers(lng = ~lon, lat = ~lat,
                       radius = 1, color = ~pal("Inpatient Treatment"), fillOpacity = 0.5,
                       popup = label, label= ~Name, data = Recovery_In,
                       group = "Inpatient Treatment") %>%
      addCircleMarkers(lng = ~lon, lat = ~lat,
                       radius = 1, color = ~pal("Outpatient Treatment"), fillOpacity = 0.5,
                       popup = label, label= ~Name, data = Recovery_Out,
                       group = "Outpatient Treatment") %>%
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
    
    meetingtime <- paste(sep = " ",
                         df$day,
                         df$time,
                         df$ampm)
    
    label <- paste(sep="<br>",
                   df$meeting,
                   df$address,
                   meetingtime)
    
    pal <- colorFactor(
      palette = colors,
      levels = filter_checkbox)
    
    # Create icons
    Icon_AdultChild <- makeIcon(
      iconUrl = "stuff/Icons/Icon_AdultChildren.png",
      iconWidth = 10, iconHeight = 10
    )
    Icon_AlcAnonymous <- makeIcon(
      iconUrl = "stuff/Icons/Icon_AlcAnonymous.png",
      iconWidth = 10, iconHeight = 10
    )
    Icon_AlAnon <- makeIcon(
      iconUrl = "stuff/Icons/Icon_AlAnon.png",
      iconWidth = 10, iconHeight = 10
    )
    Icon_Celebrate <- makeIcon(
      iconUrl = "stuff/Icons/Icon_Celebrate.png",
      iconWidth = 10, iconHeight = 10
    )
    Icon_Crush <- makeIcon(
      iconUrl = "stuff/Icons/Icon_Crush.png",
      iconWidth = 10, iconHeight = 10
    )
    Icon_IDRA <- makeIcon(
      iconUrl = "stuff/Icons/Icon_IDRA.png",
      iconWidth = 10, iconHeight = 10
    )
    Icon_NarAnon <- makeIcon(
      iconUrl = "stuff/Icons/Icon_NarAnon.png",
      iconWidth = 10, iconHeight = 10
    )
    Icon_NarcAnonymous <- makeIcon(
      iconUrl = "stuff/Icons/Icon_NarcoticsAnonymous.png",
      iconWidth = 10, iconHeight = 10
    )
    Icon_PillsAnonymous <- makeIcon(
      iconUrl = "stuff/Icons/Icon_PillsAnonymous.png",
      iconWidth = 10, iconHeight = 10
    )
    Icon_Refuge <- makeIcon(
      iconUrl = "stuff/Icons/Icon_Refuge.png",
      iconWidth = 10, iconHeight = 10
    )
    Icon_Smart <- makeIcon(
      iconUrl = "stuff/Icons/Icon_Smart.png",
      iconWidth = 10, iconHeight = 10
    )
    
    # Edit starting dataframe and input for lng and lat inside addCircleMarkers to change data used
    filtered_data_meetings() %>%
      leaflet() %>%
      addTiles() %>%
      addPolygons(data = st_transform(ia_counties, crs='+proj=longlat +datum=WGS84'),
                  weight = 1, color="#333333") %>%
      addMarkers(lng = ~longitude, lat = ~latitude,
                 icon = Icon_AlcAnonymous, 
                 popup=label, label = ~meeting, data= Meetings_AA_data(),
                 group = "Alcoholics Anonymous") %>%
      addMarkers(lng = ~longitude, lat = ~latitude,
                 icon=Icon_AlAnon,
                 popup=label, label = ~meeting, data= Meetings_AAnon_data(),
                 group = "Al-anon") %>%
      addMarkers(lng = ~longitude, lat = ~latitude,
                 icon = Icon_AdultChild,
                 popup=label, label = ~meeting, data= Meetings_AdultChildA_data(),
                 group = "Adult children of alcoholic") %>%
      addMarkers(lng = ~longitude, lat = ~latitude, 
                 icon=Icon_Celebrate,
                 popup=label, label = ~meeting, data= Meetings_Celebrate_data(),
                 group = "Celebrate") %>%
      addMarkers(lng = ~longitude, lat = ~latitude,
                 icon=Icon_Crush,
                 popup=label, label = ~meeting, data = Meetings_Crush,
                 group = "CRUSH") %>%
      addMarkers(lng = ~longitude, lat = ~latitude,
                 icon=Icon_IDRA,
                 popup=label, label = ~meeting, data = Meetings_IDRA,
                 group = "Iowa Dual Recovery Anonymous (IDRA)") %>%
      addMarkers(lng = ~longitude, lat = ~latitude,
                 icon = Icon_NarAnon,
                 popup=label, label = ~meeting, data= Meetings_NA_data(),
                 group = "Narcotics Anonymous") %>%
      addMarkers(lng = ~longitude, lat = ~latitude,
                 icon=Icon_NarAnon,
                 popup=label, label = ~meeting, data= Meetings_NAnon,
                 group = "Nar-Anon") %>%
      addMarkers(lng = ~longitude, lat = ~latitude,
                 icon=Icon_PillsAnonymous,
                 popup=label, label = ~meeting, data= Meetings_PA,
                 group = "Pills Anonymous") %>%
      addMarkers(lng = ~longitude, lat = ~latitude,
                 icon=Icon_Refuge,
                 popup=label, label = ~meeting, data= Meetings_RR,
                 group = "Refuge Recovery") %>%
      addMarkers(lng = ~longitude, lat = ~latitude,
                 icon=Icon_Smart,
                 popup=label, label = ~meeting, data= Meetings_Smart_data(),
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
  
  
  #mymap_proxy <- leafletProxy("change3")
  
  #observe({
    #fdata <- filtered_data_meetings()
    #mymap_proxy %>%
      #clearMarkers() %>%
      #addMarkers(lng = fdata$longitude, lat = fdata$latitude) %>%
      #flyTo(lng = fdata$longitude, lat = fdata$latitude, zoom = 10)
  #})
  

 output$change4 <- renderPlotly({
   
      
      plot <- ggplot() +
        geom_polygon(data = meetings_county, aes(x = long, y = lat, group = group, fill = meeting_rate), color = "black") +
        coord_map() +
        ggtitle("Recovery Support Meetings Per 10,000 People") +
        theme(plot.title = element_text(hjust = 0.5)) +
        xlab("") +
        ylab("")
      
      plot <- plot + 
        theme_bw() + 
        theme(plot.title = element_text(hjust = 0.5), panel.border = element_blank(), panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"), axis.text = element_blank(), axis.ticks = element_blank())
      
      plot2 <- plot + scale_fill_gradient(low = "#e5f5e0", high = "#31a354") +
        labs(fill = "Meetings Per\n10,000 People")
      
      ggplotly(plot2)
    
  })
 
 output$change5 <- renderPlotly({
   

     ggplotly(ggplot(meetings %>%
              separate(col = time, into = c("Hour", "Minute"), sep = "\\:") %>%
              mutate(Hour = as.numeric(Hour)) %>%
              mutate(DayNight = ifelse(ampm == "am" | Hour < 5, "Day", "Evening")) %>%
              mutate(DayNight = as.factor(DayNight)) %>%
              filter(is.na(DayNight)==FALSE), aes(x = day, fill = type)) +
     geom_bar() +
     facet_wrap(~DayNight) +
     ylab("Count") +
     ggtitle("Number of Meetings Each Day by Type") +
     theme(axis.text.x = element_text(angle = 90)))
   
   
 })
 
 output$change6 <- renderPlotly({
   
  
    ggplotly(ggplot(topCities) +
     geom_bar(aes(x = city, fill = city)) +
     coord_flip() +
     ggtitle("Number of Weekly Meetings By City") +
     theme(legend.position = "none")+labs(x="Top Cities"))
   
   
   
 })
 
 output$change7 <- renderPlotly({
   
     ggplotly(ggplot(topCities_pop %>% group_by(city,currentPop) %>% summarise(count=n()) %>% 
              mutate(meetings_per_10k=count*10000/currentPop))+
     geom_bar(aes(x=reorder(city,meetings_per_10k),y=meetings_per_10k),stat = "identity",fill = "#31A354")+
     coord_flip()+ggtitle("Number of Weekly Meetings, Top Cities, per 10000 people")+
     theme(legend.position = "none")+labs(x="Top Cities in Iowa",y="Meetings per 10000 people"))
   
 })

}

#----------------------------------------------#
shinyApp(ui = ui, server = server)
