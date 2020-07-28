#Sidebar for panel selection


sidebar <- dashboardSidebar(
  
  sidebarMenu(
    menuItem("Project Description", icon = icon("diagnoses"), tabName = "description"),
    menuItem("Hospitals", icon = icon("map"), tabName = 'hospitals'),
    menuItem("Health Clinics", icon = icon("leaf"), tabName = "health"),
    menuItem("Iowa Works", icon = icon("leaf"), tabName = "iaworks"),
    menuItem("Multi Layer Plotting", icon = icon("layer-group"), tabName = "multiple")
  )
  
)

#--------------------------------------------------------------------------------------------#

#Main body with multiple panel options

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
                h1("2020 DSPG Syscare"),
                
                h2("Project Description"),
                p("Formal Systems of Care Infrastructure"),
                
                h2("Project Goals"),
                p("The larger objectives of this contract is lower substance use relapse among Iowan’s by strengthening and expanding substanceabuse recovery services, infrastructure, and programming in Iowa. 
                  The specific goal of the project is to facilitate the development of Recovery Community Centers in Iowa."),
                
                h2("Our Approach"),
                p("To identify data sources containing geospatial information regardingformal systems of care (FSC) and community resources that are beneficial to people recovering from a significant shock that causes harm on multiple fronts (mental, physical, emotional, and spiritual wellbeing, employment and\or marital status, housing security, incarceration, etc.). 
                  Steps involved: 
                  
                  a) Identify FSCs in Iowa
                  b) Collect metadata on FSCs (e.g. name, address, type of care)
                  c) Consolidatethe data together in a structured database that supports statistical and spatial analysis"),
                
                h2("Ethical Considerations"),
                p("We took the utmost caution when it came to the privacy of our clients data.")
            )
  )
  
),


tabItem(tabName = 'hospitals', #Panel 1
        
        
        fluidRow(
          
          box(
            title = "Iowa Hospitals Information System",
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

tabItem(tabName = 'health',  #Panel 2
        
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
                              choices = unique(health_clinics$COUNTY),
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

tabItem(tabName = 'iaworks',  #Panel 3
        
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
                  
                  selectInput(inputId = "county3", label = strong("Select a County"),
                              choices = unique(health_clinics$COUNTY),
                              selected = NULL)
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

tabItem(tabName = 'multiple',  #Panel 4
        
        fluidRow(
          
          box(
            title = "To be continued",
            status = "warning",
            width = NULL,
            fluidRow(
              column(
                width = 3,
                boxPad(
                  color = "grey"#,
                  
                  #selectInput(inputId = "county3", label = strong("Select a County"),
                  # choices = unique(health_clinics$COUNTY),
                  #selected = NULL)
                )
              ),
              column(
                width = 9,
                leafletOutput("multiple_leafy", height = "500px")
              )
            )
          )
        ))


)
  )


#-------------------------------------------------------------------------------------------------#

ui <- dashboardPage(
  dashboardHeader(title = "Syscare Shiny App"),
  sidebar = sidebar,
  body = body,
  skin = "black"
)
