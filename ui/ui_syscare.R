sidebar <- dashboardSidebar(
  
  sidebarMenu(
    
    menuItem("Project Descriptions", icon = icon(" fa-file-text "), tabName = "description"),
    
    
    menuItem("Syscare", icon = icon("leaf"),
             menuSubItem("Panel 1", tabName = "syscare_1", icon = icon("angle-right")),
             menuSubItem("Panel 2", tabName = "syscare_2", icon = icon("angle-right")),
             menuSubItem("Panel 3", tabName = "syscare_3", icon = icon("angle-right")),
             menuSubItem("Panel 4", tabName = "syscare_4", icon = icon("angle-right"))),
    
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
