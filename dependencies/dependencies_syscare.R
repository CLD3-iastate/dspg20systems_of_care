#Load required Packages

packages <- c("dplyr", "purrr", "readr", "stringr", "readr", "readxl",
              "magrittr", "stringr", "ggplot2", "shiny", "sentimentr",
              "shinythemes", "shinydashboard", "shinydashboardPlus", "plotly", "leaflet", "sf")

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





df <- read.csv('Reco_Data.csv')

df = df[-c(144),]


# Grouping Recovery centers data by type
Recovery_SU <- df[df$ServiceType == "SubstanceUseTreatment", ]
Recovery_RH <- df[df$ServiceType == "RecoveryHousing", ]
Recovery_S <- df[df$ServiceType == "Shelters", ]


df2 <- read.csv('All_Meetings_Geocoded.csv')


# Grouping Meetings data by type
Meetings_AA <- filter(df2, df2$Type %in% c("Alcoholics Anonymous", "Al-anon"))
Meetings_AdultChildA <- df2[df2$Type == "Adult children of alcoholic", ]
Meetings_Celebrate <- df2[df2$Type == "Celebrate", ]
Meetings_Crush <- df2[df2$Type == "CRUSH", ]
Meetings_IDRA <- df2[df2$Type == "IDRA", ]
Meetings_NarAnon <- filter(df2, df2$Type %in% c("Narcotics Anonymous", "Nar-Anon"))
Meetings_PA <- df2[df2$Type == "Pills Anonymous", ]
Meetings_RR <- df2[df2$Type == "Refuge Recovery", ]
Meetings_Smart <- df2[df2$Type == "SMART", ]