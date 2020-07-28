#Load required Packages

packages <- c("dplyr", "purrr", "readr", "stringr", "readr", "readxl",
              "magrittr", "stringr", "ggplot2", "shiny", "sentimentr",
              "shinythemes", "shinydashboard", "shinydashboardPlus", "plotly", "leaflet", "sf", "DT")

for (pkgs in packages){
  if(!require(pkgs, character.only = TRUE)){ # Condition 
    install.packages(pkgs) # Install if not
    library(pkgs) # Load if installed
  }
}

#------------------------------------------------------------------------------------------#
data_hospitals <- hospitals #Df Change Name

counties <- ia_counties #Df Change Name


data_hospitals <-  data_hospitals %>% st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, agr = "identity") #create object sf

data_hospitals <- data_hospitals %>% filter(!is.na(BEDS)) #filter out NAs

combined <- st_join(data_hospitals, st_transform(counties, crs='+proj=longlat +datum=WGS84')) #Choose correct datum the user wants to use



county_counts <- combined %>% group_by(CO_FIPS) %>% summarise(bed_count = sum(BEDS)) #Create bed count column

county_counts <- county_counts %>% st_set_geometry(NULL) 

county_counts = left_join(st_transform(ia_counties, crs='+proj=longlat +datum=WGS84'), county_counts, by = "CO_FIPS") #Join dataframes

county_counts[is.na(county_counts$bed_count), "bed_count"] = 0 # Set all NAs to 0


county_counts <- county_counts %>% mutate(beds_percap = bed_count/POPESTIMATE2019) #Create beds_percap column


county_counts <- county_counts %>% mutate(beds_per1000 = beds_percap*1000) #Create beds_per1000 column
county_counts = county_counts %>% mutate(ln_beds_percap = log(beds_percap)) #Create beds_percap column


health_clinics <- health.clinics #Df change Name


datasets = c("Area Substance Abuse Council", "Community and Family Resources", "Southwest MHDS") #For Multi Layer Plot