# Any code in this file is guaranteed to be called before either
# ui.R or server.R

library(shiny)
library(leaflet)
library(tools)
library(sp)
source("linechart.R")
source("barchart.R")

dimensions <- list.files("data")
dataTree <- list()
allCategories <- list()
shortFileNames <- list()
# Loop through "data" directory 
for(dimension in dimensions) {
  dataTree[[dimension]] <- list()
  # Loop through every directory inside "data"
  for(file in list.files(paste0("data/", dimension))) {
    # Save filenames with short names as key for display in dropdown inputs
    dataTree[[dimension]][file_path_sans_ext(file)] <- file
    # Depending on what kind of values we need later, these help us with looking up the correct files/shortnames
    allCategories[file_path_sans_ext(file)] <- paste0(dimension, "/",file)
    shortFileNames[paste0(dimension, "/",file)] <- file_path_sans_ext(file)
  }
}

# Load geodata and shortnames once, because it doesn't change
countriesOrig <- geojsonio::geojson_read("json/custom.geo.json", what="sp")
shortnames <- read.table("config/shortnames.csv", 
                         strip.white=TRUE, header=TRUE, sep = ",")  #, stringsAsFactors=FALSE