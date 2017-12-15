# Server logic for hdi map 
server <- function(input, output, session) {
  # Groups are used to dynamically show/hide polygon layers, so we do not have to remove them (LeafletProxy didn't do the job)
  groups = c()
  
  # Main function to update the values displayed for each country after we change the hdi dimension with our dropdowns  
  countriesData <- function(hdi) {
      # Sometimes the last columns in hdi files have comments inside them. We strip those
      hdi[,-2] <- as.numeric(gsub("[^0-9.]", "", as.matrix(hdi[,-2])))
      countries <- countriesOrig
      # Sometimes names in our geodata and the hdi files do not match, so we need altnames 
      # Structure: Geodata name = HDI name
      altnames <- c("Ivory Coast"="Côte d'Ivoire", 
                    "Northern Cyprus"="Cyprus", 
                    "Democratic Republic of the Congo"="Congo \\(Democratic Republic of the\\)",
                    "North Korea"="Korea \\(Democratic People's Rep. of\\)",
                    "South Korea"="Korea \\(Republic of\\)",
                    "Laos"="Lao People's Democratic Republic"
      )
      
      thdi <- c()
      # We save our date with the names from the geodata, because we need to match those in the UI
      # Missing data is saved as NA 
      for (row in rownames(countries@data[,c("geounit", "name")])) {
        # Geodata has 2 kinds of unique "names". "name" and "geounit" 
        name <- countries@data[row, "name"]
        geounit <- countries@data[row, "geounit"]
        
        altname <- altnames[toString(geounit)]
        if(!is.na(altname)) {
          geounit <- altname
        }
        
        # search hdi file for geounit name
        idx <- grep(geounit, hdi[,"Country"], ignore.case = TRUE)

        # Sometimes similar names appears in a hdi file and give multiple matches
        # i.e. Niger/Nigeria or Congo/Congo (Democratic Republic of the)
        # In that case we do a complete match with trimmed whitespace
        if(length(idx) == 0) {
          # if we found no match for "geounit", maybe for "name"?
          idx <- grep(name, hdi[,"Country"], ignore.case = TRUE)
          
          if(length(idx) > 1) {
            idx <- match(name, trimws(hdi[,"Country"]))
          }
        } else if(length(idx) > 1) {
          idx <- match(geounit, trimws(hdi[,"Country"]))
        } 
        
        # When we have a match, we save the hdi index with the row key of the geodata
        # We need this structure for easy merging
        if(length(idx) != 0) {
          thdi[row] = idx
        } else {
          thdi[row] = NA
        }
      }
      # With this we insert all found hdi data into our geodata data frame
      countries@data <- data.frame(countries@data, hdi[thdi,])
      return(countries)
  }
  
  # Update the country data, whenever we change the dropdown input (Select another dimension)
  countriesSp <- reactive({
    # read.table will throw a few warnings we don't care about (NA coercion, wrong number of cols because of comments)
    suppressWarnings(countriesData(read.table(paste0("data/",isolate(input$dimensionSelect),"/",input$dataSelect), 
                      skip=1, strip.white=TRUE, header=TRUE, sep = ",", dec=".")))  #, stringsAsFactors=FALSE
  })
  
  # Because years in hdi files are not consistent, we need to update the available ones whenever
  # the dropdown input respectively the countryData changes
  years <- reactive({
    sub("X", "", grep("X\\d{4}", names(countriesSp()), value=TRUE))
  })

  # Same with current year. Most recent available year changes from file to file  
  currentYear <- reactive({
    if(input$year <= length(years())) {
      currentYear <- paste0("X", years()[input$year+1]) #paste0("X", input$year)
    } else if(input$year > length(years())) {
      currentYear <- paste0("X", max(years()))
    } else {
      currentYear <- paste0("X", max(years()))
    }
    currentYear
  })
  
  # Sometimes year slider gets buggy and returns a year, which isn't valid for current countryData
  # In this case we change the year to the most recent year available in the data
  currentCountryData <- reactive({
    if(currentYear() %in% names(countriesSp())) {
      countriesSp()@data[,currentYear()]
    } else {
      maxYear <- paste0("X", max(years()))
      countriesSp()@data[,maxYear]
    }
  })
  
  # Creates a colorpalette with 10 bins from the current data. Used for country polygons and legend
  colorpal <- reactive({
    bins <- c(0:10/10)
    colorNumeric("YlOrRd", domain = currentCountryData())
  })
  
  # With our 2-step dropdown file selection we need to change the second dropdown 
  # according to the selection in the first 
  observe({
    dimension <- input$dimensionSelect
    
    updateSelectInput(session, "dataSelect",
                      label = dimension,
                      choices = dataTree[dimension]
    )
  })
  
  # The available data for our linechar input field is just all available country names 
  observe({
    countryNames <- countriesSp()@data[,'geounit']
    updateSelectizeInput(session, "countryLineChart", choices = countryNames)
  })
  
  # Initializes our worldmap
  output$hdimap <- renderLeaflet({
    # Try to set the starting view more or less big enough to display the whole world, 
    # but small enough so we don't see multiple worldmaps
    box <- bbox(countriesOrig)
    box2 <- box
    
    box2[1] <- -180 #Je größer, desto nördlicher die südliche Grenze 
    box2[2] <- 180 #Je größer, desto nördlicher die nördliche Grenze 
    box2[3] <- 360 #Je größer, desto östlicher die östliche Grenze
    box2[4] <- -360 #Je größer, desto östlicher die westliche Grenze
    
    map <- leaflet(countriesOrig, options = leafletOptions(minZoom = 2, maxZoom = 8, maxBounds = box2))  %>%
      addTiles() %>%
      fitBounds(lng1 = box[1], lat1 = box[2]-2, lng2 = box[3], lat2 = box[4]+-2)
  })
  
  # Labels for hover over countries. Just a short name of the current dimension and it's value for the country
  labels <- reactive({
    short <- shortnames[match(isolate(input$dataSelect),shortnames[,"Filename"]),"Short"]
    sprintf(
      "<strong>%s</strong><br/>%s: %s",
      countriesSp()$name,  short, currentCountryData()
    ) %>% lapply(htmltools::HTML)
  })

  # Main observe for updating our worldmap. Use priority -1 so all other values can update before we start to 
  # change the visualization.
  observe(priority = -1,{
    pal <- colorpal()
    # Use the current year and hdi dimension to get a unique display group.
    # With this we can hide other groups and do not have to use clearShapes (Which removes all polygons for a split second)
    currentGroup <- paste0(input$dataSelect,currentYear())
    map <- leafletProxy("hdimap", data=countriesSp()) # build proxy based on current data
    if(!(currentGroup %in% names(groups[currentGroup]))) {
      # If we haven't displayed this group yet.
      #clearShapes() %>%
      addPolygons(
        map = map, # our map instance
        fillColor = pal(currentCountryData()), # use our custom color palette
        weight = 2, # Stroke width in px
        opacity = 1, # Stroke opacity
        color = "white", # Stroke color
        dashArray = "3", # dash pattern. 3px dash length, 3px dash gaps
        fillOpacity = 0.7, # fill opacity inside polygons
        layerId = c(1:length(countriesSp())), # give each country a layerId
        group = currentGroup, # set id of our current group
        highlight = highlightOptions( # Options for hover over countries
          weight = 5, # Make strokes thicker
          color = "#666", # And highlight borders
          dashArray = "", # Solid lines
          fillOpacity = 0.7, 
          bringToFront = TRUE),
        label = labels(), # Add our custom labels
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")
      )
    } else {
      # we already displayed it once, so we can simply reactivate it. 
      showGroup(map, currentGroup) 
    }
    # In the end hide all except the current group.
    groups[currentGroup] <<- TRUE
    hideGroup(map, names(groups[groups == FALSE]))
    groups[currentGroup] <<- FALSE
  })

  # Separate observer and proxy for update of label  
  observe({
    proxy <- leafletProxy("hdimap")
    
    # removing and adding new label is faster, so we can just clearControls()
    proxy %>% clearControls()
    # Again use our custom shortnames and color palette
    shortname <- shortnames[match(input$dataSelect,shortnames[,"Filename"]),"Short"]
    pal <- colorpal()
    
    # And use "No data" for all countries without data for current dimension
    proxy %>% addLegend(pal = pal, values = isolate(currentCountryData()), opacity = 0.7, 
    title = shortname, na.label="No data", position = "bottomright")
  })
  
  # Observe click on shapes (i.e., countries)
  observe({
    click <- input$hdimap_shape_click
    if(is.null(click)) {
      return()
    }

    # Find boundingbox of clicked country
    box <- bbox(countriesSp()[click$id,])
    
    # Put selected country into linechart input
    updateSelectizeInput(session, "countryLineChart", 
                         selected = countriesSp()@data[click$id,'geounit'])

    # Update map to zoom inside country boundaries
    leafletProxy("hdimap") %>%
      fitBounds(lng1 = box[1], lat1 = box[2], lng2 = box[3], lat2 = box[4])
  })
  
  # Update our yearSlider by inserting and replacing custom javascript inside the yearSlider uiOutput
  output$yearSliderUI <- renderUI({
    # Slider uses ionRangeSlider (https://github.com/IonDen/ion.rangeSlider)
    list((HTML(sprintf('
                        <script type="text/javascript">
                        $(document).ready(function() {
                        var vals = [%s];
                        $(\'#year\').data(\'ionRangeSlider\').update(
                        {values:vals,
                        min: %s,
                        max: %s,
                        from:%s})
                        })
                        </script>
                        ', toString(years()), min(years()), max(years()), min(years()))
    )))
  })

  # Update NVD3 linechart with our data.
  output$linechart <- renderLineChart({
    # Return a data frame. Each column will be a series in the line chart.
    if(!is.null(input$countryLineChart)) {
      first <- TRUE
      for(country in input$countryLineChart) {
        # We need to create our data frame with the correct amount of rows. All following columns can be appended
        if(first) {
          df <- data.frame(c(t(countriesSp()@data[match(country, countriesSp()@data[,"geounit"]),paste0("X",years())])))
          first <- FALSE
        } else {
          df <- data.frame(df, c(t(countriesSp()@data[match(country, countriesSp()@data[,"geounit"]),paste0("X",years())])))
        }
      }
      # rows = xAxis = time (years)
      row.names(df) <- years()
      # cols = line (category) = countries
      names(df) <- input$countryLineChart
      # values = yAxis
      df
    }
  })
  
  # Update NVD3 barchart with our data. Similar to linechart, but we now need multiple dimensions as well.
  output$barchart <- renderBarChart({
    if(!is.null(input$countryLineChart) && !is.null(input$dimensionBarChart)) {
      firstCountry <- TRUE
      # rows = xAxis = Countries 
      for(country in input$countryLineChart) {
        firstDim <- TRUE
        dimensionNames <- c()
        # columns = bars (categories) = dimensions
        for(dimensionFile in input$dimensionBarChart) {
          # Use shortnames as dimension names
          dimensionName <- shortFileNames[[dimensionFile]]
          
          dimensionNames <- c(dimensionNames, dimensionName)
          
          # Need to read all selected hdi files (dimensions)
          hdi <- read.table(paste0("data/",dimensionFile), 
                            skip=1, strip.white=TRUE, header=TRUE, sep = ",", dec=".")  #, stringsAsFactors=FALSE
          
          # And then generate the countryData the same way we would do for our worldmap
          cd <- suppressWarnings(countriesData(hdi))
          
          # Then find the data for the selected year, if there is no data set it to 'NA'
          if(currentYear() %in% names(cd)) {
            value <- cd@data[match(country, cd@data[,"geounit"]), currentYear()]
          } else {
            value <- 'NA'
          }
          # Again need to create data frame with correct number of rows first (1 row)
          if(firstDim) {
            dimensionData <- data.frame(c(t(value)))
            firstDim <- FALSE
          } else {
            dimensionData <- data.frame(dimensionData, c(t(value)))
          }
        }
        # Now we need to create the result with correct number of columns first
        if(firstCountry) {
          countryDimensionData <- dimensionData
          firstCountry <- FALSE
        } else {
          #Then we can use rbind to add rows to the final result.
          countryDimensionData <- rbind(countryDimensionData, dimensionData)
        }
      }
      # Finally set the correct dimension/column names and country/rownames
      colnames(countryDimensionData) <- dimensionNames
      rownames(countryDimensionData) <- input$countryLineChart
      countryDimensionData
    }
  })
}