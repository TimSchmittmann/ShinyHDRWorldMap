# All of the UI elements go into the ui variable. Using fillPage, because we need all the space we can get.
ui <- fillPage(
  # include files inside <head>
  tags$head(
    tags$link(rel="stylesheet", type="text/css", href="css/styles.css"),
    tags$script(src="d3/d3.v3.min.js"),
    tags$script(src="nvd3/nv.d3.min.js"),
    tags$link(rel="stylesheet", type="text/css", href="nvd3/nv.d3.css")
  ),
  # Div at the top to select the dimension from the HDR data
  div(
    id = "controls",
    inputPanel(
      class="categorySelects",
      # As select options we use our local file structure 
      selectInput("dimensionSelect", "Dimension:", 
                  names(dataTree), 
                  selected="Human Development Index (HDI)"),
      # And preselect the HDI
      selectInput("dataSelect", "Human Development Index (HDI)", 
                  dataTree["Human Development Index (HDI)"], 
                  selected="Human Development Index (HDI)", width='300px')
    )
  ),
  sidebarLayout(
    position = "right",
    # Sidebarpanel contains the NVD3 linechart and multibarchart
    # lineChartOutput and barChartOutput are declared in linechar.R and barchart.R
    sidebarPanel(
      selectizeInput("countryLineChart", label = "Countries of Interest",
                     choices = unique(c("Norway", "Sweden")), multiple = T,
                     options = list(maxItems = 8, placeholder = 'Select a country')),
      lineChartOutput("linechart", yaxis="Human Development Index (HDI)"),
      selectizeInput("dimensionBarChart", label = "Dimensions of Interest",
                     choices = allCategories, multiple = T,
                     options = list(maxItems = 8, placeholder = 'Select a dimension')),
      barChartOutput("barchart", yaxis=FALSE)
    ),
    # Main panel contains the worldmap
    mainPanel(
      # yearSlider updates by injecting javascript inside the page, so we use uiOutput
      uiOutput('yearSliderUI'),
      div(class="outer",
          fluidRow(
            column(8, leafletOutput("hdimap", width="100%", height="80vh"))
          ),
          fluidRow(
            column(6, offset = 1, class="year-slider",
                   sliderInput("year", "Year",
                               min = 1990, max = 2015,
                               value = 2015, animate = TRUE)
            )
          )
      )
    )
  )
)
