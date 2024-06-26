library(leaflet)
# Choices for drop-downs
vars <- areas %>% st_drop_geometry() %>% colnames()
vars <- vars[vars != "NEW_FID"]

navbarPage("RLI Sharks and Rays", id="nav",

  tabPanel("Interactive map",
    div(class="outer",

      tags$head(
        # Include our custom CSS
        includeCSS("styles.css"),
        includeScript("gomap.js")
      ),

      # If not using custom CSS, set height of leafletOutput to a number instead of percent
      leafletOutput("map", width="100%", height="100%"),
      
      # todo: integrate loading screen
      # https://davidruvolo51.github.io/shinytutorials/tutorials/leaflet-loading-screens/

      # Shiny versions prior to 0.11 should use class = "modal" instead.
      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
        draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
        width = 330, height = "auto",

        h2("RLI Explorer"),
        selectInput("area", "Areas", c("in progress", "testing")),
        selectInput("variables", "Variables", c("univariate", "bivariate")),
        selectInput("color", "Color", vars),
        conditionalPanel(
          condition = "input.variables == 'bivariate'",
          # Only prompt for opacity if bivariate plot
          selectInput("opacity", "Opacity", vars),
        ),

        # plotOutput("histCentile", height = 200),
        # plotOutput("scatterCollegeIncome", height = 250)
      ),

      tags$div(id="cite",
        'Data compiled for ', tags$em('Red List Index'), ' by Dulvy Lab (2023). - put a proper reference in here later'
      )
    )
  ),
# 
#   tabPanel("Data explorer",
#     fluidRow(
#       column(3,
#         selectInput("states", "States", c("All states"="", structure(state.abb, names=state.name), "Washington, DC"="DC"), multiple=TRUE)
#       ),
#       column(3,
#         conditionalPanel("input.states",
#           selectInput("cities", "Cities", c("All cities"=""), multiple=TRUE)
#         )
#       ),
#       column(3,
#         conditionalPanel("input.states",
#           selectInput("zipcodes", "Zipcodes", c("All zipcodes"=""), multiple=TRUE)
#         )
#       )
#     ),
#     fluidRow(
#       column(1,
#         numericInput("minScore", "Min score", min=0, max=100, value=0)
#       ),
#       column(1,
#         numericInput("maxScore", "Max score", min=0, max=100, value=100)
#       )
#     ),
#     hr(),
#     DT::dataTableOutput("ziptable")
#   ),

  conditionalPanel("false", icon("crosshair"))
)
