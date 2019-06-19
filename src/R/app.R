# HEADER ----
#' ---
#' title: "Gold standard analysis of all networks"
#' author: "Nicolas Delhomme & Tommi Suvitaival"
#' date: "`r Sys.Date()`"
#' output:
#'  html_document:
#'    toc: true
#'    number_sections: true
#' ---

# Setup
# library(shiny)
# options(shiny.port = 12001)
# options(shiny.host = "130.239.72.58")
# runApp('~/Git/UPSCb/projects/spruce-shiny-pacbio-blast/src/R/pacBioBlast')

# libraries
suppressPackageStartupMessages(library(colourpicker))
suppressPackageStartupMessages(library(here))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(RColorBrewer))
suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(tidyverse))

# DATA ----
message("Loading the data")

dat <- readRDS(here::here("data","data_visby.rds"))

cols <- RColorBrewer::brewer.pal(8,"Dark2")

# UI ----
message("Setting the UI")

# Define UI 
ui <- fluidPage(
  
  # Application title
  titlePanel("StalkR"),
  
  # Sidebar with a selection for the dates
  sidebarLayout(
    sidebarPanel(
      
      h3("Stalk"),
      fluidRow(
        column(6,style=list("padding-right: 5px;"),
               dateInput("date_start","From",
                         value=format(lubridate::as_datetime(Sys.time()) - months(1),"%Y-%m-%d"))
        ),
        column(6,style=list("padding-left: 5px;"),
               dateInput("date_end","To",
                         value=format(Sys.time(),"%Y-%m-%d"))
        )),
      hr(),
      h3("Pimp"),
      fluidRow(
        column(12,style=list("padding-right: 5px;"),
               colourpicker::colourInput("colour",label="Color",
                                         showColour ="background",
                                         palette="limited",
                                         allowedCols=cols,
                                         value=cols[1],
                                         allowTransparent = TRUE)
        )
      ),
      tags$br()
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      textOutput("textOutput")
    )
    
  ),
  uiOutput("ui")
)

# SERVER ----
#message("Setting the Server")
# Define server logic required to draw a histogram
server <- function(input, output) {
  ## TODO ADD A VALIDATE
  dts <- reactive({
    shiny::req(input$date_start)
    shiny::req(input$date_end)
    c(start=as_datetime(input$date_start),
      end=as_datetime(input$date_end))
  })
  output$textOutput <- renderText({
    sprintf("We stalked from %s to %s (%s seconds)",
            dts()[["start"]],dts()[["end"]],
            dts()[["start"]] %--% dts()[["end"]]  %>% int_length())
  })
}

# RUN ----
message("Serving")

shinyApp(ui = ui, server = server)

