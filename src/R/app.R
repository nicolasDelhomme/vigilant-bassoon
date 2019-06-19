# Source ----
source(here::here("src/R/global.R"))

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
      
      # dataset
      fluidRow(
        column(6,style=list("padding-right: 5px;"),
               selectInput("whom","Favorite",c("Tommy","Nico"),
                           selected="Tommy")),
        column(6,style=list("padding-left: 5px;"),
               textInput("other","Other")
        )),
      
      # ruler
      hr(),
      
      # date
      fluidRow(
        column(6,style=list("padding-right: 5px;"),
               dateInput("date_start","From",
                         value=format(today() - days(1),"%Y-%m-%d"))
        ),
        column(6,style=list("padding-left: 5px;"),
               dateInput("date_end","To",
                         value=format(today(),"%Y-%m-%d"))
        )),
      
      # action button
      fluidRow(
        column(6,offset=3,style=list("padding-left: 5px;"),
               actionButton("date_action","Update")
        )
      ),
      
      # ruler
      hr(),
      h3("Pimp"),
      # palette
      fluidRow(
        column(12,style=list("padding-right: 5px;"),
               colourpicker::colourInput("colour",label="Color",
                                         showColour ="background",
                                         palette="limited",
                                         allowedCols=cols,
                                         value=cols[1],
                                         allowTransparent = TRUE)
        )
        # lines
      ),
      tags$br()
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      textOutput("textOutput"),
      leafletOutput("plot_output",width="400px")
    )
    
  ),
  uiOutput("ui")
)

# SERVER ----
#message("Setting the Server")
# Define server logic required to draw a histogram
server <- function(input, output) {
  ## TODO ADD VALIDATION
  
  dset <- reactive({
    switch(input$whom,
           "Tommy" = dat$tommy,
           "Nico" = dat$nico)
  })
  
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
  output$plot_output <- renderLeaflet({
    input$date_action
    plot_stalkR_map(dset(),
                    isolate(dts()[["start"]]),
                    isolate(dts()[["end"]]))
  })
}

# RUN ----
message("Serving")

shinyApp(ui = ui, server = server)

