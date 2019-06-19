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
                         value=format(today(),"%Y-%m-%d")))
      ),
      fluidRow(
        column(4,style=list("padding-right: 5px;"),
               actionButton("date_action","Update"))
      ),
      
      # time
      tags$div(id="times"),
      
      # ruler
      hr(),
      h3("Pimp"),
      # palette
      fluidRow(
        column(12,style=list("padding-right: 5px;"),
               selectInput("plt","Palette",c("None","Dark2","Set3"),
                          selected="None"))
      ),
      # average
      fluidRow(
        column(12,style=list("padding-right: 5px;"),
               radioButtons("avg","Average",c("on","off"),selected="off",
                                              inline=TRUE))),
      # lines
      tags$div(id="lines"),
      tags$br()
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Plot",leafletOutput("plot_output",width="100%")),
                  tabPanel("Table",dataTableOutput("table")),
                  tabPanel("Version",dataTableOutput("version"))
                  ))
    
  ),
  uiOutput("ui")
)

# SERVER ----
#message("Setting the Server")
# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # vars
  lineUI <- FALSE
  lines <- TRUE
  timeUI <- FALSE
  hstart <- NULL
  hstop <- NULL
  
  # dataset
  dset <- reactive({
    validate(need(input$other=="","You cannot stalk others yet!"))
    switch(input$whom,
           "Tommy" = dat$tommy,
           "Nico" = dat$nico)
  })
  
  # calendar
  dts <- reactive({
    
    shiny::req(input$date_start)
    shiny::req(input$date_end)
    
    validate(need(input$date_start <= input$date_end,
                  "The 'To' date cannot be older than the 'From' date"))
    
    min.date <- min(as_datetime(dset()$year.month.day))
    max.date <- max(as_datetime(dset()$year.month.day))
    
    validate(need(input$date_start <= max.date,
                  sprintf("There is no data past %s",max.date)))
    
    validate(need(input$date_end >= min.date,
                  sprintf("There is no data prior %s",min.date)))
    
    sdate <- input$date_start
    edate <- input$date_end

    if(sdate == edate){
      sdate <- as_datetime(edate) + minutes(1)
      edate <- as_datetime(edate) + days(1) -hours(2)
    }
    
    tibble(
      start=sdate,
      end=edate)
  })
  
  # update based on cal
  observeEvent(input$date_action,{
    #message("update1",lineUI,lines)
    if(!isolate(dts()$start) %--% isolate(dts()$end) %>% int_length() <= days(1)){
      if(!lineUI){
        insertUI("#lines",
                 ui = 
                   fluidRow(
                     column(6,
                            radioButtons("radiolines",
                                         "Lines",
                                         c("on","off"),
                                         selected="off",
                                         inline=TRUE))))
        lineUI <<- TRUE
        lines <<- FALSE
        #message("update2p",lineUI,lines)
      }
      
      if(timeUI){
        removeUI("#times")
        timeUI <<- FALSE
      }
      
    } else {
      if(lineUI){
        removeUI("#lines")
        lineUI <<- FALSE
        lines <<- TRUE
        #message("update2m",lineUI,lines)
      }
      if(!timeUI){
        insertUI("#times",
                 ui=fluidRow(
                   column(12,sliderInput("hour","Hour",value = c(0,23),
                                         min = 0,max=23))
                 ))
        timeUI <<- TRUE
      }
    }
    #message("update3",lineUI,lines)
    })
  
  # some text - used for debug
  # output$textOutput <- renderText({
  #   input$date_action
  #   sprintf("We stalked from %s to %s (%s seconds), %s,%s",
  #           isolate(dts()$start),
  #           isolate(dts()$end),
  #           isolate(dts()$start) %--% isolate(dts()$end)  %>% int_length(),
  #           lineUI,lines)
  # })

  # plot
  output$plot_output <- renderLeaflet({
    
    input$date_action
    llines <- lines
    if(!is.null(input$radiolines)){
      llines <- switch(input$radiolines,
                          "on"=TRUE,
                          "off"=FALSE)
      if(!lineUI){
        llines <- lines
        }
      }
    
    if(! is.null(input$hour)){
      hstart <- input$hour[1]
      hstop <- input$hour[2]
      if(!timeUI){
        
      }
    }
    
    # message("plot",lineUI,lines,llines)
    # 
    # message("plot ",isolate(dts()$start)," ",
    #         isolate(dts()$end))
    # 
    # message("plot ",hstart," ",
    #         hstop)
    # 
    
    pal <- switch(input$plt,
                  "None"=NULL,
                  "Dark2"=brewer.pal(8,"Dark2"),
                  "Set3"=brewer.pal(12,"Set3"))
    
      plot_stalkR_map(dset(),
                      isolate(as.POSIXct(dts()$start)),
                      isolate(as.POSIXct(dts()$end)),
                      print.lines=llines,
                      highlight.start = hstart,
                      highlight.end = hstop,
                      palette=pal,
                      print.average = switch(input$avg,
                                             "on"=TRUE,
                                             "off"=FALSE))
  })
  
  output$table <- renderDataTable({dset() %>% 
        filter(as_date(year.month.day) >= dts()$start &
                 as_date(year.month.day) <= dts()$end)})

  output$version <- renderDataTable({R.version %>% unlist %>% enframe})
   
}

# RUN ----
message("Serving")

shinyApp(ui = ui, server = server)

