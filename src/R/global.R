# Header ----
#' ---
#' title: "Stalking Tommi"
#' author: "Nicolas Delhomme & Tommi Suvitaival"
#' date: "`r Sys.Date()`"
#' ---

# Libraries ----
suppressPackageStartupMessages(library(colourpicker))
suppressPackageStartupMessages(library(grDevices))
suppressPackageStartupMessages(library(here))
suppressPackageStartupMessages(library(leaflet))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(RColorBrewer))
suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(tidyverse))

# Functions ----
plot_stalkR_map <- 
  function(
    x,
    time.start = NULL,
    time.end = NULL,
    palette = NULL,
    print.lines = NULL,
    ...
  ) {
    
    # if ( !is.null( time.start ) ) {
    #   
    #   stopifnot( is.time( time.start ) )
    #   stopifnot( !is.null( time.end ) )
    #   
    # }
    # 
    # if ( !is.null( time.end ) ) {
    #   
    #   stopifnot( is.time( time.end ) )
    #   
    # }
    # 

    if ( !is.null( time.start ) & !is.null( time.end ) ) {
      
      x.in <- x
      
      x <- 
        x %>% 
        dplyr::filter( 
          time.posix >= time.start &
            time.posix <= time.end )
      
    }
    
    rows.to.plot <- 
      which( 
        apply( X = !is.na( x[ , c ( "longitude", "latitude" ) ] ), 
               MAR = 1, 
               FUN = all 
        )
      )
    
    if ( length( unique( x$"year.month.day" ) ) == 1 ) {
      
      x$"t" <- as.factor( x = x$"hour" )
      
      if ( is.null( print.lines ) ) {
        
        print.lines <- TRUE
        
      }
      
    } else {
    
      x$"t" <- as.factor( x = x$"year.month.day" )
    
      if ( is.null( print.lines ) ) {
        
        print.lines <- FALSE
        
      }
      
    }
    
    if ( is.null( palette ) ) {
      
      palette <- 
        grDevices::rainbow( n = nlevels( x$"t" ) )
        # grDevices::rainbow( n = nlevels( x$"year.month.day" ) )
        # grDevices::rainbow( n = nlevels( x$"day.factor" ) )
      
    }
    
    x$"colors" <- palette[ x$"t" ]
    
    y <- 
      leaflet::leaflet( data = x ) %>%
      leaflet::addTiles() %>% 
      leaflet::addCircles(
        lng = ~ longitude,
        lat = ~ latitude,
        stroke = TRUE,
        color = "black",
        opacity = 0.1,
        fill = TRUE,
        fillColor = ~ colors, # palette[ x$"t"[ i ] ], # palette[ x$"year.month.day"[ i ] ], # palette[ x$"day.factor"[ i ] ],
        fillOpacity = 1, # 0.5
        ...
      )
    
    # if ( print.lines ) { # Does not work yet.
    #   
    #   y <-
    #     y %>%
    #     leaflet::addPolylines(
    #       lng = ~ longitude,
    #       lat = ~ latitude,
    #       stroke = TRUE,
    #       color = ~ colors, # palette[ x$"t"[ i ] ], # palette[ x$"year.month.day"[ i ] ], # palette[ x$day.factor[ i ] ],
    #       opacity = 0.5
    #     )
    #   
    # }
      
    
    for ( i in rows.to.plot ) {

    #   y <-  
    #     y %>% 
    #     leaflet::addCircles(
    #       lng = x$"longitude"[ i ],
    #       lat = x$"latitude"[ i ],
    #       stroke = TRUE,
    #       color = "black",
    #       opacity = 0.1,
    #       fill = TRUE,
    #       fillColor  = palette[ x$"t"[ i ] ], # palette[ x$"year.month.day"[ i ] ], # palette[ x$"day.factor"[ i ] ],
    #       fillOpacity = 1, # 0.5
    #       ...
    #     )

      if ( print.lines & i > rows.to.plot[ 1 ] ) {
        
        y <-
          y %>%
          leaflet::addPolylines(
            lng = x$"longitude"[ ( i-1 ):i ],
            lat = x$"latitude"[ ( i-1 ):i ],
            stroke = TRUE,
            color = palette[ x$"t"[ i ] ], # palette[ x$"year.month.day"[ i ] ], # palette[ x$day.factor[ i ] ],
            opacity = 0.5
          )
      }

    }
    
    return( y )
    
  }

# Data ----
# DATA ----
message("Loading the data")

dat <- readRDS(here::here("data","data_visby.rds"))

cols <- RColorBrewer::brewer.pal(8,"Dark2")
