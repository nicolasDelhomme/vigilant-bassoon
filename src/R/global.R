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
    highlight.start = NULL,
    highlight.end = NULL,
    palette = NULL,
    print.lines = NULL,
    print.average = FALSE,
    print.individual = TRUE,
    ...
  ) {
    
    # if ( !is.null( time.start ) ) { # Does not work yet.
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
      is.hours <- TRUE
      
      if ( is.null( print.lines ) ) {
        
        print.lines <- TRUE
        
      }
      
    } else {
    
      x$"t" <- as.factor( x = x$"year.month.day" )
      is.hours <- FALSE
    
      if ( is.null( print.lines ) ) {
        
        print.lines <- FALSE
        
      }
      
    }
    
    if ( is.null( palette ) ) {
      
      palette <- grDevices::rainbow( n = nlevels( x$"t" ) )
     
    }
    
    palette <-
      leaflet::colorFactor( 
        palette = palette,
        domain = levels( x$"t" ) 
      )
    
    x$"is.avg" <- FALSE
    
    if ( print.individual | print.average ) {
    
      if ( print.average ) {
        
        x.grp <- 
          group_by(
            .data = x,
            t
          )
        
        x.grp <-
          summarize_at(
            .tbl = x.grp,
            .vars = c( "longitude", "latitude" ), 
            .funs = mean
          )
        
        x.grp$is.avg <- TRUE
        
        if ( print.individual ) {
          
          x <- bind_rows( x, x.grp )
          
        } else {
          
          x <- x.grp
          
        }
        
      }
      
    } else {
      
      x <- x[ NULL, ]
      
    }
    
    x$"t.color" <- x$"t"
    
    if ( !is.null( highlight.start ) ) {
      
      x$"t.color"[ as.integer( x$"t" ) < as.integer( highlight.start ) ] <- NA
      
    }
    
    if ( !is.null( highlight.end ) ) {
      
      x$"t.color"[ as.integer( x$"t" ) > as.integer( highlight.end ) ] <- NA
      
    }
      
    # for ( i in 1:nrow( x.grp ) ) {
    #   
    #   y <-
    #     y%>%
    #     leaflet::addCircleMarkers(
    #       radius = 2, # ~ifelse(type == "ship", 6, 10),
    #       color = "black", # palette( x.grp$t[ i ] ) #,
    #       # stroke = FALSE, fillOpacity = 0.5
    #     )
    #   
    # }
    
    y <- 
      leaflet::leaflet( data = x ) %>%
      leaflet::addTiles() %>% 
      leaflet::addCircles(
        lng = ~ longitude,
        lat = ~ latitude,
        stroke = FALSE, # TRUE,
        # color = "black",
        # opacity = 0.1,
        # fill = TRUE,
        color = ~ palette( t.color ), # ~ colors, # palette[ x$"t"[ i ] ], # palette[ x$"year.month.day"[ i ] ], # palette[ x$"day.factor"[ i ] ],
        fillOpacity = ~ ifelse( test = is.avg, yes = 0.5, no = 0.75 ),
        radius = ~ ifelse( test = is.avg, yes = 100, no = 10 ),  # 0.5
        ...
      ) %>%
      leaflet::addLegend(
        position = "bottomleft",
        pal = palette,
        values = ~ t,
        title = ifelse( test = is.hours, yes = "Hour", no = "Day" ),
        # labFormat = labelFormat( prefix = "$" ),
        opacity = 1
      )
    
    # if ( print.lines ) { # Does not work yet.
    # 
    #   y <-
    #     y %>%
    #     leaflet::addPolylines(
    #       lng = ~ longitude,
    #       lat = ~ latitude,
    #       stroke = TRUE,
    #       color = ~ t, # palette[ x$"t"[ i ] ], # palette[ x$"year.month.day"[ i ] ], # palette[ x$day.factor[ i ] ],
    #       opacity = 0.5
    #     )
    # 
    # }
      
    
    for ( i in rows.to.plot ) {

      if ( print.lines & i > rows.to.plot[ 1 ] ) {
        
        y <-
          y %>%
          leaflet::addPolylines(
            lng = x$"longitude"[ ( i-1 ):i ],
            lat = x$"latitude"[ ( i-1 ):i ],
            stroke = TRUE,
            color = palette( x$"t.color"[ i ] ), # palette[ x$"t"[ i ] ], # palette[ x$"year.month.day"[ i ] ], # palette[ x$day.factor[ i ] ],
            opacity = 0.5
          )
      }

    }
    
    return( y )
    
  }

# Data ----
message("Loading the data")

# Find the files
files <- list.files(here::here("data"),full.names = TRUE)

# load them in a named tible
dat <- map(files,
    readRDS) %>% set_names(files %>% basename() %>% sub("_.*","",.))

# Palettes ----
cols <- RColorBrewer::brewer.pal(8,"Dark2")


