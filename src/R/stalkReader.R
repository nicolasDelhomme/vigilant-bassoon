stalkReader <-
  function( 
    file,
    include.time.cols = TRUE,
    time.start = NULL,
    time.end = NULL
  ) {
    
    require( "tidyverse" )
    
    x <- rjson::fromJSON ( file = file )
    
    # Transform
    
    x <- x$"locations"
    
    y <- purrr::transpose( .l = x )
    
    y <-
      purrr:::map( 
        .x = y,
        .f = ~ unlist( .x ) 
      )
    
    y <- tibble::as_tibble( x = y )
    
    
    
    y$"time.posix" <- 
      as.POSIXct( 
        x = as.numeric( y$"timestampMs" ) / 1000, 
        origin = "1970-01-01", 
        tz = "UTC"
      )
    
    # Subset
    
    if ( !is.null( time.start ) ) {
      
      y <- 
        y %>% 
        filter( time.posix > time.start )
      
    }
    
    if ( !is.null( time.end ) ) {
      
      y <-
        y %>%
        filter( time.posix < time.end )
      
    }
    
    # Reformat coordinates
    
    y$"latitude" <-
      y$"latitudeE7" / 10000000
    
    y$"longitude" <-
      y$"longitudeE7" / 10000000
    
    y$"latitude.un" <- y$"latitude"
    y$"longitude.un" <- y$"longitude"
    
    tmp <- 
      c( TRUE, 
         y[ -1, "latitude.un" ] == y[ -nrow( y ), "latitude.un" ] &
           y[ -1, "longitude.un" ] == y[ -nrow( y ), "longitude.un" ])
    
    y[ which( tmp ), "latitude.un" ] <- NA
    y[ which( tmp ), "longitude.un" ] <- NA
    
    
    # Create additional time columns.
    
    y$"time" <- 
      lubridate::as_datetime( 
        x = as.numeric( y$"timestampMs" ) / 1000, 
        origin = lubridate::origin
      )
    
    if ( include.time.cols ) {
      
      
      y$"year.and.month" <-
        strftime( 
          x = y$"time",
          format = "%y-%b" )
      
      y$"year" <-
        strftime( 
          x = y$"time",
          format = "%Y" )
      
      y$"day" <-
        strftime(
          x = y$"time",
          format = "%j"
        )
      
      y$"day" <- as.numeric( y$"day" )
      
      y$"year.and.day" <-
        strftime(
          x = y$"time",
          format = "%Y-%j"
        )
      
      y$"month" <-
        strftime(
          x = y$"time",
          format = "%B"
        )
      
      y$"month" <- 
        factor(
          x = y$"month",
          levels = month.name
        )
      
      y$"year.month.day" <-
        strftime(
          x = y$"time",
          format = "%y-%b-%d"
        )
      
      y$"weekday" <-
        strftime(
          x = y$"time",
          format = "%a"
        )
      
      y$"hour" <-
        strftime(
          x = y$"time",
          format = "%H"
        )
      
    }
    
    return( y )
    
  }