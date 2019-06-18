plot_stalkR_map <- 
  function(
    x,
    time.start = NULL,
    time.end = NULL,
    palette = NULL,
    ...
  ) {
    
    require( "dplyr" )
    require( "grDevices" )
    require( "leaflet" )
    
    if ( !is.null( time.start ) ) {
      
      stopifnot( is.time( time.start ) )
      stopifnot( !is.null( time.end ) )
      
    }
    
    if ( !is.null( time.end ) ) {
      
      stopifnot( is.time( time.end ) )
      
    }
    
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
    
    if ( is.null( palette ) ) {
      
      palette <- 
        grDevices::rainbow( n = nlevels( x$"day.factor" ) )
      
    }
    
    y <- 
      leaflet::leaflet() %>%
      leaflet::addTiles()
    
    
    for ( i in rows.to.plot ) {
      
      y <-  
        y %>% 
        leaflet::addCircles(
          lng = x$"longitude"[ i ],
          lat = x$"latitude"[ i ],
          stroke = TRUE,
          color = "black",
          opacity = 0.1,
          fill = TRUE,
          fillColor  = palette[ x$"day.factor"[ i ] ],
          fillOpacity = 1, # 0.5
          ...
        )
      
      if ( i > rows.to.plot[ 1 ] ) {
        y <-  
          y %>% 
          leaflet::addPolylines(
            lng = x$"longitude"[ ( i-1 ):i ],
            lat = x$"latitude"[ ( i-1 ):i ],
            stroke = TRUE,
            color = palette[ x$day.factor[ i ] ],
            opacity = 0.5
          )
      }
      
    }
    
    return( y )
    
  }