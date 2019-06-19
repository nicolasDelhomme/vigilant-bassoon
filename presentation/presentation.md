stalkR
========================================================
author: Nicolas Delhomme & Tommi Suvitaival
date: 20.6.2019
autosize: true
output: 
  ioslides_presentation

<style>
.small-code pre code {
  font-size: 1em;
}
</style>

Introduction
========================================================

stalkR:

- Personal location data from Google
- Visualization in R

Steps
========================================================

- Request to download your location data from Google Takeout
- Download
- Unzip
- Re-format the data with our function read_location_data_from_Google()
- Run the Shiny App
- Stalk

Google Takeout (1)
========================================================

![alt text](figures/Google-data-screenshot.png)

***

![alt text](figures/Google-takeout-screenshot.png)

Google Takeout (2)
========================================================

![alt text](figures/Google-takeout-screenshot.png)

***

![alt text](figures/Google-download-screenshot.png)

Loading JSON into R
========================================================
class: small-code




```r
x <-
  rjson::fromJSON(
    file = file.path
  )

View( x )
```

***

![alt text](figures/data_loaded-screenshot.png)


Using stalkReader
========================================================
class: small-code


```r
x <- 
  stalkReader( 
    file = file.path,
    time.start = as.POSIXct( x = "2019-06-09 23:00" ),
    time.end = as.POSIXct( x = "2019-06-20 23:30" )
  )

str( x )
```

```
Classes 'tbl_df', 'tbl' and 'data.frame':	2882 obs. of  18 variables:
 $ timestampMs   : chr  "1560114945924" "1560114968511" "1560114989551" "1560115054520" ...
 $ latitudeE7    : num  5.89e+08 5.89e+08 5.89e+08 5.89e+08 5.89e+08 ...
 $ longitudeE7   : num  1.8e+08 1.8e+08 1.8e+08 1.8e+08 1.8e+08 ...
 $ accuracy      : num  16 16 16 16 16 16 47 35 29 43 ...
 $ time.posix    : POSIXct, format: "2019-06-09 21:15:45" "2019-06-09 21:16:08" ...
 $ latitude      : num  58.9 58.9 58.9 58.9 58.9 ...
 $ longitude     : num  18 18 18 18 18 ...
 $ latitude.un   : num  NA NA NA NA NA ...
 $ longitude.un  : num  NA NA NA NA NA ...
 $ time          : POSIXct, format: "2019-06-09 21:15:45" "2019-06-09 21:16:08" ...
 $ year.and.month: chr  "19-Jun" "19-Jun" "19-Jun" "19-Jun" ...
 $ year          : chr  "2019" "2019" "2019" "2019" ...
 $ day           : num  160 160 160 160 160 160 160 160 160 160 ...
 $ year.and.day  : chr  "2019-160" "2019-160" "2019-160" "2019-160" ...
 $ month         : Factor w/ 12 levels "January","February",..: 6 6 6 6 6 6 6 6 6 6 ...
 $ year.month.day: chr  "19-Jun-09" "19-Jun-09" "19-Jun-09" "19-Jun-09" ...
 $ weekday       : chr  "Sun" "Sun" "Sun" "Sun" ...
 $ hour          : chr  "23" "23" "23" "23" ...
```

Data Used by stalkR Shiny App
========================================================
class: small-code


```r
x[ sample.int( n = nrow( x ), size = 10 ), 
   c( "longitude", "latitude", "time.posix", "year.month.day", "hour" ) ]
```

```
# A tibble: 10 x 5
   longitude latitude time.posix          year.month.day hour 
       <dbl>    <dbl> <dttm>              <chr>          <chr>
 1      18.3     57.6 2019-06-14 06:41:10 19-Jun-14      08   
 2      18.3     57.6 2019-06-13 21:18:37 19-Jun-13      23   
 3      18.3     57.6 2019-06-11 06:13:20 19-Jun-11      08   
 4      18.3     57.7 2019-06-12 15:54:03 19-Jun-12      17   
 5      18.3     57.6 2019-06-17 06:12:39 19-Jun-17      08   
 6      18.3     57.6 2019-06-10 20:23:21 19-Jun-10      22   
 7      18.3     57.6 2019-06-14 15:10:25 19-Jun-14      17   
 8      18.3     57.6 2019-06-16 18:38:26 19-Jun-16      20   
 9      18.3     57.6 2019-06-16 20:56:03 19-Jun-16      22   
10      18.3     57.6 2019-06-11 18:23:18 19-Jun-11      20   
```

Creating a Map with plot_stalkR_map()
========================================================
class: small-code



```
Error in file(con, "rb") : cannot open the connection
```
