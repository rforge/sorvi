# This file is a part of the soRvi program (http://louhos.github.com/sorvi/)

# Copyright (C) 2010-2012 Louhos <louhos.github.com>. All rights reserved.

# This program is open source software; you can redistribute it and/or modify 
# it under the terms of the FreeBSD License (keep this notice): 
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful, 
# but WITHOUT ANY WARRANTY; without even the implied warranty of 
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.





#' Load static Google Map
#'
#' Get static map from Google Maps API and convert it to ggplot2-compatible form.
#' See Terms and Conditions from http://code.google.com/apis/maps/documentation/staticmaps/index.html.
#' https://github.com/hadley/ggplot2/wiki/Crime-in-Downtown-Houston,-Texas-:-Combining-ggplot2-and-Google-Maps
#'
#' @param center Coordinates for the center of the map
#' @param zoom Zoom-level
#' @param GRAYSCALE Grayscale or colours?
#' @param scale Scale of the map, 1: less details, faster to load, 2: more details, much slower to load
#' @param maptype Type of the map
#' @param destfile Temporary file to save the obtained map picture
#' @param n_pix Size of the figure (max = 640)
#' @param format Format of the map picture (png32 is best)
#'
#' @return df Map data frame
#' 
#' @author Juuso Parkkinen \email{sorvi-commits@@lists.r-forge.r-project.org}
#' @export
GetStaticmapGoogleMaps <- function(center, zoom = 10, GRAYSCALE = FALSE, scale = 1, maptype = 'map', destfile = 'TemporaryMap.png', n_pix = 640, format = "png32") {

  .InstallMarginal("ReadImages")
  .InstallMarginal("png")
  .InstallMarginal("RgoogleMaps")
  .InstallMarginal("reshape2")
  .InstallMarginal("plyr")

  # Get map with given scale
  if (scale==1) 
    RgoogleMaps::GetMap(center = center[c('lat','lon')], GRAYSCALE=GRAYSCALE, size = c(n_pix, n_pix), 
           zoom = zoom, format = format, maptype = maptype, destfile=destfile)
  else if (scale==2)
    RgoogleMaps::GetMap(center = center[c('lat','lon')], GRAYSCALE=GRAYSCALE, size = c(n_pix, n_pix),
           zoom = zoom, format = format, maptype = paste(maptype, "&scale=2", sep=""), destfile=destfile)
  else
    stop("Invalid scale-value!")
  
  # Read downloaded map png and delete the temporary files
  map <- png::readPNG(destfile)
  if (file.exists("TemporaryMap.png"))
    file.remove("TemporaryMap.png")
  if (file.exists("TemporaryMap.png.rda"))
    file.remove("TemporaryMap.png.rda")
  
  # Double the number of pixels if scale==2
  n_pix <- n_pix*scale 
  
  # Deal with color
  if(GRAYSCALE == FALSE) {
    cat("Colours: yes\n")
    map <- apply(map, 1:2, function(v) rgb(v[1], v[2], v[3]))     
  } else {
    cat("Colours: no\n")
    nrow <- nrow(map)
    ncol <- ncol(map)
    map <- grey(ReadImages::rgb2grey(map, coefs = c(0, 1, 0)))
    map <- matrix(map, nrow = nrow, ncol = ncol)
  }

  # Reshape map for plotting
  m_map <- reshape2::melt(map)
  names(m_map) <- c('x','y','fill')
  m_map <- within(m_map,{
    x <- x - n_pix/2 - 1
    y <- y - n_pix/2 - 1
  })     
  
  mapInfo <- list(lat = center['lat'], lon = center['lon'], zoom = zoom, map)
  XY_cent <- RgoogleMaps::LatLon2XY.centered(mapInfo, center['lat'], center['lon'])
  
  # Geocode pixel references
  s <- (-n_pix/2) : (n_pix/2 - 1)  
  lat_wrapper <- function(x) RgoogleMaps::XY2LatLon(mapInfo, -n_pix/2, x)[1]
  lats <- apply(data.frame(s), 1, lat_wrapper)  
  lon_wrapper <- function(y) RgoogleMaps::XY2LatLon(mapInfo, y, -n_pix/2)[2]
  lons <- apply(data.frame(s), 1, lon_wrapper)
  
  # Merge colors to latlons and return
  df_xy   <- expand.grid(x = s, y = s)
  df_ll   <- expand.grid(lat = rev(lats), lon = lons)
  df_xyll <- data.frame(df_xy, df_ll)
  df <- suppressMessages(plyr::join(df_xyll, m_map, type = 'right'))
  df <- df[,c('lon','lat','fill')]
  return(df)
}


#' Get geo code from Google Map
#'
#' Get gecode for given street address from Google Maps API
#' See Terms and Conditions from http://code.google.com/apis/maps/documentation/geocoding/
#'
#' @param str Street address, e.g. 'Mannerheimintie, 00100, FI'
#'
#' @return coordinates (lat, lon)
#' 
#' @author Juuso Parkkinen \email{sorvi-commits@@lists.r-forge.r-project.org}
#' @export
GetGeocodeGoogleMaps <- function(str) {

  .InstallMarginal("XML")

  u <- paste('http://maps.google.com/maps/api/geocode/xml?sensor=false&address=',str)
  doc <- XML::xmlTreeParse(u, useInternal=TRUE)
  lat <- sapply(XML::getNodeSet(doc, "/GeocodeResponse/result/geometry/location/lat"), function(el) XML::xmlValue(el))
  lon <- sapply(XML::getNodeSet(doc, "/GeocodeResponse/result/geometry/location/lng"), function(el) XML::xmlValue(el))
  return(c(lat,lon))
}


#' Get geo code from OpenStreetMap
#'
#' Get gecode for given plave from OpenStreetMap Nominatim
#' See http://wiki.openstreetmap.org/wiki/Nominatim
#'
#' @param query Either a street address, e.g. 'Mannerheimintie+100,Helsinki' or place, e.g. 'Eduskuntatalo'
#'
#' @return coordinates (lat, lon)
#' 
#' @author Juuso Parkkinen \email{sorvi-commits@@lists.r-forge.r-project.org}
#' @export
GetGeocodeOpenStreetMap <- function(query) {
  
  .InstallMarginal("RCurl")
  .InstallMarginal("rjson")

  u <- paste("http://nominatim.openstreetmap.org/search?q=",query,"&format=json", sep="")
  val <- RCurl::getURI(u)
  res <- rjson::fromJSON(val)
  if (length(res)>0)
    return(as.numeric(c(res[[1]]$lat, res[[1]]$lon)))
  else # Geocode not found
    return(NULL)
}


#' Get map theme
#'
#' Get black map theme for ggplot2
#'
#' @return theme_map A ggplot2 theme object
#' 
#' @author Juuso Parkkinen \email{sorvi-commits@@lists.r-forge.r-project.org}
#' @export
GetThemeMap <- function() {
  
  .InstallMarginal("ggplot2")
  
  theme_map <- ggplot2::theme_bw()
  theme_map$panel.background <- ggplot2::theme_blank()
  theme_map$panel.grid.major <- ggplot2::theme_blank()
  theme_map$panel.grid.minor <- ggplot2::theme_blank()
  theme_map$axis.ticks <- ggplot2::theme_blank()
  theme_map$axis.text.x <- ggplot2::theme_blank()
  theme_map$axis.text.y <- ggplot2::theme_blank()
  theme_map$axis.title.x <- ggplot2::theme_blank()
  theme_map$axis.title.y <- ggplot2::theme_blank()

  return(theme_map)  
}

