
# R function to create the distance data for the different levels of European NUTS regions

eudistance <- function(level = "NUTS level (0/1/2/3/all)", map_centroids = "Map (TRUE/FALSE)"){
  
  options(warn=-1)
  pkgs <- c("eurostat","SimDesign","sf")
  if(length(pkgs <- setdiff(pkgs, rownames(installed.packages())))) install.packages(pkgs)
  rm(pkgs)
  
  library("eurostat")
  library("SimDesign")
  library("sf")
  
  nuts <- SimDesign::quiet(eurostat::get_eurostat_geospatial(nuts_level = level))
  nuts_centroid = SimDesign::quiet(sf::st_centroid(nuts$geometry,of_largest_polygon = TRUE))
  nuts_distance <- as.data.frame(sf::st_distance(nuts_centroid, nuts_centroid))
  output <- data.frame(nuts$id,nuts$NUTS_NAME,nuts$LEVL_CODE,nuts$CNTR_CODE,nuts$geometry,nuts_centroid)
  output <- cbind(output,nuts_distance)
  colnames(output)  <- c(paste("NUTS_",level,"_code",sep=""),paste("NUTS_",level,"_name",sep=""),"Level_code","Country_code","Geometry","Centroid",as.character(nuts$id))
  
  if (!is.null("map_centroids") & map_centroids == TRUE){
    
    pkgs <- c("rworldmap")
    if(length(pkgs <- setdiff(pkgs, rownames(installed.packages())))) install.packages(pkgs)
    
    library("rworldmap")
    
    coord <- as.data.frame(sf::st_coordinates(output$Centroid))
    newmap <- rworldmap::getMap(resolution = "low")
    
    rworldmap::mapCountryData(newmap,colourPalette = c("white","white"),mapRegion="Europe",borderCol = "grey",oceanCol = "lightblue",mapTitle = paste("Centroids of NUTS-",level," regions (n = ",nrow(output),")",sep=""),addLegend=FALSE)
    points(coord$X,coord$Y,cex=1,col="black",bg="red",pch=21)
    
  }
  
  options(warn=0)
  return(output)

}
