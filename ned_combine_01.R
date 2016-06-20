############################
##  Process_elevation.R
##  Original Author:          James T. Durant (hzd3@cdc.gov ; 770.488.0668)
##  Location:                 ATSDR/DCHI/SSB: Atlanta, GA
##
##  Purpose:                  This script merges 2 10m NED files and clips to model domain.
##                            Produces KML files of elevation, monitor data and text files for AERMAP.
##
###########################


############################
### Libraries-------------------------------------------------------------------
############################
##
##  *** start of code
##

library(sp)
library(rgdal)
library(raster)
library(plotKML)


##
##  *** end of code
##
############################
### Libraries-------------------------------------------------------------------
############################


############################
### Import and Project Data-----------------------------------------------------
############################
##
##  *** start of code
##

##NED Data to Rasters-----------------------------------------------------------

myFile1 <- "./data/imgn33w111_13.img"
myFile2 <- "./data/imgn34w111_13.img"


ned_33w111_13 <- raster(myFile1)
ned_34w111_13 <- raster(myFile2)



ned_merged <- merge(ned_33w111_13, ned_34w111_13)



##Import Point Data to determine domain-----------------------------------------
set.seed(1)

# to protect identities I am using random locations.
people <- data.frame(Longitude  = runif(50, -110.8, -110.7),
                     Latitude = runif(50, 32.95, 33.01))
monitors <- read.csv("./data/Station_locations.csv")
sources <- read.csv("./data/sources.csv")
names(monitors)[names(monitors) %in% c("longitude", "latitude")] <- c("Longitude", "Latitude")

monitors <- monitors[c("Longitude", "Latitude")]
sources <- sources[c("Longitude", "Latitude")]

people$type <- "People"
monitors$type <- "Monitor"
sources$type <- "Source"

mergePoints <- plyr::rbind.fill(list(people, monitors,sources))

coordinates(mergePoints) <- c("Longitude", "Latitude")
proj4string(mergePoints) <- CRS("+init=epsg:4326")
mergePoints2 <- spTransform(mergePoints, CRS(proj4string(ned_merged)))

##Plot Data---------------------------------------------------------------------

plot(ned_merged)
points(mergePoints, pch=16, col="red", cex=1)

##
##  *** end of code
##
############################
### Import and Project Data-----------------------------------------------------
############################

############################
### Determine Model Domain------------------------------------------------------
############################
##
##  *** start of code
##


##Function as.SpatialPolygons.bbox translates bounding box to polygon-----------

as.SpatialPolygons.bbox <- function( bbox, proj4stringFrom=CRS("+proj=longlat
+datum=WGS84"), proj4stringTo=NULL ) {
  # Create unprojected bbox as spatial object
    bboxMat <- rbind( c(bbox['Longitude','min'],bbox['Latitude','min']),
                     c(bbox['Longitude','min'],bbox['Latitude','max']),
                     c(bbox['Longitude','max'],bbox['Latitude','max']),
                     c(bbox['Longitude','max'],bbox['Latitude','min']),
                     c(bbox['Longitude','min'],bbox['Latitude','min']) ) # clockwise, 5 points to close it
    bboxSP <- SpatialPolygons( list(Polygons(list(Polygon(bboxMat)),"bbox")),
                              proj4string=proj4stringFrom  )

    if(!is.null(proj4stringTo)) {
    bboxSP <- spTransform( bboxSP, proj4stringTo )
  }
  bboxSP
}

merge.bbox <- bbox(mergePoints)         #get bounding box of point data
merge.bbox[1,1] <- merge.bbox[1,1] - 0.01 #add 0.01 degree to bounding box
merge.bbox[2,1] <- merge.bbox[2,1]  -0.01
merge.bbox[1,2] <- merge.bbox[1,2] + 0.01
merge.bbox[2,2] <- merge.bbox[2,2] + 0.01

model.domain <- as.SpatialPolygons.bbox(merge.bbox)

##Output domain to KML file-----------------------------------------------------
## We create a clip slightly bigger than model domain to allow for interperalation
## in AERMAP


curDir <- getwd()
setwd("./output")
kml(model.domain, file.name=
      "modelDomain.kml")

clip.bbox <- merge.bbox
clip.bbox[1,1] <- merge.bbox[1,1] - 0.01
clip.bbox[2,1] <- merge.bbox[2,1]  -0.01
clip.bbox[1,2] <- merge.bbox[1,2] + 0.01
clip.bbox[2,2] <- merge.bbox[2,2] + 0.01

clip.domain <- as.SpatialPolygons.bbox(clip.bbox)
kml(clip.domain, file.name = "clipDomain.kml")
plot(clip.domain)
plot(model.domain, add=TRUE) #nice

##
##  *** end of code
##
############################
### Determine Model Domain------------------------------------------------------
############################



############################
### Clip Raster-----------------------------------------------------------------
############################
##
##  *** start of code
##

ned_cropped <- crop(ned_merged, clip.domain)
pdf(file="ned_cropped.pdf")
plot(ned_cropped)
plot(model.domain, add=TRUE)
dev.off()

##Output to tiff and KML--------------------------------------------------------
writeRaster(ned_cropped, filename = "./hayden_ned.tif",
            options=c("COMPRESS=NONE", "TFW=YES"), overwrite=TRUE)
kml(ned_cropped, colour=layer, folder.name="hayden_elevation",
    file.name="hayden_elevation.kml")
kml(mergePoints, colour=mergePoints$type, file.name="hayden_receptors.kml")


##
##  *** end of code
##
############################
### Clip Raster-----------------------------------------------------------------
############################


############################
### Create text outputs of receptors for AERMAP---------------------------------
############################
##
##  *** start of code
##

# transform points to UTM
desc_receptors <- spTransform(mergePoints, CRS("+proj=utm +zone=12 +datum=WGS84"))
receptors <- coordinates(desc_receptors)

monitors <- receptors[mergePoints@data == "Monitor",]
people <- receptors[mergePoints@data == "People",]
people <- unique(people)


monitors <- apply(monitors, 1, function(x) paste(round(x,0), collapse=" "))
people <- apply(people, 1, function(x) paste(round(x,0), collapse=" "))

monitors <- paste("   DISCCART   ", monitors)
people <- paste("   DISCCART   ", people)

writeLines(monitors, "monitor_recpt.txt")
writeLines(people, "people_recpt.txt")

# get spatial extent of model domain
model.domain.utm <- spTransform(model.domain, CRS("+proj=utm +zone=12 +datum=WGS84"))
write(bbox(model.domain.utm), "model_domain.txt")

setwd(curDir)

##
##  *** end of code
##
############################
### Create text outputs of receptors for AERMAP---------------------------------
############################






