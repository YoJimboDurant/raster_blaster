############################
##  merge_nlcd92.R
##  Original Author:          James T. Durant (hzd3@cdc.gov ; 770.488.0668)
##  Location:                 ATSDR/DCHI/SSB: Atlanta, GA
##
##  Purpose:                  This script merges 2 USGS NLCD1992 land use
##                            tiff files and crops and merges them for US EPA's
##                            aersurface program
###########################


# Merge nlcd92 raster files and subset to some domain near portland airport
############################
### Libraries-------------------------------------------------------------------
############################
##
##  *** start of code
##

library(sp)
library(raster)
library(ggplot2)
library(rgdal)

##
##  *** end of code
##
############################
### Libraries-------------------------------------------------------------------
############################


###########################
### Import and Project Data-----------------------------------------------------
############################
##
##  *** start of code
##

rasterFiles <- c("./data/oregon.nlcd.tif",
                 "./data/washington.nlcd.tif")


# met tower location
surf_Latitude = 45.54056
surf_Longitude = 	-122.94861

buffer = 0.5

dfx <- data.frame(lat = seq(round(surf_Latitude - buffer, digits=1),
round(surf_Latitude + buffer, digits=1), length =10),
lon =seq(round(surf_Longitude - buffer, digits=1),
             round(surf_Longitude + buffer, digits=1), length =10))

coordinates(dfx) <-c("lon", "lat")
proj4string(dfx) <- CRS("+init=epsg:4326")



x <- raster(rasterFiles[[1]])
y <- raster(rasterFiles[[2]])

dfx <- spTransform(dfx, CRS(proj4string(x)))
rastX <- crop(x, extent(dfx))
rastY <- crop(y, extent(dfx))

NAvalue(rastX) <- 0
NAvalue(rastY) <- 0

plot(rastX)
plot(rastY)


rastxy <- merge(rastX,rastY, filename="./output/nlcd1992_clipped_bad.tif", 
                    options=c("TFW=YES", NBITS=8), overwrite=TRUE)

#does not work with AERSURFACE - and it took a while to 
#figure out why. Some observations that finally helped me:

plot(rastxy) # produced different plot that plot(rastX) and plot(rastY)

attr(rastxy, "legend")
attr(rastX, "legend")
attr(rastY, "legend")

# missing color table
# good thing that they are identical in both rasters

identical(rastX@legend@colortable, rastY@legend@colortable)

# we can add the color table directly to the object
# this is the clumsy way
# xy@legend@colortable <- rastX@legend@colortable

# you should always try and use methods for the class

colortable(rastxy) <- colortable(rastX)
writeRaster(rastxy, file="./output/nlcd1992_clipped.tif",
            options=c("COMPRESS=NONE", "TFW=YES"), overwrite=TRUE)
curDir <- getwd()
setwd("./output")

rastxy1 <- reproject(rastxy)
colortable(rastxy1) <- colortable(rastxy)
KML(rastxy1, file="nlcd1992.kml", overwrite=TRUE)
