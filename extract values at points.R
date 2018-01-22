# Extract raster values to point files
# For Kamloops PEM project - extract attributes to our training and testing points
# August 2016 - Heather Richardson
# http://gis.stackexchange.com/questions/60527/how-to-extract-values-from-rasters-at-location-of-points-in-r
# https://www.r-bloggers.com/extract-values-from-numerous-rasters-in-less-time/
###################################################################################################################

# install.packages("raster")
library(raster)
# install.packages("rgdal")
library(rgdal)
#install.packages("RSAGA")
# library(RSAGA)

#this file contains the coordinates
FILE <- "D:\\McQueenLake\\mcqueen_IDFxh2.csv"
# convert from table to matrix
UTM <- read.table(FILE, header=TRUE, as.is=TRUE, sep=",")
UTM <- UTM[,3:4]
str(UTM)


# list contains all raster files we want to extract the attributes from. 
# I used the pattern='.sdat' to find all required files in SAGA format

f <- list.files(path="D:\\McQueenLake\\ASC_nonull\\",recursive=TRUE, full.names=TRUE, all.files=FALSE, pattern ='.asc$')
#list.files(path="D:\\McQueenLake\\ASC_nonull\\",recursive=TRUE, full.names=FALSE, all.files=FALSE, pattern ='.asc$')
a = FALSE
for(i in 1:length(f)){

	pr <- raster(f[i])
	# extract raster values at specified coordinates
	attributes <- extract(pr, coordinates(UTM), df=TRUE)
	# transpose attributes in data frame. The write function only writes in rows (top to bottom)
	attributes <- t(attributes)
	# write(x, i) appends data x to file i. 
	write(attributes[2,], file="D:\\McQueenLake\\mcqueen_idfxh2_att.csv", sep=",", append=a, ncol=length(attributes))
	a=TRUE
}
#list.files(path="U:\\Heather_Richardson\\All_Cleaned_Layers\\", full.names=FALSE, pattern='.asc')

