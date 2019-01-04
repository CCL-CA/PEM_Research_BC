# Extract raster values to point files
# For Kamloops PEM project - extract attributes to our training and testing points
# August 2016 - Heather Richardson
# adjusted for test and training data sets 

# http://gis.stackexchange.com/questions/60527/how-to-extract-values-from-rasters-at-location-of-points-in-r
# https://www.r-bloggers.com/extract-values-from-numerous-rasters-in-less-time/
###################################################################################################################

### Step 1). Load packages and set up libraries: 

# note this only needs to be run once
# install.packages(c("raster","rgdal","RSAGA", "tidyr","dplyr")
# install.packages("Vectorize")

library(tidyr)
library(raster)
library(rgdal)
library(sp)
library(rgdal)
library(sf)
library(ggplot2)
#library(Vectorize)

## check the home directory  
setwd("D:/PEM_DATA/Analysis")
## set up location of drives to input and output

field.data.folder = ("../Data/Field_data")
#field.data.folder = ("C:/PEM_DATA/Data/Field_data/")

input.folder = ("../Data/Layers") #list.files(input.folder)
#input.folder = ("C:/PEM_DATA/Data/Layers/")

out.folder = ("../Analysis/RandomForest/inputs")

ss.folder = ("../Data/Deception_ss/")
pem.gdb = ("..Data/Deception_ss/Pem.gdb") # contains 

#### Step 2) Select pt data you want to use. 

# enter points file.
# note need to combine the Old file with the latest file. Note these have different fields and need to be treated differently then combined. 

#pts.file = "AllDeception_Pts_Consolidated_to_July13.csv"
#pts.file ="AllDeception_Pts_Consolidated_to_2018-09-23.csv"
pts.file ="AllDeception_Pts_Consolidated_WHM.csv"

## or manually choose file: 
#pts.file  <- file.choose() 

###########################################################
# Step 1:  open points file and extract Lats and Longs
###########################################################

pts = read.csv(paste(field.data.folder,"/",pts.file,sep = ''),stringsAsFactors = FALSE)

## check the files for errors
# problem with Dates that might need to be fixed 
#unique(pts$Date) ; range(pts$Date)

# extract the Lat/Longs. 
LatLon <- pts %>% dplyr::select(c(Longitude,Latitude,ObjectID,GlobalID))
LatLon <- na.omit(LatLon)

#length(LatLon$Longitude) # check the length of the files
# check type of files: 
#str(LatLon)

# get co-ordinates and convert from WGs to albers to match the base layers
coordinates(LatLon)=~Longitude + Latitude
proj4string(LatLon)=CRS("+init=epsg:4326") # set it to lat-long
pts = spTransform(LatLon,CRS("+init=epsg:3005"))

plot(pts)

###################################################################
#### Step 2) Select layers you want to extract by data you want to use. 
###################################################################

# list contains all raster files we want to extract the attributes from. 

layers.list = as.list(list.dirs(input.folder,full.names=TRUE))

# STILL TO DO
# Adjust these script so automate the scale 
#foi = c("Dec_25m","Dec_10m","Dec_5m") 

# create a list of layer files to use 
LOI = c(layers.list[8],layers.list[3],layers.list[6])#,layers.list[4])

# loop through all data folders/data sets to generate the csv attribute files for 5,10,25m scales. 

for(ii in 1:length(LOI)) { 
    #ii = 3
    i = LOI[ii]
    i.name = gsub(input.folder,"",paste(i))
    i.name = gsub("/layers","",i.name)
    i.scale = gsub("/Dec_","",i.name)
    
    ## check the list of rasters to extract from: 
    #f <- list.files(path=paste(i),recursive=TRUE, full.names=TRUE, all.files=TRUE, pattern ='.tif$')
    Covariates <-  list.files(path= paste(i),recursive=TRUE, full.names=TRUE, all.files=TRUE, pattern ="\\.tif$")
    Covariates <-  stack(Covariates)
    proj4string(Covariates) <- CRS("+init=epsg:3005") 
    #prs <- "+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
    #pr1 <- projectRaster(Covariates,crs = prs)
    # http://spatialreference.org/ref/    Albers BC is espg:3005+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0 
    attributes <- raster::extract(Covariates, coordinates(pts), df=TRUE)
    attributes <- as.data.frame(attributes)
    LatLon <- as.data.frame(LatLon)
    training <- cbind(LatLon,attributes)
    training$scale = paste(i.scale)
    
    write.csv(training, paste(out.folder,i.name,"_pts_att.csv", sep=""))
                    }


################################################################################
# Step 3: Insersect BGC layer with pts data and add new field.

pts.0 = read.csv(paste(field.data.folder,"/",pts.file,sep = ''),stringsAsFactors = FALSE)

## fix this bit 
pts.sub = pts.0 %>% dplyr::select(c( Longitude, Latitude,GlobalID,Biogeoclimatic.Unit))

# using sf 
pts.sf = st_as_sf(pts.sub, coords = c("Longitude","Latitude")) # read in as sf object
st_crs(pts.sf) = 4326   # assign a CRS based on data collection 
pts.BC = st_transform(pts.sf,3005) # convert CRS to BC albers
ggplot(pts.BC) + geom_sf(data = pts.BC, colour = "red", fill = NA)

# read in BGC layer (created previously in ArcMap)
## Set your input geodatabases ## edit these to your filepath and name of gdb
subset(ogrDrivers(), grepl("GDB", name))
sslist <- ogrListLayers(pem.gdb); print(sslist)

BGC.0 = st_read(dsn = pem.gdb,layer = "BEC_WL_AOI") # read in the layer of interest
BGC.0 = BGC.0 %>% dplyr::select(c(MAP_LABEL)) ; head(BGC.0)
## plot the results
#plot(st_geometry(pts.BC))
#plot(st_geometry(BGC.0),add =  T)

# intersect the points with the mapped BGC units
pts.int <- st_intersection(pts.BC,BGC.0)   # intersect with ranges
pts.int.df <- data.frame(pts.int)
pts.int.df <- pts.int.df %>% dplyr::select(-(geometry))
head(pts.int.df)
head(pts.0)  

pts.out = dplyr::left_join(pts.0,pts.int.df)#,by = "GlobalID")
pts.out <- pts.out %>% dplyr::select(-(X))# remove X columns 
pts.out$BGC_test <- mapply(grepl, pattern=pts.out$Biogeoclimatic.Unit, x=pts.out$MAP_LABEL)

## Formatting crew names and other error checking # already done
#pts.out$Crew
#head(pts.out)

#pts.out =  pts.out %>% 
#        mutate(Crew = toupper(trimws(Crew, which= "both"))) %>%
#        mutate(Crew = gsub(". ",",",Crew))%>%
#        mutate(Crew = gsub(",,",",",Crew))
#unique(pts.out$Crew) 

pts.file.out = "AllDeception_Pts_Consolidated_WHM_BGC.csv"

write.csv(pts.out,paste(field.data.folder,"/",pts.file.out,sep = ''))









### OLD STUFF #####

#for(i in 1:length(f)){

#	pr <- raster(f[i])
#	# extract raster values at specified coordinates
#	attributes <- extract(pr, coordinates(pts), df=TRUE)
	# transpose attributes in data frame. The write function only writes in rows (top to bottom)
#	attributes <- t(attributes)
	# write(x, i) appends data x to file i. 
#	write(attributes, file="E:\\RandomForest_Deception\\AllDeception_Pts_att.csv", sep=",", append=a, ncol=length(attributes))
#	a=TRUE
#}
#list.files(path="U:\\Heather_Richardson\\All_Cleaned_Layers\\", full.names=FALSE, pattern='.asc')


