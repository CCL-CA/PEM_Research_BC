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

## ## set up location of drives to input and output

setwd("D:/PEM_DATA/")#check the home directory  # set up work directory 
field.data.folder = ("Data/Field_data")         # point to field data   #field.data.folder = ("C:/PEM_DATA/Data/Field_data/")
input.folder = ("Data/Layers")                  # point to where layers are stored  #list.files(input.folder) #input.folder = ("C:/PEM_DATA/Data/Layers/")
out.folder = ("Analysis/RandomForest/inputs")   # Point to where your random forest outputs will be stored
ss.folder = ("Data/Deception_ss/")        
pem.gdb = ("Data/Deception_ss/Pem.gdb") # contains 

#### Step 2) Select pt data you want to use. 

pts.file ="AllDeception_Pts_Consolidated_WHM.csv"

## or manually choose file: 
#pts.file  <- file.choose() 

###########################################################
# Step 1:  open points file and extract Lats and Longs
###########################################################

pts = read.csv(paste(field.data.folder,"/",pts.file,sep = ''),stringsAsFactors = FALSE)

LatLon <- pts %>% dplyr::select(c(Longitude,Latitude,ObjectID,GlobalID))     # extract the Lat/Longs. 
LatLon <- na.omit(LatLon)         #length(LatLon$Longitude) # error check:check the length of the files

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
#foi = c("Data/Layers/Dec_25m","Data/Layers/Dec_10m","Data/Layers/Dec_5m") 
#layers.list[grep("Data/Layers/Dec_25m",layers.list)]
#layers.list[grep(foi,layers.list)]


# create a list of layer files to use 
LOI = c(layers.list[16],layers.list[3])# ,layers.list[18])#,layers.list[4])

# loop through all data folders/data sets to generate the csv attribute files for 5,10,25m scales. 

for(ii in 1:length(LOI)) { 
   # ii = 1
    i = LOI[ii]
    i.name = gsub(input.folder,"",paste(i))
    i.name = gsub("/layers","",i.name)
    i.scale = gsub("/Dec_","",i.name)
  
    ## check the list of rasters to extract from: 
    #f <- list.files(path=paste(i),recursive=TRUE, full.names=TRUE, all.files=TRUE, pattern ='.tif$')
    Covariates <-  list.files(path= paste(i),recursive=TRUE, full.names=TRUE, all.files=TRUE, pattern ="\\.tif$")
    Covariates <-  stack(Covariates) #,quick = TRUE)
    #plot(Covariates)
    
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

pts.file.out = "AllDeception_Pts_Consolidated_WHM_BGC.csv"

write.csv(pts.out,paste(field.data.folder,"/",pts.file.out,sep = ''))


