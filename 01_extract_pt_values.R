# Extract raster values to point files. 
# check point values against BGC mapping 
# Incorporates air interp values and cleans data. # this will require changes once WHM reviews 

# For Deception PEM project - extract attributes to our training and testing points

# Based on original script : August 2016 - Heather Richardson


# help/Reference files

# http://gis.stackexchange.com/questions/60527/how-to-extract-values-from-rasters-at-location-of-points-in-r
# https://www.r-bloggers.com/extract-values-from-numerous-rasters-in-less-time/

###################################################################################################################
# To run this script you will need: 
# - Training points data 
# - Air interp data
# - BGC layer 
# - Layer with attributes you want to extract

###################################################################################################################
### Step 1). Load packages and set up libraries: 

library(tidyr)
library(raster)
library(rgdal)
library(sp)
library(rgdal)
library(sf)
library(ggplot2)
library(dplyr)
library(gtools)

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

#### Read in Air-interp points and format to match points 
air.0 <- st_read(paste(field.data.folder,"Deception Lake.shp",sep = "/"))
air.0 <- st_transform(air.0,3005)
air <- as.data.frame(air.0)
airxy <- as.data.frame(st_coordinates(air.0)) 
air <- cbind(air,airxy)  
#length(air$X)
air <- air %>% dplyr::filter(X >0)
#length(air$X)


# adjust air interp photos
# reclass anything Am as Xvh and anything that is Gb as Xvs.
#I will need to look at each individual point to confirm but we can go with this for now.

# GP notes 
# convert realm to wetland and terrestrial 
# Convert group to G/A??R/W 
# convert Physio to NFor????? leave for Will 

# not likely to allocate air interp to more than CLass
# aim to allocate forest to SS and NoFor to Class. 

#head(air)
air <- air %>% mutate(Site.Physiog_5m = "Nfor"  ) # temp fix - WHM to review      
air <- air %>% mutate(Site.Realm_5m = ifelse(Realm == "Wetland","Wetland","Terrestrial"))  # convert to terrestiral and wetland
air <- air %>% mutate(Site.Group_5m = ifelse(Realm == "Wetland","W",
                                             ifelse(Realm == "Alpine","A",
                                                    ifelse(Realm == "Rock","R",
                                                           ifelse(Realm == "Grassland","G",NA)))))                 
air <- air %>% mutate(Site.Class_5m = Class )               
air <- air %>% mutate(Site.Class_5m = ifelse(Class == "Am","Xvh",
                                             ifelse(Class == "Gb","Xvs",paste(Site.Class_5m))))                
#unique(air$Class)
#unique(air$Site.Class_5m) 
air <- air %>% mutate(Comment..max.255.characters = "Comments") 
air <- air %>% mutate(Air.Interp = "Yes", GlobalID = paste("Air_",Point_NBR,sep = "" )) 
air <- air %>% dplyr::select(-c("Comments", "geometry","Id","Point_NBR","Realm", "Class", "Assoc"))

#air.sf = st_as_sf(air, coords = c("X","Y")) # read in as sf object
#st_crs(air.sf) = 3005 

###########################################################
# Step 1:  open points file and extract Lats and Longs
###########################################################
pts = read.csv(paste(field.data.folder,"/",pts.file,sep = ''),stringsAsFactors = FALSE)
pts = pts %>% dplyr::select(-X)
pts.sf = st_as_sf(pts, coords = c("Longitude","Latitude")) # read in as sf object
st_crs(pts.sf) = 4326   # assign a CRS based on data collection 
pts.BC = st_transform(pts.sf,3005) # convert CRS to BC albers
pts.BC = as.data.frame(cbind(pts.BC,st_coordinates(pts.BC))) #,st_coordinates(pts.sf))) 
pts.BC = pts.BC %>% dplyr::select(-geometry)
#ggplot(pts.BC) + geom_sf(data = pts.BC, colour = "red", fill = NA)


pts.all <- smartbind(pts.BC,air) # join air interp to other field data
#length(air$X); length(pts.BC$X) ; length(pts.all$ObjectID)  ## error check the details. 

row.names(pts.all)<-NULL 
LatLon <- pts.all %>% dplyr::select(c(X,Y,GlobalID))     # extract the Lat/Longs. 
coordinates(LatLon)=~X + Y
proj4string(LatLon)=CRS("+init=epsg:3005") # set it to lat-long

pts.sp = cbind(LatLon,coordinates(LatLon))

###########################################################################
# list contains all raster files we want to extract the attributes from. 
layers.list = as.list(list.dirs(input.folder,full.names=TRUE))

# create a list of layer files to use 
LOI = c(layers.list[16],layers.list[3] )#,layers.list[18])#,layers.list[4])

# loop through all data folders/data sets to generate the csv attribute files for 5,10,25m scales. 

for(ii in 1:length(LOI)) { 
  #ii = 1
  i = LOI[ii]
  i.name = gsub(input.folder,"",paste(i))
  i.name = gsub("/layers","",i.name)
  i.scale = gsub("/Dec_","",i.name)
  
  Covariates <-  list.files(path= paste(i),recursive=TRUE, full.names=TRUE, all.files=TRUE, pattern ="\\.tif$")
  Covariates <-  stack(Covariates) #,quick = TRUE)
  #plot(Covariates)
  
  proj4string(Covariates) <- CRS("+init=epsg:3005")   # http://spatialreference.org/ref/    Albers BC is espg:3005+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0 
  attributes <- raster::extract(Covariates, coordinates(pts.sp), df=TRUE)
  attributes <- as.data.frame(attributes)
  
  LatLon <- as.data.frame(LatLon)
  #pts.sp <- as.data.frame(pts.sp)
  training <- cbind(LatLon,attributes)
  training$scale = paste(i.scale)
  rownames(training) <- NULL
  write.csv(training, paste(out.folder,i.name,"_pts_att.csv", sep=""))
}


################################################################################
# Step 3: Insersect BGC layer with pts data and add new field.

# using sf 
pts.sf = st_as_sf(pts.all, coords = c("X","Y")) # read in as sf object
st_crs(pts.sf) = 3005   # assign a CRS based on data collection 

# read in BGC layer (created previously in ArcMap)
## Set your input geodatabases ## edit these to your filepath and name of gdb
subset(ogrDrivers(), grepl("GDB", name))
sslist <- ogrListLayers(pem.gdb); print(sslist)

BGC.0 = st_read(dsn = pem.gdb,layer = "BEC_WL_AOI") # read in the layer of interest
BGC.0 = BGC.0 %>% dplyr::select(c(MAP_LABEL)) ; head(BGC.0)

## plot the results: 
#plot(st_geometry(pts.sf),col = "red")
#plot(st_geometry(BGC.0),add =  T)

# intersect the points with the mapped BGC units
pts.int <- st_intersection(pts.sf,BGC.0)   # intersect with ranges # this may take some time
pts.int.df <- data.frame(pts.int)
pts.int.df <- cbind(pts.int.df,st_coordinates(pts.int))

pts.int.df <- pts.int.df %>% dplyr::select(-(geometry)) #; #head(pts.int.df)
pts.out <- pts.int.df

# check if the called BGC is the same as the mapped BGC if the same = 0, if different = 1.
pts.out$BGC_test <- mapply(grepl,pattern=pts.out$Biogeoclimatic.Unit, x=pts.out$MAP_LABEL)
#test<- pts.out %>% group_by(Biogeoclimatic.Unit) %>% summarise(count = n()) ; test

# update the BGC for the air interp values (as this currently doesn not have one)
air <- pts.out %>% dplyr::filter(Air.Interp == "Yes")
non.air <- pts.out %>% dplyr::filter(is.na(Air.Interp))

# update the BGC for the air interp: 
air <- air%>% mutate(Biogeoclimatic.Unit = MAP_LABEL)
pts.out = rbind(non.air,air)   # ; length(pts.out$ObjectID)

#test<- pts.out %>% group_by(Biogeoclimatic.Unit) %>% summarise(count = n()) ; test
#unique(pts.out$Air.Interp)        

pts.file.out = "AllDeception_Pts_Consolidated_WHM_BGC_Air.csv"

write.csv(pts.out,paste(field.data.folder,"/",pts.file.out,sep = ''))

########################################################################################

## Set up the empty tables to write to 

## may adjust this to excel version at some point in the future.....



## The first time you run this script you need to generate blank tables for each level of response to aggregate resulst summary
## this is to set up table which will house the summary data 
## NOTE : only need to do this ONCE! before running models; 

## Responses 
#response.file <- c("Biogeoclimatic.Unit","Site.Physiog_5m","Site.Realm_5m","Site.Group_5m") #,"Site.Class_5m","Site.Series.Map.Unit_5m")
#for ( i in 1:length(response.file)){
#  #i = 1
#  temp.res = response.file[i]
#  temp.data = as.data.frame(unique(pts.0[[paste(temp.res)]])); colnames(temp.data)<-paste(temp.res)
#  write.csv(temp.data, file= paste(model.folder,"/",temp.res,"_prop.correct.csv",sep=""),row.names = FALSE)
#             }



