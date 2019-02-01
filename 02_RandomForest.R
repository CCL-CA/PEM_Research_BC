
# script 2: Run the random forest models based on subset of selected variables: 

## Install packages and check libraries;  # note this only needs to be run once
#install.packages(c("raster","rgdal","RSAGA", "tidyr","dplyr", "ModelMap","randomforest",dep = T)) 

#.libPaths("E:/R packages351") # specify the location of libraries is not using default.

library(dplyr)
library(tidyr)
library(raster)
library(rgdal)
library(sp)
library(rgdal)
library(ModelMap)
library(dplyr)
library(sf)
library(ggplot2)
library(stringr)
library(randomForest)
library(gsubfn)
library(tidyverse)

rm(list=ls())

# INPUT 1: set up location of drives to input and output

setwd("D:/PEM_DATA/")#check the home directory  # set up work directory 

    # read in the list with all model parameters
    mparam <- read.csv("Model_param.csv",header = TRUE,stringsAsFactors = TRUE)
    
    
# INPUT 2: select the model you want to run (Ammend to run multiple models) # add a loop here 
m.no <- 2

    # set up folders 
    mp <- mparam[m.no,] #view list of parameters
    field.data.folder = mparam[m.no,"field.data"]
    in.folder = mparam[m.no,"in.folder"]
    model.folder = mparam[m.no,"model.folder"]
    layer.folder = mparam[m.no,"layer.folder"]
    map.output.folder = mparam[m.no,"map.output.folder"]
    
    # set up model name and description
    MODELfn <- mparam[m.no,"model.no"]
    M.descrip <- mparam[m.no,"M.description"]
    
    #read in the raw point file 
    pts.file = mparam[m.no,"raw.data.file"]
    pts.0 = read.csv(paste(field.data.folder,"/",pts.file,sep = ''),stringsAsFactors = FALSE)     #head(pts.0) ; length(pts.0$Longitude) # error checks

    # subset the columns of interest
    pts = pts.0 %>% dplyr::select(c(Longitude, Latitude,GlobalID,Biogeoclimatic.Unit,Site.Physiog_5m,Site.Realm_5m,Site.Group_5m,Site.Class_5m,
                                    Site.Association_5m,Site.Series.Map.Unit_5m,Site.Var.Phase.Map.Unit_5m,MAP_LABEL,BGC_test,Crew,Experience,Random.Point.ID,Certainty,Transition))
    pts <- pts %>% dplyr::filter(Site.Series.Map.Unit_5m != "")# remove rows with no site series data

    
# PART 1: TESTING DATA QUALITY METRICS: Certainty/Staff/transition zone / random points used 
    
    # Certainty: subset the sample data by certainty 
    cert <- as.character(mparam[m.no,"Certainty"])
    cert.r <- regmatches(cert, gregexpr("[[:digit:]]+", cert)) ; cert.r <- as.numeric(unlist(cert.r))
    pts <- pts %>% dplyr::filter(Certainty %in% cert.r)
    
    # Transition Zone: subset the sample data by transition zone 
    trans <- as.character(mparam[m.no,"Transition"])
    trans.r <- regmatches(trans, gregexpr("[[:digit:]]+", trans)) ; trans.r <- as.numeric(unlist(trans.r))
    pts <- pts %>% dplyr::filter(Transition %in% trans.r)
    
    # Field Crew Experience: subset the sample data by experience of field staff
    expe <- as.character(mparam[m.no,"Experience"])
    expe.r <- regmatches(expe, gregexpr("[[:digit:]]+", expe)) ; expe.r <- as.numeric(unlist(expe.r))
    pts <- pts %>% dplyr::filter(Experience %in% expe.r)

    ## STILL TO COMPLETE 
    # Random Points: subset the sample data by random point or not 
    #randpt <- as.character(mparam[m.no,"Random.Pt.ID"])
    #if(randpt == "yes") { 
    #  pts <- pts %>% dplyr::filter(is.na(Random.Point.ID))} else {
    #    pts <- pts %>% drop_na( Random.Point.ID) } 
    
  
    # Field Crew Individuals: subset the sample data by crew = blank = all staff
    #"BJR","BJR,WM","BJR,WM,EC","EAC","EAC,WHM","EBL","KSD","PRD","WHM","WHM,AMR","WHM,BJR", "WHM,KSD","WHM,EAC", "WHM,EAC","WHM,EAC,KSD","HPG"  
    crew.r <- as.character(mparam[m.no,"Crew"])
    crew.r.oi <- str_split(crew.r, ",", n = Inf, simplify = FALSE)  # to upper needed?
    crew.r.oi  <- as.character(str_trim(unlist(crew.r.oi),"both"))
    if (crew.r.oi == "") { 
      pts <- pts } else { 
      pts <- pts %>% filter(str_detect(Crew, crew.r.oi))}
    
     
# PART 2: SELECTING / Limiting to Levels with BGC vs Forest/Non - Forest TESTING DATA QUALITY METRICS: Certainty/Staff/transition zone / random points used
                
## Subset the data per BGC unit: either by field defined BGC or by GIS defined BGC
    ## This includes: 
    ## 1) Biogeoclimatic.Unit
    ## 2) Site.Group_5m : Forest or non_forest 
    ## 3) Site.Series.Map.Unit_5m  () site series within the SS name? 
    ## 4) GIS mapped BGS = (MAP_LABEL),Note use the column BGC_test if you want to check the difference between the field assigned BGC and the GIS defined BGC
                
                
      # BGC Unit : subset the sample data by BGC assigned during the survey 
      bgc.unit <- as.character(mparam[m.no,"BGC.unit"])
      bgc.unit.oi <- str_split(bgc.unit, ",", n = Inf, simplify = FALSE) 
      bgc.unit.oi  <- as.character(str_trim(unlist(bgc.unit.oi ),"both"))
      if (bgc.unit.oi == "") { 
        pts <- pts } else { 
          pts <- pts %>% filter(Biogeoclimatic.Unit %in% bgc.unit.oi)}
      
      # Forest: Non-Forest Testing : subset the sample data by BGC assigned during the survey   ## 2) Site.Group_5m : Forest or non_forest 
      Fnonf <- as.character(mparam[m.no,"Site.Phys"])
      Fnonf.oi <- str_split(Fnonf, ",", n = Inf, simplify = FALSE) 
      Fnonf.oi  <- as.character(str_trim(unlist(Fnonf.oi ),"both"))
      if (Fnonf.oi == "") { 
        pts <- pts } else { 
          pts <- pts %>% filter(str_detect(Site.Physiog_5m, Fnonf.oi))}
    
      ## Terrestrial or Wetland: Site.Realm : Terrestrial or Wetland 
      TerWet <- as.character(mparam[m.no,"Site.Realm"])
      TerWet.oi <- str_split(TerWet, ",", n = Inf, simplify = FALSE) 
      TerWet.oi  <- as.character(str_trim(unlist(TerWet.oi ),"both"))
      if (TerWet.oi == "") { 
        pts <- pts } else { 
          pts <- pts %>% filter(str_detect(Site.Realm_5m,TerWet.oi ))}
      

      print(paste("You have",length(pts$Longitude) ,"samples for this model",sep = " ")) 

       
## ---------- DECISION 3: Select the scale at which the points were extracted (5m,10m,25m) ---------------------
      
 
      
#att.files = list.files(in.folder) # this provides a list of the csv files generated with attributes 
att.files = list.files(path=paste(in.folder,"/"),recursive=TRUE, full.names=FALSE, all.files=TRUE, pattern ="\\_pts_att.csv")
      
      
# need to clean this bit to select the scales (run through a loop)       
file1 = att.files[2]            # choose the scale of interest (change the # to select a different scale)

#file1 <- select.list(att.files, multiple = TRUE,
#                             title = "Choose Folder Containing Spatial Files",graphics = TRUE)

scale.analysis = str_sub(gsub("*D_|_pts.*","", file1),start = -3)   # grab the scale to add to summary table

foi.0 = read.csv(paste(in.folder,file1,sep = "/"),header = TRUE) # read in the file
foi.0 = foi.0 %>%
  dplyr::select(-c(X,Longitude,Latitude,ObjectID,ID)) 

qdatafn = left_join(pts.t,foi.0, by = "GlobalID") # join the attribute file to the Site series data set (field data)

# tidy up the data set 
qdatafn <- qdatafn %>%
  mutate (ID = seq(1,length(qdatafn$GlobalID),1)) %>%
  dplyr::select(-c(GlobalID))

        # GP Notes to fix error when running model (if using na.omit) If your subset contains 
        #levels(droplevels(qdatafn$SiteSeries))
        #qdatafn$SiteSeries <- factor(qdatafn$SiteSeries);
        #groupA <- droplevels(dataset2[dataset2$order=="groupA",])

# create a testing and training data set by random allocation of points 
# User needs to define the proportion of test/training set 

prop.test=0.2 # change this as needed 

get.test(proportion.test=prop.test,
          qdatafn=qdatafn,
          seed=42,
          folder=in.folder,
          qdata.trainfn="qdatafn.train.csv",
          qdata.testfn="qdatafn.test.csv")

#qdatafn <- is.matrix(qdatafn)

#get the working directory and assign it to the variable "folder" so we know where to put the results of the analysis
#this is also the folder that contains the data layers

###############################################################
## SET UP THE MODEL PARAMETERS
###############################################################

#In addition to Random Forests, the program will also do a model based on  Stochastic Gradient Boosting
model.type <-"RF"

# Give a name for the model that will be used to identify the outputs 
MODELfn <- "ModelTestingSBBS"
M_description <-  "TemporaryRuns" #write in a description here as you want "

#Identify the factors in the csv file that contain data to help predict the individual site series
predList <- c("AnisotropicHeating",
              "CHM",
              "GeneralCurvature",
              "Dec_dem_BCALbers",
              "MultiResValleyBottomFlatness",
              "TWI",
              "Slope",
              "Openness_Negative",
              "Openness_Positive",
              "TerrainRuggedness",
              "TopographicPosition",
              "Biogeoclimatic.Unit",
              "Li_below2ave",	
              "Li_below2min",	
              "Li_below2max",	
              "Li_demcov",	
              "Li_p95",
              "NDVI",
              "Sen_B01",
              "Sen_B02",	
              "Sen_B03",	
              "Sen_B04",	
              "Sen_B05",
              "Sen_B06",
              "Sen_B07",	
              "Sen_B08",
              "Sen_B08A",	
              "Sen_B09",
              "Sen_B10",
              "Sen_B11",
              "Sen_B12",
              "Sen_TCI.1",	
              "Sen_TCI.2",
              "Sen_TCI.3")
            
#If any of the predictors were categorical variables I would identify them with the following command
#except it would be predFactor <- c("List the variable") - Don't forget the c in front of the first parenthesis

predFactor <- ("Biogeoclimatic.Unit")

#predFactor <- FALSE  #I would type this if there were no categorical variables #BGC layer? 

# calcaulte the no of input parameters to be used later on in summary table 
if (predFactor == FALSE){ length.pred = 0}else { length.pred = length(predFactor) }
no.params <-length(predList) + length.pred

#Define the response variable and state whether it is binary (present absent 1/0) or continuous or categorical.  These variables match
#the names I have used in the csv file.

response.name = "Site.Series.Map.Unit_5m" 

#or
#response.name <- select.list(colnames(pts.t[,5:11]), multiple = FALSE,
#                             title = "Choose Functional Level",graphics = TRUE)

#qdatafn2 <- qdatafn

####Optional remove categories with < X training points
response.number <- count(qdatafn, vars= qdatafn[,c(response.name)] )
response.good <-response.number[response.number$n >10,] ## set minimum number of training points to allow inclusion of unit
qdatafn2 <- qdatafn [qdatafn[,c(response.name)] %in% response.good$vars,]

###########sample size calculation for rebalancing (or not)
sampsize <- count(qdatafn2,vars= qdatafn2[,c(response.name)] )
sampsize <- sampsize$n
#sampsize = 30
response.type <- "categorical"

#Not sure what the seed does except starts the model from a given point. Don't change this. 
seed <- 44

#The csv file must have a unique value for each point-the unique rowname is in column "ID"
unique.rowname <- "ID"
#unique.rowname <- "GlobalID"

#This limits the number of rows that the program reads at a given point so you don't blow up the memory.  I am not sure
#how much to put here - this is the value they used in the example
numrows = 400

#as.data.frame(qdatafn)
#This identifies the csv file that is a crosswalk table between the predicted factors (elev, slope, etc.) and the names of the
#raster grids that each one is represented by - you don't need the full path in the name of the file as suggested by the ModelMap pdf file

#MODEL CREATION
#This builds the model for the 103 site series using all of the previous information I have typed in above.
model.obj.ex3 <- model.build(model.type = model.type, 
                             qdata.trainfn = qdatafn2,
                             folder = model.folder, unique.rowname= unique.rowname, MODELfn = MODELfn, predList = predList,
                             predFactor = predFactor, response.name = response.name, ntree = 100, sampsize = sampsize,
                             response.type = response.type, seed = seed, na.action = "na.roughfix", replace=TRUE)


## Note if you get this error: You need to drop all factors from your subset.  (See code above) 
      #Error in randomForest.default(x, y, mtry = mtryStart, ntree = ntreeTry,  : 
      #Can't have empty classes in y.
## this is also caused when you use na.omit in the model code as it removes NAs and hence you end up with levels that are misssing in your data set that cant be fixed above/
##Save output
print(model.obj.ex3$confusion, digits=2)
#Novel <-outlier(model.obj.ex3, X1)
write.csv(model.obj.ex3$confusion[, 'class.error'], file= paste(model.folder,"/",MODELfn,"/", MODELfn,"_Confusion_by_MapUnit.csv",sep=""))

write.csv(model.obj.ex3$proximity, file= paste(model.folder,"/",MODELfn,"/", MODELfn, "_Proximity.csv",sep=""))
write.csv(model.obj.ex3$importance, file= paste(model.folder,"/",MODELfn,"/",MODELfn, "_Importance.csv",sep=""))
write.csv(model.obj.ex3$err.rate, file= paste(model.folder,"/",MODELfn,"/",MODELfn, "_Error.csv",sep=""))
write.csv(model.obj.ex3$confusion, file= paste(model.folder,"/",MODELfn,"/",MODELfn, "_ConfusionMatrix.csv",sep=""))
VIP <- varImpPlot(model.obj.ex3, sort=TRUE) 
write.csv(VIP, file= paste(model.folder,"/",MODELfn,"/",MODELfn, "_VariableImport.csv",sep=""))
#dev.copy(pdf,(paste(model.folder,"/",MODELfn,"/", MODELfn ,'VarImpPlot.pdf')))
#dev.off()

#MODEL DIAGNOSTICS
#This generates several diagnostic graphs of the model including Out-Of_bag model predictions including
#CSv files of presence-absence thresholds optimized by 12 criteria.  It also produces a Variable importance map.
#It also generates a histogram, calibration plot, a ROC plot and AUC plot and error rate

# create a folder with the name of the model outputs

dir.create(model.folder<-file.path(model.folder,MODELfn ), showWarnings = TRUE)


model.pred.ex3 <- model.diagnostics(model.obj = model.obj.ex3,
                                    qdata.trainfn = qdatafn2, folder = model.folder, MODELfn = MODELfn,
                                    unique.rowname = unique.rowname, prediction.type = "OOB",
                                    #device.type = c("jpeg", "pdf"), cex = 1.2) # ajust this if you want the outputs in another format 
                                    device.type = "pdf", cex = 1.2)


## Note if you get this error: You need to drop all factors from your subset.  
      #Error in randomForest.default(x, y, mtry = mtryStart, ntree = ntreeTry,  : 
      #Can't have empty classes in y.


## This line is working only for some catergories
#model.interaction.plot(model.obj.ex3, x = "GeneralCurvature", y = "TWI", plot.type= "image", device.type = "pdf", MODELfn = MODELfn, folder = model.folder)


#MODEL FIT METRICS
# calculate the confusion matrix 
pred <- read.table(paste(model.folder,"/",MODELfn,"_pred.csv",sep = ""),header = TRUE, sep = ",", stringsAsFactors = TRUE)

pred$pred<- as.factor(pred$pred) ; pred$obs<- as.factor(pred$obs)
sslevels <- unique(c(levels(pred$pred),levels(pred$obs)))
pred$pred<- factor(pred$pred,levels = sslevels)
pred$obs<- factor(pred$obs,levels = sslevels)
CMX <- table(predicted = pred$pred,observed = pred$obs)
CMX.diag <- diag(CMX)
CMX.OMISSION <- 1-(CMX.diag/apply(CMX,2,sum))
Ave.om = mean(CMX.OMISSION,na.rm = TRUE)
CMX.COMISION <- 1-(CMX.diag/apply(CMX,1,sum))
Ave.com = mean(CMX.COMISION,na.rm = TRUE)
CMX.PCC <- sum(CMX.diag)/sum(CMX)  #Percent Correctly Classified
# estimate Kappa and Kappa (SD)
CMX.KAPPA <-PresenceAbsence::Kappa(CMX)
kap = CMX.KAPPA[1,1]; kap.sd = CMX.KAPPA[1,2]

# create a table with ommission and commision per model 
OM <- cbind(CMX.OMISSION,CMX.COMISION)
OM <- data.frame(OM)
OM <- tibble::rownames_to_column(OM, "SS")
OM <-rbind(c("Model",MODELfn,MODELfn),OM)
#write.csv(OM,paste(model.folder,"/","MODEL_OM_COM.csv",sep = ""),row.names = FALSE) # (add if statement) if running the script from scratch write this line out 

# MAUC # this bit is not quite working yet
#response = pred$obs
#response1 = gsub("/",".",response)
#VOTE = HandTill2001::multcap(response = gsub("/",".",pred$obs),
#                             predicted = as.matrix(pred[,-c(1,2,3)]))
#mauc <- HandTill2001::auc(VOTE)



#model.folder = "Analysis/RandomForest/outputs"

# output the model output by adding a row to a table with all imputs
sum.data <- read.csv(paste(model.folder,"/","MODEL_OUTPUTS.csv",sep = ""),header = TRUE,stringsAsFactors = FALSE)
dline = c(MODELfn,M.descrip, no.params,scale.analysis,CMX.PCC,Ave.om,Ave.com,kap,kap.sd)
sum.data <- rbind(sum.data,dline)
write.csv(sum.data,paste(model.folder,"/","MODEL_OUTPUTS.csv",sep = ""),row.names = FALSE)

# output the model omission/ commission values by adding columnsto a table with all imputs
OM.data <- read.csv(paste(model.folder,"/","MODEL_OM_COM.csv",sep = ""),header = TRUE,stringsAsFactors = FALSE)
OM.data <- left_join(OM.data,OM,by = "SS")  # might need to adjust this one to another join if there is new Site Series classifications
write.csv(OM.data,paste(model.folder,"/","MODEL_OM_COM.csv",sep = ""),row.names = FALSE)


#################################
# MAP PRODUCTION # produce a map for the current model 
#################################
data.folder <- gsub("*D_|_pts.*","", file1)

folder = paste(layer.folder,data.folder,"layers/",sep = "/") # create the filepath where the layers are stored (use the same scale as points extracted from )
list.files(folder,pattern ="\\.tif$")

rastLUTfn <- "ModelMapData_LUT.csv" # need to update this with more layers
rastLUTfn <- read.table(paste(in.folder,rastLUTfn,sep = "/"),
                        header=FALSE,
                        sep=",",
                        stringsAsFactors=FALSE)
rastLUTfn[,1] <- paste(folder,rastLUTfn[,1],sep="/")


#The function model.mapmake() creates an ascii text files and an imangine image file of predictions for each map pixel.

model.mapmake( model.obj=model.obj.ex3,
               folder=model.folder,
               MODELfn=MODELfn,
               rastLUTfn=rastLUTfn,
               na.action="na.omit")#,
               # Mapping arguments
               #map.sd=TRUE)
MODELfn

#############################################################
# Plot 1: 
l <- seq(100,0,length.out=101)
c <- seq(0,100,length.out=101)
col.ramp <- hcl(h = 120, c = c, l = l)

opar <- par(mfrow=c(1,2),mar=c(3,3,2,1),oma=c(0,0,3,4),xpd=NA)
mapgrid.a <- raster(paste(model.folder,"/",MODELfn,"_map.img",sep=""))
#mapgrid.b <- raster(paste(model.folder,"/",MODELfn.b,"_map.img",sep=""))
zlim <- c(0,max(maxValue(mapgrid.a)))
legend.label<-rev(pretty(zlim,n=23))
legend.colors<-col.ramp[trunc((legend.label/max(legend.label))*100)+1]

label.doc = read.csv(paste(model.folder,"/",MODELfn,"_map_key.csv",sep = ""))
category = label.doc$category

legend.label<-paste(legend.label,category,sep="")
image(mapgrid.a,
      col=col.ramp,
      xlab="",ylab="",xaxt="n",yaxt="n",
      zlim=zlim,
      asp=1,bty="n",main="")

mtext(response.name,side=3,line=1,cex=1.2)

legend( x=xmax(mapgrid.a),y=ymax(mapgrid.a),
        legend=legend.label,
        fill=legend.colors,
        bty="n",
        cex=1)
#mtext("Percent Cover",side=3,line=1,cex=1.5,outer=T)
par(opar)

# -------- CALCULATE THE SUMMARY TABLE FOR PROPORTION OF PREDICTED SS --------------------------------------------- ## 

#Extract all the locations within the raster and summary 
freq.ss = data.frame(freq(mapgrid.a))
freq.ss <- merge(label.doc,freq.ss, by.x = "integercode",by.y ="value" )

# all rasters squares (25m)
all.locations.25 <- sum(freq.ss$count)  # note this will change on the scale at which the predictions are being made

freq.ss <- freq.ss %>%
  dplyr:: select(category,count) %>% 
  mutate(pc_predict = round((count/all.locations.25)*100,2))

freq.ss <-rbind(c("Model",MODELfn,MODELfn),freq.ss)

# if exists statement 
map.pred <-read.csv(paste(model.folder,"/","MAP_Predict_pc.csv",sep = ""))
map.pred <- left_join(map.pred,freq.ss,by = "category")

#length(map.pred$category) 
#length(freq.ss$category)

write.csv(freq.ss,paste(model.folder,"/","MAP_Predict_pc.csv",sep = ""),row.names = FALSE)

##xx = data.frame(mapgrid.a)



