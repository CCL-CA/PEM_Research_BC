# Script 2: Run the random forest models based on subset of selected variables: 
# Feb 27th 2019 

# HOW TO RUN THIS SCRIPT:
# 1) set up model parameters in the accompanying xlxs sheet
# 2) ensure you have run script 1: 01_extract_pt_values.R

## Install packages and check libraries;  # note this only needs to be run once
#install.packages(c("raster","rgdal","RSAGA", "tidyr","dplyr", "ModelMap","randomforest",dep = T)) 

# note if having problems loading the xlsx package or rJava package see here:
#https://www.r-statistics.com/2012/08/how-to-load-the-rjava-package-after-the-error-java_home-cannot-be-determined-from-the-registry/
#https://stackoverflow.com/questions/2399027/cannot-load-rjava-because-cannot-load-a-shared-library
#Sys.setenv(JAVA_HOME='C:\\Program Files (x86)\\Java\\jre1.8.0_181\\bin') # for 32-bit version
#Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_201\\') # for 64-bit version

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
library(RColorBrewer)
library(colorspace)
library(xlsx)
library(rJava)
library(tibble)

rm(list=ls())

# INPUT 1: set up location of drives to input and output

setwd("D:/PEM_DATA/")#check the home directory  # set up work directory 

# read in the Parameters for each of the excel sheets
mparam <- read.xlsx("Model_params.xlsx", "Model_params", header=TRUE, colClasses=NA)
mlayers <-read.xlsx("Model_params.xlsx", "Model_layers", header = TRUE)       # read in csv file
mtpt <- read.xlsx("Model_params.xlsx", "Model_Tpts", header = TRUE,colClasses=NA)
mrf <- read.xlsx("Model_params.xlsx", "Model_rF", header = TRUE,colClasses=NA)
mmu <- read.xlsx("Model_params.xlsx","Model_MapUnit",header = TRUE,colClasses = NA)

m.to.run <- mparam %>% dplyr::select(To.Run,model.no) %>% dplyr::filter(To.Run == 1) # make a listof models to run
m.to.run <- m.to.run$model.no
m.to.run
m.to.run <- 4; m.no <- m.to.run # tester

if(length(m.to.run)<2) { 
  m.no <- m.to.run } else { 
    print("You are now running multiple models - this may take some time.....") } 
for (i in 1:length(m.to.run)) { 
  #i = 1
  m.no <- m.to.run[i]
  
  print (paste("currently running model:", m.no,sep = ""))
  
  # set up folders 
  mp <- mparam[m.no,] #view list of parameters
  field.data.folder = mparam[m.no,"field.data"]
  in.folder = mparam[m.no,"in.folder"]
  model.folder = mparam[m.no,"model.folder"]
  layer.folder = mparam[m.no,"layer.folder"]
  map.output.folder = mparam[m.no,"map.output.folder"]
  
  #read in the raw point file 
  pts.file = mparam[m.no,"raw.data.file"]
  pts.0 = read.csv(paste(field.data.folder,"/",pts.file,sep = ''),stringsAsFactors = TRUE,row.names = NULL)     #head(pts.0) ; length(pts.0$Longitude) # error check
  
  # subset the columns of interest
  pts = pts.0 %>% dplyr::select(c(X, Y,GlobalID,Biogeoclimatic.Unit,Site.Physiog_5m,Site.Realm_5m,Site.Group_5m,Site.Class_5m,
                                  Site.Association_5m,Site.Series.Map.Unit_5m,Site.Var.Phase.Map.Unit_5m,MAP_LABEL,BGC_test,Crew,Experience,Random.Point.ID,Certainty,Transition,Air.Interp))
  print(length(pts$X))
  
  #############################################
  # Part 1: Do you want to run the model for all BGC's/Map units or split into subzones
  # build in loop to run models in parts based on : 
  # - BGC  
  # - Site.Physiog_5m
  # - Site.Realm_5m
  # - Site.Group_5m
  # - Site.Class_5m
  # - Site.Series.Map.Unit_5m
  # - Site.Var.Phase.Map.Unit_5m
  
  #############################################3    
  # Part 2: Select the response variable
  #Define the response variable 
  response.name<- as.character(mparam[m.no,"Model.Response"]) #response.name = "Site.Series.Map.Unit_5m" 
  response.type<- as.character(mparam[m.no,"Response.type"])     #response.type <- "categorical"
  
  ##############################################
  # Part 3: Select the spatial layers 
  #Read in file that Identifies the factors in the csv file that contain data to help predict the individual site series
  m.pred.0 <-  as.character(mparam[m.no,"Spatial.layers.model"]  )  # get the training point model selected 
  m.pred<- mlayers %>% dplyr::select(c(Layers,Catergory., m.pred.0)) # get the details on the TP model from other excel sheet
  m.pred <-m.pred %>% filter(!!(as.name(paste(m.pred.0,sep =""))) == 1)                   # select layers marked with a 1.
  
  predList <- m.pred %>% dplyr::filter(Catergory. == "Continuous") %>% dplyr::select(Layers)     # filter continuous variables and create a list
  predList = as.vector(unlist(predList[["Layers"]])) 
  
  # make list of catergorical variables 
  predFactor.full <- m.pred %>% dplyr::filter(Catergory. == "Catergorical") %>% dplyr::select(Layers)
  predFactor.full<- c(predFactor.full$Layers)
  
  if (length(predFactor.full) == 0) { predFactor <-  FALSE } else {predFactor <- predFactor.full} # if no catergorical variables predFactor = FALSE
  
  # calcaulte the no of input parameters to be used later on in summary table 
  if (predFactor == FALSE){ length.pred = 0} else { length.pred = length(predFactor) }
  no.params <-length(predList) + length.pred
  no.params 
  
  #####################################
  # PART 4: TRAINING POINT SELECTION BASED ON DATA QUALITY METRICS: Certainty/Staff/transition zone / random points used
  # blank values in the excel table = include all data 
  tp.model <-  as.character(mparam[m.no,"Training.pt.model"]  )  # get the training point model selected 
  tpt.metrics <- mtpt %>% dplyr::select(c(Tpt_metrics,tp.model)) # get the details on the TP model from other excel sheet
  
  # Certainty: subset the sample data by certainty 
  cert <- as.character(tpt.metrics[1,2])
  if (is.na(cert)) { 
    pts <- pts } else  { 
      cert.r <- regmatches(cert, gregexpr("[[:digit:]]+", cert)) ; cert.r <- as.numeric(unlist(cert.r))
      pts <- pts %>% dplyr::filter(Certainty %in% cert.r) }
  
  length(pts$GlobalID)  
  
  # Transition Zone: subset the sample data by transition zone 
  trans <- as.character(tpt.metrics[2,2]) 
  if (is.na(trans)) { 
    pts <- pts } else  { 
      print("Filtering by transition zones")#}
      trans.r <- regmatches(trans, gregexpr("[[:digit:]]+", trans)) ; trans.r <- as.numeric(unlist(trans.r))
      pts <- pts %>% dplyr::filter(Transition %in% trans.r)}
  
  # Field Crew Experience: subset the sample data by experience of field staff
  expe <- as.character(tpt.metrics[3,2])  
  if (is.na(expe)) { 
    pts <- pts } else  { 
      print("filtering for experience")#)}
      expe.r <- regmatches(expe, gregexpr("[[:digit:]]+", expe)) ; expe.r <- as.numeric(unlist(expe.r))
      pts <- pts %>% dplyr::filter(Experience %in% expe.r)}
  
  #Random Points: subset the sample data by random point or not 
  # if NA then all points are used if 1 in TP1 metrics only random pts used. 
  randpt <- as.character(tpt.metrics[4,2])  
  if (is.na(randpt)) { 
    pts <- pts } else  { 
      print("using random points only ")#}
      pts <- pts %>% drop_na(Random.Point.ID) } 
  
  ## STILL TO COMPLETE 
  # Field Crew Individuals: subset the sample data by crew = blank = all staff
  crew.r <- as.character(tpt.metrics[5,2])
  crew.r.oi <- str_split(crew.r, ",", n = Inf, simplify = FALSE) 
  crew.r.oi  <- as.character(str_trim(unlist(crew.r.oi),"both"))
  if (is.na(crew.r.oi)) { 
    pts <- pts } else  { 
      print ("filtering by individual crew ")#}
      pts <- pts %>% dplyr::filter(str_detect(Crew, crew.r.oi))}
  #length(pts$X)
  
  ## STILL TO COMPLETE 
  ## Air interpt points.Note Air interp points cannot be included when mapping to site series level 
  air <- as.character(tpt.metrics[8,2])  
  if (is.na(air)) { 
    pts <- pts } else  { print ("Still need to fix the air interp filter")}
  #length(pts$X)
  
  # remove any rows where the response variable is zero    
  pts <- pts %>% dplyr::filter(response.name != "") # remove rows with no site series data
  length(pts$X)
  pts<- pts %>% drop_na(response.name)
  length(pts$X)
  
  # Subset to the minimum number of points 
  min.no <- as.character(tpt.metrics[7,2])  
  min.no <- 5
  response.number <- count(pts, vars= pts[,c(response.name)])
  response.good <-response.number[response.number$n >min.no,]             ## set minimum number of training points to allow inclusion of unit
  pts <- pts [pts[,c(response.name)] %in% response.good$vars,]
  length(pts$X)
  
  # STILL TO DO 
  # decide on what balance needs to be set (none,min,medium, etc)
  #mean.no <- round(mean(response.number$n),0)
  #length(pts$X)
  
  ###########sample size calculation 
  sampsize <- count(pts,vars= pts[,c(response.name)] )
  sampsize <- sampsize$n ; sampsize
  #length(pts$X)
  
  # add atribute values to subset fo selected points
  m.scale <- as.character(mparam[m.no,"scale"])## Select the scale at which the points were extracted (5m,10m,25m) # this may not be needed??
  
  # read in the attribute layer file generted in script 01_extract_pts 
  foi.0 = read.csv(paste(in.folder,paste("Dec_",m.scale,"m_pts_att.csv",sep = ""),sep = "/"),header = TRUE) # read in the file
  foi.0 = foi.0 %>% dplyr::select(-c(X.1)) 
  foi.0 = foi.0 %>% dplyr::select(-c(X,Y,ID)) # length(foi.0$X)
  qdatafn = left_join(pts,foi.0, by = "GlobalID") # join the attribute file to the Site series data set (field data)
  
  # tidy up the data set 
  qdatafn <- qdatafn %>% mutate (ID = seq(1,length(qdatafn$GlobalID),1)) %>% dplyr::select(-c(GlobalID))
  
  qdatafn$Site.Series.Map.Unit_5m <- droplevels(qdatafn)$Site.Series.Map.Unit_5m
  qdatafn$Site.Group_5m <- droplevels(qdatafn)$Site.Group_5m
  qdatafn$Site.Class_5m <- droplevels(qdatafn)$Site.Class_5m
  #qdatafn$Site.Class_5m <- droplevels(qdatafn)$Site.Class_5m
  #unique(qdatafn$Site.Class_5m)
  
  ###################################################
  # Part 5: Set up the model hyperparameters
  ###################################################
  
  # set up model name and description
  MODELfn <- paste("M",mparam[m.no,"model.no"],sep = "")
  M_description <- paste(mp$Training.pt.model,mp$Spatial.layers.model,mp$Map.unit.response,mp$Rf.model,sep = "_")
  
  model.type <-"RF"   #In addition to Random Forests, the program will also do a model based on  Stochastic Gradient Boosting
  
  rf.model <-  as.character(mparam[m.no,"Rf.model"]  )  # get the training point model selected 
  rf.metrics <- mrf %>% dplyr::select(c(tpts.options,rf.model)) # get the details on the TP model from other excel sheet
  
  # Propotion of data to be tested/training ertainty: subset the sample data by certainty 
  test.prop <- rf.metrics[1,2]
  
  get.test(proportion.test=test.prop,
           qdatafn=qdatafn,
           seed=42,
           folder=in.folder,
           qdata.trainfn="qdatafn.train.csv",
           qdata.testfn="qdatafn.test.csv")
  
  # set seed value
  seed <- as.numeric(rf.metrics[2,2])      #Not sure what the seed does except starts the model from a given point.
  unique.rowname <- "ID"  #The csv file must have a unique value for each point-the unique rowname is in column "ID"
  numrows = 400 # still to be automated #This limits the number of rows that the program reads at a given point so you don't blow up the memory. untested numbers
  n.tree = as.numeric(rf.metrics[3,2])
  
  #####################################################################################3
  
  ## RUN THE MODEL #######
  
  #MODEL CREATION##
  
  model.obj <- model.build(model.type = model.type, 
                           qdata.trainfn = qdatafn,
                           folder = model.folder, unique.rowname= unique.rowname, MODELfn = MODELfn, predList = predList,
                           predFactor = predFactor, response.name = response.name, ntree = n.tree, sampsize = sampsize,
                           response.type = response.type, seed = seed, na.action = "na.roughfix", replace=TRUE)
  
  ## Note if you get this error: You need to drop all factors from your subset.  (See code above) 
  #Error in randomForest.default(x, y, mtry = mtryStart, ntree = ntreeTry,  :  #Can't have empty classes in y.
  ## this is also caused when you use na.omit in the model code as it removes NAs and hence you end up with levels that are misssing in your data set that cant be fixed above/
  
  ### output metrics to model folder 
  dir.create(model.folder.out<-file.path(model.folder,MODELfn ), showWarnings = TRUE)   # create a folder with the name of the model outputs
  # write out summary files to folder
  write.csv(model.obj$confusion[, 'class.error'], file= paste(model.folder.out,"/", MODELfn,"_Confusion_by_MapUnit.csv",sep=""))
  # create a list of proportion of correct 
  map.unit.correct <- as.data.frame(model.obj$confusion[, 'class.error']) 
  colnames(map.unit.correct)<- "error"
  map.unit.correct <- tibble::rownames_to_column(map.unit.correct, response.name)
  map.unit.correct <- map.unit.correct %>% mutate(prop.correct = 1- error) %>% dplyr::select(paste(response.name), prop.correct)
  colnames(map.unit.correct)<- c(response.name,paste("M",m.no,"_prop.correct",sep = ""))
  
  # read in the cumulative results table and add columns
  prop.cor<- read.table(paste(model.folder,"/",response.name,"_prop.correct.csv",sep = ""),header = TRUE, sep = ",", stringsAsFactors = TRUE)
  #prop.cor <- prop.cor %>% dplyr::select(-c(X))
  prop.cor<- left_join(prop.cor,map.unit.correct)
  write.csv(prop.cor,paste(model.folder,"/",response.name,"_prop.correct.csv",sep = ""),row.names = FALSE) 
  
  #write.csv(model.obj$proximity, file= paste(model.folder.out,"/", MODELfn, "_Proximity.csv",sep=""))
  write.csv(model.obj$importance, file= paste(model.folder.out,"/",MODELfn, "_Importance.csv",sep=""))
  write.csv(model.obj$err.rate, file= paste(model.folder.out,"/",MODELfn, "_Error.csv",sep=""))
  write.csv(model.obj$confusion, file= paste(model.folder.out,"/",MODELfn, "_ConfusionMatrix.csv",sep=""))
  VIP <- varImpPlot(model.obj, sort=TRUE) 
  write.csv(VIP, file= paste(model.folder.out,"/",MODELfn, "_VariableImport.csv",sep=""))
  
  # write out pdf plot
  pdf(file=paste(model.folder,"/",MODELfn,"/", MODELfn ,'_VarImpPlot.pdf',sep = ""),width=12,height=8)
  varImpPlot(model.obj, sort=TRUE) 
  dev.off()
  
  #MODEL DIAGNOSTICS
  model.pred <- model.diagnostics(model.obj = model.obj,
                                  qdata.trainfn = qdatafn, folder = model.folder.out, MODELfn = MODELfn,
                                  unique.rowname = unique.rowname, prediction.type = "OOB",
                                  #device.type = c("jpeg", "pdf"), cex = 1.2) # ajust this if you want the outputs in another format 
                                  device.type = "pdf", cex = 1.2)
  
  #MODEL FIT METRICS
  pred <- read.table(paste(model.folder.out,"/",MODELfn,"_pred.csv",sep = ""),header = TRUE, sep = ",", stringsAsFactors = TRUE)
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
  OM <- tibble::rownames_to_column(OM,response.name)
  OM <-rbind(c("Model",MODELfn,MODELfn),OM)
  #write.csv(OM,paste(model.folder,"/","MODEL_OM_COM.csv",sep = ""),row.names = FALSE) # (add if statement) if running the script from scratch write this line out 
  
  # output the model output by adding a row to a table with all imputs
  #response.name
  
  sum.data <- read.csv(paste(model.folder,"/","MODEL_OUTPUTS.csv",sep = ""),header = TRUE,stringsAsFactors = FALSE)
  dline = c(MODELfn,M_description, no.params,m.scale,CMX.PCC,Ave.om,Ave.com,kap,kap.sd)
  sum.data <- rbind(sum.data,dline)
  write.csv(sum.data,paste(model.folder,"/","MODEL_OUTPUTS.csv",sep = ""),row.names = FALSE)
  
  # output the model omission/ commission values by adding columnsto a table with all imputs
  OM.data <- read.csv(paste(model.folder,"/",response.name,"_MODEL_OM_COM.csv",sep = ""),header = TRUE,stringsAsFactors = FALSE)
  OM.data <- left_join(OM.data,OM,by = response.name)  # might need to adjust this one to another join if there is new Site Series classifications
  write.csv(OM.data,paste(model.folder,"/",response.name,"_MODEL_OM_COM.csv",sep = ""),row.names = FALSE)
  
  
  #################################
  # MAP PRODUCTION # produce a map for the current model 
  #################################
  
  #data.folder <- gsub("*D_|_pts.*","", file1)
  
  layer.folder = paste(layer.folder,"/Dec_",m.scale,"m/","layers/",sep = "") # create the filepath where the layers are stored (use the same scale as points extracted from )
  list.files(layer.folder,pattern ="\\.tif$")
  rastLUTfn <- "ModelMapData_LUT.csv" # need to update this with more layers
  rastLUTfn <- read.table(paste(in.folder,rastLUTfn,sep = "/"),
                          header=FALSE,
                          sep=",",
                          stringsAsFactors=FALSE)
  rastLUTfn[,1] <- paste(layer.folder,rastLUTfn[,1],sep="")
  
  model.mapmake(model.obj=model.obj,
                folder=model.folder.out,
                MODELfn=MODELfn,
                rastLUTfn=rastLUTfn,
                na.action="na.omit")#,
  # Mapping arguments
  #map.sd=TRUE)
  
} # end loop for multiple model runs. 












## Still to do.....
## consolidate all the sheets into a single xlxs file. 
#prop.cor <- read.xlsx(paste(model.folder,"model_output_test.xlsx",sep = "/"), paste(response.name,"PC",sep = ""), header=TRUE, colClasses=NA)
#prop.cor<- left_join(prop.cor,map.unit.correct,by = "Site.Series.Map.Unit_5m")
#write.xlsx(prop.cor,paste(model.folder,"model_output_test.xlsx",sep = "/"), sheet = paste(response.name,"PC",sep = ""), 
#col.names=TRUE, row.names=FALSE, append=FALSE, showNA=TRUE, password=NULL)


#############################################################
# Plot 1: 
l <- seq(100,0,length.out=101)
c <- seq(0,100,length.out=101)
col.ramp <- hcl(h = 120, c = c, l = l)

opar <- par(mfrow=c(1,2),mar=c(3,3,2,1),oma=c(0,0,3,4),xpd=NA)
mapgrid.a <- raster(paste(model.folder.out,"/",MODELfn,"_map.img",sep=""))
zlim <- c(1,max(maxValue(mapgrid.a)))

#legend.label<-rev(pretty(zlim,n=23)) ## OR 
legend.label<-pretty(zlim,n=23)

legend.colors<-col.ramp[trunc((legend.label/max(legend.label))*100)+1]
#legend.colors<-heat.colors(max(maxValue(mapgrid.a)),alpha = 1)
#legend.colors<-diverging_hcl(28)
#legend.colors<- diverge_hcl(28, h = c(120), c = 80, l = c(80,0))
#legend.colors<-terrain_hcl(28)

image(mapgrid.a,
      col=col.ramp,
      #col = legend.colors,
      xlab="",ylab="",xaxt="n",yaxt="n",
      zlim=zlim,
      asp=1,bty="n",main="")

mtext(paste(MODELfn,": ",response.name,sep = ""),side=3,line=1,cex=1.2) # add a title
label.doc = read.csv(paste(model.folder.out,"/",MODELfn,"_map_key.csv",sep = ""))
category = label.doc$category
legend.label<-paste(legend.label,category,sep="_")

legend(x =xmax(mapgrid.a),y=ymax(mapgrid.a),
       legend=legend.label,
       fill=legend.colors,
       bty="n",
       cex=0.8)
#mtext("Percent Cover",side=3,line=1,cex=1.5,outer=T)
par(opar)

# -------- CALCULATE THE SUMMARY TABLE FOR PROPORTION OF PREDICTED SS --------------------------------------------- ## 


##  TO DO ....STILL NEED TO AUTOMATE THIS SECTION TO ACCOUNT FOR DIFFERENT SCALES


#Extract all the locations within the raster and summary 
freq.ss = data.frame(freq(mapgrid.a))
#freq.ss <- merge(label.doc,freq.ss, by.x = "integercode",by.y ="value",all.y = TRUE ) # with NA values 
freq.ss <- merge(freq.ss,label.doc, by.y = "integercode",by.x ="value" )

# all rasters squares (25m)
all.locations.25 <- sum(freq.ss$count)  # note this will change on the scale at which the predictions are being made

freq.ss <- freq.ss %>%
  dplyr:: select(category,count) %>% 
  mutate(pc_predict = round((count/all.locations.25)*100,2))

freq.ss <-rbind(c("Model",MODELfn,MODELfn),freq.ss)

# if exists statement  

## STILL TO DO ....ADjust this to add a column per model as curently overwritting each model. 

map.pred <-read.csv(paste(model.folder,"/","MAP_Predict_pc.csv",sep = ""))
map.pred <- left_join(map.pred,freq.ss,by = "category") # NA where not predicted 

write.csv(freq.ss,paste(model.folder,"/","MAP_Predict_pc.csv",sep = ""),row.names = FALSE)


} # end of loop for multiple models 









# Write the first data set in a new workbook
write.xlsx(USArrests, file = "myworkbook.xlsx",
           sheetName = "USA-ARRESTS", append = FALSE)
# Add a second data set in a new worksheet
write.xlsx(mtcars, file = "myworkbook.xlsx", 
           sheetName="MTCARS", append=TRUE)
# Add a third data set
write.xlsx(iris, file = "myworkbook.xlsx",
           sheetName="IRIS", append=TRUE)



