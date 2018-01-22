#Load the ModelMap program
library("ModelMap")

#In addition to Random Forests, the program will also do a model based on  Stochastic Gradient Boosting

model.type <-"RF"


#define the data file - this is a file that contains the points that I image-interpreted 
#and then I extracted the values for the underlying raster grids (which include aspect, slope, elevation etc.)
qdatafn <- read.table("D:\\McQueenLake\\mcqueen_idfxh2_attributes4.csv", sep =",", header=TRUE)
str(qdatafn)
names(qdatafn)
#get the working directory and assign it to the variable "folder" so we know where to put the results of the analysis
#this is also the folder that contains the data layers
folder <- "D:\\McQueenLake\\ASC_nonull\\"

# Give a name for the model that will be used to identify the outputs 
MODELfn <- "MMap_McQueen_idfxh2_4"

#Identify the factors in the csv file that contain data to help predict the individual site series
predList <- c("aspect.asc",
              "chm.asc",
              "convergindex.asc",
              "dianiheating.asc",
              "directinsol.asc",
              "mrrtf.asc",
              "mrvbf.asc",
              "negativeopen.asc",
              "overflowdist.asc",
              "positiveopen.asc",
              "slope.asc",
              "terrruggind.asc",
              "topoposind.asc",
              "topowetind.asc",
              "totalcurv.asc",
              "vdisttochan.asc")
              
#If any of the predictors were categorical variables I would identify them with the following command
#except it would be predFactor <- c("List the variable") - Don't forget the c in front of the first parenthesis
predFactor <- FALSE

#predFactor <- FALSE  #I would type this if there were no categorical variables

#Define the response variable and state whether it is binary (present absent 1/0) or continuous or categorical.  These variables match
#the names I have used in the csv file.

response.name = "SiteUnit"

response.type <- "categorical"

#Not sure what the seed does except starts the model from a given point. Don't change this. 
seed <- 44

#The csv file must have a unique value for each point-the unique rowname is in column "ID"
unique.rowname <- "Id"

#This limits the number of rows that the program reads at a given point so you don't blow up the memory.  I am not sure
#how much to put here - this is the value they used in the example
numrows = 500

#This identifies the csv file that is a crosswalk table between the predicted factors (elev, slope, etc.) and the names of the
#raster grids that each one is represented by - you don't need the full path in the name of the file as suggested by the ModelMap pdf file

rastLUTfn <- "D:\\McQueenLake\\mcqueen_LUT.csv"

#I guess this reads the csv file
rastLUTfn <- read.table(rastLUTfn, header = FALSE, sep = ",", stringsAsFactors = FALSE)
rastLUTfn[, 1] <- paste(folder, rastLUTfn[, 1], sep = "/")

#MODEL CREATION
#This builds the model for the 103 site series using all of the previous information I have typed in above.
model.obj.ex3 <- model.build(model.type = model.type, qdata.trainfn = qdatafn,
                             folder = folder, unique.rowname=unique.rowname, MODELfn = MODELfn, predList = predList,
                             predFactor = predFactor, response.name = response.name,
                             response.type = response.type, seed = seed)
model.obj.ex3

#MODEL DIAGNOSTICS
#This generates several diagnostic graphs of the model including Out-Of_bag model predictions including
#CSv files of presence-absence thresholds optimized by 12 criteria.  It also produces a Variable importance map.
#It also generates a histogram, calibration plot, a ROC plot and AUC plot and error rate
model.pred.ex3 <- model.diagnostics(model.obj = model.obj.ex3,
                                    qdata.trainfn = qdatafn, folder = folder, MODELfn = MODELfn,
                                    unique.rowname = unique.rowname, prediction.type = "OOB",
                                    device.type = c("jpeg", "pdf"), cex = 1.2)
#model.pred.ex3
#MAP PRODUCTION
#The function model.mapmake() creates an ascii text files and an imangine image file of predictions for each map pixel.

model.mapmake(model.obj = model.obj.ex3, folder="D:\\McQueenLake\\ASC_nonull\\", MODELfn=MODELfn, rastLUTfn=rastLUTfn, na.action = "na.omit")
