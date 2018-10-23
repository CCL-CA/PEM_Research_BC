#################################################################################################
# PEM Project 2018 
#
# Purpose : - Collate data collected from survey123 forms into managable dataset 
#           - Quick summary of data colected to date. 
# 
# Date: June 6th 2018 & Sept 18th 
#
# Any problems contact: genevieve.perkins@gov.bc.ca
#################################################################################################
# 
#Prep Instructions: 
#1) download most recent data from Survey123 and export files as CSV format 

# get libraries
x <- c("dplyr","ggplot2","tidyr","stringr") #,"rgeos", "maptools", "dplyr", "tidyr", "tmap", "ggplot2","raster","readxl", "gdata")
lapply(x, library, character.only = TRUE) # load the required packages

# set up workspace and list of files to open 
#wd.save <-getwd()
#out.dir <- paste(wd.save,"/Outputs/",sep = "")
#all.files.0 <- list.files(path='.', pattern= '.csv', all.files = TRUE, full.names=T, recursive=T, include.dirs=TRUE, ignore.case = TRUE)  # list all directories/files containing TimelapseData.ddb

graph.dir = "C:/PEM_DATA/Data/Field_data/Summary_graphs/"
data.dir = "C:/PEM_DATA/Data/Field_data/"

##########################################################################################################

# Deception V1

#########################################################################################################
# 95 columns

data2.0 <- read.csv(paste(data.dir,"Deception_06June_Raw.csv",sep = ""),stringsAsFactors=FALSE)
#data2.0 <- read.csv(paste(wd.save,dec.files[1],sep = ""),stringsAsFactors=FALSE)
##Need to split out early dates the reorder (first SIte series are in 1m and onlt appear if random points are added.

data2.1 = data2.0[c(1:8),]
data2.2 = data2.0[c(9:length(data2.0$ObjectID)),]

# fix the first part of data (23-5-2018)
data2.1 = data2.1 %>% 
  select(ObjectID, GlobalID,Crew, Date, General.Location, Latitude, Longitude, Random.Point.ID, Certainty, y, x, Accuracy..m., Elevation..m.,Biogeoclimatic.Unit,Site.Group,
         Site.Series.Map.Unit.1,Transition,Structural.Stage, Structural.Stage.Modifier, Site.Modifier, Soil.Moisture.Regime,
         Soil.Nutrient.Regime, Type.of.Disturbance, Site.Disturbance..s., Other.Site.Disturbance, Comment,CreationDate,Creator,EditDate,Editor,Point.ID)

data2.1<- data2.1 %>% 
  rename(BGC = Biogeoclimatic.Unit) %>% 
  rename(Site.Group_5m = Site.Group) %>%
  rename(Transition_5m = Transition) %>%
  rename(Site.Series.Map.Unit_5m_1 =  Site.Series.Map.Unit.1) %>%
  mutate(Site.Group_1m = NA,Site.Series.Map.Unit_1m_1 = NA,Transition_1m = "", Site.Group_1m_2=NA,Site.Series.Map.Unit_1m_2 = NA,Site.Group_5m_2=NA,Site.Series.Map.Unit_5m_2=NA)

data2.1 <- mutate(data2.1, Transition_5m = ifelse(Transition_5m == "null",0,Transition_5m))
data2.1 <- mutate(data2.1, Transition_5m = as.numeric(Transition_5m))

# fix the second part of data
head(data2.2)

# 1m primary 
SS1.1 = c('Site.Series.Map.Unit','Site.Series.Map.Unit.1','Site.Series.Map.Unit.2','Site.Series.Map.Unit.3','Site.Series.Map.Unit.4','Site.Series.Map.Unit.5',
          'Site.Series.Map.Unit.6','Site.Series.Map.Unit.7','Site.Series.Map.Unit.8','Site.Series.Map.Unit.9','Site.Series.Map.Unit.10','Site.Series.Map.Unit.11',
          'Site.Series.Map.Unit.12','Site.Series.Map.Unit.13','Site.Series.Map.Unit.14') 
# 1m secondary (transition)
SS1.2 = c('Site.Series.Map.Unit.15','Site.Series.Map.Unit.16','Site.Series.Map.Unit.17','Site.Series.Map.Unit.18','Site.Series.Map.Unit.19','Site.Series.Map.Unit.20',
          'Site.Series.Map.Unit.21','Site.Series.Map.Unit.22','Site.Series.Map.Unit.23','Site.Series.Map.Unit.24','Site.Series.Map.Unit.25','Site.Series.Map.Unit.26',
          'Site.Series.Map.Unit.27','Site.Series.Map.Unit.28','Site.Series.Map.Unit.29') 
# 5m primary 
SS5.1 = c('Site.Series.Map.Unit.30','Site.Series.Map.Unit.31','Site.Series.Map.Unit.32','Site.Series.Map.Unit.33','Site.Series.Map.Unit.34','Site.Series.Map.Unit.35',
          'Site.Series.Map.Unit.36','Site.Series.Map.Unit.37','Site.Series.Map.Unit.38','Site.Series.Map.Unit.39','Site.Series.Map.Unit.40','Site.Series.Map.Unit.41',
          'Site.Series.Map.Unit.42','Site.Series.Map.Unit.43','Site.Series.Map.Unit.44') 
# 5m secondary (transition)
SS5.2 = c('Site.Series.Map.Unit.45','Site.Series.Map.Unit.46','Site.Series.Map.Unit.47','Site.Series.Map.Unit.48','Site.Series.Map.Unit.49','Site.Series.Map.Unit.50',
          'Site.Series.Map.Unit.51','Site.Series.Map.Unit.52','Site.Series.Map.Unit.53','Site.Series.Map.Unit.54','Site.Series.Map.Unit.55','Site.Series.Map.Unit.56',
          'Site.Series.Map.Unit.57','Site.Series.Map.Unit.58','Site.Series.Map.Unit.59') 
data2.2 <- data2.2 %>% unite_('Site.Series.Map.Unit_5m_1',SS5.1 ,sep = ""); data2.2$Site.Series.Map.Unit_5m_1 = str_replace_all(data2.2$Site.Series.Map.Unit_5m_1,"NA","")
data2.2 <- data2.2 %>% unite_('Site.Series.Map.Unit_5m_2',SS5.2 ,sep = ""); data2.2$Site.Series.Map.Unit_5m_2 = str_replace_all(data2.2$Site.Series.Map.Unit_5m_2,"NA","")
data2.2 <- data2.2 %>% unite_('Site.Series.Map.Unit_1m_1',SS1.1,sep = "");data2.2$Site.Series.Map.Unit_1m_1 = str_replace_all(data2.2$Site.Series.Map.Unit_1m_1,"NA","")
data2.2 <- data2.2 %>% unite_('Site.Series.Map.Unit_1m_2',SS1.2 ,sep = ""); data2.2$Site.Series.Map.Unit_1m_2 = str_replace_all(data2.2$Site.Series.Map.Unit_1m_2,"NA","")

# Tidy up the data.names; 
data2.2<- data2.2 %>% 
  rename(BGC = Biogeoclimatic.Unit.1) %>% 
  rename(Site.Group_1m = Site.Group) %>% 
  rename(Transition_1m = Transition) %>% 
  rename(Site.Group_1m_2 = Site.Group.1) %>% 
  rename(Site.Group_5m = Site.Group.2) %>% 
  rename(Transition_5m = Transition.1) %>% 
  rename(Site.Group_5m_2 = Site.Group.3) %>% 
  select(-Biogeoclimatic.Unit)

data2.2 <- data2.2 %>%
  select(ObjectID,Crew, Date, General.Location,Latitude,Longitude,y,x,Accuracy..m.,Elevation..m.,Random.Point.ID,Certainty,
         BGC,Site.Group_5m,Site.Series.Map.Unit_5m_1,Transition_5m,Site.Group_5m_2,Site.Series.Map.Unit_5m_2,
         Site.Group_1m,Site.Series.Map.Unit_1m_1,Transition_1m,Site.Group_1m_2,Site.Series.Map.Unit_1m_2,
         Structural.Stage,Structural.Stage.Modifier,Site.Modifier,Soil.Moisture.Regime,Soil.Nutrient.Regime,Type.of.Disturbance, 
         Site.Disturbance..s.,Other.Site.Disturbance,Comment,GlobalID,CreationDate,Creator,Point.ID) 

data2.1 <- data2.1 %>%
  select(ObjectID,Crew, Date, General.Location,Latitude,Longitude,y,x,Accuracy..m.,Elevation..m.,Random.Point.ID,Certainty,
         BGC,Site.Group_5m,Site.Series.Map.Unit_5m_1,Transition_5m,Site.Group_5m_2,Site.Series.Map.Unit_5m_2,
         Site.Group_1m,Site.Series.Map.Unit_1m_1,Transition_1m,Site.Group_1m_2,Site.Series.Map.Unit_1m_2,
         Structural.Stage,Structural.Stage.Modifier,Site.Modifier,Soil.Moisture.Regime,Soil.Nutrient.Regime,Type.of.Disturbance, 
         Site.Disturbance..s.,Other.Site.Disturbance,Comment,GlobalID,CreationDate,Creator,Point.ID) 

dout = bind_rows(data2.1,data2.2)


################################################################################################################
## Summary table / graphs 
#write.csv(dout,"Deception20180611.csv")
write.csv(dout,paste(data.dir,"Deception_compile_V1_",Sys.Date(),".csv",sep = ""))

## Summary table / graphs 
ggplot(dout, aes(Longitude, Latitude, col = Site.Series.Map.Unit_5m_1)) + geom_point(size = 3)
ggsave(paste(graph.dir,"Deception_V1_SS_5m",Sys.Date(),".png",sep = ""), last_plot())

ggplot(dout, aes(Site.Series.Map.Unit_5m_1)) + geom_bar(stat = 'count') +  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave(paste(graph.dir,"Deception_V1_Freq_SS_5m",Sys.Date(),".png",sep = ""), last_plot())

# summary of site series data at 5m and 1m dta collection 
sum5 <- dout %>%
  group_by(Site.Series.Map.Unit_5m_1) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

sum1 <- dout %>%
  group_by(Site.Series.Map.Unit_1m_1) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))


#######################################################################################
# Start running stript here : September 23rd 
# this fomats the latest download and appends to the data prepped by Will 

# Deception V2: Data sort and summary

#######################################################################################

#data1 <- dout
 
# select the file you want to view
data2.2 <- read.csv(paste(data.dir,"Deception_18Sep_Raw.csv",sep = ""),stringsAsFactors=FALSE)
#data2.2 <- read.csv(paste(wd.save,dec.files[2],sep = ""),stringsAsFactors=FALSE)

#head(data2.2)

# 5m primary 
SS5.1 = c('Site.Series.Map.Unit','Site.Series.Map.Unit.1','Site.Series.Map.Unit.2','Site.Series.Map.Unit.3','Site.Series.Map.Unit.4','Site.Series.Map.Unit.5',
          'Site.Series.Map.Unit.6','Site.Series.Map.Unit.7','Site.Series.Map.Unit.8','Site.Series.Map.Unit.9','Site.Series.Map.Unit.10','Site.Series.Map.Unit.11',
          'Site.Series.Map.Unit.12','Site.Series.Map.Unit.13','Site.Series.Map.Unit.14','Site.Series.Map.Unit.15') 

# 5m secondary (transition)
SS5.2 = c('Site.Series.Map.Unit.16','Site.Series.Map.Unit.17','Site.Series.Map.Unit.18','Site.Series.Map.Unit.19','Site.Series.Map.Unit.20',
          'Site.Series.Map.Unit.21','Site.Series.Map.Unit.22','Site.Series.Map.Unit.23','Site.Series.Map.Unit.24','Site.Series.Map.Unit.25','Site.Series.Map.Unit.26',
          'Site.Series.Map.Unit.27','Site.Series.Map.Unit.28','Site.Series.Map.Unit.29','Site.Series.Map.Unit.30','Site.Series.Map.Unit.31') 

# 5m primary 
SS1.1 = c('Site.Series.Map.Unit.32','Site.Series.Map.Unit.33','Site.Series.Map.Unit.34','Site.Series.Map.Unit.35',
          'Site.Series.Map.Unit.36','Site.Series.Map.Unit.37','Site.Series.Map.Unit.38','Site.Series.Map.Unit.39','Site.Series.Map.Unit.40','Site.Series.Map.Unit.41',
          'Site.Series.Map.Unit.42','Site.Series.Map.Unit.43','Site.Series.Map.Unit.44','Site.Series.Map.Unit.45','Site.Series.Map.Unit.46','Site.Series.Map.Unit.47') 

# 5m secondary (transition)
SS1.2 = c('Site.Series.Map.Unit.48','Site.Series.Map.Unit.49','Site.Series.Map.Unit.50',
          'Site.Series.Map.Unit.51','Site.Series.Map.Unit.52','Site.Series.Map.Unit.53','Site.Series.Map.Unit.54','Site.Series.Map.Unit.55','Site.Series.Map.Unit.56',
          'Site.Series.Map.Unit.57','Site.Series.Map.Unit.58','Site.Series.Map.Unit.59','Site.Series.Map.Unit.60','Site.Series.Map.Unit.61','Site.Series.Map.Unit.62',
         'Site.Series.Map.Unit.63') 

data2.2 <- data2.2 %>% unite_('Site.Series.Map.Unit_5m_1',SS5.1 ,sep = ""); data2.2$Site.Series.Map.Unit_5m_1 = str_replace_all(data2.2$Site.Series.Map.Unit_5m_1,"NA","")
data2.2 <- data2.2 %>% unite_('Site.Series.Map.Unit_5m_2',SS5.2 ,sep = ""); data2.2$Site.Series.Map.Unit_5m_2 = str_replace_all(data2.2$Site.Series.Map.Unit_5m_2,"NA","")
data2.2 <- data2.2 %>% unite_('Site.Series.Map.Unit_1m_1',SS1.1,sep = "");data2.2$Site.Series.Map.Unit_1m_1 = str_replace_all(data2.2$Site.Series.Map.Unit_1m_1,"NA","")
data2.2 <- data2.2 %>% unite_('Site.Series.Map.Unit_1m_2',SS1.2 ,sep = ""); data2.2$Site.Series.Map.Unit_1m_2 = str_replace_all(data2.2$Site.Series.Map.Unit_1m_2,"NA","")

# Tidy up the data.names; 
data2.2<- data2.2 %>% 
  rename(BGC = Biogeoclimatic.Unit) %>% 
  rename(Site.Group_5m = Site.Group) %>% 
  rename(Transition_5m = Transition) %>% 
  rename(Site.Group_5m_2 = Site.Group.1) %>% 
  rename(Site.Group_1m = Site.Group.2) %>% 
  rename(Transition_1m = Transition.1) %>% 
  rename(Site.Group_1m_2 = Site.Group.3) %>%
  rename(Comment = Comment..max.255.characters.)

data2.2 <- data2.2 %>%
  select(ObjectID,Crew, Date, General.Location,Latitude,Longitude,y,x,Accuracy..m.,Elevation..m.,Random.Point.ID,Certainty,
         BGC,Site.Group_5m,Site.Series.Map.Unit_5m_1,Transition_5m,Site.Group_5m_2,Site.Series.Map.Unit_5m_2,
         Site.Group_1m,Site.Series.Map.Unit_1m_1,Transition_1m,Site.Group_1m_2,Site.Series.Map.Unit_1m_2,
         Structural.Stage,Structural.Stage.Modifier,Site.Modifier,Soil.Moisture.Regime,Soil.Nutrient.Regime, 
         Site.Disturbance..s.,Comment,GlobalID,CreationDate,Creator) 

dout = data2.2

## Summary table / graphs 
write.csv(data2.2,paste(data.dir,"Deception_compile_V2_",Sys.Date(),".csv",sep = ""))


###################################################################################################
# September 18th ## last download

# read in the compiled data vetted by Will ( )
vetted = read.csv(paste(data.dir,"AllDeception_Pts_Consolidated_to_July13.csv",sep = ""),stringsAsFactors = FALSE)

#newest.data = read.csv(paste(data.dir,"Deception_compile_V2_2018-09-18.csv",sep = ""),stringsAsFactors = FALSE)
newest.data = data2.2 %>% dplyr::filter(ObjectID > 1579) # remove the data already added to previous file
  
# format the new data to match the compiled dataset 

names(newest.data)
names(vetted)

###Columns changed by Will to check : 
#Experince for HPG? - talk to will about this. Yes =1 and yes to Non-GNSS?
#Non-GNSS point


newest.data <- newest.data %>% 
  mutate(Biogeoclimatic.Unit = BGC, Site.Series.Map.Unit_5m = Site.Series.Map.Unit_5m_1, Experience = 1, 
         Non.GNSS.point = 'N', Transition =  Transition_5m, Site.Series.Map.Unit_1m = Site.Series.Map.Unit_1m_1,
         Comment..max.255.characters = Comment, EditDate = NA,Editor = NA, Office.Comment=NA)  
        
new <- newest.data %>%
  select(Longitude,Latitude,ObjectID,GlobalID,
         Crew,Experience, Date, General.Location,
         Random.Point.ID,Non.GNSS.point,Accuracy..m.,Elevation..m.,
         Biogeoclimatic.Unit,Site.Group_5m,Site.Series.Map.Unit_5m,Certainty,
         Transition, Site.Group_5m_2,Site.Series.Map.Unit_5m_2,Site.Group_1m,
         Site.Series.Map.Unit_1m_1,Transition_1m,Site.Group_1m_2,Site.Series.Map.Unit_1m_2,
          Structural.Stage,Structural.Stage.Modifier,Site.Modifier,Soil.Moisture.Regime,
         Soil.Nutrient.Regime, Site.Disturbance..s.,Comment..max.255.characters,CreationDate,
         Creator,EditDate,Editor,Office.Comment) 

head(new)
out = bind_rows(vetted,new)

## Summary table / graphs 
write.csv(out,paste(data.dir,"AllDeception_Pts_Consolidated_to_",Sys.Date(),".csv",sep = ""))






##########################################################################################################


###old stuff ##





## Summary table / graphs 
ggplot(dout, aes(Longitude, Latitude, col = Site.Series.Map.Unit_5m_1)) + geom_point(size = 3)
ggsave(paste(out.dir,"Deception_V2_SS_5m",Sys.Date(),".png",sep = ""), last_plot())

ggplot(dout, aes(Site.Series.Map.Unit_5m_1)) + geom_bar(stat = 'count') +  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave(paste(out.dir,"Deception_V2_Freq_SS_5m",Sys.Date(),".png",sep = ""), last_plot())

#ggplot(dout, aes(Site.Series.Map.Unit_5m_1)) + geom_freqpoly( stat = 'count') 
#ggplot(dout, aes(Site.Series.Map.Unit_5m_1)) + geom_dotplot(stat = 'count') 

# summary of site series data at 5m and 1m dta collection 
sum5 <- dout %>%
  group_by(Site.Series.Map.Unit_5m_1) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

sum1 <- dout %>%
  group_by(Site.Series.Map.Unit_1m_1) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))
