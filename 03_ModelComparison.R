# script to compared model outputs from running random forest models

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
library(xlsx)
library(rJava)


# check the home directory  

setwd("D:/PEM_DATA/")#check the home directory  # set up work directory 

# read in the Parameters for each of the excel sheets
mparam <- read.xlsx("Model_params.xlsx", "Model_params", header=TRUE, colClasses=NA)

# set up folders 
field.data.folder = mparam[1,"field.data"]
in.folder = mparam[1,"in.folder"]
model.folder = mparam[1,"model.folder"]
layer.folder = mparam[1,"layer.folder"]
map.output.folder = mparam[m.no,"map.output.folder"]
m.summary.output.folder = "D:/PEM_DATA/Analysis/RandomForest/outputs/Model_compare/" ##Save all output summaries here

# compare per response varible: ie

### SiteSeries##
    prop.cor<- read.table(paste(model.folder,"/","Site.Series.Map.Unit_5m_prop.correct.csv",sep = ""),header = TRUE, sep = ",", stringsAsFactors = TRUE)
    #prop.cor<- prop.cor %>% dplyr::select(-c(X,X.1))
    prop.cor <- prop.cor %>% gather(prop.cor, "Model",2:length(prop.cor))
    prop.cor<- prop.cor %>% mutate(Model.no = str_remove(prop.cor,"_prop.correct"))
    
    # if you want to plot only some models
    mlist = c("M28","M29")
    prop.cor <- prop.cor %>% dplyr::filter(Model.no %in% mlist)
    prop.cor <- prop.cor %>% drop_na(Model)
    
    plot.all.ss <- prop.cor %>%
      ggplot(aes(x=Model.no, y=Model)) + 
      geom_boxplot() +
      geom_jitter(size=3, position=position_jitter(width=0.04, height=0.0001)) +
      xlab("Models") + 
      scale_y_continuous(limits = c(0,1))+ 
      ylab("Site Series (prop. correctly assigned)")+
      ggtitle("Proportion of correctly assigned site series per model")
    plot.all.ss
    ggsave(paste(m.summary.output.folder,"m.prop.correct.ss.jpeg",sep = ""),width = 20, height = 20, units = "cm")
    
    plot.by.ss <- prop.cor %>%
      ggplot(aes(x=Site.Series.Map.Unit_5m , y=Model,col = Model.no)) + 
      geom_point(size = 3)  + 
      #geom_line(aes(x=Site.Series.Map.Unit_5m , y=Model))+
      #geom_jitter(size=1, position=position_jitter(width=0.2, height=0.0001)) +
      xlab("Models") + 
      ylab("Site Series(prop. correctly assigned)")+
      ggtitle("Correctly assigned Site Series by model")+
      scale_y_continuous(limits = c(0,1))+ 
      theme(axis.text.x = element_text(angle = 90))
    plot.by.ss     
    ggsave(paste(m.summary.output.folder,"m.correct.ss.jpeg",sep = ""),width = 30, height = 20, units = "cm")


#### Site Physiology ####
      prop.cor<- read.table(paste(model.folder,"/","Site.Physiog_5m_prop.correct.csv",sep = ""),header = TRUE, row.names=NULL, sep = ",", stringsAsFactors = TRUE)
      prop.cor<- prop.cor %>% dplyr::select(-c(X))
      prop.cor <- prop.cor %>% gather(prop.cor, "Model",2:length(prop.cor))
      prop.cor<- prop.cor %>% mutate(Model.no = str_remove(prop.cor,"_prop.correct"))
      
      plot.all.sp <- prop.cor %>%
        ggplot( aes(x=Model.no, y=Model)) + 
        geom_point () +
       # geom_jitter(size=1, position=position_jitter(width=0.2, height=0.0001)) +
        xlab("Models") + 
        ylab("Site physiology (prop. correctly assigned)") + 
        ggtitle("Proportion of correctly assigned Site physiology (Forest/Non Forest) per model")+
        scale_y_continuous(limits = c(0,1))
      plot.all.sp
      ggsave(paste(m.summary.output.folder,"m.prop.correct.sp.jpeg",sep = ""),width = 20, height = 20, units = "cm")

    plot.by.sp <- prop.cor %>%
      ggplot(aes(x=Site.Physiog_5m , y=Model,col = Model.no)) + 
      geom_point()  + 
      #geom_jitter(size=1, position=position_jitter(width=0.1, height=0.0001)) +
      xlab("Models") + 
      ylab("Site Physiology (prop. correctly assigned)")+
      theme(axis.text.x = element_text(angle = 90))+ 
      ggtitle("Correctly assigned Site physiology (Forest/Non Forest) per model") +
      scale_y_continuous(limits = c(0,1))
    plot.by.sp    
    ggsave(paste(m.summary.output.folder,"m.correct.sp.pdf",sep = ""),width = 20, height = 20, units = "cm")

    
#### Site Class ####
    prop.cor<- read.table(paste(model.folder,"/","Site.Class_5m_prop.correct.csv",sep = ""),header = TRUE, row.names=NULL, sep = ",", stringsAsFactors = TRUE)
    prop.cor<- prop.cor %>% dplyr::select(-c(X))
    prop.cor <- prop.cor %>% gather(prop.cor, "Model",2:length(prop.cor))
    prop.cor<- prop.cor %>% mutate(Model.no = str_remove(prop.cor,"_prop.correct"))
    
    # if you want to plot only some models
    mlist = c("M29","M31","M32")
    prop.cor <- prop.cor %>% dplyr::filter(Model.no %in% mlist)
    prop.cor <- prop.cor %>% drop_na(Model)
    
    
    
    plot.all.sp <- prop.cor %>%
      ggplot( aes(x=Model.no, y=Model)) + 
      geom_boxplot () +
       geom_jitter(size=3, position=position_jitter(width=0.02, height=0.0001)) +
      xlab("Models") + 
      ylab("Site Class (prop. correctly assigned)") + 
      ggtitle("Proportion of correctly assigned Site Class per model")+
      scale_y_continuous(limits = c(0,1))
    plot.all.sp
    ggsave(paste(m.summary.output.folder,"m.prop.correct.sc.jpeg",sep = ""),width = 20, height = 20, units = "cm")
    
    plot.by.sp <- prop.cor %>%
      ggplot(aes(x=Site.Class_5m , y=Model,col = Model.no)) + 
     # geom_point(size = 3)  + 
      geom_jitter(size=3, position=position_jitter(width=0.15, height=0.03)) +
      xlab("Models") + 
      ylab("Site Class (prop. correctly assigned)")+
      theme(axis.text.x = element_text(angle = 90))+ 
      ggtitle("Correctly assigned Site Class per model") +
      scale_y_continuous(limits = c(-0.05,1))
    plot.by.sp    
    ggsave(paste(m.summary.output.folder,"m.correct.sc.jpeg",sep = ""),width = 20, height = 20, units = "cm")
    

#### Site Realm ####
    prop.cor<- read.table(paste(model.folder,"/","Site.Realm_5m_prop.correct.csv",sep = ""),header = TRUE, row.names=NULL, sep = ",", stringsAsFactors = TRUE)
    prop.cor<- prop.cor %>% dplyr::select(-c(X))
    prop.cor <- prop.cor %>% gather(prop.cor, "Model",2:length(prop.cor))
    prop.cor<- prop.cor %>% mutate(Model.no = str_remove(prop.cor,"_prop.correct"))
    
    plot.all.sp <- prop.cor %>%
      ggplot( aes(x=Model.no, y=Model)) + 
      geom_point () +
      #geom_jitter(size=1, position=position_jitter(width=0.2, height=0.0001)) +
      xlab("Models") + 
      ylab("Site Realm (prop. correctly assigned)") + 
      ggtitle("Proportion of correctly assigned Site Realm (Terrestrial/Wetland) per model")+
      scale_y_continuous(limits = c(0,1))
    plot.all.sp
    ggsave(paste(m.summary.output.folder,"m.prop.correct.sr.pdf",sep = ""),width = 20, height = 20, units = "cm")
    
    plot.by.sp <- prop.cor %>%
      ggplot(aes(x=Site.Realm_5m , y=Model,col = Model.no)) + 
      geom_point()  + 
      #geom_jitter(size=1, position=position_jitter(width=0.1, height=0.0001)) +
      xlab("Models") + 
      ylab("Site Realm (prop. correctly assigned)")+
      theme(axis.text.x = element_text(angle = 90))+ 
      ggtitle("Correctly assigned Site Realm (Terrestrial/Wetland) per model") +
      scale_y_continuous(limits = c(0,1))
    plot.by.sp    
    ggsave(paste(m.summary.output.folder,"m.correct.sr.pdf",sep = ""),width = 20, height = 20, units = "cm")
    

#### Site Group ####
    prop.cor<- read.table(paste(model.folder,"/","Site.Group_5m_prop.correct.csv",sep = ""),header = TRUE, row.names=NULL, sep = ",", stringsAsFactors = TRUE)
    prop.cor<- prop.cor %>% dplyr::select(-c(X))
    prop.cor <- prop.cor %>% gather(prop.cor, "Model",2:length(prop.cor))
    prop.cor<- prop.cor %>% mutate(Model.no = str_remove(prop.cor,"_prop.correct"))
    
    plot.all.ss <- prop.cor %>%
      ggplot(aes(x=Model.no, y=Model)) + 
      geom_boxplot() +
      geom_jitter(size=1, position=position_jitter(width=0.1, height=0.0001)) +
      xlab("Models") + 
      scale_y_continuous(limits = c(0,1))+ 
      ylab("Site Group (prop. correctly assigned)")+
    ggtitle("Proportion of correctly assigned site group per model")
    plot.all.ss
     ggsave(paste(m.summary.output.folder,"m.prop.correct.sg.pdf",sep = ""),width = 20, height = 20, units = "cm")
    
    plot.by.sp <- prop.cor %>%
      ggplot(aes(x=Site.Group_5m , y=Model,col = Model.no)) + 
      geom_point()  + 
      geom_jitter(size=1, position=position_jitter(width=0.1, height=0.0001)) +
      xlab("Models") + 
      ylab("Site Group (prop. correctly assigned)")+
      theme(axis.text.x = element_text(angle = 90))+ 
      ggtitle("Correctly assigned Site physiology (Forest/Non Forest) per model") +
      scale_y_continuous(limits = c(0,1))
    plot.by.sp    
    ggsave(paste(m.summary.output.folder,"m.correct.sg.pdf",sep = ""),width = 20, height = 20, units = "cm")
    
    




#################################################################################################

# Comparison between Modesl 

################################################################################################

# compare numbers: 
# output the model output by adding a row to a table with all imputs
sum.data <- read.csv(paste(output.folder,"/","MODEL_OUTPUTS.csv",sep = ""),header = TRUE,stringsAsFactors = FALSE)


# still to add to this part 




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

# check the home directory  
getwd()

# Step 1: set up location of drives to input and output
field.data.folder = ("../../Data/Field_data")         # location of full data set 
in.folder = ("../../Analysis/RandomForest/inputs")    # location of attributes 
output.folder = ("../../Analysis/RandomForest/outputs") # location of model outputs 
layer.folder = ("../../Data/Layers")



#################################################################################################

# Comparison between Modesl 

################################################################################################

# compare numbers: 
# output the model output by adding a row to a table with all imputs
sum.data <- read.csv(paste(output.folder,"/","MODEL_OUTPUTS.csv",sep = ""),header = TRUE,stringsAsFactors = FALSE)


# still to add to this part 




########################################
# MAP COMPARISON 
########################################
# get a list of all model img outputs 
outfiles <- list.files(output.folder) 
imgs <- outfiles[grep("_map.img",outfiles)]
imgs <- imgs[-c(grep("_map.img.aux",imgs))]
imgs

# select the two maps to compare by entering the data 
map1 = imgs[1]
map2 = imgs[2]

# read in the img predicted surfaces
mapgrid.1 <- raster(paste(output.folder,map1 ,sep="/"))
mapgrid.2 <- raster(paste(output.folder,map2 ,sep="/"))

# build the matching key 
map.key.1 <- read.csv(paste(output.folder,gsub("_map.img","_map_key.csv",map1),sep = "/"))
map.key.2 <- read.csv(paste(output.folder,gsub("_map.img","_map_key.csv",map2),sep = "/"))
map.key.all <- data.frame(full_join(map.key.1,map.key.2,by= "category"))
key = map.key.all %>%
  mutate(id = row.y,v =integercode.x) %>%
  select(c(id,v)) 


### Build a map showing the differences in predictions; 
# check that the keys are matching (i.e the site series number (within the rasters match the other raster) )

if(identical(map.key.all$integercode.x,map.key.all$integercode.y) == TRUE){ 
        dif <- mapgrid.1 - mapgrid.2 ; plot(dif)
        mapgrid.2a = mapgrid.2
                        }else { 
          #If the keys dont match and you get the message above : reclassify the raster match the other coding by converting the numbers to the matching SS codes of map 1. 
        mapgrid.2a = subs(mapgrid.2,key,subsWithNA=FALSE)
        # once they are matching subset map 1 from matching map1
        dif <- mapgrid.1 - mapgrid.2a ; plot(dif) 
        print("original rasters dont match, second raster adjusted to match")}


## Map all three maps to compare
# Plot 1: Full colour plot of the different values

# convert the colour pallett for the dif plot




#l <- seq(100,0,length.out=101)
l <- seq(100,25,length.out=101)
c <- seq(0,100,length.out=101)
#c <- seq(0,75,length.out=101)
col.ramp <- hcl(h = 140, c = c, l = l)

opar <- par(mfrow=c(1,3),mar=c(3,3,2,1),oma=c(0,0,3,4),xpd=NA)
zlim <- c(0,max(maxValue(mapgrid.1)))
#legend.label<-rev(pretty(zlim,n=23))
#legend.colors<-col.ramp[trunc((legend.label/max(legend.label))*100)+1]

image(mapgrid.1,
      col=col.ramp,
      xlab="",ylab="",xaxt="n",yaxt="n",
      zlim=zlim,
      asp=1,bty="n",main="")
mtext(map1 ,side=3,line=1,cex=1.2)


image(mapgrid.2a,
      col=col.ramp,
      xlab="",ylab="",xaxt="n",yaxt="n",
      zlim=zlim,
      asp=1,bty="n",main="")
mtext(map2 ,side=3,line=1,cex=1.2)

image(dif,
      col=col.ramp,
      xlab="",ylab="",xaxt="n",yaxt="n",
      zlim=zlim,
      asp=1,bty="n",main="")
mtext(Difference,side=3,line=1,cex=1.2)


# or try the plot function 

roi <- raster::stack(mapgrid.1,mapgrid.2a,dif)

library(rasterVis)
library(RColorBrewer)



#l <- seq(100,0,length.out=101)
l <- seq(100,25,length.out=101)
c <- seq(0,100,length.out=101)
#c <- seq(0,75,length.out=101)
col.ramp <- hcl(h = 140, c = c, l = l)

#opar <- par(mfrow=c(1,3),mar=c(3,3,2,1),oma=c(0,0,3,4),xpd=NA)

levelplot(mapgrid.1, 
          margin=FALSE,
          par.settings=list(
            axis.line=list(col='transparent')),
            # suppress axes and legend outline
          scales=list(draw=FALSE),            # suppress axis labels
          col.regions=col.ramp,                   # colour ramp
          at=seq(0, 79, len=101))            # colour ramp breaks
 

plot(mapgrid.1)
plot(mapgrid.2a)
plot(dif)

library(viridis)
levelplot(roi, 
          margin=FALSE,                       
          colorkey=list(
            space='bottom',                   
            labels=list(at=-5:5, font=4),
            axis.line=list(col='black'),
            width=0.75
          ),    
          par.settings=list(
            strip.border=list(col='transparent'),
            strip.background=list(col='transparent'),
            axis.line=list(col='transparent')
          ),
          scales=list(draw=FALSE),            
          col.regions=viridis,                   
          at=seq(-5, 5, len=101))
          #names.attr=rep('', nlayers(s)))     




















#############################################################
# Plot 1: 
l <- seq(100,0,length.out=101)
c <- seq(0,100,length.out=101)
col.ramp <- hcl(h = 120, c = c, l = l)

opar <- par(mfrow=c(1,2),mar=c(3,3,2,1),oma=c(0,0,3,4),xpd=NA)
mapgrid.a <- raster(paste(output.folder,"/",MODELfn,"_map.img",sep=""))
#mapgrid.b <- raster(paste(output.folder,"/",MODELfn.b,"_map.img",sep=""))
zlim <- c(0,max(maxValue(mapgrid.a)))
legend.label<-rev(pretty(zlim,n=23))
legend.colors<-col.ramp[trunc((legend.label/max(legend.label))*100)+1]

label.doc = read.csv(paste(output.folder,"/",MODELfn,"_map_key.csv",sep = ""))
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














# Plot 2: Binary plot of the different values (same prediction = white, different ss preiction = red )
difyn <- reclassify(dif, c(1, 79, 1))
plot(difyn)

# Plot 1: 
l <- seq(100,0,length.out=101)
c <- seq(0,100,length.out=101)
col.ramp <- hcl(h = 120, c = c, l = 79)
#col.ramp <-csphd(hcl(h = seq(120, 0, length = 4) + 150))
#col.ramp <- rainbow(79)

par(mfrow=c(1,3))
image(mapgrid.1,
      col=col.ramp,
      xlab="",ylab="",xaxt="n",yaxt="n",
      zlim=zlim,#,
      asp=1,bty="n",main=""
)
mtext(map1,side=3,line=1,cex=1.2)

image(mapgrid.2a,
      col=col.ramp,
      xlab="",ylab="",xaxt="n",yaxt="n",
      zlim=zlim,
      asp=1,bty="n",main="")
mtext(map2,side=3,line=1,cex=1.2)

image(difyn,
      col=col.ramp,
      xlab="",ylab="",xaxt="n",yaxt="n",
      zlim=zlim,
      asp=1,bty="n",main="")
