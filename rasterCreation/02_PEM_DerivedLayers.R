#### Integration of SAGA and R ###########################################
#### PEM Derived Layer Generation
#### Following:
####     - QGIS Process modeling developed by Lukas Jaron and C. Chisholm
####     - Hengl and MAcMillan (used examples of connecting to SAGA)
####         https://envirometrix.github.io/PredictiveSoilMapping/software.html
####
#### R script by Colin Chisholm
#### Initiated:  March 10, 2019

#### Tested to work with SAGA 7.2 
#### https://sourceforge.net/projects/saga-gis/files/SAGA%20-%207/SAGA%20-%207.2.0/saga-7.2.0_x64.zip/download

#### OVERVIEW / outline ###########
# A. Install and Load Libraried
# B. Set up working Environment
#    - Link to SAGA install (machine specific)
# C. Load Functions: 
#    - PrepSAGA() creates a SAGA version of the DTM ensuring Projection Info is assigned.  Option to transform projection is commented out
#    - pemDerivedLayers() requires the DTM, and makes system calls to SAGA to generate all dtm derived layers
#    - convertASC()  converts all the SAGA files to .asc (ESRI raster).  Also ignores files used for processing (e.g. Channel Network, Basins)
#    - convertTIF()  option to convert a set of rasters to geoTiff 
#    - cleanUp()  removes temp files (SAGA .sdats) 
# D. >> RUN SCRIPT HERE << -- Set Variables and call the functions 

## Future refinement:
# 1. build loop to call function for all DTM resolutions


# #### R Libraries #########################################################
    # # Install Libraries -- to potentially be used (not loaded but installed) -
    # # H&M's list -- this does not load all the packages ... just installs them
    # #               if needed
    # ls <- c("reshape", "Hmisc", "rgdal", "raster", "sf", "GSIF", "plotKML",
    #         "nnet", "plyr", "ROCR", "randomForest", "quantregForest",
    #         "psych", "mda", "h2o", "h2oEnsemble", "dismo", "grDevices",
    #         "snowfall", "hexbin", "lattice", "ranger",
    #         "soiltexture", "aqp", "colorspace", "Cubist",
    #         "randomForestSRC", "ggRandomForests", "scales",
    #         "xgboost", "parallel", "doParallel", "caret",
    #         "gam", "glmnet", "matrixStats", "SuperLearner",
    #         "quantregForest", "intamap", "fasterize", "viridis")
    #
    # new.packages <- ls[!(ls %in% installed.packages()[,"Package"])]
    # if(length(new.packages)) install.packages(new.packages)

# # Repeating the pattern for packages CC uses --------------------------
    ls <- c("dplyr","ggplot2","tidyr","stringr",
            "readxl", "foreign", "lsr", "car",  "moments", "psych",
            "latex2exp","gtools", "knitr", "rgeos", "maptools", "raster", "sp",
            "rgdal", "mapview", "sf", "RSAGA")
    new.packages <- ls[!(ls %in% installed.packages()[,"Package"])]
    if(length(new.packages)) install.packages(new.packages)
    rm(ls, new.packages)

# Load Needed Libraried -----------------------------------------------
    x <- c("dplyr","ggplot2","tidyr","stringr")             # Data table manipulation and graphing
    # x <- append(x, c("readxl", "foreign"))                  # Import of alternated data types (e.g. xlsx or dbf)
    # x <- append(x, c("lsr", "car",  "moments", "psych"))    # Statisical packages (psych give me describe)
    # x <- append(x, c("latex2exp","gtools", "knitr"))        # Options for formatting and rmd latex formating
    x <- append(x, c("sp", "rgdal", "raster"))              # Essential GIS libraries
    # x <- append(x, c("rgeos", "mapview", "sf" ))            # Mor GIS libraries and viewing
    # x <- append(x, c("maptools", "RSAGA"))                  # more GIS / Raster tools
    lapply(x, library, character.only = TRUE)  # load the required packages
    rm(x)




##### Link SAGA to R --------------------------------------------------
  if(Sys.info()['sysname']=="Windows"){saga_cmd = "C:\\SAGA\\saga_cmd.exe"
  } else {saga_cmd = "saga_cmd"}  ;   system(paste(saga_cmd, "-v"))



##### FUNCTIONS ######################################################################

PrepSAGA <- function(){
  ##### Map Projection ----------------------------------------------
  # SEE WARNING 
  # crs(DTM) # Confirm if it is correct 
  
  # PROJ <- crs(DTM) # Use if the layer has a DTM 
  
  # Assign Projection if not declared 
  # WARNING ONLY Assign if known to be correct 
  # PROJ <- crs(paste("+init=epsg:",3005, sep = ""))  #26910 is the EPSG codefor  NAD83 UTM 10U
  #                               #  3005 for BC Albers
  crs(DTM) <- PROJ 
  crs(DTM)
  
  
  
  # Transform it needed -----------------
  # WARNING This will warp the data ... ideally DTM is generated from the transformed LAS
  rTransform <- function(){
    DTM <- raster(paste(DTMpath, "\\", DTMname, sep = "")) # raster package is problematic for SAGA format
    DTM <- projectRaster(DTM, crs = PROJ)
    writeRaster(DTM, paste(DTMpath, "\\", "tmp.tif", sep= ""), overwrite = TRUE)  #
    DTM <- readGDAL(paste(DTMpath, "\\", "tmp.tif", sep = "")) # re-open as with GDAL to facilitateSAGA export 
    file.remove(paste(DTMpath, "\\", "tmp.tif", sep = ""))
  }
  # rTransform() # Only if necessary 


# OUTPUTS: ------------------------------------------------------------
  # tmpOut <- paste(DTMpath, "\\", "sagaTmp", sep = "")
  ifelse(!dir.exists(file.path(tmpOut)),               #if tmpOut Does not Exists
          dir.create(file.path(tmpOut)), FALSE)        #create tmpOut

  setwd(tmpOut)

##### >> Stage 00 -- Convert DTM to SAGA format ##############################
  # sDTM <- "dtm.sdat"
  # sDTM <- paste(tmpOut, "\\", sDTM, sep = "")
  # writeRaster(DTM, sDTM, driver = "SAGA", overwrite = TRUE)  # save SAGA Version
  writeGDAL(DTM, sDTM, driver = "SAGA")  # save SAGA Version using rgdal
  rm(DTM) # not needed
}







#-------------------------------------------------------------------------
##### PEM TERRAIN LAYERS -- from SAGA ####################################
#with a DTM build, above, generate all the derived Layers
pemDerivedLayers <- function(sDTM){
      ##### >> 11 -- saga:channelnetwork ------------------------------------
        channelsNetwork <- "Channel_network_grid.sgrd"
        sysCMD <- paste(saga_cmd, "ta_channels 0", "-ELEVATION", sDTM,    #input DTM
                        "-CHNLNTWRK", channelsNetwork,                    #output
                        "-INIT_GRID", sDTM,                               #Initiation Grid (can be dtm)
                        "-INIT_VALUE", 0, "-INIT_METHOD", 2,              #default params
                        "-DIV_CELLS", 10.0, "-MINLEN", 10.0)
        system(sysCMD)

      ##### >> 12 -- Fill Sinks (Wang and Liu)  -----------------------------
      # Note: consider Using: Fill Sinks XXL (Wang and Liu)
      # http://www.saga-gis.org/saga_tool_doc/6.2.0/ta_preprocessor_4.html
        sinksFilled <- "Filled_sinks.sgrd"
        basins <-      "Basins.sgrd"
        sysCMD <- paste(saga_cmd, "ta_preprocessor 4", "-ELEV" , sDTM,    #input
                        "-FILLED", sinksFilled,                           #output
                        "-MINSLOPE ", 0.01,                               #params
                        "-WSHED",  basins)                                #additional output
        system(sysCMD)

      ##### >> 13 Catchment Area --------------------------------------------
      # http://www.saga-gis.org/saga_tool_doc/6.2.0/ta_hydrology_0.html
        tCatchment <- "tCatchment.sgrd"
        sysCMD <- paste(saga_cmd, "ta_hydrology 0", "-ELEVATION", sinksFilled,    # input from 21
                        "-FLOW", tCatchment,                                      # outPut
                        "-METHOD", 4
        )
        system(sysCMD)

      ##### >> 14 --  Flow Width and Specific Catchment Area ----------------
      # http://www.saga-gis.org/saga_tool_doc/6.2.0/ta_hydrology_19.html
        sCatchment <- "Specific_Catchment.sgrd"
        sysCMD <- paste(saga_cmd, "ta_hydrology 19", "-DEM", sinksFilled,    # input from 21
                        "-SCA", sCatchment,                                  # outPut
                        "-TCA", tCatchment,                                  # total catchment area 31
                        "-METHOD", 1
                        )
        system(sysCMD)

      ##### >> 15 MRVBF -----------------------------------------------------
      # http://www.saga-gis.org/saga_tool_doc/6.2.0/ta_morphometry_8.html
        MRVBF <- "MRVBF.sgrd"
        MRRTF <- "MRRTF.sgrd"
        sysCMD <- paste(saga_cmd, "ta_morphometry 8", "-DEM", sDTM,          # input raw dtm
                        "-MRVBF", MRVBF, "-MRRTF", MRRTF,                    # outputs
                        "-T_SLOPE", 16, "-T_PCTL_V", 0.4, "-T_PCTL_R", 0.35, # Default params
                        "-P_SLOPE", 4.0, "-P_PCTL", 3.0, "-UPDATE", 1,
                        "-CLASSIFY", 0,"-MAX_RES", 100
        )
        system(sysCMD)


      ##### >> 16 Overland Flow Distance to Channel Network -----------------
      # http://www.saga-gis.org/saga_tool_doc/6.2.0/ta_channels_4.html
        hDistance <- "OverlandFlowDistance.sgrd"
        vDistance  <- "VerticalDistance.sgrd"
        sysCMD <- paste(saga_cmd, "ta_channels 4", "-ELEVATION ", sDTM, "-CHANNELS", channelsNetwork,
                        "-DISTANCE", hDistance, "-DISTVERT", vDistance,      # inputs above, outputs here
                        "-METHOD", 1
                        )
        system(sysCMD)


      ##### >> 17 Terrain Ruggedness Index ----------------------------------
      # http://www.saga-gis.org/saga_tool_doc/6.2.0/ta_morphometry_16.html
        TRI <- "TRI.sgrd"
        sysCMD <- paste(saga_cmd, "ta_morphometry 16", "-DEM", sDTM,
                        "-TRI", TRI,
                        "-MODE", 1, "-RADIUS", 3.0, "-DW_WEIGHTING", 1,
                        "-DW_IDW_POWER", 1, "-DW_IDW_OFFSET", 0,
                        "-DW_BANDWIDTH", 1
                        )
        system(sysCMD)

      ##### >> 18 Convergence Index -----------------------------------------
      # http://www.saga-gis.org/saga_tool_doc/6.2.0/ta_morphometry_1.html
        convergence <- "Convergence.sgrd"
        sysCMD <- paste(saga_cmd, "ta_morphometry 1", "-ELEVATION ", sDTM,
                        "-RESULT", convergence,
                        "-METHOD", 0, "-NEIGHBOURS", 1)
        system(sysCMD)

      ##### >> 19 Openness --------------------------------------------------
      # http://www.saga-gis.org/saga_tool_doc/6.2.0/ta_lighting_5.html
        POS <- "OpennessPositive.sgrd"
        NEG <- "OpennessNegative.sgrd"
        sysCMD <- paste(saga_cmd, "ta_lighting 5", "-DEM", sDTM,
                        "-POS", POS, "-NEG", NEG,
                        "-RADIUS", 1000, "-METHOD", 1,
                        "-DLEVEL",  3, "-NDIRS", 8)
        system(sysCMD)

      ##### >> 20 Diuranal Anisotropic Heating ------------------------------
      # http://www.saga-gis.org/saga_tool_doc/6.2.0/ta_morphometry_12.html
        dAH <- "dAH.sgrd"
        sysCMD <- paste(saga_cmd, "ta_morphometry 12", "-DEM", sDTM,
                        "-DAH", dAH,
                        "-ALPHA_MAX", 202.5)
        system(sysCMD)


      ##### >> 21 Slope Aspect and Curvature --------------------------------
      # http://www.saga-gis.org/saga_tool_doc/6.2.0/ta_morphometry_0.html
        Slope <- "Slope.sgrd"
        Aspect <- "Aspect.sgrd"
        Curvature <- "gCurvature.sgrd"
        tCurve <- "tCurve.sgrd"
        sysCMD <- paste(saga_cmd, "ta_morphometry 0", "-ELEVATION", sDTM,
                        "-SLOPE", Slope, "-ASPECT", Aspect,
                        "-C_GENE", Curvature, "-C_TOTA", tCurve,
                        "-METHOD", 6, "-UNIT_SLOPE", 0, "-UNIT_ASPECT", 0)
        system(sysCMD)

      ##### >> 22 Topogrphic Position Index ---------------------------------
      # http://www.saga-gis.org/saga_tool_doc/6.2.0/ta_morphometry_18.html
        TPI <- "TPI.sgrd"
        sysCMD <- paste(saga_cmd, "ta_morphometry 18", "-DEM", sDTM,
                        "-TPI", TPI,
                        "-STANDARD", 0, "-RADIUS_MIN", 0, "-RADIUS_MAX", 100,
                        "-DW_WEIGHTING", 0, "-DW_IDW_POWER", 1, "-DW_IDW_OFFSET", 1,
                        "-DW_BANDWIDTH", 75)
        system(sysCMD)

      ##### >> 23 Topographic Wetness Index ---------------------------------
      # http://www.saga-gis.org/saga_tool_doc/6.2.0/ta_hydrology_20.html
        TWI <- "TWI.sgrd"
        sysCMD <- paste(saga_cmd, "ta_hydrology 20", "-SLOPE", Slope,
                        "-AREA", sCatchment,
                        "-TWI", TWI,
                        "-CONV",0,  "-METHOD", 0)
        system(sysCMD)
      }

##### Save Processed Grids #########################################################
convertASC <- function(){
    rasterfiles <- list.files(pattern = ".sdat")
    rasterfiles <- grep(rasterfiles, pattern = "aux.xml", inv = T, value = T) #inv inverts (not), value returns the names
    # rasterfiles
    rasterfiles <- rasterfiles[-c(2, 3, 6, 15, 16)] # Remove intermediate rasters and dtm from the list....
      # Basins, Channel_network_grid, dtm (don't need to recreate), Specific_Catchment, TotalCatchment
                                               # HR also included the filled_sinks.dtm ... in my mind this is a
    outFiles <- gsub("sdat", "asc", rasterfiles )
    outFiles

  # Use gdal to convert these to asc ----------------------------------
    for(i in 1:length(rasterfiles)){
      r <- readGDAL(rasterfiles[i])
      w <- paste("..\\", outFiles[i], sep = "")
      # OLD Version lost projection info write.asciigrid(r, w, attr = 1, na.value = -9999)#, proj4string = CRS(r)) #sp package
      writeGDAL(r, w)
      
      # writeRaster(r, w, format = "ascii")

    }
}

# convert .sdat to .tif format ----------------------------------------
# Requires the input directory 
convertTIF <- function(InputDir, outDir, iRasterType){ # specify directories and raster inputType
  # Testing
  # InputDir <- "E:\\tmpGIS\\pemR\\25m\\"
  # outDir   <- "E:\\tmpGIS\\pemR\\25m\\tif"
  # iRasterType <- "asc"
  
  
  ifelse(!dir.exists(file.path(outDir)),               #if tmpOut Does not Exists
          dir.create(file.path(outDir)), FALSE)        #create tmpOut
  
  setwd(InputDir)
  rasterfiles <- list.files(pattern = iRasterType)
  rasterfiles <- grep(rasterfiles, pattern = "aux.xml", inv = T, value = T) #inv inverts (not), value returns the names
  print(rasterfiles)
  # rasterfiles <- rasterfiles[-c(2, 3, 6)] # Remove intermediate rasters and dtm from the list....
  # HR also included the filled_sinks.dtm ... in my mind this is a
  outFiles <- gsub(iRasterType, "tif", rasterfiles )
  print(outFiles)
  
  # Use gdal to convert these to tif ----------------------------------
  for(i in 1:length(rasterfiles)){
    # i <- 1 # Testing
    r <- readGDAL(rasterfiles[i])
    w <- paste(outDir,"\\", outFiles[i], sep = "")
    writeGDAL(r, w)
  }
}


# Clean up --------------------------- WARNING deletes intermediate files ------------------
cleanUp <- function(){
    setwd(DTMpath)   # change dir (one level up)
    #rm(sDTM, tmpOut) # remove unneeded variables 
    unlink("sagaTmp", recursive = TRUE)       # unlink directory 
    #file.remove("sagaTmp", recursive = TRUE ) # remove tmp dir and files.
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## RUN SCRIPT HERE --  EXECUTE Script #####################################

##### Set Up Environmental Variables ----------------------------------
# INPUTS: Load DTM Raster ------------
# Change these to fit needs 
  DTMpath <- "E:\\tmpGIS\\pemR\\25m"  # where is the dtm located
  DTMname     <- "dtm_25m.tif"        # name of the DTM 
  
  DTM <- readGDAL(paste(DTMpath, "\\", DTMname, sep = "")) # read with GDAL, and Write with GDAL works well.

  PROJ <- crs(paste("+init=epsg:", 26910, sep = ""))  #26910 is the EPSG codefor  NAD83 UTM 10U
  #                               #  3005 for BC Albers
  
  
# Temporary File Settings -- used to create a tmp folder in the same directory as the original DTM 
# Should not need to be changed -- these need to be global vaiables as they are used by multiple functions.
  tmpOut <- paste(DTMpath, "\\", "sagaTmp", sep = "")
  sDTM <- "dtm.sdat"
  sDTM <- paste(tmpOut, "\\", sDTM, sep = "")


##### Call the functions ----------------------------------------------
# Prepare for SAGA execution 
  PrepSAGA()
# Run all layers at once
  pemDerivedLayers(sDTM)
# Convert to .asc
  convertASC()
# Clean up -- remove tmp files 
  cleanUp()
    
## Utility -- 
# Alternate convert to .tif 
  # specify InputDir, OutputDir, InputFileFormat (e.g. "asc", "sdat")
  # convertTIF("E:\\tmpGIS\\pemR\\25m\\sagaTmp", "E:\\tmpGIS\\pemR\\25m\\tif", "sdat")
  # cleanUp()
  
  
  