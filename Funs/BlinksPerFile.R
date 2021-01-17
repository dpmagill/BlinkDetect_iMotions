BlinksPerFile <- 
    function(PathData, PathFunctions, NDays, ThresholdAffdex, ThresholdET_Min, 
        ThresholdET_Max, InterBlinkBufferAffdex, InterBlinkBufferET, 
        NormalizeAffdexTF, MergeBlinksNonZeroSpanAffdexTF, 
        MergeBlinksNonZeroThresholdAffdex) {
################################################################################
#Function BlinksPerFile 
#        
#    Douglas Magill
#    12/17/2020
#
#    Description:
#        
#    Designed to be called by script CustomBlinkCode.R.
#
#    Return a list of that contains the blink rate (blinks per minute) and other
#    variables for each participant for each study day. This file sends each 
#    file, which corresponds to a participant and a day, to function GetBlinks,
#    which determines blinks and returns blink information.
#
################################################################################
   
      
##### Load additional functions #####
      
#Load subfunctions of BlinksPerFile
source(paste0(PathFunctions, "SetupDataOutput.R"))
source(paste0(PathFunctions, "TimestampsMS.R"))
source(paste0(PathFunctions, "GetBlinks.R"))
source(paste0(PathFunctions, "GetBlinksAffdex.R"))
source(paste0(PathFunctions, "GetBlinksAffdex_Interblink.R"))
source(paste0(PathFunctions, "GetBlinksAffdex_OneBlinkPerSpan.R"))
source(paste0(PathFunctions, "GetBlinksET.R"))
      
            
##### Identify data files exported from iMotions ######

#Initialize list of file names
#This list will contain a sublist of file names corresponding to each day.
FileNames <- list()

#Determine the number of participants per day
#Preallocate vector to indicate the number of participants per day.
NIDByDay <- 
    matrix(NA_integer_,
           nrow = NDays,
           ncol = 1)

for (i in 1 : NDays) {

    #Folder name by day 
    #I.e., "Day1".
    day <- paste0("Day", as.character(i))  
    
    #Files in folder of corresponding day  
    FileNames_ith <- list.files(paste0(PathData, "/", day))
  
    NIDByDay[i] <- length(FileNames_ith)
    
    FileNames[[i]] <- FileNames_ith
}


##### Assign data.frame to hold results ######

dat <- SetupDataOutput(NDays, NIDByDay, FileNames)


##### Calculate blink information from each file #####

#Loop across days:

Counter <- 0

for (i in 1 : NDays) {
    
    #Append day number
    #I.e., "Day1".
    day <- paste0("Day", as.character(i))
    
    #Loop across participants in the ith day
    for (j in 1 : NIDByDay[i]) {
        
        Counter <- Counter + 1
        
        fileName <- 
            paste0(PathData,
                   day,
                   "/",
                   FileNames[[i]][j])
        
        #Import the "sensor data" csv file
        data <- 
            read.csv(fileName,
                     header = FALSE)
        
        
        ##### Strip unneeded rows ######
        
        #Find row that corresponds to the header names
        #(There are other rows above).
        StartIdx <- which(data$V1 == "1") - 1
        
        #Remove rows above header names
        data <- data.frame(data[StartIdx : nrow(data),])
        
        #Treat the top row as headers
        names(data) <- data[1,]
        
        #Remove the top row now
        data <- data.frame(data[2 : nrow(data),]) 

        
        ##### Verify presence of necessary columns ######
        
        MissingColumnsTF <- 
            is.null(data$ET_GazeLeftx)  || 
            is.null(data$ET_GazeLefty)  ||
            is.null(data$ET_GazeRightx) ||
            is.null(data$ET_GazeRighty) ||
            is.null(data$Eye.Closure)   ||
            is.null(data$Timestamp)
        
        if (MissingColumnsTF) {
          
            stop ()
        }
        
        ##### Strip additional unneeded rows ######       
        
        #Affdex data
        #Also remove irrelevant rows such as "StartSlide" and "StartMedia" 
        dataAffdex <- data[(data$Eye.Closure) != "",]
        
        #Eye-tracking data
        #Also remove irrelevant rows such as "StartSlide" and "StartMedia" 
        dataET <- data[(data$ET_GazeLeftx) != "",] 
        
        
        ##### Calculate blink rate ######
        
        #Arguments:
        
        #Convert the timestamps from milliseconds to seconds
        TimestampsAffdex <- as.numeric(dataAffdex$Timestamp) / 1000
        TimestampsET <- as.numeric(dataET$Timestamp) / 1000
        
        #Affdex info
        EyeClosuresAffdex <- as.numeric(dataAffdex$Eye.Closure)
        
        #Eye-tracking info
        EyeClosuresET <- 
            data.frame(
                LeftX = dataET$ET_GazeLeftx,
                LeftY = dataET$ET_GazeLefty,
                RightX = dataET$ET_GazeRightx,
                RightY = dataET$ET_GazeRighty)
        
        #Blink output
        BlinkOutput <- 
            GetBlinks(EyeClosuresAffdex, EyeClosuresET, TimestampsAffdex,  
                TimestampsET, ThresholdAffdex, ThresholdET_Min, ThresholdET_Max, 
                InterBlinkBufferAffdex, InterBlinkBufferET, NormalizeAffdexTF,
                MergeBlinksNonZeroSpanAffdexTF, 
                MergeBlinksNonZeroThresholdAffdex) 

        #Count of blinks
        dat$NBlinksAffdex[Counter] <- BlinkOutput[[1]]$NBlinks
        dat$NBlinksET[Counter]     <- BlinkOutput[[2]]$NBlinks
        
        #Assign the blink rate column
        #Note: using the blink rate rather than the number of blinks can be 
        #useful if iMotions wasn't able to record the full video for a given 
        #participant.
        dat$BlinkRateAffdex[Counter] <- BlinkOutput[[1]]$BlinkRateMin
        dat$BlinkRateET[Counter]     <- BlinkOutput[[2]]$BlinkRateMin
        
        #Assign the timestamps of unique blinks 
        #There will be one timestamp per unique blink.
        #To fit multiple timestamps in a single cell, cast to type character and
        #collapse.
        dat$BlinkTimestampAffdex[Counter] <- 
            paste0(
                BlinkOutput[[1]]$BlinkTimestamp, 
                collapse = "; "
            )
        
        dat$BlinkTimestampET[Counter] <- 
            paste0(
                BlinkOutput[[2]]$BlinkTimestamp, 
                collapse = "; "
            )
    }
}

#Return data.frame with organized blink information
return (dat)

}