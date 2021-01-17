SetupDataOutput <- function(NDays, NIDByDay, FileNames) {
################################################################################
#Function SetupDataOutput 
#        
#    Douglas Magill
#    12/17/2020
#
#    Description:
#        
#    Designed to be called by function BlinksPerFile.
#
#    Prepare the data.frame to be used as output of the blink information. 
#
################################################################################
    
    
#The total instances a participant was run
#Note: some participants may have missed a day, which is why N participants *
#N days is not used.
Ndat <- sum(NIDByDay)

#Preallocate data.frame
#Columns: the participant ID, the day the ID was run, and various columns of
#blink information.
dat <- 
    data.frame(
        Day                  = rep(NA_integer_,   Ndat),
        ID                   = rep(NA_character_, Ndat),
        NBlinksAffdex        = rep(NA_integer_,   Ndat),
        NBlinksET            = rep(NA_integer_,   Ndat),
        BlinkRateAffdex      = rep(NA_real_,      Ndat),
        BlinkRateET          = rep(NA_real_,      Ndat),
        BlinkTimestampAffdex = rep(NA_character_, Ndat),
        BlinkTimestampET     = rep(NA_character_, Ndat)               
    ) 

#Assign the day and ID columns:

start   <- 0
end     <- 0
counter <- 0

#Loop across the five days
for (i in 1 : NDays) {
    
    start <- end + 1
    
    #Number of participants for ith day
    NID <- NIDByDay[i]
    
    end <- start + NID - 1
    
    #Assign day number
    dat$Day[start : end] <- i 
    
    #Loop across the number of participans for a given day
    #Extract and assign the ID number from the file name of the iMotions sensor
    #data file.
    for (j in 1 : NID) {
      
        counter <- counter + 1
        
        #The file name of the jth ID of the ith day  
        FileName_ij <- FileNames[[i]][j]
        
        #The number of characters in the file name other than the '.csv' 
        #extension. 
        #Subtract 4 to ignore the extension. 
        FileName_ijLength <- nchar(FileName_ij) - 4
        
        #Start from the 5th character because the first 4 characters represent 
        #the order that iMotions exported the files rather than the ID number.
        dat$ID[counter] <- substr(FileName_ij, 5, FileName_ijLength)
    }
}

return (dat)

}