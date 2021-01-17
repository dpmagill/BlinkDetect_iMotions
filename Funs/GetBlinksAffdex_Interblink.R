GetBlinksAffdex_Interblink <- 
    function (EyeClosureTFAffdex, EyeClosureTrueTimeAffdex, 
              InterBlinkBufferAffdex) {
################################################################################
#Function GetBlinksAffdex_Interblink 
#        
#    Douglas Magill
#    12/17/2020
#
#    Description:
#        
#    Designed to be called by function GetBlinksAffdex.
#
#    Segment eye closures, as determined by Affdex, into separate blinks 
#    according to InterBlinkBufferAffdex. 
#
################################################################################
    
NFramesEyeClosures <- length(EyeClosureTrueTimeAffdex)

#Preallocate vector indicating unique blinks. To determine unique blinks,
#see whether there is a sufficient distance (i.e., the inter-blink buffer)
#between a given frame and a prior frame. During the calculation, only 
#frames where an eye closure occurred are compared. Hence, this vector is  
#the length of the number of frames where an eye closure occurred.
UniqueBlinkTFAffdex <- rep(TRUE, NFramesEyeClosures)

#Compare each frame where an eye closure occurred to prior frames where an 
#eye closure occurred.  If the distance criterion is met (i.e., the  
#inter-blink buffer), UniqueBlinkTF == TRUE.
#No comparison is needed if there is only one frame with an eye closure.
if (NFramesEyeClosures > 1) {

    for (i in 2 : NFramesEyeClosures) {
            
        ContinueTF <- TRUE
    
        Counter <- 1L      
        
        while (ContinueTF) {
            
            #Index of a prior frame classified as an eye closure
            ii <- i - Counter
            
            #Continue to go back until the most distant prior frame possible  
            #is reached.
            if (ii != 0) {

                #Time interval between the current frame (i) and the iith 
                #prior frame.
                Interval <- 
                    EyeClosureTrueTimeAffdex[i] - EyeClosureTrueTimeAffdex[ii]                    
            
                #If (1) the interval is not smaller than the inter-blink  
                #interval and (2) the iith prior frame has not been  
                #previously invalidated. (If the prior frame has been
                #previously invalidated, no comparison is needed).
                if (Interval < InterBlinkBufferAffdex && 
                    UniqueBlinkTFAffdex[ii]) {

                    #Disqualify the frame
                    #This is the key output variable of this loop
                    UniqueBlinkTFAffdex[i] <- FALSE
                        
                    #End while loop 
                    ContinueTF <- FALSE
                    
                #Continue to a previous frame
                } else {
                    
                    Counter <- Counter + 1L    
                }
            
            #There are no more previous frames        
            } else {
                
                #End while loop
                ContinueTF <- FALSE
            }
        }
    }
}


#UniqueBlinkTFAffdex is the length of frames classifed as eye closures; expand 
#the length to align with all frames:
     
BlinkTFAffdex <- logical(length = length(EyeClosureTFAffdex))    

BlinkTFAffdex[EyeClosureTFAffdex] <- UniqueBlinkTFAffdex
    
    return (BlinkTFAffdex)
}