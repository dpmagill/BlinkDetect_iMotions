GetBlinksAffdex <- 
    function (EyeClosuresAffdex, TimestampsZeroAffdex, ThresholdAffdex, 
              InterBlinkBufferAffdex, NormalizeAffdexTF, 
              MergeBlinksNonZeroSpanAffdexTF, 
              MergeBlinksNonZeroThresholdAffdex) {
################################################################################
#Function GetBlinksAffdex 
#        
#    Douglas Magill
#    12/17/2020
#
#    Description:
#      
#    Designed to be called by function GetBlinks.
#
#    Return a logical vector indicating the location of blinks as determined by
#    Affdex thresholds.
#
#    This function performs an operation similar to that used by the iMotions R 
#    script.
#
################################################################################

        
##### Normalize eye closure percentages #####

#This can help better identify eye-closures for participants with 
#idiosyncratically low percentages.

#This operation is not conducted in the iMotions R script.
 
if (NormalizeAffdexTF) {
                       
    #Find the maximum percentage
    MaxPercentage <- max(EyeClosuresAffdex)
    
    #Normalize only if at least one frame does not have a percentage that equals
    #100.
    if (MaxPercentage < 100) {
    
        #Find the factor needed to make the maximum percentage 100
        NormFactor <- 100 / MaxPercentage
    
        #Normalize eye-closure percentages
        EyeClosuresAffdex <- EyeClosuresAffdex * NormFactor
    }
}

  
##### Determine frames with an eye closure #####                
        
#Determine, for each frame, whether there is an eye closure
#Logical vector.
EyeClosureTFAffdex <- EyeClosuresAffdex > ThresholdAffdex

#Vector of timestamps where an eye closure occurred
EyeClosureTrueTimeAffdex <- TimestampsZeroAffdex[EyeClosureTFAffdex]


##### Segment eye closures into separate blinks by interblink buffer ######

#Segment eye closures into blinks
#Logical vector with length equal to the number of all frame.
BlinkTFAffdex <- 
    GetBlinksAffdex_Interblink(EyeClosureTFAffdex, EyeClosureTrueTimeAffdex, 
        InterBlinkBufferAffdex)


##### Count continuously-elevated spans as only one blink ######

#This function performs an operation similar to that used by the iMotions R 
#script.

if (MergeBlinksNonZeroSpanAffdexTF) {

    #BlinkTF = logical vector that aligns with the length of frames.
    BlinkTFAffdex <-
        GetBlinksAffdex_OneBlinkPerSpan(BlinkTFAffdex, EyeClosureTFAffdex, 
            EyeClosuresAffdex, MergeBlinksNonZeroThresholdAffdex)
}


##### Return logical index indicating blinks ######

return (BlinkTFAffdex) 

}