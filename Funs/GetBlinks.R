GetBlinks <- 
    function (EyeClosuresAffdex, EyeClosuresET, TimestampsAffdex, TimestampsET, 
              ThresholdAffdex, ThresholdET_Min, ThresholdET_Max, 
              InterBlinkBufferAffdex, InterBlinkBufferET, NormalizeAffdexTF,
              MergeBlinksNonZeroSpanAffdexTF, 
              MergeBlinksNonZeroThresholdAffdex) {
################################################################################
#Function GetBlinks 
#        
#    Douglas Magill
#    12/17/2020
#
#    Description:
#        
#    Designed to be called by function BlinksPerFile.
#
#    Return a list of that contains the blink rate (blinks per minute) and other
#    variables for each participant for each study day.
#
################################################################################
        
        
##### Modify timestamps to begin at zero for ease of use ##### 
        
TimestampsZeroAffdex <- TimestampsAffdex - TimestampsAffdex[1]

TimestampsZeroET <- TimestampsET - TimestampsET[1]    
       

##### Determine location of blinks with Affdex #####   
        
BlinkTFAffdex <- 
    GetBlinksAffdex(EyeClosuresAffdex, TimestampsZeroAffdex, ThresholdAffdex, 
        InterBlinkBufferAffdex, NormalizeAffdexTF,
        MergeBlinksNonZeroSpanAffdexTF, MergeBlinksNonZeroThresholdAffdex)


##### Determine location of blinks with eye-tracking #####

BlinkTFET <- 
    GetBlinksET(EyeClosuresET, TimestampsZeroET, ThresholdET_Min,  
        ThresholdET_Max, InterBlinkBufferET)


##### Calculate output arguments #####

#Blink rate:

#Note: using the blink rate rather than the number of blinks can be useful if
#iMotions wasn't able to record the full video for a given participant.

#Check whether any elements are NA
#This is not expected to be the case.
#If so, this will interfere with the sum function below.
if ( any(is.na(BlinkTFAffdex)) ||
     any(is.na(BlinkTFET)) ) {
    
    stop()
}

#Number of unique blinks
NBlinksAffdex <- sum(BlinkTFAffdex)
NBlinksET <- sum(BlinkTFET)

#Blink rate (blinks/second)
BlinkRateAffdex <- 
    NBlinksAffdex / 
    (TimestampsZeroAffdex[length(TimestampsZeroAffdex)] - 
     TimestampsZeroAffdex[1])

BlinkRateET <- 
    NBlinksET / 
    (TimestampsZeroET[length(TimestampsZeroET)] - 
     TimestampsZeroET[1])

#Blink rate (blinks/minute)
BlinkRateMinAffdex <- BlinkRateAffdex * 60
BlinkRateMinET <- BlinkRateET * 60

#Timestamps of unique blinks:

#Vector of timestamps corresponding to each unique blink
#Round to two digits for readability.

BlinkTimestampAffdex <-
    round(
        TimestampsZeroAffdex[BlinkTFAffdex],
        digits = 2
    )

BlinkTimestampET <-
    round(
        TimestampsZeroET[BlinkTFET],
        digits = 2
    )


##### Convert blink timestamps to minute:second form ######

#Affdex:

BlinkTimestamp_MinSecAffdex <- TimestampsMS(NBlinksAffdex, BlinkTimestampAffdex)

#ET:

BlinkTimestamp_MinSecET <- TimestampsMS(NBlinksET, BlinkTimestampET)


###### Return output arguments ######

return (
    list(
        Affdex = list(
            "NBlinks" = NBlinksAffdex,
            "BlinkRateMin" = BlinkRateMinAffdex, 
            "BlinkTimestamp" = BlinkTimestamp_MinSecAffdex
        ),
        ET = list(
            "NBlinks" = NBlinksET,
            "BlinkRateMin" = BlinkRateMinET, 
            "BlinkTimestamp" = BlinkTimestamp_MinSecET            
        )
    )
)


##### Currently unused return arguments ###### 

#Vector of logical values corresponding to each unique blink: length aligns with 
#the entire stimulus presentation.  
#BlinkTF <- rep(FALSE, length(TimestampsZero))
#BlinkTF[EyeClosureTF] <- UniqueBlinkTF

#Vector of timestamps corresponding to each unique blink: length aligns with the 
#entire stimulus presentation. Frames that do not correspond to a unique 
#blink == NA.
#blinkTimestamp <- rep(FALSE, length(TimestampsZero))
#blinkTimestamp[BlinkTF] <- EyeClosureTrueTime


}