GetBlinksET <- 
    function (EyeClosuresET, TimestampsZeroET, ThresholdET_Min, ThresholdET_Max, 
              InterBlinkBufferET) {
################################################################################
#Function GetBlinksET 
#        
#    Douglas Magill
#    12/17/2020
#
#    Description:
#      
#    Designed to be called by function GetBlinks.
#
#    Return a logical vector indicating the location of blinks as determined by
#    the eye-tracking thresholds.
#
#    This function performs an operation similar to that used by the iMotions R 
#    script.
#
################################################################################

        
###### Identify potential blinks ######

#Samples where all eye-coordinates are missing
#I.e., -1. 
#This criteria is based on the criteria in the iMotions R script.
EyeClosureTFET <- 
    as.numeric(EyeClosuresET$LeftX)  < 0 &
    as.numeric(EyeClosuresET$LeftY)  < 0 &
    as.numeric(EyeClosuresET$RightX) < 0 &
    as.numeric(EyeClosuresET$RightY) < 0  

    #Preallocate output
    #Logical vector indicating the locations of blinks
    #One index is assigned for each blink.
    BlinkTFET <- logical(length = length(EyeClosureTFET))    
                 
#If at least one eye-missing present
if ( sum(EyeClosureTFET) > 0 ) {     
        
    ##### Label each potential-blink span with a different label #####
            
    #E.g., c(T, T, T, F, T, F, T, T, F) ==> c(1, 1, 1, NA, 2, NA, 3, 3, NA)
    
    #Preallocate vector indicating continuous sections
    #0 == not part of a section.
    
    NSamples <- length(EyeClosureTFET)
    
    Spans <- integer(length = NSamples)
    
    #Initialize label
    Label <- 1L
    
    #Initialize flag that a span has ended
    SpanEndedTF <- TRUE
    
    #Initialize flag that at least one potential-blink classified as a blink
    AnySpanPassedTF <- FALSE
    
    #Loop across all frames
    for (i in 1 : NSamples) {
        
        #Label 
        if (EyeClosureTFET[i]) {
            
            Spans[i] <- Label
            
            SpanEndedTF <- FALSE
            
        } else if (!SpanEndedTF) {
            
            #Advance the label every time a section is completed
            Label <- Label + 1L
            
            #Indicate a span has ended
            SpanEndedTF <- TRUE
        }
    }
    
    
    ###### Determine whether a potential blink is a blink ###### 
    
    #If there is at least one span classified as a potential blink
    if ( any(Spans == 1L) ) {
    
        #Mean interval between timestamps
        MeanSampleInterval <- mean( diff(TimestampsZeroET) )
        
        #Loop across spans to find duration of each potential blink
        for (i in 1 : max(Spans)) {
                
            #Logical index of ith span
            SpanLogIdx <- Spans == i
            
            #Linear index of ith span
            SpanIdx <- which(SpanLogIdx)
            
            #N samples in ith span
            SpanNSamples <- length(SpanIdx)

            #If only one sample in ith span
            if (SpanNSamples == 1L) {
                
                #Use the mean sample interval as the duration of the potential
                #blink.
                Diff <- MeanSampleInterval
                
                #The following approach calculated the duration by subtracting 
                #the timestamp from a nearby timestamp. However, this resulted
                #in false positives.  This is because a laggy timestamp (i.e., a
                #timestamp with a larger-than-usual gap between itself and the
                #last) looked as if it had a longer duration than it actually 
                #did.
                # #If index 1
                # if (SpanIdx == 1L) { 
                #      
                #     Diff <- TimestampsZeroET[2] - TimestampsZeroET[1]
                # 
                # #If other index    
                # } else {
                #     
                #     Diff <- 
                #         TimestampsZeroET[SpanIdx] - 
                #         TimestampsZeroET[SpanIdx - 1]                            
                # }
            
            #More than one sample in ith span       
            } else {
                
                #Timestamps corresponding to a potential blink span
                Times <- TimestampsZeroET[SpanLogIdx]
                
                #Use the duration between the last and first samples of the span
                #as the duration of the potential blink.
                Diff <- Times[SpanNSamples] - Times[1]             
            }
            
            
            #Determine whether the duration of the potential blink satisfies the 
            #duration thresholds:
            
            RejectTF <- 
                Diff < ThresholdET_Min || #minimum duration
                Diff > ThresholdET_Max    #maximum duration
            
            #If potential-blink passed duration min and max thresholds
            if (!RejectTF) {
                
                #Determine whether the duration satisfies the interblink
                #threshold:
                                
                #If a previous potential-blink classified as a blink
                #The threshold cannot be applied if there is not a previous 
                #point classified as a blink.
                if (AnySpanPassedTF) {
                  
                    #First index of ith span
                    SpanIdxFirst <- SpanIdx[1]                   
                      
                    Diff <- 
                        TimestampsZeroET[SpanIdxFirst] - 
                        TimestampsZeroET[SpanIdxLast_Previous]
                    
                    RejectTF <- Diff <= InterBlinkBufferET 
                }
                
                #If passed 
                if (!RejectTF) {
                    
                    #Record index of last timestamp of ith span for comparison 
                    #on the next iteration (span i + 1).
                    SpanIdxLast_Previous <- SpanIdx[SpanNSamples]               
                    
                    #Flag that that at least one potential-blink has passed all
                    #of the thresholds.
                    AnySpanPassedTF <- TRUE
                    
                    #Assign the index to correspond to the blink:
                    
                    #If one sample in span
                    if (SpanNSamples == 1L) {
                        
                        IdxToUse <- SpanIdx
                        
                    } else {
                        
                        IdxToUse <- floor( median(SpanIdx) )    
                    }
                                    
                    #Classify index as blink
                    BlinkTFET[IdxToUse] <- TRUE                   
                }
            }
        }
    }
}

#Return logical index indicating blinks
return (BlinkTFET) 
              

}