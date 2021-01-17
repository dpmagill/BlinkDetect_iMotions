GetBlinksAffdex_OneBlinkPerSpan <- 
    function (BlinkTFAffdex, EyeClosureTF, EyeClosurePercentages, 
              MergeBlinksNonZeroThresholdAffdex) {
################################################################################
#Function GetBlinksAffdex_OneBlinkPerSpan 
#        
#    Douglas Magill
#    12/17/2020
#
#    Description:
#      
#    Designed to be called by function GetBlinksAffdex.
#
#    Remove points classified as blinks, as determined by Affdex, that fall 
#    within the same continuous eye closure span.  Sometimes, several points 
#    within a continuous eye closure can be classified as distinct blinks. This
#    would occur if the span was longer than the interblink interval. Because 
#    more than one blink cannot occur when the eye is closed, count the entire 
#    span as only one blink. 
#
#    This function performs an operation similar to that used by the iMotions R 
#    script.
#
#    I'm not sure if this was intentional in the iMotions R script, but it 
#    tended to count segments as spans even when the probability percentage 
#    within the span was not always above the specified eye-closure threshold 
#    (EyeClosureThreshold). To replicate this a bit, the eye-closure probability 
#    percentage threshold used for spans is set to a low value.
#
################################################################################

             
#If at least one unique blink present
if ( sum(BlinkTFAffdex) > 0 ) {     
        
    
    ##### Use a low probability percentage to detect spans #####                
    
    #Note: this percentage will likely be smaller than EyeClosureThreshold.        
            
    EyeClosure_LowProbTF <- 
        EyeClosurePercentages > MergeBlinksNonZeroThresholdAffdex       
                 
    
    ##### Label each continuously-elevated section with a different label #####
            
    #E.g., c(T, T, T, F, T, F, T, T, F) ==> c(1, 1, 1, NA, 2, NA, 3, 3, NA)
    
    #Preallocate vector indicating continuous sections
    #0 == not part of a section.
    Spans <- integer(length = length(EyeClosure_LowProbTF))
    
    #Initialize label
    Label <- 1L
    
    #Initialize flag that a span has ended
    SpanEndedTF <- TRUE
    
    #Loop across all frames
    for (i in 1 : length(EyeClosure_LowProbTF)) {
        
        #Label
        if (EyeClosure_LowProbTF[i]) {
            
            Spans[i] <- Label
            
            SpanEndedTF <- FALSE
            
        } else if (!SpanEndedTF) {
            
            #Advance the label every time a section is completed
            Label <- Label + 1L
            
            #Indicate a span has ended
            SpanEndedTF <- TRUE
        }
    }
    
    
    ###### Classify only one blink per eye-closure span ###### 
    
    #If there is at least one frame classified as an eye closure
    if ( any(Spans == 1L) ) {
    
        #Loop across spans
        for (i in 1 : max(Spans)) {
            
            #Only assess a span if there is more than one frame with an eye closure
            if ( sum(Spans == i) > 1L) {
                
                #Only assess a span if there is more than one frame classified as a
                #blink.
                if ( sum(BlinkTFAffdex[Spans == i]) > 1L ) {
                    
                    #Find linear index of frames within the span
                    LinIdxSegment <- which(Spans == i)
                    
                    #Select only one index to represent a blink:
                    
                    IdxToUse <- round( median(LinIdxSegment) ) 
                    
                    BlinkTFAffdex[IdxToUse] <- TRUE
                    
                    #Deselect all other linear indices within the span:
                    
                    IdxNotUse <- LinIdxSegment[LinIdxSegment != IdxToUse]
                    
                    BlinkTFAffdex[IdxNotUse] <- FALSE
                }
            }
        }
    }
}


#Reassign the logical index indicating blinks
return (BlinkTFAffdex)

}