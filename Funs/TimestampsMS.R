TimestampsMS <- 
    function (NBlinks, BlinkTimestamp) {
################################################################################
#Function TimestampsMS 
#        
#    Douglas Magill
#    12/17/2020
#
#    Description:
#        
#    Designed to be called by function GetBlinks.
#
#    Return a vector of timestamps corresponding to blinks in minutes:seconds
#    form.
#
################################################################################

#If at least 1 blink
if (NBlinks > 0L) {

    #Preallocate vector to hold blink timestamps in min:sec form
    BlinkTimestamp_MinSec <-
        character(length = length(BlinkTimestamp))
    
    for (i in 1 : length(BlinkTimestamp)) {
        
        #Blink timestamp in seconds
        Sec <- round(BlinkTimestamp[i], 2)
        
        #Blink timestamp in min:sec
        
        #Blink timestamp minutes portion 
        Min <- as.character( floor(Sec / 60) )
        
        #Blink timestamp seconds portion
        SecRemainder <- round(Sec %% 60, 2) 
        
        #Add a leading zero if a single-digit second:
        
        NDigits <- nchar(as.character(floor(SecRemainder)))
        
        if (NDigits == 1L) {
            
            SecRemainderCh <- as.character(SecRemainder)
            
            SecRemainderCh <- paste0("0", SecRemainderCh)
            
        } else {
            
            SecRemainderCh <- as.character(SecRemainder)
        }
            
        #Combine minutes and seconds
        BlinkTimestamp_MinSec[i] <- paste0(Min, ":", SecRemainderCh)
    }

#If no blinks
} else {
    
    BlinkTimestamp_MinSec <- NA_character_
}
   
        
return (BlinkTimestamp_MinSec)        
             
}