################################################################################
#Script CustomBlinkCode
#        
#    Douglas Magill
#    12/17/2020
#
#
#    Description
#    -----------
#        
#    Obtain the blink rate, from both Affdex and eye-tracking, of each 
#    participant on each of five study days. Data are from the Billboard study 
#    collected in late Winter 2020. 
#
#    The code is based upon two iMotions R scripts, one of which uses Affdex to
#    determine blinks and of which uses eye-tracking to determine blinks (see
#    https://help.imotions.com/hc/en-us/articles/360012871420-R-Notebook-Facial-expression-analysis-based-blink-detection
#    and https://help.imotions.com/hc/en-us/articles/360012867980-R-Notebook-Eye-tracking-based-blink-detection ).
#    However, code from the iMotions R scripts is not reused; rather, custom 
#    code was developed to replicate the iMotions R script by only using similar
#    parameters (i.e., input thresholds). 
#
#    The purpose of this code is (1) to help the author understand the  
#    operations, (2) to fix some issues identified in the output from the  
#    iMotions R scripts, (3) to verify the functioning of the iMotions R 
#    scripts, and (4) to make an improvement to the blink-detection operations
#    for the Affdex data. As the output from this code seems to improve upon the
#    the iMotions R scripts, it is recommended to use the current code rather 
#    than than the iMotions scripts.
#    
#    Regarding point (2) -- fixing issues with the output of the iMotions R
#    script -- at least two issues needed to be remedied:
#        (A) The first issue is that the script uses an improper duration as the 
#            denominator for calculating the blink rate (N blinks / duration). 
#            Specifically, for an unknown reason, for some participants on some 
#            days, the video recording stopped before the video stimulus 
#            completed. Rather than using the duration of the video recording
#            for the denominator, the iMotions script uses the duration of the 
#            video stimulus as the denominator. Consequently, the blink rate 
#            returned is less than it should be. (Note that this issue affects
#            Affdex blink rate but not eye-tracking blink rate). The present 
#            script corrects this. 
#            On a slightly related note, note that, when viewing the "replay" 
#            video in iMotions, the chart of "Blink detection (AFFDEX)" will not 
#            align with "Facial Expression: Eye Closure". This is not 
#            necessarily because "Blink detection (AFFDEX)" is wrong; rather, it
#            is because the chart of "Blink detection (AFFDEX)" is stretched out
#            to the duration of the video stimulus when it should only extend to 
#            the end of the video recording. However, this is not to say that
#            "Blink detection (AFFDEX)" is entirely correct: see point B.
#        (B) For unknown reasons, the iMotions R Affdex script does not count 
#            some eye-closures (even ones well-permitted by the thresholds). For 
#            example, see the "replay" for participant 2 on day 1. The reasons
#            for this have not been investigated, but one could do so by 
#            consulting the iMotions R Affdex script. The present script does 
#            not have this issue.
#
#     Regarding point 3 -- verify the functioning of the iMotions R scripts --
#     beside the two issues identified in point 2, one departure from the Affdex 
#     reference (https://help.imotions.com/hc/en-us/articles/360012871420-R-Notebook-Facial-expression-analysis-based-blink-detection) 
#     was identified. No departures from the eye-tracking reference (https://help.imotions.com/hc/en-us/articles/360012867980-R-Notebook-Eye-tracking-based-blink-detection)  
#     were identified. 
#     Regarding the departure from the Affdex reference, a span of non-zero 
#     confidence of eye closure was found to result in a merged blink in the 
#     iMotions Affdex R but not in the present script. As an example, see the 
#     "replay" for participant 5 on day 1 for 00:10 through 00:19. Specifically,
#     although the eye-closure confidence falls below the specified eye-closure 
#     threshold numerous times during this span, the entire span is still 
#     counted as a single blink. I'm not sure whether this occurrence is 
#     intentional, but it is actually helpful as it removes potential false 
#     positives. Consequently, I attempted to replicate this effect in the 
#     present code (see function GetBlinksAffdex_OneBlinkPerSpan).
#
#     Regarding point 4 -- an improvement -- I introduced normalizing the Affdex
#     eye-closure across participants such that, if a participant never had an
#     eye closure reach 100 confidence, all eye-closures would be adjusted such
#     that the maximum eye-closure would equal 100 (see function 
#     GetBlinksAffdex). This seemed to improve the accuracy of the Affdex 
#     algorithm to detect blinks.
#
#
#     Summary of Operations
#     ---------------------
#
#     Note: an asterisk denotes that a step not included in the iMotions 
#     references.  
#
#     1. Parse inputs and gather data (function BlinksPerFile)
#     2. Organize operations (function GetBlinks)
#     3. Affdex operations
#         A.* Normalize the eye-closures such that, for each participant, the
#             maximum eye closure should equal 100 (function GetBlinksAffdex).
#         B.  Determine eye closures above eye-closure threshold (function 
#             GetBlinksAffdex)
#         C.  Determine whether space between blinks is sufficient (function
#             GetBlinksAffdex_Interblink)
#         D.* Limit one blink to a non-zero span (function 
#             GetBlinksAffdex_OneBlinkPerSpan)
#     4. Eye-tracking operations
#         A.  Determine segments of missing eyes that exceed the minimum-
#             duration threshold and do not exceed the maximum-duration 
#             threshold (function GetBlinksET).
#         B.  Determine whether segments that passed the thresholds mentioned
#             previously (in A) have a sufficient space between them (function 
#             GetBlinksET).
#     5. Return organized data.frame that contains the blink rate broken down by
#        participant and day. The number of blinks and the timestamps of blinks
#        are also provided. 
#     
#   
#     Inputs
#     ------
#
#     1. Parameters
#     2. Data
#     
#
#    Input Data
#    ----------
#
#    The data were exported from iMotions using the "sensor data" option. The 
#    data are from five iMotions studies, each corresponding to one of the five 
#    days. Note that when exporting sensor data from iMotions, a separate file 
#    is produced for each participant. As a result, the total number of files is
#    5 x N participants. where there are five files for each participant, each 
#    corresponding to one of the five days. The current code loads each of these
#    files in turn but stores results across all participants in a single 
#    data.frame.
#
#    The files should be placed in a parent folder, specified by input PathData.
#    Within the parent folder, there should be five child folders. All data 
#    files should be contained within these child folders. The child folders
#    should take the names "Day1", "Day2", "Day3", etc.
#
################################################################################


##### Specify inputs #####

#Data info:

#Specify path of iMotions data
#Use the "sensor data" option in iMotions to export the files. There will be a
#separate file for each participant.
PathData <- 
  "G:/My Drive/U Akron/CBA RA/Billboard/March 2020 Study/Blink Detection/SensorData/"

#Number of days across which data were collected
#Within the folder specified by DataPath, there should be a child folder 
#corresponding to each day.  The child folders should take the names "Day1", 
#"Day2", "Day3", etc.
NDays <- 5

#Affdex parameters:

#Specify Affdex probability threshold (as percentage)
#In other words, enter probability .10 as percentage 10
#The form is as a percentage because this is the form in the data file.
ThresholdAffdex <- .5

#Specify the amount of time, in seconds, that should be between blinks
#Otherwise, each frame during an individual eye closure would be counted as a   
#separate blink.
InterBlinkBufferAffdex <- .9

#Affdex parameters that are not included in Affdex iMotions reference:

#Specify whether to normalize eye closure percentages
#This can help better identify eye-closures for participants with 
#idiosyncratically low percentages. This operation is not conducted in the 
#iMotions R script.
NormalizeAffdexTF <- TRUE

#Specify whether to merge blinks into one blink if they lie within the same span
#of non-zero eye-closure values. Note that a non-zero span includes values that
#may be below ThresholdAffdex. 
MergeBlinksNonZeroSpanAffdexTF <- TRUE

#Specify the threshold to use to classify non-zero spans as non-zero. This is
#because all eye-closures are likely to be close to zero but not exactly zero. 
MergeBlinksNonZeroThresholdAffdex <- .05

#Eye-tracking parameters:

#Minimum time closed, in seconds
ThresholdET_Min <- .1

#Maximum time closed, in seconds
ThresholdET_Max <- .5

#Minimum time between blinks, in seconds
InterBlinkBufferET <- .3

#Functions location:

#Specify location of function files
#The files sourced below should be located in this directory. Note that the 
#files should be located directly in this location and not nested in a child 
#folder.
PathFunctions <- "G:/My Drive/U Akron/CBA RA/Billboard/March 2020 Study/Blink Detection/CustomBlinkCode/Funs/"

#Output csv location:

PathOutput <- "G:/My Drive/U Akron/CBA RA/Billboard/March 2020 Study/Blink Detection/CustomBlinkCode/Output/"


##### Load functions to compute blink info #####

#Load function BlinksPerFile
source(paste0(PathFunctions, "BlinksPerFile.R"))


##### Return data.frame with organized blink information #####

BlinkInfo <- 
    BlinksPerFile(PathData, PathFunctions, NDays, ThresholdAffdex,  
        ThresholdET_Min, ThresholdET_Max, InterBlinkBufferAffdex, 
        InterBlinkBufferET, NormalizeAffdexTF, MergeBlinksNonZeroSpanAffdexTF, 
        MergeBlinksNonZeroThresholdAffdex)  

#Write to csv file
write.table(BlinkInfo, 
            file = paste0(PathOutput, "BlinksOutput.csv"), 
            sep = ",", #comma-separated   #for tab-sep, use "\t"
            row.names = FALSE, #no column of row names 
            col.names = TRUE, #row of column names
            na = "") #represent missing values as blanks

