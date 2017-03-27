# Reproducible Research: Peer Assessment 2


## Loading and preprocessing the data


```r
stormDataFilename <- "stormData.csv.bz2"

stormDataURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"

if(!(stormDataFilename %in% list.files())) {
        download.file(stormDataURL, destfile = stormDataFilename, mode = "wb")
}

stormDataDocFilename <- "stormDataDoc.pdf"

stormDataDocURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf"

if(!(stormDataDocFilename %in% list.files())) {
        download.file(stormDataDocURL, destfile = stormDataDocFilename, mode = "wb")
}
        
stormDataFAQFilename <- "stormDataFAQ.pdf"

stormDataFAQURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2FNCDC%20Storm%20Events-FAQ%20Page.pdf"

if(!(stormDataFAQFilename %in% list.files())) {
        download.file(stormDataFAQURL, destfile = stormDataFAQFilename, mode = "wb")
}
```