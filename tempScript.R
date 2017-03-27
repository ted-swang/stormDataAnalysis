library(dplyr)
library(ggplot2)
library(tidyr)
library(forcats)

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

if(!("stormDataInit" %in% ls())) stormDataInit <- read.csv(stormDataFilename)

stormData <- stormDataInit %>%
        tbl_df %>%
        select(EVTYPE, FATALITIES, INJURIES, PROPDMG, CROPDMG) %>%
        filter(FATALITIES > 0 | INJURIES > 0 | PROPDMG > 0 | CROPDMG > 0)

stormData$EVTYPE <- stormData$EVTYPE %>% fct_drop

TStormWindFactors <- c(
        stormData$EVTYPE[grep("TSTM WIND*", stormData$EVTYPE, ignore.case = TRUE)] %>% 
                unique %>%
                as.character,
        stormData$EVTYPE[grep("THUNDERSTORM WIND*",stormData$EVTYPE, ignore.case = TRUE)] %>% 
                unique %>%
                as.character,
        "THUNDERSTORM  WINDS", "LIGHTNING AND THUNDERSTORM WIN", 
        "THUNDERTORM WINDS", "THUNDERSTORMS WINDS", "THUNDERSTORMW", "TSTMW"
        )


TStormWindFactors <- TStormWindFactors[
        !(TStormWindFactors %in% 
        c("NON-TSTM WIND", "NON TSTM WIND", "TORNADOES, TSTM WIND, HAIL")
        ) 
        ]

TornadoFactors <- c(
        stormData$EVTYPE[grep("TORNADO", stormData$EVTYPE,ignore.case= TRUE)]%>%
                unique %>%
                as.character,
        "FUNNEL CLOUD"
        )
        
HeatFactors <- c(
        stormData$EVTYPE[grep("HEAT", stormData$EVTYPE, ignore.case = TRUE)] %>%
        unique %>%
        as.character,
        stormData$EVTYPE[grep("WARM", stormData$EVTYPE, ignore.case = TRUE)] %>%
        unique %>%
        as.character
)

FloodFactors <- c(
        stormData$EVTYPE[grep("FLOOD", stormData$EVTYPE, ignore.case = TRUE)]%>%
        unique %>%
        as.character,
        "DROWNING", "EXCESSIVE RAINFALL", "RAPIDLY RISING WATER", "Torrential Rainfall", "HIGH WATER"
)

HurricaneFactors <-  c(
        stormData$EVTYPE[grep("HURRICANE",stormData$EVTYPE,ignore.case=TRUE)]%>%
                unique %>%
                as.character,
        stormData$EVTYPE[grep("TYPHOON",stormData$EVTYPE,ignore.case=TRUE)]%>%
                unique %>%
                as.character,
        stormData$EVTYPE[grep("TROPICAL",stormData$EVTYPE,ignore.case=TRUE)]%>%
                unique %>%
                as.character,
        "TSUNAMI"
        ) %>% 
        unique

stormData$EVTYPE <- stormData$EVTYPE %>%
        fct_collapse(
                TSTORMWIND = TStormWindFactors,
                TORNADO = TornadoFactors,
                HEAT = HeatFactors,
                FLOOD = FloodFactors,
                TROPSTORM = HurricaneFactors
                     )

popHealthFull <- stormData %>%
        filter(FATALITIES > 0 | INJURIES >0) %>%
        group_by(EVTYPE) %>%
        summarize(FATALITIES = sum(FATALITIES), INJURIES = sum(INJURIES)) %>%
        mutate(CASUALTIES = FATALITIES + INJURIES) %>%
        arrange(desc(CASUALTIES))

popHealthFull$EVTYPE <- popHealthFull$EVTYPE %>% fct_drop
        
thr <- popHealthFull$CASUALTIES[10]

sumFat <- sum(popHealthFull$FATALITIES)
sumInj <- sum(popHealthFull$INJURIES)

popHealthSummary <- popHealthFull %>%
        mutate(EVTYPE=ifelse(CASUALTIES>=thr,as.character(EVTYPE),"OTHER")) %>%
        group_by(EVTYPE) %>%
        summarize(FATALITIES = sum(FATALITIES), INJURIES = sum(INJURIES)) %>%
        mutate(FATALITIES = FATALITIES/sumFat, INJURIES = INJURIES/sumInj) %>%
        gather(CATEGORY, VALUE, FATALITIES:INJURIES)

popHealthSummary$EVTYPE <- popHealthSummary$EVTYPE %>% 
        as_factor %>% 
        fct_relevel(as.character(popHealthFull$EVTYPE[1:10]))

g <- ggplot(popHealthSummary, aes(x = EVTYPE, y = VALUE, fill = CATEGORY)) +
        geom_col(position = "dodge") + 
        theme(axis.text.x = element_text(angle = 90, hjust = 1))

econ <- stormData %>%
        group_by(EVTYPE) %>%
        summarize(PropertyDamage = sum(PROPDMG), CropDamage = sum(CROPDMG))