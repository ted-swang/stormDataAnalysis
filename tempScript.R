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
        select(EVTYPE, FATALITIES, INJURIES, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP) %>%
        filter(FATALITIES > 0 | INJURIES > 0 | PROPDMG > 0 | CROPDMG > 0)

stormData$EVTYPE <- stormData$EVTYPE %>% fct_drop

## treating "H" as hundred, "K" as thousand, "M" as million, "B" as billion, and numeric values as scientific notation exponents, we prepare the columns PROPDMGEXP and CROPDMGEXP for numerical calculation
stormData$PROPDMGEXP <- stormData$PROPDMGEXP %>% 
        fct_collapse(
                "0" = c("0", "", "+", "-", "?"),
                "2" = c("h", "H", "2"),
                "3" = c("K", "3"),
                "6" = c("M", "m", "6"),
                "9" = c("B")
                ) %>%
        as.character %>%
        as.numeric

stormData$CROPDMGEXP <- stormData$CROPDMGEXP %>% 
        fct_collapse(
                "0" = c("0", "", "?"),
                "3" = c("K", "k"),
                "6" = c("M", "m"),
                "9" = c("B")
        ) %>%
        as.character %>%
        as.numeric

## We update stormData by replacing the damage columns with their values (including values from EXP columns), and get rid of the EXP columns
stormData <- stormData %>% 
        mutate(PROPDMG = PROPDMG*(10^PROPDMGEXP), CROPDMG =  CROPDMG*(10^CROPDMGEXP)) %>%
        select(-PROPDMGEXP, -CROPDMGEXP)

## Factor names for Thunderstorm related factors including wind and lightning.
## Does not include tornadoes, floods, or hail
TStormFactors <- c("THUNDERSTORM WINDS", "THUNDERSTORM WIND", "THUNDERSTORM WINS", "THUNDERSTORM WINDS LIGHTNING", "THUNDERSTORM WINDS/FUNNEL CLOU", "SEVERE THUNDERSTORM", "SEVERE THUNDERSTORMS", "SEVERE THUNDERSTORM WINDS", "THUNDERSTORMS WINDS", "THUNDERSTORMS", "THUNDERSTORM WINDSS", "LIGHTNING THUNDERSTORM WINDS", "LIGHTNING AND THUNDERSTORM WIN", "THUNDERSTORM WINDS53", "THUNDERSTORM WINDS 13", "THUNDERSTORM  WINDS", "THUNDERSTORMS WIND", "THUNDERSTORM WIND/LIGHTNING", "THUNDERSTORM", "THUNDERSTORM WIND G50", "THUNDERSTORM WINDS.", "THUNDERSTORM WIND G55", "THUNDERSTORM WIND G60", "THUNDERSTORM WINDS G60", "THUNDERSTORM WIND 65MPH", "THUNDERSTORM WIND/ TREES", "THUNDERSTORM WIND/AWNING", "THUNDERSTORM WIND 98 MPH", "THUNDERSTORM WIND TREES", "THUNDERSTORM WIND 60 MPH", "THUNDERSTORM WINDS 63 MPH", "THUNDERSTORM WIND/ TREE", "THUNDERSTORM DAMAGE TO", "THUNDERSTORM WIND 65 MPH", "THUNDERSTORM WIND.", "THUNDERSTORM WINDS AND", "THUNDERSTORM WIND G52", "THUNDERSTORMW", "THUNDERSTORMWINDS", "THUNDERSTORM WIND (G40)", "MARINE THUNDERSTORM WIND", "TSTM WIND", "TSTM WIND 55", "TSTM WIND G58", "TSTM WIND DAMAGE", "TSTM WINDS", "TSTMW", "TSTM WIND 65)", "Tstm Wind", "TSTM WIND (G45)", "TSTM WIND 40", "TSTM WIND 45", "TSTM WIND (41)", "TSTM WIND (G40)", "TSTM WIND AND LIGHTNING", " TSTM WIND (G45)", "TSTM WIND  (G45)", "TSTM WIND (G35)", " TSTM WIND", "TSTM WIND G45", "MARINE TSTM WIND", "LIGHTNING", "LIGHTNING AND HEAVY RAIN", "HEAVY RAIN/LIGHTNING", "LIGHTNING/HEAVY RAIN", "LIGHTNING INJURY", "LIGHTNING.", "LIGHTNING  WAUSEON", "LIGHTING", "THUNDERTORM WINDS", "THUDERSTORM WINDS", "THUNDERESTORM WINDS", "THUNDEERSTORM WINDS", "THUNERSTORM WINDS", "LIGNTNING", "THUNDERSTROM WIND", "TUNDERSTORM WIND", "HEAVY RAIN/SEVERE WEATHER")

## factor names for tornado related factors, including waterspouts dust devils, and whirlwinds
TornadoFactors <- c("TORNADO", "TORNADO F0", "WATERSPOUT/TORNADO", "WATERSPOUT TORNADO", "TORNADOES, TSTM WIND, HAIL", "WATERSPOUT-TORNADO", "COLD AIR TORNADO", "WATERSPOUT/ TORNADO", "TORNADO F3", "TORNADO F1", "TORNADO F2", "TORNADOES", "FUNNEL CLOUD", "WATERSPOUT", "WATERSPOUT-", "DUST DEVIL WATERSPOUT", "DUST DEVIL", "Dust Devil", "Whirlwind", "WHIRLWIND", "GUSTNADO", "TORNDAO", "LANDSPOUT")

## factor names for heat related factors
HeatFactors <- c("HEAT", "EXCESSIVE HEAT", "HEAT WAVE", "EXTREME HEAT", "DROUGHT/EXCESSIVE HEAT", "RECORD HEAT", "HEAT WAVE DROUGHT", "HEAT WAVES", "RECORD/EXCESSIVE HEAT", "Heat Wave", "UNSEASONABLY WARM", "UNSEASONABLY WARM AND DRY", "WARM WEATHER", "HYPERTHERMIA/EXPOSURE")

## factor names for flood related factors. 
FloodFactors <- c("ICE STORM/FLASH FLOOD", "FLASH FLOODING", "FLASH FLOOD", "FLASH FLOODING/THUNDERSTORM WI", "FLOODING", "BREAKUP FLOODING", "RIVER FLOOD", "COASTAL FLOOD", "FLOOD", "FLOODING/HEAVY RAIN", "HEAVY SURF COASTAL FLOODING", "FLOOD/FLASH FLOOD", "FLOOD/RAIN/WINDS", "FLASH FLOOD WINDS", "FLASH FLOOD/", "URBAN FLOOD", "COASTAL FLOODING", "URBAN/SMALL STREAM FLOOD", "URBAN FLOODING", "MINOR FLOODING", "FLASH FLOODS", "FLOODS", "RURAL FLOOD", "MAJOR FLOOD", "ICE JAM FLOODING", "SMALL STREAM FLOOD", "LAKE FLOOD", "URBAN AND SMALL STREAM FLOODIN", "RIVER AND STREAM FLOOD", "HIGH WINDS/COASTAL FLOOD", "RIVER FLOODING", "FLOOD/RIVER FLOOD", "MUD SLIDES URBAN FLOODING", "HEAVY SNOW/HIGH WINDS & FLOOD", "HEAVY RAIN AND FLOOD", "FLASH FLOOD FROM ICE JAMS", "FLASH FLOOD - HEAVY RAIN", "FLASH FLOOD/ STREET", "FLOOD FLASH", "FLASH FLOOD/FLOOD", "FLOOD/FLASH", "HEAVY RAINS/FLOODING", "THUNDERSTORM WINDS/FLOODING", "SNOWMELT FLOODING", "FLASH FLOODING/FLOOD", "THUNDERSTORM WINDS/ FLOOD", "FLOOD & HEAVY RAIN", "FLOOD/FLASHFLOOD", "URBAN FLOODS", "FLASH FLOOD/LANDSLIDE", "FLASH FLOOD LANDSLIDES", "Ice jam flood (minor", "Erosion/Cstl Flood", "River Flooding", "Coastal Flooding", "TIDAL FLOODING", "Tidal Flooding", "Coastal Flood", "COASTAL FLOODING/EROSION", "COASTAL  FLOODING/EROSION", "FLOOD/FLASH/FLOOD", " FLASH FLOOD", "LAKESHORE FLOOD", "DROWNING", "EXCESSIVE RAINFALL", "RAPIDLY RISING WATER", "Torrential Rainfall", "HIGH WATER", "URBAN/SML STREAM FLD", "DAM BREAK", "URBAN AND SMALL", "URBAN SMALL", "URBAN/SMALL STREAM")

## factor names for hurricane related factors
HurricaneFactors <-  c("HURRICANE OPAL/HIGH WINDS", "HURRICANE ERIN", "HURRICANE OPAL", "HURRICANE", "HURRICANE-GENERATED SWELLS", "HURRICANE EMILY", "HURRICANE GORDON", "HURRICANE FELIX", "Hurricane Edouard", "HURRICANE/TYPHOON", "TYPHOON", "TROPICAL STORM ALBERTO", "TROPICAL STORM", "TROPICAL STORM GORDON", "TROPICAL STORM JERRY", "TROPICAL STORM DEAN", "TROPICAL DEPRESSION", "TSUNAMI")

## factor names for hail related factors
HailFactors <- c("HAIL", "THUNDERSTORM WINDS/HAIL", "THUNDERSTORM WINDS HAIL", "HAIL/WINDS", "HAIL/WIND", "WIND/HAIL", "HAIL 150", "HAIL 075", "HAIL 100", "HAIL 125", "HAIL 200", "HAIL 0.75", "THUNDERSTORM HAIL", "THUNDERSTORM WINDSHAIL", "HAIL 75", "THUNDERSTORM WIND/HAIL", "HAIL 175", "HAIL 275", "HAIL 450", "HAILSTORM", "HAIL DAMAGE", "TSTM WIND/HAIL", "SMALL HAIL", "GUSTY WIND/HAIL", "MARINE HAIL")

## factor names for winter storm related factors, including ice, sleet, snow, mixed precip, icy roads, and freezing rain
WinterStormFactors <- c("WINTER STORM", "WINTER STORM HIGH WINDS", "WINTER STORMS", "WINTER WEATHER", "HEAVY SNOW/WINTER STORM", "BLIZZARD/WINTER STORM", "Wintry Mix", "WINTRY MIX", "WINTER WEATHER MIX", "WINTER WEATHER/MIX", "HEAVY SNOW", "HEAVY SNOW/WIND", "HEAVY SNOWPACK", "SNOW", "FREEZING RAIN/SNOW", "THUNDERSNOW", "HEAVY RAIN/SNOW", "SNOW AND HEAVY SNOW", "SNOW/HEAVY SNOW", "SNOW AND ICE", "SNOW AND ICE STORM", "HEAVY LAKE SNOW", "HEAVY SNOW/FREEZING RAIN", "HEAVY SNOW/WINTER STORM", "HEAVY SNOW AND HIGH WINDS", "SNOW/COLD", "HEAVY SNOW SQUALLS", "SNOW SQUALL", "SNOW/ICE STORM", "HEAVY SNOW/SQUALLS", "HEAVY SNOW-SQUALLS", "SNOW FREEZING RAIN", "SNOW/SLEET", "SNOW/FREEZING RAIN", "SNOW SQUALLS", "SNOW/SLEET/FREEZING RAIN", "RECORD SNOW", "BLOWING SNOW", "HEAVY SNOW/BLIZZARD", "ICE AND SNOW", "HEAVY SNOW/ICE", "HIGH WIND/HEAVY SNOW", "SNOW/ICE", "HEAVY SNOW/BLIZZARD/AVALANCHE", "SNOW/ BITTER COLD", "SNOW/HIGH WINDS", "HIGH WINDS/SNOW", "HEAVY SNOW AND STRONG WINDS", "SNOW ACCUMULATION", "SNOW/ ICE", "SNOW/BLOWING SNOW", "Heavy snow shower", "Light snow", "Light Snow", "Snow Squalls", "Light Snowfall", "Snow", "COLD AND SNOW", "RAIN/SNOW", "Lake Effect Snow", "LIGHT SNOW", "blowing snow", "EXCESSIVE SNOW", "LAKE EFFECT SNOW", "LATE SEASON SNOW", "FALLING SNOW/ICE", "LAKE-EFFECT SNOW", "ICE STORM", "GLAZE ICE", "SLEET/ICE STORM", "ICE", "ICE FLOES", "GLAZE/ICE STORM", "ICE JAM", "ICE/STRONG WINDS", "BLACK ICE", "ICE ROADS", "ICE ON ROAD", "SLEET", "FREEZING RAIN/SLEET", "FREEZING RAIN", "FREEZING DRIZZLE", "Freezing Spray", "Freezing Drizzle",  "Freezing Rain", "Freezing drizzle", "LIGHT FREEZING RAIN", "FREEZING FOG", "BLIZZARD", "GROUND BLIZZARD", "HIGH WIND/BLIZZARD", "GLAZE", "Glaze", "MIXED PRECIP", "ICY ROADS", "Mixed Precipitation", "MIXED PRECIPITATION", "HEAVY MIX")

## factor names for wind related factors.
## does not include thunderstorms, hail, cold
## does include rain/wind factors, microbursts
WindFactors <- c("HIGH WINDS", "WIND", "HIGH WIND", "HIGH WINDS HEAVY RAINS", "HIGH WIND/SEAS", "HIGH WINDS/HEAVY RAIN", "GUSTY WINDS", "WINDS", "HIGH WIND DAMAGE", "STRONG WIND", "DRY MIRCOBURST WINDS", "MICROBURST WINDS", "STRONG WINDS", "HIGH WINDS/", "HIGH  WINDS", "WIND DAMAGE", "WIND STORM", "DUST STORM/HIGH WINDS", "STORM FORCE WINDS", "RAIN/WIND", "HIGH WIND AND SEAS", "HIGH WIND 48", "Strong Wind", "Strong Winds", "Wind Damage", "Gusty wind/rain", "GUSTY WIND/HVY RAIN", "Wind", "Gusty Winds", "GUSTY WIND", "Gusty winds", "GRADIENT WIND", "gradient wind", "Gradient wind", "Heavy surf and wind", "HIGH WIND (G40)", "WIND AND WAVE", "NON-SEVERE WIND DAMAGE", "NON-TSTM WIND", "NON TSTM WIND", "MARINE HIGH WIND", 
"MARINE STRONG WIND", "SEVERE TURBULENCE", "MICROBURST", "DRY MICROBURST", "WET MICROBURST", "Microburst", "DOWNBURST")

## factor names for fire-related factors
FireFactors <- c("WILD FIRES", "WILDFIRE", "GRASS FIRES", "LIGHTNING FIRE", "FOREST FIRES", "WILDFIRES", "WILD/FOREST FIRE", "WILD/FOREST FIRES", "BRUSH FIRE")

## factor names for cold-related factors, including frost, freeze, hypothermia
ColdFactors <- c("COLD", "EXTREME COLD", "COLD AND WET CONDITIONS", "RECORD COLD", "COLD WAVE", "FOG AND COLD TEMPERATURES", "UNSEASONABLY COLD", "HIGH WINDS/COLD", "COLD/WINDS", "COLD WEATHER", "Unseasonable Cold", "Extreme Cold", "Extended Cold", "Cold", "Cold Temperature", "EXTREME COLD/WIND CHILL", "COLD/WIND CHILL","EXTREME WIND CHILL", "EXTREME WINDCHILL", "LOW TEMPERATURE", "FROST", "FROST\\FREEZE", "Early Frost", "Frost/Freeze", "FROST/FREEZE", "FREEZE", "DAMAGING FREEZE", "FROST\\FREEZE", "HARD FREEZE", "Freeze", "Damaging Freeze", "Frost/Freeze", "AGRICULTURAL FREEZE", "HYPOTHERMIA", "Hypothermia/Exposure", "HYPOTHERMIA/EXPOSURE")

## factor names for fog-related factors. Does not include cold weather
## includes dense smoke
FogFactors <- c("DENSE FOG", "FOG", "DENSE SMOKE")

## factor names for marine weather related factors.
## includes rip currents, heavy surf, coastal storm, storm surge, coastal erosion, low and high tides
## does not include hurricanes, tropical storms
MarineFactors <- c("RIP CURRENT", "RIP CURRENTS", "HIGH SURF", "HEAVY SURF/HIGH SURF", "STORM SURGE", "HEAVY SURF", "STORM SURGE/TIDE", "HIGH SEAS", "ROUGH SEAS", "MARINE MISHAP", "High Surf", "RIP CURRENTS/HEAVY SURF", "ROUGH SURF", "COASTAL STORM", "Marine Accident", "HEAVY SEAS", "ROGUE WAVE", "Coastal Storm", "COASTALSTORM", "HAZARDOUS SURF", "Heavy Surf", "HIGH SWELLS", "HIGH WAVES", "COASTAL SURGE", "HEAVY SWELLS", "Heavy Rain/High Surf", "COASTAL EROSION", "   HIGH SURF ADVISORY", "Beach Erosion", "ASTRONOMICAL HIGH TIDE", "ASTRONOMICAL LOW TIDE", "HIGH TIDES", "SEICHE")

## factor names for heavy rain related factors, not including thunderstorms
RainFactors <- c("HEAVY RAIN", "HEAVY RAINS", "RECORD RAINFALL", "RAINSTORM", "COOL AND WET", "HVY RAIN", "RAIN", "EXCESSIVE WETNESS", "HEAVY SHOWER", "HEAVY RAIN/SMALL STREAM URBAN", "UNSEASONAL RAIN", "HEAVY PRECIPITATION")

## factor names for landslide related factors, including mudslides, avalanches, and rocklides
LandslideFactors <- c("LANDSLIDE", "AVALANCHE", "MUDSLIDES", "MUD SLIDES", "MUDSLIDE", "LANDSLIDES", "Landslump", "Mudslides", "Mudslide", "ROCK SLIDE", "AVALANCE", "MUD SLIDE")

## factor names for dry related factors. 
## includes volcanic ash, drought
DryFactors <- c("DUST STORM", "BLOWING DUST", "VOLCANIC ASH", "DROUGHT")

## factor names that don't fit into any other category
OtherFactors <- c("?", "OTHER", "Other", "HIGH", "APACHE COUNTY")

stormData$EVTYPE <- stormData$EVTYPE %>%
        fct_collapse(
                TSTORM = TStormFactors,
                TORNADO = TornadoFactors,
                HEAT = HeatFactors,
                FLOOD = FloodFactors,
                TROPSTORM = HurricaneFactors,
                WINTERSTORM = WinterStormFactors,
                WIND = WindFactors,
                FIRE = FireFactors,
                HAIL = HailFactors,
                COLD = ColdFactors,
                FOG = FogFactors, 
                MARINE = MarineFactors,
                RAIN = RainFactors, 
                LANDSLIDE = LandslideFactors,
                DRY = DryFactors,
                OTHER = OtherFactors
                     ) 

popHealth <- stormData %>%
        filter(FATALITIES > 0 | INJURIES >0) %>%
        group_by(EVTYPE) %>%
        summarize(FATALITIES = sum(FATALITIES), INJURIES = sum(INJURIES))

popHealth$EVTYPE <- popHealth$EVTYPE %>% 
        fct_drop

popHealthTidy <- popHealth %>%
        gather(CATEGORY, VALUE, FATALITIES:INJURIES)

g1 <- ggplot(popHealthTidy, aes(x = fct_reorder(EVTYPE, VALUE, sum, .desc = TRUE), y = VALUE, fill = CATEGORY)) +
        geom_col(position = "stack") + 
        theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust = 0.5))

econ <- stormData %>%
        filter(PROPDMG > 0 | CROPDMG >0) %>%
        group_by(EVTYPE) %>%
        summarize(PropertyDamage = sum(PROPDMG), CropDamage = sum(CROPDMG))

econ$EVTYPE <- econ$EVTYPE %>% 
        fct_drop

econTidy <- econ %>%
        gather(CATEGORY, VALUE, PropertyDamage:CropDamage)

g2 <- ggplot(econTidy, aes(x = fct_reorder(EVTYPE, VALUE, sum, .desc = TRUE), y = VALUE, fill = CATEGORY)) +
        geom_col(position = "stack") + 
        theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust = 0.5))

popHealthSummary <- popHealth %>%
        mutate(CASUALTIES = FATALITIES + INJURIES) %>%
        select(EVTYPE, CASUALTIES)

econSummary <- econ %>%
        mutate(TotalDamage = PropertyDamage + CropDamage) %>%
        select(EVTYPE, TotalDamage)