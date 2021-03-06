# Reproducible Research: Peer Assessment 2


## Data Processing
First, we load some libraries we will be using for this analysis


```r
library(dplyr)
library(lubridate)
library(tidyr)
library(forcats)
library(ggplot2)
```

We next load the data into R, assuming the data file is in the same directory as this document.


```r
stormDataFilename <- "stormData.csv.bz2"
stormDataInit <- read.csv(stormDataFilename)
```

After reading in the data, we take a look at the first few rows of the dataset (there are 902297).

```r
dim(stormDataInit)
```

```
## [1] 902297     37
```


```r
stormDataInit %>% tbl_df %>% head
```

```
## # A tibble: 6 × 37
##   STATE__           BGN_DATE BGN_TIME TIME_ZONE COUNTY COUNTYNAME  STATE
##     <dbl>             <fctr>   <fctr>    <fctr>  <dbl>     <fctr> <fctr>
## 1       1  4/18/1950 0:00:00     0130       CST     97     MOBILE     AL
## 2       1  4/18/1950 0:00:00     0145       CST      3    BALDWIN     AL
## 3       1  2/20/1951 0:00:00     1600       CST     57    FAYETTE     AL
## 4       1   6/8/1951 0:00:00     0900       CST     89    MADISON     AL
## 5       1 11/15/1951 0:00:00     1500       CST     43    CULLMAN     AL
## 6       1 11/15/1951 0:00:00     2000       CST     77 LAUDERDALE     AL
## # ... with 30 more variables: EVTYPE <fctr>, BGN_RANGE <dbl>,
## #   BGN_AZI <fctr>, BGN_LOCATI <fctr>, END_DATE <fctr>, END_TIME <fctr>,
## #   COUNTY_END <dbl>, COUNTYENDN <lgl>, END_RANGE <dbl>, END_AZI <fctr>,
## #   END_LOCATI <fctr>, LENGTH <dbl>, WIDTH <dbl>, F <int>, MAG <dbl>,
## #   FATALITIES <dbl>, INJURIES <dbl>, PROPDMG <dbl>, PROPDMGEXP <fctr>,
## #   CROPDMG <dbl>, CROPDMGEXP <fctr>, WFO <fctr>, STATEOFFIC <fctr>,
## #   ZONENAMES <fctr>, LATITUDE <dbl>, LONGITUDE <dbl>, LATITUDE_E <dbl>,
## #   LONGITUDE_ <dbl>, REMARKS <fctr>, REFNUM <dbl>
```

For this analysis, we are answering questions about population health and economic consequences of weather, so we extract only the data we need for this analysis, namely data where  fatalities, injuries, property damage, or crop damage are strictly greater than 0. We select columns correponding to this data, along with the event type.


```r
stormData <- stormDataInit %>%
        tbl_df %>%
        mutate(Year = year(mdy_hms(BGN_DATE))) %>%
        select(
                Year,
                EventType = EVTYPE, 
                Fatalities = FATALITIES, 
                Injuries = INJURIES, 
                PropertyDamage = PROPDMG, 
                PROPDMGEXP, 
                CropDamage = CROPDMG, 
                CROPDMGEXP
                ) %>%
        filter(
                Fatalities > 0 | 
                Injuries > 0 | 
                PropertyDamage > 0 | 
                CropDamage > 0
                )
```

The variables `PROPDMGEXP` and `CROPDMGEXP` give us a multiplicative factor for dollars of damage caused by a weather event. We take "H" to mean hundred, "K" thousand, "M" million, "B" billion, and numeric values to mean a scientific notation exponents (i.e. "2" corresponds to a multiplicative factor of 10^2), we prepare the columns PROPDMGEXP and CROPDMGEXP for numerical calculation. We use the `fct_collapse` function from the `forcats` package to make these columns uniform by making them all scientific notation exponents. For rows missing data in these columns, we assume the value is 0. Finally, we transform this into numeric data.


```r
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
```

Now we compute the dollar values of the `PropertyDamage` and `CropDamage` columns, and get rid of the `PROPDMGEXP` and `CROPDMGEXP` columns.


```r
stormData <- stormData %>% 
        mutate(PropertyDamage = PropertyDamage*(10^PROPDMGEXP), CropDamage =  CropDamage*(10^CROPDMGEXP)) %>%
        select(-PROPDMGEXP, -CROPDMGEXP)
```

We now want to get an idea of common types of weather events listed.


```r
table(stormData$EventType) %>% sort(decreasing = TRUE) %>% head(10)
```

```
## 
##          TSTM WIND  THUNDERSTORM WIND            TORNADO 
##              63234              43655              39944 
##               HAIL        FLASH FLOOD          LIGHTNING 
##              26130              20967              13293 
## THUNDERSTORM WINDS              FLOOD          HIGH WIND 
##              12086              10175               5522 
##        STRONG WIND 
##               3370
```

We immediately notice a problem: the values of `EventType` may use different names to describe the same type of event (notice, for instance, the top two event: `TSTM WIND` and `THUNDERSTORM WIND`). To remedy this, We first define variables that group together names of similar factors.


```r
## factor names for tornado related factors, including waterspouts, dust devils, and whirlwinds
TornadoFactors <- c("TORNADO", "TORNADO F0", "WATERSPOUT/TORNADO", "WATERSPOUT TORNADO", "TORNADOES, TSTM WIND, HAIL", "WATERSPOUT-TORNADO", "COLD AIR TORNADO", "WATERSPOUT/ TORNADO", "TORNADO F3", "TORNADO F1", "TORNADO F2", "TORNADOES", "FUNNEL CLOUD", "WATERSPOUT", "WATERSPOUT-", "DUST DEVIL WATERSPOUT", "DUST DEVIL", "Dust Devil", "Whirlwind", "WHIRLWIND", "GUSTNADO", "TORNDAO", "LANDSPOUT")

## factor names for flood related factors. 
FloodFactors <- c("ICE STORM/FLASH FLOOD", "FLASH FLOODING", "FLASH FLOOD", "FLASH FLOODING/THUNDERSTORM WI", "FLOODING", "BREAKUP FLOODING", "RIVER FLOOD", "COASTAL FLOOD", "FLOOD", "FLOODING/HEAVY RAIN", "HEAVY SURF COASTAL FLOODING", "FLOOD/FLASH FLOOD", "FLOOD/RAIN/WINDS", "FLASH FLOOD WINDS", "FLASH FLOOD/", "URBAN FLOOD", "COASTAL FLOODING", "URBAN/SMALL STREAM FLOOD", "URBAN FLOODING", "MINOR FLOODING", "FLASH FLOODS", "FLOODS", "RURAL FLOOD", "MAJOR FLOOD", "ICE JAM FLOODING", "SMALL STREAM FLOOD", "LAKE FLOOD", "URBAN AND SMALL STREAM FLOODIN", "RIVER AND STREAM FLOOD", "HIGH WINDS/COASTAL FLOOD", "RIVER FLOODING", "FLOOD/RIVER FLOOD", "MUD SLIDES URBAN FLOODING", "HEAVY SNOW/HIGH WINDS & FLOOD", "HEAVY RAIN AND FLOOD", "FLASH FLOOD FROM ICE JAMS", "FLASH FLOOD - HEAVY RAIN", "FLASH FLOOD/ STREET", "FLOOD FLASH", "FLASH FLOOD/FLOOD", "FLOOD/FLASH", "HEAVY RAINS/FLOODING", "THUNDERSTORM WINDS/FLOODING", "SNOWMELT FLOODING", "FLASH FLOODING/FLOOD", "THUNDERSTORM WINDS/ FLOOD", "FLOOD & HEAVY RAIN", "FLOOD/FLASHFLOOD", "URBAN FLOODS", "FLASH FLOOD/LANDSLIDE", "FLASH FLOOD LANDSLIDES", "Ice jam flood (minor", "Erosion/Cstl Flood", "River Flooding", "Coastal Flooding", "TIDAL FLOODING", "Tidal Flooding", "Coastal Flood", "COASTAL FLOODING/EROSION", "COASTAL  FLOODING/EROSION", "FLOOD/FLASH/FLOOD", " FLASH FLOOD", "LAKESHORE FLOOD", "DROWNING", "EXCESSIVE RAINFALL", "RAPIDLY RISING WATER", "Torrential Rainfall", "HIGH WATER", "URBAN/SML STREAM FLD", "DAM BREAK", "URBAN AND SMALL", "URBAN SMALL", "URBAN/SMALL STREAM")

## factor names for hail related factors
HailFactors <- c("HAIL", "THUNDERSTORM WINDS/HAIL", "THUNDERSTORM WINDS HAIL", "HAIL/WINDS", "HAIL/WIND", "WIND/HAIL", "HAIL 150", "HAIL 075", "HAIL 100", "HAIL 125", "HAIL 200", "HAIL 0.75", "THUNDERSTORM HAIL", "THUNDERSTORM WINDSHAIL", "HAIL 75", "THUNDERSTORM WIND/HAIL", "HAIL 175", "HAIL 275", "HAIL 450", "HAILSTORM", "HAIL DAMAGE", "TSTM WIND/HAIL", "SMALL HAIL", "GUSTY WIND/HAIL", "MARINE HAIL")

## Factor names for Thunderstorm related factors including wind and lightning.
## Does not include tornadoes, floods, or hail
TStormFactors <- c("THUNDERSTORM WINDS", "THUNDERSTORM WIND", "THUNDERSTORM WINS", "THUNDERSTORM WINDS LIGHTNING", "THUNDERSTORM WINDS/FUNNEL CLOU", "SEVERE THUNDERSTORM", "SEVERE THUNDERSTORMS", "SEVERE THUNDERSTORM WINDS", "THUNDERSTORMS WINDS", "THUNDERSTORMS", "THUNDERSTORM WINDSS", "LIGHTNING THUNDERSTORM WINDS", "LIGHTNING AND THUNDERSTORM WIN", "THUNDERSTORM WINDS53", "THUNDERSTORM WINDS 13", "THUNDERSTORM  WINDS", "THUNDERSTORMS WIND", "THUNDERSTORM WIND/LIGHTNING", "THUNDERSTORM", "THUNDERSTORM WIND G50", "THUNDERSTORM WINDS.", "THUNDERSTORM WIND G55", "THUNDERSTORM WIND G60", "THUNDERSTORM WINDS G60", "THUNDERSTORM WIND 65MPH", "THUNDERSTORM WIND/ TREES", "THUNDERSTORM WIND/AWNING", "THUNDERSTORM WIND 98 MPH", "THUNDERSTORM WIND TREES", "THUNDERSTORM WIND 60 MPH", "THUNDERSTORM WINDS 63 MPH", "THUNDERSTORM WIND/ TREE", "THUNDERSTORM DAMAGE TO", "THUNDERSTORM WIND 65 MPH", "THUNDERSTORM WIND.", "THUNDERSTORM WINDS AND", "THUNDERSTORM WIND G52", "THUNDERSTORMW", "THUNDERSTORMWINDS", "THUNDERSTORM WIND (G40)", "MARINE THUNDERSTORM WIND", "TSTM WIND", "TSTM WIND 55", "TSTM WIND G58", "TSTM WIND DAMAGE", "TSTM WINDS", "TSTMW", "TSTM WIND 65)", "Tstm Wind", "TSTM WIND (G45)", "TSTM WIND 40", "TSTM WIND 45", "TSTM WIND (41)", "TSTM WIND (G40)", "TSTM WIND AND LIGHTNING", " TSTM WIND (G45)", "TSTM WIND  (G45)", "TSTM WIND (G35)", " TSTM WIND", "TSTM WIND G45", "MARINE TSTM WIND", "LIGHTNING", "LIGHTNING AND HEAVY RAIN", "HEAVY RAIN/LIGHTNING", "LIGHTNING/HEAVY RAIN", "LIGHTNING INJURY", "LIGHTNING.", "LIGHTNING  WAUSEON", "LIGHTING", "THUNDERTORM WINDS", "THUDERSTORM WINDS", "THUNDERESTORM WINDS", "THUNDEERSTORM WINDS", "THUNERSTORM WINDS", "LIGNTNING", "THUNDERSTROM WIND", "TUNDERSTORM WIND", "HEAVY RAIN/SEVERE WEATHER")

## factor names for heat related factors
HeatFactors <- c("HEAT", "EXCESSIVE HEAT", "HEAT WAVE", "EXTREME HEAT", "DROUGHT/EXCESSIVE HEAT", "RECORD HEAT", "HEAT WAVE DROUGHT", "HEAT WAVES", "RECORD/EXCESSIVE HEAT", "Heat Wave", "UNSEASONABLY WARM", "UNSEASONABLY WARM AND DRY", "WARM WEATHER", "HYPERTHERMIA/EXPOSURE")

## factor names for cold-related factors, including frost, freeze, hypothermia
## does not include snow, winter storms, or avalanches.
ColdFactors <- c("COLD", "EXTREME COLD", "COLD AND WET CONDITIONS", "RECORD COLD", "COLD WAVE", "FOG AND COLD TEMPERATURES", "UNSEASONABLY COLD", "HIGH WINDS/COLD", "COLD/WINDS", "COLD WEATHER", "Unseasonable Cold", "Extreme Cold", "Extended Cold", "Cold", "Cold Temperature", "EXTREME COLD/WIND CHILL", "COLD/WIND CHILL","EXTREME WIND CHILL", "EXTREME WINDCHILL", "LOW TEMPERATURE", "FROST", "FROST\\FREEZE", "Early Frost", "Frost/Freeze", "FROST/FREEZE", "FREEZE", "DAMAGING FREEZE", "FROST\\FREEZE", "HARD FREEZE", "Freeze", "Damaging Freeze", "Frost/Freeze", "AGRICULTURAL FREEZE", "HYPOTHERMIA", "Hypothermia/Exposure", "HYPOTHERMIA/EXPOSURE")

## factor names for hurricane related factors, including tropical storms, typhoons, tropical depressions, tsunamis
HurricaneFactors <-  c("HURRICANE OPAL/HIGH WINDS", "HURRICANE ERIN", "HURRICANE OPAL", "HURRICANE", "HURRICANE-GENERATED SWELLS", "HURRICANE EMILY", "HURRICANE GORDON", "HURRICANE FELIX", "Hurricane Edouard", "HURRICANE/TYPHOON", "TYPHOON", "TROPICAL STORM ALBERTO", "TROPICAL STORM", "TROPICAL STORM GORDON", "TROPICAL STORM JERRY", "TROPICAL STORM DEAN", "TROPICAL DEPRESSION", "TSUNAMI")

## factor names for winter storm related factors, including ice, sleet, snow, mixed precip, icy roads, and freezing rain
WinterStormFactors <- c("WINTER STORM", "WINTER STORM HIGH WINDS", "WINTER STORMS", "WINTER WEATHER", "HEAVY SNOW/WINTER STORM", "BLIZZARD/WINTER STORM", "Wintry Mix", "WINTRY MIX", "WINTER WEATHER MIX", "WINTER WEATHER/MIX", "HEAVY SNOW", "HEAVY SNOW/WIND", "HEAVY SNOWPACK", "SNOW", "FREEZING RAIN/SNOW", "THUNDERSNOW", "HEAVY RAIN/SNOW", "SNOW AND HEAVY SNOW", "SNOW/HEAVY SNOW", "SNOW AND ICE", "SNOW AND ICE STORM", "HEAVY LAKE SNOW", "HEAVY SNOW/FREEZING RAIN", "HEAVY SNOW/WINTER STORM", "HEAVY SNOW AND HIGH WINDS", "SNOW/COLD", "HEAVY SNOW SQUALLS", "SNOW SQUALL", "SNOW/ICE STORM", "HEAVY SNOW/SQUALLS", "HEAVY SNOW-SQUALLS", "SNOW FREEZING RAIN", "SNOW/SLEET", "SNOW/FREEZING RAIN", "SNOW SQUALLS", "SNOW/SLEET/FREEZING RAIN", "RECORD SNOW", "BLOWING SNOW", "HEAVY SNOW/BLIZZARD", "ICE AND SNOW", "HEAVY SNOW/ICE", "HIGH WIND/HEAVY SNOW", "SNOW/ICE", "HEAVY SNOW/BLIZZARD/AVALANCHE", "SNOW/ BITTER COLD", "SNOW/HIGH WINDS", "HIGH WINDS/SNOW", "HEAVY SNOW AND STRONG WINDS", "SNOW ACCUMULATION", "SNOW/ ICE", "SNOW/BLOWING SNOW", "Heavy snow shower", "Light snow", "Light Snow", "Snow Squalls", "Light Snowfall", "Snow", "COLD AND SNOW", "RAIN/SNOW", "Lake Effect Snow", "LIGHT SNOW", "blowing snow", "EXCESSIVE SNOW", "LAKE EFFECT SNOW", "LATE SEASON SNOW", "FALLING SNOW/ICE", "LAKE-EFFECT SNOW", "ICE STORM", "GLAZE ICE", "SLEET/ICE STORM", "ICE", "ICE FLOES", "GLAZE/ICE STORM", "ICE JAM", "ICE/STRONG WINDS", "BLACK ICE", "ICE ROADS", "ICE ON ROAD", "SLEET", "FREEZING RAIN/SLEET", "FREEZING RAIN", "FREEZING DRIZZLE", "Freezing Spray", "Freezing Drizzle",  "Freezing Rain", "Freezing drizzle", "LIGHT FREEZING RAIN", "FREEZING FOG", "BLIZZARD", "GROUND BLIZZARD", "HIGH WIND/BLIZZARD", "GLAZE", "Glaze", "MIXED PRECIP", "ICY ROADS", "Mixed Precipitation", "MIXED PRECIPITATION", "HEAVY MIX")

## factor names for wind related factors.
## does not include thunderstorms, hail, cold
## does include rain/wind factors, microbursts
WindFactors <- c("HIGH WINDS", "WIND", "HIGH WIND", "HIGH WINDS HEAVY RAINS", "HIGH WIND/SEAS", "HIGH WINDS/HEAVY RAIN", "GUSTY WINDS", "WINDS", "HIGH WIND DAMAGE", "STRONG WIND", "DRY MIRCOBURST WINDS", "MICROBURST WINDS", "STRONG WINDS", "HIGH WINDS/", "HIGH  WINDS", "WIND DAMAGE", "WIND STORM", "DUST STORM/HIGH WINDS", "STORM FORCE WINDS", "RAIN/WIND", "HIGH WIND AND SEAS", "HIGH WIND 48", "Strong Wind", "Strong Winds", "Wind Damage", "Gusty wind/rain", "GUSTY WIND/HVY RAIN", "Wind", "Gusty Winds", "GUSTY WIND", "Gusty winds", "GRADIENT WIND", "gradient wind", "Gradient wind", "Heavy surf and wind", "HIGH WIND (G40)", "WIND AND WAVE", "NON-SEVERE WIND DAMAGE", "NON-TSTM WIND", "NON TSTM WIND", "MARINE HIGH WIND", 
"MARINE STRONG WIND", "SEVERE TURBULENCE", "MICROBURST", "DRY MICROBURST", "WET MICROBURST", "Microburst", "DOWNBURST")

## factor names for fire-related factors
FireFactors <- c("WILD FIRES", "WILDFIRE", "GRASS FIRES", "LIGHTNING FIRE", "FOREST FIRES", "WILDFIRES", "WILD/FOREST FIRE", "WILD/FOREST FIRES", "BRUSH FIRE")

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

## factor names that don't readily fit into any other category
OtherFactors <- c("?", "OTHER", "Other", "HIGH", "APACHE COUNTY")
```

We use the function `fct_collapse` from the `forcats` package to consolidate our `EventType` factor vector.


```r
stormData$EventType <- fct_collapse(stormData$EventType,
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
```

Let's now take a look at a sample of our processed data.


```r
set.seed(10)
stormData %>% sample_n(10)
```

```
## # A tibble: 10 × 6
##     Year   EventType Fatalities Injuries PropertyDamage CropDamage
##    <dbl>      <fctr>      <dbl>    <dbl>          <dbl>      <dbl>
## 1   2002      TSTORM          0        0          20000          0
## 2   1998       FLOOD          1        2       31950000          0
## 3   2000 WINTERSTORM          0        0          30000          0
## 4   2007      TSTORM          0        0          10000          0
## 5   1989     TORNADO          0       12        2500000          0
## 6   1996      TSTORM          0        0           1000          0
## 7   1997      TSTORM          0        0           5000          0
## 8   1997        COLD          1        0              0          0
## 9   2005       FLOOD          0        0           2000          0
## 10  2000      TSTORM          0        0            500          0
```

## Results

We now summarize our data by finding the sum of all of our relevant categories for all years, 1950 to 2011.


```r
stormDataSummaryAll <- stormData %>% 
        group_by(EventType) %>%
        summarize(
                Fatalities = sum(Fatalities), 
                Injuries = sum(Injuries), 
                PropertyDamage = sum(PropertyDamage), 
                CropDamage = sum(CropDamage)
                )
stormDataSummaryAll
```

```
## # A tibble: 16 × 5
##      EventType Fatalities Injuries PropertyDamage  CropDamage
##         <fctr>      <dbl>    <dbl>          <dbl>       <dbl>
## 1       MARINE        789      842    48092088650     2355000
## 2        FLOOD       1560     8708   168272042535 12388597200
## 3       TSTORM       1542    14678    14574957106  1218950828
## 4        OTHER          0        5          65500     1034400
## 5         COLD        470      328      265534400  3430826500
## 6    LANDSLIDE        269      225      331117900    20017000
## 7  WINTERSTORM        672     6402    12439056511  5316281400
## 8          DRY         22      444     1052175000 13975666000
## 9         FIRE         90     1608     8501628500   403281630
## 10     TORNADO       5667    91482    58613917407   417463070
## 11        RAIN         98      255      712389190   951652800
## 12         FOG         80     1076       22929500           0
## 13        WIND        453     1920     6087049723   750840400
## 14        HEAT       3179     9243       20325750   904479280
## 15        HAIL         20     1467    16022991537  3111712873
## 16   TROPSTORM        234     1845    93216599560  6211033800
```

To see which type of weather events had the greatest effect on population health, we limit our view to fatalities and injuries. 


```r
popHealthAll <- stormDataSummaryAll %>% 
        select(EventType, Fatalities, Injuries) %>% 
        gather(Category, Value, Fatalities:Injuries)

g1 <- ggplot(popHealthAll, aes(x = fct_reorder(EventType, Value, sum, .desc = TRUE), y = Value, fill = Category)) 

g1 + geom_col(position = "stack") + theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust = 0.5))
```

![](stormDataAnalysis_files/figure-html/popHealthAll-1.png)<!-- -->


```r
econAll <- stormDataSummaryAll %>% 
        select(EventType, PropertyDamage, CropDamage) %>% 
        gather(Category, Value, PropertyDamage:CropDamage)

g2 <- ggplot(econAll, aes(x = fct_reorder(EventType, Value, sum, .desc = TRUE), y = Value, fill = Category)) 

g2 + geom_col(position = "stack") + theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust = 0.5))
```

![](stormDataAnalysis_files/figure-html/econAll-1.png)<!-- -->
