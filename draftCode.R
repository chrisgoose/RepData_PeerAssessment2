##Here are the data for the project:
url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"

if(!file.exists("repdata_data_StormData.csv.bz2")) {
        download.file(url, "repdata_data_StormData.csv.bz2", method="curl")
}

colnames(stormData)


stormData <- fread("repdata_data_StormData.csv.bz2",
                   select = c("REFNUM", "EVTYPE", "FATALITIES", "INJURIES", 
                              "PROPDMG", "PROPDMGEXP","CROPDMG", "CROPDMGEXP"))

#create lower dimension table
stormData2 <- stormData[, c("REFNUM", "EVTYPE", "FATALITIES", "INJURIES", 
                            "PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP")]

#change lowercase labels to uppercase, i.e., 'm' to 'M'
stormData2$PROPDMGEXP <- toupper(stormData2$PROPDMGEXP)
stormData2$CROPDMGEXP <- toupper(stormData2$CROPDMGEXP)

#change alpha & symbolic labels to numeric for exponents
stormData2[stormData2$PROPDMGEXP == "B","PROPDMGEXP"] <- 9
stormData2[stormData2$PROPDMGEXP == "M","PROPDMGEXP"] <- 6
stormData2[stormData2$PROPDMGEXP == "K","PROPDMGEXP"] <- 3
stormData2[stormData2$PROPDMGEXP == "H","PROPDMGEXP"] <- 2
stormData2[stormData2$PROPDMGEXP %in% c("+","-","?","_", ""),"PROPDMGEXP"] <- 0

stormData2[stormData2$CROPDMGEXP == "B","CROPDMGEXP"] <- 9
stormData2[stormData2$CROPDMGEXP == "M","CROPDMGEXP"] <- 6
stormData2[stormData2$CROPDMGEXP == "K","CROPDMGEXP"] <- 3
stormData2[stormData2$CROPDMGEXP == "H","CROPDMGEXP"] <- 2
stormData2[stormData2$CROPDMGEXP %in% c("+","-","?","_", ""),"CROPDMGEXP"] <- 0

unique(c(stormData2$PROPDMGEXP, stormData2$CROPDMGEXP))

stormData2$PROPDMG <- with(stormData2, PROPDMG * 10^as.numeric(PROPDMGEXP))
stormData2$CROPDMG <- with(stormData2, CROPDMG * 10^as.numeric(CROPDMGEXP))

stormData2 <- stormData2[, c("REFNUM", "EVTYPE", "FATALITIES", "INJURIES", 
                            "PROPDMG", "CROPDMG")]
names(stormData2)[1:6] <- c("id", "eventType", "fatalities", "injuries", 
                            "propertyDamage", "cropDamage")


###################################################
#make tidy fatalities data, then rank events by sum
fatality <- stormData2[, c("id", "eventType","fatalities")]
fatalitySum <- aggregate(fatality$fatalities, 
                         by = list(eventType = fatality$eventType), sum)
fatalitySum <- fatalitySum[order(fatalitySum$x, decreasing = TRUE), ]
head(fatalitySum, 15)

#tornado > all else
fatalitySum$x[[1]]
sum(fatalitySum[2:6,2])

#make barplot
require(ggplot2)
ggplot(fatalitySum[1:10, ], aes(x = reorder(eventType, -x), y = x)) + 
        geom_bar(stat = "identity") + xlab("Storm Event Type") + 
        ylab("Sum of Fatalities") + ggtitle("Fatalities by Event, 1950-2011") + 
        theme(axis.text.x = element_text(angle = 90))

#################################################
#make tidy injuries data, then rank events by sum
injury <- stormData2[, c("id", "eventType","injuries")]
injurySum <- aggregate(injury$injuries, by = list(eventType = injury$eventType), 
                       sum)
injurySum <- injurySum[order(injurySum$x, decreasing = TRUE), ]
head(injurySum, 15)

#make barplot
require(ggplot2)
ggplot(injurySum[1:10, ], aes(x = reorder(eventType, -x), y = x)) + 
        geom_bar(stat = "identity") + xlab("Storm Event Type") + 
        ylab("Sum of Fatalities") + ggtitle("Fatalities by Event, 1950-2011") + 
        theme(axis.text.x = element_text(angle = 90))

#tornado > all else
injurySum$x[[1]]
sum(injurySum$x)


#####################################################
#add prop & crop damage, then rank events by sum
econ <- stormData2[, c("id", "eventType","cropDamage", "propertyDamage")]
econ$econDamage <- with(econ, econ$cropDamage + econ$propertyDamage)
econSum <- aggregate(econ$econDamage, by = list(eventType = econ$eventType), 
                       sum)
econSum <- econSum[order(econSum$x, decreasing = TRUE), ]
head(econSum, 15)

#make barplot
require(ggplot2)
ggplot(econSum[1:10, ], aes(x = reorder(eventType, -x), y = x)) + 
        geom_bar(stat = "identity") + xlab("Storm Event Type") + 
        ylab("Sum of Economic Damage ($)") + 
        ggtitle("Economic Property & Crop Damage by Event, 1950-2011") + 
        theme(axis.text.x = element_text(angle = 90))

#drought > next 2
econSum$x[[1]]
sum(econSum[2:3,2])
