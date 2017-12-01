library(dplyr)
library(tidyr)
library(ggplot2)
library(ggmap)
library(openair)
library(scales)
library(gridExtra)

## Load NYC 311 noise complaints data
noise <- read.csv('data/NYC_noise_2016_1.csv', stringsAsFactors = F) %>%
  tbl_df()

## Filter NYC 311 dataRemove missing data
noise <- filter(noise, !is.na(Latitude) | !is.na(Longitude)) %>%
  select(Created.Date, Complaint.Type, Borough, Latitude, Longitude)

## Data Sample
head.matrix(noise, n = 5L)

## Convert created date to POSIXlt object for using date elements
noise$Created.Date <- as.POSIXlt(noise$Created.Date, "%m/%d/%Y", tz = 'EST')

## Create date fields to be used for time series analysis
noise <- mutate(noise, year = noise$Created.Date$year + 1900)
noise <- mutate(noise, month = noise$Created.Date$mon + 1)
noise <- mutate(noise, day = noise$Created.Date$mday)

## Round lat/lon to 3 decimals, to allow for density calculation based on location
noise <- mutate(noise, lat = signif(noise$Latitude, 5))
noise <- mutate(noise, lon = signif(noise$Longitude, 5))

## Download New York City map from Google map

mapstyle = "element:geometry%7Ccolor:0x212121&style=element:labels.icon%7Cvisibility:off&style=element:labels.text.fill%7Ccolor:0x757575&style=element:labels.text.stroke%7Ccolor:0x212121&style=feature:administrative%7Celement:geometry%7Ccolor:0x757575&style=feature:administrative.country%7Celement:labels.text.fill%7Ccolor:0x9e9e9e&style=feature:administrative.land_parcel%7Cvisibility:off&style=feature:administrative.locality%7Celement:labels.text.fill%7Ccolor:0xbdbdbd&style=feature:administrative.neighborhood%7Cvisibility:off&style=feature:poi%7Celement:labels.text%7Cvisibility:off&style=feature:poi%7Celement:labels.text.fill%7Ccolor:0x757575&style=feature:poi.business%7Cvisibility:off&style=feature:poi.park%7Celement:geometry%7Ccolor:0x181818&style=feature:poi.park%7Celement:labels.text.fill%7Ccolor:0x616161&style=feature:poi.park%7Celement:labels.text.stroke%7Ccolor:0x1b1b1b&style=feature:road%7Celement:geometry.fill%7Ccolor:0x2c2c2c&style=feature:road%7Celement:labels%7Cvisibility:off&style=feature:road%7Celement:labels.icon%7Cvisibility:off&style=feature:road%7Celement:labels.text.fill%7Ccolor:0x8a8a8a&style=feature:road.arterial%7Celement:geometry%7Ccolor:0x373737&style=feature:road.highway%7Celement:geometry%7Ccolor:0x3c3c3c&style=feature:road.highway.controlled_access%7Celement:geometry%7Ccolor:0x4e4e4e&style=feature:road.local%7Celement:labels.text.fill%7Ccolor:0x616161&style=feature:transit%7Cvisibility:off&style=feature:transit%7Celement:labels.text.fill%7Ccolor:0x757575&style=feature:water%7Celement:geometry%7Ccolor:0x000000&style=feature:water%7Celement:labels.text%7Cvisibility:off&style=feature:water%7Celement:labels.text.fill%7Ccolor:0x3d3d3"
nycMap <- get_googlemap("New York City", maptype='roadmap', style = mapstyle, zoom = 11, messaging = F)
manhMap <- get_googlemap("Manhattan, NY", maptype = 'roadmap', style = mapstyle, zoom = 12, messaging = F)
bronMap <- get_googlemap("Bronx, NY", maptype = 'roadmap', style = mapstyle, zoom = 12, messaging = F)
brooMap <- get_googlemap("Brooklyn, NY", maptype = 'roadmap', style = mapstyle, zoom = 12, messaging = F)
queeMap <- get_googlemap("Queens, NY", maptype = 'roadmap', style = mapstyle, zoom = 12, messaging = F)
statMap <- get_googlemap("Staten Island, NY", maptype = 'roadmap', style = mapstyle, zoom = 12, messaging = F)

## Prepare to compare distribution of complaints by borough
byborough <- group_by(select(noise, -Created.Date), Borough, year)
byborough <- summarise(byborough, count = n())
byborough <- group_by(byborough, year)
byborough <- mutate(byborough, percent = byborough$count / sum(byborough$count) *100)

inmanh <- group_by(select(noise, -Created.Date), Borough, year) %>%
  subset(Borough=='MANHATTAN') %>%
  group_by(year)

inbroo <- group_by(select(noise, -Created.Date), Borough, year) %>%
  subset(Borough=='BROOKLYN') %>%
  group_by(year)

inbron <- group_by(select(noise, -Created.Date), Borough, year) %>%
  subset(Borough=='BRONX') %>%
  group_by(year)

inquee <- group_by(select(noise, -Created.Date), Borough, year) %>%
  subset(Borough=='QUEENS') %>%
  group_by(year)

instat <- group_by(select(noise, -Created.Date), Borough, year) %>%
  subset(Borough=='STATEN ISLAND') %>%
  group_by(year)


## Prepare to compare distribution of complaints by noise type
bynoisetype <- group_by(select(noise, -Created.Date), Complaint.Type, year)
bynoisetype <- summarise(bynoisetype, count = n())
bynoisetype <- group_by(bynoisetype, year)
bynoisetype <- mutate(bynoisetype, percent = bynoisetype$count / sum(bynoisetype$count)*100)

## Prepare data for daily analysis
noisedf <- read.csv('data/NYC_noise_2016_1.csv', stringsAsFactors = F) %>%
  filter(!is.na(Created.Date)) %>%
  select(-Location)
caldata <- select(noisedf, Created.Date, Unique.Key)
caldata <- mutate(caldata, Created.Date = as.character(Created.Date))
caldata <- mutate(caldata, Created.Date = substr(Created.Date, 0, 10))
caldata <- group_by(caldata, Created.Date)
caldata <- summarise(caldata, count = n())
names(caldata)[1] <- "date"
caldata$date <- as.POSIXct(caldata$date, "%m/%d/%Y", tz = "EST")

## Map noise pollution by borough
mp <- ggmap(nycMap) +
  stat_density2d(aes(x=lon, y=lat, fill='red', alpha=..level.., size=0), data=noise, geom = 'polygon') +
  facet_wrap(~ year) + ggtitle("Noise pollution in 2016") +
  theme(axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), legend.position = 'none') +
  xlab("") + ylab("")
mp

mp1 <- ggmap(manhMap) +
  stat_density2d(aes(x=lon, y=lat, fill='red', alpha=..level.., size=0), data=inmanh, geom = 'polygon') +
  facet_wrap(~ Borough) + 
  theme(axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), legend.position = 'none') +
  xlab("") + ylab("")

mp2 <- ggmap(brooMap) +
  stat_density2d(aes(x=lon, y=lat, fill='red', alpha=..level.., size=0), data=inbroo, geom = 'polygon') +
  facet_wrap(~ Borough) + 
  theme(axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), legend.position = 'none') +
  xlab("") + ylab("")

grid.arrange(mp1, mp2, nrow = 1, ncol = 2)

mp3 <- ggmap(bronMap) +
  stat_density2d(aes(x=lon, y=lat, fill='red', alpha=..level.., size=0), data=inbron, geom = 'polygon') +
  facet_wrap(~ Borough) + 
  theme(axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), legend.position = 'none') +
  xlab("") + ylab("")

mp4 <- ggmap(queeMap) +
  stat_density2d(aes(x=lon, y=lat, fill='red', alpha=..level.., size=0), data=inquee, geom = 'polygon') +
  facet_wrap(~ Borough) + 
  theme(axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), legend.position = 'none') +
  xlab("") + ylab("")
grid.arrange(mp3, mp4, nrow = 1, ncol = 2)

ggmap(statMap) +
  stat_density2d(aes(x=lon, y=lat, fill='red', alpha=..level.., size=0), data=instat, geom = 'polygon') +
  facet_wrap(~ Borough) + 
  theme(axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), legend.position = 'none') +
  xlab("") + ylab("")


## Map noise pollution by noise type
ggmap(nycMap) +
  stat_density2d(aes(x=lon, y=lat, fill='red', alpha=..level.., size=0), data=noise, geom = 'polygon') +
  facet_wrap(~ Complaint.Type) + ggtitle("Noise pollution in 2016") +
  theme(axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), legend.position = 'none') +
  xlab("") + ylab("")

## Plot distribution of noise pollutions by borough
ggplot(byborough, aes(x = reorder(Borough, -count), y = count, fill = Borough)) + 
  geom_bar(stat = 'identity') + 
  ggtitle('Distribution of Noise Complaints by Borough in 2016') +
  xlab("Borough") + ylab('Count') +
  scale_y_continuous(labels=comma) +
  geom_text(aes(label = paste0(round(byborough$percent,2),"%")), size = 5, hjust = 0.5, vjust = 2, position = 'identity')

## Plot distribution of noise pollutions by type
ggplot(bynoisetype, aes(x=count, y=reorder(Complaint.Type, count), fill = Complaint.Type)) + 
  geom_bar(stat = 'identity') +
  ggtitle('Distribution of Noise Complaints by Type in 2016') +
  xlab("Percent of Complains") + ylab('Types of noise') +
  scale_x_continuous(labels=comma) +
  geom_text(aes(label = paste0(round(bynoisetype$percent,2),"%")), size = 4, hjust = 0.5, vjust = 2, position = 'identity')

## Plot a calendar heatmap on Noise Complaints by day in 2016
calendarPlot(caldata, pollutant = "count", year = 2016, main = "Noise Complaints by Day in 2016", cols=c('yellow','orange','red'))