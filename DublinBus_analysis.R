savehistory(file="Q3.Rhistory")

library(tidyverse)
library(readr)
library(dplyr)
library(skimr)
library(lubridate)
library(stringr)
#install.packages("ggmap")
library(ggmap)

#functions to create: Mode
getmode <- function(v)
{
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v,uniqv)))]
}

Dublinbus<- unzip("DublinBusGTFS.zip")

#read tables in
agency <- read.table("agency.txt", sep = ',',header = TRUE)
routes <- read.table("routes.txt", sep=',',header = TRUE)
stoptimes <- read.table("stop_times.txt", sep=',',header = TRUE)
trips <- read.table("trips.txt", sep =',',header = TRUE)
calendar <- read.table("calendar.txt", sep=',',header = TRUE)
calendarDates<- read.table("calendar_dates.txt", sep=',',header = TRUE)
shapes <- read.table("shapes.txt" ,sep =',',header = TRUE)
stops <-read.table( "stops.txt",sep =',',header = TRUE)
transfers <- read.table("transfers.txt" ,sep =',', header = TRUE)


#View the files
View(trips)
View(shapes) #ignore this.
View(stops) #stopID - can I map the long lat to a google map?
View(routes) #only 1 routetype, agency, 199 unique routes.
View(stoptimes) #use tripID - 922,293 rows - maybe not for exploratory  analysis
View(calendarDates) # has service types and exceptions
# View(transfers) ignore transfers!
View(calendar)

#review the rows
nrow(agency)
nrow(stoptimes)


#routes
names(routes)
length(unique(routes$route_short_name))
length(unique(routes$route_id))


#trips
length(unique(trips$trip_id))
unique(trips$direction_id)
length(unique())

#shapes
length(unique(stoptimes$stop_id))

stops %>%
  group_by(stop_name)%>%
  summarise(no_of_stops = length(unique(stop_id)))%>%
  arrange(desc(no_of_stops))

#stoptimes
stoptimes$delayDep <- mutate(stoptimes$departure_time - stoptimes$arrival_time)

w <- strptime(stoptimes$departure_time, format = "%H:%M:%S")
View(stoptimes)
library(hms)
stoptimes$departure_time <- as.hms(stoptimes$departure_time)
str(stoptimes)
stoptimes <- data.frame(stoptimes, stringsAsFactors = F)

str(w)
#route with most stops
stoptimes[which.max(stoptimes$stop_sequence),]
names(trip_id)
Route <-
  trips%>%
        select(route_id)%>%
        filter(trip_id == '210.2706.0-33-b12-1.217.O')

location <- stops %>%
            select("stop_lon", "stop_lat") 



#update to lastest version of ggmap
if(!requireNamespace("devtools")) install.packages("devtools")
devtools::install_github("dkahle/ggmap")

#Getting a map of the whole city stops
library(ggmap)
register_google(key = 'AIzaSyC1GWdpuvtc_RBIH9enML_kHkS4SHUHFmk')
geocode('Dublin')

library(sf)
library(mapview)

locations_sf<- st_as_sf(location, coords = c("stop_lon", "stop_lat"), crs= 4326)
mapview(locations_sf)
View(location)

#shapes
shapes_sf <- st_as_sf(shapes, coords = c("shape_pt_lon", "shape_pt_lat"), crs= 4326)
mapview(shapes_sf)

bw_map <- get_googlemap(center = c(-6.26, 53.3), zoom = 10,
                        color = "bw",
                        style = "feature:road|visibility:off&style=element:labels|visibility:off&style=feature:administrative|visibility:off")

ggmap(bw_map)+
  geom_point(data=location, aes(x= stop_lon, y= stop_lat))
#merging tables of importance 
trips <- trips %>%
          select(-c(shape_id, trip_headsign, direction_id, block_id))



#clean the data set in order to reduce down the lines
head(stoptimes)

stoptimes <- stoptimes%>%
              select(trip_id, arrival_time, departure_time, stop_id, stop_sequence, stop_headsign)

head(routes)
routes <- routes %>%
          select(route_id, route_short_name, route_long_name, route_type)



stopRoutes <- merge(x=stoptimes, y= trips, by= 'trip_id')

names(routes)
names(stops)
names(trips)
names(stoptimes)

stopRoutes <- as.data.frame(merge(x=stopRoutes, y = routes, by = 'route_id'))


stopBusy <- stopRoutes %>%
            group_by(stop_id)%>%
            summarise(No_of_routes = length(unique(route_id)))%>%
            arrange(desc(No_of_routes))

head(stopBusy)
#Select the top 20 busiest stops
top15 <- stopBusy %>% 
          top_n(15, No_of_routes)
top15 <- merge(x= top15, y = stops, by = 'stop_id')
head(top15)
#run analysis on this as a whole!!
#more specific coords
bw_map <- get_googlemap(center = c(-6.2520, 53.3499), zoom = 12,
                        color = "bw",
                        style = "feature:road|visibility:off&style=element:labels|visibility:off&style=feature:administrative|visibility:off")

ggmap(bw_map)+
  geom_point(data = top15, aes(x= stop_lon, y= stop_lat, size = No_of_routes, colour = stop_id), alpha=0.6)+
  theme(legend.position = 'none')






#Focus on specific route - this is becoming a 24hr route and is on the Malahide road which is hte 2nd
#head(stopRoutes)
Route15 <- stopRoutes%>%
  filter(route_short_name.x == '15')

#View(Route15)
names(Route15)
length(unique(Route15$route_id))
length(unique(Route15$trip_id))
length(unique(Route15$service_id))
length(unique(Route15$route_type.x))
length(unique(Route15$stop_sequence))
unique(Route15$route_type)

max(Route15$stop_sequence)
min(Route15$arrival_time)


library(lubridate)

Route15$arrival_times <- as.character(Route15$arrival_time)
Route15$departure_times <- as.character(Route15$departure_time)
head(Route15)

min(Route15$arrival_times)
min(Route15$departure_times)


#maps
Route15 <- merge(Route15, stops, by = "stop_id")
Route15$arrival_times <- as.character(Route15$arrival_time)
Route15$departure_times <- as.character(Route15$departure_time)
str(Route15)
bw_map <- get_googlemap(center = c(-6.2520, 53.3499), zoom = 12,
                        color = "bw",
                        style = "feature:road|visibility:off&style=element:labels|visibility:off&style=feature:administrative|visibility:off")

ggmap(bw_map)+
  geom_point(data = Route15, aes(x= stop_lon, y= stop_lat, colour = stop_id), alpha=0.6)+
  theme(legend.position = 'none')

Mon_Fri_Home <- Route15 %>%
              mutate_if(is.factor, as.character) %>%
            filter(service_id == '1' & direction_id == '1' & arrival_time <'10:00:00')
             

length(unique(Mon_Fri_Home$trip_id))

Mon_Fri_Work  <- Route15 %>%
  mutate_if(is.factor, as.character) %>%
  filter(service_id == '1' & direction_id == '0' & arrival_time <'10:00:00')

Mon_Fri_Home <- Route15 %>%
  mutate_if(is.factor, as.character) %>%
  filter(service_id == '1' & direction_id == '1' & departure_time >'16:00:00'& departure_time <'19:00:00')

length(unique(Mon_Fri_Home$trip_id))
nrow(Mon_Fri_Home)

Sun_Home <- Route15 %>%
  filter(service_id == '2' & direction_id == '1')
length(unique(Sun_Home$trip_id))

Sat <- Route15 %>%
  filter(service_id == '3' & direction_id == '0' & arrival_time <'10:00:00')

length(unique(Sat$trip_id))
