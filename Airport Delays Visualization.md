Airport Delays Visualization
==========

### Question: What airports are the worst to fly in/out of?

**For this exercise, we will define the worst airport as the airport with the longest delays.**

Load libraries.

Read in airport data file and longitude/latitude data. Process the data to speed up computations.

``` r
airport<-read.csv("https://raw.githubusercontent.com/jgscott/STA380/master/data/ABIA.csv", header=TRUE)

longlat<-read.socrata ('https://opendata.socrata.com/dataset/Airport-Codes-mapped-to-Latitude-Longitude-in-the-/rxrh-4cxm')

attach(airport)

airportkeep<-c("UniqueCarrier","ArrDelay","DepDelay","Origin","Dest","Cancelled")

airport<-airport[airportkeep]
#Drop variables that are irrelevant for this analysis.
airport$ArrDelay[is.na(airport$ArrDelay)]=0 #Replace NA's (missing delays) with zeros. Assume NA means no delay.

airport$DepDelay[is.na(airport$DepDelay)]=0 #Replace NA's (missing delays) with zeros. Assume NA means no delay.

ll<-subset(longlat, locationID%in%airport$Origin | locationID%in%airport$Dest) #keep the airport codes that are in my dataset

usamap<-map_data("usa") #importing USA map
```

First, create dummy variables to divide the flights into two sets: those departing from Austin and those arriving in Austin. We expect these to have different patterns of delays.

``` r
airport$departing<-"Departing from Austin"
airport$departing[airport$Dest=="AUS"]<-"Arriving into Austin"
airport$departing<-as.factor(airport$departing)
```

Examine the distribution of arrival and departure delays, split by whether flights are arriving into Austin or departing from Austin. We will ignore outliers by zooming in to the area around zero.

We see arrivals have a much wider spread, both for flights departing from and arriving to Austin: More flights arrive earlier than expected and later than expected as opposed to departing earlier than expected and later than expected.

Both the arrival delays and the departure delays have similar enough distributions for flights into Austin and out of Austin that we can safely consider them together rather than separately.

``` r
ggarrivalin<-ggplot(data=subset(airport,departing=="Arriving into Austin"))+geom_histogram(aes(x=ArrDelay), binwidth=10)+ coord_cartesian(xlim = c(-50,100),ylim=c(0,30000)) + labs(title="Arrival Delays for Flights\nArriving into Austin", x="Arrival Delay in Minutes", y="Number of Flights")

ggarrivalout<-ggplot(data=subset(airport,departing=="Departing from Austin"))+geom_histogram(aes(x=ArrDelay), binwidth=10)+ coord_cartesian(xlim = c(-50,100),ylim=c(0,30000)) + labs(title="Arrival Delays for Flights\nDeparting from Austin", x="Departure Delay in Minutes", y="Number of Flights")

ggdeparturein<-ggplot(data=subset(airport,departing=="Arriving into Austin"))+geom_histogram(aes(x=DepDelay), binwidth=10)+ coord_cartesian(xlim = c(-50,100),ylim=c(0,30000)) + labs(title="Departure Delays for Flights\nArriving into Austin", x="Arrival Delay in Minutes", y="Number of Flights")

ggdepartureout<-ggplot(data=subset(airport,departing=="Departing from Austin"))+geom_histogram(aes(x=DepDelay), binwidth=10)+ coord_cartesian(xlim = c(-50,100),ylim=c(0,30000)) + labs(title="Departure Delays for Flights\nDeparting from Austin", x="Departure Delay in Minutes", y="Number of Flights")

g<-plot_grid(ggarrivalin, ggarrivalout, ggdeparturein, ggdepartureout, ncol=2)

ggdraw(add_sub(g,label="Arrival delays have wider spreads,\n both flying into Austin and flying out.",x=.5,y=.5,vpadding=grid::unit(2,"lines"),fontface="bold",size=15))
```

![](Exercises2_files/figure-markdown_github/unnamed-chunk-4-1.png?raw=True)

Calculate average arrival and departure delays by airport.

``` r
airportagg<-ddply(airport,.(Origin), summarize, AvgArrDelay=mean(ArrDelay), AvgDepDelay=mean(DepDelay))
airportagg<-merge(airportagg, ll, by.x="Origin", by.y="locationID")
```

Create graphs. The worst departure delays overall (both for flights arriving at Austin and departing from Austin) are in the mid-Atlantic region, with TYS (in Knoxville) being a clear outlier. We see similar patterns for arrival delays. TYS is still by far the worst, and the mid-Atlantic region still tends higher than everywhere else.

``` r
airportagg$Longitude=-(airportagg$Longitude) #uniformizing longitude between dataset and map

ggdep<-ggplot(airportagg) + geom_map(data=usamap, map = usamap, aes(map_id=region,x=long,y=lat), fill="white", color="black") + geom_point(aes(x=Longitude,y=Latitude,size=AvgDepDelay),alpha=.5,color="blue")+ggtitle(paste0(airportagg$Origin[which.max(airportagg$AvgDepDelay)], " has the worst departure delays: ", round(airportagg$AvgDepDelay[which.max(airportagg$AvgDepDelay)],0), " minutes on average")) + scale_size_continuous("Minutes",breaks=c(-25,0,20,40,60), labels=c(-25,0,20,40,60), limits=c(-25,100), range=c(1,10))+theme(axis.line=element_blank(), axis.text=element_blank(),axis.ticks=element_blank(), axis.title=element_blank())

ggarr<-ggplot(airportagg) + geom_map(data=usamap, map = usamap, aes(map_id=region,x=long,y=lat), fill="white", color="black") + geom_point(aes(x=Longitude,y=Latitude,size=AvgArrDelay),alpha=.5,color="blue")+ggtitle(paste0(airportagg$Origin[which.max(airportagg$AvgArrDelay)], " has the worst arrival delays: ", round(airportagg$AvgArrDelay[which.max(airportagg$AvgArrDelay)],0), " minutes on average"))+ scale_size_continuous("Minutes",breaks=c(-25,0,20,40,60), labels=c(-25,0,20,40,60), limits=c(-25,100), range=c(1,10))+theme(axis.line=element_blank(), axis.text=element_blank(),axis.ticks=element_blank(), axis.title=element_blank())

q<-plot_grid(ggdep,ggarr)

ggdraw(add_sub(q,label="Avoid TYS if possible",x=.5,y=.5,vpadding=grid::unit(1,"lines"),fontface="bold",size=15))
```

![](Exercises2_files/figure-markdown_github/unnamed-chunk-6-1.png?raw=true)

While we have seen the results aggregated over all flights, we may also want to consider only what happens when a flight is in fact delayed. In other words, which airport most quickly resolves delays? We filter by only departure delays so we retain all the flights that made up for the delay. TYS is still the worst, but many other big airports on the East Coast (as well as OKC) take about an hour to resolve departure delays. Southwestern airports only take about 20 minutes to resolve delays.

``` r
airport_delays<-subset(airport,DepDelay>0)

airportagg<-ddply(airport_delays,.(Origin), summarize, AvgArrDelay=mean(ArrDelay), AvgDepDelay=mean(DepDelay))
airportagg<-merge(airportagg, ll, by.x="Origin", by.y="locationID")

airportagg$Longitude=-(airportagg$Longitude)

ggdep<-ggplot(airportagg) + geom_map(data=usamap, map = usamap, aes(map_id=region,x=long,y=lat), fill="white", color="black") + geom_point(aes(x=Longitude,y=Latitude,size=AvgDepDelay),alpha=.5,color="blue")+ggtitle(paste0(airportagg$Origin[which.max(airportagg$AvgDepDelay)], " has the worst departure delays: ", round(airportagg$AvgDepDelay[which.max(airportagg$AvgDepDelay)],0), " minutes on average")) + scale_size_continuous("Minutes",breaks=c(-25,0,20,40,60), labels=c(-25,0,20,40,60), limits=c(-25,100), range=c(1,10))+theme(axis.line=element_blank(), axis.text=element_blank(),axis.ticks=element_blank(), axis.title=element_blank())

ggarr<-ggplot(airportagg) + geom_map(data=usamap, map = usamap, aes(map_id=region,x=long,y=lat), fill="white", color="black") + geom_point(aes(x=Longitude,y=Latitude,size=AvgArrDelay),alpha=.5,color="blue")+ggtitle(paste0(airportagg$Origin[which.max(airportagg$AvgArrDelay)], " has the worst arrival delays: ", round(airportagg$AvgArrDelay[which.max(airportagg$AvgArrDelay)],0), " minutes on average"))+ scale_size_continuous("Minutes",breaks=c(-25,0,20,40,60), labels=c(-25,0,20,40,60), limits=c(-25,100), range=c(1,10))+theme(axis.line=element_blank(), axis.text=element_blank(),axis.ticks=element_blank(), axis.title=element_blank())

q<-plot_grid(ggdep,ggarr)

ggdraw(add_sub(q,label="Avoid the East Coast. Fly to the Southwest.",x=.5,y=.5,vpadding=grid::unit(1,"lines"),fontface="bold",size=15))
```

![](Exercises2_files/figure-markdown_github/unnamed-chunk-7-1.png?raw=true)

### Summary

The worst airport with regards to delays is TYS (Knoxville). However, the East Coast in general has trouble with delays. Southwestern cities handle delays particularly well.
