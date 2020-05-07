### covid_heat_map.R
### Pulls COVID data from the database, subsets it to Oregon, and creates two types of 'heat' maps
### Kelsey Watkins
### 2020/04/27

library(data.table)
library(magrittr)
library(dplyr)
library(ggplot2)
library(stringr)
library(RPostgreSQL)
library(maps)
library(RColorBrewer)
library(gridExtra)

#### Access the COVID-19 Database ####
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname = "Control_Test",
                 host = "10.12.50.107", port = 5432, 
                 user = 'covid_users', password = 'thissucks19')
full.dt<-dbGetQuery(con,'SELECT * FROM covid_data.report_data') %>% data.table
dbDisconnect(con)
#####################################

### Summary of Oregon cases using the same method that Zach presented in the first class ###
oregon_cases.dt <- full.dt[which((grepl(', OR', full.dt$province_state) | full.dt$province_state =='Oregon') & !is.na(full.dt$latitude)),] %>% 
  group_by(last_update) %>%
  summarize(Recovered=max(recovered),Deaths=max(deaths),Confirmed=max(confirmed),Active=max(active)) %>%
  data.table

melted.dt <- melt(oregon_cases.dt, id.vars='last_update', variable.name = 'Cases', 
                  value.name='Number.Reported')
ggplot(melted.dt, aes(x=last_update, y=Number.Reported, color=Cases)) +
  geom_point() + 
  geom_line()


### Subset data down to only Oregon data ###
#Look at unique values for Oregon province_state, we had a change in reporting so we are going to only use the data from the update onwards.
print(full.dt$province_state %>% unique)
print(full.dt[which((grepl(', OR', full.dt$province_state) | full.dt$province_state =='Oregon') & !is.na(full.dt$latitude)),]$province_state %>% unique)
covid.OR<-full.dt[which(full.dt$province_state =='Oregon' & !is.na(full.dt$latitude) & !is.na(full.dt$admin2)),]

# Look at the data split by date to determine how to work with it #
covid.OR %>% split(.,.$last_update) %>% .[c(1,2)]

# Add new fields and make the date pretty for plotting
covid.OR$month<-covid.OR$last_update %>% format(., "%B")

# Separate each month into 'periods' for analysis over time. (I don't know why I didn't do weeks lol)
covid.OR$period<-"NA"
covid.OR[covid.OR$last_update %>% format(., "%d") %in% paste0('0',1:9),]$period<-paste0(covid.OR[covid.OR$last_update %>% format(., "%d") %in% paste0('0',1:9),]$month,' 1-9')
covid.OR[covid.OR$last_update %>% format(., "%d") %in% 10:19,]$period<-paste0(covid.OR[covid.OR$last_update %>% format(., "%d") %in% 10:19,]$month,' 10-19')
covid.OR[covid.OR$last_update %>% format(., "%d") %in% 20:31,]$period<-paste0(covid.OR[covid.OR$last_update %>% format(., "%d") %in% 20:31,]$month,' 20-31')

# Create factor levels for ggplot facet wrap
covid.OR$period<-covid.OR$period %>% factor(levels = unique(.))

# Look at only deaths firsts, so subset out only that data
subset<-covid.OR[,c('deaths','latitude', 'longitude', 'period')]

# The function we will be using to create our heat map requires that each value be a unique 'event' with its own coordinate location
# rather than being able to weight the points by a particular value, because of this we must replicate our rows and their lat/long
# data by the number of the parameter of interest.
subset.expanded <- subset[rep(1:nrow(subset), subset$deaths)]
# Check out the results
tail(subset.expanded, 20)

# Next we are going to summarize our data for the point based heat map that we are going to do.
subset.summary<- subset[,.(deaths=max(deaths)), by=c('period','latitude', 'longitude')]
print(subset.summary)

# Generate a map of Oregon to plot the COVID data on using 'maps' package
all_states<-map_data('state')
head(all_states)

Oregon<-all_states[all_states$region=='oregon',]
p <- ggplot() +
  geom_polygon(data=Oregon, aes(x=long, y=lat))
print(p)

## Plot a heat map layer: Polygons with fill colors based on
## relative frequency of events
COVID.map <- p + stat_density2d(data=subset.expanded, aes(x=longitude, y=latitude, fill=..level.., alpha=..level..), geom="polygon")
print(COVID.map)

# Remove legends and add title
# Define the spectral colors to fill the density contours using the 'RColorBrewer' package
display.brewer.all(n=NULL, type="all", select=NULL, exact.n=TRUE,
                   colorblindFriendly=FALSE)
COVID.map <- COVID.map + theme(legend.position = 'none') +
  ggtitle("Oregon Covid-19 Deaths: Heat Map") +
  scale_fill_gradientn(colours=brewer.pal(7, "Reds"))
print(COVID.map)

## Plot Covid-19 by the period that we created earlier
COVID.heat.map <- COVID.map + facet_wrap(~period)
print(COVID.heat.map)

# Create new map with geom_point layer that uses the max deaths by location and period to dictate size 
# (heat map with no interpolation + represents all values)
COVID.point.map <- p + geom_point(data=subset.summary, aes(x=longitude, y=latitude, size=deaths, color=deaths), alpha = 0.7) +
  ggtitle("Oregon Covid-19 Deaths: Point") +
  scale_color_gradientn(colours=brewer.pal(7, "Reds")) + 
  facet_wrap(~period)
print(COVID.point.map)

# Plot both maps next to each other and compare using the gridExtra package
deaths <-grid.arrange(COVID.heat.map, COVID.point.map, ncol = 2)


#Now for confirmed cases
subset.c<-covid.OR[,c('confirmed','latitude', 'longitude', 'period')]
subset.expanded.c <- subset.c[rep(1:nrow(subset.c), subset.c$confirmed)]
subset.summary.c<- subset.c[,.(confirmed=max(confirmed)), by=c('period','latitude', 'longitude')]

## Plot a heat map layer: Polygons with fill colors based on
## relative frequency of events which we created by replicating rows by the number of events.
confirmed.heat.map <- p + 
  stat_density2d(data=subset.expanded.c, aes(x=longitude, y=latitude, fill=..level.., alpha=..level..), geom="polygon") +
  scale_fill_gradientn(colours=brewer.pal(7, "Reds")) +
  theme(legend.position = 'none') +
  ggtitle("Oregon Confirmed Covid-19 Cases: Heat Map") +
  facet_wrap(~period)

print(confirmed.heat.map)

confirmed.point.map <-p +
  geom_point(data=subset.summary.c, aes(x=longitude, y=latitude, size=confirmed, color=confirmed),alpha = 0.7)+
  scale_color_gradientn(colours=brewer.pal(7, "Reds")) +
  ggtitle("Oregon Confirmed Covid-19 Cases: Heat Map") +
  facet_wrap(~period)
print(confirmed.point.map)
# Plot both maps next to each other and compare using the gridExtra package
confirmed <- grid.arrange(confirmed.heat.map, confirmed.point.map, ncol = 2)

#Now for active cases
subset.a<-covid.OR[,c('active','latitude', 'longitude', 'period')]
subset.expanded.a <- subset.a[rep(1:nrow(subset.a), subset.a$active)]
subset.summary.a<- subset.a[,.(active=max(active)), by=c('period','latitude', 'longitude')]

## Plot a heat map layer: Polygons with fill colors based on
## relative frequency of events which we created by replicating rows by the number of events.
active.heat.map <- p + 
  stat_density2d(data=subset.expanded.a, aes(x=longitude, y=latitude, fill=..level.., alpha=..level..), geom="polygon") +
  scale_fill_gradientn(colours=brewer.pal(7, "Reds")) +
  theme(legend.position = 'none') +
  ggtitle("Oregon Active Covid-19 Cases: Heat Map") +
  facet_wrap(~period)

print(active.heat.map)

active.point.map <-p +
  geom_point(data=subset.summary.a, aes(x=longitude, y=latitude, size=active, color=active),alpha = 0.7)+
  scale_color_gradientn(colours=brewer.pal(7, "Reds")) +
  ggtitle("Oregon Active Covid-19 Cases: Heat Map") +
  facet_wrap(~period)
print(active.point.map)
# Plot both maps next to each other and compare using the gridExtra package
active <- grid.arrange(active.heat.map, active.point.map, ncol = 2)

# Redo the last one and adjust periods so your maps aren't scrunchie
active.point.map <-p +
  geom_point(data=subset.summary.a[subset.summary.a$period %in% c('April 10-19', 'April 20-31'),], aes(x=longitude, y=latitude, size=active, color=active),alpha = 0.7)+
  scale_color_gradientn(colours=brewer.pal(7, "Reds")) +
  ggtitle("Oregon Active Covid-19 Cases: Heat Map") +
  facet_wrap(~period)
print(active.point.map)

# Plot both maps next to each other and compare using the gridExtra package
active <- grid.arrange(active.heat.map, active.point.map, ncol = 2)

#Plot all maps together :)
all_maps <- grid.arrange(deaths, confirmed, active, nrow = 3)
ggsave(plot=all_maps, 'E:/KWatkins/Development_Meeting/covid_maps.jpeg', height = 11, width = 10)
