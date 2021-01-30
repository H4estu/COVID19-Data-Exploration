# sf_example_simple.R

library(sf)
library(magrittr)
library(data.table)
library(RPostgreSQL)

# source('C:/Users/zach/Documents/Code/COVID19-Data-Exploration/scripts/R/session2/insert_report_data.R')
# -------- Access the COVID-19 Database --------- #
source(file.path(git.path,'Code/config_files/db_config.R'))
con <- db_connect.fn()
report_data.dt<-dbGetQuery(con,"SELECT * FROM covid_data.report_data WHERE country_region = 'US'") %>% data.table
dbDisconnect(con)  
# ----------------------------------------------- #

# remove NA's 
report_data.dt <- report_data.dt[!is.na(report_data.dt$latitude) & 
                                     !is.na(report_data.dt$longitude),]

# Create spatial object from data table and view on global map
report_data.sf <- st_as_sf(report_data.dt, coords = c("longitude", "latitude"), crs=4326)

us_data.dt <- report_data.dt[report_data.dt$country_region=='US',]
# us_data.dt <- us_data.dt[us_data.dt$confirmed<=sd(us_data.dt$confirmed)]
us_data.dt <- us_data.dt[us_data.dt$confirmed<=1000,]
us_data.sf <- st_as_sf(us_data.dt, coords = c("longitude", "latitude"), crs=4326)

bound <- st_bbox(c(xmin=-125.77148,ymin=23.24135,xmax=-64.59961, ymax=48.86471))

# plot(us_data.sf['confirmed'], extent=bound)