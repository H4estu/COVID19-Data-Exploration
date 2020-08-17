# parse_daily_csvs.R
#
# Update data/ directory with newly added csvs from the main CSSE repo located 
# at https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_daily_reports.
#
# Author: Z. Wallace
# Created: 3.29.20


library(magrittr)
library(xml2)
library(lubridate)
library(data.table)
library(dplyr)
library(ggplot2)

# Choose country to look at
country.switch <- 'US'

# Set paths 
git.path <- Sys.getenv('HOME')  # Where the base COVID19-Data-Exploration folder lives.

# Pull in list of daily data.
xml.path <- 'https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_daily_reports'
report.file <- download_html(xml.path)
html.read <- read_html(report.file)
csv.list <- xml_text(html.read) %>% strsplit(split='\\n') %>% unlist

# Parse html for the csv names
dates.list <- lapply(csv.list, function(x) {
    if (grepl(pattern='.csv', x, fixed=TRUE)) {
        regex <- regexpr('\\d{2}-\\d{2}-\\d{4}.csv', x)
        return(substr(x, start=regex[[1]], stop=regex[[1]]+attr(regex, 'match.length')))
    }
}) 
dates.list[sapply(dates.list, is.null)] <- NULL
dates.list <- dates.list %>% tstrsplit(split='.csv', keep=1) %>% unlist

# curr_csvs.list <- list.files(paste0(git.path, '/Code/COVID19-Data-Exploration/data/'))
# Get list of dates we do have
source(file.path(git.path, 'Code/config_files/db_config.R'))
con <- db_connect.fn()
curr_dates.list <- dbGetQuery(con, 'SELECT last_update FROM covid_data.report_data') %>%
     group_by(last_update) %>% summarize
dbDisconnect(con)

# Force dates to be in m/d/Y to comply with JHU CSSE date formatting.  This code
# makes me very sad :(
curr_dates.list$last_update <- curr_dates.list$last_update %>% year %>% 
    paste0(curr_dates.list$last_update,'-',.) %>% sub('\\d{4}-',"",.)
# Insert data for dates we don't have into database
missing_dates <- dates.list[!(dates.list %in% curr_dates.list$last_update)]
if (length(missing_dates) > 0) {
    source(file.path(git.path, 
                     'Code/COVID19-Data-Exploration/scripts/R/session2/insert_report_data.R'),
           local=T)
    con <- db_connect.fn()
    tryCatch({
<<<<<<< HEAD
        insert_report_data(con, missing_dates)  
    }, 
    error = function(e) {
        print('ERROR: Database not updated!')
        print(e)
    })
    dbDisconnect(con)
}

# Acquire COVID data from database.  Default to most recent 3-month period
con <- db_connect.fn()
end.date <- Sys.Date()
start.date <- end.date - 90
q1 <- "SELECT * FROM covid_data.report_data"
q2 <- paste0("WHERE last_update >= '", start.date, "'::DATE AND last_update <= '", 
             end.date,"'::DATE")
data.dt <- dbGetQuery(con, paste(q1, q2)) %>% data.table
dbDisconnect(con)

data.dt[confirmed%>%is.na, 'confirmed'] <- 0
data.dt[deaths%>%is.na, 'deaths'] <- 0
data.dt[recovered%>%is.na, 'recovered'] <- 0

# Summarize country data and plot by case type
country_cases.dt <- data.dt[data.dt$country_region==country.switch,] %>% 
    group_by(last_update) %>% 
    summarize(confirmed=sum(confirmed),
              deaths=sum(deaths), 
              recovered=sum(recovered)) %>% 
    data.table

# Confirmed
melted.dt <- melt(country_cases.dt[,c('last_update','confirmed')], 
                  id.vars='last_update', variable.name = 'cases', 
                  value.name='number.reported')
ggplot(melted.dt, aes(x=last_update, y=number.reported, color=cases)) +
=======
        tmp.dt$Last_Update <- tmp.dt$Last_Update %>% 
            trim %>% 
            parse_date_time(orders=c('%m/%d/%y','%m/%d/%Y','%Y-%m-%d'))
        return(tmp.dt)},
        warning = function(w) {
            message(paste('Warning!  Check file:', x))
        },
        error = function(e) {
            message(paste('Error!  Check file', x))
        }
    )
}) %>% rbindlist(fill=TRUE)

data.dt[data.dt$Recovered%>%is.na, 'Recovered'] <- 0
data.dt[data.dt$Deaths%>%is.na, 'Deaths'] <- 0
data.dt[data.dt$Confirmed%>%is.na, 'Confirmed'] <- 0


country_cases.dt <- data.dt[data.dt$Country_Region==country.switch & 
                                !is.na(data.dt$Admin2),] %>% 
    group_by(Last_Update) %>% 
    summarize(Recovered=max(Recovered),Deaths=max(Deaths)) %>% 
    data.table

# Make separate data table for confirmed, since it can be an order of magnitude
# higher than recovered/death reports.
country_confirmed.dt <- data.dt[data.dt$Country_Region==country.switch & 
                                !is.na(data.dt$Admin2),] %>% 
    group_by(Last_Update) %>% 
    summarize(Confirmed=max(Confirmed)) %>% data.table

melted.dt <- melt(country_confirmed.dt, id.vars='Last_Update', variable.name = 'Cases', 
                  value.name='Number.Reported')
ggplot(melted.dt, aes(x=Last_Update, y=Number.Reported, color=Cases)) +
>>>>>>> 2350afaf2e2aa949a7721bdff93f44013cf33e48
    geom_point() + 
    geom_line() +
    ggtitle(paste("Total Confirmed Cases in", country.switch))

# Deaths
melted.dt <- melt(country_cases.dt[,c('last_update','deaths')], 
                  id.vars='last_update', variable.name = 'cases', 
                  value.name='number.reported')
ggplot(melted.dt, aes(x=last_update, y=number.reported, color=cases)) +
    geom_point() + 
    geom_line() +
    ggtitle(paste("Total Deaths in", country.switch))

# Recovered
melted.dt <- melt(country_cases.dt[,c('last_update','recovered')], 
                  id.vars='last_update', variable.name = 'cases', 
                  value.name='number.reported')
ggplot(melted.dt, aes(x=last_update, y=number.reported, color=cases)) +
    geom_point() + 
    geom_line() +
    ggtitle(paste("Total Recovered Cases in", country.switch))
