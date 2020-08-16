library(magrittr)
library(xml2)
library(lubridate)
library(data.table)
library(dplyr)
library(ggplot2)
library(RPostgreSQL)
library(utils)

insert_report_data <- function(con, missing_dates){
  
  trim<- function(x) return(tstrsplit(x, "\\s|[A-Z]", keep=1) %>% unlist)
  
  raw.path <- 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/'
  
  missing_dates <- paste0(missing_dates, '.csv')
  # x <- missing_dates[[1]]
  data.dt <- lapply(missing_dates, function(x, raw.path) {
    tmp.dt <- read.csv(url(paste0(raw.path, x)))
    colnames(tmp.dt) <- colnames(tmp.dt) %>% gsub('ï..', '', .) %>% gsub('[.|\\/]', '_', .)
    
    tryCatch({
      tmp.dt$Last_Update <- tmp.dt$Last_Update %>% paste
      tmp.dt$Last_Update <- tmp.dt$Last_Update %>% 
        trim %>% 
        # parse_date_time(orders=c('%m/%d/%y','%m/%d/%Y','%Y-%m-%d'))
        as.Date(tryFormats=c('%m/%d/%y','%m/%d/%Y','%Y-%m-%d'))
      # Account for mis-labeled dates in the csvs
      date.csv.name <- tstrsplit(x, '.csv', keep=1) %>% unlist %>% 
        as.Date(tryFormats=c('%m/%d/%y','%m-%d-%Y','%Y-%m-%d'))
      if (any(tmp.dt$Last_Update != date.csv.name))
        tmp.dt$Last_Update <- date.csv.name
      return(tmp.dt)},
      warning = function(w) {
        message(paste('Warning!  Check file:', x))
      },
      error = function(e) {
        message(paste('Error!  Check file', x))
      }
    )
  }, raw.path) %>% rbindlist(fill=TRUE) 
  
  data.dt$Last_Update <- data.dt$Last_Update %>% as.Date()
  if(grep('^lat$',names(data.dt),ignore.case=TRUE,value=FALSE) %>% length > 0){
    data.dt$Latitude <-data.dt[[grep('^lat$',names(data.dt),ignore.case=TRUE,value=TRUE)]]
  }
  if(grep('^long_$',names(data.dt),ignore.case=TRUE,value=FALSE) %>% length > 0){
    data.dt$Longitude <-data.dt[[grep('^long_$',names(data.dt),ignore.case=TRUE,value=TRUE)]]
  }
  
  colnames(data.dt) <- colnames(data.dt) %>% tolower
  
  table.template <- dbGetQuery(con, 'SELECT * FROM covid_data.report_data') %>% data.table %>% .[0,]
  table.template <- table.template[,-'id']
  
  # # Remove old lat/lon columns in favor of better-named ones
  # data.dt <- data.dt[, -c(grep('^lat$',names(data.dt),ignore.case=TRUE,value=TRUE),
  #                        grep('^long_$',names(data.dt),ignore.case=TRUE,value=TRUE)
  # ), with=FALSE]
  # Change remote table so that incidence rate and case-fatality-ratio columns are included
  data.dt <- data.dt[, -!(colnames(data.dt) %in% colnames(table.template)), with=FALSE]
  
  rbind(table.template, data.dt, fill=TRUE)
  tryCatch({
    dbWriteTable(con, c('covid_data','report_data'), data.dt, append=TRUE, row.names=FALSE)
    },
    error = function(e) {
      print('ERROR:  Table not written')
      printe(e)
  })
}
# 
# xml.path <- 'https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_daily_reports'
# report.file <- download_html(xml.path)
# html.read <- read_html(report.file)
# csv.list <- xml_text(html.read) %>% strsplit(split='\\n') %>% unlist
# 
# dates.list <- lapply(csv.list, function(x) {
#   if (grepl(pattern='.csv', x, fixed=TRUE)) { 
#     regex <- regexpr('\\d{2}-\\d{2}-\\d{4}.csv', x)
#     return(substr(x, start=regex[[1]], stop=regex[[1]]+attr(regex, 'match.length')))
#   }
# }) %>% unlist 
# dates.list <- dates.list %>% lapply(., function(x){gsub('*.csv$', '', x)}) %>% unlist
# 
# # -------- Access the COVID-19 Database --------- #
# source(file.path(git.path,'Code/config_files/db_config.R'))
# con <- db_connect.fn()
# # ----------------------------------------------- #
# report_data <- dbGetQuery(con, 'SELECT last_update FROM covid_data.report_data')
# dbDisconnect(con)
# 
# missing_dates <- setdiff(dates.list %>% paste, format(report_data$last_update, "%m-%d-%Y") %>% paste %>% unique)
# missing_dates <- dates.list[1:10]
# 
# if(length(missing_dates) > 0){
#   insert_report_data(con, missing_dates)
# }









