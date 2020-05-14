# db_config.R
#
# Connect to the postgres database that lives on Kyle's machine


db_connect.fn<-function(x){
    library(RPostgreSQL)
    
    drv <- dbDriver("PostgreSQL")
    con <- dbConnect(drv, dbname = "Control_Test",
                     host = "10.12.50.107", port = 5432,
                     user = 'covid_users', password = 'thissucks19')
    return(con)
}