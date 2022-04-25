# Store all relevant Cityworks info related to WIC complaints in the fieldwork schema
# Written by: Farshad Ebrahimi- 04/22/2022


#install and load the required packages. 

    install.packages("DBI")
    install.packages("RPostgreSQL")
    install.packages("RPostgres")
    install.packages("odbc")
    install.packages("dplyr")
    install.packages("sf")
    
    
    library(DBI)
    library(RPostgreSQL)
    library(RPostgres)
    library(odbc)
    library(dplyr)
    library(sf)
    
    
    

# Connect to the cityworks database

    cw <- dbConnect(odbc(),
                Driver = "ODBC Driver 17 for SQL Server",
                Server = "PWDCWSQLT",
                Database = "PWD_Cityworks",
                uid = Sys.getenv("cw_uid"),
                pwd= Sys.getenv("cw_pwd"))
    
    
# Get the data from previous work and store in-memory-Data frame CWTABLE

    CWTABLE <- dbGetQuery(cw, "SELECT wo.WORKORDERID, wo.INITIATEDATE AS WO_INITIATEDATE, wo.LOCATION, wo.WOXCOORDINATE, 
    wo.WOYCOORDINATE, woe.ENTITYUID AS FACILITYID FROM Azteca.WORKORDER wo INNER JOIN 
    Azteca.REQUESTWORKORDER rwo ON wo.WORKORDERID = rwo.WORKORDERID LEFT JOIN 
    Azteca.REQUEST r ON rwo.REQUESTID = r.REQUESTID LEFT JOIN Azteca.WORKORDERENTITY woe ON 
    wo.WORKORDERID = woe.WORKORDERID WHERE 
    ((wo.DESCRIPTION = 'A - PROPERTY INVESTIGATION' AND r.DESCRIPTION = 'WATER IN CELLAR') OR 
    (wo.DESCRIPTION = 'A - LEAK INVESTIGATION' AND r.DESCRIPTION = 'WATER IN CELLAR'))
    ")
    
# Get unique Workorderid from CWTABLE
    
    DIST_WO_ID <- select(CWTABLE, WORKORDERID) %>% distinct

# Get the workorder comments and ID from cityworks 
# Multiple comments per workorderid, so need to concatenate the comments and separate by comma
    
    
    CM_TABLE_FRAME <- dbGetQuery(cw, "SELECT WORKORDERID, COMMENTS from Azteca.WOCOMMENT" )
    UNIQUE_WO_CM<- CM_TABLE_FRAME %>%
      group_by(WORKORDERID) %>%
      summarise(COMMENTS = toString(sort(unique(COMMENTS))))
    
# inner join the unique workorderid from wic with concatenated comments
  
    comments_wic <- inner_join(DIST_WO_ID, UNIQUE_WO_CM, by = "WORKORDERID")
    
# Connect to Pg12 and write 2 tables to DB (Primary key is workorderid in fieldwork.cityworks_wic_comments )
# Disconnect from the DB
    
    con <- dbConnect(odbc(), dsn = "mars_data")
    dbWriteTable (con, SQL("fieldwork.cityworks_wic"),CWTABLE)
    dbWriteTable (con, SQL("fieldwork.cityworks_wic_comments"), comments_wic)
    dbDisconnect(cw)