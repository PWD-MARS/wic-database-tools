
#install and load the required packages. 

    install.packages("DBI")
    install.packages("RPostgreSQL")
    install.packages("RPostgres")
    install.packages("odbc")
    install.packages("dplyr")
    
    library(DBI)
    library(RPostgreSQL)
    library(RPostgres)
    library(odbc)
    library(dplyr)

# Connect to the cityworks database

    cw <- dbConnect(odbc(),
                Driver = "ODBC Driver 17 for SQL Server",
                Server = "PWDCWSQLT",
                Database = "PWD_Cityworks",
                uid = Sys.getenv("cw_uid"),
                pwd= Sys.getenv("cw_pwd"))
    odbcListObjects(cw)

    odbcListObjects(cw, catalog="PWD_Cityworks", schema="azteca")
    
    
# Get the data from previous work and store in-memory-Data frame CWTABLE

    CWTABLE <- dbGetQuery(cw, "SELECT wo.WORKORDERID, wo.INITIATEDATE AS WO_INITIATEDATE, wo.LOCATION, wo.WOXCOORDINATE, 
    wo.WOYCOORDINATE, woe.ENTITYUID AS FACILITYID FROM Azteca.WORKORDER wo INNER JOIN 
    Azteca.REQUESTWORKORDER rwo ON wo.WORKORDERID = rwo.WORKORDERID LEFT JOIN 
    Azteca.REQUEST r ON rwo.REQUESTID = r.REQUESTID LEFT JOIN Azteca.WORKORDERENTITY woe ON 
    wo.WORKORDERID = woe.WORKORDERID WHERE 
    ((wo.DESCRIPTION = 'A - PROPERTY INVESTIGATION' AND r.DESCRIPTION = 'WATER IN CELLAR') OR 
    (wo.DESCRIPTION = 'A - LEAK INVESTIGATION' AND r.DESCRIPTION = 'WATER IN CELLAR'))
    ")
    
    

# Get the workorder comments and ID from cityworks, remove column SEQID, group by 
# workorderid. Multiple comments per id, so need to concatenate the comments and separate by comma
    
    
    CM_TABLE_FRAME <- dbGetQuery(cw, "SELECT * from Azteca.WOCOMMENT" )
    CM_TABLE_WOID_CM <- CM_TABLE_FRAME [ , 1:2]
    UNIQUE_WO_CM<- CM_TABLE_WOID_CM %>%
      group_by(WORKORDERID) %>%
      summarise(COMMENTS = toString(sort(unique(COMMENTS))))
    
    
# inner join the comments and the WIC table (FINALTABLE with 7 variables)
    
    FINALTABLE<- inner_join(CWTABLE, UNIQUE_WO_CM, by = "WORKORDERID")
    
    
# Connect to Pg12 and write to DB
    
    con <- dbConnect(odbc(), dsn = "mars_data")
    dbWriteTable (con, SQL("fieldwork.cityworks_WIC"), FINALTABLE)
    dbGetQuery(con,"alter table if exists fieldwork.cityworks_WIC add constraint unique_id PRIMARY KEY (WORKORDERID) ")