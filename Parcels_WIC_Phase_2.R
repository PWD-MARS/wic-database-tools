# Connect to GISDATA DB Get the parcels layer from GISDATA
#dummy edit from Taylor
#dummy edit from Farshad Ebrahimi

###Taylor says: Style comments - You should be using section headers, and hierarchical indentation to demarcate your sections
###Section headers on the left at indent level 0
  ###Comments and code at indent level 1
    ###One level of indent per scope shift (curly brace) {
      ###like this
    #} Closing curly brace at same indent level as the line of code that opened the curly brace

###Taylor says: Divide the script into the following sections:
  ###1: Gathering nonspatial data
  ###2: Matching WICs to parcels by facility ID
  ###3: Matching WICs to parcels by address
  ###4: Gathering spatial data
  ###5: Matching WICs to parcels by geoprocessing
  ###6: Writing results to DB

# Section 1: Gathering nonspatial data
  library(dplyr)
  library(odbc)
  library(sf)

  ###Comments at same indent level as relevant code block
  GISDB <- dbConnect(odbc(),
                  Driver = "ODBC Driver 17 for SQL Server",
                  Server = "PWDGISSQL",
                  Database = "GISDATA",
                  uid = "gisread",
                  pwd= "gisread")


  dsn <- paste0("MSSQL:server=PWDGISSQL;",
                      "database= GISDATA;",
                      "UID=gisread;",
                      "PWD=gisread;")

    

  Parcels_Frame <- dbGetQuery(GISDB, "SELECT * from GISDATA.GISAD.PWD_PARCELS")
  
  ###Taylor says: Why query again instead of using select statements in dplyr?
  Parcels_facility_id <- select(Parcels_Frame, FACILITYID)
  Parcels_Address_id <- select(Parcels_Frame, ADDRESS, FACILITYID)
  
  ###Taylor says: Fully expand network drive names for compatibility
  PARCELS_SPATIAL <- st_read(dsn = "\\\\pwdoows\\oows\\Watershed Sciences\\GSI Monitoring\\09 GIS Data\\PWD_PARCELS ", layer = "PWD_PARCELS")
  st_crs(PARCELS_SPATIAL) = 2272
  
  # Connect to MARS DB and get the workorderid, comments and associated info
  con <- dbConnect(odbc(), dsn = "mars_data")
  WORKORDER_ID <- dbGetQuery(con, "SELECT * from fieldwork.cityworks_wic")
  WORKORDER_CM <- dbGetQuery(con, "SELECT * from fieldwork.cityworks_wic_comments")
    
# Remove {} from facilityid in cityworks 
    ###Taylor says: You can do this in one go with a capture group
    ###Regex = in: a literal {, followed by a capture group containing 0 or more characters, followed by a literal }
    ###Regex = out: The first capture group
    WORKORDER_ID$FACILITYID<-gsub("\\{(.*)\\}","\\1",as.character(WORKORDER_ID$FACILITYID))


### Section 2: Matching by facility ID
  # Inner join the workorderid and Parcels_facility_id ON facility ID
  WOID_FACID_Match <- inner_join (WORKORDER_ID, Parcels_Address_id  , by = "FACILITYID")
   
   
# Get FIRST OUTPUT BASED ON FACILITYID MATCH STORED IN WOID_FAC_UNIQ_REL
   
    ###Taylor says: Use dplyr and do it in one go. Why are you changing the address field to be named location?
    #WOID_FAC_UNIQ_REL <- WOID_FACID_Match[c("WORKORDERID","ADDRESS","FACILITYID")]
    #names ( WOID_FAC_UNIQ_REL) <- c("WORKORDERID", "LOCATION", "FACILITYID")
    WOID_FAC_UNIQ_REL <- select(WOID_FACID_Match, WORKORDERID, LOCATION = ADDRESS, FACILITYID)
  
# Remove the rows that we were matched based on the facilityid from the cityworks table 
     
    WORKORDER_ID <- anti_join(WORKORDER_ID, WOID_FACID_Match, by = "WORKORDERID")
    
# Remove the rows that were matched from the Parcels_Address_id table
    
    Parcels_Address_id <-anti_join(Parcels_Address_id, WOID_FACID_Match, by = "FACILITYID" )
   
# drop the facility id from workorderid as these ids are not meaningful 
    ###Taylor says: Use dplyr to be explicit
    #WORKORDER_ID_Location <-  WORKORDER_ID[c(1:5)]
    WORKORDER_ID_Location <-  select(WORKORDER_ID, -FACILITYID)

# Inner join the  WORKORDER_ID_Location with Parcels_Address_id to extract the facility id
   
    WO_ID_ADDRESS_MATCH <- inner_join(WORKORDER_ID_Location,Parcels_Address_id,by = c("LOCATION" = "ADDRESS"))

# GET SECOND OUTPUT BASED ON FACILITYID MATCH STORED IN WOID_FAC_UNIQ_Add_REL )  

    WOID_FAC_UNIQ_Add_REL <- WO_ID_ADDRESS_MATCH[c("WORKORDERID", "LOCATION", "FACILITYID")]
   
# Remove the rows that we were matched based on the address from the cityworkstable 
   
    WORKORDER_ID <- anti_join(WORKORDER_ID, WO_ID_ADDRESS_MATCH, by = "WORKORDERID")
    
# Remove the rows that were matched from the Parcels_Address_id table
    
    Parcels_Address_id <-anti_join(Parcels_Address_id, WO_ID_ADDRESS_MATCH, by = "FACILITYID" ) 
    
# starting the GIS data handling here
    
# Build the spatial object from the XY coordinates in Cityworks
   
    ###Taylor says: ??? why do this?
    WORKORDER_IDS <- WORKORDER_ID
   
     xy_coor <-  c(WORKORDER_IDS[,4],WORKORDER_IDS[,5])
     ###Taylor says: Label your arguments so you don't have to skip them like this
     #mat_coor <- matrix(xy_coor, ,2) 
     #mat_coor <- na.omit(mat_coor)
     mat_coor <- matrix(data = xy_coor, ncol = 2) %>% na.omit
  
   ###Taylor says: Why the base R pipe and not %>%? Is there an advantage to this?
     WO_SPATIAL <- mat_coor |> 
       as.matrix() |> 
       st_multipoint() |> 
       st_sfc() |> 
       st_cast('POINT')
     
     st_crs(WO_SPATIAL) <- 2272
     
# Intersect the Parcel polygons and the points
     
     WO_PARC_INTERSECT <- st_intersects(WO_SPATIAL, PARCELS_SPATIAL )

     
# get the index of cityworks and Parcels tables to buid table COORD_MATCHED_TABLE containing the XY and Facility ID     
     ###Taylor says: Explain each line of code here and what it's accomplishing.
     ###Comments are messages to your future self - why this function and not something else?
     ###That sapply isn't actually summing anything, just unlisting
     ###And unlist() drops gaps, so the indices would change, for example. This is important context and you *will* forget it
     Element_To_Voc <- sapply(WO_PARC_INTERSECT, sum)
     Index_WO <- which(Element_To_Vec != 0, T)
     Index_Parcel <- Element_To_Vec[Index_WO]
     
     
     mat_coor <- data.frame (mat_coor)
     names(mat_coor) <- c("WOXCOORDINATE","WOYCOORDINATE")
     WO_MATCHED_XY <- mat_coor[Index_WO, ]
     Parcel_Matched_Polyg <- Parcels_Frame[Index_Parcel, ]
     
     COORD_MATCHED_TABLE <- cbind(Parcel_Matched_Polyg,WO_MATCHED_XY)
     
# Get workorderid, address, and facilityid (Third output is stored in WOID_Based_XY )
     
     ###Taylor says: dplyr could accomplish all of this in a single pipe operation
     ### WOID_BASED_XY <- WORKORDER_ID %>% select(varnames) %>% inner_join(x, by = ...) %>% select(varnames with renaming)
     WORKORDER_ID  <- WORKORDER_ID[c(1:5)]
     WOID_Based_XY <- inner_join (WORKORDER_ID, COORD_MATCHED_TABLE  , by = c("WOXCOORDINATE" = "WOXCOORDINATE", "WOYCOORDINATE" = "WOYCOORDINATE")) 
     WOID_Based_XY <- WOID_Based_XY[c("WORKORDERID", "ADDRESS", "FACILITYID")]
     names (WOID_Based_XY) <- c("WORKORDERID", "LOCATION", "FACILITYID")
     
# union the matching tables-Final output is in FACID_ADD_XY
   
    FACID_ADD <- union_all (WOID_FAC_UNIQ_REL,WOID_FAC_UNIQ_Add_REL)
    FACID_ADD_XY <- union_all (FACID_ADD, WOID_Based_XY)
    FACID_ADD_XY <- unique(FACID_ADD_XY)
    
# Place holder for writing the final table in Mars database 
    con <- dbConnect(odbc(), dsn = "mars_data")
    dbWriteTable (con, SQL("fieldwork.gis_parcels"),FACID_ADD_XY)
   
