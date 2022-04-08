# Connect to GISDATA DB Get the parcels layer from GISDATA
#dummy edit from Taylor
#dummy edit from Farshad E

    GISDB <- dbConnect(odbc(),
                    Driver = "ODBC Driver 17 for SQL Server",
                    Server = "pwdgis4",
                    Database = "GISDATA",
                    uid = "gisread",
                    pwd= "gisread")


    dsn <- paste0("MSSQL:server=pwdgis4;",
                        "database= GISDATA;",
                        "UID=gisread;",
                        "PWD=gisread;")

    

    Parcels_Frame <- dbGetQuery(GISDB, "SELECT * from GISDATA.GISAD.PWD_PARCELS")
    Parcels_facility_id <- dbGetQuery(GISDB, "SELECT FACILITYID from GISDATA.GISAD.PWD_PARCELS")
    Parcels_Address_id <- dbGetQuery(GISDB, "SELECT ADDRESS, FACILITYID from GISDATA.GISAD.PWD_PARCELS")
    
    PARCELS_SPATIAL <- st_read(dsn = "P:\\Watershed Sciences\\GSI Monitoring\\09 GIS Data\\PWD_PARCELS ", layer = "PWD_PARCELS")
    st_crs(PARCELS_SPATIAL) = 2272
    
# Connect to MARS DB and get the workorderid, comments and associated info
    
    con <- dbConnect(odbc(), dsn = "mars_data")
    WORKORDER_ID <- dbGetQuery(con, "SELECT * from fieldwork.cityworks_wic")
    WORKORDER_CM <- dbGetQuery(con, "SELECT * from fieldwork.cityworks_wic_comments")
    
    
    
# Remove {} from facilityid in cityworks 
    WORKORDER_ID$FACILITYID<-gsub("{","",as.character(WORKORDER_ID$FACILITYID), fixed=TRUE)
    WORKORDER_ID$FACILITYID<-gsub("}","",as.character(WORKORDER_ID$FACILITYID), fixed=TRUE)
    


# Inner join the workorderid and Parcels_facility_id ON facility ID
  
    WOID_FACID_Match <- inner_join (WORKORDER_ID, Parcels_Address_id  , by = "FACILITYID")
   
   
# Get FIRST OUTPUT BASED ON FACILITYID MATCH STORED IN WOID_FAC_UNIQ_REL
   
    WOID_FAC_UNIQ_REL <- WOID_FACID_Match[c("WORKORDERID","ADDRESS","FACILITYID")]
    names ( WOID_FAC_UNIQ_REL) <- c("WORKORDERID", "LOCATION", "FACILITYID")
  
# Remove the rows that we were matched based on the facilityid from the cityworks table 
     
    WORKORDER_ID <- anti_join(WORKORDER_ID, WOID_FACID_Match, by = "WORKORDERID")
    
# Remove the rows that were matched from the Parcels_Address_id table
    
    Parcels_Address_id <-anti_join(Parcels_Address_id, WOID_FACID_Match, by = "FACILITYID" )
   
# drop the facility id from workorderid as these ids are not meaningful 
    
    WORKORDER_ID_Location <-  WORKORDER_ID[c(1:5)]

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
   
    WORKORDER_IDS <- WORKORDER_ID
   
     xy_coor <-  c(WORKORDER_IDS[,4],WORKORDER_IDS[,5])
     mat_coor <- matrix(xy_coor, ,2)
     mat_coor <- na.omit(mat_coor)
  
   
     WO_SPATIAL <- mat_coor |> 
       as.matrix() |> 
       st_multipoint() |> 
       st_sfc() |> 
       st_cast('POINT')
     
     st_crs(WO_SPATIAL) <- 2272
     
# Intersect the Parcel polygons and the points
     
     WO_PARC_INTERSECT <- st_intersects(WO_SPATIAL, PARCELS_SPATIAL )

     
# get the index of cityworks and Parcels tables to buid table COORD_MATCHED_TABLE containing the XY and Facility ID     
     
     Element_To_Vec <- sapply(WO_PARC_INTERSECT, sum)
     Index_WO <- which(Element_To_Vec != 0, T)
     Index_Parcel <- Element_To_Vec[Index_WO]
     
     
     mat_coor <- data.frame (mat_coor)
     names(mat_coor) <- c("WOXCOORDINATE","WOYCOORDINATE")
     WO_MATCHED_XY <- mat_coor[Index_WO, ]
     Parcel_Matched_Polyg <- Parcels_Frame[Index_Parcel, ]
     
     COORD_MATCHED_TABLE <- cbind(Parcel_Matched_Polyg,WO_MATCHED_XY)
     
# Get workorderid, address, and facilityid (Third output is stored in WOID_Based_XY )
     
     WORKORDER_ID  <- WORKORDER_ID[c(1:5)]
     WOID_Based_XY <- inner_join (WORKORDER_ID, COORD_MATCHED_TABLE  , by = c("WOXCOORDINATE" = "WOXCOORDINATE", "WOYCOORDINATE" = "WOYCOORDINATE")) 
     WOID_Based_XY <- WOID_Based_XY[c("WORKORDERID", "ADDRESS", "FACILITYID")]
     names (WOID_Based_XY) <- c("WORKORDERID", "LOCATION", "FACILITYID")
     
# union the matching tables-Final output is in FACID_ADD_XY
   
    FACID_ADD <- union_all (WOID_FAC_UNIQ_REL,WOID_FAC_UNIQ_Add_REL)
    FACID_ADD_XY <- union_all (FACID_ADD, WOID_Based_XY)
    FACID_ADD_XY <- unique(FACID_ADD_XY)
# Place holder for writing the final table in Mars database 
  
   
