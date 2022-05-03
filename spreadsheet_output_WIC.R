#Create the first deliverable of the WIC project
#Written by: Farshad Ebrahimi- 5/3/2022.

### Section 1: data gathering

  # Connect to MARS DB and gather all the data:
  # System ID
  # Parcel address (per Julie's request)
  # WIC complaint date
  # Construction status (pre/mid/post/NA)
  # Buffer distance
  # Work order ID
  # Work order comments)
  

    library(DBI)
    library(odbc)
    library(dplyr)
    library(tidyr)
      
    con <- dbConnect(odbc(), dsn = "mars_data")
    
    wic_workorders <- dbGetQuery(con, "SELECT * FROM fieldwork.wic_workorders ")
    wic_comments <- dbGetQuery(con, "SELECT * FROM fieldwork.wic_comments ")
    wic_parcels <- dbGetQuery(con, "SELECT * FROM fieldwork.wic_parcels ")
    wic_smps <- dbGetQuery(con, "SELECT * FROM fieldwork.wic_smps ")
    wic_conphase <- dbGetQuery(con, "SELECT * FROM fieldwork.wic_conphase ")
    

    
### Section 2: processing the data into 3 tables (buffers: 25, 50, and 100 ft)
    
    output_25ft <- inner_join(wic_smps, wic_conphase, by = c("phase_lookup_uid"="wic_uid") ) %>% select(-phase_lookup_uid,-wic_smps_uid) %>% inner_join(wic_comments, by="workorder_id") %>% select(-wic_comments_uid)%>% 
    inner_join(wic_parcels, by = c("wic_facility_id"="facility_id","workorder_id" = "workorder_id")) %>% select(-wic_parcels_uid,-wic_facility_id) %>% filter(buffer_ft == 25)                                    
 
    output_50ft <- inner_join(wic_smps, wic_conphase, by = c("phase_lookup_uid"="wic_uid") ) %>% select(-phase_lookup_uid,-wic_smps_uid) %>% inner_join(wic_comments, by="workorder_id") %>% select(-wic_comments_uid)%>% 
    inner_join(wic_parcels, by = c("wic_facility_id"="facility_id","workorder_id" = "workorder_id")) %>% select(-wic_parcels_uid,-wic_facility_id) %>% filter(buffer_ft == 50)    
    
    output_100ft <- inner_join(wic_smps, wic_conphase, by = c("phase_lookup_uid"="wic_uid") ) %>% select(-phase_lookup_uid,-wic_smps_uid) %>% inner_join(wic_comments, by="workorder_id") %>% select(-wic_comments_uid)%>% 
    inner_join(wic_parcels, by = c("wic_facility_id"="facility_id","workorder_id" = "workorder_id")) %>% select(-wic_parcels_uid,-wic_facility_id) %>% filter(buffer_ft == 100)                                    
    
    
    