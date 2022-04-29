#Associating parcels with GSI
#Written by: Farshad Ebrahimi- 4/22/2022.


# Connect to MARS DB and get the SMP IDs and Facility IDs from external.assets.


## Section 1: Gather the SMP layers and Parcels shapefile



      library(DBI)
      library(RPostgreSQL)
      library(RPostgres)
      library(odbc)
      library(dplyr)
      library(sf)
      library(tidyr)




      con <- dbConnect(odbc(), dsn = "mars_data")
      SMP_FAC_ID <- dbGetQuery(con, "SELECT facility_id, smp_id FROM external.assets WHERE component_id is NULL ")
      
      
# Connect to GSI DB and get asin, blue roof, bumpout, cistern, drainage well, green roof, permeable pavement, 
# planter, rain garden, swale, tree trench, trench, and wetland
      
      dsn_infra_pub <- paste0("MSSQL:server=PWDGISSQL;",
                              "database=INFRASTRUCTURE_PUBLIC;",
                              "UID=gisread;",
                              "PWD=gisread;")
      
      
      basin <- suppressWarnings(st_read(dsn_infra_pub, "gisad.GSWIBASIN", quiet = TRUE)) 
       
      blueroof <- suppressWarnings(st_read(dsn_infra_pub, "gisad.GSWIBLUEROOF", quiet = TRUE)) 
      
      bumpout <- suppressWarnings(st_read(dsn_infra_pub, "gisad.GSWIBUMPOUT", quiet = TRUE)) 
      
      cistern <- suppressWarnings(st_read(dsn_infra_pub, "gisad.GSWICISTERN", quiet = TRUE)) 
      
  # something wrong with drainagewell 
      
      #drainagewell <- suppressWarnings(st_read(dsn_infra_pub, "GISAD.gswiDrainageWell", quiet = TRUE))
      
      greenroof <- suppressWarnings(st_read(dsn_infra_pub, "gisad.GSWIGREENROOF", quiet = TRUE))
      
      permeablepavement <- suppressWarnings(st_read(dsn_infra_pub, "gisad.GSWIPERMEABLEPAVEMENT", quiet = TRUE))
      
      planter <- suppressWarnings(st_read(dsn_infra_pub, "gisad.GSWIPLANTER", quiet = TRUE))
      
      raingarden <- suppressWarnings(st_read(dsn_infra_pub, "gisad.GSWIRAINGARDEN", quiet = TRUE))
      
      swale <- suppressWarnings(st_read(dsn_infra_pub, "gisad.GSWISWALE", quiet = TRUE))
      
      treetrench <- suppressWarnings(st_read(dsn_infra_pub, "gisad.GSWITREETRENCH", quiet = TRUE))
      
      trench <- suppressWarnings(st_read(dsn_infra_pub, "gisad.GSWITRENCH", quiet = TRUE)) 
      
      wetland <- suppressWarnings(st_read(dsn_infra_pub, "gisad.GSWIWETLAND", quiet = TRUE))
      
      st_crs ( basin) <- 2272 
      
      st_crs (blueroof) <- 2272
      
      st_crs(bumpout) <- 2272
      
      st_crs(cistern) <- 2272
        
  # something wrong with drainagewell 
      
      #st_crs (drainagewell) <- 2272
      
      st_crs (greenroof) <- 2272
      
      st_crs (permeablepavement) <- 2272
      
      st_crs (planter) <- 2272
      
      st_crs(raingarden) <- 2272
      
      st_crs (swale) <- 2272
      
      st_crs(treetrench) <- 2272
      
      st_crs(trench) <- 2272
      
      st_crs (wetland) <- 2272
      
      
  # Get the parcel layer-Shapefile stored in network 
      
      PARCELS_SPATIAL <- st_read(dsn = "\\\\pwdoows\\oows\\Watershed Sciences\\GSI Monitoring\\09 GIS Data\\PWD_PARCELS ", layer = "PWD_PARCELS")
      
      st_crs(PARCELS_SPATIAL) = 2272
      
      
  # filter to get WIC associated polygons and delete the layer
      
      
      WIC_ID_TABLE <- dbGetQuery(con, "SELECT * from fieldwork.wic_parcels")
      
      PARCELS_SPATIAL$FACILITYID<-gsub("\\{(.*)\\}","\\1",as.character(PARCELS_SPATIAL$FACILITYID))
      
      Parcels_WIC_Filterd <- inner_join(PARCELS_SPATIAL, WIC_ID_TABLE, by = "FACILITYID") 
      
      rm(PARCELS_SPATIAL)
      
      Parcels_filtered_df <-as.data.frame(Parcels_WIC_Filterd)
      
      Parcels_filtered_df$WO_INITIATEDATE <- as.Date(Parcels_filtered_df$WO_INITIATEDATE)
      
      
  # Drop columns except the SMP_ID and Merge all SMPs
      
      basin <- basin %>% select(SMP_ID)
      
      blueroof<- blueroof %>% select(SMP_ID)
      
      bumpout <- bumpout %>% select(SMP_ID)
      
      cistern <- cistern %>% select(SMP_ID)
      
      #drainagewell <- drainagewell %>% select(SMP_ID)
      
      greenroof<- greenroof %>% select(SMP_ID)
      
      permeablepavement <- permeablepavement %>% select(SMP_ID)
      
      planter<- planter %>% select(SMP_ID)
      
      raingarden <- raingarden %>% select(SMP_ID)
      
      swale <- swale %>% select(SMP_ID)
      
      treetrench <- treetrench %>% select(SMP_ID)
      
      trench<- trench %>% select(SMP_ID)
      
      wetland <- wetland %>% select(SMP_ID)
      
  # something wrong with drainagewell 
      
     # SMP <- bind_rows(basin, blueroof, bumpout, cistern, drainagewell, greenroof, permeablepavement, planter, raingarden, swale, treetrench, trench, wetland)
        
      SMP <- bind_rows(basin, blueroof, bumpout, cistern, greenroof, permeablepavement, planter, raingarden, swale, treetrench, trench, wetland)
      
      
  # filter SMPs to non-private ones
      
      Boolean <- grepl ("\\d+-\\d+-\\d+", SMP[["SMP_ID"]])
      
      SMPID_IND <- which(as.numeric(Boolean)!=0, T)
      
      SMP <- SMP[SMPID_IND, ]
      
      
## Section 2: Create buffers (25, 50, and 100 ft) around SMPs and intersect them with the WIC Parcels
      
        
  #set 25, 50, 100 ft buffer around the SMPs
      
      
      SMP_buffer_25 <- st_buffer(SMP, 25)
      
      SMP_buffer_50 <- st_buffer(SMP, 50)
      
      SMP_buffer_100 <- st_buffer(SMP, 100)
      
    
  #INTERSECT the WIC with buffered SMPs
      
      SMP_inters_25 <- st_intersects(SMP_buffer_25, Parcels_WIC_Filterd)
      
      SMP_inters_50 <- st_intersects(SMP_buffer_50, Parcels_WIC_Filterd)
      
      SMP_inters_100 <- st_intersects(SMP_buffer_100, Parcels_WIC_Filterd)
      
      
      
## Section 3: Extract the intersecting SMP-PARCEL and populate the result dataframe 
      
      
  # create a conditional loop, that loops through each element of the list column of intersect (sparse matrix) , gets the indexes of the 
  # WIC_parcels, and populates a data frame, consisting the SMP_ID, FACILITYID of the wic -parcel, and the size of buffer
      
    
  # buffer 25 ft

      Inters_Obj <- SMP_inters_25
      
      GSI <- as.data.frame(SMP)
      
      Buffer <- 25
      
      output <- NULL
      
      df <- NULL
      
      for(i in 1:length(Inters_Obj)) {
        
      temp <- Inters_Obj[[i]]
       
        if (length(temp) > 0) {
          
            
        FACI_ID <- Parcels_filtered_df[temp, "FACILITYID"]
        
        WO_ID <- Parcels_filtered_df[temp, "WORKORDERID"]
        
        WO_Date <- Parcels_filtered_df[temp, "WO_INITIATEDATE"]
        
        
                 
        SMPID <- GSI [i,"SMP_ID"]
                 
        SMPID_Vec <- rep(SMPID, length(temp))
                 
        Buffer_Vec <-  rep(Buffer, length(temp))
                 
        df <- data.frame(SMPID_Vec, FACI_ID, Buffer_Vec, WO_ID, WO_Date)
                 
        output <- rbind(output, df ) 
        
        }
      
      output_25 <-output
       
      } 
      
      
  # buffer 50 ft 
      
      Inters_Obj <- SMP_inters_50
      
      GSI <- as.data.frame(SMP)
      
      Buffer <- 50
      
      output <- NULL
      
      df <- NULL
      
      for(i in 1:length(Inters_Obj)) {
        
      temp <- Inters_Obj[[i]]
        
        if (length(temp) > 0) {
          FACI_ID <- Parcels_filtered_df[temp, "FACILITYID"]
          
          WO_ID <- Parcels_filtered_df[temp, "WORKORDERID"]
          
          WO_Date <- Parcels_filtered_df[temp, "WO_INITIATEDATE"]
          
          SMPID <- GSI [i,"SMP_ID"]
          
          SMPID_Vec <- rep(SMPID, length(temp))
          
          Buffer_Vec <-  rep(Buffer, length(temp))
          
          df <- data.frame(SMPID_Vec, FACI_ID, Buffer_Vec, WO_ID, WO_Date)
          
          output <- rbind(output, df ) 
        }
        
      output_50 <-output
        
      }
      
  # buffer 100 ft 
      
      Inters_Obj <- SMP_inters_100
      
      GSI <- as.data.frame(SMP)
      
      Buffer <- 100
      
      output <- NULL
      
      df <- NULL
      
      for(i in 1:length(Inters_Obj)) {
        
      temp <- Inters_Obj[[i]]
        
        if (length(temp) > 0) {
          
          FACI_ID <- Parcels_filtered_df[temp, "FACILITYID"]
          
          WO_ID <- Parcels_filtered_df[temp, "WORKORDERID"]
          
          WO_Date <- Parcels_filtered_df[temp, "WO_INITIATEDATE"]
          
          
          SMPID <- GSI [i,"SMP_ID"]
          
          SMPID_Vec <- rep(SMPID, length(temp))
          
          Buffer_Vec <-  rep(Buffer, length(temp))
          
          df <- data.frame(SMPID_Vec, FACI_ID, Buffer_Vec, WO_ID, WO_Date)
          
          output <- rbind(output, df ) 
          
        }
      
        output_100 <-output
        
      }
    
      
## Section 4: Final processing of the result data frame nad writing to DB 
  #stick them together, name them, add the system id
###Taylor says: Filter to complete cases (ie, no NAs or gaps in data) and write to DB
      
      Result <- bind_rows(output_25, output_50, output_100) 
      
      if (length(Result) > 0) {
        
        names(Result) <- c("smp_id", "wic_parcel_facilityid","buffer", "workorderid","wo_initiatedate")
        
      }
      
      Result <- Result %>% na.omit
      
      Result['SYSTEM_ID'] <- gsub('-\\d+$','',Result$smp_id ) 
      

      
## section 5: categorizing the SMPs to pre, during, or post construction
  
      
      wic_conphase <- data.frame(wic_uid = 1:4, phase = c("pre-construction", "during construction", "post-construction", "unknown"))
      
      external.cipit_project <- dbGetQuery(con, "SELECT * FROM external.cipit_project")
      
      external.smpbdv <- dbGetQuery(con, "SELECT * FROM  external.smpbdv")
      
      worknumber <- inner_join(Result,external.smpbdv, by = "smp_id" )  %>% select( smp_id, wic_parcel_facilityid,buffer, workorderid,wo_initiatedate, worknumber)
      
      smp_milestones <- inner_join(external.cipit_project, worknumber, by = c("work_number" = "worknumber"  ))  %>% select (smp_id, wic_parcel_facilityid,buffer, workorderid, wo_initiatedate, construction_start_date, pc_ntp_date, construction_complete_date, contract_closed_date) %>% unique()
      
      
  #setting the lookup_id's default in smpmilestone to 4
      

      smp_milestones['phase_lookup_uid'] <- 4
      
      for(i in 1:nrow(smp_milestones)) {
        
        if (!is.na(smp_milestones[i, "construction_start_date"]) && !is.na(smp_milestones[i, "construction_complete_date"]) ) {
          
            if (smp_milestones[i, "wo_initiatedate"] >= smp_milestones[i, "construction_start_date"] && smp_milestones[i, "wo_initiatedate"] <= smp_milestones[i, "construction_complete_date"]  ) {
              
              smp_milestones[i, "phase_lookup_uid"] <- 2
              
            } else if (smp_milestones[i, "wo_initiatedate"] < smp_milestones[i, "construction_start_date"]) {
              
              smp_milestones[i, "phase_lookup_uid"] <- 1
              
            } else {
              
              smp_milestones[i, "phase_lookup_uid"] <- 3
            
          }
          
        
        } else if (!is.na(smp_milestones[i, "pc_ntp_date"]) && !is.na(smp_milestones[i, "construction_complete_date"] )) {
          
            if (smp_milestones[i, "wo_initiatedate"] >= smp_milestones[i, "pc_ntp_date"] && smp_milestones[i, "wo_initiatedate"] <= smp_milestones[i, "construction_complete_date"]  ) {
              
              smp_milestones[i, "phase_lookup_uid"] <- 2
              
            } else if (smp_milestones[i, "wo_initiatedate"] < smp_milestones[i, "pc_ntp_date"]) {
              
              smp_milestones[i, "phase_lookup_uid"] <- 1
              
            } else {
              
              smp_milestones[i, "phase_lookup_uid"] <- 3
            
          }
          
        } else if (!is.na(smp_milestones[i, "construction_start_date"]) && !is.na(smp_milestones[i, "contract_closed_date"])) {
        
            if (smp_milestones[i, "wo_initiatedate"] >= smp_milestones[i, "construction_start_date"] && smp_milestones[i, "wo_initiatedate"] <= smp_milestones[i, "contract_closed_date"]  ) {
              
              smp_milestones[i, "phase_lookup_uid"] <- 2
              
            } else if (smp_milestones[i, "wo_initiatedate"] < smp_milestones[i, "construction_start_date"]) {
              
              smp_milestones[i, "phase_lookup_uid"] <- 1
              
            } else {
              
              smp_milestones[i, "phase_lookup_uid"] <- 3
            
          }
        
        
          
        } else if (!is.na(smp_milestones[i, "pc_ntp_date"]) && !is.na(smp_milestones[i, "contract_closed_date"])) {
          
            if (smp_milestones[i, "wo_initiatedate"] >= smp_milestones[i, "pc_ntp_date"] && smp_milestones[i, "wo_initiatedate"] <= smp_milestones[i, "contract_closed_date"]  ) {
              
              smp_milestones[i, "phase_lookup_uid"] <- 2
              
            } else if (smp_milestones[i, "wo_initiatedate"] < smp_milestones[i, "pc_ntp_date"]) {
              
              smp_milestones[i, "phase_lookup_uid"] <- 1
              
            } else {
              
              smp_milestones[i, "phase_lookup_uid"] <- 3
              
            }
          
          
        } else { 
          
              smp_milestones[i, "phase_lookup_uid"] <- 4
          
            
          }
        

      }
      
      fieldwork.wic_smps <- smp_milestones %>% select(workorderid,smp_id, wic_parcel_facilityid, buffer_ft = buffer, phase_lookup_uid)

      fieldwork.wic_smps['SYSTEM_ID'] <- gsub('-\\d+$','',fieldwork.wic_smps$smp_id ) 

      
## Section 6: Writing results to DB
      
      dbWriteTable (con, SQL("fieldwork.wic_smps"),fieldwork.wic_smps)
      
      dbWriteTable (con, SQL("fieldwork.wic_conphase"),wic_conphase)
      
      dbDisconnect(con)
        

      
      
      
      
      
      
      
      
      
