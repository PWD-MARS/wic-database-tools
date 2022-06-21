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
      sf::sf_use_s2(FALSE)
      




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
      
      Parcels_WIC_Filterd <- inner_join(PARCELS_SPATIAL, WIC_ID_TABLE, by = c("FACILITYID"="facility_id") )
      
      rm(PARCELS_SPATIAL)
      
      Parcels_filtered_df <-as.data.frame(Parcels_WIC_Filterd)
      
      Parcels_filtered_df$wo_initiatedate <- as.Date(Parcels_filtered_df$wo_initiatedate)
      
      
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
      
      SMP_SYS <- SMP
      
  # combine sf object (SMPs) for each system id
      
      SMP_SYS['system_id'] <- gsub('-\\d+$','',SMP_SYS$SMP_ID ) 
      
      system_list <- SMP_SYS %>% 
        select(system_id) %>%
        st_set_geometry(NULL)%>%
        distinct()
      
      system_sf_all <- NULL
      for (i in 1:nrow(system_list)){
        
        id <- system_list[i,1]
        sys_merge <- SMP_SYS %>% filter(system_id == id)
        SMP_combined <- st_union(sys_merge, by_feature = FALSE)
        system_sf <- bind_cols(id,SMP_combined)
        system_sf_all <- bind_rows(system_sf_all, system_sf)
        
      }
      system_sf_all <- st_as_sf(system_sf_all)
      st_crs (system_sf_all) <- 2272
      names(system_sf_all) <-c("system_id","SHAPE")
      st_geometry(system_sf_all) <- "SHAPE"

      
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
      
      GSI <- SMP
      
      Buffer <- 25
      
      output <- NULL
      
      df <- NULL
      
      for(i in 1:length(Inters_Obj)) {
        
      temp <- Inters_Obj[[i]]
       
        if (length(temp) > 0) {
          
            
        FACI_ID <- Parcels_WIC_Filterd[temp, "FACILITYID"] 
        
        WO_ID <- Parcels_WIC_Filterd[temp, "workorder_id"]
        
        WO_Date <- Parcels_WIC_Filterd[temp, "wo_initiatedate"]
                 
        SMPID <- GSI [i,"SMP_ID"]
        
        FACI_ID <- FACI_ID %>% st_set_geometry(NULL)
        
        WO_ID <- WO_ID %>% st_set_geometry(NULL)
        
        WO_Date <- WO_Date %>% st_set_geometry(NULL)
        
        SMPID <- SMPID %>% st_set_geometry(NULL)
        
        
                 
        df <- data.frame(SMPID, FACI_ID, Buffer, WO_ID, WO_Date)
                 
        output <- rbind(output, df ) 
        
        }
      
      output_25 <- output 
      
      } 
      
      names(output_25) <- c("smp_id", "wic_facility_id","buffer_ft", "workorder_id","wo_initiatedate")
      rownames(output_25) <- NULL
      
      
      
  # buffer 50 ft 
      
      Inters_Obj <- SMP_inters_50
      
      GSI <- SMP
      
      Buffer <- 50
      
      output <- NULL
      
      df <- NULL
      
      for(i in 1:length(Inters_Obj)) {
        
        temp <- Inters_Obj[[i]]
        
        if (length(temp) > 0) {
          
          
          FACI_ID <- Parcels_WIC_Filterd[temp, "FACILITYID"] 
          
          WO_ID <- Parcels_WIC_Filterd[temp, "workorder_id"]
          
          WO_Date <- Parcels_WIC_Filterd[temp, "wo_initiatedate"]
          
          SMPID <- GSI [i,"SMP_ID"]
          
          FACI_ID <- FACI_ID %>% st_set_geometry(NULL)
          
          WO_ID <- WO_ID %>% st_set_geometry(NULL)
          
          WO_Date <- WO_Date %>% st_set_geometry(NULL)
          
          SMPID <- SMPID %>% st_set_geometry(NULL)
          
          
          
          df <- data.frame(SMPID, FACI_ID, Buffer, WO_ID, WO_Date)
          
          output <- rbind(output, df ) 
          
        }
        
        output_50 <- output 
        
      } 
      
      names(output_50) <- c("smp_id", "wic_facility_id","buffer_ft", "workorder_id","wo_initiatedate")
      rownames(output_50) <- NULL
      
  # buffer 100 ft 
      
      Inters_Obj <- SMP_inters_100
      
      GSI <- SMP
      
      Buffer <- 100
      
      output <- NULL
      
      df <- NULL
      
      for(i in 1:length(Inters_Obj)) {
        
        temp <- Inters_Obj[[i]]
        
        if (length(temp) > 0) {
          
          
          FACI_ID <- Parcels_WIC_Filterd[temp, "FACILITYID"] 
          
          WO_ID <- Parcels_WIC_Filterd[temp, "workorder_id"]
          
          WO_Date <- Parcels_WIC_Filterd[temp, "wo_initiatedate"]
          
          SMPID <- GSI [i,"SMP_ID"]
          
          FACI_ID <- FACI_ID %>% st_set_geometry(NULL)
          
          WO_ID <- WO_ID %>% st_set_geometry(NULL)
          
          WO_Date <- WO_Date %>% st_set_geometry(NULL)
          
          SMPID <- SMPID %>% st_set_geometry(NULL)
          
          
          
          df <- data.frame(SMPID, FACI_ID, Buffer, WO_ID, WO_Date)
          
          output <- rbind(output, df ) 
          
        }
        
        output_100 <- output 
        
      } 
      
      names(output_100) <- c("smp_id", "wic_facility_id","buffer_ft", "workorder_id","wo_initiatedate")
      rownames(output_100) <- NULL
    
      
## Section 4: Final processing of the result data frame nad writing to DB 
  #stick them together, name them, add the system id
###Taylor says: Filter to complete cases (ie, no NAs or gaps in data) and write to DB
      
      Result <- bind_rows(output_25, output_50, output_100) 
      
      if (length(Result) > 0) {
        
        names(Result) <- c("smp_id", "wic_facility_id","buffer_ft", "workorder_id","wo_initiatedate")
        
      }
      
      Result <- Result %>% na.omit
      
      Result['system_id'] <- gsub('-\\d+$','',Result$smp_id ) 
      

      
## section 5: categorizing the SMPs to pre, during, or post construction
  
      
      wic_conphase <- data.frame(wic_uid = 1:4, phase = c("pre-construction", "during construction", "post-construction", "unknown"))
      
      external.cipit_project <- dbGetQuery(con, "SELECT * FROM external.cipit_project")
      
      external.smpbdv <- dbGetQuery(con, "SELECT * FROM  external.smpbdv")
      
      worknumber <- inner_join(Result,external.smpbdv, by = "smp_id" )  %>% select( smp_id, wic_facility_id,buffer_ft, workorder_id,wo_initiatedate, worknumber)
      
      smp_milestones <- inner_join(external.cipit_project, worknumber, by = c("work_number" = "worknumber"  ))  %>% select (smp_id, wic_facility_id,buffer_ft, workorder_id,wo_initiatedate, construction_start_date, pc_ntp_date, construction_complete_date, contract_closed_date) %>% unique()
      
      
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
      
      fieldwork.wic_smps <- smp_milestones %>% select(workorder_id,smp_id, wic_facility_id, buffer_ft, phase_lookup_uid)

      fieldwork.wic_smps['system_id'] <- gsub('-\\d+$','',fieldwork.wic_smps$smp_id ) 
      
      Parcels_WIC_Filterd <- Parcels_WIC_Filterd %>%
        filter(Parcels_WIC_Filterd$workorder_id %in% smp_milestones$workorder_id)
      
      Parcels_WIC_Filterd <- inner_join(Parcels_WIC_Filterd, smp_milestones, by =c("FACILITYID"="wic_facility_id")) %>%
        select(ADDRESS, smp_id, buffer_ft)%>%
        distinct()
      
        
      SMP <- SMP %>% 
        filter(SMP$SMP_ID %in% smp_milestones$smp_id ) 
      
      SMP <- st_transform(SMP, 4326)
      Parcels_WIC_Filterd <- st_transform(Parcels_WIC_Filterd, 4326)
      
      SMPtext <- SMP %>% st_geometry() %>% st_as_text()
      Parcel_text <- Parcels_WIC_Filterd %>% st_geometry() %>% st_as_text()
      
      Parcel_address <- Parcels_WIC_Filterd %>% st_set_geometry(NULL)
      SMP_ID <- SMP["SMP_ID"] %>% st_set_geometry(NULL)
      
      SMP_ID['WKT'] <- SMPtext
      Parcel_address['WKT'] <-Parcel_text
      Parcel_address <- distinct(Parcel_address)
      
 
      
## Section 6: Writing results to DB
      
      dbWriteTable (con, SQL("fieldwork.wic_smps"),fieldwork.wic_smps,append= TRUE, row.names = FALSE)
      
      dbWriteTable (con, SQL("fieldwork.wic_conphase"),wic_conphase,append= TRUE, row.names = FALSE)
      
      dbWriteTable (con, SQL("fieldwork.wic_smp_mapid"),SMP_ID,append= TRUE, row.names = FALSE)
    
      dbWriteTable (con, SQL("fieldwork.wic_parcels_address"),Parcel_address,append= TRUE, row.names = FALSE)
      
      
      dbDisconnect(con)
        

      
      
      
      
      
      
      
      
      
