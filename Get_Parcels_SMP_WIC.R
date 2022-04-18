# Connect to MARS DB and get the SMP IDs and Facility IDs from external.assets

      con <- dbConnect(odbc(), dsn = "mars_data")
      SMP_FAC_ID <- dbGetQuery(con, "SELECT facility_id, smp_id FROM external.assets WHERE component_id is NULL ")
      
      
# COnnect to GSI DB and get asin, blue roof, bumpout, cistern, drainage well, green roof, permeable pavement, 
# planter, rain garden, swale, tree trench, trench, and wetland
      
      dsn_infra_pub <- paste0("MSSQL:server=PWDGISSQL;",
                              "database=INFRASTRUCTURE_PUBLIC;",
                              "UID=gisread;",
                              "PWD=gisread;")
      
      
      basin <- suppressWarnings(st_read(dsn_infra_pub, "gisad.GSWIBASIN_WGS84", quiet = TRUE)) 
       
      blueroof <- suppressWarnings(st_read(dsn_infra_pub, "gisad.GSWIBLUEROOF_WGS84", quiet = TRUE)) 
      
      bumpout <- suppressWarnings(st_read(dsn_infra_pub, "gisad.GSWIBUMPOUT_WGS84", quiet = TRUE)) 
      
      cistern <- suppressWarnings(st_read(dsn_infra_pub, "gisad.GSWICISTERN_WGS84", quiet = TRUE)) 
      
      drainagewell <- suppressWarnings(st_read(dsn_infra_pub, "gisad.GSWIDRAINAGEWELL_WGS84", quiet = TRUE))
      
      greenroof <- suppressWarnings(st_read(dsn_infra_pub, "gisad.GSWIGREENROOF_WGS84", quiet = TRUE))
      
      permeablepavement <- suppressWarnings(st_read(dsn_infra_pub, "gisad.GSWIPERMEABLEPAVEMENT_WGS84", quiet = TRUE))
      
      planter <- suppressWarnings(st_read(dsn_infra_pub, "gisad.GSWIPLANTER_WGS84", quiet = TRUE))
      
      raingarden <- suppressWarnings(st_read(dsn_infra_pub, "gisad.GSWIRAINGARDEN_WGS84", quiet = TRUE))
      
      swale <- suppressWarnings(st_read(dsn_infra_pub, "gisad.GSWISWALE_WGS84", quiet = TRUE))
      
      treetrench <- suppressWarnings(st_read(dsn_infra_pub, "gisad.GSWITREETRENCH_WGS84", quiet = TRUE))
      
      trench <- suppressWarnings(st_read(dsn_infra_pub, "gisad.GSWITRENCH_WGS84", quiet = TRUE)) 
      
      wetland <- suppressWarnings(st_read(dsn_infra_pub, "gisad.GSWIWETLAND_WGS84", quiet = TRUE))
      
        st_crs ( basin) <- 2272 
      
        st_crs (blueroof) <- 2272
      
        st_crs(bumpout) <- 2272
      
        st_crs(cistern) <- 2272
      
        st_crs (drainagewell) <- 2272
      
        st_crs (greenroof) <- 2272
      
        st_crs (permeablepavement) <- 2272
      
        st_crs (planter) <- 2272
      
        st_crs(raingarden) <- 2272
      
        st_crs (swale) <- 2272
      
        st_crs(treetrench) <- 2272
      
        st_crs(trench) <- 2272
      
        st_crs (wetland) <- 2272
      
      
#Get the parcel layer 
      
      PARCELS_SPATIAL <- st_read(dsn = "P:\\Watershed Sciences\\GSI Monitoring\\09 GIS Data\\PWD_PARCELS ", layer = "PWD_PARCELS")
      st_crs(PARCELS_SPATIAL) = 2272
      
      
# filter to get WIC associated polygons and delete the layer
      
      
      WIC_ID_TABLE <- dbGetQuery(con, "SELECT * from fieldwork.gis_parcels")
      WIC_ID <- WIC_ID_TABLE[,"FACILITYID"]
      PARCELS_SPATIAL$FACILITYID<-gsub("{","",as.character(PARCELS_SPATIAL$FACILITYID), fixed=TRUE)
      PARCELS_SPATIAL$FACILITYID<-gsub("}","",as.character(PARCELS_SPATIAL$FACILITYID), fixed=TRUE)
      Parcels_WIC_Filterd <- PARCELS_SPATIAL [PARCELS_SPATIAL$FACILITYID %in% WIC_ID, ]
      rm(PARCELS_SPATIAL)
      Parcels_filtered_df <-as.data.frame(Parcels_WIC_Filterd)
      
      
      
      
      
      
# Drop columns except the SMP_ID  and Merge all SMPs
      
      basin <- basin %>% select(SMP_ID)
      
      blueroof<- blueroof %>% select(SMP_ID)
      
      bumpout <- bumpout %>% select(SMP_ID)
      
      cistern <- cistern %>% select(SMP_ID)
      
      drainagewell <- drainagewell %>% select(SMP_ID)
      
      greenroof<- greenroof %>% select(SMP_ID)
      
      permeablepavement <- permeablepavement %>% select(SMP_ID)
      
      planter<- planter %>% select(SMP_ID)
      
      raingarden <- raingarden %>% select(SMP_ID)
      
      swale <- swale %>% select(SMP_ID)
      
      treetrench <- treetrench %>% select(SMP_ID)
      
      trench<- trench %>% select(SMP_ID)
      
      wetland <- wetland %>% select(SMP_ID)
      
      SMP <- bind_rows(basin, blueroof, bumpout, cistern, drainagewell, greenroof, permeablepavement, planter, raingarden, swale, treetrench, trench, wetland)
        
        
        
#set 25 ft buffer around the SMPs
      
      
      SMP_buffer_25 <- st_buffer(SMP, 25)
      SMP_buffer_50 <- st_buffer(SMP, 50)
      SMP_buffer_100 <- st_buffer(SMP, 100)
      
    
#INTERSECT the WIC with buffered GSIs
      
      SMP_inters_25 <- st_intersects(SMP_buffer_25, Parcels_WIC_Filterd)
      SMP_inters_50 <- st_intersects(SMP_buffer_50, Parcels_WIC_Filterd)
      SMP_inters_100 <- st_intersects(SMP_buffer_100, Parcels_WIC_Filterd)
      
      
# create a conditional loop, that loops through each element of the list column of intersect (sparse matrix) , gets the indeces of the 
# WIC_parcels, and populates a data frame, consisting the SMP_ID, FACILITYID of the wic -parcel, and the size of buffer
      
    
# buffer 25 ft
      Inters_Obj <- SMP_inters_25
      GSI <- as.data.frame(SMP)
      Buffer <- 25
      output <- NULL
      
      df <- NULL
          for(i in 1:length(Inters_Obj)){
        
             temp <- Inters_Obj[[i]]
       
                if (is.null(temp) != TRUE) {
                 FACI_ID <- Parcels_filtered_df[temp, "FACILITYID"]
                 SMPID <- GSI [i,"SMP_ID"]
                 SMPID_Vec <- rep(SMPID, length(temp))
                 Buffer_Vec <-  rep(Buffer, length(temp))
                 
                 df <- data.frame(SMPID_Vec, FACI_ID, Buffer_Vec)
                 output <- rbind(output, df ) }
                 names(output) <- c("SMP_ID", "FACILITYID","Buffer")
                 output_25 <-output
       
       
                                           }
      
      output_25
      
      
# buffer 50 ft 
      
      Inters_Obj <- SMP_inters_50
      GSI <- as.data.frame(SMP)
      Buffer <- 50
      output <- NULL
      
      df <- NULL
      for(i in 1:length(Inters_Obj)){
        
        temp <- Inters_Obj[[i]]
        
        if (is.null(temp) != TRUE) {
          FACI_ID <- Parcels_filtered_df[temp, "FACILITYID"]
          SMPID <- GSI [i,"SMP_ID"]
          SMPID_Vec <- rep(SMPID, length(temp))
          Buffer_Vec <-  rep(Buffer, length(temp))
          
          df <- data.frame(SMPID_Vec, FACI_ID, Buffer_Vec)
          output <- rbind(output, df ) }
        names(output) <- c("SMP_ID", "FACILITYID","Buffer")
        output_50 <-output
        
        
      }
      
      output_50
      
      
# buffer 100 ft 
      
      Inters_Obj <- SMP_inters_100
      GSI <- as.data.frame(SMP)
      Buffer <- 100
      output <- NULL
      
      df <- NULL
      for(i in 1:length(Inters_Obj)){
        
        temp <- Inters_Obj[[i]]
        
        if (is.null(temp) != TRUE) {
          FACI_ID <- Parcels_filtered_df[temp, "FACILITYID"]
          SMPID <- GSI [i,"SMP_ID"]
          SMPID_Vec <- rep(SMPID, length(temp))
          Buffer_Vec <-  rep(Buffer, length(temp))
          
          df <- data.frame(SMPID_Vec, FACI_ID, Buffer_Vec)
          output <- rbind(output, df ) }
        names(output) <- c("SMP_ID", "FACILITYID","Buffer")
        output_100 <-output
        
        
      }
      
      output_100