#Written by: Farshad Ebrahimi_4/18/2022
# Connect to MARS DB and get the SMP IDs and Facility IDs from external.assets.

      con <- dbConnect(odbc(), dsn = "mars_data")
      SMP_FAC_ID <- dbGetQuery(con, "SELECT facility_id, smp_id FROM external.assets WHERE component_id is NULL ")
      
      
# COnnect to GSI DB and get asin, blue roof, bumpout, cistern, drainage well, green roof, permeable pavement, 
# planter, rain garden, swale, tree trench, trench, and wetland
      
      dsn_infra_pub <- paste0("MSSQL:server=PWDGISSQL;",
                              "database=INFRASTRUCTURE_PUBLIC;",
                              "UID=gisread;",
                              "PWD=gisread;")
      
      
      basin <- suppressWarnings(st_read(dsn_infra_pub, "gisad.GSWIBASIN", quiet = TRUE)) 
       
      blueroof <- suppressWarnings(st_read(dsn_infra_pub, "gisad.GSWIBLUEROOF", quiet = TRUE)) 
      
      bumpout <- suppressWarnings(st_read(dsn_infra_pub, "gisad.GSWIBUMPOUT", quiet = TRUE)) 
      
      cistern <- suppressWarnings(st_read(dsn_infra_pub, "gisad.GSWICISTERN", quiet = TRUE)) 
      
# something wrong with drainagewell data (looks empty in the GIS DB when I looked at ArcCatalog)
      
      #drainagewell <- suppressWarnings(st_read(dsn_infra_pub, "gisad.GSWIDRAINAGEWELL", quiet = TRUE))
      
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
        
# something wrong with drainagewell data (looks empty in the GIS DB when I looked at ArcCatalog)
      
        #st_crs (drainagewell) <- 2272
      
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
      
# something wrong with drainagewell data (looks empty in the GIS DB when I looked at ArcCatalog)
      
     # SMP <- bind_rows(basin, blueroof, bumpout, cistern, drainagewell, greenroof, permeablepavement, planter, raingarden, swale, treetrench, trench, wetland)
        
      SMP <- bind_rows(basin, blueroof, bumpout, cistern, greenroof, permeablepavement, planter, raingarden, swale, treetrench, trench, wetland)
      
        
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
       
                if (length(temp) > 0) {
                 FACI_ID <- Parcels_filtered_df[temp, "FACILITYID"]
                 SMPID <- GSI [i,"SMP_ID"]
                 SMPID_Vec <- rep(SMPID, length(temp))
                 Buffer_Vec <-  rep(Buffer, length(temp))
                 
                 df <- data.frame(SMPID_Vec, FACI_ID, Buffer_Vec)
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
      for(i in 1:length(Inters_Obj)){
        
        temp <- Inters_Obj[[i]]
        
        if (length(temp) > 0) {
          FACI_ID <- Parcels_filtered_df[temp, "FACILITYID"]
          SMPID <- GSI [i,"SMP_ID"]
          SMPID_Vec <- rep(SMPID, length(temp))
          Buffer_Vec <-  rep(Buffer, length(temp))
          
          df <- data.frame(SMPID_Vec, FACI_ID, Buffer_Vec)
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
      for(i in 1:length(Inters_Obj)){
        
        temp <- Inters_Obj[[i]]
        
        if (length(temp) > 0) {
          FACI_ID <- Parcels_filtered_df[temp, "FACILITYID"]
          SMPID <- GSI [i,"SMP_ID"]
          SMPID_Vec <- rep(SMPID, length(temp))
          Buffer_Vec <-  rep(Buffer, length(temp))
          
          df <- data.frame(SMPID_Vec, FACI_ID, Buffer_Vec)
          output <- rbind(output, df ) 
        }
        output_100 <-output
        
        
      }
    
      
# Final processing of the result data frame - stick them together, name them, add the system id
      
      Result <- bind_rows(output_25, output_50, output_100)
      if (length(Result) > 0) {
      names(Result) <- c("SMP_ID", "WIC_PARCEL_FACILITYID","Buffer") }
      Result['SYSTEM_ID'] <- gsub('-\\d+$','',Result$SMP_ID )

      
      
      
      
      
      
      
      
      
