
### Section 1: data gathering


      library(DBI)
      library(odbc)
      library(dplyr)
      library(RPostgreSQL)
      library(sf)
      library(tidyr)
      sf::sf_use_s2(FALSE)
      
      con <- dbConnect(odbc::odbc(), dsn = "mars_data")
      smp <- dbGetQuery(con, "SELECT * FROM fieldwork.wic_smp_mapid ")
      parcel <- dbGetQuery(con, "SELECT * FROM fieldwork.wic_parcels_address")
      
    # map geoprocessing
      
      smp_sf <- st_as_sfc(smp[,2], CRS = 4326)
      parcel_sf <- st_as_sfc(parcel[,4], crs = 4326)
      
      smp_ids <- smp %>% select(SMP_ID)
      smp_spatial <- bind_cols(smp_ids, smp_sf)
      smp_spatial <- st_as_sf(smp_spatial)
      st_crs(smp_spatial) <- 4326
      
      parcel_address <- parcel %>% select(-WKT)
      parcel_spatial <- bind_cols(parcel_address, parcel_sf)
      parcel_spatial <- st_as_sf(parcel_spatial)
      st_crs(parcel_spatial) <- 4326
      
      smp_spatial['system_id'] <- gsub('-\\d+$','',smp_spatial$SMP_ID) 
      parcel_spatial['system_id'] <- gsub('-\\d+$','',parcel_spatial$smp_id ) 
      parcel_address['system_id'] <- gsub('-\\d+$','',parcel_address$smp_id) 
      
      
      

  # distnace calculation
    
    dist <- as.data.frame (matrix(NA, nrow = nrow(parcel_spatial), ncol = 1))
    names(dist) <- "distance_ft"
    
    for (i in 1:nrow(parcel_spatial)){
      
      dist[i,1] <- st_distance(parcel_spatial[i,], smp_spatial %>%
                                 filter(SMP_ID ==parcel_address[i,"smp_id"]))/0.3048
      
    }
    parcel["dist"] <- dist
    names(parcel) <- c("address","smp_id","buffer_ft","wkt","distance_ft")
    parcel <- parcel[,c("smp_id","address","buffer_ft","distance_ft","wkt")]
    
    names(smp) <- c("smp_id","wkt")
    
    
  # Write to table fieldwork.wic_parcels_address
    
    dbWriteTable (con, SQL("fieldwork.wic_parcels_wkt"), parcel, append= TRUE, row.names = FALSE)
    dbWriteTable (con, SQL("fieldwork.wic_smp_wkt"), smp, append= TRUE, row.names = FALSE)
    
    dbDisconnect(con)
    
    
    
    
