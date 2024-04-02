
### Section 1: data gathering


      library(DBI)
      library(odbc)
      library(dplyr)
      library(RPostgreSQL)
      library(sf)
      library(tidyr)
      sf::sf_use_s2(FALSE)
      
      con <- dbConnect(odbc::odbc(), dsn = "mars14_datav2", uid = Sys.getenv("shiny_uid"), pwd = Sys.getenv("shiny_pwd"), MaxLongVarcharSize = 8190  )
      smp <- dbGetQuery(con, "SELECT * FROM fieldwork.tbl_wic_smp_wkt")
      building_footprint <- dbGetQuery(con, "SELECT * FROM fieldwork.tbl_wic_buildingfootprint_wkt")
      
    # map geoprocessing
      
      smp_sf <- st_as_sfc(smp[,2], CRS = 4326)
      building_footprint_sf <- st_as_sfc(building_footprint[,5], crs = 4326)
      
      smp_ids <- smp %>% select(smp_id)
      smp_spatial <- bind_cols(smp_ids, smp_sf)
      smp_spatial <- st_as_sf(smp_spatial)
      st_crs(smp_spatial) <- 4326
      
      smp_bld_ftprint <- building_footprint %>% select(smp_id)
      bld_spatial <- bind_cols(smp_bld_ftprint, building_footprint_sf)
      bld_spatial <- st_as_sf(bld_spatial)
      st_crs(bld_spatial) <- 4326
      
      smp_spatial['system_id'] <- gsub('-\\d+$','',smp_spatial$smp_id) 
      bld_spatial['system_id'] <- gsub('-\\d+$','',bld_spatial$smp_id ) 

      
      

  # distnace calculation
    
    dist <- as.data.frame (matrix(NA, nrow = nrow(bld_spatial), ncol = 1))
    names(dist) <- "distance_ft"
    
    for (i in 1:nrow(bld_spatial)){
      
      dist[i,1] <- st_distance(bld_spatial[i,], smp_spatial %>%
                                 filter(smp_id ==smp_bld_ftprint[i,"smp_id"]))/0.3048
      
    }
    
    
    building_footprint["distance_footprint_ft"] <- dist
    
    building_footprint <- building_footprint %>%
      select(-wic_buildingfootprint_wkt_uid, -distance_ft)
    
    dbWriteTable(con,Id(schema = "fieldwork", table = "tbl_wic_buildingfootprint_wkt"), building_footprint, append = TRUE )
    
    
