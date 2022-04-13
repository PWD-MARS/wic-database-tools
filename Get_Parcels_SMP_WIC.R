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
      
      
#set 25 ft buffer around the SMPs
      
      
      
      basin_buffer <- st_buffer(basin, 25)
      
      blueroof_buffer <- st_buffer(blueroof, 25)
      
      bumpout_buffer <- st_buffer(bumpout, 25)
      
      cistern_buffer <- st_buffer(cistern, 25)
      
      drainagewell_buffer <- st_buffer(drainagewell, 25)
      
      greenroof_buffer <- st_buffer(greenroof, 25)
      
      permeablepavement_buffer <- st_buffer(permeablepavement, 25)
      
      planter_buffer <- st_buffer(planter, 25)
      
      raingarden_buffer <- st_buffer(raingarden, 25)
      
      swale_buffer <- st_buffer(swale, 25)
      
      treetrench_buffer <- st_buffer(treetrench, 25)
      
      trench_buffer <- st_buffer(trench, 25)
      
      wetland_buffer <- st_buffer(wetland, 25)
#INTERSECT the WIC with buffered GSIs
      
      Basin_inters <- st_intersects(basin_buffer, Parcels_WIC_Filterd) 
      
      blueroof_inters <- st_intersects(blueroof_buffer,Parcels_WIC_Filterd) 
      
      bumpout_inters<- st_intersects(bumpout_buffer,Parcels_WIC_Filterd)
      
      cistern_inters <- st_intersects(cistern_buffer,Parcels_WIC_Filterd) 
      
      drainagewell_inters <-st_intersects(drainagewell_buffer, Parcels_WIC_Filterd)
      
      greenroof_inters <- st_intersects (greenroof_buffer, Parcels_WIC_Filterd) 
      
      permeablepavement_inters <- st_intersects(permeablepavement_buffer,Parcels_WIC_Filterd) 
      
      planter_inters  <-  st_intersects(planter_buffer , Parcels_WIC_Filterd) 
      
      raingarden_inters <- st_intersects(raingarden_buffer, Parcels_WIC_Filterd) 
      
      swale_inters <-  st_intersects(swale_buffer , Parcels_WIC_Filterd)
      
      treetrench_inters <- st_intersects(treetrench_buffer , Parcels_WIC_Filterd) 
      
      trench_inters <- st_intersects(trench_buffer , Parcels_WIC_Filterd)
      
      wetland_inters <- st_intersects(wetland_buffer,Parcels_WIC_Filterd)
      
      