# Connect to MARS DB and get the SMP IDs and Facility IDs from external.assets

      con <- dbConnect(odbc(), dsn = "mars_data")
      SMP_FAC_ID <- dbGetQuery(con, "SELECT facility_id, smp_id FROM external.assets WHERE component_id is NULL ")
      
      
# COnnect to GSI DB 
      
      dsn_infra_pub <- paste0("MSSQL:server=PWDGISSQL;",
                              "database=INFRASTRUCTURE_PUBLIC;",
                              "UID=gisread;",
                              "PWD=gisread;")