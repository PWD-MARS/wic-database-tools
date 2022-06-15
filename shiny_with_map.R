#### Create the shiny deliverable of the WIC project
#### Written by: Farshad Ebrahimi- 5/3/2022.

### Section 1: data gathering


    library(DBI)
    library(odbc)
    library(dplyr)
    library(shiny)
    library(reactable)
    library(RPostgreSQL)
    library(sf)
    library(leaflet)
    library(leaflet.extras)
    
    
    
    con <- dbConnect(odbc::odbc(), dsn = "mars_data", uid = Sys.getenv("shiny_uid"), pwd = Sys.getenv("shiny_pwd"))
    
    wic_workorders <- dbGetQuery(con, "SELECT * FROM fieldwork.wic_workorders ")
    wic_parcels <- dbGetQuery(con, "SELECT * FROM fieldwork.wic_parcels ")
    wic_smps <- dbGetQuery(con, "SELECT * FROM fieldwork.wic_smps ")
    wic_conphase <- dbGetQuery(con, "SELECT * FROM fieldwork.wic_conphase ")
    
    smp <- dbGetQuery(con, "SELECT * FROM fieldwork.wic_smp_mapid ")
    parcel <- dbGetQuery(con, "SELECT * FROM fieldwork.wic_parcels_address")

### map geoprocessing

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



### Section 2: processing the data into 3 tables (buffers: 25, 50, and 100 ft) and change the data format, column names and their order

    output_25ft <- inner_join(wic_smps, wic_conphase, by=c("phase_lookup_uid"="wic_uid"))%>%
      select(-phase_lookup_uid,-wic_smps_uid) %>%
      inner_join(wic_parcels, by = c("wic_facility_id"="facility_id","workorder_id" = "workorder_id")) %>%
      select(-wic_parcels_uid,-wic_facility_id) 
    
    output_25ft_dl <- output_25ft
    output_25ft_dl$smp_id<- shQuote(output_25ft$smp_id)
    output_25ft_dl$system_id<- shQuote(output_25ft$system_id)
    
    data <- output_25ft %>% 
      select(smp_id)
    names(data) <- "SMP ID"
    
    buffer <- c(25,50,100)
    
    names(output_25ft) <- c("Work Order ID", "SMP_ID", "Buffer_ft", "System ID", "Construction Phase","Address", "Complaint Date")
    
    names(output_25ft_dl) <- c("Work Order ID", "SMP_ID", "Buffer_ft", "System ID", "Construction Phase","Address", "Complaint Date")
    
    output_25ft <- output_25ft[,c("SMP_ID","System ID","Work Order ID", "Construction Phase", "Complaint Date","Address","Buffer_ft")]
    
    output_25ft_dl <- output_25ft_dl[,c("SMP_ID","System ID","Work Order ID", "Construction Phase", "Complaint Date","Address","Buffer_ft")]



### Section 3: shiny work-A navbarpage with two tabs, within tab 1 there will be 3 tabsets with 3 tables of data and corresponding download button on the sidebar

    ui <- fluidPage(
      navbarPage(
        "Water in Cellar (WIC) Complaints Around Public SMPs",   
        tabPanel("WIC-SMP Association ", titlePanel("WIC Complaints within 25, 50, and 100 feet of SMPs"),
                 
                 sidebarLayout(
                   sidebarPanel(selectizeInput(
                     'smp_id', label = 'Type a SMP ID', choices = data, selected = "178-1-1",
                     options = list(maxOptions = 5)
                   ),selectizeInput(
                     'buffer', label = 'Select a Buffer Size (ft)', choices = buffer,selected = 25,
                     options = list(maxOptions = 3)
                   ),downloadButton("WIC_dl_25ft","Download in .CSV")
                   
                   , width = 3
                   ), 
                   mainPanel( leafletOutput("map")
                   )
                 ),
                 reactableOutput("table_25ft")
                 
                 
        ),
        tabPanel("Help Page",verbatimTextOutput("help_text")
        ),
      )
    )
    
    server <- function(input, output) {
      
      
      
      
      output$table_25ft <- renderReactable({
        reactable(filter(output_25ft, SMP_ID == input$smp_id & Buffer_ft == input$buffer), showPageSizeOptions = TRUE, pageSizeOptions = c(5, 10, 15), defaultPageSize = 10,filterable = FALSE
        )
      })
      
  
    output$help_text <- renderText({
      paste("This shiny app provides information about the water-in-cellar complaints recorded in the cityworks database during various stages of SMPs constructions.", 
            "WICs were identified by collecting the work requests that had 'WATER IN CELLAR' in their descriptions. These orders were later matched with their facility ids, addresses and XY coordinates in the GIS database to associate them with parcel polygons (houses, bussinesses, etc.) near them at which water was detected in the cellar.",
            "These WIC-associated parcels were then intersected with public SMPs within 25, 50, and 100 ft of distance from them. WIC complaints were also categorized based on the construction stage of the intersecting SMP at the time when the complaint was filed.",
            "The information contains:",
            "SMP ID: ID of SMP",
            "System ID: system id of SMP",
            "Work Order ID: Work order IDs of WIC complaints",
            "Construction Phase: Construction status of SMP (pre/during/post/unknown) when the WIC complaint has been filed",
            "Complaint Date: WIC complaint date",
            "Address: collected from GIS DB and referes to the addresses associated with the complaining parcels (houses etc)",
            "Buffer (ft): Buffer distance (ft) from an SMP centroid",
            sep="\n")
    })
    
    output$WIC_dl_25ft <- downloadHandler(
      filename = function() {paste("WIC_SMP_25ft_buffer-", Sys.Date(), ".csv", sep="")},
      content = function(file) {write.csv(output_25ft_dl, file,row.names=FALSE)}
    )
    
    
    labels_parcel <- reactive({
      return( filter(parcel_address, smp_id == input$smp_id & buffer_ft == input$buffer) %>% select(ADDRESS))
    })
    
    
    output$map <- renderLeaflet({
      
      leaflet()%>%addTiles(options = providerTileOptions(minZoom = 14, maxZoom = 18))%>%  
        addPolygons(data=filter(smp_spatial, SMP_ID == input$smp_id  ), label = paste("SMP ID:",input$smp_id) , color = "red", group = "SMP") %>%
        ## Had to do label = paste(labels_parcel()[,],""), the only way labels showed correctly 
        addPolygons(data = filter(parcel_spatial, smp_id == input$smp_id & buffer_ft == input$buffer), label = paste(labels_parcel()[,],""),group = "Parcels") %>%
        addLegend(colors = c("red","blue"), labels = c("SMP","Parcel"))
    })
    
    
  }
  
  shinyApp(ui, server)






