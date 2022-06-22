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
    library(tidyr)
    
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
    smp_spatial['system_id'] <- gsub('-\\d+$','',smp_spatial$SMP_ID) 
    parcel_spatial['system_id'] <- gsub('-\\d+$','',parcel_spatial$smp_id ) 
    parcel_address['system_id'] <- gsub('-\\d+$','',parcel_address$smp_id) 
    
### Section 2: processing the data into 3 tables (buffers: 25, 50, and 100 ft) and change the data format, column names and their order

    output_25ft <- inner_join(wic_smps, wic_conphase, by=c("phase_lookup_uid"="wic_uid"))%>%
      select(-phase_lookup_uid,-wic_smps_uid) %>%
      inner_join(wic_parcels, by = c("wic_facility_id"="facility_id","workorder_id" = "workorder_id")) %>%
      select(-wic_parcels_uid,-wic_facility_id) 
    output_25ft_dl <- output_25ft
    output_25ft_dl$smp_id<- shQuote(output_25ft$smp_id)
    output_25ft_dl$system_id<- shQuote(output_25ft$system_id)
    data <- output_25ft %>% 
      select(system_id) %>%
      distinct()
    names(data) <- "SYSTEM ID"
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
                     'system_id', label = 'Type a System ID', choices = data, selected = data[1],
                     options = list(maxOptions = 5)
                   ),selectizeInput(
                     'buffer', label = 'Select a Buffer Size (ft)', choices = buffer,selected = 100,
                     options = list(maxOptions = 3)
                   ),
                   dateInput('date',label = 'Enter The Starting Date For The Stats Table (SMPs with Highest WICs)',value = "2012-06-06"
                   ),
                    tableOutput("table_stats"),
                   
                   width = 4
                   ), 
                   mainPanel( leafletOutput("map",width = "100%" ,height = "525")
                   )
                 ),
                 reactableOutput("table_25ft"),
                 downloadButton("WIC_dl_25ft","Download Table in .CSV"
                 ),
        ),
        tabPanel("Help Page",verbatimTextOutput("help_text")
        ),
      )
    )
    
    server <- function(input, output) {
      
### Populate stat table
      
      output$table_stats <- renderTable({
          stat <- output_25ft %>% 
            filter(Buffer_ft==100 & `Complaint Date` >= as.character(input$date))%>% 
            group_by(`System ID`)%>% 
            summarise(count = n()) 
          
          if (nrow(stat)==0) {
            validate("There is no WIC complaint for the selected starting date!")
          }
          
          stat <- stat[order(stat$count, decreasing = TRUE), ]
          stat <- stat[1:6, ]
          
          output_stat <- output_25ft %>%
            filter(output_25ft$`System ID` %in% stat$`System ID`) %>%
            filter(Buffer_ft==100 & `Complaint Date` >= as.character(input$date))%>%
            select(`System ID`,`Construction Phase`) %>% 
            group_by(`System ID`, `Construction Phase` )%>% 
            summarise(count = n()) 
          
          output_stat <- output_stat %>% pivot_wider(names_from = `Construction Phase`, values_from = count)
          output_stat <- as.data.frame(output_stat)
          output_stat[is.na(output_stat)] <- 0
          vec_name <- c("System ID","pre-construction","during construction","post-construction","unknown","Total")
          output_stat[vec_name[!(vec_name %in% colnames(output_stat))]] = 0
          output_stat <- output_stat[,vec_name]
          names(output_stat)<- c("System ID","Pre-Con","During-Con","Post-Con","Unknown","Total")
          output_stat$Total <- output_stat$`Pre-Con`+ output_stat$`During-Con`+ output_stat$`Post-Con`+output_stat$Unknown
          output_stat[,2] <- as.integer(output_stat[,2])
          output_stat[,3] <- as.integer(output_stat[,3])
          output_stat[,4] <- as.integer(output_stat[,4])
          output_stat[,5] <- as.integer(output_stat[,5])
          output_stat[,6] <- as.integer(output_stat[,6])
          output_stat <- output_stat[order(output_stat$Total, decreasing = TRUE), ]
          
          
          return(output_stat)
        })
      
      
      output$table_25ft <- renderReactable({
        reactable(filter(output_25ft, `System ID` == input$system_id & Buffer_ft == input$buffer) %>% select(`System ID`, `Work Order ID`, `Construction Phase`,`Complaint Date`, Address), showPageSizeOptions = TRUE, pageSizeOptions = c(10, 25, 50), defaultPageSize = 10,filterable = FALSE
        )
      })
      
    output$help_text <- renderText({
      paste("This shiny app provides information about the water-in-cellar complaints recorded in the cityworks database during various stages of SMPs constructions.", 
            "WICs were identified by collecting the work requests that had 'WATER IN CELLAR' in their descriptions. These orders were later matched with their facility ids, addresses and XY coordinates in the GIS database to associate 
them with parcel polygons (houses, bussinesses, etc.) near them at which water was detected in the cellar.",
            "These WIC-associated parcels were then intersected with public SMPs within 25, 50, and 100 ft of distance from them. WIC complaints were also categorized based on the construction stage of the intersecting SMP at the time 
when the complaint was filed.",
            "The information contains:",
            "",
            "System ID: system id of SMP",
            "Work Order ID: Work order IDs of WIC complaints",
            "Construction Phase: Construction status of SMP (pre/during/post/unknown) when the WIC complaint has been filed",
            "Complaint Date: WIC complaint date",
            "Address: collected from GIS DB and referes to the addresses associated with the complaining parcels (houses etc)",
            "",
            "The stats table shows the SMP systems with the highest number of WICs since the starting date specified by user (default is the date when the first ever WIC was filed",
            "The distances between objects in the map can be measured using the toolkit on the top left of the map",
            sep="\n")
    })
    
    output$WIC_dl_25ft <- downloadHandler(
      filename = function() {paste("WIC_SMP-", Sys.Date(), ".csv", sep="")},
      content = function(file) {write.csv(filter(output_25ft, `System ID` == input$system_id & Buffer_ft == input$buffer), file,row.names=FALSE)}
    )
    
    labels_parcel <- reactive({
      return( filter(parcel_address, system_id == input$system_id & buffer_ft == input$buffer) %>% select(ADDRESS))
    })
    
    output$map <- renderLeaflet({
      
      leaflet()%>%addTiles(options = providerTileOptions(minZoom = 16, maxZoom = 19))%>%  
        addPolygons(data=filter(smp_spatial, system_id == input$system_id ), label = paste("System ID:",input$system_id) , color = "red", group = "SMP System") %>%
        ## Had to do label = paste(labels_parcel()[,],""), the only way labels showed correctly 
        addPolygons(data = filter(parcel_spatial, system_id == input$system_id & buffer_ft == input$buffer), label = paste(labels_parcel()[,],""),group = "Parcels") %>%
        addLegend(colors = c("red","blue"), labels = c("SMP System","Parcel")) %>%
        addDrawToolbar(polylineOptions = drawPolylineOptions(metric = FALSE, feet = TRUE), polygonOptions = FALSE, circleOptions=FALSE,
                       rectangleOptions=FALSE, markerOptions=FALSE, circleMarkerOptions= FALSE,
        editOptions=editToolbarOptions(selectedPathOptions=selectedPathOptions())
        
      )
        
    }) 

  }
  
  shinyApp(ui, server)






