#Create the shiny deliverable of the WIC project
#Written by: Farshad Ebrahimi- 5/3/2022.

### Section 1: data gathering

  # Connect to MARS DB and gather all the data:
  # System ID
  # Parcel address (per Julie's request)
  # WIC complaint date
  # Construction status (pre/mid/post/NA)
  # Buffer distance
  # Work order ID
  # Work order comments)


    library(DBI)
    library(odbc)
    library(dplyr)
    library(tidyr)
    library(xlsx)
    library(shiny)
    library(DT)
    library(reactable)
    
    
    con <- dbConnect(odbc(), dsn = "mars_data")

    wic_workorders <- dbGetQuery(con, "SELECT * FROM fieldwork.wic_workorders ")
    wic_comments <- dbGetQuery(con, "SELECT * FROM fieldwork.wic_comments ")
    wic_parcels <- dbGetQuery(con, "SELECT * FROM fieldwork.wic_parcels ")
    wic_smps <- dbGetQuery(con, "SELECT * FROM fieldwork.wic_smps ")
    wic_conphase <- dbGetQuery(con, "SELECT * FROM fieldwork.wic_conphase ")



### Section 2: processing the data into 3 tables (buffers: 25, 50, and 100 ft) and change the data format

    output_25ft <- inner_join(wic_smps, wic_conphase, by = c("phase_lookup_uid"="wic_uid") ) %>% select(-phase_lookup_uid,-wic_smps_uid) %>% inner_join(wic_comments, by="workorder_id") %>% select(-wic_comments_uid)%>% 
      inner_join(wic_parcels, by = c("wic_facility_id"="facility_id","workorder_id" = "workorder_id")) %>% select(-wic_parcels_uid,-wic_facility_id) %>% filter(buffer_ft == 25)                                    
    
    output_50ft <- inner_join(wic_smps, wic_conphase, by = c("phase_lookup_uid"="wic_uid") ) %>% select(-phase_lookup_uid,-wic_smps_uid) %>% inner_join(wic_comments, by="workorder_id") %>% select(-wic_comments_uid)%>% 
      inner_join(wic_parcels, by = c("wic_facility_id"="facility_id","workorder_id" = "workorder_id")) %>% select(-wic_parcels_uid,-wic_facility_id) %>% filter(buffer_ft == 50)    
    
    output_100ft <- inner_join(wic_smps, wic_conphase, by = c("phase_lookup_uid"="wic_uid") ) %>% select(-phase_lookup_uid,-wic_smps_uid) %>% inner_join(wic_comments, by="workorder_id") %>% select(-wic_comments_uid)%>% 
      inner_join(wic_parcels, by = c("wic_facility_id"="facility_id","workorder_id" = "workorder_id")) %>% select(-wic_parcels_uid,-wic_facility_id) %>% filter(buffer_ft == 100)
    
    output_25ft$wo_initiatedate <- as.Date(output_25ft$wo_initiatedate)
    output_50ft$wo_initiatedate <- as.Date(output_50ft$wo_initiatedate)
    output_100ft$wo_initiatedate <- as.Date(output_100ft$wo_initiatedate)
    
    output_all <- bind_rows(output_25ft,output_50ft,output_100ft)
    
    intro <- "This spreadsheet contains information about the water-in-cellar complaints recorded in the cityworks database during various stages of SMPs constructions. 
        WICs were identified by collecting the work requests that had 'WATER IN CELLAR' in their descriptions. These orders were later matched with their facility ids, addresses and XY coordinates in the GIS DB 
        to associate them with parcel facility IDs. These parcels will be the structures (homes, businesses, etc) at which water was detected in the cellar. 
        Then these WIC parcels were intersected with SMPs within 25, 50, and 100 ft of distance from them. WIC complains were then categorized based on the 
        construction stage of the intersecting SMP
        
        The information contains:
    
    system_id: system id of SMP
    location: collected from GIS DB and referes to the addresses associated with the complaining parcels (houses etc)
    wo_initiatedate: WIC complaint date
    hase: Construction status of SMP (pre/mid/post/NA) when the WIC complaint has been filed
    buffer_ft: Buffer distance (ft) from an SMP centroid
    workorder_id: Work order IDs of WIC complaints
    comments: Work order comments"  
    

### Section 3: shiny 
    
    ui <- fluidPage(navbarPage(
      "WIC",   
      tabPanel("Help",intro),
      tabPanel("WIC", titlePanel("WIC components"),
               
               sidebarLayout(
                 
                 sidebarPanel(
                   downloadButton("download1")
                 ),
                 
                 mainPanel(
                   tabsetPanel(
                     tabPanel("25 ft", reactableOutput("table_1")), 
                     tabPanel("50 ft", reactableOutput("table_2")), 
                     tabPanel("100 ft", reactableOutput("table_3"))
                   )
                 )
               ))
    ))
    server <- function(input, output) {
      output$table_1 <- renderReactable({
        reactable(output_25ft, showPageSizeOptions = TRUE, pageSizeOptions = c(5, 10, 15), defaultPageSize = 5,filterable = TRUE,  columns = list(
          buffer_ft = colDef(filterable = FALSE)
        ))
      })
      
      output$table_2 <- renderReactable({
        reactable(output_50ft, showPageSizeOptions = TRUE, pageSizeOptions = c(5, 10, 15), defaultPageSize = 5,filterable = TRUE,  columns = list(
          buffer_ft = colDef(filterable = FALSE)
        ))
      })
      
      output$table_3 <- renderReactable({
        reactable(output_100ft, showPageSizeOptions = TRUE, pageSizeOptions = c(5, 10, 15), defaultPageSize = 5,filterable = TRUE,  columns = list(
          buffer_ft = colDef(filterable = FALSE)
        ))
      })
    }
    
    shinyApp(ui, server)
  
    
    
    
    
    
    