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
    library(shiny)
    library(DT)
    library(reactable)
    
    
    con <- dbConnect(odbc(), dsn = "mars_data")

    wic_workorders <- dbGetQuery(con, "SELECT * FROM fieldwork.wic_workorders ")
    wic_comments <- dbGetQuery(con, "SELECT * FROM fieldwork.wic_comments ") ### Let's skip comments for now. Too clunky in the UI. We can add later
    wic_parcels <- dbGetQuery(con, "SELECT * FROM fieldwork.wic_parcels ")
    wic_smps <- dbGetQuery(con, "SELECT * FROM fieldwork.wic_smps ")
    wic_conphase <- dbGetQuery(con, "SELECT * FROM fieldwork.wic_conphase ")



### Section 2: processing the data into 3 tables (buffers: 25, 50, and 100 ft) and change the data format
    ###Taylor says: format pipes like i showed you in the spreadsheet script
    
    output_25ft <- inner_join(wic_smps, wic_conphase, by = c("phase_lookup_uid"="wic_uid") ) %>%
    select(-phase_lookup_uid,-wic_smps_uid) %>%
    inner_join(wic_comments, by="workorder_id") %>% 
    select(-wic_comments_uid)%>% 
    inner_join(wic_parcels, by = c("wic_facility_id"="facility_id","workorder_id" = "workorder_id")) %>%
    select(-wic_parcels_uid,-wic_facility_id) %>% 
    filter(buffer_ft == 25)                                    
    
    output_50ft <- inner_join(wic_smps, wic_conphase, by = c("phase_lookup_uid"="wic_uid") ) %>%
    select(-phase_lookup_uid,-wic_smps_uid) %>% 
    inner_join(wic_comments, by="workorder_id") %>% 
    select(-wic_comments_uid)%>% 
    inner_join(wic_parcels, by = c("wic_facility_id"="facility_id","workorder_id" = "workorder_id")) %>%
    select(-wic_parcels_uid,-wic_facility_id) %>% filter(buffer_ft == 50)    
    
    output_100ft <- inner_join(wic_smps, wic_conphase, by = c("phase_lookup_uid"="wic_uid") ) %>%
    select(-phase_lookup_uid,-wic_smps_uid) %>% 
    inner_join(wic_comments, by="workorder_id") %>% 
    select(-wic_comments_uid)%>% 
    inner_join(wic_parcels, by = c("wic_facility_id"="facility_id","workorder_id" = "workorder_id")) %>%
    select(-wic_parcels_uid,-wic_facility_id) %>% 
    filter(buffer_ft == 100)
    
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
  
    
  

### Section 3: shiny work
    
    ui <- fluidPage(navbarPage(
      "WIC",   ###Taylor says: more informative name. If we have multiple wic shiny apps we must differentiate
      ###Taylor says: Switch the order of these tabs. App first, help second
      tabPanel("Help",intro), ###Rename to something like Help Page/Introduction or something more informative
      tabPanel("WIC", titlePanel("WIC components"), ###Rename both: What are we doing with the WICs?
              ### We're finding their distance from public SMPs. Comvey this in the name
               
               sidebarLayout(
                 
                 sidebarPanel(
                   downloadButton("wici","download"), width = 3 ###Always capitalize labels
                 ),
                 
                 mainPanel(
                   tabsetPanel(
                     tabPanel("25 ft", reactableOutput("table_1")), ###Taylor says: More explicit output object names
                     tabPanel("50 ft", reactableOutput("table_2")), ###Taylor says: More explicit tab mames: "25 ft from SMP" or something
                     tabPanel("100 ft", reactableOutput("table_3"))
                   )
                 )
               ))
    ))
    server <- function(input, output) {
      ###Taylor says: Format header names to be in plain english, not variable names. Work Order ID instead of workorder_id, Buffer (ft) instead of buffer_ft, etc
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
      
      
      
      ###Taylor says: Replace with CSV output
      # output$wici <- downloadHandler(
      #   filename = function() { "ae.xlsx"},
      #   content = function(file) {write.xlsx(mtcars, path = file)}
      # )
      
    }
    
    shinyApp(ui, server)
  
    
    
    
    
    
    