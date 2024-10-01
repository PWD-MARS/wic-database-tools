#### Create the shiny deliverable of the WIC project
#### Written by: Farshad Ebrahimi- 5/3/2022.

### Section 1: data gathering


  library(DBI)
  library(odbc)
  library(dplyr)
  library(shiny)
  library(reactable)
  library(RPostgreSQL)
  
  
  con <- dbConnect(odbc::odbc(), dsn = "mars_data", uid = Sys.getenv("shiny_uid_pg12_user"), pwd = Sys.getenv("shiny_pwd_pg12_user"))
  
  wic_workorders <- dbGetQuery(con, "SELECT * FROM fieldwork.wic_workorders ")
  wic_parcels <- dbGetQuery(con, "SELECT * FROM fieldwork.wic_parcels ")
  wic_smps <- dbGetQuery(con, "SELECT * FROM fieldwork.wic_smps ")
  wic_conphase <- dbGetQuery(con, "SELECT * FROM fieldwork.wic_conphase ")
  


### Section 2: processing the data into 3 tables (buffers: 25, 50, and 100 ft) and change the data format, column names and their order

  output_25ft <- inner_join(wic_smps, wic_conphase, by=c("phase_lookup_uid"="wic_uid"))%>%
    select(-phase_lookup_uid,-wic_smps_uid) %>%
    inner_join(wic_parcels, by = c("wic_facility_id"="facility_id","workorder_id" = "workorder_id")) %>%
    select(-wic_parcels_uid,-wic_facility_id) %>% 
    filter(buffer_ft == 25)                                    
  
  output_50ft <- inner_join(wic_smps, wic_conphase, by = c("phase_lookup_uid"="wic_uid") ) %>%
    select(-phase_lookup_uid,-wic_smps_uid) %>% 
    inner_join(wic_parcels, by = c("wic_facility_id"="facility_id","workorder_id" = "workorder_id")) %>%
    select(-wic_parcels_uid,-wic_facility_id) %>% 
    filter(buffer_ft == 50)    
  
  output_100ft <- inner_join(wic_smps, wic_conphase, by = c("phase_lookup_uid"="wic_uid") ) %>%
    select(-phase_lookup_uid,-wic_smps_uid) %>% 
    inner_join(wic_parcels, by = c("wic_facility_id"="facility_id","workorder_id" = "workorder_id")) %>%
    select(-wic_parcels_uid,-wic_facility_id) %>% 
    filter(buffer_ft == 100)
  
  output_25ft_dl <- output_25ft
  output_50ft_dl <- output_50ft
  output_100ft_dl <- output_100ft
  
  output_25ft_dl$smp_id<- shQuote(output_25ft$smp_id)
  output_50ft_dl$smp_id<- shQuote(output_50ft$smp_id)
  output_100ft_dl$smp_id<- shQuote(output_100ft$smp_id)
  
  output_25ft_dl$system_id<- shQuote(output_25ft$system_id)
  output_50ft_dl$system_id<- shQuote(output_50ft$system_id)
  output_100ft_dl$system_id<- shQuote(output_100ft$system_id)
  
  names(output_25ft_dl) <- c("Work Order ID", "SMP ID", "Buffer (ft)", "System ID", "Construction Phase","Address", "Complaint Date")
  names(output_50ft_dl) <- c("Work Order ID", "SMP ID", "Buffer (ft)", "System ID", "Construction Phase","Address", "Complaint Date")
  names(output_100ft_dl) <- c("Work Order ID", "SMP ID", "Buffer (ft)", "System ID", "Construction Phase","Address", "Complaint Date")
  
  output_25ft_dl <- output_25ft_dl[,c("SMP ID","System ID","Work Order ID", "Construction Phase", "Complaint Date","Address","Buffer (ft)")]
  output_50ft_dl <- output_50ft_dl[,c("SMP ID","System ID","Work Order ID", "Construction Phase", "Complaint Date","Address","Buffer (ft)")]
  output_100ft_dl <- output_100ft_dl[,c("SMP ID","System ID","Work Order ID", "Construction Phase", "Complaint Date","Address","Buffer (ft)")]
  

  names(output_25ft) <- c("Work Order ID", "SMP ID", "Buffer (ft)", "System ID", "Construction Phase","Address", "Complaint Date")
  names(output_50ft) <- c("Work Order ID", "SMP ID", "Buffer (ft)", "System ID", "Construction Phase","Address", "Complaint Date")
  names(output_100ft) <- c("Work Order ID", "SMP ID", "Buffer (ft)", "System ID", "Construction Phase","Address", "Complaint Date")
  
  output_25ft <- output_25ft[,c("SMP ID","System ID","Work Order ID", "Construction Phase", "Complaint Date","Address","Buffer (ft)")]
  output_50ft <- output_50ft[,c("SMP ID","System ID","Work Order ID", "Construction Phase", "Complaint Date","Address","Buffer (ft)")]
  output_100ft <- output_100ft[,c("SMP ID","System ID","Work Order ID", "Construction Phase", "Complaint Date","Address","Buffer (ft)")]
  output_all <- bind_rows(output_25ft,output_50ft,output_100ft)

### Section 3: shiny work-A navbarpage with two tabs, within tab 1 there will be 3 tabsets with 3 tables of data and corresponding download button on the sidebar

  ui <- fluidPage(
    navbarPage(
      "Water in Cellar (WIC) Complaints Around Public SMPs",   
      tabPanel("WIC-SMP Association ", titlePanel("WIC Complaints within 25, 50, and 100 feet of SMPs"),
               tabsetPanel(
                 tabPanel("25 ft from SMP",  
                          sidebarLayout(
                            sidebarPanel(
                              downloadButton("WIC_dl_25ft","Download in .CSV"), width = 3
                            ), 
                            mainPanel(reactableOutput("table_25ft")
                            )
                          )
                 ), 
                 tabPanel("50 ft from SMP",  
                          sidebarLayout(
                            sidebarPanel(
                              downloadButton("WIC_dl_50ft","Download in .CSV"), width = 3
                            ), 
                            mainPanel(reactableOutput("table_50ft")
                            )
                          )
                 ), 
                 tabPanel("100 ft from SMP", 
                          sidebarLayout(
                            sidebarPanel(
                              downloadButton("WIC_dl_100ft","Download in .CSV"), width = 3
                            ), 
                            mainPanel(reactableOutput("table_100ft"))
                          )
                 ) 
               )
      ),
      tabPanel("Help Page",verbatimTextOutput("help_text")
      ),
    )
  )
  server <- function(input, output) {
    output$table_25ft <- renderReactable({
      reactable(output_25ft, showPageSizeOptions = TRUE, pageSizeOptions = c(5, 10, 15), defaultPageSize = 10,filterable = TRUE
      )
    })
    
    output$table_50ft <- renderReactable({
      reactable(output_50ft, showPageSizeOptions = TRUE, pageSizeOptions = c(5, 10, 15), defaultPageSize = 10,filterable = TRUE
      )
    })
    
    output$table_100ft <- renderReactable({
      reactable(output_100ft, showPageSizeOptions = TRUE, pageSizeOptions = c(5, 10, 15), defaultPageSize = 10,filterable = TRUE  
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
    output$WIC_dl_50ft <- downloadHandler(
      filename = function() { paste("WIC_SMP_50ft_buffer-", Sys.Date(), ".csv", sep="")},
      content = function(file) {write.csv(output_50ft_dl, file,row.names=FALSE)}
    )
    
    output$WIC_dl_100ft <- downloadHandler(
      filename = function() { paste("WIC_SMP_100ft_buffer-", Sys.Date(), ".csv", sep="")},
      content = function(file) {write.csv(output_100ft_dl, file,row.names=FALSE)}
    )
  }
  
  shinyApp(ui, server)






