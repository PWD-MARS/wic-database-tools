
### Section 1: data gathering & prep

    #load the libraries
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
    library(shinydashboard)
    library(tippy)
    #Creating a function to show comments in the table with hovering mouse
    render.reactable.cell.with.tippy <- function(text, tooltip){
      div(
        style = "text-decoration: underline;
                text-decoration-style: dotted;
                text-decoration-color: #FF6B00;
                cursor: info;
                white-space: nowrap;
                overflow: hidden;
                text-overflow: ellipsis;",
        tippy(text = text, tooltip = tooltip)
      )
    }

    #Gather the data from the database
    con <- dbConnect(odbc::odbc(), dsn = "mars_data", uid = Sys.getenv("shiny_uid"), pwd = Sys.getenv("shiny_pwd"), MaxLongVarcharSize = 8190  )
    wic_workorders <- dbGetQuery(con, "SELECT * FROM fieldwork.wic_workorders ")
    wic_comments <- dbGetQuery(con, "SELECT * FROM fieldwork.wic_comments")
    wic_parcels <- dbGetQuery(con, "SELECT * FROM fieldwork.wic_parcels ")
    wic_smps <- dbGetQuery(con, "SELECT * FROM fieldwork.wic_smps ")
    wic_conphase <- dbGetQuery(con, "SELECT * FROM fieldwork.wic_conphase ")
    smp <- dbGetQuery(con, "SELECT * FROM fieldwork.wic_smp_wkt ")
    parcel <- dbGetQuery(con, "SELECT * FROM fieldwork.wic_parcels_wkt")
    parcel_all <- dbGetQuery(con, "SELECT * FROM fieldwork.wic_all_parcels_wkt")
    
    #map geoprocessing
    smp_sf <- st_as_sfc(smp[,"wkt"], CRS = 4326)
    parcel_sf <- st_as_sfc(parcel[,"wkt"], crs = 4326)
    smp_ids <- smp %>% select(smp_id)
    smp_spatial <- bind_cols(smp_ids, smp_sf)
    smp_spatial <- st_as_sf(smp_spatial)
    st_crs(smp_spatial) <- 4326
    parcel_address <- parcel %>% select(-wkt)
    parcel_spatial <- bind_cols(parcel_address, parcel_sf)
    parcel_spatial <- st_as_sf(parcel_spatial)
    st_crs(parcel_spatial) <- 4326
    smp_spatial['system_id'] <- gsub('-\\d+$','',smp_spatial$smp_id) 
    parcel_spatial['system_id'] <- gsub('-\\d+$','',parcel_spatial$smp_id ) 
    parcel_address['system_id'] <- gsub('-\\d+$','',parcel_address$smp_id) 
    
    parcel_all_sf <- st_as_sfc(parcel_all[,"wkt"], crs = 4326)
    parcel_all_address <- parcel_all %>% select(-wkt,-wic_all_parcels_wkt_uid)
    parcel_all_spatial <- bind_cols(parcel_all_address, parcel_all_sf)
    parcel_all_spatial <- st_as_sf(parcel_all_spatial)
    st_crs(parcel_all_spatial) <- 4326
    all_parcel_address <- parcel_all %>% select(-wkt)
    
    #calculating distance from system
    
    distance_sys <-parcel_address %>%
      select(system_id,address, distance_ft) %>% 
      distinct()%>%
      group_by(system_id, address )%>% 
      summarise(dist_ft = min(distance_ft)) 
    
    parcel_address <- parcel_address %>%
      select(-distance_ft)
    
    parcel_address <- parcel_address %>%
      inner_join(distance_sys, by=c("system_id"="system_id","address"="address"))
    
    parcel_address[,"dist_ft"] <- format(round(parcel_address[,"dist_ft"], 2), nsmall = 2)
    
### Section 2: processing the "table" data 
    
    output_25ft <- inner_join(wic_smps, wic_conphase, by=c("phase_lookup_uid"="wic_uid"))%>%
      select(-phase_lookup_uid,-wic_smps_uid) %>%
      inner_join(wic_parcels, by = c("wic_facility_id"="facility_id","workorder_id" = "workorder_id")) %>%
      inner_join(wic_comments, by="workorder_id")%>%
      select(-wic_parcels_uid,-wic_facility_id, -wic_comments_uid) 
    
    output_25ft <- output_25ft %>%
      inner_join(distance_sys, by=c("system_id"="system_id","location"="address"))
    
    output_25ft[,"dist_ft"] <- format(round(output_25ft[,"dist_ft"], 2), nsmall = 2)
    
    output_25ft_dl <- output_25ft
    output_25ft_dl$smp_id<- shQuote(output_25ft$smp_id)
    output_25ft_dl$system_id<- shQuote(output_25ft$system_id)
    data <- output_25ft %>% 
      select(system_id) %>%
      distinct()
    
    names(data) <- "SYSTEM ID"
    buffer <- c(25,50,100)
    names(output_25ft) <- c("Work Order ID", "SMP_ID", "Buffer_ft", "System ID", "Construction Phase","Address", "Complaint Date","Comments","Distance (ft)")
    names(output_25ft_dl) <- c("Work Order ID", "SMP_ID", "Buffer_ft", "System ID", "Construction Phase","Address", "Complaint Date","Comments","Distance (ft)")
    output_25ft <- output_25ft[,c("SMP_ID","System ID","Work Order ID", "Construction Phase", "Complaint Date","Address","Buffer_ft","Distance (ft)","Comments")]
    output_25ft_dl <- output_25ft_dl[,c("SMP_ID","System ID","Work Order ID", "Construction Phase", "Complaint Date","Address","Buffer_ft","Distance (ft)","Comments")]
    
### Section 3: Shiny work

  ui <- dashboardPage(skin = 'blue',
                      dashboardHeader(title = "Water in Cellar (WIC) Complaints", titleWidth = 500),
                      dashboardSidebar( 
                        fluidRow(
                          column(12,
                                 box(title = ' Total Number of WICs per SMP System (buffer 25 ft) ', width = 14, height = 40, background = "light-blue",solidHeader = TRUE)
                                )
                                ),
                        dateInput('date',label = 'Starting Date',value = "2012-06-06", width = 200
                                 ),
                         reactableOutput("table_stats"),
                                            width = 500),
                      dashboardBody(
                        fluidRow(
                                 leafletOutput("map",width = "100%" ,height = "600")
                                ),
                        fluidRow(
                          column(6,
                                 selectizeInput(
                                   'system_id', label = 'System ID', choices = data, selected = "555-3",
                                   options = list(maxOptions = 5),width = 900
                                 )
                          ),
                          column(6,
                                 selectizeInput(
                                   'buffer', label = 'Buffer Size (ft)', choices = buffer,selected = 25,
                                   options = list(maxOptions = 3), width = 900
                                 )
                                 
                          )
                        ),
                        fluidRow(
                          reactableOutput("table_25ft")
                        ),
                        fluidRow(
                          downloadButton("WIC_dl_25ft","Download Table in .CSV"
                          )
                        )
                      )
                     )
  
  server <- function(input, output) {
    
    #Populate reactive stat table
    reactive_stats <- reactive({
     
      output_stat <- output_25ft %>%
        filter(Buffer_ft==25 & `Complaint Date` >= as.character(input$date))%>%
        select(`System ID`,`Construction Phase`, `Work Order ID`) %>% 
        distinct()%>%
        select(`System ID`,`Construction Phase`) %>% 
        group_by(`System ID`, `Construction Phase` )%>% 
        summarise(count = n()) 
      output_stat <- output_stat %>%
        pivot_wider(names_from = `Construction Phase`, values_from = count)
      output_stat <- as.data.frame(output_stat)
      output_stat[is.na(output_stat)] <- 0
      vec_name <- c("System ID","pre-construction","during construction","post-construction","unknown","Total")
      output_stat[vec_name[!(vec_name %in% colnames(output_stat))]] = 0
      output_stat <- output_stat[,vec_name]
      names(output_stat)<- c("System ID","Pre-Con","During-Con","Post-Con","Unknown","Total")
      output_stat$Total <- output_stat$`Pre-Con`+ output_stat$`During-Con`+ output_stat$`Post-Con`+output_stat$Unknown
      output_stat <- output_stat %>% mutate(across(.cols=2:6, .fns=as.integer))
      output_stat <- output_stat[order(output_stat$Total, decreasing = TRUE), ]
      
      return(output_stat)
      
    })
    
    output$table_stats <- renderReactable(reactable(reactive_stats(),
                                                    searchable = FALSE,
                                                    pagination = FALSE,
                                                    height = 930,
                                                    striped = TRUE,
                                                    filterable = FALSE, 
                                                    fullWidth = TRUE,
                                                    columns = list(
                                                      `System ID` = colDef(width = 85),
                                                      `Pre-Con` = colDef(width = 75),
                                                      `During-Con` = colDef(width = 85),
                                                      `Post-Con` = colDef(width = 75),
                                                       Unknown = colDef(width = 75),
                                                       Total = colDef(width = 75)
                                                      
                                                      
                                                                  ),
                                                    theme = reactableTheme(
                                                                            color = "hsl(233, 9%, 87%)",
                                                                            backgroundColor = "hsl(233, 9%, 19%)",
                                                                            borderColor = "hsl(233, 9%, 22%)",
                                                                            stripedColor = "hsl(233, 12%, 22%)",
                                                                            highlightColor = "hsl(233, 12%, 24%)",
                                                                            inputStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
                                                                            selectStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
                                                                            pageButtonHoverStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
                                                                            pageButtonActiveStyle = list(backgroundColor = "hsl(233, 9%, 28%)")
                                                                          )
                                                    )
                                          )
                                                                                                                                                                                                                           
    
    output$table_25ft <- renderReactable({
      reactable(filter(output_25ft, `System ID` == input$system_id & Buffer_ft == input$buffer) %>% select(`System ID`, `Work Order ID`, `Construction Phase`,`Complaint Date`, Address,`Distance (ft)`, Comments)%>% distinct(),
                searchable = FALSE,
                pagination = TRUE,
                showPageSizeOptions = TRUE,
                height = 350,
                striped = TRUE,
                fullWidth = TRUE,
                filterable = FALSE,
                columns = list(
                    Comments = colDef(
                      html = TRUE,
                      cell =  function(value, index, name) {
                        render.reactable.cell.with.tippy(text = value, tooltip = value)}
                    )
                  )
                )
                
                
      
    })

    
    output$WIC_dl_25ft <- downloadHandler(
      filename = function() {paste("WIC_SMP-", Sys.Date(), ".csv", sep="")},
      content = function(file) {write.csv(filter(output_25ft_dl, `System ID` == shQuote(input$system_id) & Buffer_ft == input$buffer), file,row.names=FALSE)}
    )
    
    labels_address <- reactive({
      return( filter(parcel_address, system_id == input$system_id & buffer_ft == input$buffer) %>% select(address))
    })
    
    labels_dist <- reactive({
      return( filter(parcel_address, system_id == input$system_id & buffer_ft == input$buffer) %>% select(dist_ft))
    })
    
    labels_address_all <- reactive({
      return( filter(all_parcel_address, system_id == input$system_id) %>% select(address))
    })
    
    output$map <- renderLeaflet({
      
      map <- leaflet()%>%
        addTiles(options = providerTileOptions(minZoom = 16, maxZoom = 19))%>% 
        addPolygons(data = filter(parcel_all_spatial, system_id == input$system_id),
                    group = "All Parcels within 25 ft",
                    color = "green",
                    label = paste(labels_address_all()[,],"")) %>%
        addLayersControl(overlayGroups = "All Parcels within 25 ft")%>%
        hideGroup("All Parcels within 25 ft")%>%
        addPolygons(data=filter(smp_spatial, system_id == input$system_id ),
                    label = paste("System ID:",input$system_id) , 
                    color = "red", 
                    group = "SMP System") %>%
        ## Had to do label = paste(labels_parcel()[,],""), the only way labels showed correctly 
        addPolygons(data = filter(parcel_spatial, system_id == input$system_id & buffer_ft == input$buffer),
                    label = paste(labels_address()[,],"|","Distance:",labels_dist()[,],"ft"),
                    group = "Parcels") %>%
        addLegend(colors = c("red","blue"), 
                  labels = c("SMP System","Parcel")) %>%
        addDrawToolbar(polylineOptions = drawPolylineOptions(metric = FALSE, feet = TRUE),
                       polygonOptions = FALSE,
                       circleOptions=FALSE,
                       rectangleOptions=FALSE,
                       markerOptions=FALSE,
                       circleMarkerOptions= FALSE,
                       editOptions=editToolbarOptions(selectedPathOptions=selectedPathOptions()) 
                       
        )
      
    }) 
    
  }
  
  shinyApp(ui, server)