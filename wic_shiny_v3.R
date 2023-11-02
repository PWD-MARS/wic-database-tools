### Water-in-Cellar Shiny App
### Author: Farshad Ebrahimi, Last Modified: 10/18/2023

### Section 1: data gathering & prep

    #load the libraries
    library(DBI)
    library(odbc)
    library(dplyr)
    library(tidyverse)
    library(shiny)
    library(reactable)
    library(shinythemes)
    library(RPostgreSQL)
    library(sf)
    library(leaflet)
    library(leaflet.extras)
    library(tidyr)
    library(shinydashboard)
    library(tippy)
    #shinyjs() to use easy java script functions
    library(shinyjs)
    #create negate of %in%
    `%!in%` = Negate(`%in%`)
    
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
    con <- dbConnect(odbc::odbc(), dsn = "mars14_datav2", uid = Sys.getenv("shiny_uid"), pwd = Sys.getenv("shiny_pwd"), MaxLongVarcharSize = 8190  )
    #con <- dbConnect(odbc::odbc(), dsn = "mars_data_pg14", MaxLongVarcharSize = 8190  )
    #con <- dbConnect(RPostgres::Postgres(), dbname = "mars_data", host="PWDOOWSDBS.pwd.phila.local" , port="5434", user="mars_admin", password="lepton-gossip-underreact-polo-chair")
    
    # get the unmonitored SMP list
    unmonitored_smp_view_postcon_on <- dbGetQuery(con, "SELECT * FROM fieldwork.viw_unmonitored_postcon_on")
    
    #processing unmonitored smps
    smpbdv_df <- dbGetQuery(con,"SELECT distinct system_id, smp_id FROM external.tbl_smpbdv")
    inlets <- dbGetQuery(con, " SELECT admin.fun_component_to_smp(i.component_id) AS smp_id, admin.fun_smp_to_system(admin.fun_component_to_smp(i.component_id)) AS system_id, * FROM external.tbl_gswiinlet i
  WHERE i.component_id IS NOT NULL AND i.component_id NOT like '%-53-%' AND lifecycle_status != 'REM' AND plug_status != 'NA'
  ORDER BY (admin.fun_component_to_smp(i.component_id));") %>%
      select(system_id, component_id, plug_status) %>%
      distinct()
    
    # get the list of system with eligible inlet critera_table 4 encompasses all-no inlet or at least one online:
    eligble_inlet <- unmonitored_smp_view_postcon_on %>%
      left_join(inlets, by = "system_id") %>%
      filter(plug_status == "ONLINE" | is.na(plug_status)) %>%
      dplyr::select(system_id) %>%
      distinct()
    
    #systems in which all smps are NOT available
    system_with_null_smps <- smpbdv_df %>%
      left_join(unmonitored_smp_view_postcon_on, by = c("system_id","smp_id"), multiple = "all") %>%
      filter(is.na(smp_type)) %>%
      select(system_id) %>%
      distinct() 
    
    
    unmonitored_list <- unmonitored_smp_view_postcon_on %>%
      filter(system_id %in% eligble_inlet$system_id) %>%
      anti_join(system_with_null_smps, by = "system_id") %>%
      dplyr::select(system_id) %>%
      distinct()
    
    
    # work orders
    wic_workorders <- dbGetQuery(con, "SELECT * FROM fieldwork.tbl_wic_workorders ")
    # comments
    wic_comments <- dbGetQuery(con, "SELECT * FROM fieldwork.tbl_wic_comments")
    # wic parcels and location
    wic_parcels <- dbGetQuery(con, "SELECT * FROM fieldwork.tbl_wic_parcels ")
    # wic-smp table
    wic_smps <- dbGetQuery(con, "SELECT * FROM fieldwork.tbl_wic_smps ")
    # construction phase
    wic_conphase <- dbGetQuery(con, "SELECT * FROM fieldwork.tbl_wic_conphase ")
    # smp polygons in wkt
    smp <- dbGetQuery(con, "SELECT * FROM fieldwork.tbl_wic_smp_wkt ")
    # wic parcel polygons in wkt
    parcel <- dbGetQuery(con, "SELECT * FROM fieldwork.tbl_wic_parcels_wkt")
    # all sourounding parcels within 25ft polygons in wkt
    parcel_all <- dbGetQuery(con, "SELECT * FROM fieldwork.tbl_wic_all_parcels_wkt")
    
    # get all smps in Philadelphia
    smp_all <- dbGetQuery(con, "SELECT * FROM fieldwork.tbl_wic_all_smp_wkt")
    
    
    ## highlight keywords such as STORM, RAIN, GSI
    wic_comments <- wic_comments  %>% mutate(Keywords = paste(ifelse(str_detect(wic_comments$comments, fixed("STORM")), "STORM", ""),
                                                    ifelse(str_detect(wic_comments$comments, fixed("RAIN")), "RAIN", ""), 
                                                    ifelse(str_detect(wic_comments$comments, fixed("GSI")), "GSI", ""),
                                                    sep = " "))
    
    
    
    # Buildings footprints in wkt
    buidling_footprint <- dbGetQuery(con, "SELECT * FROM fieldwork.tbl_wic_buildingfootprint_wkt")
    
    
    ## map geo-processioning; converting WKT to SF object
    
    # wkt to SF in CRS = 4326
    smp_sf <- st_as_sfc(smp[,"wkt"], CRS = 4326)
    parcel_sf <- st_as_sfc(parcel[,"wkt"], crs = 4326)
    smp_all_sf <- st_as_sfc(smp_all[,"wkt"], crs = 4326)
    
    # attaching the smp_ids and setting crs
    smp_ids <- smp %>% select(smp_id)
    smp_ids_all <- smp_all %>% select(smp_id)
    
    smp_spatial <- bind_cols(smp_ids, smp_sf)
    smp_spatial <- st_as_sf(smp_spatial)
    st_crs(smp_spatial) <- 4326
    
    smp_all_spatial <- bind_cols(smp_ids_all, smp_all_sf)
    smp_all_spatial <- st_as_sf(smp_all_spatial)
    st_crs(smp_all_spatial) <- 4326
    
    # attaching the address to parcels and setting crs
    parcel_address <- parcel %>% select(-wkt)
    parcel_spatial <- bind_cols(parcel_address, parcel_sf)
    parcel_spatial <- st_as_sf(parcel_spatial)
    st_crs(parcel_spatial) <- 4326
    
    # attaching the system ids
    smp_spatial['system_id'] <- gsub('-\\d+$','',smp_spatial$smp_id) 
    parcel_spatial['system_id'] <- gsub('-\\d+$','',parcel_spatial$smp_id ) 
    parcel_address['system_id'] <- gsub('-\\d+$','',parcel_address$smp_id) 
    
    # converting all sorounding parcels to SF
    parcel_all_sf <- st_as_sfc(parcel_all[,"wkt"], crs = 4326)
    parcel_all_address <- parcel_all %>% select(-wkt,-wic_all_parcels_wkt_uid)
    parcel_all_spatial <- bind_cols(parcel_all_address, parcel_all_sf)
    parcel_all_spatial <- st_as_sf(parcel_all_spatial)
    st_crs(parcel_all_spatial) <- 4326
    all_parcel_address <- parcel_all %>% select(-wkt)
    
    
    #building footprint, convert to SF and set crs, add system ids
    buidling_footprint_sf <- st_as_sfc(buidling_footprint[,"wkt"], crs = 4326)
    buidling_footprint_df <- buidling_footprint %>% select(-wkt,-wic_buildingfootprint_wkt_uid)
    buidling_footprint_spatial <- bind_cols(buidling_footprint_df, buidling_footprint_sf)
    buidling_footprint_spatial <- st_as_sf(buidling_footprint_spatial)
    st_crs(buidling_footprint_spatial) <- 4326
    buidling_footprint_spatial['system_id'] <- gsub('-\\d+$','',buidling_footprint_spatial$smp_id ) 
    buidling_footprint['system_id'] <- gsub('-\\d+$','',buidling_footprint$smp_id) 
    
    #calculating distance from system; pick min distance of smp-wic parcel
    distance_sys <- parcel_address %>%
      select(system_id,address, distance_ft) %>% 
      distinct()%>%
      group_by(system_id, address )%>% 
      summarise(dist_ft = min(distance_ft)) 
    
    parcel_address <- parcel_address %>%
      select(-distance_ft)
    
    parcel_address <- parcel_address %>%
      inner_join(distance_sys, by=c("system_id"="system_id","address"="address"))
    
    # round the digits
    parcel_address[,"dist_ft"] <- format(round(parcel_address[,"dist_ft"], 2), nsmall = 2)
    
### Section 2: processing WIC tabular data 
    
    # table for all buffers processing 
    output_all_buffers <- inner_join(wic_smps, wic_conphase, by=c("phase_lookup_uid"="wic_uid"))%>%
      select(-phase_lookup_uid,-wic_smps_uid) %>%
      inner_join(wic_parcels, by = c("wic_facility_id"="facility_id","workorder_id" = "workorder_id")) %>%
      inner_join(wic_comments, by="workorder_id")%>%
      select(-wic_parcels_uid,-wic_facility_id, -wic_comments_uid, -Keywords) 
    
    output_all_buffers <- output_all_buffers %>%
      inner_join(distance_sys, by=c("system_id"="system_id","location"="address"))
    
    output_all_buffers[,"dist_ft"] <- format(round(output_all_buffers[,"dist_ft"], 2), nsmall = 2)
    
    # download table for all buffers processing 
    output_all_buffers_dl <- output_all_buffers
    output_all_buffers_dl$smp_id<- shQuote(output_all_buffers$smp_id)
    output_all_buffers_dl$system_id<- shQuote(output_all_buffers$system_id)
    data <- output_all_buffers %>% 
      select(system_id) %>%
      distinct()
    # vector  of all systems ids with wics 
    names(data) <- "SYSTEM ID"
    # vector of all used buffers
    buffer <- c(25,50,100)
    
    # final name processing, order of columns for both the output and download tables
    names(output_all_buffers) <- c("Work Order ID", "SMP_ID", "Buffer_ft", "System ID", "Construction Phase","Address", "Complaint Date","Comments","Property Distance (ft)")
    names(output_all_buffers_dl) <- c("Work Order ID", "SMP_ID", "Buffer_ft", "System ID", "Construction Phase","Address", "Complaint Date","Comments","Property Distance (ft)")
    output_all_buffers <- output_all_buffers[,c("SMP_ID","System ID","Work Order ID", "Construction Phase", "Complaint Date","Address","Buffer_ft","Property Distance (ft)","Comments")]
    output_all_buffers_dl <- output_all_buffers_dl[,c("SMP_ID","System ID","Work Order ID", "Construction Phase", "Complaint Date","Address","Buffer_ft","Property Distance (ft)","Comments")]
    
### Section 3: Shiny work starts here
# 
#   ui <- dashboardPage(skin = 'blue',
#                       dashboardHeader(title = "Water in Cellar (WIC) Complaints", titleWidth = 500),
#                       dashboardSidebar( 
#                         fluidRow(
#                           column(12,
#                                  box(title = ' Total Number of WICs per SMP System (buffer 25 ft) ', width = 14, height = 40, background = "light-blue",solidHeader = TRUE)
#                                 )
#                                 ),
#                         dateInput('date',label = 'Starting Date',value = "2012-06-06", width = 200
#                                  ),
#                          reactableOutput("table_stats"),
#                                             width = 500),
#                       dashboardBody(
#                         fluidRow(
#                                  leafletOutput("map",width = "100%" ,height = "550")
#                                 ),
#                         fluidRow(
#                           column(6,
#                                  selectizeInput(
#                                    'system_id', label = 'System ID', choices = data, selected = "555-3",
#                                    options = list(maxOptions = 5),width = 900
#                                  )
#                           ),
#                           column(6,
#                                  selectizeInput(
#                                    'buffer', label = 'Buffer Size (ft)', choices = buffer,selected = 100,
#                                    options = list(maxOptions = 3), width = 900
#                                  )
#                                  
#                           )
#                         ),
#                         fluidRow(
#                           reactableOutput("table_wic")
#                         ),
#                         fluidRow(
#                           downloadButton("table_wic_dl","Download Table in .CSV"
#                           )
#                         )
#                       )
#                      )
  
    ui <- fluidPage(
      navbarPage(
        "Water in Cellar (WIC) Complaints", #theme = shinytheme("cerulean"),
        tabPanel(
          "WIC",
          fluidRow(
            column(7, 
                   fluidRow(
                     column(6, selectizeInput('system_id', label = 'System ID', choices = data, selected = "555-3", options = list(maxOptions = 5),width = 500)),
                     column(6, selectizeInput('buffer', label = 'Buffer Size (ft)', choices = buffer,selected = 100, options = list(maxOptions = 3), width = 500))),
                   reactableOutput("table_wic")
                   ),
            column(5, leafletOutput("map",width = "100%" ,height = "830"))
                      )
          
        ),
        tabPanel(
          "Totalizer",
        fluidRow(column(5, box(title = ' Total Number of WICs per SMP System (buffer 25 ft) ', width = 14, height = 40, background = "light-blue",solidHeader = TRUE),
                        dateInput('date',label = 'Starting Date',value = "2012-06-06", width = 200),
                        reactableOutput("table_stats", width = 630)),
                 column(7, box(title = ' Release Notes', width = 14, height = 40, background = "light-blue",solidHeader = TRUE),
                        h4('10/25/2023: The outline changed into two tabs, giving more space to the WIC table and the map')))
          #downloadButton("table_wic_dl","Download Table in .CSV")
          
        )
      )
    )

  server <- function(input, output) {
    
    #Populate reactive stat table
    reactive_stats <- reactive({
      
      
      # grouping to get headcounts
      output_stat <- output_all_buffers %>%
        filter(Buffer_ft==25 & `Complaint Date` >= as.character(input$date))%>%
        select(`System ID`,`Construction Phase`, `Work Order ID`) %>% 
        distinct()%>%
        select(`System ID`,`Construction Phase`) %>% 
        group_by(`System ID`, `Construction Phase` )%>% 
        summarise(count = n())
      
      # a message that no wic are to be shown 
      if (nrow(output_stat)==0) {
        validate("There is No WIC to Show for the Selectd Starting Date")
      }
      
      # transposing to the right format
      output_stat <- output_stat %>%
        pivot_wider(names_from = `Construction Phase`, values_from = count)
      output_stat <- as.data.frame(output_stat)
      # replace NA with zero
      output_stat[is.na(output_stat)] <- 0
      # renaming 
      vec_name <- c("System ID","pre-construction","during construction","post-construction","unknown","Total")
      # replace NA with zero
      output_stat[vec_name[!(vec_name %in% colnames(output_stat))]] = 0
      output_stat <- output_stat[,vec_name]
      names(output_stat)<- c("System ID","Pre-Con","During-Con","Post-Con","Unknown","Total")
      # summing up for the total row
      output_stat$Total <- output_stat$`Pre-Con`+ output_stat$`During-Con`+ output_stat$`Post-Con`+output_stat$Unknown
      output_stat <- output_stat %>% 
        mutate(across(.cols=2:6, .fns=as.integer))
      output_stat <- output_stat[order(output_stat$Total, decreasing = TRUE), ]
      
      
      return(output_stat)
      
    })
    
    output$table_stats <- renderReactable(reactable(reactive_stats(),
                                                    searchable = FALSE,
                                                    pagination = FALSE,
                                                    striped = TRUE,
                                                    filterable = FALSE, 
                                                    fullWidth = TRUE,
                                                    columns = list(
                                                      `System ID` = colDef(width = 100),
                                                      `Pre-Con` = colDef(width = 100),
                                                      `During-Con` = colDef(width = 100),
                                                      `Post-Con` = colDef(width = 100),
                                                       Unknown = colDef(width = 100),
                                                       Total = colDef(width = 100)
                                                      
                                                      
                                                                  )
                                          ))
    
    table_wic_rv <- reactive(output_all_buffers %>%
                               filter(`System ID` == input$system_id & Buffer_ft == input$buffer) %>%
                               mutate("Eligible for Monitoring?" =  case_when(`System ID` %in% unmonitored_list$system_id ~ "Yes",
                                                                              `System ID` %!in% unmonitored_list$system_id ~ "No")) %>%
                               select(`System ID`, `Work Order ID`, `Construction Phase`,`Complaint Date`, Address,`Property Distance (ft)`, `Eligible for Monitoring?`)%>% 
                               distinct())
                                                                                                                                                                                                                           
    
    output$table_wic <- renderReactable({
      reactable(table_wic_rv(),
                searchable = FALSE,
                defaultPageSize = 25,
                pagination = TRUE,
                showPageSizeOptions = TRUE,
                height = 770,
                striped = TRUE,
                fullWidth = TRUE,
                selection = "single",
                onClick = "select",
                theme = reactableTheme(
                  rowSelectedStyle = list(backgroundColor = "#ffe4cc", boxShadow = "inset 2px 0 0 0 #ffa62d")
                ),
                selectionId = "row_selected",
                filterable = FALSE,
                columns = list(
                  `System ID` = colDef(width = 90),
                  `Work Order ID` = colDef(width = 115),
                  `Construction Phase` = colDef(width = 150),
                  `Complaint Date` = colDef(width = 145),
                   Address = colDef(width = 165),
                  `Property Distance (ft)` = colDef(width = 165)
                  
                  
                ),
                details = function(index) {
                  nested_notes <- wic_comments[wic_comments$workorder_id == table_wic_rv()$`Work Order ID`[index], ] %>%
                    select(Comments = comments,  Keywords)
                  htmltools::div(style = "padding: 1rem",
                                 reactable(nested_notes, 
                                           columns = list(
                                             Comments = colDef(width = 800),
                                             Keywords = colDef(width = 200)
                                           ), 
                                           outlined = TRUE)
                  )
                }
                )
                
                
      
    })

    
    output$table_wic_dl <- downloadHandler(
      filename = function() {paste("WIC_SMP-", Sys.Date(), ".csv", sep="")},
      content = function(file) {write.csv(filter(output_all_buffers_dl, `System ID` == shQuote(input$system_id) & Buffer_ft == input$buffer), file,row.names=FALSE)}
    )
    
  ## Reactive lables for map polygons 
    labels_address <- reactive({
      return( filter(parcel_address, system_id == input$system_id & buffer_ft == input$buffer) %>% select(address))
    })
    
    labels_dist <- reactive({
      return( filter(parcel_address, system_id == input$system_id & buffer_ft == input$buffer) %>% select(dist_ft))
    })
    
    labels_address_all <- reactive({
      return( filter(all_parcel_address, system_id == input$system_id) %>% select(address))
    })
    
    labels_footprint <- reactive({
      return( filter(buidling_footprint, system_id == input$system_id & buffer_ft == input$buffer) %>% select(address))
    })
    
  
    
    # output$map <- renderLeaflet({
    #   
    #   
    #   if (nrow(filter(parcel_spatial, system_id == input$system_id & buffer_ft == input$buffer))==0) {
    #     validate("There is No WIC to Show for this Buffer Size")
    #   }
    #   
    #   
    #   map <- leaflet()%>%
    #     addProviderTiles(providers$OpenStreetMap, group = 'OpenStreetMap', options = providerTileOptions(minZoom = 16, maxZoom = 19)) %>% 
    #     addProviderTiles(providers$Esri.WorldImagery, group='ESRI Satellite', options = providerTileOptions(minZoom = 16, maxZoom = 19)) %>% 
    #     addPolygons(data=filter(smp_spatial, system_id == input$system_id ),
    #                 label = paste("System ID:",input$system_id) , 
    #                 color = "blue", 
    #                 group = "SMP System") %>%
    #     addPolygons(data = filter(parcel_all_spatial, system_id == input$system_id),
    #                 group = "All Parcels within 25 ft",
    #                 color = "green",
    #                 label = paste(labels_address_all()[,],"")) %>%
    #     addPolygons(data = filter(buidling_footprint_spatial, system_id == input$system_id & buffer_ft == input$buffer),
    #                 label = labels_footprint()[,],
    #                 color = "black",
    #                 group = "Building Footprint") %>%
    #     addLayersControl(overlayGroups = c("All Parcels within 25 ft","Building Footprint"),baseGroups = c('OpenStreetMap', 'ESRI Satellite'))%>%
    #     hideGroup(c("All Parcels within 25 ft","Building Footprint"))%>%
    #     ## Had to do label = paste(labels_parcel()[,],""), the only way labels showed correctly 
    #     addPolygons(data = filter(parcel_spatial, system_id == input$system_id & buffer_ft == input$buffer),
    #                 label = paste(labels_address()[,],"|","Distance:",labels_dist()[,],"ft"),
    #                 group = "Parcels",
    #                 color="red") %>%
    #     addLegend(colors = c("blue","red","black","green"), 
    #               labels = c("System","WIC Property Line","WIC Building Footprint","All Property Lines (WIC/NON-WIC)")) %>%
    #     addDrawToolbar(polylineOptions = drawPolylineOptions(metric = FALSE, feet = TRUE),
    #                    polygonOptions = FALSE,
    #                    circleOptions=FALSE,
    #                    rectangleOptions=FALSE,
    #                    markerOptions=FALSE,
    #                    circleMarkerOptions= FALSE,
    #                    editOptions=editToolbarOptions(selectedPathOptions=selectedPathOptions()) 
    #                    
    #     )
    #   
    #   return(map)
    #   
    # }) 
    
    
  
### If a row is selected, the map will have an extra layer (the highlighted polygon in yellow)
    observe({
      if(length(input$row_selected) != 0){
        
        output$map <- renderLeaflet({
          
          
          if (nrow(filter(parcel_spatial, system_id == input$system_id & buffer_ft == input$buffer))==0) {
            validate("There is No WIC to Show for this Buffer Size")
          }
          
          
          bounds <- reactive(smp_spatial %>% 
                               filter(system_id == input$system_id) %>%
                               st_bbox() %>% 
                               as.character())
          
          map <- leaflet()%>%
            addProviderTiles(providers$OpenStreetMap, group = 'OpenStreetMap', options = providerTileOptions(minZoom = 16, maxZoom = 19)) %>% 
            addProviderTiles(providers$Esri.WorldImagery, group='ESRI Satellite', options = providerTileOptions(minZoom = 16, maxZoom = 19)) %>% 
            addPolygons(data=filter(smp_spatial, system_id == input$system_id ),
                        label = paste("System ID:",input$system_id) , 
                        color = "blue", 
                        group = "SMP System") %>%
            addPolygons(data = filter(parcel_all_spatial, system_id == input$system_id),
                        group = "All Parcels within 25 ft",
                        color = "green",
                        label = paste(labels_address_all()[,],"")) %>%
            addPolygons(data = filter(buidling_footprint_spatial, system_id == input$system_id & buffer_ft == input$buffer),
                        label = labels_footprint()[,],
                        color = "black",
                        group = "Building Footprint") %>%
            addPolygons(data= smp_all_spatial,
                        label = paste("SMP ID:", smp_all_spatial$smp_id) , 
                        color = "purple", 
                        group = "All SMP") %>%
            fitBounds(bounds()[1], bounds()[2], bounds()[3], bounds()[4]) %>%
            addLayersControl(overlayGroups = c("All Parcels within 25 ft","Building Footprint", "All SMP"),baseGroups = c('OpenStreetMap', 'ESRI Satellite'))%>%
            hideGroup(c("All Parcels within 25 ft","Building Footprint", "All SMP"))%>%
            ## Had to do label = paste(labels_parcel()[,],""), the only way labels showed correctly 
            addPolygons(data = filter(parcel_spatial, system_id == input$system_id & buffer_ft == input$buffer),
                        label = paste(labels_address()[,],"|","Distance:",labels_dist()[,],"ft"),
                        group = "Parcels",
                        color="red") %>%
            addPolygons(data = filter(parcel_spatial, address == table_wic_rv()$Address[input$row_selected]),
                        label = table_wic_rv()$Address[input$row_selected],
                        color="yellow",
                        group = "highlight",
                        layerId = table_wic_rv()$Address[input$row_selected]) %>%
            addLegend(colors = c("blue","red","black","green"), 
                      labels = c("System","WIC Property Line","WIC Building Footprint","All Property Lines (WIC/NON-WIC)")) %>%
            addDrawToolbar(polylineOptions = drawPolylineOptions(metric = FALSE, feet = TRUE),
                           polygonOptions = FALSE,
                           circleOptions=FALSE,
                           rectangleOptions=FALSE,
                           markerOptions=FALSE,
                           circleMarkerOptions= FALSE,
                           editOptions=editToolbarOptions(selectedPathOptions=selectedPathOptions()) 
                           
            )
          
          return(map)
          
        }) 

      } else{
        
        output$map <- renderLeaflet({
          
          
          if (nrow(filter(parcel_spatial, system_id == input$system_id & buffer_ft == input$buffer))==0) {
            validate("There is No WIC to Show for this Buffer Size")
          }
          
          
          bounds <- reactive(smp_spatial %>% 
                               filter(system_id == input$system_id) %>%
                               st_bbox() %>% 
                               as.character())
          
          map <- leaflet()%>%
            addProviderTiles(providers$OpenStreetMap, group = 'OpenStreetMap', options = providerTileOptions(minZoom = 16, maxZoom = 19)) %>% 
            addProviderTiles(providers$Esri.WorldImagery, group='ESRI Satellite', options = providerTileOptions(minZoom = 16, maxZoom = 19)) %>% 
            addPolygons(data=filter(smp_spatial, system_id == input$system_id ),
                        label = paste("System ID:",input$system_id) , 
                        color = "blue", 
                        group = "SMP System") %>%
            addPolygons(data = filter(parcel_all_spatial, system_id == input$system_id),
                        group = "All Parcels within 25 ft",
                        color = "green",
                        label = paste(labels_address_all()[,],"")) %>%
            addPolygons(data = filter(buidling_footprint_spatial, system_id == input$system_id & buffer_ft == input$buffer),
                        label = labels_footprint()[,],
                        color = "black",
                        group = "Building Footprint") %>%
            addPolygons(data= smp_all_spatial,
                        label = paste("SMP ID:", smp_all_spatial$smp_id) , 
                        color = "purple", 
                        group = "All SMP") %>%
            fitBounds(bounds()[1], bounds()[2], bounds()[3], bounds()[4]) %>%
            addLayersControl(overlayGroups = c("All Parcels within 25 ft","Building Footprint", "All SMP"),baseGroups = c('OpenStreetMap', 'ESRI Satellite'))%>%
            hideGroup(c("All Parcels within 25 ft","Building Footprint", "All SMP"))%>%
            ## Had to do label = paste(labels_parcel()[,],""), the only way labels showed correctly 
            addPolygons(data = filter(parcel_spatial, system_id == input$system_id & buffer_ft == input$buffer),
                        label = paste(labels_address()[,],"|","Distance:",labels_dist()[,],"ft"),
                        group = "Parcels",
                        color="red") %>%
            addLegend(colors = c("blue","red","black","green"), 
                      labels = c("System","WIC Property Line","WIC Building Footprint","All Property Lines (WIC/NON-WIC)")) %>%
            addDrawToolbar(polylineOptions = drawPolylineOptions(metric = FALSE, feet = TRUE),
                           polygonOptions = FALSE,
                           circleOptions=FALSE,
                           rectangleOptions=FALSE,
                           markerOptions=FALSE,
                           circleMarkerOptions= FALSE,
                           editOptions=editToolbarOptions(selectedPathOptions=selectedPathOptions()) 
                           
            )
          
          return(map)
          
        }) 
        
        }

    }
    )

    
    
  }
  
  shinyApp(ui, server)
  
  