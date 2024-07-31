### Water-in-Cellar Shiny App
### Author: Farshad Ebrahimi, Last Modified: 03/19/2023

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
    library(shinyWidgets)
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

    # Get deployment table
    deployments_list <- dbGetQuery(con, "SELECT *, admin.fun_smp_to_system(smp_id) as system_id FROM fieldwork.viw_deployment_full_cwl") %>%
      select(system_id)
    
    #processing unmonitored smps
    smpbdv_df <- dbGetQuery(con,"SELECT distinct system_id, smp_id FROM external.tbl_smpbdv")
   
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
    smp_all_spatial['system_id'] <- gsub('-\\d+$','',smp_all_spatial$smp_id) 
    
    
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
    
    #calculating property distance from system; pick min distance of smp-wic parcel
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
    
    
    
    #calculating building footprint distance from system; pick min distance of smp-wic parcel
    footprint_distance_sys <- buidling_footprint %>%
      select(system_id, address, distance_footprint_ft) %>% 
      distinct()%>%
      group_by(system_id, address )%>% 
      summarise(distance_bld_footprint_ft = min(distance_footprint_ft)) 
    
    
### Section 2: processing WIC tabular data 
    
    # table for all buffers processing 
    output_all_buffers <- inner_join(wic_smps, wic_conphase, by=c("phase_lookup_uid"="wic_uid"))%>%
      select(-phase_lookup_uid,-wic_smps_uid) %>%
      inner_join(wic_parcels, by = c("wic_facility_id"="facility_id","workorder_id" = "workorder_id")) %>%
      left_join(wic_comments, by="workorder_id")%>%
      select(-wic_parcels_uid,-wic_facility_id, -wic_comments_uid, -Keywords) 
    
    output_all_buffers <- output_all_buffers %>%
      inner_join(distance_sys, by=c("system_id"="system_id","location"="address")) %>%
      inner_join(footprint_distance_sys, by=c("system_id"="system_id","location"="address"))
    
    output_all_buffers[,"dist_ft"] <- format(round(output_all_buffers[,"dist_ft"], 2), nsmall = 2)
    output_all_buffers[,"distance_bld_footprint_ft"] <- format(round(output_all_buffers[,"distance_bld_footprint_ft"], 2), nsmall = 2)
    
    
    # download table for all buffers processing 
    output_all_buffers_dl <- output_all_buffers
    output_all_buffers_dl$smp_id<- shQuote(output_all_buffers$smp_id)
    output_all_buffers_dl$system_id<- shQuote(output_all_buffers$system_id)
    data <- odbc::dbGetQuery(con, paste0("select distinct system_id from external.mat_assets where system_id like '%-%'")) %>% 
      dplyr::arrange(system_id)
    # vector  of all systems ids with wics 
    names(data) <- "SYSTEM ID"
    # vector of all used buffers
    buffer <- c(25,50,100)
    
    # final name processing, order of columns for both the output and download tables
    names(output_all_buffers) <- c("Work Order ID", "SMP_ID", "Buffer_ft", "System ID", "Construction Phase","Address", "Complaint Date","Comments","Property Distance (ft)","Buidling Footprint Distance (ft)")
    names(output_all_buffers_dl) <- c("Work Order ID", "SMP_ID", "Buffer_ft", "System ID", "Construction Phase","Address", "Complaint Date","Comments","Property Distance (ft)","Buidling Footprint Distance (ft)")
    output_all_buffers <- output_all_buffers[,c("SMP_ID","System ID","Work Order ID", "Construction Phase", "Complaint Date","Address","Buffer_ft","Property Distance (ft)","Buidling Footprint Distance (ft)","Comments")]
    output_all_buffers_dl <- output_all_buffers_dl[,c("SMP_ID","System ID","Work Order ID", "Construction Phase", "Complaint Date","Address","Buffer_ft","Property Distance (ft)","Buidling Footprint Distance (ft)","Comments")]
    
    # date of latest wic in the app
    latest_wic_date <- output_all_buffers %>%
      arrange(desc(`Complaint Date`)) %>%
      select(`Complaint Date`) %>%
      pull %>%
      .[1]
      
  
    ui <- fluidPage(
      navbarPage(
        "MARS Water in Cellar (WIC) Complaints", theme = shinytheme("cerulean"),
        tabPanel(
          "WIC",
          fluidRow(
            column(7, 
                   fluidRow(
                     column(6, selectizeInput('system_id', label = 'System ID', choices = data, selected = "555-3", width = 500)),
                     column(6, selectizeInput('buffer', label = 'Buffer Size from Property (ft)', choices = buffer,selected = 100, options = list(maxOptions = 3), width = 500))),
                   reactableOutput("table_wic")
                   ),
            column(5,leafletOutput("map",width = "90%" ,height = "830"))
                      )
          
        ),
        tabPanel(
          "Totalizer",
        fluidRow(column(5, box(title = paste(' Total WICs per System (buffer 25 ft); ', 'Latest WIC Date: ', latest_wic_date ), width = 14, height = 40, background = "light-blue",solidHeader = TRUE),
                        dateInput('date',label = 'Starting Date',value = "2012-06-06", width = 200),
                        reactableOutput("table_stats", width = 630)),
                 column(7, box(title = ' Release Notes by Farshad Ebrahimi (Farshad.Ebrahimi@Phila.Gov)', width = 14, height = 40, background = "light-blue",solidHeader = TRUE),
                        h3('WIC App v.3.0.0, 11/02/2023:'),
                        h5("1.	Added “Eligible for Monitoring” column to the WIC app to show if the system is in the unmonitored smp app list"),
                        h5("2.  The adjacent GSI systems have been incorporated  into the map as “All SMP” layer that can be shown through the top right menu of the map (default: hidden)"),
                        h5("3.	Comment section became an arrow (nested table) that expands if the user clicks"),
                        h5("4.	Highlighting keywords such as RAIN, STORMWATER, GSI can help sort out the systems: I added “Keywords” column in the nested table, if empty, no keywords detected "),
                        h5("5.	Changed the columns width so that all columns have appropriate space (system id should be narrow, address line wider) "),
                        h5("6.	Clicking on the table rows should highlight the map polygon in yellow"),
                        h5("7.	At %100 browser zoom level, all components of the app (totalizer, map, and table) are reasonably visible and there is not a need to scroll to see things. "),
                        h5("8.	Totalizer and versioning comments are now under a new tab (“Totalizer”).")))
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
                               mutate("Previously Monitored?" =  case_when(`System ID` %in% deployments_list$system_id ~ "Yes",
                                                                              `System ID` %!in% deployments_list$system_id ~ "No")) %>%
                               select(ID = `Work Order ID`, `Construction Phase`,Date = `Complaint Date`, Address,`Property Distance (ft)`,`Footprint Distance (ft)`=`Buidling Footprint Distance (ft)`, `Previously Monitored?`)%>% 
                               distinct())
                                                                                                                                                                                                                           
    
    output$table_wic <- renderReactable({
      
      if (nrow(filter(parcel_spatial, system_id == input$system_id)) != 0 & nrow(filter(parcel_spatial, system_id == input$system_id & buffer_ft == input$buffer)) == 0) {
        validate("There is No WIC to Show within this Buffer Size, Expand the Buffer!")
      } else if (nrow(filter(parcel_spatial, system_id == input$system_id)) == 0)  {
        validate("There is No WIC within 100 ft to Show for this System ID")
      }
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
                  #`System ID` = colDef(width = 90),
                   ID = colDef(width = 75),
                  `Construction Phase` = colDef(width = 150),
                   Date = colDef(width = 110),
                   Address = colDef(width = 165),
                  `Property Distance (ft)` = colDef(width = 165),
                  `Footprint Distance (ft)` = colDef(width = 170)
                  
                  
                ),
                details = function(index) {
                  nested_notes <- wic_comments[wic_comments$workorder_id == table_wic_rv()$`ID`[index], ] %>%
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
    

### If a row is selected, the map will have an extra layer (the highlighted polygon in yellow)
    observe({
      if(length(input$row_selected) != 0){
        
        output$map <- renderLeaflet({
          
          
          if (nrow(filter(parcel_spatial, system_id == input$system_id))== 0) {
            #validate("There is No WIC within 100 ft to Show for this System ID")
            map <- leaflet()%>%
              addProviderTiles(providers$OpenStreetMap, group = 'OpenStreetMap', options = providerTileOptions(minZoom = 16, maxZoom = 19)) %>%
              addPolygons(data=filter(smp_all_spatial, system_id == input$system_id ),
                          label = paste("System ID:",input$system_id) ,
                          color = "blue",
                          group = "SMP System") %>%
              addLegend(colors = "blue", 
                        labels = "System")
            
          } else if (nrow(filter(parcel_spatial, system_id == input$system_id)) != 0 & nrow(filter(parcel_spatial, system_id == input$system_id & buffer_ft == input$buffer)) == 0) {
            
            map <- leaflet()%>%
              addProviderTiles(providers$OpenStreetMap, group = 'OpenStreetMap', options = providerTileOptions(minZoom = 16, maxZoom = 19)) %>%
              addPolygons(data=filter(smp_spatial, system_id == input$system_id ),
                          label = paste("System ID:",input$system_id) ,
                          color = "blue",
                          group = "SMP System") %>%
              addLegend(colors = "blue", 
                        labels = "System")
            
            
            
            
          } else {

          
          
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
          
          }  
        }) 

      } else{
        
        output$map <- renderLeaflet({
          
          if (nrow(filter(parcel_spatial, system_id == input$system_id))== 0) {
            #validate("There is No WIC within 100 ft to Show for this System ID")
            map <- leaflet()%>%
              addProviderTiles(providers$OpenStreetMap, group = 'OpenStreetMap', options = providerTileOptions(minZoom = 16, maxZoom = 19)) %>%
              addPolygons(data=filter(smp_all_spatial, system_id == input$system_id ),
                          label = paste("System ID:",input$system_id) ,
                          color = "blue",
                          group = "SMP System") %>%
              addLegend(colors = "blue", 
                        labels = "System")
            
          } else if (nrow(filter(parcel_spatial, system_id == input$system_id)) != 0 & nrow(filter(parcel_spatial, system_id == input$system_id & buffer_ft == input$buffer)) == 0) {
            
              map <- leaflet()%>%
                addProviderTiles(providers$OpenStreetMap, group = 'OpenStreetMap', options = providerTileOptions(minZoom = 16, maxZoom = 19)) %>%
                addPolygons(data=filter(smp_spatial, system_id == input$system_id ),
                            label = paste("System ID:",input$system_id) ,
                            color = "blue",
                            group = "SMP System") %>%
                addLegend(colors = "blue", 
                          labels = "System")

            
            
            
          } else {
          
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
          
          
          }  
        }) 
        
        }

    }
    )

    
    
  }
  
  shinyApp(ui, server)
  
  