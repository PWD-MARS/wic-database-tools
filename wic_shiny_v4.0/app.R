### Water-in-Cellar Shiny App V 4.0 
### Notes: Major overhaul of the maint. script + addition of POSTGIS to the PostgreSQL DB
### Author: Farshad Ebrahimi 
### Last Modified: 09/20/2024

# 0.0 Set Up ----
## 0.1 Load libraries ----
# Shiny
library(shiny)
library(shinyjs)
library(shinydashboard)
library(shinyWidgets)
library(shinythemes)
library(reactable)
library(reactablefmtr)
# Connection
library(DBI)
library(odbc)
library(RPostgreSQL)
# Data Processing/Mapping
library(tidyverse)
library(sf)
library(leaflet)
library(leaflet.extras)
# else 
library(xlsx)

#create negate of %in%
`%!in%` = Negate(`%in%`)

## 0.2 DB connections ----
# DB connections & functions
mars_con <- dbConnect(RPostgres::Postgres(),
                      host = "PWDMARSDBS1.pwd.phila.local",
                      port = 5434,
                      dbname = "mars_data",
                      user = Sys.getenv("shiny_uid"),
                      password = Sys.getenv("shiny_pwd"))

# replace special characters with friendlier characters
special_char_replace <- function(note){
  
  note_fix <- note %>%
    str_replace_all(c("•" = "-", "ï‚§" = "-", "“" = '"', '”' = '"'))
  
  return(note_fix)
  
}
## 0.3 Loading required tables  ----
# Get wic tables
# WICS 
wic_sys <- dbGetQuery(mars_con, "SELECT *, data.fun_date_to_fiscal_quarter(date) as quarter FROM fieldwork.viw_wics_100ft")
# WICs Comments
wic_comments <- dbGetQuery(mars_con, "SELECT * FROM fieldwork.beta_tbl_wic_comment")

# Look up tables for status record keeping
wic_system_status_lookup <- dbGetQuery(mars_con, "SELECT * FROM fieldwork.beta_tbl_wic_system_status_lookup")
wic_wo_status_lookup <- dbGetQuery(mars_con, "SELECT * FROM fieldwork.beta_tbl_wic_wo_status_lookup")

# WIC Polygons- crs global 4326 for leaflet mapping
wic_property_geom <- st_read(dsn = mars_con, Id(schema="fieldwork", table = "beta_tbl_wic_propertyline_geom")) %>%
  st_transform(crs = 4326) %>%
  filter(address %in% wic_sys$wic_address)

wic_footprint_geom <- st_read(dsn = mars_con, Id(schema="fieldwork", table = "beta_tbl_wic_footprint_geom")) %>%
  st_transform(crs = 4326) %>%
  filter(address %in% wic_sys$wic_address)

# Grouping smp polygons by system- setting crs global 4326 for leaflet mapping 
wic_smp_geom <- st_read(dsn = mars_con, Id(schema="fieldwork", table = "beta_tbl_wic_smp_geom")) %>%
  st_transform(crs = 4326) 
wic_smp_geom['system_id'] <- gsub('-\\d+$','', wic_smp_geom$smp_id) 

# grouping by system-id and setting CRS= 2272 for st-buffer
wic_sys_geom <- wic_smp_geom %>%
  group_by(system_id) %>%
  summarise(system_geom = st_combine(smp_geom)) %>%
  st_as_sf() %>%
  st_transform(crs = 2272)
# creating a buffer spatial layer for mapping. This is to show intersection of buffer and wics.
wic_sys_geom_buffered <- st_buffer(wic_sys_geom, 100) %>%
  st_transform(crs = 4326)

# gauge data
gauge_event <- dbGetQuery(mars_con, "SELECT distinct tbl_gage_event.gage_uid, tbl_gage_event.eventdatastart_edt::date AS event_startdate FROM data.tbl_gage_event where tbl_gage_event.eventdataend_edt > '2010-01-01'")
gauge_sys <- dbGetQuery(mars_con, "select distinct admin.fun_smp_to_system(smp_id) as system_id, gage_uid from admin.tbl_smp_gage where smp_id like '%-%-%'")

#Deployment History
# Get deployment table
deployments_list <- dbGetQuery(mars_con, "SELECT distinct admin.fun_smp_to_system(smp_id) as system_id FROM fieldwork.viw_deployment_full_cwl") 
# Find the immediate event_startdate smaller than complaint_date
#immediate_event <- event_startdate[event_startdate < complaint_date] %>%
# max(na.rm = TRUE)

## 0.4 Loading UI selection options ---- 
# System ids
system_id_all <- odbc::dbGetQuery(mars_con, "select distinct system_id from external.mat_assets where system_id like '%-%'") %>% 
  dplyr::arrange(system_id) %>%  
  dplyr::pull()

# Workorder ids
wo_id_all <- wic_sys %>% 
  select(workorder_id) %>%
  dplyr::pull()

# Fiscal quarter lookup
fq_lookup <- dbGetQuery(mars_con,"select * from admin.tbl_fiscal_quarter_lookup")
q_list <- fq_lookup %>%
  select(fiscal_quarter) %>%
  pull

# WIC system Status list
status_choice <- wic_system_status_lookup %>%
  select(status) %>%
  distinct() %>%
  pull

# 1.0 Define UI ----
# Define UI
ui <- tagList(useShinyjs(), navbarPage("WIC App v4.0", id = "TabPanelID", theme = shinytheme("cyborg"),
                                       ## 1.1 Tab "WIC Status" ----
                                       tabPanel("WIC Status", value = "status", 
                                                titlePanel("System Status Near WICs"),
                                                sidebarLayout(
                                                  
                                                  sidebarPanel(
                                                    selectInput("system_id", "System ID", choices = c("All", system_id_all), selected = "All"),
                                                    selectInput("date_range", "Date Range", choices = c("To-Date", "Fiscal Quarter")),
                                                    conditionalPanel(condition = "input.date_range == 'Fiscal Quarter'", 
                                                                     fluidRow(column(12,
                                                                                     selectInput("f_q", "Fiscal Quarter", choices = q_list, selected = "FY24Q2")))
                                                    ), 
                                                    selectInput("status", "System Status", choices = c("All", status_choice) , selected = "All"),
                                                    sliderInput("prop_dist",
                                                                "Property Distance (ft):",
                                                                min = 0,
                                                                max = 100,
                                                                value = 25),
                                                    checkboxInput("single_wic", "Only Show Most Recent WIC per System ID", value = TRUE, width = NULL),
                                                    downloadButton("download_table", "Download"), actionButton("clear_main", "Clear All Fields"),
                                                    width = 3
                                                  ),
                                                  
                                                  mainPanel(
                                                    strong(span(textOutput("table_name"), style = "font-size:22px")),
                                                    reactableOutput("wic_table"), width = 9
                                                    
                                                  )
                                                )
                                       ),
                                       ## 1.2 Tab "WIC Investigation" ----
                                       tags$head(
                                         tags$style(HTML("
                                                    .custom-box {
                                                      background-color: #2c2c2c; /* Dark grey background */
                                                      color: white; /* White text color */
                                                      border: 1px solid #4d4d4d; /* Border color */
                                                    }
                                                    .custom-box .box-header {
                                                      background-color: #3c3c3c; /* Slightly lighter header */
                                                      color: white; /* Header text color */
                                                    }
                                                  "))
                                                                                     ),
                                       tabPanel("WIC Investigation", value = "wic_insight",
                                                fluidRow(
                                                  column(7,
                                                         box(width = NULL, solidHeader = TRUE, status = "primary", class = "custom-box",
                                                             fluidRow(
                                                               column(3, selectizeInput('system_id_edit', "System ID", choices = c("", system_id_all), selected = "")),
                                                               column(3, selectizeInput('workorder_edit', "Work Order ID", choices = c("", wo_id_all), selected = "")),
                                                               column(3, selectInput("edit_status", "System Status", choices = c("", status_choice), selected = "")),
                                                               column(3, selectInput("check_woid", "Work Order Status", choices = c("", "Checked", "Unchecked"), selected = ""))
                                                             ),
                                                             fluidRow(
                                                               column(12, conditionalPanel("input.sys_stat_selected != 0", textAreaInput("system_note", "Notes", width = "100%", height = "40%")))
                                                             ),
                                                             fluidRow(
                                                               column(12, actionButton("save_edit", "Save/Edit"), actionButton("clear", "Clear All Fields"))
                                                             )
                                                         ),
                                                         h4(textOutput("sys_stat_table_name")),
                                                         reactableOutput("sys_stat_table"),
                                                         h4(textOutput("wo_stat_table_name")),
                                                         reactableOutput("wo_stat_table")
                                                  ),
                                                  column(5, leafletOutput("map", width = "100%", height = "1000"))
                                                )
                                       ),
                                       ## 1.3 Tab "Documentation" ----
                                       tabPanel("Documentation", value = "document", 
                                                titlePanel("App History"),
                                                sidebarLayout(
                                                  sidebarPanel(
                                                    fluidRow()
                                                    
                                                    
                                                  ),
                                                  mainPanel(
                                                    h2(textOutput("app_notes")),
                                      
                                                    
                                                  ))
                                                
                                       )
)
)

# 2.0 Define server logic ----
server <- function(input, output, session) {
  ## 2.1 Serverside data prep ---- 
  # Table name
  output$table_name <- renderText(ifelse(input$single_wic, return(paste("Most Recent WIC Per System within ", input$prop_dist," ft from Property Line: ")), return(paste("All WICs within ", input$prop_dist," ft from Property Line: "))))
  
  #initialzie reactive values
  rv <- reactiveValues()

  #process text field to prevent sql injection
  rv$reason_step <- reactive(gsub('\'', '\'\'',  input$system_note))
  rv$input_note  <- reactive(special_char_replace(rv$reason_step()))
  
  # row selections 
  rv$row_wic_table <- reactive(getReactableState("wic_table", "selected"))
  rv$row_sys_stat_table <- reactive(getReactableState("sys_stat_table", "selected"))
  rv$row_wo_stat_table <- reactive(getReactableState("wo_stat_table", "selected"))
  
  
  # Tables with the system ids and workorder ids for status record keeping
  rv$wic_system_status <- reactive(dbGetQuery(mars_con, "SELECT * FROM fieldwork.beta_tbl_wic_system_status
                                              INNER JOIN fieldwork.beta_tbl_wic_system_status_lookup
                                              USING(wic_system_status_lookup_uid)"))
  
  rv$wic_wo_status <- reactive(dbGetQuery(mars_con, "SELECT * FROM fieldwork.beta_tbl_wic_wo_status 
                                              INNER JOIN fieldwork.beta_tbl_wic_wo_status_lookup
                                              USING(wic_wo_status_lookup_uid)"))
  # filtering table to show most recent wo-id per system
  all_wo <- wic_sys %>%
    select(workorder_id) %>%
    pull
  single_wo <- wic_sys %>%
    arrange(desc(date)) %>%
    group_by(system_id) %>%
    summarise(workorder_id = workorder_id[1]) %>%
    pull
  # reactive filters
  rv$date_filter <- reactive(ifelse(input$date_range == "To-Date", return(q_list), return(input$f_q)))
  rv$sys_filter <- reactive(ifelse(input$system_id == "All", return(system_id_all), return(input$system_id)))
  rv$status_filter <- reactive(ifelse(input$status == "All", return(status_choice), return(input$status)))
  rv$recent_wic_filter <- reactive(ifelse(input$single_wic, return(single_wo), return(all_wo)))
  
  ## 2.2 Reactive filtering for tab "WIC Status" ----  
  # filter the table based on the sidebar inputs-get the most recent wic per system
  rv$wic_table_filter <- reactive(wic_sys %>%
                                    inner_join(rv$wic_system_status(), by = "system_id") %>%
                                    filter(workorder_id %in% rv$recent_wic_filter() &
                                             quarter %in% rv$date_filter() &
                                             system_id %in% rv$sys_filter() &
                                             status %in% rv$status_filter() &
                                             property_dist_ft <= input$prop_dist))
  
  ## 2.3 Output wic_table for Tab "WIC Status"----
  output$wic_table <- renderReactable(
    reactable(rv$wic_table_filter() %>%
                select(`System ID` = system_id, `Workorder ID` = workorder_id, `Address` = wic_address, `WIC Date` = date, Phase = phase, `Dist. Property (ft)` = property_dist_ft, `Dist. Footprint (ft)` = footprint_dist_ft , `System Status` = status), 
              fullWidth = TRUE,
              selection = "single",
              searchable = TRUE,
              onClick = "select",
              showPageSizeOptions = TRUE,
              pageSizeOptions = c(25, 50, 100),
              defaultPageSize = 25,
              height = 1000,
              theme = darkly(),
              columns = list(
                `System ID` = colDef(width = 100),
                `Workorder ID` = colDef(width = 120),
                `Dist. Property (ft)` = colDef(width = 140),
                `Dist. Footprint (ft)` = colDef(width = 140),
                 Phase = colDef(width = 150)),
              details = function(index) {
                sys_nested_notes <- rv$wic_system_status()[rv$wic_system_status()$system_id == rv$wic_table_filter()$system_id[index], ] %>%
                  select(`MARS Comments on the System:` = notes)
                htmltools::div(style = "padding: 1rem",
                               reactable(sys_nested_notes, 
                                         theme = darkly(),
                                         columns = list(
                                           `MARS Comments on the System:` = colDef(width = 950)
                                         ), 
                                         outlined = TRUE)
                )
              }
              ))
  
  ## 2.4 Download button ----
  output$download_table <- downloadHandler(
    
    filename = function() {
      paste("All WICs (100 ft)", "_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(filename){
      
      df_list <- list(wic_sys %>%
                        inner_join(rv$wic_system_status(), by = "system_id") %>%
                        inner_join(wic_comments, by = "workorder_id") %>%
                        select(SystemID = system_id, WorkorderID = workorder_id, Address = wic_address, Date = date, Phase = phase, DistProperty_ft = property_dist_ft, DistFootprint_ft = footprint_dist_ft , SystemStatus = status, CityWorks_Comment = comment))
      write.xlsx(x = df_list , file = filename)
    }
  )
  
  
  ## 2.5 Clear button Tab 1----
  observeEvent(input$clear_main, {
    showModal(modalDialog(title = "Clear All Fields", 
                          "Are you sure you want to clear all fields on this tab?", 
                          modalButton("No"), 
                          actionButton("confirm_clear_main_pcs", "Yes")))
  })
  
  # clear button
  observeEvent(input$confirm_clear_main_pcs, {
    reset("status")
    reset("prop_dist")
    reset("f_q")
    reset("date_range")
    reset("system_id")
    reset("single_wic")
    reset("table_name")
    reset("wic_table")
    updateReactable("wic_table", selected = NA)
    
    removeModal()
  })
  
  ## 2.6 Switch tab to "WIC Investigation"----
  
 # Toggle state to switch select inputs
  observe(toggleState(id = "system_id_edit", condition = is.null(rv$row_wo_stat_table())  ))
  observe(toggleState(id = "workorder_edit", condition = !is.null(rv$row_wo_stat_table())))
  observe(toggleState(id = "check_woid", condition = !is.null(rv$row_wo_stat_table())))
  observe(toggleState(id = "edit_status", condition = !is.null(rv$row_sys_stat_table())))
  observe(toggleState(id = "save_edit", condition = !is.null(rv$row_wo_stat_table()) | !is.null(rv$row_sys_stat_table())))
  observe(toggleState(id = "clear", condition = !is.null(rv$row_wo_stat_table()) | !is.null(rv$row_sys_stat_table()) | input$system_id_edit != "" ))
  
  

  
  observeEvent(rv$row_wic_table(), {
    if (!is.null(rv$row_wic_table())) {
      updateTabsetPanel(session, "TabPanelID", selected = "wic_insight")
      updateSelectInput(session, "system_id_edit", selected = rv$wic_table_filter()$system_id[rv$row_wic_table()])
     
    }
    
  })
  
  # Only select row from a single table at a time 
  observeEvent(rv$row_sys_stat_table(), {
    if(!is.null(rv$row_sys_stat_table())){
    updateReactable("wo_stat_table", selected = NA)
    reset("check_woid")
    reset("workorder_edit")
    updateSelectInput(session, "edit_status", selected = rv$sys_stat()$status[rv$row_sys_stat_table()])
    updateTextAreaInput(session, "system_note", value = rv$sys_stat()$notes[rv$row_sys_stat_table()])
    }
  }
  )
  
  observeEvent(rv$row_wo_stat_table(), {
    if(!is.null(rv$row_wo_stat_table())){
    updateReactable("sys_stat_table", selected = NA)
    reset("edit_status")
    reset("system_note")
    updateSelectInput(session, "check_woid", selected = rv$wo_stat()$status[rv$row_wo_stat_table()])
    updateSelectInput(session, "workorder_edit", selected = rv$wo_stat()$workorder_id[rv$row_wo_stat_table()])  
    }
  }
  )
 
 ## 2.6 WIC Investigation Server Side ----
  output$sys_stat_table_name <- renderText(paste("System ", input$system_id_edit, " Status and Notes:"))
  output$wo_stat_table_name <- renderText(paste("System ", input$system_id_edit, " WIC Details:"))

  # ### 2.6.1 Mapping ----
  output$map <- renderLeaflet({
    # creating basemap here 
    map <- leaflet() %>%
      addProviderTiles(providers$OpenStreetMap, group = 'OpenStreetMap', options = providerTileOptions(maxZoom = 20)) %>%
      addProviderTiles(providers$Esri.WorldTopoMap, group = 'Esri.WorldTopoMap', options = providerTileOptions(maxZoom = 20)) %>%
      addProviderTiles(providers$Esri.WorldImagery, group='Esri.WorldImagery', options = providerTileOptions(maxZoom = 19)) %>%
      addPolygons(data = wic_sys_geom_buffered, color = "#00ffbf", label = paste("System ID: ", wic_sys_geom_buffered$system_id), labelOptions = labelOptions(style = list("font-weight" = "bold", "font-size" = "14px")), group = "Buffer (100ft)") %>%
      addPolygons(data = wic_smp_geom, color = "#000000", label = paste("System ID: ", wic_smp_geom$system_id), labelOptions = labelOptions(style = list("font-weight" = "bold", "font-size" = "14px")), group = "System", layerId = ~system_id) %>%
      addPolygons(data = wic_property_geom, color = "red", label = paste("Address: ", wic_property_geom$address), labelOptions = labelOptions(style = list("font-weight" = "bold", "font-size" = "14px")), group = "Property Line") %>%
      addPolygons(data = wic_footprint_geom, color = "purple", label = paste("Address", wic_footprint_geom$address), labelOptions = labelOptions(style = list("font-weight" = "bold", "font-size" = "14px")), group = "Footprint") %>%
      addLayersControl(overlayGroups = c("System","Buffer (100ft)","Property Line", "Footprint"), baseGroups = c("OpenStreetMap", "Esri.WorldTopoMap", "Esri.WorldImagery")) %>%
      hideGroup(c("Footprint", "Buffer (100ft)")) %>%
      setView(lng = -75.1652 , lat = 39.9526  , zoom = 11) # zoom in philly
    
  })
  
  observeEvent(list(input$system_id_edit, rv$row_wo_stat_table()), {
    # Calculate the bounds of the system polygon the view is set
    selected_system_geom <- wic_smp_geom %>%
      filter(system_id == input$system_id_edit) 
    
    bounds <- st_bbox(selected_system_geom)
    
    # Calculate the center of the bounds
    center_lat <- (unname(bounds["ymin"]) + unname(bounds["ymax"])) / 2
    center_lng <- (unname(bounds["xmin"]) + unname(bounds["xmax"])) / 2
    
    # get the selected wic geom to highlight
    selected_wic_geom <- wic_property_geom %>%
      filter(address == ifelse(!is.null(rv$row_wo_stat_table()), rv$wo_stat()[rv$row_wo_stat_table(), "wic_address"], "Ignore")) 
    
    if (nrow(selected_system_geom) == 0) {
      leafletProxy("map") %>%
        clearGroup("selected_wic") %>%
        clearGroup("selected_system") %>%
        setView(lng = -75.1652 , lat = 39.9526  , zoom = 11) 
      
    } else if (nrow(selected_system_geom) > 0 & nrow(selected_wic_geom) == 0) {
      leafletProxy("map") %>%
        clearGroup("selected_wic") %>%
        clearGroup("selected_system") %>%
        addPolygons(data = selected_system_geom,
                    fillColor = "black",   # Change to desired fill color
                    color = "yellow",
                    weight = 2,           # Border weight
                    opacity = 1,          # Border opacity
                    fillOpacity = 0.5,    # Fill opacity
                    label = paste("Selected System ID:", input$system_id_edit),  # Always visible label
                    labelOptions = labelOptions(style = list("font-weight" = "bold", "font-size" = "14px")),
                    group = "selected_system") %>%
        setView(lng = center_lng, lat = center_lat, zoom = 19) 
      
    } else if(nrow(selected_system_geom) > 0 & nrow(selected_wic_geom) > 0){
      leafletProxy("map") %>%
        clearGroup("selected_wic") %>%
        clearGroup("selected_system") %>%
        addPolygons(data = selected_system_geom,
                    fillColor = "black",   # Change to desired fill color
                    color = "yellow",
                    weight = 2,           # Border weight
                    opacity = 1,          # Border opacity
                    fillOpacity = 0.5,    # Fill opacity
                    label = paste("Selected System ID:", input$system_id_edit),  # Always visible label
                    labelOptions = labelOptions(style = list("font-weight" = "bold", "font-size" = "14px")),
                    group = "selected_system") %>%
        addPolygons(data = selected_wic_geom,
                    fillColor = "yellow",  # Change to your desired highlight color
                    color = "#ff9900",
                    weight = 2,           # Border weight
                    opacity = 1,          # Border opacity
                    fillOpacity = 0.7,    # Fill opacity
                    label = paste("Selected WIC Address:", selected_wic_geom$address),  # Display address
                    labelOptions = labelOptions(style = list("font-weight" = "bold", "font-size" = "14px")),
                    group = "selected_wic") %>%
        setView(lng = center_lng, lat = center_lat, zoom = 19) 
    }
  })
  
  
  # Observe click events on the map
  observeEvent(input$map_shape_click, {
    click <- input$map_shape_click
    if (!is.null(click)) {
        system_id_clicked <- click$id  # Get the system_id from the clicked polygon
        updateSelectInput(session, "system_id_edit", selected = system_id_clicked)
    }
  })


  
  ## 2.6.2 System and work order status tables ----
  
  # sys_stat table filtered
  rv$sys_stat <- reactive(wic_sys %>%
                            filter(system_id == input$system_id_edit) %>%
                            inner_join(rv$wic_system_status(), by = "system_id") %>%
                            select(system_id, status, notes) %>%
                            distinct())
  
  output$sys_stat_table <- renderReactable(
    reactable(rv$sys_stat() %>%
                select(`System ID` = system_id, `System Status` = status),
              theme = darkly(),
              defaultPageSize = 1,
              fullWidth = TRUE,
              selection = "single",
              onClick = "select",
              selectionId = "sys_stat_selected",
              searchable = FALSE,
              details = function(index) {
                sys_stat_nested_notes <- rv$wic_system_status()[rv$wic_system_status()$system_id == rv$sys_stat()$system_id[index], ] %>%
                  select(`MARS Comments on the System:` = notes)
                htmltools::div(style = "padding: 1rem",
                               reactable(sys_stat_nested_notes, 
                                         theme = darkly(),
                                         columns = list(
                                           `MARS Comments on the System:` = colDef(width = 1000)
                                         ), 
                                         outlined = TRUE)
                )
              }
              
    ))
  
  # wo_stat table filtered
  rv$wo_stat <- reactive(wic_sys %>%
                           filter(system_id == input$system_id_edit) %>%
                           inner_join(rv$wic_wo_status(), by = "workorder_id") %>%
                           select(workorder_id, wic_address, date, phase, property_dist_ft, footprint_dist_ft, status) %>%
                           distinct())

  output$wo_stat_table <- renderReactable(
    reactable(rv$wo_stat() %>%
                select(`Workorder ID` = workorder_id, `Address` = wic_address, `WIC Date` = date, Phase = phase, `Dist. Property (ft)` = property_dist_ft, `Dist. Footprint (ft)` = footprint_dist_ft, `WIC Status` = status),
              theme = darkly(),
              defaultPageSize = 15,
              fullWidth = TRUE,
              selection = "single",
              onClick = "select",
              selectionId = "wo_stat_selected",
              searchable = FALSE,
              columns = list(
                `Workorder ID` = colDef(width = 120),
                Address = colDef(width = 200),
                `Dist. Property (ft)` = colDef(width = 140),
                `Dist. Footprint (ft)` = colDef(width = 140),
                Phase = colDef(width = 150)),
              details = function(index) {
                cw_wic_nested_notes <- wic_comments[wic_comments$workorder_id == rv$wo_stat()$workorder_id[index], ] %>%
                  select(`Cityworks Comments on the Workorder:` = comment)
                htmltools::div(style = "padding: 1rem",
                               reactable(cw_wic_nested_notes, 
                                         theme = darkly(),
                                         columns = list(
                                           `Cityworks Comments on the Workorder:` = colDef(width = 1000)
                                         ), 
                                         outlined = TRUE)
                )
              },
              rowStyle = function(index) {
                if (rv$wo_stat()$status[index] == "Checked") {
                  list(backgroundColor = "darkgreen")  # Light green background for checked status
                } else {
                  NULL  # No additional style for other statuses
                }
              }

    )
    )
  
  
  ## 2.6.3 Save/edit----
  
  ### On click "save_edit"
  
  observeEvent(input$save_edit, {
    if(!is.null(rv$row_sys_stat_table())){
      
      rv$sys_stat_uid <- reactive(wic_system_status_lookup %>%
        filter(status == input$edit_status) %>%
        select(wic_system_status_lookup_uid) %>%
        pull())
      
      rv$update_sys_status_q <- reactive(paste("Update fieldwork.beta_tbl_wic_system_status SET notes = '",  rv$input_note(), "', wic_system_status_lookup_uid = ", ifelse(length(rv$sys_stat_uid()) == 0, 4, rv$sys_stat_uid())," where system_id = '", input$system_id_edit, "'", sep = ""))
      dbGetQuery(mars_con, rv$update_sys_status_q())
      
      # refresh data and reset tables
      rv$wic_system_status <- reactive(dbGetQuery(mars_con, "SELECT * FROM fieldwork.beta_tbl_wic_system_status
                                              INNER JOIN fieldwork.beta_tbl_wic_system_status_lookup
                                              USING(wic_system_status_lookup_uid)"))
      
      reset("sys_stat_table")
      reset("wo_stat_table")
      reset("wic_table")
      reset("edit_status")
      reset("system_note")
      
    } else if(!is.null(rv$row_wo_stat_table())) {
      
      rv$wo_stat_uid <- reactive(wic_wo_status_lookup %>%
                                    filter(status == input$check_woid) %>%
                                    select(wic_wo_status_lookup_uid) %>%
                                    pull())
      
      
      rv$update_wo_status_q <- reactive(paste("Update fieldwork.beta_tbl_wic_wo_status SET wic_wo_status_lookup_uid = ", ifelse(length(rv$wo_stat_uid()) == 0, 2, rv$wo_stat_uid())," where workorder_id = ", input$workorder_edit, sep = ""))
      dbGetQuery(mars_con, rv$update_wo_status_q())
      
      # refresh data and reset tables
      rv$wic_wo_status <- reactive(dbGetQuery(mars_con, "SELECT * FROM fieldwork.beta_tbl_wic_wo_status 
                                              INNER JOIN fieldwork.beta_tbl_wic_wo_status_lookup
                                              USING(wic_wo_status_lookup_uid)"))
      reset("sys_stat_table")
      reset("wo_stat_table")
      reset("wic_table")
      reset("workorder_edit")
      reset("check_woid")
    }
})
  

  
  ## 2.6.4 Clear button Tab 2----
  observeEvent(input$clear, {
    showModal(modalDialog(title = "Clear All Fields", 
                          "Are you sure you want to clear all fields on this tab?", 
                          modalButton("No"), 
                          actionButton("confirm_clear_pcs", "Yes")))
  })
  
  # clear button
  observeEvent(input$confirm_clear_pcs, {
    
    reset("sys_stat_table")
    reset("wo_stat_table")
    reset("wic_table")
    reset("workorder_edit")
    reset("check_woid")
    reset("edit_status")
    reset("system_note")
    reset("system_id_edit")
    updateReactable("wic_table", selected = NA)
    
    removeModal()
  })
  
}
# Run the application 
shinyApp(ui = ui, server = server)
