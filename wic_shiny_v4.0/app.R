### Water-in-Cellar Shiny App V 4.0 
### Notes: Major overhaul of the maint. script + addition of POSTGIS to the PostgreSQL DB
### Author: Farshad Ebrahimi 
### Last Modified: 10/01/2024

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
library(data.table)

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
wic_comments <- dbGetQuery(mars_con, "SELECT * FROM fieldwork.tbl_wic_comment")

# Look up tables for status record keeping
wic_system_status_lookup <- dbGetQuery(mars_con, "SELECT * FROM fieldwork.tbl_wic_system_status_lookup")
wic_wo_status_lookup <- dbGetQuery(mars_con, "SELECT * FROM fieldwork.tbl_wic_wo_status_lookup")

# WIC Polygons- crs global 4326 for leaflet mapping
wic_property_geom <- st_read(dsn = mars_con, Id(schema="fieldwork", table = "tbl_wic_propertyline_geom")) %>%
  st_transform(crs = 4326) %>%
  filter(address %in% wic_sys$wic_address)

wic_footprint_geom <- st_read(dsn = mars_con, Id(schema="fieldwork", table = "tbl_wic_footprint_geom")) %>%
  st_transform(crs = 4326) %>%
  filter(address %in% wic_sys$wic_address)

# Grouping smp polygons by system- setting crs global 4326 for leaflet mapping 
wic_smp_geom <- st_read(dsn = mars_con, Id(schema="fieldwork", table = "tbl_wic_smp_geom")) %>%
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

## 0.4 Loading UI selection options ---- 
# System ids
system_id_assets <- dbGetQuery(mars_con, "SELECT DISTINCT system_id FROM external.mat_assets WHERE system_id ~ '^[0-9]+-[0-9]+$' order by system_id;")

system_id_geom <- wic_smp_geom %>% 
  st_set_geometry(NULL) %>%
  select(system_id) %>%
  distinct()

system_id_all <- union_all(system_id_assets, system_id_geom) %>%
  distinct() %>%
  arrange(system_id) %>%
  pull

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

# life cycle status
lifecycle_status <- dbGetQuery(mars_con,"select distinct system_id, lifecycle_status from external.viw_assets_smp_lifecycle_status")


# cipit status
systembdv <- dbGetQuery(mars_con,"select distinct system_id, sys_dataphase, cipit_status from external.tbl_systembdv")


# 1.0 Define UI ----
# Define UI
ui <- tagList(useShinyjs(), navbarPage("WIC App v4.0", id = "TabPanelID", theme = shinytheme("cyborg"),
                                       ## 1.1 Tab "WIC Status" ----
                                       tabPanel("System WIC Status", value = "status",
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
                                                    strong(span(textOutput("table_name"), style = "color: deepskyblue; font-size:22px")),
                                                    tags$head(
                                                      # Add custom CSS to change the header background color
                                                      tags$style(HTML("
                                                                        .rt-th {
                                                                          background-color:  #00CC99; /* Change this to your desired color */
                                                                          color: black; /* Text color */
                                                                        }
                                                                      "))),

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
                                                               column(12, actionButton("save_edit", "Save/Edit"), actionButton("clear", "Clear All Fields"), actionButton("redraw", "Re-Draw Map"))
                                                             )
                                                         ),
                                                         strong(span(textOutput("sys_stat_table_name"), style = "color: deepskyblue; font-size:22px")),
                                                         reactableOutput("sys_stat_table"),
                                                         strong(span(textOutput("wo_stat_table_name"), style = "color: deepskyblue; font-size:22px")),
                                                         reactableOutput("wo_stat_table")

                                                  ),
                                                  column(5, leafletOutput("map", width = "100%", height = "1000"))
                                                )
                                       ),
                                       ## 1.3 Tab "Documentation" ----
                                       tabPanel("Documentation", value = "document",
                                                titlePanel(tags$p("Release Notes by ",
                                                                  tags$a(href = "mailto:farshad.ebrahimi@phila.gov", "Farshad Ebrahimi"))),
                                                column(4,
                                                       h4(style = "color: deepskyblue;", 'WIC App v.4.0.0, 10/01/2024:'),
                                                       h5("1. The app was reorganized to show system status (E.g., Need 2nd Opinion) and be filterable by sidebar"),
                                                       h5("2. A landing page showing all WICs and system status was added"),
                                                       h5("3.	WIC Investigation tab has system-specific meta data"),
                                                       h5("4.	WIC Investigation tab is interactive now, user can add status to systems as well as check/uncheck each WIC"),
                                                       h5("5.	Rain gauge meta data, most recent rain date and days from rain meta data was added to WIC Investigation tab"),
                                                       h5("6.	The map uses leafletletproxy for smoother user experience"),
                                                       h5("7.	Systems are now clickable in the map, updating the adjacent tables with system data"),
                                                       h5("8.	All WICs are now shown in the map"),
                                                       h5("9.	The app now pulls the spatial polygons directly from MARS DB with addition of POSTGIS extention"),
                                                       h4(style = "color: deepskyblue;", 'WIC App v.3.0.0, 11/02/2023:'),
                                                       h5("1.	Added “Eligible for Monitoring” column to the WIC app to show if the system is in the unmonitored smp app list"),
                                                       h5("2. The adjacent GSI systems have been incorporated  into the map as “All SMP” layer that can be shown through the top right menu of the map (default: hidden)"),
                                                       h5("3.	Comment section became an arrow (nested table) that expands if the user clicks"),
                                                       h5("4.	Highlighting keywords such as RAIN, STORMWATER, GSI can help sort out the systems: I added “Keywords” column in the nested table, if empty, no keywords detected "),
                                                       h5("5.	Changed the columns width so that all columns have appropriate space (system id should be narrow, address line wider) "),
                                                       h5("6.	Clicking on the table rows should highlight the map polygon in yellow"),
                                                       h5("7.	At %100 browser zoom level, all components of the app (totalizer, map, and table) are reasonably visible and there is not a need to scroll to see things. "),
                                                       h5("8.	Totalizer and versioning comments are now under a new tab (“Totalizer”)."))
                                                
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
  rv$wic_system_status <- reactive(dbGetQuery(mars_con, "SELECT * FROM fieldwork.tbl_wic_system_status
                                              INNER JOIN fieldwork.tbl_wic_system_status_lookup
                                              USING(wic_system_status_lookup_uid)"))
  
  rv$wic_wo_status <- reactive(dbGetQuery(mars_con, "SELECT * FROM fieldwork.tbl_wic_wo_status 
                                              INNER JOIN fieldwork.tbl_wic_wo_status_lookup
                                              USING(wic_wo_status_lookup_uid)"))
  # filtering table to show most recent wo-id per system
  all_wo <- wic_sys %>%
    select(wic_system_uid) %>%
    pull
  single_wo <- wic_sys %>%
    arrange(desc(date)) %>%
    group_by(system_id) %>%
    summarise(wic_system_uid = wic_system_uid[1], workorder_id = workorder_id[1]) %>%
    select(wic_system_uid) %>%
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
                                    filter(wic_system_uid %in% rv$recent_wic_filter() &
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
                `Dist. Property (ft)` = colDef(width = 140, style = list(textAlign = "center")),
                `Dist. Footprint (ft)` = colDef(width = 140, style = list(textAlign = "center")),
                 Phase = colDef(width = 150)),
              details = function(index) {
                sys_nested_notes <- rv$wic_system_status()[rv$wic_system_status()$system_id == rv$wic_table_filter()$system_id[index], ] %>%
                  select(`MARS Comments on the System:` = notes)
                htmltools::div(style = "padding: 1rem",
                               reactable(sys_nested_notes, 
                                         theme = darkly(),
                                         list(`MARS Comments on the System:`= colDef(html = TRUE, style = list(color = "#F5DEB3", fontweight = "bold"))),
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
  
  ## Switch tab to "WIC Investigation"
 # Toggle state to switch select inputs
  observe(toggleState(id = "system_id_edit", condition = is.null(rv$row_wo_stat_table())  ))
  observe(toggleState(id = "workorder_edit", condition = FALSE))
  observe(toggleState(id = "check_woid", condition = !is.null(rv$row_wo_stat_table())))
  observe(toggleState(id = "edit_status", condition = !is.null(rv$row_sys_stat_table())))
  observe(toggleState(id = "save_edit", condition = (!is.null(rv$row_wo_stat_table()) | !is.null(rv$row_sys_stat_table())) & ((input$workorder_edit != "" & input$check_woid !="") | (input$system_id_edit != "" & input$edit_status != ""))))
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
 
 # 2.6 WIC Investigation Server Side ----
  output$sys_stat_table_name <- renderText(paste("Status & Notes for System: ", input$system_id_edit))
  output$wo_stat_table_name <- renderText(paste( "WIC Details for System: ", input$system_id_edit))

  # ### 2.6.1 Mapping ----
  output$map <- renderLeaflet({
    # creating basemap here 
    map <- leaflet() %>%
      addProviderTiles(providers$OpenStreetMap, group = 'OpenStreetMap', options = providerTileOptions(maxZoom = 19)) %>%
      addProviderTiles(providers$Esri.WorldTopoMap, group = 'Esri.WorldTopoMap', options = providerTileOptions(maxZoom = 19)) %>%
      addProviderTiles(providers$Esri.WorldImagery, group='Esri.WorldImagery', options = providerTileOptions(maxZoom = 19)) %>%
      addPolygons(data = wic_sys_geom_buffered, color = "#00ffbf", label = paste("System ID: ", wic_sys_geom_buffered$system_id), labelOptions = labelOptions(style = list("font-weight" = "bold", "font-size" = "14px")), group = "Buffer (100ft)") %>%
      addPolygons(data = wic_smp_geom, color = "#000000", label = paste("System ID: ", wic_smp_geom$system_id), labelOptions = labelOptions(style = list("font-weight" = "bold", "font-size" = "14px")), group = "System", layerId = ~system_id) %>%
      addPolygons(data = wic_property_geom, color = "red", label = paste("Address: ", wic_property_geom$address), labelOptions = labelOptions(style = list("font-weight" = "bold", "font-size" = "14px")), group = "Property Line") %>%
      addPolygons(data = wic_footprint_geom, color = "purple", label = paste("Address", wic_footprint_geom$address), labelOptions = labelOptions(style = list("font-weight" = "bold", "font-size" = "14px")), group = "Footprint") %>%
      addLayersControl(overlayGroups = c("System","Buffer (100ft)","Property Line", "Footprint"), baseGroups = c("OpenStreetMap", "Esri.WorldTopoMap", "Esri.WorldImagery")) %>%
      hideGroup(c("Footprint", "Buffer (100ft)")) %>%
      addLegend(colors = c("#000000","#00ffbf","red","purple"), 
                labels = c("System","Buffer Area (100ft)" , "WIC Property Line","WIC Building Footprint")) %>%
      addDrawToolbar(polylineOptions = drawPolylineOptions(metric = FALSE, feet = TRUE),
                     polygonOptions = FALSE,
                     circleOptions=FALSE,
                     rectangleOptions=FALSE,
                     markerOptions=FALSE,
                     circleMarkerOptions= FALSE,
                     editOptions=editToolbarOptions(selectedPathOptions=selectedPathOptions())) %>%
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
  
  # Days from rain calcs
  rv$sys_rains <- reactive(gauge_sys %>%
                             inner_join(gauge_event, by = "gage_uid") %>%
                             filter(system_id == input$system_id_edit) %>%
                             select(event_startdate) %>%
                             arrange(desc(event_startdate)))
  
  # sys_stat table filtered
  rv$sys_stat <- reactive(wic_sys %>%
                            filter(system_id == input$system_id_edit) %>%
                            left_join(rv$wic_system_status(), by = "system_id") %>%
                            left_join(gauge_sys, by = "system_id") %>%
                            left_join(systembdv, by = "system_id") %>%
                            left_join(lifecycle_status, by = "system_id") %>%
                            mutate(monitored = ifelse(system_id %in% deployments_list$system_id, "Yes", "No")) %>%
                            select(system_id, status, notes, monitored, gage_uid, sys_dataphase, cipit_status, lifecycle_status) %>%
                            distinct())
  
  output$sys_stat_table <- renderReactable(
    reactable(rv$sys_stat() %>%
                select(`System ID` = system_id, `Previously Monitored?` = monitored, `System Status` = status, `Data Phase` = sys_dataphase, `CIPIT Status` = cipit_status, `Lifecycle Status`= lifecycle_status, `Gauge Id` = gage_uid),
              theme = darkly(),
              defaultPageSize = 1,
              fullWidth = TRUE,
              selection = "single",
              onClick = "select",
              selectionId = "sys_stat_selected",
              searchable = FALSE,
              columns = list(
                `Previously Monitored?` = colDef(width = 200)),
              details = function(index) {
                sys_stat_nested_notes <- rv$wic_system_status()[rv$wic_system_status()$system_id == rv$sys_stat()$system_id[index], ] %>%
                  select(`MARS Comments on the System:` = notes)
                htmltools::div(style = "padding: 1rem",
                               reactable(sys_stat_nested_notes, 
                                         list(`MARS Comments on the System:`= colDef(html = TRUE, style = list(color = "#F5DEB3", fontweight = "bold"))),
                                         theme = darkly(),
                                         outlined = TRUE)
                )
              }
              
    ))
  
  # wo_stat table filtered
  rv$wo_stat <- reactive({
    wo_tbl <- wic_sys %>%
                filter(system_id == input$system_id_edit) %>%
                left_join(rv$wic_wo_status(), by = "workorder_id") %>%
                mutate(immediate_event = as.Date(NA)) %>%
                mutate(days_from_rain = as.numeric(NA)) %>%
                select(workorder_id, wic_address, date, phase, property_dist_ft, footprint_dist_ft, status, immediate_event, days_from_rain) %>%
                distinct()
    # determine the immediate rain event
    if(nrow(wo_tbl) > 0){
      for (i in 1:nrow(wo_tbl)) {
        wo_tbl[i,"immediate_event"] <- max(rv$sys_rains()$event_startdate[rv$sys_rains()$event_startdate < wo_tbl$date[i]], na.rm = TRUE)
      }
      wo_tbl <- wo_tbl %>%
        mutate(days_from_rain = data.table::fifelse((date - immediate_event) == Inf | (date - immediate_event) == -Inf, NA, date - immediate_event)) %>% # If there is no gauge id assigned to a system, return NA for date of rain
        mutate(immediate_event = data.table::fifelse(immediate_event == Inf | immediate_event == -Inf, NA, immediate_event))
    }
    return(wo_tbl)
    })

  output$wo_stat_table <- renderReactable(
    reactable(rv$wo_stat() %>%
                select(`WO ID` = workorder_id, `Address` = wic_address, `WIC Date` = date, Phase = phase, `Dist.Prop (ft)` = property_dist_ft, `Dist.Ftp (ft)` = footprint_dist_ft, `Rain Date` = immediate_event, `Days from`= days_from_rain, `WIC Status` = status),
              theme = darkly(),
              defaultPageSize = 15,
              fullWidth = TRUE,
              selection = "single",
              onClick = "select",
              selectionId = "wo_stat_selected",
              searchable = FALSE,
              columns = list(
                `WO ID` = colDef(width = 70),
                Address = colDef(width = 190),
                `WIC Date` = colDef(width = 100),
                `Dist.Prop (ft)` = colDef(width = 100, style = list(textAlign = "center")),
                `Dist.Ftp (ft)` = colDef(width = 100, style = list(textAlign = "center")),
                `Days from` = colDef(style = list(textAlign = "center")),
                Phase = colDef(width = 130)),
              details = function(index) {
                cw_wic_nested_notes <- wic_comments[wic_comments$workorder_id == rv$wo_stat()$workorder_id[index], ] 
                # Highlight "rain" in red, considering case and word boundaries
                cw_wic_nested_notes$comment <- gsub("\\b(rain|rainfall|rainy|raining|rains|rained|storm|storms|storming|gsi|smp)\\b", "<span style='background-color: red; color: white;'>\\1</span>", cw_wic_nested_notes$comment, ignore.case = TRUE)
                cw_wic_nested_notes <- cw_wic_nested_notes %>%
                  arrange(comment_id) %>%
                  select(`Cityworks Comments on the Workorder:` = comment)
                htmltools::div(style = "padding: 1rem",
                               reactable(cw_wic_nested_notes,
                                         list(`Cityworks Comments on the Workorder:` = colDef(html = TRUE, style = list(color = "#F5DEB3", fontweight = "bold"))),
                                         theme = darkly(),
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
      
      rv$update_sys_status_q <- reactive(paste("Update fieldwork.tbl_wic_system_status SET notes = '",  rv$input_note(), "', wic_system_status_lookup_uid = ", ifelse(length(rv$sys_stat_uid()) == 0, 4, rv$sys_stat_uid())," where system_id = '", input$system_id_edit, "'", sep = ""))
      dbGetQuery(mars_con, rv$update_sys_status_q())
      
      # refresh data and reset tables
      rv$wic_system_status <- reactive(dbGetQuery(mars_con, "SELECT * FROM fieldwork.tbl_wic_system_status
                                              INNER JOIN fieldwork.tbl_wic_system_status_lookup
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
      
      rv$update_wo_status_q <- reactive(paste("Update fieldwork.tbl_wic_wo_status SET wic_wo_status_lookup_uid = ", ifelse(length(rv$wo_stat_uid()) == 0, 2, rv$wo_stat_uid())," where workorder_id = ", input$workorder_edit, sep = ""))
      dbGetQuery(mars_con, rv$update_wo_status_q())
      
      # refresh data and reset tables
      rv$wic_wo_status <- reactive(dbGetQuery(mars_con, "SELECT * FROM fieldwork.tbl_wic_wo_status 
                                              INNER JOIN fieldwork.tbl_wic_wo_status_lookup
                                              USING(wic_wo_status_lookup_uid)"))
      reset("sys_stat_table")
      reset("wo_stat_table")
      reset("wic_table")
      reset("workorder_edit")
      reset("check_woid")
    }
})
  
  ## 2.6.4 Clear button Tab 2 ----
  observeEvent(input$clear, {
    showModal(modalDialog(title = "Clear All Fields", 
                          "Are you sure you want to clear all fields on this tab?", 
                          modalButton("No"), 
                          actionButton("confirm_clear_pcs", "Yes")))
  })
  
  observeEvent(input$confirm_clear_pcs, {
    
    reset("sys_stat_table")
    reset("wo_stat_table")
    reset("wic_table")
    reset("workorder_edit")
    reset("check_woid")
    reset("edit_status")
    reset("system_note")
    reset("system_id_edit")
    reset("map")
    updateReactable("wic_table", selected = NA)
    removeModal()
  })
  
  ## 2.6.5 Re-draw Map ----
  observeEvent(input$redraw, {
    # re-creating basemap here 
    output$map <- renderLeaflet({
      # creating basemap here 
      map <- leaflet() %>%
        addProviderTiles(providers$OpenStreetMap, group = 'OpenStreetMap', options = providerTileOptions(maxZoom = 19)) %>%
        addProviderTiles(providers$Esri.WorldTopoMap, group = 'Esri.WorldTopoMap', options = providerTileOptions(maxZoom = 19)) %>%
        addProviderTiles(providers$Esri.WorldImagery, group='Esri.WorldImagery', options = providerTileOptions(maxZoom = 19)) %>%
        addPolygons(data = wic_sys_geom_buffered, color = "#00ffbf", label = paste("System ID: ", wic_sys_geom_buffered$system_id), labelOptions = labelOptions(style = list("font-weight" = "bold", "font-size" = "14px")), group = "Buffer (100ft)") %>%
        addPolygons(data = wic_smp_geom, color = "#000000", label = paste("System ID: ", wic_smp_geom$system_id), labelOptions = labelOptions(style = list("font-weight" = "bold", "font-size" = "14px")), group = "System", layerId = ~system_id) %>%
        addPolygons(data = wic_property_geom, color = "red", label = paste("Address: ", wic_property_geom$address), labelOptions = labelOptions(style = list("font-weight" = "bold", "font-size" = "14px")), group = "Property Line") %>%
        addPolygons(data = wic_footprint_geom, color = "purple", label = paste("Address", wic_footprint_geom$address), labelOptions = labelOptions(style = list("font-weight" = "bold", "font-size" = "14px")), group = "Footprint") %>%
        addLayersControl(overlayGroups = c("System","Buffer (100ft)","Property Line", "Footprint"), baseGroups = c("OpenStreetMap", "Esri.WorldTopoMap", "Esri.WorldImagery")) %>%
        hideGroup(c("Footprint", "Buffer (100ft)")) %>%
        addLegend(colors = c("#000000","#00ffbf","red","purple"), 
                  labels = c("System","Buffer Area (100ft)" , "WIC Property Line","WIC Building Footprint")) %>%
        addDrawToolbar(polylineOptions = drawPolylineOptions(metric = FALSE, feet = TRUE),
                       polygonOptions = FALSE,
                       circleOptions=FALSE,
                       rectangleOptions=FALSE,
                       markerOptions=FALSE,
                       circleMarkerOptions= FALSE,
                       editOptions=editToolbarOptions(selectedPathOptions=selectedPathOptions())) %>%
        setView(lng = -75.1652 , lat = 39.9526  , zoom = 11) # zoom in philly
      
    })
    
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
}
# Run the application 
shinyApp(ui = ui, server = server)
