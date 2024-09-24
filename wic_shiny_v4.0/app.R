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
# DB connections
mars_con <- dbConnect(RPostgres::Postgres(),
                      host = "PWDMARSDBS1.pwd.phila.local",
                      port = 5434,
                      dbname = "mars_data",
                      user = Sys.getenv("shiny_uid"),
                      password = Sys.getenv("shiny_pwd"))
## 0.3 Loading required tables ----
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
  st_transform(crs = 4326)
wic_footprint_geom <- st_read(dsn = mars_con, Id(schema="fieldwork", table = "beta_tbl_wic_footprint_geom")) %>%
  st_transform(crs = 4326)

# Grouping smp polygons by system- setting crs global 4326 for leaflet mapping 
wic_smp_geom <- st_read(dsn = mars_con, Id(schema="fieldwork", table = "beta_tbl_wic_smp_geom")) %>%
  st_transform(crs = 4326)
wic_smp_geom['system_id'] <- gsub('-\\d+$','', wic_smp_geom$smp_id)
wic_sys_geom <- wic_smp_geom %>%
  group_by(system_id) %>%
  summarize(sys_geom = st_combine(smp_geom)) %>%
  st_as_sf()

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
ui <- tagList(useShinyjs(), navbarPage("WIC App v4.0", id = "TabPanelID", theme = shinytheme("flatly"),
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
                                                                value = 100),
                                                    checkboxInput("single_wic", "Only Show Most Recent WIC per System ID", value = TRUE, width = NULL),
                                                    fluidRow(column(12, strong("Download all WICs"))),
                                                    downloadButton("download_table", "Download"), width = 3
                                                  ),
                                                  
                                                  mainPanel(
                                                    strong(span(textOutput("table_name"), style = "font-size:22px")),
                                                    reactableOutput("wic_table"), width = 9
                                                    
                                                  )
                                                )
                                       ),
                                       ## 1.2 Tab "WIC Investigation" ----
                                       tabPanel("WIC Investigation", value = "wic_insight", 
                                                titlePanel("WICs around the System"), 
                                                sidebarLayout(
                                                  
                                                  sidebarPanel(
                                             
                                                    
                                                  ),
                                                  mainPanel(
                                                    
                                                    
                                                  )
                                                )),
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
  
  # row selections 
  rv$row_wic_table <- reactive(getReactableState("wic_table", "selected"))
  
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
              theme = reactableTheme(
                rowSelectedStyle = list(backgroundColor = "lightblue")
              ),
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
                                         columns = list(
                                           `MARS Comments on the System:` = colDef(width = 1000)
                                         ), 
                                         outlined = TRUE)
                )
              }
              ))
  ## 2.4 Switch tab to "WIC Investigation"----
  observeEvent(rv$row_wic_table(), {
    if (!is.null(rv$row_wic_table())) {
      updateTabsetPanel(session, "TabPanelID", selected = "wic_insight")
      
    }
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
