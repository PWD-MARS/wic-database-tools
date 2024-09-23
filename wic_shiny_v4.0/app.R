### Water-in-Cellar Shiny App V 4.0 
### Notes: Major overhaul of the maint. script + addition of POSTGIS to the PostgreSQL DB
### Author: Farshad Ebrahimi 
### Last Modified: 09/20/2024

# 0.0 Set Up 
# 0.1 Load libraries ----
library(shiny)
library(shinyjs)
library(shinydashboard)
library(shinyWidgets)
library(DBI)
library(odbc)
library(tidyverse)
library(reactable)
library(shinythemes)
library(RPostgreSQL)
library(sf)
library(leaflet)
library(leaflet.extras)
library(xlsx)

#create negate of %in%
`%!in%` = Negate(`%in%`)

# 0.2 DB connections and loading required tables ----
# DB connections
mars_con <- dbConnect(RPostgres::Postgres(),
                      host = "PWDMARSDBS1.pwd.phila.local",
                      port = 5434,
                      dbname = "mars_data",
                      user = Sys.getenv("shiny_uid"),
                      password = Sys.getenv("shiny_pwd"))

# Get wic tables
# WICS 
wic_sys <- dbGetQuery(mars_con, "SELECT *, data.fun_date_to_fiscal_quarter(date) as quarter FROM fieldwork.viw_wics_100ft")
# WICs Comments
wic_comments <- dbGetQuery(mars_con, "SELECT * FROM fieldwork.beta_tbl_wic_comment")
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

# System ids
system_id_all <- odbc::dbGetQuery(mars_con, "select distinct system_id from external.mat_assets where system_id like '%-%'") %>% 
  dplyr::arrange(system_id) %>%  
  dplyr::pull()

# gauge data
gauge_event <- dbGetQuery(mars_con, "SELECT distinct tbl_gage_event.gage_uid, tbl_gage_event.eventdatastart_edt::date AS event_startdate FROM data.tbl_gage_event where tbl_gage_event.eventdataend_edt > '2010-01-01'")
gauge_sys <- dbGetQuery(mars_con, "select distinct admin.fun_smp_to_system(smp_id) as system_id, gage_uid from admin.tbl_smp_gage where smp_id like '%-%-%'")

# Find the immediate event_startdate smaller than complaint_date
#immediate_event <- event_startdate[event_startdate < complaint_date] %>%
 # max(na.rm = TRUE)

# Deployment History
# Get deployment table
deployments_list <- dbGetQuery(mars_con, "SELECT distinct admin.fun_smp_to_system(smp_id) as system_id FROM fieldwork.viw_deployment_full_cwl") 

# Fiscal quarter lookup
fq_lookup <- dbGetQuery(mars_con,"select * from admin.tbl_fiscal_quarter_lookup")
q_list <- fq_lookup %>%
  select(fiscal_quarter) %>%
  pull

# Post-con status look up
status_lookup <- dbGetQuery(mars_con, "select * from fieldwork.beta_tbl_wic_status_lookup")
# Status list
status_choice <- status_lookup %>%
  select(status) %>%
  distinct() %>%
  pull

# 1.0 Define UI ----
# Define UI
ui <- tagList(useShinyjs(), navbarPage("WIC App v4.0", id = "TabPanelID", theme = shinytheme("flatly"),
                                       tabPanel("Post-Construction Status Table", value = "status", 
                                                titlePanel("Systems Near WICs"),
                                                sidebarLayout(
                                                  
                                                  sidebarPanel(
                                                    selectInput("date_range", "Date Range", choices = c("To-Date", "Fiscal Quarter")),
                                                    conditionalPanel(condition = "input.date_range == 'Fiscal Quarter'", 
                                                                     fluidRow(column(12,
                                                                                     selectInput("f_q", "Fiscal Quarter", choices = q_list, selected = "FY24Q2")))
                                                    ), 
                                                    selectInput("status", "System Status", choices = c("", status_choice) , selected = ""),
                                                    
                                                    # DL Button 
                                                    
                                                    fluidRow(column(12, strong("Download all WICs"))),
                                                    
                                                    downloadButton("download_table", "Download")
                                                  ),
                                                  
                                                  mainPanel(
                                                    strong(span(textOutput("table_name"), style = "font-size:22px")),
                                                    reactableOutput("wic_table")
                                                    
                                                  )
                                                )
                                       ),
                                       tabPanel("WIC Investigation", value = "add_edit_insight", 
                                                titlePanel("Add/Edit WIC Findings"), 
                                                sidebarLayout(
                                                  
                                                  sidebarPanel(
                                             
                                                    
                                                  ),
                                                  mainPanel(
                                                    
                                                  )
                                                )),
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
  
  #initialzie reactive values
  rv <- reactiveValues()
  
  output$wic_table <- renderReactable(
    reactable(wic_sys, 
              fullWidth = TRUE,
              selection = "single",
              searchable = TRUE,
              onClick = "select",
              showPageSizeOptions = TRUE,
              pageSizeOptions = c(25, 50, 100),
              defaultPageSize = 25,
              height = 1000))
  

}

# Run the application 
shinyApp(ui = ui, server = server)
