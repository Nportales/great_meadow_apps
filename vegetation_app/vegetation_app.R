#### Wetland Vegetation Dashboard ####

#---------------------------------------------#
####        Load Required Packages         ####
#---------------------------------------------#

library(shiny)
library(tidyverse)
library(shinyWidgets)
library(DT)
library(bslib)

#-------------------------------------------#
####        Read & Prepare Data          ####
#-------------------------------------------#

# For data exploration, app building, or debugging, temporarily set working directory to app folder
# setwd("vegetation_app")

vmmi_data <- read.csv("data/vis_FOA_NETN_VMMI_2011_2025_20260324.csv")
species_data <- read.csv("data/vis_FOA_NETN_spplist_2011_2025_20260324.csv")


#-----------------------#
####    Constants    ####
#-----------------------#

PICKER_OPTIONS <- list(
  `actions-box` = TRUE,
  `deselect-all-text` = "Clear all",
  `select-all-text` = "Select all",
  `live-search` = TRUE,
  style = "btn-outline-primary"
)

#-----------------------#
####    Functions    ####
#-----------------------#

create_picker_input <- function(id, label, choices, selected,
                                multiple = TRUE, none_text = "Choose options") {
  pickerInput(
    id,
    label = div(icon("leaf"), label),
    choices = sort(unique(choices)),
    selected = selected,
    multiple = multiple,
    options = c(PICKER_OPTIONS, list(`none-selected-text` = none_text))
  )
}

#----------------#
####    UI    ####
#----------------#

ui <- page_fluid(
  theme = bs_theme(
    version = 5, bootswatch = "flatly",
    primary = "#2E7D32", secondary = "#66BB6A",
    base_font = font_google("Open Sans"),
    heading_font = font_google("Open Sans", wght = c(400, 700))
  ),
  
  tags$head(
    tags$style(HTML("
      .content-section {
        margin: 30px 0; padding: 25px; border-radius: 15px;
        box-shadow: 0 4px 12px rgba(0,0,0,0.1);
        background: linear-gradient(135deg, #ffffff 0%, #f8f9fa 100%);
        border: 2px solid #1B365D;
      }
      .sidebar-custom {
        background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%);
        border-radius: 10px; padding: 20px;
      }
      .main-title {
        background: linear-gradient(135deg, #2E7D32 0%, #66BB6A 100%);
        color: white; padding: 30px;
        text-align: center;
        border-radius: 0 0 20px 20px;
      }
      .dataTables_wrapper { font-size: 0.85rem !important; }
    "))
  ),
  
  div(class = "main-title",
      h1("Wetland Vegetation Dashboard")
  ),
  
  #--------------------------------#
  ####   VMMI + Species Section ####
  #--------------------------------#
  
  div(class = "content-section",
      layout_sidebar(
        
        sidebar = sidebar(
          class = "sidebar-custom", width = 300,
          
          h4("VMMI Controls", style = "color: #2E7D32;"),
          
          create_picker_input("vmmi_site", "Select Site(s):",
                              vmmi_data$site.name,
                              selected = unique(vmmi_data$site.name)[1]),
          
          create_picker_input("vmmi_year", "Select Years:",
                              vmmi_data$year,
                              selected = tail(sort(unique(vmmi_data$year)), 4)),
          
          radioButtons("vmmi_summary", "Summarize By:",
                       choices = c("Each Year" = "year",
                                   "Average Across Years" = "multi"),
                       selected = "year"),
          
          br(),
          
          downloadButton("download_vmmi", "Download VMMI Table",
                         class = "btn-primary btn-sm")
        ),
        
        card(
          full_screen = TRUE,
          card_header(class = "bg-primary text-white",
                      "Vegetation Multimetric Index (VMMI)"),
          div(style = "padding: 10px;",
              dataTableOutput("vmmi_table"))
        )
      )
  ),
  
  div(class = "content-section",
      layout_sidebar(
        
        sidebar = sidebar(
          class = "sidebar-custom", width = 300,
          
          h4("Species Controls", style = "color: #2E7D32;"),
          
          create_picker_input("sp_site", "Select Site(s):",
                              species_data$site.name,
                              selected = unique(species_data$site.name)[1]),
          
          create_picker_input("sp_year", "Select Years:",
                              species_data$year,
                              selected = unique(species_data$year)),
          
          checkboxInput("sp_invasive", "Show invasive only", FALSE),
          
          textInput("species_search", "Search species (name):", ""),
          
          br(),
          
          downloadButton("download_species", "Download Species Table",
                         class = "btn-primary btn-sm")
        ),
        
        card(
          full_screen = TRUE,
          card_header(class = "bg-primary text-white",
                      "Species List Explorer"),
          div(style = "padding: 10px;",
              dataTableOutput("species_table"))
        )
      )
  )
)

#--------------------#
####    SERVER    ####
#--------------------#

server <- function(input, output, session) {
  
  #-----------------------------#
  ####    VMMI Processing   ####
  #-----------------------------#
  
  vmmi_filtered <- reactive({
    req(input$site, input$year)
    
    vmmi_data %>%
      filter(site.name %in% input$site,
             year %in% input$year)
  })
  
  vmmi_summary <- reactive({
    
    df <- vmmi_filtered()
    
    switch(input$summary_type,
           
           "year" = {
             df %>%
               mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
               select(
                 Site = site.name,
                 Year = year,
                 `Mean COC` = mean.coc,
                 `Invasive Cover` = inv.cov,
                 `Bryophyte Cover` = bryo.cov,
                 `Stress Tolerance Cover` = strtol.cov,
                 VMMI = vmmi,
                 `VMMI Rating` = vmmi.rating
               )
           },
           
           "multi" = {
             df %>%
               group_by(site.name) %>%
               summarise(
                 Year = paste0(min(year), "–", max(year)),
                 across(c(mean.coc, inv.cov, bryo.cov, strtol.cov, vmmi),
                        ~ round(mean(.x, na.rm = TRUE), 2)),
                 vmmi.rating = names(sort(table(vmmi.rating), decreasing = TRUE))[1],
                 .groups = "drop"
               ) %>%
               rename(
                 Site = site.name,
                 `Mean COC` = mean.coc,
                 `Invasive Cover` = inv.cov,
                 `Bryophyte Cover` = bryo.cov,
                 `Stress Tolerance Cover` = strtol.cov,
                 VMMI = vmmi,
                 `VMMI Rating` = vmmi.rating
               )
           }
    )
  })
  
  #-----------------------------#
  ####   Species Processing ####
  #-----------------------------#
  
  species_filtered <- reactive({
    df <- species_data %>%
      filter(site.name %in% input$sp_site,
             year %in% input$sp_year)
    
    if (input$sp_invasive) {
      df <- df %>% filter(invasive == "Yes")
    }
    
    if (input$species_search != "") {
      df <- df %>%
        filter(
          str_detect(latin.name, regex(input$species_search, ignore_case = TRUE)) |
            str_detect(common.name, regex(input$species_search, ignore_case = TRUE))
        )
    }
    
    df
  })
  
  species_summary <- reactive({
    species_filtered() %>%
      group_by(latin.name, common.name, invasive) %>%
      summarise(
        `Years Found` = paste(sort(unique(year)), collapse = ", "),
        .groups = "drop"
      )
  })
  
  #-----------------------------#
  ####     Render Tables     ####
  #-----------------------------#
  
  output$vmmi_table <- renderDataTable({
    datatable(vmmi_summary(),
              options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$species_table <- renderDataTable({
    datatable(species_summary(),
              filter = "top",
              options = list(pageLength = 25, scrollX = TRUE))
  })
  
  #-----------------------------#
  ####      Download        ####
  #-----------------------------#
  
  output$download_vmmi <- downloadHandler(
    filename = function() paste0("vmmi_", Sys.Date(), ".csv"),
    content = function(file) write.csv(vmmi_summary(), file, row.names = FALSE)
  )
  
  output$download_species <- downloadHandler(
    filename = function() paste0("species_", Sys.Date(), ".csv"),
    content = function(file) write.csv(species_summary(), file, row.names = FALSE)
  )
}

# Run app
shinyApp(ui, server)

