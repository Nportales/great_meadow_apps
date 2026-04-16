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

# vmmi
vmmi_data <- read.csv("data/vis_FOA_NETN_VMMI_2011_2025_20260324.csv")

# species lists
species_data <- read.csv("data/vis_FOA_NETN_spplist_2011_2025_20260324.csv") %>%
  filter(!str_detect(latin.name, regex("unknown", ignore_case = TRUE)))

# monitoring sites
monitoring_sites <- read.csv("data/monitoring_sites.csv")

# Create lookup: labels = display names, values = site codes
site_lookup <- monitoring_sites %>%
  filter(site.name %in% vmmi_data$site.name) %>%
  distinct(site.name, display.site.name) %>%
  mutate(
    wetland_priority = case_when(
      grepl("Great Meadow", display.site.name) ~ 1,
      grepl("Gilmore Meadow", display.site.name) ~ 2,
      TRUE ~ 3
    ),
    site_num = readr::parse_number(display.site.name)
  ) %>%
  arrange(wetland_priority, site_num) %>%
  { setNames(.$site.name, .$display.site.name) }

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
    label = div(icon(if(id %in% c("vmmi_site", "sp_site")) "map-marker" else "calendar"), label),
    choices = choices,
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
        margin: 20px 0; padding: 25px; border-radius: 15px;
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
  
  #Main title
  div(class = "main-title",
      h1("Wetland Vegetation Dashboard",
         style = "margin: 0; font-size: 2rem; text-shadow: 2px 2px 4px rgba(0,0,0,0.3)")
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
                              choices = site_lookup,
                              selected = site_lookup[1]),
          
          create_picker_input("vmmi_year", "Select Year(s):",
                              choices = sort(unique(vmmi_data$year)),
                              selected = NULL),
          tags$small(
            style = "color: #6c757d; display: block; margin-top: -8px; margin-bottom: 10px; font-style: italic;",
            "*Note: Year options update based on selected site(s)."
          ),
          
          radioButtons("vmmi_summary", "Summarize By:",
                       choices = c("Each Year" = "year",
                                   "Average Across Years" = "multi"),
                       selected = "year"),
          
          br(),
          
          downloadButton("download_vmmi", "Download VMMI Table",
                         class = "btn-primary btn-sm"),
          
          div(style = "margin-top: 15px; text-align: center;",
              tags$a(href = "#about",
                     class = "btn btn-primary btn-sm", icon("info-circle"),
                     "About")),
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
                              choices = site_lookup,
                              selected = site_lookup[1]),
          
          create_picker_input("sp_year", "Select Year(s):",
                              choices = sort(unique(species_data$year)),
                              selected = NULL),
          
          tags$small(
            style = "color: #6c757d; display: block; margin-top: -8px; margin-bottom: 10px; font-style: italic;",
            "*Note: Year options update based on selected site(s)."
          ),
          
          checkboxInput("sp_invasive", "Show invasives only", FALSE),
          
          textInput("species_search", "Search species (name):", ""),
          
          br(),
          
          downloadButton("download_species", "Download Species Table",
                         class = "btn-primary btn-sm"),
          
          div(style = "margin-top: 15px; text-align: center;",
              tags$a(href = "#about",
                     class = "btn btn-primary btn-sm", icon("info-circle"),
                     "About")),
        ),
        
        card(
          full_screen = TRUE,
          card_header(class = "bg-primary text-white",
                      "Species Lists"),
          div(style = "padding: 10px;",
              dataTableOutput("species_table"))
        )
      )
  ),
  # About section
  div(id = "about",
      class = "brush-info-section",
      card(
        card_header(class = "bg-primary text-white", "About"),
        includeHTML("./www/About.html")
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
  
  # first make year choices reactive to selected site(s)
  observeEvent(input$vmmi_site, {
    
    available_years <- vmmi_data %>%
      filter(site.name %in% input$vmmi_site) %>%
      pull(year) %>%
      unique() %>%
      sort()
    
    updatePickerInput(
      session,
      "vmmi_year",
      choices = available_years,
      selected = available_years
    )
  }, ignoreNULL = FALSE)
  
  # processing
  vmmi_filtered <- reactive({
    req(input$vmmi_site, input$vmmi_year)
    
    vmmi_data %>%
      filter(site.name %in% input$vmmi_site,
             year %in% input$vmmi_year)
  })
  
  vmmi_summary <- reactive({
    
    df <- vmmi_filtered() %>%
      left_join(monitoring_sites, by = "site.name")
    
    switch(input$vmmi_summary,
           
           "year" = {
             df %>%
               mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
               select(
                 Site = display.site.name,
                 Year = year,
                 `Mean COC` = mean.coc,
                 `Invasive Cover` = inv.cov,
                 `Bryophyte Cover` = bryo.cov,
                 `Stress Tolerance Cover` = strtol.cov,
                 VMMI = vmmi,
                 `VMMI Rating` = vmmi.rating
               ) %>%
               mutate(
                 Site = factor(Site, levels = names(site_lookup))
               ) %>%
               arrange(Site, Year)
           },
           
           "multi" = {
             df %>%
               group_by(site.name) %>%
               summarise(
                 Site = first(display.site.name),
                 Year = paste0(min(year), "–", max(year)),
                 across(c(mean.coc, inv.cov, bryo.cov, strtol.cov, vmmi),
                        ~ round(mean(.x, na.rm = TRUE), 2)),
                 
                 # store mean VMMI separately (for rating) in temp column
                 vmmi_mean = mean(vmmi, na.rm = TRUE),
                 
                 # assign rating based on averaged VMMI
                 vmmi.rating = case_when(
                   vmmi_mean > 60.94853 ~ "Good",
                   vmmi_mean < 41.48136 ~ "Poor",
                   TRUE ~ "Fair"
                 ),
                 .groups = "drop"
               ) %>%
               # now remove temp column
               select(-site.name, -vmmi_mean) %>%
               
               rename(
                 `Mean COC` = mean.coc,
                 `Invasive Cover` = inv.cov,
                 `Bryophyte Cover` = bryo.cov,
                 `Stress Tolerance Cover` = strtol.cov,
                 VMMI = vmmi,
                 `VMMI Rating` = vmmi.rating
               ) %>%
               mutate(
                 Site = factor(Site, levels = names(site_lookup))
               ) %>%
               arrange(Site)
           }
    )
  })
  
  #-----------------------------#
  ####   Species Processing ####
  #-----------------------------#
  
  # first make species year choices reactive to selected site(s)
  observeEvent(input$sp_site, {
    
    available_years <- species_data %>%
      filter(site.name %in% input$sp_site) %>%
      pull(year) %>%
      unique() %>%
      sort()
    
    updatePickerInput(
      session,
      "sp_year",
      choices = available_years,
      selected = available_years
    )
  }, ignoreNULL = FALSE)
  
  # processing 
  species_filtered <- reactive({
    df <- species_data %>%
      filter(site.name %in% input$sp_site,
             year %in% input$sp_year)
    
    if (input$sp_invasive) {
      df <- df %>% filter(invasive == "true")
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
      left_join(monitoring_sites, by = "site.name") %>%
      group_by(latin.name, common.name, invasive) %>%
      summarise(
        `Latin Name` = first(latin.name),
        `Common Name` = first(common.name),
        Invasive = first(invasive),
        `Years Found` = paste(sort(unique(year)), collapse = ", "),
        `Site(s)` = paste(unique(display.site.name), collapse = ", "),
        .groups = "drop"
      ) %>%
      select(`Latin Name`, `Common Name`, Invasive, `Years Found`, `Site(s)`)
  })
  
  #-----------------------------#
  ####     Render Tables     ####
  #-----------------------------#
  
  output$vmmi_table <- renderDataTable({
    datatable(vmmi_summary(),
              rownames = FALSE, options = list(pageLength = 25, scrollX = TRUE, dom = "tip"))
  })
  
  output$species_table <- renderDataTable({
    datatable(
      species_summary(),
      rownames = FALSE, options = list(pageLength = 25, scrollX = TRUE, dom = "tip"))
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

