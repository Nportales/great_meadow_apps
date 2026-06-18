#### Wetland Vegetation Dashboard ####

#---------------------------------------------#
####        Load Required Packages         ####
#---------------------------------------------#

library(shiny)
library(tidyverse)
library(shinyWidgets)
library(DT)
library(bslib)
library(patchwork)

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

# Time series plotting function
plot_veg_metric <- function(data, grand_data, metric, y_label, title, show_labels = FALSE) {
  
  # Sort wetlands_in_data to ensure Great Meadow is first
  wetlands_in_data <- unique(as.character(data$wetland))
  wetlands_in_data <- sort(wetlands_in_data, decreasing = TRUE)  # Great before Gilmore
  
  # Enforce consistent ordering - Great Meadow first
  data$wetland <- factor(data$wetland,
                         levels = c("Great Meadow", "Gilmore Meadow"))
  
  # Filter grand_data to only include wetlands present in data
  grand_data <- grand_data %>%
    filter(wetland %in% wetlands_in_data) %>%
    mutate(wetland = factor(wetland, levels = c("Great Meadow", "Gilmore Meadow")))
  
  avg_col   <- paste0(metric, "_avg")
  se_col    <- paste0(metric, "_se")
  grand_col <- paste0(metric, "_avg_grand")
  
  # Create grand mean labels with values - only for wetlands present
  gm_grand <- if ("Great Meadow" %in% wetlands_in_data) {
    grand_data %>% filter(wetland == "Great Meadow") %>% pull(!!sym(grand_col))
  } else {
    NULL
  }
  
  gl_grand <- if ("Gilmore Meadow" %in% wetlands_in_data) {
    grand_data %>% filter(wetland == "Gilmore Meadow") %>% pull(!!sym(grand_col))
  } else {
    NULL
  }
  
  # Create a combined factor for the legend - only for present wetlands
  # Ensure Great Meadow comes before Gilmore Meadow
  grand_levels <- paste0(wetlands_in_data, " Grand Mean")
  grand_data <- grand_data %>%
    mutate(grand_label = factor(
      paste0(wetland, " Grand Mean"),
      levels = grand_levels
    ))
  
  # Build label vector dynamically - Great Meadow first
  grand_labels <- c()
  grand_label_names <- c()
  if (!is.null(gm_grand)) {
    grand_labels <- c(grand_labels, sprintf("Great Meadow (%.2f)", gm_grand))
    grand_label_names <- c(grand_label_names, "Great Meadow Grand Mean")
  }
  if (!is.null(gl_grand)) {
    grand_labels <- c(grand_labels, sprintf("Gilmore Meadow (%.2f)", gl_grand))
    grand_label_names <- c(grand_label_names, "Gilmore Meadow Grand Mean")
  }
  names(grand_labels) <- grand_label_names
  
  # Create color values for grand means
  grand_colors <- setNames(
    c("black", "grey67")[match(wetlands_in_data, c("Great Meadow", "Gilmore Meadow"))],
    grand_levels
  )
  
  p <- ggplot(data, aes(x = year, y = .data[[avg_col]],
                        color = wetland, shape = wetland, group = wetland)) +
    
    geom_line(linewidth = 1.2) +
    
    geom_point(
      size = 6,
      position = position_jitter(width = 0.05, height = 0)
    ) +
    
    geom_errorbar(
      aes(ymin = .data[[avg_col]] - .data[[se_col]],
          ymax = .data[[avg_col]] + .data[[se_col]],
          alpha = wetland),
      width = 0
    ) +
    
    # ADD SITE COUNT LABELS if requested
    {if (show_labels && "n_sites" %in% names(data)) 
      geom_text(
        aes(label = n_sites,
            y = .data[[avg_col]] + pmax(.data[[se_col]], 0, na.rm = TRUE),
            # Adjust vjust based on SE value ranges
            vjust = case_when(
              is.na(.data[[se_col]]) | .data[[se_col]] == 0 ~ -1.5, # no SE
              .data[[se_col]] < 1 ~ -1.2, # small SE
              TRUE ~ -0.5 # large SE
            )),
        size = 3.8,
        fontface = "bold",
        color = "grey25",
        show.legend = FALSE
      )
    } +
    
    # Grand mean lines - use wetland for color mapping
    geom_hline(
      data = grand_data,
      aes(yintercept = .data[[grand_col]], 
          linetype = grand_label,
          color = wetland),
      linewidth = 1,
      show.legend = c(linetype = TRUE, color = FALSE),  # Only show linetype in legend
      key_glyph = "path"
    ) +
    
    scale_x_continuous(
      breaks = seq(min(data$year), max(data$year), by = 1)
    ) +
    
    scale_y_continuous(n.breaks = 8) +
    
    scale_color_manual(
      name = "Wetland Annual Mean",
      values = c(
        "Great Meadow" = "black",
        "Gilmore Meadow" = "grey67"
      ),
      breaks = c("Great Meadow", "Gilmore Meadow"),  
      drop = TRUE
    ) +
    
    scale_shape_manual(
      name = "Wetland Annual Mean",
      values = c(
        "Great Meadow" = 16,
        "Gilmore Meadow" = 17
      ),
      breaks = c("Great Meadow", "Gilmore Meadow"), 
      drop = TRUE
    ) +
    
    scale_linetype_manual(
      name = "Wetland Grand Mean",
      values = setNames(
        rep("dashed", length(grand_levels)),
        grand_levels
      ),
      labels = grand_labels,
      breaks = grand_levels,
      drop = TRUE
    ) +
    
    scale_alpha_manual(
      values = c(
        "Great Meadow" = 0.85,
        "Gilmore Meadow" = 1
      ),
      guide = "none"
    ) +
    
    labs(
      title = title,
      x = "Year",
      y = y_label
    ) +
    
    theme_minimal() +
    
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
      axis.text.y = element_text(size = 12),
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14),
      legend.position = "right",
      legend.justification = "left",
      legend.box.margin = margin(0, 0, 0, 5),
      legend.margin = margin(0, 0, 0, 0),
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 11),
      legend.key.size = unit(1.2, "cm"),
      legend.spacing.y = unit(0.2, "cm"),
      legend.box = "vertical",
      plot.margin = margin(10, 5, 10, 10),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(color = "black", linewidth = 0.5),
      axis.ticks = element_line(color = "black", linewidth = 0.5),
      axis.ticks.length = unit(0.2, "cm")
    ) +
    
    guides(
      color = guide_legend(order = 1, override.aes = list(linewidth = 1.2)),
      shape = guide_legend(order = 1),
      linetype = guide_legend(
        order = 2,
        override.aes = list(
          color = grand_colors,  # Use dynamically created color vector
          linewidth = 1
        )
      )
    )
  
  # ADD SHADING ONLY FOR VMMI
  if (metric == "vmmi") {
    p <- p +
      annotate("rect",
               xmin = -Inf, xmax = Inf,
               ymin = -Inf, ymax = 41.48136,
               fill = "red", alpha = 0.08) +
      
      annotate("rect",
               xmin = -Inf, xmax = Inf,
               ymin = 41.48136, ymax = 60.94853,
               fill = "goldenrod", alpha = 0.04) +
      
      annotate("rect",
               xmin = -Inf, xmax = Inf,
               ymin = 60.94853, ymax = Inf,
               fill = "green3", alpha = 0.08) +
      
      annotate("text",
               x = min(data$year),
               y = 63,
               label = "Good",
               color = "darkgreen",
               hjust = 0,
               fontface = "bold") +
      
      annotate("text",
               x = min(data$year),
               y = 44,
               label = "Fair",
               color = "goldenrod",
               hjust = 0,
               fontface = "bold") +
      
      annotate("text",
               x = min(data$year),
               y = 20,
               label = "Poor",
               color = "red",
               hjust = 0,
               fontface = "bold")
  }
  
  return(p)
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
      border: 2px solid #2E7D32;
    }
    .sidebar-custom {
      background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%);
      border-radius: 10px; padding: 0px 20px 20px 20px;
    }
    .main-title {
      background: linear-gradient(135deg, #2E7D32 0%, #66BB6A 100%);
      color: white; 
      padding: 30px; 
      margin: -15px -15px 0 -15px;
      text-align: center;
      box-shadow: 0 2px 8px rgba(0,0,0,0.1);
      border-radius: 0;  
    }
    .dataTables_wrapper { font-size: 0.85rem !important; }
    
    /* Navigation bar styles */
    .nav-bar {
      background: #e9ecef;
      padding: 8px 0;
      margin: 0 -15px 30px -15px;
      box-shadow: 0 2px 8px rgba(0,0,0,0.08);
      border: 1px solid #dee2e6;
      position: sticky;
      top: 0;
      z-index: 1000;
    }
    .nav-bar a {
      color: #2E7D32;
      text-decoration: none;
      padding: 5px 18px;
      margin: 0 5px;
      border-radius: 4px;
      transition: all 0.3s ease;
      font-weight: 600;
      font-size: 0.9rem;
      border: 1px solid transparent;
    }
    .nav-bar a:hover {
      background-color: #2E7D32;  /* Green background on hover */
      color: white;  /* White text on hover */
      border: 1px solid #2E7D32;
      transform: translateY(-1px);  /* Slight lift effect */
      box-shadow: 0 2px 4px rgba(46, 125, 50, 0.2);
    }
    .nav-bar-container {
      display: flex;
      justify-content: center;
      align-items: center;
      flex-wrap: wrap;
    }
    .brush-info-section {
      background: linear-gradient(135deg, #e8f5e9 0%, #f1f8e9 100%);
      border-radius: 12px; padding: 20px; margin: 20px 0;
      border: 1px solid #66BB6A;
    }
  "))
  ),
  
  # Main title
  div(class = "main-title",
      h1("Wetland Vegetation Dashboard",
         style = "margin: 0; font-size: 2rem; text-shadow: 2px 2px 4px rgba(0,0,0,0.3)")
  ),
  
  # Navigation bar
  div(class = "nav-bar",
      div(class = "nav-bar-container",
          tags$a(href = "#plots", "Metrics Plots"),
          tags$a(href = "#table", "Metrics Table"),
          tags$a(href = "#list", "Species Lists"),
          tags$a(href = "#about", "About")
      )
  ),
  
  #--------------------------------#
  ####   Time Series Section    ####
  #--------------------------------#
  
  div(id = "plots", class = "content-section",
      layout_sidebar(
        
        sidebar = sidebar(
          class = "sidebar-custom", width = 300,
          
          h4("Plot Controls", style = "color: #2E7D32;"),
          
          create_picker_input(
            "ts_metric",
            "Select Metric(s):",
            choices = c(
              "VMMI" = "vmmi",
              "Mean COC" = "mean.coc",
              "Invasive Cover" = "inv.cov",
              "Bryophyte Cover" = "bryo.cov",
              "Stress Tolerance Cover" = "strtol.cov"
            ),
            selected = "vmmi",
            multiple = TRUE,
            none_text = "Choose metrics"
          ),
          
          checkboxGroupInput(
            "ts_wetland",
            div("Select Wetland(s):"),
            choices = c("Great Meadow", "Gilmore Meadow"),
            selected = c("Great Meadow", "Gilmore Meadow")
          ),
          
          checkboxInput(
            "ts_show_labels",
            "Show number of sites surveyed each year",
            value = FALSE
          ),
          
          tags$small(
            style = "color: #6c757d; display: block; margin-top: -8px; margin-bottom: 10px; font-style: italic;",
            "*Note: Vegetation data was collected inconsistently over time. Read more in the About section."
          ),
          
          div(style = "padding: 10px; background-color: #e8f5e9; border-radius: 8px; border-left: 4px solid #2E7D32;",
              p(icon("info-circle"), "Points show annual means ± SE across sites within each wetland. Dashed lines represent grand means across all years.", 
                style = "margin: 0; font-size: 0.9rem; color: #1b5e20;")),
          
          div(style = "margin-top: 15px; text-align: center;",
              downloadButton("download_plot", "Download Plot",
                             class = "btn-primary btn-sm", icon = icon("image"))),
          
          div(style = "margin-top: 5px; text-align: center;",
              tags$a(href = "#about",
                     class = "btn btn-primary btn-sm", icon("info-circle"),
                     "About"))
        ),
        
        card(
          full_screen = TRUE,
          card_header(class = "bg-primary text-white",
                      "Vegetation Metrics Plots"),
          div(style = "padding: 20px;",
              uiOutput("ts_plots", height = "600px"))
        )
      )
  ),
  
  #--------------------------------#
  ####       VMMI Section       ####
  #--------------------------------#
  
  div(id = "table", class = "content-section",
      layout_sidebar(
        
        sidebar = sidebar(
          class = "sidebar-custom", width = 300,
          
          h4("Table Controls", style = "color: #2E7D32;"),
          
          create_picker_input("vmmi_site", "Select Site(s):",
                              choices = site_lookup,
                              selected = site_lookup[1]),
          
          create_picker_input("vmmi_year", "Select Year(s):",
                              choices = sort(unique(vmmi_data$year)),
                              selected = NULL),
          tags$small(
            style = "color: #6c757d; display: block; margin-top: -8px; margin-bottom: 10px; font-style: italic;",
            "*Note: Year options update based on selected site(s). Wetland-level summaries select all sites and years by default."
          ),
          
          radioButtons("vmmi_summary", "Summarize Statistics By:",
                       choices = c("Site per Year" = "year",
                                   "Site Averaged Across Years" = "multi",
                                   "Wetland per Year" = "wetland_year",
                                   "Wetland Averaged Across Years" = "all_wetlands"),
                       selected = "year"),
          
          div(style = "margin-top: 15px; text-align: center;",
              downloadButton("download_vmmi", "Download Table",
                             class = "btn-primary btn-sm", icon = icon("download"))),
          
          div(style = "margin-top: 5px; text-align: center;",
              tags$a(href = "#about",
                     class = "btn btn-primary btn-sm", icon("info-circle"),
                     "About")),
        ),
        
        card(
          full_screen = TRUE,
          card_header(class = "bg-primary text-white",
                      "Vegetation Metrics Table"),
          div(style = "padding: 10px;",
              dataTableOutput("vmmi_table"))
        )
      )
  ),
  
  #--------------------------------#
  ####   Species List Section   ####
  #--------------------------------#
  
  div(id = "list", class = "content-section",
      layout_sidebar(
        
        sidebar = sidebar(
          class = "sidebar-custom", width = 300,
          
          h4("Table Controls", style = "color: #2E7D32;"),
          
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
          
          div(style = "margin-top: 15px; text-align: center;",
              downloadButton("download_species", "Download Table",
                             class = "btn-primary btn-sm", icon = icon("download"))),
          
          div(style = "margin-top: 5px; text-align: center;",
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
  
  # Update site and year selections based on summary level
  observeEvent(input$vmmi_summary, {
    if (input$vmmi_summary %in% c("wetland_year", "all_wetlands")) {
      # For wetland-level summaries, select all sites and years
      all_sites <- site_lookup
      
      # Get all available years for selected sites
      available_years <- vmmi_data %>%
        filter(site.name %in% all_sites) %>%
        pull(year) %>%
        unique() %>%
        sort()
      
      # Update both inputs
      updatePickerInput(
        session,
        "vmmi_site",
        selected = all_sites
      )
      
      updatePickerInput(
        session,
        "vmmi_year",
        choices = available_years,
        selected = available_years
      )
    }
  }, ignoreInit = TRUE)
  
  # processing
  vmmi_filtered <- reactive({
    req(input$vmmi_site, input$vmmi_year)
    
    vmmi_data %>%
      filter(site.name %in% input$vmmi_site,
             year %in% input$vmmi_year)
  })
  
  vmmi_summary <- reactive({
    
    df <- vmmi_filtered() %>%
      left_join(monitoring_sites, by = "site.name") %>%
      mutate(
        wetland = case_when(
          grepl("Great Meadow", display.site.name) ~ "Great Meadow",
          grepl("Gilmore Meadow", display.site.name) ~ "Gilmore Meadow",
          TRUE ~ "Other"
        )
      )
    
    switch(input$vmmi_summary,
           
           "year" = {
             # Site per Year
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
             # Site Averaged Across Years
             df %>%
               group_by(site.name) %>%
               summarise(
                 Site = first(display.site.name),
                 Wetland = first(wetland),
                 Year = paste0(min(year), "–", max(year)),
                 across(c(mean.coc, inv.cov, bryo.cov, strtol.cov, vmmi),
                        ~ round(mean(.x, na.rm = TRUE), 2)),
                 
                 vmmi_mean = mean(vmmi, na.rm = TRUE),
                 
                 vmmi.rating = case_when(
                   vmmi_mean > 60.94853 ~ "Good",
                   vmmi_mean < 41.48136 ~ "Poor",
                   TRUE ~ "Fair"
                 ),
                 .groups = "drop"
               ) %>%
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
           },
           
           "wetland_year" = {
             # Wetland per Year (NEW)
             df %>%
               group_by(wetland, year) %>%
               summarise(
                 `Number of Sites` = n_distinct(site.name), 
                 across(c(mean.coc, inv.cov, bryo.cov, strtol.cov, vmmi),
                        ~ round(mean(.x, na.rm = TRUE), 2)),
                 
                 vmmi_mean = mean(vmmi, na.rm = TRUE),
                 
                 vmmi.rating = case_when(
                   vmmi_mean > 60.94853 ~ "Good",
                   vmmi_mean < 41.48136 ~ "Poor",
                   TRUE ~ "Fair"
                 ),
                 .groups = "drop"
               ) %>%
               select(-vmmi_mean) %>%
               rename(
                 Year = year,
                 Wetland = wetland,
                 `Mean COC` = mean.coc,
                 `Invasive Cover` = inv.cov,
                 `Bryophyte Cover` = bryo.cov,
                 `Stress Tolerance Cover` = strtol.cov,
                 VMMI = vmmi,
                 `VMMI Rating` = vmmi.rating
               ) %>%
               select(Wetland, Year, `Number of Sites`, everything()) %>%
               arrange(Wetland, Year)
           },
           
           "all_wetlands" = {
             # Wetland Averaged Across Years (NEW)
             df %>%
               group_by(wetland) %>%
               summarise(
                 Year = paste0(min(year), "–", max(year)),
                 `Number of Sites` = n_distinct(site.name), 
                 across(c(mean.coc, inv.cov, bryo.cov, strtol.cov, vmmi),
                        ~ round(mean(.x, na.rm = TRUE), 2)),
                 
                 vmmi_mean = mean(vmmi, na.rm = TRUE),
                 
                 vmmi.rating = case_when(
                   vmmi_mean > 60.94853 ~ "Good",
                   vmmi_mean < 41.48136 ~ "Poor",
                   TRUE ~ "Fair"
                 ),
                 .groups = "drop"
               ) %>%
               select(-vmmi_mean) %>%
               rename(
                 Wetland = wetland,
                 `Mean COC` = mean.coc,
                 `Invasive Cover` = inv.cov,
                 `Bryophyte Cover` = bryo.cov,
                 `Stress Tolerance Cover` = strtol.cov,
                 VMMI = vmmi,
                 `VMMI Rating` = vmmi.rating
               ) %>%
               select(Wetland, Year, `Number of Sites`, everything()) %>%
               arrange(Wetland)
           }
    )
  })
  
  #-------------------------------#
  ####  Time Series Processing ####
  #-------------------------------#
  
  # Compute statistics for time series
  veg_stats <- reactive({
    req(input$ts_wetland)
    
    vmmi_with_wetland <- vmmi_data %>%
      left_join(monitoring_sites, by = "site.name") %>%
      mutate(
        wetland = case_when(
          grepl("Great Meadow", display.site.name) ~ "Great Meadow",
          grepl("Gilmore Meadow", display.site.name) ~ "Gilmore Meadow",
          TRUE ~ "Other"
        )
      ) %>%
      filter(wetland %in% input$ts_wetland)
    
    vmmi_with_wetland %>%
      group_by(wetland, year) %>%
      summarise(
        across(
          c(vmmi, mean.coc, inv.cov, bryo.cov, strtol.cov),
          list(avg = ~mean(.x, na.rm = TRUE),
               se  = ~sd(.x, na.rm = TRUE) / sqrt(n())),
          .names = "{.col}_{.fn}"
        ),
        n_sites = n_distinct(site.name),
        .groups = "drop"
      )
  })
  
  # Compute grand means
  veg_grand <- reactive({
    req(veg_stats())
    
    veg_stats() %>%
      group_by(wetland) %>%
      summarise(
        across(
          ends_with("_avg"),
          ~ mean(.x, na.rm = TRUE),
          .names = "{.col}_grand"
        ),
        .groups = "drop"
      )
  })
  
  # Render time series plot
  # In server (around line 672), replace the entire output$ts_plot section with:
  output$ts_plots <- renderUI({
    req(input$ts_metric, input$ts_wetland)
    
    metric_labels <- c(
      "vmmi" = "VMMI",
      "mean.coc" = "Mean COC",
      "inv.cov" = "Invasive Cover (%)",
      "bryo.cov" = "Bryophyte Cover (%)",
      "strtol.cov" = "Stress Tolerance Cover (%)"
    )
    
    # Create a plot for each selected metric
    plot_outputs <- lapply(seq_along(input$ts_metric), function(i) {
      metric <- input$ts_metric[i]
      output_id <- paste0("plot_", metric)
      
      # Generate the plot
      output[[output_id]] <- renderPlot({
        plot_veg_metric(
          data = veg_stats(),
          grand_data = veg_grand(),
          metric = metric,
          y_label = metric_labels[metric],
          title = paste(metric_labels[metric], "Over Time"),
          show_labels = input$ts_show_labels
        )
      })
      
      # Return the plot output UI
      plotOutput(output_id, height = "600px")
    })
    
    # Stack all plots vertically
    tagList(plot_outputs)
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
      df <- df %>% filter(invasive == "TRUE")
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
      # Extract just the site number from display name
      mutate(
        wetland = case_when(
          grepl("Great Meadow", display.site.name) ~ "Great Meadow",
          grepl("Gilmore Meadow", display.site.name) ~ "Gilmore Meadow",
          TRUE ~ "Other"
        ),
        site_number = str_extract(display.site.name, "\\d+$")  # Extract trailing numbers
      ) %>%
      group_by(latin.name, common.name, invasive) %>%
      summarise(
        `Latin Name` = first(latin.name),
        `Common Name` = first(common.name),
        Invasive = first(invasive),
        `Years Found` = paste(sort(unique(year)), collapse = ", "),
        
        # Group sites by wetland
        `Site(s)` = {
          site_df <- data.frame(
            wetland = wetland,
            site_num = site_number
          ) %>%
            distinct() %>%
            arrange(wetland, site_num)
          
          # Create grouped string
          site_groups <- site_df %>%
            group_by(wetland) %>%
            summarise(sites = paste(site_num, collapse = ", "), .groups = "drop") %>%
            mutate(formatted = paste0(wetland, " ", sites))
          
          paste(site_groups$formatted, collapse = "; ")
        },
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
  
  # Download plot as PNG
  output$download_plot <- downloadHandler(
    filename = function() {
      paste0("VegMetrics_", Sys.Date(), ".png")
    },
    content = function(file) {
      metric_labels <- c(
        "vmmi" = "VMMI",
        "mean.coc" = "Mean COC",
        "inv.cov" = "Invasive Cover (%)",
        "bryo.cov" = "Bryophyte Cover (%)",
        "strtol.cov" = "Stress Tolerance Cover (%)"
      )
      
      # Create all selected plots
      plots <- lapply(input$ts_metric, function(metric) {
        plot_veg_metric(
          data = veg_stats(),
          grand_data = veg_grand(),
          metric = metric,
          y_label = metric_labels[metric],
          title = paste(metric_labels[metric], "Over Time"),
          show_labels = input$ts_show_labels
        )
      })
      
      # Stack plots vertically using patchwork
      combined_plot <- wrap_plots(plots, ncol = 1)
      
      # Save with height adjusted for number of plots
      ggsave(file, plot = combined_plot, device = "png", 
             width = 10, height = 7 * length(input$ts_metric), dpi = 300, units = "in")
    }
  )
  
}


# Run app
shinyApp(ui, server)



