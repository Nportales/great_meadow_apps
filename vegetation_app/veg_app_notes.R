#### Wetland Vegetation Dashboard NOTES ####

#### n=sites side panel option -------------------------------------------------

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
  sd_col    <- paste0(metric, "_sd")
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
      position = position_jitter(width = 0.03, height = 0)
    ) +
    
    geom_errorbar(
      aes(ymin = .data[[avg_col]] - .data[[sd_col]],
          ymax = .data[[avg_col]] + .data[[sd_col]]),
      width = 0, alpha = 0.6
    ) +
    
    # ADD SITE COUNT LABELS if requested
    {if (show_labels && "n_sites" %in% names(data)) 
      geom_text(
        aes(label = n_sites),
        vjust = -1.6,
        size = 3.8,
        fontface = "bold",
        color = "grey25",
        show.legend = FALSE,
        position = position_jitter(width = 0.03, height = 0, seed = 42)
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
      breaks = c("Great Meadow", "Gilmore Meadow"),  # Explicit order
      drop = TRUE
    ) +
    
    scale_shape_manual(
      name = "Wetland Annual Mean",
      values = c(
        "Great Meadow" = 16,
        "Gilmore Meadow" = 17
      ),
      breaks = c("Great Meadow", "Gilmore Meadow"),  # Explicit order
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
      plot.margin = margin(10, 5, 10, 10)
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
  ####       VMMI Section       ####
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
          
          radioButtons("vmmi_summary", "Summarize VMMI Statistics By:",
                       choices = c("Site per Year" = "year",
                                   "Site Averaged Across Years" = "multi",
                                   "Wetland per Year" = "wetland_year",
                                   "Wetland Averaged Across Years" = "all_wetlands"),
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
  
  #--------------------------------#
  ####   Time Series Section    ####
  #--------------------------------#
  
  div(class = "content-section",
      layout_sidebar(
        
        sidebar = sidebar(
          class = "sidebar-custom", width = 300,
          
          h4("Plot Controls", style = "color: #2E7D32;"),
          
          selectInput(
            "ts_metric",
            div(icon("chart-line"), "Select Metric:"),
            choices = c(
              "VMMI" = "vmmi",
              "Mean COC" = "mean.coc",
              "Invasive Cover" = "inv.cov",
              "Bryophyte Cover" = "bryo.cov",
              "Stress Tolerance Cover" = "strtol.cov"
            ),
            selected = "vmmi"
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
              p(icon("info-circle"), "Lines show annual means ± SD across sites within each wetland. Dashed lines represent grand means across all years.", 
                style = "margin: 0; font-size: 0.9rem; color: #1b5e20;")),
          
          br(),
          downloadButton("download_plot", "Download Plot",
                         class = "btn-primary btn-sm"),
          
          div(style = "margin-top: 15px; text-align: center;",
              tags$a(href = "#about",
                     class = "btn btn-primary btn-sm", icon("info-circle"),
                     "About"))
        ),
        
        card(
          full_screen = TRUE,
          card_header(class = "bg-primary text-white",
                      "VMMI Metrics Over Time"),
          div(style = "padding: 20px;",
              plotOutput("ts_plot", height = "600px"))
        )
      )
  ),
  
  #--------------------------------#
  ####   Species List Section   ####
  #--------------------------------#
  
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
               arrange(Wetland, Year)
           },
           
           "all_wetlands" = {
             # Wetland Averaged Across Years (NEW)
             df %>%
               group_by(wetland) %>%
               summarise(
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
    
    # Add wetland column to vmmi_data based on site names
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
               sd  = ~sd(.x, na.rm = TRUE)),
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
  output$ts_plot <- renderPlot({
    req(input$ts_metric, input$ts_wetland)
    
    metric_labels <- c(
      "vmmi" = "VMMI",
      "mean.coc" = "Mean COC",
      "inv.cov" = "Invasive Cover (%)",
      "bryo.cov" = "Bryophyte Cover (%)",
      "strtol.cov" = "Stress Tolerance Cover (%)"
    )
    
    plot_veg_metric(
      data = veg_stats(),
      grand_data = veg_grand(),
      metric = input$ts_metric,
      y_label = metric_labels[input$ts_metric],
      title = paste(metric_labels[input$ts_metric], "Over Time"),
      show_labels = input$ts_show_labels
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
  
  # Download plot as PNG
  output$download_plot <- downloadHandler(
    filename = function() {
      metric_names <- c(
        "vmmi" = "VMMI",
        "mean.coc" = "MeanCOC",
        "inv.cov" = "InvasiveCover",
        "bryo.cov" = "BryophyteCover",
        "strtol.cov" = "StressToleranceCover"
      )
      paste0(metric_names[input$ts_metric], "_", Sys.Date(), ".png")
    },
    content = function(file) {
      # Create the plot
      metric_labels <- c(
        "vmmi" = "VMMI",
        "mean.coc" = "Mean COC",
        "inv.cov" = "Invasive Cover (%)",
        "bryo.cov" = "Bryophyte Cover (%)",
        "strtol.cov" = "Stress Tolerance Cover (%)"
      )
      
      p <- plot_veg_metric(
        data = veg_stats(),
        grand_data = veg_grand(),
        metric = input$ts_metric,
        y_label = metric_labels[input$ts_metric],
        title = paste(metric_labels[input$ts_metric], "Over Time"),
        show_labels = input$ts_show_labels
      )
      
      # Save as PNG with high resolution
      ggsave(file, plot = p, device = "png", 
             width = 10, height = 7, dpi = 300, units = "in")
    }
  )
  
}


# Run app
shinyApp(ui, server)








#### app version prior to any n=site work or hover plotly-----------------------

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

# Time series plotting function
plot_veg_metric <- function(data, grand_data, metric, y_label, title) {
  
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
  sd_col    <- paste0(metric, "_sd")
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
      position = position_jitter(width = 0.03, height = 0)
    ) +
    
    geom_errorbar(
      aes(ymin = .data[[avg_col]] - .data[[sd_col]],
          ymax = .data[[avg_col]] + .data[[sd_col]]),
      width = 0, alpha = 0.6
    ) +
    
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
      breaks = c("Great Meadow", "Gilmore Meadow"),  # Explicit order
      drop = TRUE
    ) +
    
    scale_shape_manual(
      name = "Wetland Annual Mean",
      values = c(
        "Great Meadow" = 16,
        "Gilmore Meadow" = 17
      ),
      breaks = c("Great Meadow", "Gilmore Meadow"),  # Explicit order
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
      plot.margin = margin(10, 5, 10, 10)
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
  ####       VMMI Section       ####
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
          
          radioButtons("vmmi_summary", "Summarize VMMI Statistics By:",
                       choices = c("Site per Year" = "year",
                                   "Site Averaged Across Years" = "multi",
                                   "Wetland per Year" = "wetland_year",
                                   "Wetland Averaged Across Years" = "all_wetlands"),
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
  
  #--------------------------------#
  ####   Time Series Section    ####
  #--------------------------------#
  
  div(class = "content-section",
      layout_sidebar(
        
        sidebar = sidebar(
          class = "sidebar-custom", width = 300,
          
          h4("Plot Controls", style = "color: #2E7D32;"),
          
          selectInput(
            "ts_metric",
            div(icon("chart-line"), "Select Metric:"),
            choices = c(
              "VMMI" = "vmmi",
              "Mean COC" = "mean.coc",
              "Invasive Cover" = "inv.cov",
              "Bryophyte Cover" = "bryo.cov",
              "Stress Tolerance Cover" = "strtol.cov"
            ),
            selected = "vmmi"
          ),
          
          checkboxGroupInput(
            "ts_wetland",
            div("Select Wetland(s):"),
            choices = c("Great Meadow", "Gilmore Meadow"),
            selected = c("Great Meadow", "Gilmore Meadow")
          ),
          
          br(),
          div(style = "padding: 10px; background-color: #e8f5e9; border-radius: 8px; border-left: 4px solid #2E7D32;",
              p(icon("info-circle"), "Lines show annual means ± SD across sites within each wetland. Dashed lines represent grand means across all years.", 
                style = "margin: 0; font-size: 0.9rem; color: #1b5e20;")),
          
          br(),
          downloadButton("download_plot", "Download Plot",
                         class = "btn-primary btn-sm"),
          
          div(style = "margin-top: 15px; text-align: center;",
              tags$a(href = "#about",
                     class = "btn btn-primary btn-sm", icon("info-circle"),
                     "About"))
        ),
        
        card(
          full_screen = TRUE,
          card_header(class = "bg-primary text-white",
                      "VMMI Metrics Over Time"),
          div(style = "padding: 20px;",
              plotOutput("ts_plot", height = "500px"))
        )
      )
  ),
  
  #--------------------------------#
  ####   Species List Section   ####
  #--------------------------------#
  
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
               arrange(Wetland, Year)
           },
           
           "all_wetlands" = {
             # Wetland Averaged Across Years (NEW)
             df %>%
               group_by(wetland) %>%
               summarise(
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
    
    # Add wetland column to vmmi_data based on site names
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
               sd  = ~sd(.x, na.rm = TRUE)),
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
  output$ts_plot <- renderPlot({
    req(input$ts_metric, input$ts_wetland)
    
    metric_labels <- c(
      "vmmi" = "VMMI",
      "mean.coc" = "Mean COC",
      "inv.cov" = "Invasive Cover (%)",
      "bryo.cov" = "Bryophyte Cover (%)",
      "strtol.cov" = "Stress Tolerance Cover (%)"
    )
    
    plot_veg_metric(
      data = veg_stats(),
      grand_data = veg_grand(),
      metric = input$ts_metric,
      y_label = metric_labels[input$ts_metric],
      title = paste(metric_labels[input$ts_metric], "Over Time")
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
  
  # Download plot as PNG
  output$download_plot <- downloadHandler(
    filename = function() {
      metric_names <- c(
        "vmmi" = "VMMI",
        "mean.coc" = "MeanCOC",
        "inv.cov" = "InvasiveCover",
        "bryo.cov" = "BryophyteCover",
        "strtol.cov" = "StressToleranceCover"
      )
      paste0(metric_names[input$ts_metric], "_", Sys.Date(), ".png")
    },
    content = function(file) {
      # Create the plot
      metric_labels <- c(
        "vmmi" = "VMMI",
        "mean.coc" = "Mean COC",
        "inv.cov" = "Invasive Cover (%)",
        "bryo.cov" = "Bryophyte Cover (%)",
        "strtol.cov" = "Stress Tolerance Cover (%)"
      )
      
      p <- plot_veg_metric(
        data = veg_stats(),
        grand_data = veg_grand(),
        metric = input$ts_metric,
        y_label = metric_labels[input$ts_metric],
        title = paste(metric_labels[input$ts_metric], "Over Time")
      )
      
      # Save as PNG with high resolution
      ggsave(file, plot = p, device = "png", 
             width = 10, height = 6, dpi = 300, units = "in")
    }
  )
  
}


# Run app
shinyApp(ui, server)




#### hover plotly - shiny assistant --------------------------------------------
#### Wetland Vegetation Dashboard ####

#---------------------------------------------#
####        Load Required Packages         ####
#---------------------------------------------#

library(shiny)
library(tidyverse)
library(shinyWidgets)
library(DT)
library(bslib)
library(plotly)

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

plot_veg_metric <- function(data, grand_data, metric, y_label, title) {
  
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
  sd_col    <- paste0(metric, "_sd")
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
      position = position_jitter(width = 0.03, height = 0)
    ) +
    
    geom_errorbar(
      aes(ymin = .data[[avg_col]] - .data[[sd_col]],
          ymax = .data[[avg_col]] + .data[[sd_col]]),
      width = 0, alpha = 0.6
    ) +
    
    # Grand mean lines - use wetland for color mapping
    geom_hline(
      data = grand_data,
      aes(yintercept = .data[[grand_col]], 
          linetype = grand_label,
          color = wetland),
      linewidth = 1,
      show.legend = c(linetype = TRUE, color = FALSE),
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
      plot.margin = margin(10, 5, 10, 10)
    ) +
    
    guides(
      color = guide_legend(order = 1, override.aes = list(linewidth = 1.2)),
      shape = guide_legend(order = 1),
      linetype = guide_legend(
        order = 2,
        override.aes = list(
          color = grand_colors,
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

# Interactive version with hover tooltips
plot_veg_metric_interactive <- function(data, grand_data, metric, y_label, title) {
  
  # Sort wetlands_in_data to ensure Great Meadow is first
  wetlands_in_data <- unique(as.character(data$wetland))
  wetlands_in_data <- sort(wetlands_in_data, decreasing = TRUE)
  
  # Enforce consistent ordering
  data$wetland <- factor(data$wetland,
                         levels = c("Great Meadow", "Gilmore Meadow"))
  
  grand_data <- grand_data %>%
    filter(wetland %in% wetlands_in_data) %>%
    mutate(wetland = factor(wetland, levels = c("Great Meadow", "Gilmore Meadow")))
  
  avg_col   <- paste0(metric, "_avg")
  sd_col    <- paste0(metric, "_sd")
  n_col     <- paste0(metric, "_n")
  grand_col <- paste0(metric, "_avg_grand")
  
  # Get grand mean values
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
  
  # Create hover text
  data <- data %>%
    mutate(
      hover_text = paste0(
        "<b>", wetland, "</b><br>",
        "Year: ", year, "<br>",
        "Mean: ", round(.data[[avg_col]], 2), "<br>",
        "SD: ± ", round(.data[[sd_col]], 2), "<br>",
        "n sites: ", .data[[n_col]]
      )
    )
  
  # Initialize plotly object
  fig <- plot_ly()
  
  # Add VMMI shading if applicable
  if (metric == "vmmi") {
    fig <- fig %>%
      add_trace(
        type = "scatter",
        mode = "none",
        x = c(min(data$year), max(data$year), max(data$year), min(data$year)),
        y = c(-Inf, -Inf, 41.48136, 41.48136),
        fill = "toself",
        fillcolor = "rgba(255, 0, 0, 0.08)",
        line = list(width = 0),
        showlegend = FALSE,
        hoverinfo = "skip"
      ) %>%
      add_trace(
        type = "scatter",
        mode = "none",
        x = c(min(data$year), max(data$year), max(data$year), min(data$year)),
        y = c(41.48136, 41.48136, 60.94853, 60.94853),
        fill = "toself",
        fillcolor = "rgba(218, 165, 32, 0.04)",
        line = list(width = 0),
        showlegend = FALSE,
        hoverinfo = "skip"
      ) %>%
      add_trace(
        type = "scatter",
        mode = "none",
        x = c(min(data$year), max(data$year), max(data$year), min(data$year)),
        y = c(60.94853, 60.94853, Inf, Inf),
        fill = "toself",
        fillcolor = "rgba(0, 255, 0, 0.08)",
        line = list(width = 0),
        showlegend = FALSE,
        hoverinfo = "skip"
      ) %>%
      add_annotations(
        x = min(data$year),
        y = c(20, 44, 63),
        text = c("Poor", "Fair", "Good"),
        xanchor = "left",
        showarrow = FALSE,
        font = list(color = c("red", "goldenrod", "darkgreen"), size = 12, family = "bold")
      )
  }
  
  # Add data for each wetland
  for (wetland_name in wetlands_in_data) {
    wetland_data <- data %>% filter(wetland == wetland_name)
    color <- if(wetland_name == "Great Meadow") "black" else "grey67"
    shape <- if(wetland_name == "Great Meadow") "circle" else "triangle-up"
    
    # Add error bars
    fig <- fig %>%
      add_trace(
        data = wetland_data,
        x = ~year,
        y = ~.data[[avg_col]],
        error_y = list(
          type = "data",
          array = ~.data[[sd_col]],
          color = color,
          thickness = 1.5,
          width = 0
        ),
        type = "scatter",
        mode = "none",
        showlegend = FALSE,
        hoverinfo = "skip"
      )
    
    # Add line
    fig <- fig %>%
      add_trace(
        data = wetland_data,
        x = ~year,
        y = ~.data[[avg_col]],
        type = "scatter",
        mode = "lines",
        line = list(color = color, width = 2.5),
        name = wetland_name,
        legendgroup = wetland_name,
        showlegend = TRUE,
        hoverinfo = "skip"
      )
    
    # Add points
    fig <- fig %>%
      add_trace(
        data = wetland_data,
        x = ~year,
        y = ~.data[[avg_col]],
        text = ~hover_text,
        type = "scatter",
        mode = "markers",
        marker = list(
          color = color,
          size = 10,
          symbol = shape,
          line = list(width = 0)
        ),
        name = wetland_name,
        legendgroup = wetland_name,
        showlegend = FALSE,
        hovertemplate = "%{text}<extra></extra>"
      )
  }
  
  # Add grand mean lines
  if (!is.null(gm_grand)) {
    fig <- fig %>%
      add_trace(
        x = c(min(data$year), max(data$year)),
        y = c(gm_grand, gm_grand),
        type = "scatter",
        mode = "lines",
        line = list(color = "black", width = 2, dash = "dash"),
        name = sprintf("Great Meadow (%.2f)", gm_grand),
        legendgroup = "grand_gm",
        showlegend = TRUE,
        hoverinfo = "skip"
      )
  }
  
  if (!is.null(gl_grand)) {
    fig <- fig %>%
      add_trace(
        x = c(min(data$year), max(data$year)),
        y = c(gl_grand, gl_grand),
        type = "scatter",
        mode = "lines",
        line = list(color = "grey67", width = 2, dash = "dash"),
        name = sprintf("Gilmore Meadow (%.2f)", gl_grand),
        legendgroup = "grand_gl",
        showlegend = TRUE,
        hoverinfo = "skip"
      )
  }
  
  # Configure layout
  fig <- fig %>%
    layout(
      title = list(text = title, x = 0.5, xanchor = "center"),
      xaxis = list(
        title = "Year",
        dtick = 1,
        tickangle = -45
      ),
      yaxis = list(
        title = y_label
      ),
      hovermode = "closest",
      hoverlabel = list(
        bgcolor = "white",
        font = list(size = 12, color = "black"),
        bordercolor = "black"
      ),
      legend = list(
        orientation = "v",
        x = 1.02,
        y = 1,
        xanchor = "left",
        yanchor = "top",
        title = list(text = "<b>Wetland Annual Mean</b><br><br><b>Wetland Grand Mean</b>")
      ),
      plot_bgcolor = "white",
      paper_bgcolor = "white"
    )
  
  return(fig)
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
  ####       VMMI Section       ####
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
          
          radioButtons("vmmi_summary", "Summarize VMMI Statistics By:",
                       choices = c("Site per Year" = "year",
                                   "Site Averaged Across Years" = "multi",
                                   "Wetland per Year" = "wetland_year",
                                   "Wetland Averaged Across Years" = "all_wetlands"),
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
  
  #--------------------------------#
  ####   Time Series Section    ####
  #--------------------------------#
  
  div(class = "content-section",
      layout_sidebar(
        
        sidebar = sidebar(
          class = "sidebar-custom", width = 300,
          
          h4("Plot Controls", style = "color: #2E7D32;"),
          
          selectInput(
            "ts_metric",
            div(icon("chart-line"), "Select Metric:"),
            choices = c(
              "VMMI" = "vmmi",
              "Mean COC" = "mean.coc",
              "Invasive Cover" = "inv.cov",
              "Bryophyte Cover" = "bryo.cov",
              "Stress Tolerance Cover" = "strtol.cov"
            ),
            selected = "vmmi"
          ),
          
          checkboxGroupInput(
            "ts_wetland",
            div("Select Wetland(s):"),
            choices = c("Great Meadow", "Gilmore Meadow"),
            selected = c("Great Meadow", "Gilmore Meadow")
          ),
          
          br(),
          div(style = "padding: 10px; background-color: #e8f5e9; border-radius: 8px; border-left: 4px solid #2E7D32;",
              p(icon("info-circle"), "Lines show annual means ± SD across sites within each wetland. Dashed lines represent grand means across all years.", 
                style = "margin: 0; font-size: 0.9rem; color: #1b5e20;")),
          
          br(),
          downloadButton("download_plot", "Download Plot",
                         class = "btn-primary btn-sm"),
          
          div(style = "margin-top: 15px; text-align: center;",
              tags$a(href = "#about",
                     class = "btn btn-primary btn-sm", icon("info-circle"),
                     "About"))
        ),
        
        card(
          full_screen = TRUE,
          card_header(class = "bg-primary text-white",
                      "VMMI Metrics Over Time"),
          div(style = "padding: 20px;",
              plotlyOutput("ts_plot", height = "500px"))
        )
      )
  ),
  
  #--------------------------------#
  ####   Species List Section   ####
  #--------------------------------#
  
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
               arrange(Wetland, Year)
           },
           
           "all_wetlands" = {
             # Wetland Averaged Across Years (NEW)
             df %>%
               group_by(wetland) %>%
               summarise(
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
    
    # Add wetland column to vmmi_data based on site names
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
               sd  = ~sd(.x, na.rm = TRUE),
               n   = ~n()),  # NEW: add n for each metric
          .names = "{.col}_{.fn}"
        ),
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
  
  # Render time series plot (interactive with hover)
  output$ts_plot <- renderPlotly({
    req(input$ts_metric, input$ts_wetland)
    
    metric_labels <- c(
      "vmmi" = "VMMI",
      "mean.coc" = "Mean COC",
      "inv.cov" = "Invasive Cover (%)",
      "bryo.cov" = "Bryophyte Cover (%)",
      "strtol.cov" = "Stress Tolerance Cover (%)"
    )
    
    plot_veg_metric_interactive(
      data = veg_stats(),
      grand_data = veg_grand(),
      metric = input$ts_metric,
      y_label = metric_labels[input$ts_metric],
      title = paste(metric_labels[input$ts_metric], "Over Time")
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
  
  # Download plot as PNG
  output$download_plot <- downloadHandler(
    filename = function() {
      metric_names <- c(
        "vmmi" = "VMMI",
        "mean.coc" = "MeanCOC",
        "inv.cov" = "InvasiveCover",
        "bryo.cov" = "BryophyteCover",
        "strtol.cov" = "StressToleranceCover"
      )
      paste0(metric_names[input$ts_metric], "_", Sys.Date(), ".png")
    },
    content = function(file) {
      # Create the plot
      metric_labels <- c(
        "vmmi" = "VMMI",
        "mean.coc" = "Mean COC",
        "inv.cov" = "Invasive Cover (%)",
        "bryo.cov" = "Bryophyte Cover (%)",
        "strtol.cov" = "Stress Tolerance Cover (%)"
      )
      
      p <- plot_veg_metric(
        data = veg_stats(),
        grand_data = veg_grand(),
        metric = input$ts_metric,
        y_label = metric_labels[input$ts_metric],
        title = paste(metric_labels[input$ts_metric], "Over Time")
      )
      
      # Save as PNG with high resolution
      ggsave(file, plot = p, device = "png", 
             width = 10, height = 6, dpi = 300, units = "in")
    }
  )
  
}


# Run app
shinyApp(ui, server)







#### hover plotly --------------------------------------------------------------
#### Wetland Vegetation Dashboard ####

#---------------------------------------------#
####        Load Required Packages         ####
#---------------------------------------------#

library(shiny)
library(tidyverse)
library(shinyWidgets)
library(DT)
library(bslib)
library(plotly)

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
plot_veg_metric <- function(data, grand_data, metric, y_label, title) {
  
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
  sd_col    <- paste0(metric, "_sd")
  grand_col <- paste0(metric, "_avg_grand")
  
  data <- data %>%
    mutate(
      tooltip_text = paste0(
        "<b>", wetland, "</b>",
        "<br>Year: ", year,
        "<br>Mean: ", round(.data[[avg_col]], 2),
        "<br>SD: ", round(.data[[sd_col]], 2),
        "<br>Sites: ", n_sites
      )
    )
  
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
      aes(text = tooltip_text),
      size = 6,
      position = position_jitter(width = 0.03, height = 0)
    ) +
    
    geom_errorbar(
      aes(ymin = .data[[avg_col]] - .data[[sd_col]],
          ymax = .data[[avg_col]] + .data[[sd_col]]),
      width = 0, alpha = 0.6
    ) +
    
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
      breaks = c("Great Meadow", "Gilmore Meadow"),  # Explicit order
      drop = TRUE
    ) +
    
    scale_shape_manual(
      name = "Wetland Annual Mean",
      values = c(
        "Great Meadow" = 16,
        "Gilmore Meadow" = 17
      ),
      breaks = c("Great Meadow", "Gilmore Meadow"),  # Explicit order
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
      plot.margin = margin(10, 5, 10, 10)
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
  ####       VMMI Section       ####
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
          
          radioButtons("vmmi_summary", "Summarize VMMI Statistics By:",
                       choices = c("Site per Year" = "year",
                                   "Site Averaged Across Years" = "multi",
                                   "Wetland per Year" = "wetland_year",
                                   "Wetland Averaged Across Years" = "all_wetlands"),
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
  
  #--------------------------------#
  ####   Time Series Section    ####
  #--------------------------------#
  
  div(class = "content-section",
      layout_sidebar(
        
        sidebar = sidebar(
          class = "sidebar-custom", width = 300,
          
          h4("Plot Controls", style = "color: #2E7D32;"),
          
          selectInput(
            "ts_metric",
            div(icon("chart-line"), "Select Metric:"),
            choices = c(
              "VMMI" = "vmmi",
              "Mean COC" = "mean.coc",
              "Invasive Cover" = "inv.cov",
              "Bryophyte Cover" = "bryo.cov",
              "Stress Tolerance Cover" = "strtol.cov"
            ),
            selected = "vmmi"
          ),
          
          checkboxGroupInput(
            "ts_wetland",
            div("Select Wetland(s):"),
            choices = c("Great Meadow", "Gilmore Meadow"),
            selected = c("Great Meadow", "Gilmore Meadow")
          ),
          
          br(),
          div(style = "padding: 10px; background-color: #e8f5e9; border-radius: 8px; border-left: 4px solid #2E7D32;",
              p(icon("info-circle"), "Lines show annual means ± SD across sites within each wetland. Dashed lines represent grand means across all years.", 
                style = "margin: 0; font-size: 0.9rem; color: #1b5e20;")),
          
          br(),
          downloadButton("download_plot", "Download Plot",
                         class = "btn-primary btn-sm"),
          
          div(style = "margin-top: 15px; text-align: center;",
              tags$a(href = "#about",
                     class = "btn btn-primary btn-sm", icon("info-circle"),
                     "About"))
        ),
        
        card(
          full_screen = TRUE,
          card_header(class = "bg-primary text-white",
                      "VMMI Metrics Over Time"),
          div(style = "padding: 20px;",
              plotlyOutput("ts_plot", height = "500px"))
        )
      )
  ),
  
  #--------------------------------#
  ####   Species List Section   ####
  #--------------------------------#
  
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
               arrange(Wetland, Year)
           },
           
           "all_wetlands" = {
             # Wetland Averaged Across Years (NEW)
             df %>%
               group_by(wetland) %>%
               summarise(
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
    
    # Add wetland column to vmmi_data based on site names
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
               sd  = ~sd(.x, na.rm = TRUE)),
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
  output$ts_plot <- renderPlotly({
    req(input$ts_metric, input$ts_wetland)
    
    metric_labels <- c(
      "vmmi" = "VMMI",
      "mean.coc" = "Mean COC",
      "inv.cov" = "Invasive Cover (%)",
      "bryo.cov" = "Bryophyte Cover (%)",
      "strtol.cov" = "Stress Tolerance Cover (%)"
    )
    
    p <- plot_veg_metric(
      data = veg_stats(),
      grand_data = veg_grand(),
      metric = input$ts_metric,
      y_label = metric_labels[input$ts_metric],
      title = paste(metric_labels[input$ts_metric], "Over Time")
    )
    
    ggplotly(p, tooltip = "text")
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
  
  # Download plot as PNG
  output$download_plot <- downloadHandler(
    filename = function() {
      metric_names <- c(
        "vmmi" = "VMMI",
        "mean.coc" = "MeanCOC",
        "inv.cov" = "InvasiveCover",
        "bryo.cov" = "BryophyteCover",
        "strtol.cov" = "StressToleranceCover"
      )
      paste0(metric_names[input$ts_metric], "_", Sys.Date(), ".png")
    },
    content = function(file) {
      # Create the plot
      metric_labels <- c(
        "vmmi" = "VMMI",
        "mean.coc" = "Mean COC",
        "inv.cov" = "Invasive Cover (%)",
        "bryo.cov" = "Bryophyte Cover (%)",
        "strtol.cov" = "Stress Tolerance Cover (%)"
      )
      
      p <- plot_veg_metric(
        data = veg_stats(),
        grand_data = veg_grand(),
        metric = input$ts_metric,
        y_label = metric_labels[input$ts_metric],
        title = paste(metric_labels[input$ts_metric], "Over Time")
      )
      
      # Save as PNG with high resolution
      ggsave(file, plot = p, device = "png", 
             width = 10, height = 6, dpi = 300, units = "in")
    }
  )
  
}


# Run app
shinyApp(ui, server)










#### hover plotly --------------------------------------------------------------

#### Wetland Vegetation Dashboard ####

#---------------------------------------------#
####        Load Required Packages         ####
#---------------------------------------------#

library(shiny)
library(tidyverse)
library(shinyWidgets)
library(DT)
library(bslib)
library(plotly)

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

plot_veg_metric <- function(data, grand_data, metric, y_label, title) {
  
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
  sd_col    <- paste0(metric, "_sd")
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
      position = position_jitter(width = 0.03, height = 0)
    ) +
    
    geom_errorbar(
      aes(ymin = .data[[avg_col]] - .data[[sd_col]],
          ymax = .data[[avg_col]] + .data[[sd_col]]),
      width = 0, alpha = 0.6
    ) +
    
    # Grand mean lines - use wetland for color mapping
    geom_hline(
      data = grand_data,
      aes(yintercept = .data[[grand_col]], 
          linetype = grand_label,
          color = wetland),
      linewidth = 1,
      show.legend = c(linetype = TRUE, color = FALSE),
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
      plot.margin = margin(10, 5, 10, 10)
    ) +
    
    guides(
      color = guide_legend(order = 1, override.aes = list(linewidth = 1.2)),
      shape = guide_legend(order = 1),
      linetype = guide_legend(
        order = 2,
        override.aes = list(
          color = grand_colors,
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

# Interactive version with hover tooltips
plot_veg_metric_interactive <- function(data, grand_data, metric, y_label, title) {
  
  # Sort wetlands_in_data to ensure Great Meadow is first
  wetlands_in_data <- unique(as.character(data$wetland))
  wetlands_in_data <- sort(wetlands_in_data, decreasing = TRUE)
  
  # Enforce consistent ordering
  data$wetland <- factor(data$wetland,
                         levels = c("Great Meadow", "Gilmore Meadow"))
  
  grand_data <- grand_data %>%
    filter(wetland %in% wetlands_in_data) %>%
    mutate(wetland = factor(wetland, levels = c("Great Meadow", "Gilmore Meadow")))
  
  avg_col   <- paste0(metric, "_avg")
  sd_col    <- paste0(metric, "_sd")
  n_col     <- paste0(metric, "_n")
  grand_col <- paste0(metric, "_avg_grand")
  
  # Create hover text
  data <- data %>%
    mutate(
      hover_text = paste0(
        "<b>", wetland, "</b><br>",
        "Year: ", year, "<br>",
        "Mean: ", round(.data[[avg_col]], 2), "<br>",
        "SD: ± ", round(.data[[sd_col]], 2), "<br>",
        "n sites: ", .data[[n_col]]
      )
    )
  
  # Create base ggplot with explicit aes mappings to control tooltip
  p <- ggplot(data, aes(x = year, y = .data[[avg_col]],
                        color = wetland, shape = wetland, group = wetland)) +
    
    # Error bars first (so they appear behind points)
    geom_errorbar(
      aes(ymin = .data[[avg_col]] - .data[[sd_col]],
          ymax = .data[[avg_col]] + .data[[sd_col]]),
      width = 0, alpha = 0.6,
      show.legend = FALSE
    ) +
    
    # Lines
    geom_line(linewidth = 1.2, show.legend = TRUE) +
    
    # Points with custom hover text
    geom_point(
      aes(text = hover_text),
      size = 5,
      show.legend = TRUE
    )
  
  # Add grand mean lines
  for (i in seq_len(nrow(grand_data))) {
    wetland_name <- as.character(grand_data$wetland[i])
    grand_value <- grand_data[[grand_col]][i]
    line_color <- if(wetland_name == "Great Meadow") "black" else "grey67"
    
    p <- p + 
      geom_hline(
        yintercept = grand_value,
        linetype = "dashed",
        color = line_color,
        linewidth = 1,
        alpha = 0.7
      )
  }
  
  p <- p +
    scale_x_continuous(
      breaks = seq(min(data$year), max(data$year), by = 1)
    ) +
    
    scale_y_continuous(n.breaks = 8) +
    
    scale_color_manual(
      name = "Wetland",
      values = c(
        "Great Meadow" = "black",
        "Gilmore Meadow" = "grey67"
      )
    ) +
    
    scale_shape_manual(
      name = "Wetland",
      values = c(
        "Great Meadow" = 16,
        "Gilmore Meadow" = 17
      )
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
      axis.title = element_text(size = 14),
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 11),
      legend.position = "right"
    )
  
  # Add VMMI shading if applicable
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
               x = min(data$year), y = 63,
               label = "Good", color = "darkgreen",
               hjust = 0, fontface = "bold") +
      
      annotate("text",
               x = min(data$year), y = 44,
               label = "Fair", color = "goldenrod",
               hjust = 0, fontface = "bold") +
      
      annotate("text",
               x = min(data$year), y = 20,
               label = "Poor", color = "red",
               hjust = 0, fontface = "bold")
  }
  
  # Convert to plotly - only use custom text tooltips
  ggplotly(p, tooltip = "text") %>%
    layout(
      hovermode = "closest",
      hoverlabel = list(
        bgcolor = "white",
        font = list(size = 12, color = "black"),
        bordercolor = "black"
      )
    ) %>%
    style(hoverinfo = "skip", traces = c(1, 2))  # Skip hover on error bars and lines
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
  ####       VMMI Section       ####
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
          
          radioButtons("vmmi_summary", "Summarize VMMI Statistics By:",
                       choices = c("Site per Year" = "year",
                                   "Site Averaged Across Years" = "multi",
                                   "Wetland per Year" = "wetland_year",
                                   "Wetland Averaged Across Years" = "all_wetlands"),
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
  
  #--------------------------------#
  ####   Time Series Section    ####
  #--------------------------------#
  
  div(class = "content-section",
      layout_sidebar(
        
        sidebar = sidebar(
          class = "sidebar-custom", width = 300,
          
          h4("Plot Controls", style = "color: #2E7D32;"),
          
          selectInput(
            "ts_metric",
            div(icon("chart-line"), "Select Metric:"),
            choices = c(
              "VMMI" = "vmmi",
              "Mean COC" = "mean.coc",
              "Invasive Cover" = "inv.cov",
              "Bryophyte Cover" = "bryo.cov",
              "Stress Tolerance Cover" = "strtol.cov"
            ),
            selected = "vmmi"
          ),
          
          checkboxGroupInput(
            "ts_wetland",
            div("Select Wetland(s):"),
            choices = c("Great Meadow", "Gilmore Meadow"),
            selected = c("Great Meadow", "Gilmore Meadow")
          ),
          
          br(),
          div(style = "padding: 10px; background-color: #e8f5e9; border-radius: 8px; border-left: 4px solid #2E7D32;",
              p(icon("info-circle"), "Lines show annual means ± SD across sites within each wetland. Dashed lines represent grand means across all years.", 
                style = "margin: 0; font-size: 0.9rem; color: #1b5e20;")),
          
          br(),
          downloadButton("download_plot", "Download Plot",
                         class = "btn-primary btn-sm"),
          
          div(style = "margin-top: 15px; text-align: center;",
              tags$a(href = "#about",
                     class = "btn btn-primary btn-sm", icon("info-circle"),
                     "About"))
        ),
        
        card(
          full_screen = TRUE,
          card_header(class = "bg-primary text-white",
                      "VMMI Metrics Over Time"),
          div(style = "padding: 20px;",
              plotlyOutput("ts_plot", height = "500px"))
        )
      )
  ),
  
  #--------------------------------#
  ####   Species List Section   ####
  #--------------------------------#
  
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
               arrange(Wetland, Year)
           },
           
           "all_wetlands" = {
             # Wetland Averaged Across Years (NEW)
             df %>%
               group_by(wetland) %>%
               summarise(
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
    
    # Add wetland column to vmmi_data based on site names
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
               sd  = ~sd(.x, na.rm = TRUE),
               n   = ~n()),  # NEW: add n for each metric
          .names = "{.col}_{.fn}"
        ),
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
  
  # Render time series plot (interactive with hover)
  output$ts_plot <- renderPlotly({
    req(input$ts_metric, input$ts_wetland)
    
    metric_labels <- c(
      "vmmi" = "VMMI",
      "mean.coc" = "Mean COC",
      "inv.cov" = "Invasive Cover (%)",
      "bryo.cov" = "Bryophyte Cover (%)",
      "strtol.cov" = "Stress Tolerance Cover (%)"
    )
    
    plot_veg_metric_interactive(
      data = veg_stats(),
      grand_data = veg_grand(),
      metric = input$ts_metric,
      y_label = metric_labels[input$ts_metric],
      title = paste(metric_labels[input$ts_metric], "Over Time")
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
  
  # Download plot as PNG
  output$download_plot <- downloadHandler(
    filename = function() {
      metric_names <- c(
        "vmmi" = "VMMI",
        "mean.coc" = "MeanCOC",
        "inv.cov" = "InvasiveCover",
        "bryo.cov" = "BryophyteCover",
        "strtol.cov" = "StressToleranceCover"
      )
      paste0(metric_names[input$ts_metric], "_", Sys.Date(), ".png")
    },
    content = function(file) {
      # Create the plot
      metric_labels <- c(
        "vmmi" = "VMMI",
        "mean.coc" = "Mean COC",
        "inv.cov" = "Invasive Cover (%)",
        "bryo.cov" = "Bryophyte Cover (%)",
        "strtol.cov" = "Stress Tolerance Cover (%)"
      )
      
      p <- plot_veg_metric(
        data = veg_stats(),
        grand_data = veg_grand(),
        metric = input$ts_metric,
        y_label = metric_labels[input$ts_metric],
        title = paste(metric_labels[input$ts_metric], "Over Time")
      )
      
      # Save as PNG with high resolution
      ggsave(file, plot = p, device = "png", 
             width = 10, height = 6, dpi = 300, units = "in")
    }
  )
  
}


# Run app
shinyApp(ui, server)







#### 5/14/26 --------------------------------------------------------------------

## Option 2 for n=site number - table below the plot

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

plot_veg_metric <- function(data, grand_data, metric, y_label, title) {
  
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
  sd_col    <- paste0(metric, "_sd")
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
      position = position_jitter(width = 0.03, height = 0)
    ) +
    
    geom_errorbar(
      aes(ymin = .data[[avg_col]] - .data[[sd_col]],
          ymax = .data[[avg_col]] + .data[[sd_col]]),
      width = 0, alpha = 0.6
    ) +
    
    # Grand mean lines - use wetland for color mapping
    geom_hline(
      data = grand_data,
      aes(yintercept = .data[[grand_col]], 
          linetype = grand_label,
          color = wetland),
      linewidth = 1,
      show.legend = c(linetype = TRUE, color = FALSE),
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
      plot.margin = margin(10, 5, 10, 10)
    ) +
    
    guides(
      color = guide_legend(order = 1, override.aes = list(linewidth = 1.2)),
      shape = guide_legend(order = 1),
      linetype = guide_legend(
        order = 2,
        override.aes = list(
          color = grand_colors,
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
  ####       VMMI Section       ####
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
          
          radioButtons("vmmi_summary", "Summarize VMMI Statistics By:",
                       choices = c("Site per Year" = "year",
                                   "Site Averaged Across Years" = "multi",
                                   "Wetland per Year" = "wetland_year",
                                   "Wetland Averaged Across Years" = "all_wetlands"),
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
  
  #--------------------------------#
  ####   Time Series Section    ####
  #--------------------------------#
  
  div(class = "content-section",
      layout_sidebar(
        
        sidebar = sidebar(
          class = "sidebar-custom", width = 300,
          
          h4("Plot Controls", style = "color: #2E7D32;"),
          
          selectInput(
            "ts_metric",
            div(icon("chart-line"), "Select Metric:"),
            choices = c(
              "VMMI" = "vmmi",
              "Mean COC" = "mean.coc",
              "Invasive Cover" = "inv.cov",
              "Bryophyte Cover" = "bryo.cov",
              "Stress Tolerance Cover" = "strtol.cov"
            ),
            selected = "vmmi"
          ),
          
          checkboxGroupInput(
            "ts_wetland",
            div("Select Wetland(s):"),
            choices = c("Great Meadow", "Gilmore Meadow"),
            selected = c("Great Meadow", "Gilmore Meadow")
          ),
          
          br(),
          div(style = "padding: 10px; background-color: #e8f5e9; border-radius: 8px; border-left: 4px solid #2E7D32;",
              p(icon("info-circle"), "Lines show annual means ± SD across sites within each wetland. Dashed lines represent grand means across all years.", 
                style = "margin: 0; font-size: 0.9rem; color: #1b5e20;")),
          
          br(),
          downloadButton("download_plot", "Download Plot",
                         class = "btn-primary btn-sm"),
          
          div(style = "margin-top: 15px; text-align: center;",
              tags$a(href = "#about",
                     class = "btn btn-primary btn-sm", icon("info-circle"),
                     "About"))
        ),
        
        card(
          full_screen = TRUE,
          card_header(class = "bg-primary text-white",
                      "VMMI Metrics Over Time"),
          div(style = "padding: 20px;",
              plotOutput("ts_plot", height = "500px")),
          card_body(
            style = "padding: 10px 20px;",
            strong("Number of Sites (n):"),
            tableOutput("ts_sample_size")
          )
        )
      )
  ),
  
  #--------------------------------#
  ####   Species List Section   ####
  #--------------------------------#
  
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
               arrange(Wetland, Year)
           },
           
           "all_wetlands" = {
             # Wetland Averaged Across Years (NEW)
             df %>%
               group_by(wetland) %>%
               summarise(
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
    
    # Add wetland column to vmmi_data based on site names
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
               sd  = ~sd(.x, na.rm = TRUE),
               n   = ~n()),  # NEW: add n for each metric
          .names = "{.col}_{.fn}"
        ),
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
  output$ts_plot <- renderPlot({
    req(input$ts_metric, input$ts_wetland)
    
    metric_labels <- c(
      "vmmi" = "VMMI",
      "mean.coc" = "Mean COC",
      "inv.cov" = "Invasive Cover (%)",
      "bryo.cov" = "Bryophyte Cover (%)",
      "strtol.cov" = "Stress Tolerance Cover (%)"
    )
    
    plot_veg_metric(
      data = veg_stats(),
      grand_data = veg_grand(),
      metric = input$ts_metric,
      y_label = metric_labels[input$ts_metric],
      title = paste(metric_labels[input$ts_metric], "Over Time")
    )
  })
  
  # NEW: Create sample size table
  output$ts_sample_size <- renderTable({
    req(input$ts_metric, input$ts_wetland)
    
    n_col <- paste0(input$ts_metric, "_n")
    
    veg_stats() %>%
      select(wetland, year, n = !!sym(n_col)) %>%
      pivot_wider(
        names_from = year,
        values_from = n,
        names_prefix = ""
      ) %>%
      arrange(desc(wetland))  # Great Meadow first
    
  }, align = 'c', bordered = TRUE, striped = FALSE, spacing = 'xs', width = "auto")
  
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
  
  # Download plot as PNG
  output$download_plot <- downloadHandler(
    filename = function() {
      metric_names <- c(
        "vmmi" = "VMMI",
        "mean.coc" = "MeanCOC",
        "inv.cov" = "InvasiveCover",
        "bryo.cov" = "BryophyteCover",
        "strtol.cov" = "StressToleranceCover"
      )
      paste0(metric_names[input$ts_metric], "_", Sys.Date(), ".png")
    },
    content = function(file) {
      # Create the plot
      metric_labels <- c(
        "vmmi" = "VMMI",
        "mean.coc" = "Mean COC",
        "inv.cov" = "Invasive Cover (%)",
        "bryo.cov" = "Bryophyte Cover (%)",
        "strtol.cov" = "Stress Tolerance Cover (%)"
      )
      
      p <- plot_veg_metric(
        data = veg_stats(),
        grand_data = veg_grand(),
        metric = input$ts_metric,
        y_label = metric_labels[input$ts_metric],
        title = paste(metric_labels[input$ts_metric], "Over Time")
      )
      
      # Save as PNG with high resolution
      ggsave(file, plot = p, device = "png", 
             width = 10, height = 6, dpi = 300, units = "in")
    }
  )
  
}


# Run app
shinyApp(ui, server)



## Option 1 for representing site numbers - n=site number per year on bottom of graph
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

plot_veg_metric <- function(data, grand_data, metric, y_label, title) {
  
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
  sd_col    <- paste0(metric, "_sd")
  grand_col <- paste0(metric, "_avg_grand")
  n_col     <- paste0(metric, "_n")  # NEW: column for sample size
  
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
  
  # NEW: Create annotation data for sample sizes
  # Group by year and create stacked labels
  # NEW: Create annotation data for sample sizes
  n_annotations <- data %>%
    arrange(wetland) %>%
    group_by(year) %>%
    mutate(
      row_num = row_number(),
      y_offset = (row_num - 1) * 0.04  # Stack with consistent spacing
    ) %>%
    ungroup()
  
  # Get y-axis range for positioning
  y_min <- min(data[[avg_col]] - data[[sd_col]], na.rm = TRUE)
  y_max <- max(data[[avg_col]] + data[[sd_col]], na.rm = TRUE)
  y_range <- y_max - y_min
  
  # Position annotations at bottom
  annotation_y_base <- y_min - (0.08 * y_range)
  
  # Create "n=" label (only once, at the left)
  n_label_x <- min(data$year) - 0.3  # Position to the left of first year
  
  p <- ggplot(data, aes(x = year, y = .data[[avg_col]],
                        color = wetland, shape = wetland, group = wetland)) +
    
    geom_line(linewidth = 1.2) +
    
    geom_point(
      size = 6,
      position = position_jitter(width = 0.03, height = 0)
    ) +
    
    geom_errorbar(
      aes(ymin = .data[[avg_col]] - .data[[sd_col]],
          ymax = .data[[avg_col]] + .data[[sd_col]]),
      width = 0, alpha = 0.6
    ) +
    
    # NEW: Add "n=" label once at the left
    annotate(
      "text",
      x = n_label_x,
      y = annotation_y_base,
      label = "n=",
      size = 3.5,
      fontface = "bold",
      hjust = 1
    ) +
    
    # NEW: Add sample size numbers below each year
    geom_text(
      data = n_annotations,
      aes(x = year, 
          y = annotation_y_base - (y_offset * y_range),
          label = .data[[n_col]],
          color = wetland),
      size = 3.5,
      fontface = "bold",
      show.legend = FALSE
    ) +
    
    # Grand mean lines - use wetland for color mapping
    geom_hline(
      data = grand_data,
      aes(yintercept = .data[[grand_col]], 
          linetype = grand_label,
          color = wetland),
      linewidth = 1,
      show.legend = c(linetype = TRUE, color = FALSE),
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
      plot.margin = margin(10, 5, 10, 10)
    ) +
    
    guides(
      color = guide_legend(order = 1, override.aes = list(linewidth = 1.2)),
      shape = guide_legend(order = 1),
      linetype = guide_legend(
        order = 2,
        override.aes = list(
          color = grand_colors,
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
  ####       VMMI Section       ####
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
          
          radioButtons("vmmi_summary", "Summarize VMMI Statistics By:",
                       choices = c("Site per Year" = "year",
                                   "Site Averaged Across Years" = "multi",
                                   "Wetland per Year" = "wetland_year",
                                   "Wetland Averaged Across Years" = "all_wetlands"),
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
  
  #--------------------------------#
  ####   Time Series Section    ####
  #--------------------------------#
  
  div(class = "content-section",
      layout_sidebar(
        
        sidebar = sidebar(
          class = "sidebar-custom", width = 300,
          
          h4("Plot Controls", style = "color: #2E7D32;"),
          
          selectInput(
            "ts_metric",
            div(icon("chart-line"), "Select Metric:"),
            choices = c(
              "VMMI" = "vmmi",
              "Mean COC" = "mean.coc",
              "Invasive Cover" = "inv.cov",
              "Bryophyte Cover" = "bryo.cov",
              "Stress Tolerance Cover" = "strtol.cov"
            ),
            selected = "vmmi"
          ),
          
          checkboxGroupInput(
            "ts_wetland",
            div("Select Wetland(s):"),
            choices = c("Great Meadow", "Gilmore Meadow"),
            selected = c("Great Meadow", "Gilmore Meadow")
          ),
          
          br(),
          div(style = "padding: 10px; background-color: #e8f5e9; border-radius: 8px; border-left: 4px solid #2E7D32;",
              p(icon("info-circle"), "Lines show annual means ± SD across sites within each wetland. Dashed lines represent grand means across all years.", 
                style = "margin: 0; font-size: 0.9rem; color: #1b5e20;")),
          
          br(),
          downloadButton("download_plot", "Download Plot",
                         class = "btn-primary btn-sm"),
          
          div(style = "margin-top: 15px; text-align: center;",
              tags$a(href = "#about",
                     class = "btn btn-primary btn-sm", icon("info-circle"),
                     "About"))
        ),
        
        card(
          full_screen = TRUE,
          card_header(class = "bg-primary text-white",
                      "VMMI Metrics Over Time"),
          div(style = "padding: 20px;",
              plotOutput("ts_plot", height = "500px"))
        )
      )
  ),
  
  #--------------------------------#
  ####   Species List Section   ####
  #--------------------------------#
  
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
               arrange(Wetland, Year)
           },
           
           "all_wetlands" = {
             # Wetland Averaged Across Years (NEW)
             df %>%
               group_by(wetland) %>%
               summarise(
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
    
    # Add wetland column to vmmi_data based on site names
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
               sd  = ~sd(.x, na.rm = TRUE),
               n   = ~n()),  # NEW: add n for each metric
          .names = "{.col}_{.fn}"
        ),
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
  output$ts_plot <- renderPlot({
    req(input$ts_metric, input$ts_wetland)
    
    metric_labels <- c(
      "vmmi" = "VMMI",
      "mean.coc" = "Mean COC",
      "inv.cov" = "Invasive Cover (%)",
      "bryo.cov" = "Bryophyte Cover (%)",
      "strtol.cov" = "Stress Tolerance Cover (%)"
    )
    
    plot_veg_metric(
      data = veg_stats(),
      grand_data = veg_grand(),
      metric = input$ts_metric,
      y_label = metric_labels[input$ts_metric],
      title = paste(metric_labels[input$ts_metric], "Over Time")
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
  
  # Download plot as PNG
  output$download_plot <- downloadHandler(
    filename = function() {
      metric_names <- c(
        "vmmi" = "VMMI",
        "mean.coc" = "MeanCOC",
        "inv.cov" = "InvasiveCover",
        "bryo.cov" = "BryophyteCover",
        "strtol.cov" = "StressToleranceCover"
      )
      paste0(metric_names[input$ts_metric], "_", Sys.Date(), ".png")
    },
    content = function(file) {
      # Create the plot
      metric_labels <- c(
        "vmmi" = "VMMI",
        "mean.coc" = "Mean COC",
        "inv.cov" = "Invasive Cover (%)",
        "bryo.cov" = "Bryophyte Cover (%)",
        "strtol.cov" = "Stress Tolerance Cover (%)"
      )
      
      p <- plot_veg_metric(
        data = veg_stats(),
        grand_data = veg_grand(),
        metric = input$ts_metric,
        y_label = metric_labels[input$ts_metric],
        title = paste(metric_labels[input$ts_metric], "Over Time")
      )
      
      # Save as PNG with high resolution
      ggsave(file, plot = p, device = "png", 
             width = 10, height = 6, dpi = 300, units = "in")
    }
  )
  
}


# Run app
shinyApp(ui, server)












#### 5/14/26 -------------------------------------------------------------------
## pre wetland level summarizing options for vmmi table stats

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

# Time series plotting function
plot_veg_metric <- function(data, grand_data, metric, y_label, title) {
  
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
  sd_col    <- paste0(metric, "_sd")
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
      position = position_jitter(width = 0.03, height = 0)
    ) +
    
    geom_errorbar(
      aes(ymin = .data[[avg_col]] - .data[[sd_col]],
          ymax = .data[[avg_col]] + .data[[sd_col]]),
      width = 0, alpha = 0.6
    ) +
    
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
      breaks = c("Great Meadow", "Gilmore Meadow"),  # Explicit order
      drop = TRUE
    ) +
    
    scale_shape_manual(
      name = "Wetland Annual Mean",
      values = c(
        "Great Meadow" = 16,
        "Gilmore Meadow" = 17
      ),
      breaks = c("Great Meadow", "Gilmore Meadow"),  # Explicit order
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
      plot.margin = margin(10, 5, 10, 10)
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
  ####       VMMI Section       ####
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
  
  #--------------------------------#
  ####   Time Series Section    ####
  #--------------------------------#
  
  div(class = "content-section",
      layout_sidebar(
        
        sidebar = sidebar(
          class = "sidebar-custom", width = 300,
          
          h4("Plot Controls", style = "color: #2E7D32;"),
          
          selectInput(
            "ts_metric",
            div(icon("chart-line"), "Select Metric:"),
            choices = c(
              "VMMI" = "vmmi",
              "Mean COC" = "mean.coc",
              "Invasive Cover" = "inv.cov",
              "Bryophyte Cover" = "bryo.cov",
              "Stress Tolerance Cover" = "strtol.cov"
            ),
            selected = "vmmi"
          ),
          
          checkboxGroupInput(
            "ts_wetland",
            div("Select Wetland(s):"),
            choices = c("Great Meadow", "Gilmore Meadow"),
            selected = c("Great Meadow", "Gilmore Meadow")
          ),
          
          br(),
          div(style = "padding: 10px; background-color: #e8f5e9; border-radius: 8px; border-left: 4px solid #2E7D32;",
              p(icon("info-circle"), "Lines show annual means ± SD across sites within each wetland. Dashed lines represent grand means across all years.", 
                style = "margin: 0; font-size: 0.9rem; color: #1b5e20;")),
          
          br(),
          downloadButton("download_plot", "Download Plot",
                         class = "btn-primary btn-sm"),
          
          div(style = "margin-top: 15px; text-align: center;",
              tags$a(href = "#about",
                     class = "btn btn-primary btn-sm", icon("info-circle"),
                     "About"))
        ),
        
        card(
          full_screen = TRUE,
          card_header(class = "bg-primary text-white",
                      "VMMI Metrics Over Time"),
          div(style = "padding: 20px;",
              plotOutput("ts_plot", height = "500px"))
        )
      )
  ),
  
  #--------------------------------#
  ####   Species List Section   ####
  #--------------------------------#
  
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
  
  #-------------------------------#
  ####  Time Series Processing ####
  #-------------------------------#
  
  # Compute statistics for time series
  veg_stats <- reactive({
    req(input$ts_wetland)
    
    # Add wetland column to vmmi_data based on site names
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
               sd  = ~sd(.x, na.rm = TRUE)),
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
  output$ts_plot <- renderPlot({
    req(input$ts_metric, input$ts_wetland)
    
    metric_labels <- c(
      "vmmi" = "VMMI",
      "mean.coc" = "Mean COC",
      "inv.cov" = "Invasive Cover (%)",
      "bryo.cov" = "Bryophyte Cover (%)",
      "strtol.cov" = "Stress Tolerance Cover (%)"
    )
    
    plot_veg_metric(
      data = veg_stats(),
      grand_data = veg_grand(),
      metric = input$ts_metric,
      y_label = metric_labels[input$ts_metric],
      title = paste(metric_labels[input$ts_metric], "Over Time")
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
  
  # Download plot as PNG
  output$download_plot <- downloadHandler(
    filename = function() {
      metric_names <- c(
        "vmmi" = "VMMI",
        "mean.coc" = "MeanCOC",
        "inv.cov" = "InvasiveCover",
        "bryo.cov" = "BryophyteCover",
        "strtol.cov" = "StressToleranceCover"
      )
      paste0(metric_names[input$ts_metric], "_", Sys.Date(), ".png")
    },
    content = function(file) {
      # Create the plot
      metric_labels <- c(
        "vmmi" = "VMMI",
        "mean.coc" = "Mean COC",
        "inv.cov" = "Invasive Cover (%)",
        "bryo.cov" = "Bryophyte Cover (%)",
        "strtol.cov" = "Stress Tolerance Cover (%)"
      )
      
      p <- plot_veg_metric(
        data = veg_stats(),
        grand_data = veg_grand(),
        metric = input$ts_metric,
        y_label = metric_labels[input$ts_metric],
        title = paste(metric_labels[input$ts_metric], "Over Time")
      )
      
      # Save as PNG with high resolution
      ggsave(file, plot = p, device = "png", 
             width = 10, height = 6, dpi = 300, units = "in")
    }
  )
  
}


# Run app
shinyApp(ui, server)







####----------------------------------------------------------------------------


## time series plots copy

# Time series plotting function
plot_veg_metric <- function(data, grand_data, metric, y_label, title) {
  
  data$wetland <- factor(data$wetland,
                         levels = c("Great Meadow", "Gilmore Meadow"))
  grand_data$wetland <- factor(grand_data$wetland,
                               levels = c("Great Meadow", "Gilmore Meadow"))
  
  avg_col   <- paste0(metric, "_avg")
  sd_col    <- paste0(metric, "_sd")
  grand_col <- paste0(metric, "_avg_grand")
  
  # Create grand mean labels with values
  gm_grand <- grand_data %>% filter(wetland == "Great Meadow") %>% pull(!!sym(grand_col))
  gl_grand <- grand_data %>% filter(wetland == "Gilmore Meadow") %>% pull(!!sym(grand_col))
  
  # Create a combined factor for the legend
  grand_data <- grand_data %>%
    mutate(grand_label = factor(
      paste0(wetland, " Grand Mean"),
      levels = c("Great Meadow Grand Mean", "Gilmore Meadow Grand Mean")
    ))
  
  p <- ggplot(data, aes(x = year, y = .data[[avg_col]],
                        color = wetland, shape = wetland, group = wetland)) +
    
    geom_line(linewidth = 1.2) +
    
    geom_point(
      size = 6,
      position = position_jitter(width = 0.03, height = 0)
    ) +
    
    geom_errorbar(
      aes(ymin = .data[[avg_col]] - .data[[sd_col]],
          ymax = .data[[avg_col]] + .data[[sd_col]]),
      width = 0, alpha = 0.6
    ) +
    
    # Grand mean lines with separate mapping
    geom_hline(
      data = grand_data,
      aes(yintercept = .data[[grand_col]], 
          linetype = grand_label,
          color = wetland),
      linewidth = 1,
      show.legend = TRUE,
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
      breaks = c("Great Meadow", "Gilmore Meadow")
    ) +
    
    scale_shape_manual(
      name = "Wetland Annual Mean",
      values = c(
        "Great Meadow" = 16,
        "Gilmore Meadow" = 17
      )
    ) +
    
    scale_linetype_manual(
      name = "Wetland Grand Mean",
      values = c(
        "Great Meadow Grand Mean" = "dashed",
        "Gilmore Meadow Grand Mean" = "dashed"
      ),
      labels = c(
        "Great Meadow Grand Mean" = sprintf("Great Meadow (%.2f)", gm_grand),
        "Gilmore Meadow Grand Mean" = sprintf("Gilmore Meadow (%.2f)", gl_grand)
      )
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
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 11),
      legend.key.size = unit(1.2, "cm"),
      legend.spacing.y = unit(0.2, "cm"),
      legend.box = "vertical",
      plot.margin = margin(10, 10, 10, 10)
    ) +
    
    guides(
      color = guide_legend(order = 1, override.aes = list(linewidth = 1.2)),
      shape = guide_legend(order = 1),
      linetype = guide_legend(
        order = 2, 
        override.aes = list(
          color = c("black", "grey67"),
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
               hjust = 0) +
      
      annotate("text",
               x = min(data$year),
               y = 44,
               label = "Fair",
               color = "goldenrod",
               hjust = 0) +
      
      annotate("text",
               x = min(data$year),
               y = 20,
               label = "Poor",
               color = "red",
               hjust = 0)
  }
  
  return(p)
}




####5/11/26 ####----------------------------------------------------------------

## NEW dashboard with plots section addition


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

# Time series plotting function
plot_veg_metric <- function(data, grand_data, metric, y_label, title) {
  
  data$wetland <- factor(data$wetland,
                         levels = c("Great Meadow", "Gilmore Meadow"))
  grand_data$wetland <- factor(grand_data$wetland,
                               levels = c("Great Meadow", "Gilmore Meadow"))
  
  avg_col   <- paste0(metric, "_avg")
  sd_col    <- paste0(metric, "_sd")
  grand_col <- paste0(metric, "_avg_grand")
  
  p <- ggplot(data, aes(x = year, y = .data[[avg_col]],
                        color = wetland, shape = wetland, group = wetland)) +
    
    geom_line(linewidth = 1.2) +
    
    geom_point(size = 4) +
    
    geom_errorbar(
      aes(ymin = .data[[avg_col]] - .data[[sd_col]],
          ymax = .data[[avg_col]] + .data[[sd_col]]),
      width = 0, alpha = 0.6
    ) +
    
    geom_hline(
      data = grand_data,
      aes(yintercept = .data[[grand_col]], color = wetland),
      linetype = "dashed",
      linewidth = 1,
      show.legend = FALSE
    ) +
    
    scale_x_continuous(
      breaks = seq(min(data$year), max(data$year), by = 1)
    ) +
    
    scale_color_manual(values = c(
      "Great Meadow" = "black",
      "Gilmore Meadow" = "grey67"
    )) +
    
    scale_shape_manual(values = c(
      "Great Meadow" = 16,
      "Gilmore Meadow" = 17
    )) +
    
    labs(
      title = title,
      x = "Year",
      y = y_label,
      color = "Wetland",
      shape = "Wetland"
    ) +
    
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      legend.position = "bottom"
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
               hjust = 0) +
      
      annotate("text",
               x = min(data$year),
               y = 44,
               label = "Fair",
               color = "goldenrod",
               hjust = 0) +
      
      annotate("text",
               x = min(data$year),
               y = 20,
               label = "Poor",
               color = "red",
               hjust = 0)
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
  ####       VMMI Section       ####
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
  
  #--------------------------------#
  ####   Time Series Section    ####
  #--------------------------------#
  
  div(class = "content-section",
      layout_sidebar(
        
        sidebar = sidebar(
          class = "sidebar-custom", width = 300,
          
          h4("Time Series Controls", style = "color: #2E7D32;"),
          
          selectInput(
            "ts_metric",
            div(icon("chart-line"), "Select Metric:"),
            choices = c(
              "VMMI" = "vmmi",
              "Mean Coefficient of Conservatism" = "mean.coc",
              "Invasive Cover" = "inv.cov",
              "Bryophyte Cover" = "bryo.cov",
              "Stress Tolerance Cover" = "strtol.cov"
            ),
            selected = "vmmi"
          ),
          
          checkboxGroupInput(
            "ts_wetland",
            div(icon("water"), "Select Wetland(s):"),
            choices = c("Great Meadow", "Gilmore Meadow"),
            selected = c("Great Meadow", "Gilmore Meadow")
          ),
          
          hr(),
          
          tags$p(
            style = "font-size: 0.9em; color: #6c757d;",
            "Lines show annual means ± SD across sites within each wetland. 
            Dashed horizontal lines indicate grand means across all years."
          ),
          
          div(style = "margin-top: 15px; text-align: center;",
              tags$a(href = "#about",
                     class = "btn btn-primary btn-sm", icon("info-circle"),
                     "About"))
        ),
        
        card(
          full_screen = TRUE,
          card_header(class = "bg-primary text-white",
                      "VMMI Statistics Over Time"),
          div(style = "padding: 20px;",
              plotOutput("ts_plot", height = "500px"))
        )
      )
  ),
  
  #--------------------------------#
  ####   Species List Section   ####
  #--------------------------------#
  
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
  
  #-------------------------------#
  ####  Time Series Processing ####
  #-------------------------------#
  
  # Compute statistics for time series
  veg_stats <- reactive({
    req(input$ts_wetland)
    
    # Add wetland column to vmmi_data based on site names
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
               sd  = ~sd(.x, na.rm = TRUE)),
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
  output$ts_plot <- renderPlot({
    req(input$ts_metric, input$ts_wetland)
    
    metric_labels <- c(
      "vmmi" = "VMMI",
      "mean.coc" = "Mean Coefficient of Conservatism",
      "inv.cov" = "Invasive Cover (%)",
      "bryo.cov" = "Bryophyte Cover (%)",
      "strtol.cov" = "Stress Tolerance Cover (%)"
    )
    
    plot_veg_metric(
      data = veg_stats(),
      grand_data = veg_grand(),
      metric = input$ts_metric,
      y_label = metric_labels[input$ts_metric],
      title = paste(metric_labels[input$ts_metric], "Over Time")
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








#### 5/11/26 ####---------------------------------------------------------------

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









## option to change display latin name of glossy buckthorn from "Rhamnus frangula" to "Frangula alnus"

# species lists
species_data <- read.csv("data/vis_FOA_NETN_spplist_2011_2025_20260324.csv") %>%
  filter(!str_detect(latin.name, regex("unknown", ignore_case = TRUE))) %>% 
  mutate(
    latin.name = if_else(latin.name == "Rhamnus frangula", "Frangula alnus", latin.name))