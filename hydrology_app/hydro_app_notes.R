# 4/3/26 -----------------------------------------------------------------------

#### Hydrology R Shiny Dashboard #### 

#---------------------------------------------#
####        Load Required Packages         ####
#---------------------------------------------#

library(shiny)
library(tidyverse)
library(lubridate)
library(patchwork)
library(shinyWidgets)
library(DT)
library(bslib)

#-------------------------------------------#
####    Read & Prepare Processed Data    ####
#-------------------------------------------#

# For data exploration, app building, or debugging, temporarily set working directory to app folder
# setwd("hydrology_app")

# Great Meadow data
gm <- read.csv("data/great_meadow_well_data_2025_20260304.csv") %>%
  mutate(date = as.Date(date),
         timestamp = as_datetime(timestamp),
         site = paste("Great Meadow", plot.num),
         water.depth = case_when(
           year == 2016 & doy_h == 159.12 & plot.num == 3 & water.depth < -120 ~ NA_real_,
           year == 2017 & doy_h == 215.02 & plot.num == 6 & water.depth < -115 ~ NA_real_,
           year == 2021 & plot.num == 3 & doy == 224 & water.depth > 400 ~ NA_real_,
           year == 2021 & plot.num == 3 & doy == 225 & water.depth > 400 ~ NA_real_,
           TRUE ~ water.depth
         ),
         siteyear = paste(site, year, sep = "_"))

# Gilmore Meadow data 
gl <- read.csv("data/gl_well_data_2025_20260127.csv") %>%
  mutate(date = as.Date(date),
         timestamp = as_datetime(timestamp),
         site = "Gilmore Meadow 1")

# Create precipitation lookup and combine datasets
gm_precip_lookup <- gm %>% select(timestamp, precip.cm, lag.precip) %>% distinct()

gl_with_gm_precip <- gl %>%
  select(-precip.cm, -lag.precip) %>%
  left_join(gm_precip_lookup, by = "timestamp") %>%
  left_join(gl %>% select(timestamp, orig_precip_cm = precip.cm, orig_lag_precip = lag.precip), 
            by = "timestamp") %>%
  mutate(precip.cm = coalesce(precip.cm, orig_precip_cm),
         lag.precip = coalesce(lag.precip, orig_lag_precip)) %>%
  select(-orig_precip_cm, -orig_lag_precip)

# Combine datasets
all_data <- bind_rows(gm, gl_with_gm_precip) %>% 
  filter(year >= 2016 & year(Sys.Date())) %>% 
  select(timestamp, date, year, doy, hr, doy_h, precip_cm = precip.cm,
         lag_precip = lag.precip, water_depth = water.depth, site)

# Water level stats
wl_stats <- read.csv("data/gm_gl_wl_stats_2025_20260304.csv") %>% 
  select(year, stat, `Gilmore Meadow 1` = gilmore.meadow, 
         `Great Meadow 1` = great.meadow.1, `Great Meadow 2` = great.meadow.2, 
         `Great Meadow 3` = great.meadow.3, `Great Meadow 4` = great.meadow.4, 
         `Great Meadow 5` = great.meadow.5, `Great Meadow 6` = great.meadow.6) %>% 
  pivot_longer(cols = -c(year, stat), names_to = "site", values_to = "value") %>% 
  pivot_wider(names_from = stat, values_from = value) %>%
  arrange(site, year) %>% 
  mutate(wetland = if_else(grepl("Great Meadow", site), "Great Meadow", "Gilmore Meadow"))

#-----------------------#
####    Constants    ####
#-----------------------#

# Site color palette (defined once)
SITE_COLORS <- c(
  "Great Meadow 1" = "black", "Great Meadow 2" = "chartreuse4", "Great Meadow 3" = "green",
  "Great Meadow 4" = "darkorange", "Great Meadow 5" = "deeppink2", "Great Meadow 6" = "purple",
  "Gilmore Meadow 1" = "darkgray",   "Great Meadow (Average)" = "darkgreen",
  "Gilmore Meadow (Average)" = "navy", "Precipitation" = "blue"
)

# Variable name mapping for significance testing
VAR_MAPPING <- c(
  "WL_mean" = "Mean Water Level (cm)",
  "WL_sd" = "SD Water Level (cm)", 
  "WL_min" = "Minimum Water Level (cm)",
  "WL_max" = "Maximum Water Level (cm)",
  "max_inc" = "Maximum Hourly Increase (cm)",
  "max_dec" = "Maximum Hourly Decrease (cm)",
  "GS_change" = "Growing Season Change (cm)",
  "prop_over_0cm" = "GS % Surface Water",
  "prop_bet_0_neg30cm" = "GS % Within 30cm",
  "prop_under_neg30cm" = "GS % Over 30cm Deep"
)

# Common pickerInput options
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

# hydrograph plot creation function
create_hydrograph_plot <- function(data) {
  req(nrow(data) > 0)
  
  minWL <- min(data$water_depth, na.rm = TRUE)
  sites <- unique(data$site)
  
  ggplot(data, aes(x = doy_h, y = water_depth)) +
    geom_line(aes(color = site), size = 0.7) +
    geom_line(aes(x = doy_h, y = lag_precip * 5 + minWL, color = "Precipitation"), size = 0.7) +
    geom_hline(yintercept = 0, color = 'brown') +
    facet_wrap(~ year, ncol = 1) +
    scale_color_manual(values = SITE_COLORS, breaks = c(sites, "Precipitation")) +
    labs(y = 'Water Level (cm)', x = 'Date') +
    scale_x_continuous(
      breaks = c(121, 152, 182, 213, 244, 274),
      labels = c('May-01', 'Jun-01', 'Jul-01', 'Aug-01', 'Sep-01', 'Oct-01')
    ) +
    scale_y_continuous(
      sec.axis = sec_axis(~ ., breaks = c(minWL, minWL + 10),
                          name = 'Hourly Precip. (cm)', labels = c('0', '2'))
    ) +
    theme_bw() +
    theme(
      plot.title = element_text(hjust = 0.5),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      axis.text.y.right = element_text(color = 'blue'),
      axis.title.y.right = element_text(color = 'blue'),
      strip.text = element_text(size = 11),
      legend.position = "bottom",
      legend.title = element_blank()
    )
}

# statistical significance function
calculate_wetland_significance <- function(data, selected_years, selected_sites, alpha = 0.05) {
  if (length(selected_years) <= 3) {
    message("⚠️ Not enough years selected for significance testing (need >3).")
    return(NULL)
  }
  
  filtered_data <- data %>%
    filter(year %in% selected_years, site %in% selected_sites) %>%
    mutate(site_group = if_else(grepl("Great Meadow", site), "Great Meadow", "Gilmore Meadow"))
  
  wetlands_present <- unique(filtered_data$site_group)
  if (length(wetlands_present) < 2 || !all(c("Great Meadow", "Gilmore Meadow") %in% wetlands_present)) {
    message("⚠️ Both Great Meadow and Gilmore Meadow must be present for comparison.")
    return(NULL)
  }
  
  stat_cols <- filtered_data %>% select(where(is.numeric), -year) %>% names()
  
  yearly_means <- filtered_data %>%
    group_by(year, site_group) %>%
    summarise(across(all_of(stat_cols), mean, na.rm = TRUE), .groups = "drop")
  
  map_dfr(stat_cols, function(var) {
    tryCatch({
      test <- t.test(as.formula(paste(var, "~ site_group")), data = yearly_means)
      data.frame(variable = var, p_value = test$p.value, significant = test$p.value < alpha)
    }, error = function(e) {
      data.frame(variable = var, p_value = NA, significant = FALSE)
    })
  })
}

# picker tool function
create_picker_input <- function(id, label, choices, selected, multiple = TRUE, none_text = "Choose options") {
  pickerInput(id, label = div(icon(if(id %in% c("selected_sites", "stats_site")) "map-marker" else "calendar"), label),
              choices = choices, selected = selected, multiple = multiple,
              options = c(PICKER_OPTIONS, list(`none-selected-text` = none_text)))
}

#----------------#
####    UI    ####
#----------------#

ui <- page_fluid(
  theme = bs_theme(
    version = 5, bootswatch = "flatly", primary = "#1B365D", secondary = "#4C6D9A",    
    success = "#2E86C1", info = "#3498db", warning = "#f39c12", danger = "#e74c3c",
    base_font = font_google("Open Sans"), heading_font = font_google("Open Sans", wght = c(400, 700))
  ),
  
  # Custom CSS
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
        box-shadow: 0 2px 8px rgba(0,0,0,0.08); border: 1px solid #dee2e6;
      }
      .main-title {
        background: linear-gradient(135deg, #1B365D 0%, #4C6D9A 100%);
        color: white; padding: 30px; margin: -15px -15px 30px -15px;
        text-align: center; border-radius: 0 0 20px 20px;
        box-shadow: 0 4px 12px rgba(0,0,0,0.15);
      }
      .brush-info-section {
        background: linear-gradient(135deg, #e8f4f8 0%, #f0f8ff 100%);
        border-radius: 12px; padding: 20px; margin: 20px 0;
        border: 1px solid #4C6D9A;
      }
      .dataTables_wrapper { font-size: 0.85rem !important; }
      .dataTables_wrapper table { font-size: 0.8rem !important; }
      .significance-info h5 {
        background-color: #fff3cd; color: #856404;
        padding: 6px 10px; border-radius: 4px;
        display: inline-block; margin-bottom: 10px;
      }
    "))
  ),
  
  # Main title
  div(class = "main-title",
      h1("Wetland Hydrology Dashboard", 
         style = "margin: 0; font-size: 2rem; text-shadow: 2px 2px 4px rgba(0,0,0,0.3)")
  ),
  
  # FIRST SECTION: Hydrographs section
  div(class = "content-section",
      layout_sidebar(
        sidebar = sidebar(
          class = "sidebar-custom", width = 300,
          h4("Hydrograph Controls", style = "color: #1B365D; margin-bottom: 20px;"),
          
          create_picker_input("selected_sites", "Select Site(s):", 
                              c(unique(all_data$site),
                                "Great Meadow (Average)",
                                "Gilmore Meadow (Average)"), 
                              "Great Meadow 1", 
                              none_text = "Choose site(s)"),
          
          create_picker_input("year", "Select Year(s):", 
                              unique(all_data$year), 
                              selected = max(all_data$year),
                              none_text = "Choose year(s)"),
          
          br(),
          div(style = "padding: 10px; background-color: #e8f4f8; border-radius: 8px; border-left: 4px solid #3498db;",
              p(icon("info-circle"), " Use the brush tool (+) by clicking and dragging with your cursor to select data on the hydrograph and view below.", 
                style = "margin: 0; font-size: 0.9rem; color: #2c3e50;")),
          
          div(style = "margin-top: 10px; text-align: center;",
              downloadButton("download_plot", "Download Hydrograph", 
                             class = "btn-primary btn-sm", icon = icon("image"))),
          
          div(style = "margin-top: 15px; text-align: center;",
              downloadButton("download_brush", "Download Selected Data", 
                             class = "btn-primary btn-sm", icon = icon("download"))),
          
          div(style = "margin-top: 15px; text-align: center;",
              tags$a(href = "#about",
                     class = "btn btn-primary btn-sm", icon("info-circle"),
                     "About")),
        ),
        
        card(
          full_screen = TRUE,
          card_header(class = "bg-primary text-white", "Hydrographs by Year"),
          plotOutput("hydrograph", height = "600px", 
                     brush = brushOpts(id = "hydro_brush", fill = "#4C6D9A", opacity = 0.3))
        )
      )
  ),
  
  # Selected data section
  div(class = "brush-info-section",
      card(
        card_header(class = "bg-success text-white", "Selected Data from Hydrograph:"),
        tableOutput("brush_info")
      )
  ),
  
  # SECOND SECTION: Water Level Stats
  div(class = "content-section",
      layout_sidebar(
        sidebar = sidebar(
          class = "sidebar-custom", width = 300,
          h4("Statistics Controls", style = "color: #1B365D; margin-bottom: 20px;"),
          
          create_picker_input("stats_site", "Select Site(s):", 
                              unique(wl_stats$site), 
                              c("Great Meadow 1", "Gilmore Meadow 1"),
                              none_text = "Choose site(s)"),
          
          create_picker_input("stats_year", "Select Years:", 
                              unique(wl_stats$year), 
                              selected = tail(sort(unique(wl_stats$year)), 4),
                              none_text = "Choose year(s)"),
          
          radioButtons("time_summary", "Summarize Water Level Statistics By:",
                       choices = c("Each Year" = "year",
                                   "Average Across Years" = "multi",
                                   "All Sites (with statistical significance)" = "all_sites"),
                       selected = "year"),
          
          br(),
          div(style = "padding: 10px; background-color: #e8f4f8; border-radius: 8px; border-left: 4px solid #3498db;",
              p(icon("info-circle"), " Growing season statistics calculated from May through October.", 
                style = "margin: 0; font-size: 0.9rem; color: #2c3e50;")),
          
          div(style = "margin-top: 15px; text-align: center;",
              downloadButton("download_stats", "Download Table", 
                             class = "btn-primary btn-sm", icon = icon("download"))),
          
          div(style = "margin-top: 15px; text-align: center;",
              tags$a(href = "#about",
                     class = "btn btn-primary btn-sm", icon("info-circle"),
                     "About")),
        ),
        
        card(
          full_screen = TRUE,
          card_header(class = "bg-primary text-white", "Growing Season Water Level Statistics"),
          div(style = "padding: 10px;",
              uiOutput("significance_info"),
              dataTableOutput("wl_stats"))
        )
      )
  ),
  
  # About section
  div(id = "about",
      class = "brush-info-section",
      card(
        card_header(class = "bg-success text-white", "About"),
        includeHTML("./www/About.html")
      )
  )
  
)


#--------------------#
####    SERVER    ####
#--------------------#

server <- function(input, output, session) {
  
  # Reactive data for plotting
  plot_data <- reactive({
    req(input$year, input$selected_sites)
    
    data <- all_data %>%
      filter(year %in% input$year, doy > 134, doy < 275)
    
    selected <- input$selected_sites
    
    # ---- Separate selections ----
    real_sites <- selected[selected %in% unique(all_data$site)]
    avg_sites  <- selected[grepl("\\(Average\\)", selected)]
    
    # ---- Real site data ----
    real_data <- data %>%
      filter(site %in% real_sites)
    
    # ---- Average data ----
    avg_data <- map_dfr(avg_sites, function(avg_name) {
      
      wetland_name <- if (grepl("Great Meadow", avg_name)) {
        "Great Meadow"
      } else {
        "Gilmore Meadow"
      }
      
      data %>%
        filter(grepl(wetland_name, site)) %>%
        group_by(year, doy_h, timestamp) %>%
        summarise(
          water_depth = mean(water_depth, na.rm = TRUE),
          lag_precip = mean(lag_precip, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(site = avg_name)
    })
    
    bind_rows(real_data, avg_data)
  })
  
  # Render hydrograph plot
  output$hydrograph <- renderPlot({
    create_hydrograph_plot(plot_data())
  })
  
  # Processed brushed data
  processed_brush_data <- reactive({
    req(input$hydro_brush)
    plot_data_filtered <- plot_data()
    req(nrow(plot_data_filtered) > 0)
    
    selected_data <- brushedPoints(plot_data_filtered, brush = input$hydro_brush,
                                   xvar = "doy_h", yvar = "water_depth") %>%
      select(timestamp, year, doy_h, site, water_depth, lag_precip) %>%
      arrange(year, doy_h) %>%
      mutate(across(c(doy_h, water_depth), ~ round(.x, 2)),
             lag_precip = round(lag_precip, 3),
             timestamp = format(timestamp, "%Y-%m-%d %H:%M:%S"))
    
    if (nrow(selected_data) == 0) {
      return(data.frame(Message = "No points selected - try brushing over the water level lines"))
    }
    
    # Create base data and pivot water depths by site
    base_data <- selected_data %>%
      group_by(year, doy_h, timestamp, lag_precip) %>%
      summarise(.groups = 'drop') %>%
      arrange(year, doy_h)
    
    water_depth_data <- selected_data %>%
      group_by(year, doy_h, timestamp, site) %>%
      summarise(water_depth = mean(water_depth, na.rm = TRUE), .groups = 'drop') %>%
      pivot_wider(names_from = site, values_from = water_depth)
    
    result_data <- base_data %>%
      left_join(water_depth_data, by = c("year", "doy_h", "timestamp")) %>%
      rename(Year = year, Timestamp = timestamp, `Day of Year` = doy_h, 
             `Precipitation (cm)` = lag_precip)
    
    # Add "Water Depth (cm)" suffix to site columns
    site_columns <- intersect(names(result_data), input$selected_sites)
    if (length(site_columns) > 0) {
      result_data <- result_data %>%
        rename_with(~ paste(.x, "Water Depth (cm)"), .cols = all_of(site_columns))
    }
    
    result_data %>% arrange(Year, `Day of Year`)
  })
  
  # Significance testing results
  significance_results <- reactive({
    req(input$stats_year, input$stats_site)
    selected_wetlands <- wl_stats %>%
      filter(site %in% input$stats_site) %>%
      pull(wetland) %>%
      unique()
    
    if (length(selected_wetlands) >= 2 && all(c("Great Meadow", "Gilmore Meadow") %in% selected_wetlands)) {
      calculate_wetland_significance(wl_stats, input$stats_year, input$stats_site, alpha = 0.05)
    } else {
      NULL
    }
  })
  
  # Check if we should show significance info
  show_significance_info <- reactive({
    req(input$stats_site, input$time_summary)
    if (input$time_summary != "all_sites") return(FALSE)
    
    selected_wetlands <- wl_stats %>%
      filter(site %in% input$stats_site) %>%
      pull(wetland) %>%
      unique()
    
    length(selected_wetlands) >= 2 && all(c("Great Meadow", "Gilmore Meadow") %in% selected_wetlands)
  })
  
  # Filtered statistics data
  filtered_stats <- reactive({
    req(input$stats_site, input$stats_year, input$time_summary)
    
    data <- wl_stats %>%
      filter(site %in% input$stats_site, year %in% input$stats_year)
    
    switch(input$time_summary,
           "year" = {
             # Per-Year Summary
             data %>%
               mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
               select(Year = year, Site = site, `Mean Water Level (cm)` = WL_mean,
                      `SD Water Level (cm)` = WL_sd, `Minimum Water Level (cm)` = WL_min,
                      `Maximum Water Level (cm)` = WL_max, `Maximum Hourly Increase (cm)` = max_inc,
                      `Maximum Hourly Decrease (cm)` = max_dec, `Growing Season Change (cm)` = GS_change,
                      `GS % Surface Water` = prop_over_0cm, `GS % Within 30cm` = prop_bet_0_neg30cm,
                      `GS % Over 30cm Deep` = prop_under_neg30cm)
           },
           "multi" = {
             # Average Across Years
             data %>%
               group_by(site, wetland) %>%
               summarise(
                 Year = paste0(min(year), "–", max(year)),
                 across(c(WL_mean, WL_sd, WL_min, WL_max, max_inc, max_dec, GS_change,
                          prop_over_0cm, prop_bet_0_neg30cm, prop_under_neg30cm), 
                        ~ round(mean(.x, na.rm = TRUE), 2)),
                 .groups = "drop"
               ) %>%
               rename(Site = site, Wetland = wetland, `Mean Water Level (cm)` = WL_mean,
                      `SD Water Level (cm)` = WL_sd, `Minimum Water Level (cm)` = WL_min,
                      `Maximum Water Level (cm)` = WL_max, `Maximum Hourly Increase (cm)` = max_inc,
                      `Maximum Hourly Decrease (cm)` = max_dec, `Growing Season Change (cm)` = GS_change,
                      `GS % Surface Water` = prop_over_0cm, `GS % Within 30cm` = prop_bet_0_neg30cm,
                      `GS % Over 30cm Deep` = prop_under_neg30cm)
           },
           "all_sites" = {
             # All Sites with significance
             data %>%
               group_by(wetland) %>%
               summarise(
                 site = "All Sites",
                 Year = paste0(min(year), "–", max(year)),
                 across(c(WL_mean, WL_sd, WL_min, WL_max, max_inc, max_dec, GS_change,
                          prop_over_0cm, prop_bet_0_neg30cm, prop_under_neg30cm), 
                        ~ round(mean(.x, na.rm = TRUE), 2)),
                 .groups = "drop"
               ) %>%
               rename(Site = site, Wetland = wetland, `Mean Water Level (cm)` = WL_mean,
                      `SD Water Level (cm)` = WL_sd, `Minimum Water Level (cm)` = WL_min,
                      `Maximum Water Level (cm)` = WL_max, `Maximum Hourly Increase (cm)` = max_inc,
                      `Maximum Hourly Decrease (cm)` = max_dec, `Growing Season Change (cm)` = GS_change,
                      `GS % Surface Water` = prop_over_0cm, `GS % Within 30cm` = prop_bet_0_neg30cm,
                      `GS % Over 30cm Deep` = prop_under_neg30cm)
           }
    )
  })
  
  # Render brush info table
  output$brush_info <- renderTable({
    processed_brush_data()
  })
  
  # Render significance info UI
  output$significance_info <- renderUI({
    req(input$time_summary == "all_sites")
    
    base_note <- HTML("
    <div style='background-color:#f9f9f9; padding:10px; border-left:4px solid #1B365D; margin-bottom:10px; font-size:13px;'>
      <strong>Note:</strong><br>
      • To compare sites, both Great Meadow and Gilmore Meadow must be selected.<br>
      • Significance testing requires more than three years of data.<br>
      • If fewer than three years are chosen, results display without significance testing.
    </div>")
    
    if (show_significance_info()) {
      sig_results <- significance_results()
      
      if (!is.null(sig_results)) {
        sig_vars <- sig_results %>% filter(significant) %>% pull(variable)
        
        if (length(sig_vars) > 0) {
          sig_display_names <- VAR_MAPPING[sig_vars]
          sig_display_names <- sig_display_names[!is.na(sig_display_names)]
          
          return(tagList(
            base_note,
            div(class = "significance-info", style = "margin-bottom: 25px;",
                h5(icon("asterisk"), " Statistical Significance"),
                p("Yellow highlighted variables show significant differences (p < 0.05) between Great Meadow and Gilmore Meadow wetlands:"),
                tags$ul(lapply(sig_display_names, function(name) tags$li(name))),
                p(class = "note", "Tests compare averages between each wetland for all selected years and sites.",
                  style = "margin-top: 5px; font-size: 0.8rem; font-style: italic; color: #6c757d;")
            )
          ))
        }
      }
    }
    
    return(base_note)
  })
  
  # Render water level stats table with significance highlighting
  output$wl_stats <- DT::renderDataTable({
    data <- filtered_stats() %>%
      arrange(desc(Site == "All Sites"), Site)
    
    dt <- datatable(data, rownames = FALSE,
                    options = list(pageLength = 10, scrollX = TRUE)) %>%
      formatStyle(columns = names(data), valueColumns = "Site")
    
    if (show_significance_info()) {
      sig_results <- significance_results()
      
      if (!is.null(sig_results)) {
        for (var_name in names(VAR_MAPPING)) {
          col_name <- VAR_MAPPING[var_name]
          sig_row <- sig_results[sig_results$variable == var_name, ]
          
          if (nrow(sig_row) > 0 && sig_row$significant) {
            dt <- dt %>%
              formatStyle(col_name, valueColumns = "Site",
                          backgroundColor = styleEqual("All Sites", "#fff3cd"))
          }
        }
      }
    }
    
    dt
  })
  
  # Download handlers
  output$download_brush <- downloadHandler(
    filename = function() paste("hydrograph_selected_data_", Sys.Date(), ".csv", sep = ""),
    content = function(file) write.csv(processed_brush_data(), file, row.names = FALSE)
  )
  
  output$download_stats <- downloadHandler(
    filename = function() {
      suffix <- switch(input$time_summary,
                       "multi" = "averaged_stats",
                       "all_sites" = "all_sites_significance_stats", 
                       "site_stats"
      )
      paste(suffix, "_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) write.csv(filtered_stats(), file, row.names = FALSE)
  )
  
  output$download_plot <- downloadHandler(
    filename = function() {
      sites_label <- paste(input$selected_sites, collapse = "_")
      years_label <- paste(input$year, collapse = "_")
      paste0("hydrograph_", sites_label, "_", years_label, "_", Sys.Date(), ".png")
    },
    content = function(file) {
      p <- create_hydrograph_plot(plot_data())
      ggsave(file, plot = p, width = 12, height = 8, dpi = 300, bg = "white")
    }
  )
}

# Run app
shinyApp(ui, server)




# 1/28/26 ----------------------------------------------------------------------

## copy of hydrology app 

#### Hydrology R Shiny Dashboard #### 

#---------------------------------------------#
####        Load Required Packages         ####
#---------------------------------------------#

library(shiny)
library(tidyverse)
library(lubridate)
library(patchwork)
library(shinyWidgets)
library(DT)
library(bslib)

#-------------------------------------------#
####    Read & Prepare Processed Data    ####
#-------------------------------------------#

# Great Meadow data
gm <- read.csv("data/processed_data/hydrology_data/gm_well_data_2025_20260127.csv") %>%
  mutate(date = as.Date(date),
         timestamp = as_datetime(timestamp),
         site = paste("Great Meadow", plot.num),
         water.depth = case_when(
           year == 2016 & doy_h == 159.12 & plot.num == 3 & water.depth < -120 ~ NA_real_,
           year == 2017 & doy_h == 215.02 & plot.num == 6 & water.depth < -115 ~ NA_real_,
           year == 2021 & plot.num == 3 & doy == 224 & water.depth > 400 ~ NA_real_,
           year == 2021 & plot.num == 3 & doy == 225 & water.depth > 400 ~ NA_real_,
           TRUE ~ water.depth
         ),
         siteyear = paste(site, year, sep = "_"))

# Gilmore Meadow data 
gl <- read.csv("data/raw_data/hydrology_data/gilmore_well_prec_data_2013-2024.csv") %>%
  rename(water.depth = GILM_WL) %>%
  mutate(
    site = "Gilmore Meadow",
    timestamp_parsed = coalesce(mdy_hm(timestamp), mdy(timestamp)),
    timestamp = as.POSIXct(timestamp_parsed),
    Date = mdy(Date)
  ) %>%
  select(timestamp, date = Date, doy, year = Year, precip.cm = precip_cm,
         water.depth, lag.precip, hr, doy_h, site)

# Create precipitation lookup and combine datasets
gm_precip_lookup <- gm %>% select(timestamp, precip.cm, lag.precip) %>% distinct()

gl_with_gm_precip <- gl %>%
  select(-precip.cm, -lag.precip) %>%
  left_join(gm_precip_lookup, by = "timestamp") %>%
  left_join(gl %>% select(timestamp, orig_precip_cm = precip.cm, orig_lag_precip = lag.precip), 
            by = "timestamp") %>%
  mutate(precip.cm = coalesce(precip.cm, orig_precip_cm),
         lag.precip = coalesce(lag.precip, orig_lag_precip)) %>%
  select(-orig_precip_cm, -orig_lag_precip)

# Combine datasets
all_data <- bind_rows(gm, gl_with_gm_precip) %>% 
  filter(year >= 2016 & year <= 2024) %>% 
  select(timestamp, date, year, doy, hr, doy_h, precip_cm = precip.cm,
         lag_precip = lag.precip, water_depth = water.depth, site)

# Water level stats
wl_stats <- read.csv("data/processed_data/gm_gl_wl_stats.csv") %>% 
  select(year, stat, `Gilmore Meadow` = gilmore.meadow, 
         `Great Meadow 1` = great.meadow.1, `Great Meadow 2` = great.meadow.2, 
         `Great Meadow 3` = great.meadow.3, `Great Meadow 4` = great.meadow.4, 
         `Great Meadow 5` = great.meadow.5, `Great Meadow 6` = great.meadow.6) %>% 
  pivot_longer(cols = -c(year, stat), names_to = "site", values_to = "value") %>% 
  pivot_wider(names_from = stat, values_from = value) %>%
  arrange(site, year) %>% 
  mutate(wetland = if_else(grepl("Great Meadow", site), "Great Meadow", "Gilmore Meadow"))

#-----------------------#
####    Constants    ####
#-----------------------#

# Site color palette (defined once)
SITE_COLORS <- c(
  "Great Meadow 1" = "black", "Great Meadow 2" = "chartreuse4", "Great Meadow 3" = "green",
  "Great Meadow 4" = "darkorange", "Great Meadow 5" = "deeppink2", "Great Meadow 6" = "purple",
  "Gilmore Meadow" = "darkgray", "Precipitation" = "blue"
)

# Variable name mapping for significance testing
VAR_MAPPING <- c(
  "WL_mean" = "Mean Water Level (cm)",
  "WL_sd" = "SD Water Level (cm)", 
  "WL_min" = "Minimum Water Level (cm)",
  "WL_max" = "Maximum Water Level (cm)",
  "max_inc" = "Maximum Hourly Increase (cm)",
  "max_dec" = "Maximum Hourly Decrease (cm)",
  "GS_change" = "Growing Season Change (cm)",
  "prop_over_0cm" = "GS % Surface Water",
  "prop_bet_0_neg30cm" = "GS % Within 30cm",
  "prop_under_neg30cm" = "GS % Over 30cm Deep"
)

# Common pickerInput options
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

# hydrograph plot creation function
create_hydrograph_plot <- function(data) {
  req(nrow(data) > 0)
  
  minWL <- min(data$water_depth, na.rm = TRUE)
  sites <- unique(data$site)
  
  ggplot(data, aes(x = doy_h, y = water_depth)) +
    geom_line(aes(color = site), size = 0.7) +
    geom_line(aes(x = doy_h, y = lag_precip * 5 + minWL, color = "Precipitation"), size = 0.7) +
    geom_hline(yintercept = 0, color = 'brown') +
    facet_wrap(~ year, ncol = 1) +
    scale_color_manual(values = SITE_COLORS, breaks = c(sites, "Precipitation")) +
    labs(y = 'Water Level (cm)', x = 'Date') +
    scale_x_continuous(
      breaks = c(121, 152, 182, 213, 244, 274),
      labels = c('May-01', 'Jun-01', 'Jul-01', 'Aug-01', 'Sep-01', 'Oct-01')
    ) +
    scale_y_continuous(
      sec.axis = sec_axis(~ ., breaks = c(minWL, minWL + 10),
                          name = 'Hourly Precip. (cm)', labels = c('0', '2'))
    ) +
    theme_bw() +
    theme(
      plot.title = element_text(hjust = 0.5),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      axis.text.y.right = element_text(color = 'blue'),
      axis.title.y.right = element_text(color = 'blue'),
      strip.text = element_text(size = 11),
      legend.position = "bottom",
      legend.title = element_blank()
    )
}

# statistical significance function
calculate_wetland_significance <- function(data, selected_years, selected_sites, alpha = 0.05) {
  if (length(selected_years) <= 3) {
    message("⚠️ Not enough years selected for significance testing (need >3).")
    return(NULL)
  }
  
  filtered_data <- data %>%
    filter(year %in% selected_years, site %in% selected_sites) %>%
    mutate(site_group = if_else(grepl("Great Meadow", site), "Great Meadow", site))
  
  wetlands_present <- unique(filtered_data$site_group)
  if (length(wetlands_present) < 2 || !all(c("Great Meadow", "Gilmore Meadow") %in% wetlands_present)) {
    message("⚠️ Both Great Meadow and Gilmore Meadow must be present for comparison.")
    return(NULL)
  }
  
  stat_cols <- filtered_data %>% select(where(is.numeric), -year) %>% names()
  
  yearly_means <- filtered_data %>%
    group_by(year, site_group) %>%
    summarise(across(all_of(stat_cols), mean, na.rm = TRUE), .groups = "drop")
  
  map_dfr(stat_cols, function(var) {
    tryCatch({
      test <- t.test(as.formula(paste(var, "~ site_group")), data = yearly_means)
      data.frame(variable = var, p_value = test$p.value, significant = test$p.value < alpha)
    }, error = function(e) {
      data.frame(variable = var, p_value = NA, significant = FALSE)
    })
  })
}

# picker tool function
create_picker_input <- function(id, label, choices, selected, multiple = TRUE, none_text = "Choose options") {
  pickerInput(id, label = div(icon(if(id %in% c("selected_sites", "stats_site")) "map-marker" else "calendar"), label),
              choices = sort(choices), selected = selected, multiple = multiple,
              options = c(PICKER_OPTIONS, list(`none-selected-text` = none_text)))
}

#----------------#
####    UI    ####
#----------------#

ui <- page_fluid(
  theme = bs_theme(
    version = 5, bootswatch = "flatly", primary = "#1B365D", secondary = "#4C6D9A",    
    success = "#2E86C1", info = "#3498db", warning = "#f39c12", danger = "#e74c3c",
    base_font = font_google("Open Sans"), heading_font = font_google("Open Sans", wght = c(400, 700))
  ),
  
  # Custom CSS
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
        box-shadow: 0 2px 8px rgba(0,0,0,0.08); border: 1px solid #dee2e6;
      }
      .main-title {
        background: linear-gradient(135deg, #1B365D 0%, #4C6D9A 100%);
        color: white; padding: 30px; margin: -15px -15px 30px -15px;
        text-align: center; border-radius: 0 0 20px 20px;
        box-shadow: 0 4px 12px rgba(0,0,0,0.15);
      }
      .brush-info-section {
        background: linear-gradient(135deg, #e8f4f8 0%, #f0f8ff 100%);
        border-radius: 12px; padding: 20px; margin: 20px 0;
        border: 1px solid #4C6D9A;
      }
      .dataTables_wrapper { font-size: 0.85rem !important; }
      .dataTables_wrapper table { font-size: 0.8rem !important; }
      .significance-info h5 {
        background-color: #fff3cd; color: #856404;
        padding: 6px 10px; border-radius: 4px;
        display: inline-block; margin-bottom: 10px;
      }
    "))
  ),
  
  # Main title
  div(class = "main-title",
      h1("Wetland Hydrology Dashboard", 
         style = "margin: 0; font-size: 2rem; text-shadow: 2px 2px 4px rgba(0,0,0,0.3)")
  ),
  
  # FIRST SECTION: Hydrographs section
  div(class = "content-section",
      layout_sidebar(
        sidebar = sidebar(
          class = "sidebar-custom", width = 300,
          h4("Hydrograph Controls", style = "color: #1B365D; margin-bottom: 20px;"),
          
          create_picker_input("selected_sites", "Select Site(s):", 
                              unique(all_data$site), "Great Meadow 1", 
                              none_text = "Choose site(s)"),
          
          create_picker_input("year", "Select Year(s):", 
                              unique(all_data$year), 2024,
                              none_text = "Choose year(s)"),
          
          br(),
          div(style = "padding: 10px; background-color: #e8f4f8; border-radius: 8px; border-left: 4px solid #3498db;",
              p(icon("info-circle"), " Use the brush tool (+) by clicking and dragging with your cursor to select data on the hydrograph and view below.", 
                style = "margin: 0; font-size: 0.9rem; color: #2c3e50;")),
          
          div(style = "margin-top: 10px; text-align: center;",
              downloadButton("download_plot", "Download Hydrograph", 
                             class = "btn-primary btn-sm", icon = icon("image"))),
          
          div(style = "margin-top: 15px; text-align: center;",
              downloadButton("download_brush", "Download Selected Data", 
                             class = "btn-primary btn-sm", icon = icon("download"))),
          
          div(style = "margin-top: 15px; text-align: center;",
              tags$a(href = "#about",
                     class = "btn btn-primary btn-sm", icon("info-circle"),
                     "About")),
        ),
        
        card(
          full_screen = TRUE,
          card_header(class = "bg-primary text-white", "Hydrographs by Year"),
          plotOutput("hydrograph", height = "600px", 
                     brush = brushOpts(id = "hydro_brush", fill = "#4C6D9A", opacity = 0.3))
        )
      )
  ),
  
  # Selected data section
  div(class = "brush-info-section",
      card(
        card_header(class = "bg-success text-white", "Selected Data from Hydrograph:"),
        tableOutput("brush_info")
      )
  ),
  
  # SECOND SECTION: Water Level Stats
  div(class = "content-section",
      layout_sidebar(
        sidebar = sidebar(
          class = "sidebar-custom", width = 300,
          h4("Statistics Controls", style = "color: #1B365D; margin-bottom: 20px;"),
          
          create_picker_input("stats_site", "Select Site(s):", 
                              unique(wl_stats$site), 
                              c("Great Meadow 1", "Gilmore Meadow"),
                              none_text = "Choose site(s)"),
          
          create_picker_input("stats_year", "Select Years:", 
                              unique(wl_stats$year), 
                              c(2024, 2023, 2022, 2021),
                              none_text = "Choose year(s)"),
          
          radioButtons("time_summary", "Summarize Water Level Statistics By:",
                       choices = c("Each Year" = "year",
                                   "Average Across Years" = "multi",
                                   "All Sites (with statistical significance)" = "all_sites"),
                       selected = "year"),
          
          br(),
          div(style = "padding: 10px; background-color: #e8f4f8; border-radius: 8px; border-left: 4px solid #3498db;",
              p(icon("info-circle"), " Growing season statistics calculated from May through October.", 
                style = "margin: 0; font-size: 0.9rem; color: #2c3e50;")),
          
          div(style = "margin-top: 15px; text-align: center;",
              downloadButton("download_stats", "Download Table", 
                             class = "btn-primary btn-sm", icon = icon("download"))),
          
          div(style = "margin-top: 15px; text-align: center;",
              tags$a(href = "#about",
                     class = "btn btn-primary btn-sm", icon("info-circle"),
                     "About")),
        ),
        
        card(
          full_screen = TRUE,
          card_header(class = "bg-primary text-white", "Growing Season Water Level Statistics"),
          div(style = "padding: 10px;",
              uiOutput("significance_info"),
              dataTableOutput("wl_stats"))
        )
      )
  ),
  
  # About section
  div(id = "about",
      class = "brush-info-section",
      card(
        card_header(class = "bg-success text-white", "About"),
        includeHTML("./www/About.html")
      )
  )
  
)


#--------------------#
####    SERVER    ####
#--------------------#

server <- function(input, output, session) {
  
  # Reactive data for plotting
  plot_data <- reactive({
    req(input$year, input$selected_sites)
    all_data %>%
      filter(year %in% input$year, doy > 134, doy < 275, site %in% input$selected_sites)
  })
  
  # Render hydrograph plot
  output$hydrograph <- renderPlot({
    create_hydrograph_plot(plot_data())
  })
  
  # Processed brushed data
  processed_brush_data <- reactive({
    req(input$hydro_brush)
    plot_data_filtered <- plot_data()
    req(nrow(plot_data_filtered) > 0)
    
    selected_data <- brushedPoints(plot_data_filtered, brush = input$hydro_brush,
                                   xvar = "doy_h", yvar = "water_depth") %>%
      select(timestamp, year, doy_h, site, water_depth, lag_precip) %>%
      arrange(year, doy_h) %>%
      mutate(across(c(doy_h, water_depth), ~ round(.x, 2)),
             lag_precip = round(lag_precip, 3),
             timestamp = format(timestamp, "%Y-%m-%d %H:%M:%S"))
    
    if (nrow(selected_data) == 0) {
      return(data.frame(Message = "No points selected - try brushing over the water level lines"))
    }
    
    # Create base data and pivot water depths by site
    base_data <- selected_data %>%
      group_by(year, doy_h, timestamp, lag_precip) %>%
      summarise(.groups = 'drop') %>%
      arrange(year, doy_h)
    
    water_depth_data <- selected_data %>%
      group_by(year, doy_h, timestamp, site) %>%
      summarise(water_depth = mean(water_depth, na.rm = TRUE), .groups = 'drop') %>%
      pivot_wider(names_from = site, values_from = water_depth)
    
    result_data <- base_data %>%
      left_join(water_depth_data, by = c("year", "doy_h", "timestamp")) %>%
      rename(Year = year, Timestamp = timestamp, `Day of Year` = doy_h, 
             `Precipitation (cm)` = lag_precip)
    
    # Add "Water Depth (cm)" suffix to site columns
    site_columns <- intersect(names(result_data), input$selected_sites)
    if (length(site_columns) > 0) {
      result_data <- result_data %>%
        rename_with(~ paste(.x, "Water Depth (cm)"), .cols = all_of(site_columns))
    }
    
    result_data %>% arrange(Year, `Day of Year`)
  })
  
  # Significance testing results
  significance_results <- reactive({
    req(input$stats_year, input$stats_site)
    selected_wetlands <- wl_stats %>%
      filter(site %in% input$stats_site) %>%
      pull(wetland) %>%
      unique()
    
    if (length(selected_wetlands) >= 2 && all(c("Great Meadow", "Gilmore Meadow") %in% selected_wetlands)) {
      calculate_wetland_significance(wl_stats, input$stats_year, input$stats_site, alpha = 0.05)
    } else {
      NULL
    }
  })
  
  # Check if we should show significance info
  show_significance_info <- reactive({
    req(input$stats_site, input$time_summary)
    if (input$time_summary != "all_sites") return(FALSE)
    
    selected_wetlands <- wl_stats %>%
      filter(site %in% input$stats_site) %>%
      pull(wetland) %>%
      unique()
    
    length(selected_wetlands) >= 2 && all(c("Great Meadow", "Gilmore Meadow") %in% selected_wetlands)
  })
  
  # Filtered statistics data
  filtered_stats <- reactive({
    req(input$stats_site, input$stats_year, input$time_summary)
    
    data <- wl_stats %>%
      filter(site %in% input$stats_site, year %in% input$stats_year)
    
    switch(input$time_summary,
           "year" = {
             # Per-Year Summary
             data %>%
               mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
               select(Year = year, Site = site, `Mean Water Level (cm)` = WL_mean,
                      `SD Water Level (cm)` = WL_sd, `Minimum Water Level (cm)` = WL_min,
                      `Maximum Water Level (cm)` = WL_max, `Maximum Hourly Increase (cm)` = max_inc,
                      `Maximum Hourly Decrease (cm)` = max_dec, `Growing Season Change (cm)` = GS_change,
                      `GS % Surface Water` = prop_over_0cm, `GS % Within 30cm` = prop_bet_0_neg30cm,
                      `GS % Over 30cm Deep` = prop_under_neg30cm)
           },
           "multi" = {
             # Average Across Years
             data %>%
               group_by(site, wetland) %>%
               summarise(
                 Year = paste0(min(year), "–", max(year)),
                 across(c(WL_mean, WL_sd, WL_min, WL_max, max_inc, max_dec, GS_change,
                          prop_over_0cm, prop_bet_0_neg30cm, prop_under_neg30cm), 
                        ~ round(mean(.x, na.rm = TRUE), 2)),
                 .groups = "drop"
               ) %>%
               rename(Site = site, Wetland = wetland, `Mean Water Level (cm)` = WL_mean,
                      `SD Water Level (cm)` = WL_sd, `Minimum Water Level (cm)` = WL_min,
                      `Maximum Water Level (cm)` = WL_max, `Maximum Hourly Increase (cm)` = max_inc,
                      `Maximum Hourly Decrease (cm)` = max_dec, `Growing Season Change (cm)` = GS_change,
                      `GS % Surface Water` = prop_over_0cm, `GS % Within 30cm` = prop_bet_0_neg30cm,
                      `GS % Over 30cm Deep` = prop_under_neg30cm)
           },
           "all_sites" = {
             # All Sites with significance
             data %>%
               group_by(wetland) %>%
               summarise(
                 site = "All Sites",
                 Year = paste0(min(year), "–", max(year)),
                 across(c(WL_mean, WL_sd, WL_min, WL_max, max_inc, max_dec, GS_change,
                          prop_over_0cm, prop_bet_0_neg30cm, prop_under_neg30cm), 
                        ~ round(mean(.x, na.rm = TRUE), 2)),
                 .groups = "drop"
               ) %>%
               rename(Site = site, Wetland = wetland, `Mean Water Level (cm)` = WL_mean,
                      `SD Water Level (cm)` = WL_sd, `Minimum Water Level (cm)` = WL_min,
                      `Maximum Water Level (cm)` = WL_max, `Maximum Hourly Increase (cm)` = max_inc,
                      `Maximum Hourly Decrease (cm)` = max_dec, `Growing Season Change (cm)` = GS_change,
                      `GS % Surface Water` = prop_over_0cm, `GS % Within 30cm` = prop_bet_0_neg30cm,
                      `GS % Over 30cm Deep` = prop_under_neg30cm)
           }
    )
  })
  
  # Render brush info table
  output$brush_info <- renderTable({
    processed_brush_data()
  })
  
  # Render significance info UI
  output$significance_info <- renderUI({
    req(input$time_summary == "all_sites")
    
    base_note <- HTML("
    <div style='background-color:#f9f9f9; padding:10px; border-left:4px solid #1B365D; margin-bottom:10px; font-size:13px;'>
      <strong>Note:</strong><br>
      • To compare sites, both Great Meadow and Gilmore Meadow must be selected.<br>
      • Significance testing requires more than three years of data.<br>
      • If fewer than three years are chosen, results display without significance testing.
    </div>")
    
    if (show_significance_info()) {
      sig_results <- significance_results()
      
      if (!is.null(sig_results)) {
        sig_vars <- sig_results %>% filter(significant) %>% pull(variable)
        
        if (length(sig_vars) > 0) {
          sig_display_names <- VAR_MAPPING[sig_vars]
          sig_display_names <- sig_display_names[!is.na(sig_display_names)]
          
          return(tagList(
            base_note,
            div(class = "significance-info", style = "margin-bottom: 25px;",
                h5(icon("asterisk"), " Statistical Significance"),
                p("Yellow highlighted variables show significant differences (p < 0.05) between Great Meadow and Gilmore Meadow wetlands:"),
                tags$ul(lapply(sig_display_names, function(name) tags$li(name))),
                p(class = "note", "Tests compare averages between each wetland for all selected years and sites.",
                  style = "margin-top: 5px; font-size: 0.8rem; font-style: italic; color: #6c757d;")
            )
          ))
        }
      }
    }
    
    return(base_note)
  })
  
  # Render water level stats table with significance highlighting
  output$wl_stats <- DT::renderDataTable({
    data <- filtered_stats() %>%
      arrange(desc(Site == "All Sites"), Site)
    
    dt <- datatable(data, rownames = FALSE,
                    options = list(pageLength = 10, scrollX = TRUE)) %>%
      formatStyle(columns = names(data), valueColumns = "Site")
    
    if (show_significance_info()) {
      sig_results <- significance_results()
      
      if (!is.null(sig_results)) {
        for (var_name in names(VAR_MAPPING)) {
          col_name <- VAR_MAPPING[var_name]
          sig_row <- sig_results[sig_results$variable == var_name, ]
          
          if (nrow(sig_row) > 0 && sig_row$significant) {
            dt <- dt %>%
              formatStyle(col_name, valueColumns = "Site",
                          backgroundColor = styleEqual("All Sites", "#fff3cd"))
          }
        }
      }
    }
    
    dt
  })
  
  # Download handlers
  output$download_brush <- downloadHandler(
    filename = function() paste("hydrograph_selected_data_", Sys.Date(), ".csv", sep = ""),
    content = function(file) write.csv(processed_brush_data(), file, row.names = FALSE)
  )
  
  output$download_stats <- downloadHandler(
    filename = function() {
      suffix <- switch(input$time_summary,
                       "multi" = "averaged_stats",
                       "all_sites" = "all_sites_significance_stats", 
                       "site_stats"
      )
      paste(suffix, "_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) write.csv(filtered_stats(), file, row.names = FALSE)
  )
  
  output$download_plot <- downloadHandler(
    filename = function() {
      sites_label <- paste(input$selected_sites, collapse = "_")
      years_label <- paste(input$year, collapse = "_")
      paste0("hydrograph_", sites_label, "_", years_label, "_", Sys.Date(), ".png")
    },
    content = function(file) {
      p <- create_hydrograph_plot(plot_data())
      ggsave(file, plot = p, width = 12, height = 8, dpi = 300, bg = "white")
    }
  )
}

# Run app
shinyApp(ui, server)







# 10/20/25----------------------------------------------------------------------

# Water level stats
wl_stats <- read.csv("data/processed_data/gm_gl_wl_stats.csv") %>% 
  select(year, stat, `Gilmore Meadow` = gilmore.meadow, 
         `Great Meadow 1` = great.meadow.1, `Great Meadow 2` = great.meadow.2, 
         `Great Meadow 3` = great.meadow.3, `Great Meadow 4` = great.meadow.4, 
         `Great Meadow 5` = great.meadow.5, `Great Meadow 6` = great.meadow.6) %>% 
  pivot_longer(cols = -c(year, stat), names_to = "site", values_to = "value") %>% 
  pivot_wider(names_from = stat, values_from = value) %>%
  arrange(site, year) %>% 
  mutate(wetland = if_else(grepl("Great Meadow", site), "Great Meadow", "Gilmore Meadow"))


# summarize stats
wl_stats_sum <- wl_stats %>% 
  group_by(wetland) %>%
  summarise(
    across(c(WL_mean, WL_sd, WL_min, WL_max, max_inc, max_dec, GS_change,
             prop_over_0cm, prop_bet_0_neg30cm, prop_under_neg30cm), 
           ~ round(mean(.x, na.rm = TRUE), 2)),
    .groups = "drop")

# statistical significance function
calculate_wetland_significance <- function(data, selected_years, selected_sites, alpha = 0.05) {
  if (length(selected_years) <= 3) {
    message("⚠️ Not enough years selected for significance testing (need >3).")
    return(NULL)
  }
  
  filtered_data <- data %>%
    filter(year %in% selected_years, site %in% selected_sites) %>%
    mutate(site_group = if_else(grepl("Great Meadow", site), "Great Meadow", site))
  
  wetlands_present <- unique(filtered_data$site_group)
  if (length(wetlands_present) < 2 || !all(c("Great Meadow", "Gilmore Meadow") %in% wetlands_present)) {
    message("⚠️ Both Great Meadow and Gilmore Meadow must be present for comparison.")
    return(NULL)
  }
  
  stat_cols <- filtered_data %>% select(where(is.numeric), -year) %>% names()
  
  yearly_means <- filtered_data %>%
    group_by(year, site_group) %>%
    summarise(across(all_of(stat_cols), mean, na.rm = TRUE), .groups = "drop")
  
  map_dfr(stat_cols, function(var) {
    tryCatch({
      test <- t.test(as.formula(paste(var, "~ site_group")), data = yearly_means)
      data.frame(variable = var, p_value = test$p.value, significant = test$p.value < alpha)
    }, error = function(e) {
      data.frame(variable = var, p_value = NA, significant = FALSE)
    })
  })
}


# Example test values
selected_years <- c(2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024)
selected_sites <- c("Gilmore Meadow", "Great Meadow 1", "Great Meadow 2", "Great Meadow 3", 
                    "Great Meadow 4", "Great Meadow 5", "Great Meadow 6")

# Run the function
test_results <- calculate_wetland_significance(wl_stats, selected_years, selected_sites, alpha = 0.05)











#### Hydrograph Visualizer Shiny Dashboard #### 

#---------------------------------------------#
####        Load Required Packages         ####
#---------------------------------------------#

library(shiny)
library(tidyverse)
library(lubridate)
library(patchwork)
library(shinyWidgets)
library(DT)
library(bslib)

#-------------------------------------------#
####    Read & Prepare Processed Data    ####
#-------------------------------------------#

# Great Meadow data
gm <- read.csv("data/processed_data/great_meadow_well_data_2024_20250915.csv") %>%
  mutate(date = as.Date(date),
         timestamp = as_datetime(timestamp),
         site = paste("Great Meadow", plot.num),
         water.depth = case_when(
           year == 2016 & doy_h == 159.12 & plot.num == 3 & water.depth < -120 ~ NA_real_,
           year == 2017 & doy_h == 215.02 & plot.num == 6 & water.depth < -115 ~ NA_real_,
           year == 2021 & plot.num == 3 & doy == 224 & water.depth > 400 ~ NA_real_,
           year == 2021 & plot.num == 3 & doy == 225 & water.depth > 400 ~ NA_real_,
           TRUE ~ water.depth
         )) %>%
  mutate(siteyear = paste(site, year, sep = "_"))

# Gilmore Meadow data 
gl <- read.csv("data/raw_data/hydrology_data/gilmore_well_prec_data_2013-2024.csv") %>%
  rename(water.depth = GILM_WL) %>%
  mutate(
    site = "Gilmore Meadow",
    
    # Try to parse full datetime, fallback to date only + midnight
    timestamp_parsed = mdy_hm(timestamp),
    timestamp_parsed = if_else(
      is.na(timestamp_parsed),
      mdy(timestamp),
      timestamp_parsed
    ),
    timestamp_parsed = as.POSIXct(timestamp_parsed),
    timestamp = timestamp_parsed,
    
    # Standardize Date column
    Date = mdy(Date)
  ) %>%
  select(timestamp, 
         date = Date,
         doy,
         year = Year,
         precip.cm = precip_cm,
         water.depth,
         lag.precip,
         hr,
         doy_h,
         site)

# Need to select Great Meadow precip data as default for all data since Gilmore Meadow has precip data too
# Create precipitation lookup from Great Meadow data
gm_precip_lookup <- gm %>%
  select(timestamp, precip.cm, lag.precip) %>%
  distinct()

# Replace Gilmore precip with Great Meadow precip for matching timestamps
gl_with_gm_precip <- gl %>%
  select(-precip.cm, -lag.precip) %>%  # Remove original precip columns
  left_join(gm_precip_lookup, by = "timestamp") %>%
  # If no match found, keep original Gilmore precip values as fallback
  left_join(
    gl %>% select(timestamp, orig_precip_cm = precip.cm, orig_lag_precip = lag.precip),
    by = "timestamp"
  ) %>%
  mutate(
    precip.cm = coalesce(precip.cm, orig_precip_cm),
    lag.precip = coalesce(lag.precip, orig_lag_precip)
  ) %>%
  select(-orig_precip_cm, -orig_lag_precip)

# Combine Great Meadow and Gilmore Meadow datasets
all_data <- bind_rows(gm, gl_with_gm_precip) %>% 
  filter(year >= 2016 & year <= 2024) %>% 
  select(timestamp, 
         date, 
         year, 
         doy, 
         hr, 
         doy_h,
         precip_cm = precip.cm,
         lag_precip = lag.precip,
         water_depth = water.depth,
         site)

# Format water level stats for WL table output 
wl_stats <- read.csv("data/processed_data/gm_gl_wl_stats.csv") %>% 
  select(year,
         stat, 
         `Gilmore Meadow` = gilmore.meadow, 
         `Great Meadow 1` = great.meadow.1, 
         `Great Meadow 2` = great.meadow.2, 
         `Great Meadow 3` = great.meadow.3, 
         `Great Meadow 4` = great.meadow.4, 
         `Great Meadow 5` = great.meadow.5, 
         `Great Meadow 6` = great.meadow.6) %>% 
  pivot_longer(cols = -c(year, stat), names_to = "site", values_to = "value") %>% 
  pivot_wider(names_from = stat, values_from = value) %>%
  arrange(site, year) %>% 
  mutate(
    wetland = case_when(
      grepl("Great Meadow", site, ignore.case = TRUE) ~ "Great Meadow",
      grepl("Gilmore Meadow", site, ignore.case = TRUE) ~ "Gilmore Meadow",
      TRUE ~ "Unknown"
    )
  )

#-----------------------#
####    Functions    ####
#-----------------------#

# Function to calculate significance tests between wetlands
calculate_wetland_significance <- function(data, selected_years, selected_sites, alpha = 0.05) {
  # Check minimum number of years
  if (length(selected_years) <= 3) {
    message("⚠️ Not enough years selected for significance testing (need >3).")
    return(NULL)
  }
  
  # Filter data for selected years and sites
  filtered_data <- data %>%
    filter(year %in% selected_years, site %in% selected_sites) %>%
    mutate(site_group = ifelse(grepl("Great Meadow", site), "Great Meadow", site))
  
  # Check both wetlands are present
  wetlands_present <- unique(filtered_data$site_group)
  if (length(wetlands_present) < 2 || !all(c("Great Meadow", "Gilmore Meadow") %in% wetlands_present)) {
    message("⚠️ Both Great Meadow and Gilmore Meadow must be present for comparison.")
    return(NULL)
  }
  
  # Get numeric stat columns
  stat_cols <- filtered_data %>%
    select(where(is.numeric), -year) %>%
    names()
  
  # Step 1: average each variable by wetland × year
  yearly_means <- filtered_data %>%
    group_by(year, site_group) %>%
    summarise(across(all_of(stat_cols), mean, na.rm = TRUE), .groups = "drop")
  
  # Step 2: run t-tests across wetlands for each variable
  t_test_results <- map_dfr(stat_cols, function(var) {
    tryCatch({
      test <- t.test(as.formula(paste(var, "~ site_group")), data = yearly_means)
      data.frame(
        variable = var,
        p_value = test$p.value,
        significant = test$p.value < alpha,
        stringsAsFactors = FALSE
      )
    }, error = function(e) {
      data.frame(
        variable = var,
        p_value = NA,
        significant = FALSE,
        stringsAsFactors = FALSE
      )
    })
  })
  
  return(t_test_results)
}




#----------------#
####    UI    ####
#----------------#

ui <- page_fluid(
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#1B365D",
    secondary = "#4C6D9A",    
    success = "#2E86C1",
    info = "#3498db",
    warning = "#f39c12",
    danger = "#e74c3c",
    base_font = font_google("Open Sans"),
    heading_font = font_google("Open Sans", wght = c(400, 700))
  ),
  
  # Custom CSS for additional styling
  tags$head(
    tags$style(HTML("
      .content-section {
        margin: 30px 0;
        padding: 25px;
        border-radius: 15px;
        box-shadow: 0 4px 12px rgba(0,0,0,0.1);
        background: linear-gradient(135deg, #ffffff 0%, #f8f9fa 100%);
        border: 2px solid #1B365D;
      }
      
      .section-title {
        color: #1B365D;
        font-weight: 600;
        margin-bottom: 20px;
        padding-bottom: 10px;
        border-bottom: 2px solid #4C6D9A;
      }
      
      .sidebar-custom {
        background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%);
        border-radius: 10px;
        padding: 20px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.08);
        border: 1px solid #dee2e6;
      }
      
      .main-title {
        background: linear-gradient(135deg, #1B365D 0%, #4C6D9A 100%);
        color: white;
        padding: 30px;
        margin: -15px -15px 30px -15px;
        text-align: center;
        border-radius: 0 0 20px 20px;
        box-shadow: 0 4px 12px rgba(0,0,0,0.15);
      }
      
      .brush-info-section {
        background: linear-gradient(135deg, #e8f4f8 0%, #f0f8ff 100%);
        border-radius: 12px;
        padding: 20px;
        margin: 20px 0;
        border: 1px solid #4C6D9A;
      }
      
      .stats-main {
        background: linear-gradient(135deg, #f8f9fa 0%, #ffffff 100%);
      }
      
      .dataTables_wrapper {
        font-size: 0.85rem !important;
      }

      .dataTables_wrapper table {
        font-size: 0.8rem !important;
      }

      .dataTables_wrapper .dataTables_info,
      .dataTables_wrapper .dataTables_paginate {
        font-size: 0.75rem !important;
      }
      
    .significance-info h5 {
      background-color: #fff3cd;
      color: #856404;
      padding: 6px 10px;
      border-radius: 4px;
      display: inline-block;
      margin-bottom: 10px;
    }
    
    .significance-info p {
      margin-bottom: 8px;
      font-size: 0.9rem;
    }
    
    .significance-info ul li {
      font-size: 0.85rem;
      color: #856404;
    }
    
    .significance-info .note {
      margin-top: 5px;
      margin-bottom: 10px;
      font-size: 0.8rem;
      font-style: italic;
      color: #6c757d;
    }
      
    "))
  ),
  
  # Main title with gradient background
  div(class = "main-title",
      h1("Wetland Hydrograph Visualizer", 
         style = "margin: 0; font-size: 2rem; text-shadow: 2px 2px 4px rgba(0,0,0,0.3)")
  ),
  
  # First section: Hydrographs 
  div(class = "content-section",
      layout_sidebar(
        sidebar = sidebar(
          class = "sidebar-custom",
          width = 300,
          h4("Hydrograph Controls", style = "color: #1B365D; margin-bottom: 20px;"),
          
          pickerInput("selected_sites", 
                      label = div(icon("map-marker"), "Select Site(s):"),
                      choices = sort(unique(all_data$site)),
                      selected = c("Great Meadow 1"),
                      multiple = TRUE,
                      options = list(
                        `actions-box` = TRUE,
                        `deselect-all-text` = "Clear all",
                        `select-all-text` = "Select all",
                        `none-selected-text` = "Choose site(s)",
                        `live-search` = TRUE,
                        style = "btn-outline-primary"
                      )),
          
          div(style = "margin-bottom: 15px;",
              pickerInput("year", 
                          label = div(icon("calendar"), "Select Year(s):"),
                          choices = sort(unique(all_data$year)),
                          selected = 2024,
                          multiple = TRUE,
                          options = list(
                            `actions-box` = TRUE,
                            `deselect-all-text` = "Clear all",
                            `select-all-text` = "Select all",
                            `none-selected-text` = "Choose year(s)",
                            `live-search` = TRUE,
                            style = "btn-outline-primary"
                          ))),
          
          br(),
          div(style = "padding: 10px; background-color: #e8f4f8; border-radius: 8px; border-left: 4px solid #3498db;",
              p(icon("info-circle"), " Use the brush tool (+) by clicking and dragging with your cursor to select data on the hydrograph and view below.", 
                style = "margin: 0; font-size: 0.9rem; color: #2c3e50;")),
          
          div(style = "margin-top: 10px; text-align: center;",
              downloadButton("download_plot", "Download Hydrograph", 
                             class = "btn-primary btn-sm", 
                             icon = icon("image"))),
          
          div(style = "margin-top: 15px; text-align: center;",
              downloadButton("download_brush", "Download Selected Data", 
                             class = "btn-primary btn-sm", 
                             icon = icon("download")))
        ),
        
        card(
          full_screen = TRUE,
          card_header(
            class = "bg-primary text-white",
            "Hydrographs by Year"
          ),
          plotOutput("hydrograph", height = "600px", 
                     brush = brushOpts(id = "hydro_brush", fill = "#4C6D9A", opacity = 0.3))
        )
      )
  ),
  
  # Selected data points section 
  div(class = "brush-info-section",
      div(class = "row",
          div(class = "col-12",
              card(
                card_header(
                  class = "bg-success text-white",
                  "Selected Data from Hydrograph:"
                ),
                tableOutput("brush_info")
              )
          )
      )
  ),
  
  # Second section: Water Level Stats
  div(class = "content-section stats-main",
      layout_sidebar(
        sidebar = sidebar(
          class = "sidebar-custom",
          width = 300,
          h4("Statistics Controls", style = "color: #1B365D; margin-bottom: 20px;"),
          
          pickerInput("stats_site", 
                      label = div(icon("map-marker"), "Select Site(s):"),
                      choices = sort(unique(wl_stats$site)),
                      selected = c("Great Meadow 1", "Gilmore Meadow"),
                      multiple = TRUE,
                      options = list(
                        `actions-box` = TRUE,
                        `deselect-all-text` = "Clear all",
                        `select-all-text` = "Select all",
                        `none-selected-text` = "Choose site(s)",
                        `live-search` = TRUE,
                        style = "btn-outline-primary"
                      )),
          
          div(style = "margin-bottom: 15px;",
              pickerInput("stats_year", 
                          label = div(icon("calendar"), "Select Years:"),
                          choices = sort(unique(wl_stats$year)),
                          selected = c(2024, 2023, 2022, 2021),
                          multiple = TRUE,
                          options = list(
                            `actions-box` = TRUE,
                            `deselect-all-text` = "Clear all",
                            `select-all-text` = "Select all",
                            `none-selected-text` = "Choose year(s)",
                            `live-search` = TRUE,
                            style = "btn-outline-primary"
                          ))),
          
          div(style = "margin-bottom: 15px;",
              radioButtons(
                "time_summary", "Summarize Water Level Statistics By:",
                choices = c(
                  "Each Year" = "year",
                  "Average Across Years" = "multi",
                  "All Sites (with statistical significance)" = "all_sites"
                ),
                selected = "year")),
          
          br(),
          div(style = "padding: 10px; background-color: #e8f4f8; border-radius: 8px; border-left: 4px solid #3498db;",
              p(icon("info-circle"), " Growing season statistics calculated from May through October.", 
                style = "margin: 0; font-size: 0.9rem; color: #2c3e50;")),
          
          div(style = "margin-top: 15px; text-align: center;",
              downloadButton("download_stats", "Download Table", 
                             class = "btn-primary btn-sm", 
                             icon = icon("download")))
        ),
        
        card(
          full_screen = TRUE,
          card_header(
            class = "bg-primary text-white",
            "Growing Season Water Level Statistics"
          ),
          
          div(style = "padding: 10px;",
              uiOutput("significance_info"),
              dataTableOutput("wl_stats"))
        )
      )
  )
)

#--------------------#
####    SERVER    ####
#--------------------#

server <- function(input, output, session) {
  
  # Reactive data for plotting
  plot_data <- reactive({
    req(input$year, input$selected_sites)
    
    all_data %>%
      filter(year %in% input$year, doy > 134, doy < 275) %>%
      filter(site %in% input$selected_sites)
  })
  
  output$hydrograph <- renderPlot({
    plot_data_filtered <- plot_data()
    req(nrow(plot_data_filtered) > 0)
    
    # Calculate minimum water level across all data for consistent precipitation baseline
    minWL <- min(plot_data_filtered$water_depth, na.rm = TRUE)
    
    # Color palette for sites
    sites <- unique(plot_data_filtered$site)
    site_colors <- c(
      "Great Meadow 1" = "black", "Great Meadow 2" = "chartreuse4", "Great Meadow 3" = "green",
      "Great Meadow 4" = "darkorange", "Great Meadow 5" = "deeppink2", "Great Meadow 6" = "purple",
      "Gilmore Meadow" = "darkgray", "Precipitation" = "blue"
    )
    
    # Create the plot
    ggplot(plot_data_filtered, aes(x = doy_h, y = water_depth)) +
      # Water level lines by site
      geom_line(aes(color = site), size = 0.7) +
      
      # Precip line (scaled by multiplier of 5 and offset by minWL)
      geom_line(aes(x = doy_h, y = lag_precip * 5 + minWL, color = "Precipitation"), 
                size = 0.7) +
      
      # Ground level reference
      geom_hline(yintercept = 0, color = 'brown') +
      
      # Facet by year
      facet_wrap(~ year, ncol = 1, scales = "free_y") +
      
      # Colors
      scale_color_manual(values = site_colors, 
                         breaks = c(sites, "Precipitation")) +
      
      # Axes and labels
      labs(y = 'Water Level (cm)', x = 'Date') +
      scale_x_continuous(
        breaks = c(121, 152, 182, 213, 244, 274),
        labels = c('May-01', 'Jun-01', 'Jul-01', 'Aug-01', 'Sep-01', 'Oct-01')
      ) +
      
      # Secondary axis for precip
      scale_y_continuous(
        sec.axis = sec_axis(~ .,
                            breaks = c(minWL, minWL + 10),
                            name = 'Hourly Precip. (cm)',
                            labels = c('0', '2'))
      ) +
      
      # Theme
      theme_bw() +
      theme(
        plot.title = element_text(hjust = 0.5),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.text.y.right = element_text(color = 'blue'),
        axis.title.y.right = element_text(color = 'blue'),
        strip.text = element_text(size = 11),
        legend.position = "bottom",
        legend.title = element_blank()
      )
  })
  
  # Reactive for processed brushed data - with site columns
  processed_brush_data <- reactive({
    req(input$hydro_brush)
    
    plot_data_filtered <- plot_data()
    req(nrow(plot_data_filtered) > 0)
    
    # Get brushed points using brushedPoints()
    selected_data <- brushedPoints(
      plot_data_filtered,
      brush = input$hydro_brush,
      xvar = "doy_h",
      yvar = "water_depth"
    ) %>%
      select(timestamp, year, doy_h, site, water_depth, lag_precip) %>%
      arrange(year, doy_h) %>%
      mutate(
        doy_h = round(doy_h, 2),
        water_depth = round(water_depth, 2),
        lag_precip = round(lag_precip, 3),
        timestamp = format(timestamp, "%Y-%m-%d %H:%M:%S")
      )
    
    if (nrow(selected_data) == 0) {
      return(data.frame(Message = "No points selected - try brushing over the water level lines"))
    }
    
    # Create the base data with unique timestamp/precip combinations
    base_data <- selected_data %>%
      group_by(year, doy_h, timestamp, lag_precip) %>%
      summarise(.groups = 'drop') %>%
      arrange(year, doy_h)
    
    # Create water depth columns for each site
    water_depth_data <- selected_data %>%
      group_by(year, doy_h, timestamp, site) %>%
      summarise(water_depth = mean(water_depth, na.rm = TRUE), .groups = 'drop') %>%
      pivot_wider(names_from = site, values_from = water_depth, names_prefix = "")
    
    # Join the data together
    result_data <- base_data %>%
      left_join(water_depth_data, by = c("year", "doy_h", "timestamp")) %>%
      rename(
        Year = year,
        Timestamp = timestamp,
        `Day of Year` = doy_h,
        `Precipitation (cm)` = lag_precip
      )
    
    # Add "Water Depth (cm)" suffix to site columns
    site_columns <- intersect(names(result_data), input$selected_sites)
    if (length(site_columns) > 0) {
      result_data <- result_data %>%
        rename_with(~ paste(.x, "Water Depth (cm)"), .cols = all_of(site_columns))
    }
    
    result_data %>% arrange(Year, `Day of Year`)
  })
  
  # Reactive for significance testing results - checks if both wetlands are selected
  significance_results <- reactive({
    req(input$stats_year, input$stats_site)
    
    # Check if both wetlands are represented in selected sites
    selected_wetlands <- wl_stats %>%
      filter(site %in% input$stats_site) %>%
      pull(wetland) %>%
      unique()
    
    # Only calculate significance if both wetlands are represented
    if (length(selected_wetlands) >= 2 && all(c("Great Meadow", "Gilmore Meadow") %in% selected_wetlands)) {
      calculate_wetland_significance(wl_stats, input$stats_year, input$stats_site, alpha = 0.05)
    } else {
      NULL
    }
  })
  
  # Reactive to determine if we should show significance info
  show_significance_info <- reactive({
    req(input$stats_site, input$time_summary)
    
    if (input$time_summary != "all_sites") return(FALSE)
    
    selected_wetlands <- wl_stats %>%
      filter(site %in% input$stats_site) %>%
      pull(wetland) %>%
      unique()
    
    return(length(selected_wetlands) >= 2 && all(c("Great Meadow", "Gilmore Meadow") %in% selected_wetlands))
  })
  
  
  # Setting up average WL stats and calculating stats output tables
  filtered_stats <- reactive({
    req(input$stats_site, input$stats_year, input$time_summary)
    
    data <- wl_stats %>%
      filter(site %in% input$stats_site, year %in% input$stats_year)
    
    if (input$time_summary == "year") {
      # ---- Option 1: Per-Year Summary ----
      data %>%
        mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
        select(
          Year = year,
          Site = site,
          `Mean Water Level (cm)` = WL_mean,
          `SD Water Level (cm)` = WL_sd,
          `Minimum Water Level (cm)` = WL_min,
          `Maximum Water Level (cm)` = WL_max,
          `Maximum Hourly Increase (cm)` = max_inc,
          `Maximum Hourly Decrease (cm)` = max_dec,
          `Growing Season Change (cm)` = GS_change,
          `GS % Surface Water` = prop_over_0cm,
          `GS % Within 30cm` = prop_bet_0_neg30cm,
          `GS % Over 30cm Deep` = prop_under_neg30cm
        )
    } else if (input$time_summary == "multi") {
      # ---- Option 2: Average Across Years ----
      # Per-site summary
      data_site <- data %>%
        group_by(site, wetland) %>%
        summarise(
          Year = paste0(min(year), "–", max(year)),
          `Mean Water Level (cm)` = round(mean(WL_mean, na.rm = TRUE), 2),
          `SD Water Level (cm)` = round(mean(WL_sd, na.rm = TRUE), 2),
          `Minimum Water Level (cm)` = round(mean(WL_min, na.rm = TRUE), 2),
          `Maximum Water Level (cm)` = round(mean(WL_max, na.rm = TRUE), 2),
          `Maximum Hourly Increase (cm)` = round(mean(max_inc, na.rm = TRUE), 2),
          `Maximum Hourly Decrease (cm)` = round(mean(max_dec, na.rm = TRUE), 2),
          `Growing Season Change (cm)` = round(mean(GS_change, na.rm = TRUE), 2),
          `GS % Surface Water` = round(mean(prop_over_0cm, na.rm = TRUE), 2),
          `GS % Within 30cm` = round(mean(prop_bet_0_neg30cm, na.rm = TRUE), 2),
          `GS % Over 30cm Deep` = round(mean(prop_under_neg30cm, na.rm = TRUE), 2),
          .groups = "drop"
        ) %>%
        rename(Site = site, Wetland = wetland)
      
    } else if (input$time_summary == "all_sites") {
      # ---- Option 3: All Sites (with statistical significance) ----
      
      sig_results <- calculate_wetland_significance(
        wl_stats,
        selected_years = input$stats_year,
        selected_sites = input$stats_site
      )
      
      # Per-wetland "All Sites" summary
      all_sites_data <- data %>%
        group_by(wetland) %>%
        summarise(
          site = "All Sites",
          Year = paste0(min(year), "–", max(year)),
          `Mean Water Level (cm)` = round(mean(WL_mean, na.rm = TRUE), 2),
          `SD Water Level (cm)` = round(mean(WL_sd, na.rm = TRUE), 2),
          `Minimum Water Level (cm)` = round(mean(WL_min, na.rm = TRUE), 2),
          `Maximum Water Level (cm)` = round(mean(WL_max, na.rm = TRUE), 2),
          `Maximum Hourly Increase (cm)` = round(mean(max_inc, na.rm = TRUE), 2),
          `Maximum Hourly Decrease (cm)` = round(mean(max_dec, na.rm = TRUE), 2),
          `Growing Season Change (cm)` = round(mean(GS_change, na.rm = TRUE), 2),
          `GS % Surface Water` = round(mean(prop_over_0cm, na.rm = TRUE), 2),
          `GS % Within 30cm` = round(mean(prop_bet_0_neg30cm, na.rm = TRUE), 2),
          `GS % Over 30cm Deep` = round(mean(prop_under_neg30cm, na.rm = TRUE), 2),
          .groups = "drop"
        ) %>%
        rename(Site = site, Wetland = wetland)
      
      all_sites_data
    }
  })
  
  
  
  # Reactive table output for selected data from hydrograph
  output$brush_info <- renderTable({
    processed_brush_data()
  })
  
  # Output for significance information display
  output$significance_info <- renderUI({
    # Only show this section when "All Sites (with statistical significance)" is selected
    req(input$time_summary == "all_sites")
    
    base_note <- HTML("
    <div style='background-color:#f9f9f9; padding:10px; border-left:4px solid #1B365D; margin-bottom:10px; font-size:13px;'>
      <strong>Note:</strong><br>
      • To compare sites, both Great Meadow and Gilmore Meadow must be selected.<br>
      • Significance testing is only available when more than three years of data are selected.<br>
      • If fewer than three years are chosen, results will display without significance testing.
    </div>
  ")
    
    # Significance info (only if available)
    if (show_significance_info()) {
      sig_results <- significance_results()
      
      if (!is.null(sig_results)) {
        sig_vars <- sig_results %>% 
          filter(significant) %>% 
          pull(variable)
        
        if (length(sig_vars) > 0) {
          # Map variable names to display names
          var_display_names <- c(
            "WL_mean" = "Mean Water Level",
            "WL_sd" = "SD Water Level", 
            "WL_min" = "Minimum Water Level",
            "WL_max" = "Maximum Water Level",
            "max_inc" = "Maximum Hourly Increase",
            "max_dec" = "Maximum Hourly Decrease",
            "GS_change" = "Growing Season Change",
            "prop_over_0cm" = "GS % Surface Water",
            "prop_bet_0_neg30cm" = "GS % Within 30cm",
            "prop_under_neg30cm" = "GS % Over 30cm Deep"
          )
          
          sig_display_names <- var_display_names[sig_vars]
          sig_display_names <- sig_display_names[!is.na(sig_display_names)]
          
          return(tagList(
            base_note,
            div(class = "significance-info", style = "margin-bottom: 25px;",
                h5(icon("asterisk"), " Statistical Significance"),
                p("Yellow highlighted variables show significant differences (p < 0.05) between Great Meadow and Gilmore Meadow wetlands:"),
                tags$ul(
                  lapply(sig_display_names, function(name) {
                    tags$li(name)
                  })
                ),
                p(class = "note", "Statistical tests compare averages across all selected years and sites within each wetland.")
            )
          ))
        }
      }
    }
    
    # Show note even if no significance info is available
    return(base_note)
  })
  
  # reactive table output for WL stats with significance highlighting
  output$wl_stats <- DT::renderDataTable({
    data <- filtered_stats()
    
    # Reorder so "All Sites" rows come first
    data <- data %>%
      dplyr::arrange(desc(Site == "All Sites"), Site)
    
    dt <- datatable(
      data,
      rownames = FALSE,
      options = list(pageLength = 10, scrollX = TRUE)
    ) %>%
      formatStyle(
        columns = names(data),  # apply to all columns
        valueColumns = "Site"
      )
    
    # Add significance highlighting only when appropriate
    if (show_significance_info()) {
      sig_results <- significance_results()
      
      if (!is.null(sig_results)) {
        # Map variable names to column names in the table
        var_mapping <- c(
          "WL_mean" = "Mean Water Level (cm)",
          "WL_sd" = "SD Water Level (cm)", 
          "WL_min" = "Minimum Water Level (cm)",
          "WL_max" = "Maximum Water Level (cm)",
          "max_inc" = "Maximum Hourly Increase (cm)",
          "max_dec" = "Maximum Hourly Decrease (cm)",
          "GS_change" = "Growing Season Change (cm)",
          "prop_over_0cm" = "GS % Surface Water",
          "prop_bet_0_neg30cm" = "GS % Within 30cm",
          "prop_under_neg30cm" = "GS % Over 30cm Deep"
        )
        
        # Apply highlighting for significant variables
        for (var_name in names(var_mapping)) {
          col_name <- var_mapping[var_name]
          sig_row <- sig_results[sig_results$variable == var_name, ]
          
          if (nrow(sig_row) > 0 && sig_row$significant) {
            dt <- dt %>%
              formatStyle(
                col_name,
                valueColumns = "Site",
                backgroundColor = styleEqual("All Sites", "#fff3cd") # light yellow highlight
              )
          }
        }
      }
    }
    
    dt
  })
  
  # Download handler for brushed data
  output$download_brush <- downloadHandler(
    filename = function() {
      paste("hydrograph_selected_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(processed_brush_data(), file, row.names = FALSE)
    }
  )
  
  # Download handler for water level stats
  output$download_stats <- downloadHandler(
    filename = function() {
      if (input$time_summary == "multi") {
        paste("wetland_averaged_stats_", Sys.Date(), ".csv", sep = "")
      } else if (input$time_summary == "all_sites") {
        paste("all_sites_significance_stats_", Sys.Date(), ".csv", sep = "")
      } else {
        paste("site_stats_", Sys.Date(), ".csv", sep = "")
      }
    },
    content = function(file) {
      write.csv(filtered_stats(), file, row.names = FALSE)
    }
  )
  
  # Download handler for hydrograph plot
  output$download_plot <- downloadHandler(
    filename = function() {
      sites_label <- paste(input$selected_sites, collapse = "_")
      years_label <- paste(input$year, collapse = "_")
      paste0("hydrograph_", sites_label, "_", years_label, "_", Sys.Date(), ".png")
    },
    content = function(file) {
      # Recreate the plot
      plot_data_filtered <- plot_data()
      req(nrow(plot_data_filtered) > 0)
      
      # Calculate minimum water level across all data for consistent precipitation baseline
      minWL <- min(plot_data_filtered$water_depth, na.rm = TRUE)
      
      # Create color palette for sites
      sites <- unique(plot_data_filtered$site)
      site_colors <- c(
        "Great Meadow 1" = "black", "Great Meadow 2" = "chartreuse4", "Great Meadow 3" = "green",
        "Great Meadow 4" = "darkorange", "Great Meadow 5" = "deeppink2", "Great Meadow 6" = "purple",
        "Gilmore Meadow" = "darkgray", "Precipitation" = "blue"
      )
      
      # Create the plot
      p <- ggplot(plot_data_filtered, aes(x = doy_h, y = water_depth)) +
        # Water level lines by site
        geom_line(aes(color = site), size = 0.7) +
        
        # Precipitation line (scaled by multiplier of 5 and offset by minWL)
        geom_line(aes(x = doy_h, y = lag_precip * 5 + minWL, color = "Precipitation"), 
                  size = 0.7) +
        
        # Ground level reference
        geom_hline(yintercept = 0, color = 'brown') +
        
        # Facet by year
        facet_wrap(~ year, ncol = 1, scales = "free_y") +
        
        # Colors
        scale_color_manual(values = site_colors, 
                           breaks = c(sites, "Precipitation")) +
        
        # Axes and labels
        labs(y = 'Water Level (cm)', x = 'Date') +
        scale_x_continuous(
          breaks = c(121, 152, 182, 213, 244, 274),
          labels = c('May-01', 'Jun-01', 'Jul-01', 'Aug-01', 'Sep-01', 'Oct-01')
        ) +
        
        # Secondary axis for precipitation
        scale_y_continuous(
          sec.axis = sec_axis(~ .,
                              breaks = c(minWL, minWL + 10),
                              name = 'Hourly Precip. (cm)',
                              labels = c('0', '2'))
        ) +
        
        # Theme
        theme_bw() +
        theme(
          plot.title = element_text(hjust = 0.5),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          axis.text.y.right = element_text(color = 'blue'),
          axis.title.y.right = element_text(color = 'blue'),
          strip.text = element_text(size = 11),
          legend.position = "bottom",
          legend.title = element_blank()
        )
      
      # Save the plot
      ggsave(file, plot = p, width = 12, height = 8, dpi = 300, bg = "white")
    }
  )
  
}

# Run app
shinyApp(ui, server)





##------------------------------------------------------------------------------




#10/17/25 Prior to making significance testing own tab--------------------------

#### Hydrograph Visualizer Shiny Dashboard #### 

#---------------------------------------------#
####        Load Required Packages         ####
#---------------------------------------------#

library(shiny)
library(tidyverse)
library(lubridate)
library(patchwork)
library(shinyWidgets)
library(DT)
library(bslib)

#-------------------------------------------#
####    Read & Prepare Processed Data    ####
#-------------------------------------------#

# Great Meadow data
gm <- read.csv("data/processed_data/great_meadow_well_data_2024_20250915.csv") %>%
  mutate(date = as.Date(date),
         timestamp = as_datetime(timestamp),
         site = paste("Great Meadow", plot.num),
         water.depth = case_when(
           year == 2016 & doy_h == 159.12 & plot.num == 3 & water.depth < -120 ~ NA_real_,
           year == 2017 & doy_h == 215.02 & plot.num == 6 & water.depth < -115 ~ NA_real_,
           year == 2021 & plot.num == 3 & doy == 224 & water.depth > 400 ~ NA_real_,
           year == 2021 & plot.num == 3 & doy == 225 & water.depth > 400 ~ NA_real_,
           TRUE ~ water.depth
         )) %>%
  mutate(siteyear = paste(site, year, sep = "_"))

# Gilmore Meadow data 
gl <- read.csv("data/raw_data/hydrology_data/gilmore_well_prec_data_2013-2024.csv") %>%
  rename(water.depth = GILM_WL) %>%
  mutate(
    site = "Gilmore Meadow",
    
    # Try to parse full datetime, fallback to date only + midnight
    timestamp_parsed = mdy_hm(timestamp),
    timestamp_parsed = if_else(
      is.na(timestamp_parsed),
      mdy(timestamp),
      timestamp_parsed
    ),
    timestamp_parsed = as.POSIXct(timestamp_parsed),
    timestamp = timestamp_parsed,
    
    # Standardize Date column
    Date = mdy(Date)
  ) %>%
  select(timestamp, 
         date = Date,
         doy,
         year = Year,
         precip.cm = precip_cm,
         water.depth,
         lag.precip,
         hr,
         doy_h,
         site)

# Need to select Great Meadow precip data as default for all data since Gilmore Meadow has precip data too
# Create precipitation lookup from Great Meadow data
gm_precip_lookup <- gm %>%
  select(timestamp, precip.cm, lag.precip) %>%
  distinct()

# Replace Gilmore precip with Great Meadow precip for matching timestamps
gl_with_gm_precip <- gl %>%
  select(-precip.cm, -lag.precip) %>%  # Remove original precip columns
  left_join(gm_precip_lookup, by = "timestamp") %>%
  # If no match found, keep original Gilmore precip values as fallback
  left_join(
    gl %>% select(timestamp, orig_precip_cm = precip.cm, orig_lag_precip = lag.precip),
    by = "timestamp"
  ) %>%
  mutate(
    precip.cm = coalesce(precip.cm, orig_precip_cm),
    lag.precip = coalesce(lag.precip, orig_lag_precip)
  ) %>%
  select(-orig_precip_cm, -orig_lag_precip)

# Combine Great Meadow and Gilmore Meadow datasets
all_data <- bind_rows(gm, gl_with_gm_precip) %>% 
  filter(year >= 2016 & year <= 2024) %>% 
  select(timestamp, 
         date, 
         year, 
         doy, 
         hr, 
         doy_h,
         precip_cm = precip.cm,
         lag_precip = lag.precip,
         water_depth = water.depth,
         site)

# Format water level stats for WL table output 
wl_stats <- read.csv("data/processed_data/gm_gl_wl_stats.csv") %>% 
  select(year,
         stat, 
         `Gilmore Meadow` = gilmore.meadow, 
         `Great Meadow 1` = great.meadow.1, 
         `Great Meadow 2` = great.meadow.2, 
         `Great Meadow 3` = great.meadow.3, 
         `Great Meadow 4` = great.meadow.4, 
         `Great Meadow 5` = great.meadow.5, 
         `Great Meadow 6` = great.meadow.6) %>% 
  pivot_longer(cols = -c(year, stat), names_to = "site", values_to = "value") %>% 
  pivot_wider(names_from = stat, values_from = value) %>%
  arrange(site, year) %>% 
  mutate(
    wetland = case_when(
      grepl("Great Meadow", site, ignore.case = TRUE) ~ "Great Meadow",
      grepl("Gilmore Meadow", site, ignore.case = TRUE) ~ "Gilmore Meadow",
      TRUE ~ "Unknown"
    )
  )

#-----------------------#
####    Functions    ####
#-----------------------#

# Function to calculate significance tests between wetlands
calculate_wetland_significance <- function(data, selected_years, selected_sites, alpha = 0.05) {
  # Check minimum number of years
  if (length(selected_years) <= 3) {
    message("⚠️ Not enough years selected for significance testing (need >3).")
    return(NULL)
  }
  
  # Filter data for selected years and sites
  filtered_data <- data %>%
    filter(year %in% selected_years, site %in% selected_sites) %>%
    mutate(site_group = ifelse(grepl("Great Meadow", site), "Great Meadow", site))
  
  # Check both wetlands are present
  wetlands_present <- unique(filtered_data$site_group)
  if (length(wetlands_present) < 2 || !all(c("Great Meadow", "Gilmore Meadow") %in% wetlands_present)) {
    message("⚠️ Both Great Meadow and Gilmore Meadow must be present for comparison.")
    return(NULL)
  }
  
  # Get numeric stat columns
  stat_cols <- filtered_data %>%
    select(where(is.numeric), -year) %>%
    names()
  
  # Step 1: average each variable by wetland × year
  yearly_means <- filtered_data %>%
    group_by(year, site_group) %>%
    summarise(across(all_of(stat_cols), mean, na.rm = TRUE), .groups = "drop")
  
  # Step 2: run t-tests across wetlands for each variable
  t_test_results <- map_dfr(stat_cols, function(var) {
    tryCatch({
      test <- t.test(as.formula(paste(var, "~ site_group")), data = yearly_means)
      data.frame(
        variable = var,
        p_value = test$p.value,
        significant = test$p.value < alpha,
        stringsAsFactors = FALSE
      )
    }, error = function(e) {
      data.frame(
        variable = var,
        p_value = NA,
        significant = FALSE,
        stringsAsFactors = FALSE
      )
    })
  })
  
  return(t_test_results)
}




#----------------#
####    UI    ####
#----------------#

ui <- page_fluid(
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#1B365D",
    secondary = "#4C6D9A",    
    success = "#2E86C1",
    info = "#3498db",
    warning = "#f39c12",
    danger = "#e74c3c",
    base_font = font_google("Open Sans"),
    heading_font = font_google("Open Sans", wght = c(400, 700))
  ),
  
  # Custom CSS for additional styling
  tags$head(
    tags$style(HTML("
      .content-section {
        margin: 30px 0;
        padding: 25px;
        border-radius: 15px;
        box-shadow: 0 4px 12px rgba(0,0,0,0.1);
        background: linear-gradient(135deg, #ffffff 0%, #f8f9fa 100%);
        border: 2px solid #1B365D;
      }
      
      .section-title {
        color: #1B365D;
        font-weight: 600;
        margin-bottom: 20px;
        padding-bottom: 10px;
        border-bottom: 2px solid #4C6D9A;
      }
      
      .sidebar-custom {
        background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%);
        border-radius: 10px;
        padding: 20px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.08);
        border: 1px solid #dee2e6;
      }
      
      .main-title {
        background: linear-gradient(135deg, #1B365D 0%, #4C6D9A 100%);
        color: white;
        padding: 30px;
        margin: -15px -15px 30px -15px;
        text-align: center;
        border-radius: 0 0 20px 20px;
        box-shadow: 0 4px 12px rgba(0,0,0,0.15);
      }
      
      .brush-info-section {
        background: linear-gradient(135deg, #e8f4f8 0%, #f0f8ff 100%);
        border-radius: 12px;
        padding: 20px;
        margin: 20px 0;
        border: 1px solid #4C6D9A;
      }
      
      .stats-main {
        background: linear-gradient(135deg, #f8f9fa 0%, #ffffff 100%);
      }
      
      .dataTables_wrapper {
        font-size: 0.85rem !important;
      }

      .dataTables_wrapper table {
        font-size: 0.8rem !important;
      }

      .dataTables_wrapper .dataTables_info,
      .dataTables_wrapper .dataTables_paginate {
        font-size: 0.75rem !important;
      }
      
    .significance-info h5 {
      background-color: #fff3cd;
      color: #856404;
      padding: 6px 10px;
      border-radius: 4px;
      display: inline-block;
      margin-bottom: 10px;
    }
    
    .significance-info p {
      margin-bottom: 8px;
      font-size: 0.9rem;
    }
    
    .significance-info ul li {
      font-size: 0.85rem;
      color: #856404;
    }
    
    .significance-info .note {
      margin-top: 5px;
      margin-bottom: 10px;
      font-size: 0.8rem;
      font-style: italic;
      color: #6c757d;
    }
      
    "))
  ),
  
  # Main title with gradient background
  div(class = "main-title",
      h1("Wetland Hydrograph Visualizer", 
         style = "margin: 0; font-size: 2rem; text-shadow: 2px 2px 4px rgba(0,0,0,0.3)")
  ),
  
  # First section: Hydrographs 
  div(class = "content-section",
      layout_sidebar(
        sidebar = sidebar(
          class = "sidebar-custom",
          width = 300,
          h4("Hydrograph Controls", style = "color: #1B365D; margin-bottom: 20px;"),
          
          pickerInput("selected_sites", 
                      label = div(icon("map-marker"), "Select Site(s):"),
                      choices = sort(unique(all_data$site)),
                      selected = c("Great Meadow 1"),
                      multiple = TRUE,
                      options = list(
                        `actions-box` = TRUE,
                        `deselect-all-text` = "Clear all",
                        `select-all-text` = "Select all",
                        `none-selected-text` = "Choose site(s)",
                        `live-search` = TRUE,
                        style = "btn-outline-primary"
                      )),
          
          div(style = "margin-bottom: 15px;",
              pickerInput("year", 
                          label = div(icon("calendar"), "Select Year(s):"),
                          choices = sort(unique(all_data$year)),
                          selected = 2024,
                          multiple = TRUE,
                          options = list(
                            `actions-box` = TRUE,
                            `deselect-all-text` = "Clear all",
                            `select-all-text` = "Select all",
                            `none-selected-text` = "Choose year(s)",
                            `live-search` = TRUE,
                            style = "btn-outline-primary"
                          ))),
          
          br(),
          div(style = "padding: 10px; background-color: #e8f4f8; border-radius: 8px; border-left: 4px solid #3498db;",
              p(icon("info-circle"), " Use the brush tool (+) by clicking and dragging with your cursor to select data on the hydrograph and view below.", 
                style = "margin: 0; font-size: 0.9rem; color: #2c3e50;")),
          
          div(style = "margin-top: 10px; text-align: center;",
              downloadButton("download_plot", "Download Hydrograph", 
                             class = "btn-primary btn-sm", 
                             icon = icon("image"))),
          
          div(style = "margin-top: 15px; text-align: center;",
              downloadButton("download_brush", "Download Selected Data", 
                             class = "btn-primary btn-sm", 
                             icon = icon("download")))
        ),
        
        card(
          full_screen = TRUE,
          card_header(
            class = "bg-primary text-white",
            "Hydrographs by Year"
          ),
          plotOutput("hydrograph", height = "600px", 
                     brush = brushOpts(id = "hydro_brush", fill = "#4C6D9A", opacity = 0.3))
        )
      )
  ),
  
  # Selected data points section 
  div(class = "brush-info-section",
      div(class = "row",
          div(class = "col-12",
              card(
                card_header(
                  class = "bg-success text-white",
                  "Selected Data from Hydrograph:"
                ),
                tableOutput("brush_info")
              )
          )
      )
  ),
  
  # Second section: Water Level Stats
  div(class = "content-section stats-main",
      layout_sidebar(
        sidebar = sidebar(
          class = "sidebar-custom",
          width = 300,
          h4("Statistics Controls", style = "color: #1B365D; margin-bottom: 20px;"),
          
          pickerInput("stats_site", 
                      label = div(icon("map-marker"), "Select Site(s):"),
                      choices = sort(unique(wl_stats$site)),
                      selected = "Great Meadow 1",
                      multiple = TRUE,
                      options = list(
                        `actions-box` = TRUE,
                        `deselect-all-text` = "Clear all",
                        `select-all-text` = "Select all",
                        `none-selected-text` = "Choose site(s)",
                        `live-search` = TRUE,
                        style = "btn-outline-primary"
                      )),
          
          div(style = "margin-bottom: 15px;",
              pickerInput("stats_year", 
                          label = div(icon("calendar"), "Select Years:"),
                          choices = sort(unique(wl_stats$year)),
                          selected = c(2024, 2023, 2022, 2021),
                          multiple = TRUE,
                          options = list(
                            `actions-box` = TRUE,
                            `deselect-all-text` = "Clear all",
                            `select-all-text` = "Select all",
                            `none-selected-text` = "Choose year(s)",
                            `live-search` = TRUE,
                            style = "btn-outline-primary"
                          ))),
          
          div(style = "margin-bottom: 15px;",
              radioButtons("time_summary", "Summarize Stats:",
                           choices = c("Each Year" = "year", "Average Across Years" = "multi"),
                           selected = "year",
                           inline = TRUE)),
          
          br(),
          div(style = "padding: 10px; background-color: #e8f4f8; border-radius: 8px; border-left: 4px solid #3498db;",
              p(icon("info-circle"), " Growing season statistics calculated from May through October.", 
                style = "margin: 0; font-size: 0.9rem; color: #2c3e50;")),
          
          div(style = "margin-top: 15px; text-align: center;",
              downloadButton("download_stats", "Download Table", 
                             class = "btn-primary btn-sm", 
                             icon = icon("download")))
        ),
        
        card(
          full_screen = TRUE,
          card_header(
            class = "bg-primary text-white",
            "Growing Season Water Level Statistics"
          ),
          
          div(style = "padding: 10px;",
              uiOutput("significance_info"),
              dataTableOutput("wl_stats"))
        )
      )
  )
)

#--------------------#
####    SERVER    ####
#--------------------#

server <- function(input, output, session) {
  
  # Reactive data for plotting
  plot_data <- reactive({
    req(input$year, input$selected_sites)
    
    all_data %>%
      filter(year %in% input$year, doy > 134, doy < 275) %>%
      filter(site %in% input$selected_sites)
  })
  
  output$hydrograph <- renderPlot({
    plot_data_filtered <- plot_data()
    req(nrow(plot_data_filtered) > 0)
    
    # Calculate minimum water level across all data for consistent precipitation baseline
    minWL <- min(plot_data_filtered$water_depth, na.rm = TRUE)
    
    # Color palette for sites
    sites <- unique(plot_data_filtered$site)
    site_colors <- c(
      "Great Meadow 1" = "black", "Great Meadow 2" = "chartreuse4", "Great Meadow 3" = "green",
      "Great Meadow 4" = "darkorange", "Great Meadow 5" = "deeppink2", "Great Meadow 6" = "purple",
      "Gilmore Meadow" = "darkgray", "Precipitation" = "blue"
    )
    
    # Create the plot
    ggplot(plot_data_filtered, aes(x = doy_h, y = water_depth)) +
      # Water level lines by site
      geom_line(aes(color = site), size = 0.7) +
      
      # Precip line (scaled by multiplier of 5 and offset by minWL)
      geom_line(aes(x = doy_h, y = lag_precip * 5 + minWL, color = "Precipitation"), 
                size = 0.7) +
      
      # Ground level reference
      geom_hline(yintercept = 0, color = 'brown') +
      
      # Facet by year
      facet_wrap(~ year, ncol = 1, scales = "free_y") +
      
      # Colors
      scale_color_manual(values = site_colors, 
                         breaks = c(sites, "Precipitation")) +
      
      # Axes and labels
      labs(y = 'Water Level (cm)', x = 'Date') +
      scale_x_continuous(
        breaks = c(121, 152, 182, 213, 244, 274),
        labels = c('May-01', 'Jun-01', 'Jul-01', 'Aug-01', 'Sep-01', 'Oct-01')
      ) +
      
      # Secondary axis for precip
      scale_y_continuous(
        sec.axis = sec_axis(~ .,
                            breaks = c(minWL, minWL + 10),
                            name = 'Hourly Precip. (cm)',
                            labels = c('0', '2'))
      ) +
      
      # Theme
      theme_bw() +
      theme(
        plot.title = element_text(hjust = 0.5),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.text.y.right = element_text(color = 'blue'),
        axis.title.y.right = element_text(color = 'blue'),
        strip.text = element_text(size = 11),
        legend.position = "bottom",
        legend.title = element_blank()
      )
  })
  
  # Reactive for processed brushed data - with site columns
  processed_brush_data <- reactive({
    req(input$hydro_brush)
    
    plot_data_filtered <- plot_data()
    req(nrow(plot_data_filtered) > 0)
    
    # Get brushed points using brushedPoints()
    selected_data <- brushedPoints(
      plot_data_filtered,
      brush = input$hydro_brush,
      xvar = "doy_h",
      yvar = "water_depth"
    ) %>%
      select(timestamp, year, doy_h, site, water_depth, lag_precip) %>%
      arrange(year, doy_h) %>%
      mutate(
        doy_h = round(doy_h, 2),
        water_depth = round(water_depth, 2),
        lag_precip = round(lag_precip, 3),
        timestamp = format(timestamp, "%Y-%m-%d %H:%M:%S")
      )
    
    if (nrow(selected_data) == 0) {
      return(data.frame(Message = "No points selected - try brushing over the water level lines"))
    }
    
    # Create the base data with unique timestamp/precip combinations
    base_data <- selected_data %>%
      group_by(year, doy_h, timestamp, lag_precip) %>%
      summarise(.groups = 'drop') %>%
      arrange(year, doy_h)
    
    # Create water depth columns for each site
    water_depth_data <- selected_data %>%
      group_by(year, doy_h, timestamp, site) %>%
      summarise(water_depth = mean(water_depth, na.rm = TRUE), .groups = 'drop') %>%
      pivot_wider(names_from = site, values_from = water_depth, names_prefix = "")
    
    # Join the data together
    result_data <- base_data %>%
      left_join(water_depth_data, by = c("year", "doy_h", "timestamp")) %>%
      rename(
        Year = year,
        Timestamp = timestamp,
        `Day of Year` = doy_h,
        `Precipitation (cm)` = lag_precip
      )
    
    # Add "Water Depth (cm)" suffix to site columns
    site_columns <- intersect(names(result_data), input$selected_sites)
    if (length(site_columns) > 0) {
      result_data <- result_data %>%
        rename_with(~ paste(.x, "Water Depth (cm)"), .cols = all_of(site_columns))
    }
    
    result_data %>% arrange(Year, `Day of Year`)
  })
  
  # Reactive for significance testing results - checks if both wetlands are selected
  significance_results <- reactive({
    req(input$stats_year, input$stats_site)
    
    # Check if both wetlands are represented in selected sites
    selected_wetlands <- wl_stats %>%
      filter(site %in% input$stats_site) %>%
      pull(wetland) %>%
      unique()
    
    # Only calculate significance if both wetlands are represented
    if (length(selected_wetlands) >= 2 && all(c("Great Meadow", "Gilmore Meadow") %in% selected_wetlands)) {
      calculate_wetland_significance(wl_stats, input$stats_year, input$stats_site, alpha = 0.05)
    } else {
      NULL
    }
  })
  
  # Reactive to determine if we should show significance info
  show_significance_info <- reactive({
    req(input$stats_site, input$time_summary)
    
    if (input$time_summary != "multi") return(FALSE)
    
    selected_wetlands <- wl_stats %>%
      filter(site %in% input$stats_site) %>%
      pull(wetland) %>%
      unique()
    
    return(length(selected_wetlands) >= 2 && all(c("Great Meadow", "Gilmore Meadow") %in% selected_wetlands))
  })
  
  
  # Setting up average WL stats and calculating stats output tables
  filtered_stats <- reactive({
    req(input$stats_site, input$stats_year, input$time_summary)
    
    data <- wl_stats %>%
      filter(site %in% input$stats_site, year %in% input$stats_year)
    
    if (input$time_summary == "year") {
      # ---- Option 1: Per-Year Summary ----
      data %>%
        mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
        select(
          Year = year,
          Site = site,
          `Mean Water Level (cm)` = WL_mean,
          `SD Water Level (cm)` = WL_sd,
          `Minimum Water Level (cm)` = WL_min,
          `Maximum Water Level (cm)` = WL_max,
          `Maximum Hourly Increase (cm)` = max_inc,
          `Maximum Hourly Decrease (cm)` = max_dec,
          `Growing Season Change (cm)` = GS_change,
          `GS % Surface Water` = prop_over_0cm,
          `GS % Within 30cm` = prop_bet_0_neg30cm,
          `GS % Over 30cm Deep` = prop_under_neg30cm
        )
    } else {
      # ---- Option 2: Average Across Years ----
      # Per-site summary
      data_site <- data %>%
        group_by(site, wetland) %>%
        summarise(
          Year = paste0(min(year), "–", max(year)),
          `Mean Water Level (cm)` = round(mean(WL_mean, na.rm = TRUE), 2),
          `SD Water Level (cm)` = round(mean(WL_sd, na.rm = TRUE), 2),
          `Minimum Water Level (cm)` = round(mean(WL_min, na.rm = TRUE), 2),
          `Maximum Water Level (cm)` = round(mean(WL_max, na.rm = TRUE), 2),
          `Maximum Hourly Increase (cm)` = round(mean(max_inc, na.rm = TRUE), 2),
          `Maximum Hourly Decrease (cm)` = round(mean(max_dec, na.rm = TRUE), 2),
          `Growing Season Change (cm)` = round(mean(GS_change, na.rm = TRUE), 2),
          `GS % Surface Water` = round(mean(prop_over_0cm, na.rm = TRUE), 2),
          `GS % Within 30cm` = round(mean(prop_bet_0_neg30cm, na.rm = TRUE), 2),
          `GS % Over 30cm Deep` = round(mean(prop_under_neg30cm, na.rm = TRUE), 2),
          .groups = "drop"
        )
      
      # Per-wetland "All Sites" summary
      data_wetland <- data %>%
        group_by(wetland) %>%
        summarise(
          site = "All Sites",
          Year = paste0(min(year), "–", max(year)),
          `Mean Water Level (cm)` = round(mean(WL_mean, na.rm = TRUE), 2),
          `SD Water Level (cm)` = round(mean(WL_sd, na.rm = TRUE), 2),
          `Minimum Water Level (cm)` = round(mean(WL_min, na.rm = TRUE), 2),
          `Maximum Water Level (cm)` = round(mean(WL_max, na.rm = TRUE), 2),
          `Maximum Hourly Increase (cm)` = round(mean(max_inc, na.rm = TRUE), 2),
          `Maximum Hourly Decrease (cm)` = round(mean(max_dec, na.rm = TRUE), 2),
          `Growing Season Change (cm)` = round(mean(GS_change, na.rm = TRUE), 2),
          `GS % Surface Water` = round(mean(prop_over_0cm, na.rm = TRUE), 2),
          `GS % Within 30cm` = round(mean(prop_bet_0_neg30cm, na.rm = TRUE), 2),
          `GS % Over 30cm Deep` = round(mean(prop_under_neg30cm, na.rm = TRUE), 2),
          .groups = "drop"
        )
      
      bind_rows(data_site, data_wetland) %>%
        distinct() %>% 
        rename(Site = site,
               Wetland = wetland)
    }
  })
  
  # Reactive table output for selected data from hydrograph
  output$brush_info <- renderTable({
    processed_brush_data()
  })
  
  # Output for significance information display
  output$significance_info <- renderUI({
    if (show_significance_info()) {
      sig_results <- significance_results()
      
      if (!is.null(sig_results)) {
        sig_vars <- sig_results %>% 
          filter(significant) %>% 
          pull(variable)
        
        if (length(sig_vars) > 0) {
          # Map variable names to display names
          var_display_names <- c(
            "WL_mean" = "Mean Water Level",
            "WL_sd" = "SD Water Level", 
            "WL_min" = "Minimum Water Level",
            "WL_max" = "Maximum Water Level",
            "max_inc" = "Maximum Hourly Increase",
            "max_dec" = "Maximum Hourly Decrease",
            "GS_change" = "Growing Season Change",
            "prop_over_0cm" = "GS % Surface Water",
            "prop_bet_0_neg30cm" = "GS % Within 30cm",
            "prop_under_neg30cm" = "GS % Over 30cm Deep"
          )
          
          sig_display_names <- var_display_names[sig_vars]
          sig_display_names <- sig_display_names[!is.na(sig_display_names)]
          
          div(class = "significance-info",
              h5(icon("asterisk"), " Statistical Significance"),
              p("Yellow highlighted variables show significant differences (p < 0.05) between Great Meadow and Gilmore Meadow wetlands:"),
              tags$ul(
                lapply(sig_display_names, function(name) {
                  tags$li(name)
                })
              ),
              p(class = "note", "Statistical tests compare averages across all selected years and sites within each wetland.")
          )
        }
      }
    }
  })
  
  # reactive table output for WL stats with significance highlighting
  output$wl_stats <- DT::renderDataTable({
    data <- filtered_stats()
    
    # Reorder so "All Sites" rows come first
    data <- data %>%
      dplyr::arrange(desc(Site == "All Sites"), Site)
    
    dt <- datatable(
      data,
      rownames = FALSE,
      options = list(pageLength = 10, scrollX = TRUE)
    ) %>%
      formatStyle(
        columns = names(data),  # apply to all columns
        valueColumns = "Site",
        backgroundColor = styleEqual("All Sites", "#d9edf7"), # light blue highlight
        fontWeight = styleEqual("All Sites", "bold")
      )
    
    # Add significance highlighting only when appropriate
    if (show_significance_info()) {
      sig_results <- significance_results()
      
      if (!is.null(sig_results)) {
        # Map variable names to column names in the table
        var_mapping <- c(
          "WL_mean" = "Mean Water Level (cm)",
          "WL_sd" = "SD Water Level (cm)", 
          "WL_min" = "Minimum Water Level (cm)",
          "WL_max" = "Maximum Water Level (cm)",
          "max_inc" = "Maximum Hourly Increase (cm)",
          "max_dec" = "Maximum Hourly Decrease (cm)",
          "GS_change" = "Growing Season Change (cm)",
          "prop_over_0cm" = "GS % Surface Water",
          "prop_bet_0_neg30cm" = "GS % Within 30cm",
          "prop_under_neg30cm" = "GS % Over 30cm Deep"
        )
        
        # Apply highlighting for significant variables
        for (var_name in names(var_mapping)) {
          col_name <- var_mapping[var_name]
          sig_row <- sig_results[sig_results$variable == var_name, ]
          
          if (nrow(sig_row) > 0 && sig_row$significant) {
            dt <- dt %>%
              formatStyle(
                col_name,
                valueColumns = "Site",
                backgroundColor = styleEqual("All Sites", "#fff3cd"), # light yellow highlight
                fontWeight = styleEqual("All Sites", "bold")
              )
          }
        }
      }
    }
    
    dt
  })
  
  # Download handler for brushed data
  output$download_brush <- downloadHandler(
    filename = function() {
      paste("hydrograph_selected_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(processed_brush_data(), file, row.names = FALSE)
    }
  )
  
  # Download handler for water level stats
  output$download_stats <- downloadHandler(
    filename = function() {
      if (input$time_summary == "multi") {
        paste("wetland_averaged_stats_", Sys.Date(), ".csv", sep = "")
      } else {
        paste("site_stats_", Sys.Date(), ".csv", sep = "")
      }
    },
    content = function(file) {
      write.csv(filtered_stats(), file, row.names = FALSE)
    }
  )
  
  # Download handler for hydrograph plot
  output$download_plot <- downloadHandler(
    filename = function() {
      sites_label <- paste(input$selected_sites, collapse = "_")
      years_label <- paste(input$year, collapse = "_")
      paste0("hydrograph_", sites_label, "_", years_label, "_", Sys.Date(), ".png")
    },
    content = function(file) {
      # Recreate the plot
      plot_data_filtered <- plot_data()
      req(nrow(plot_data_filtered) > 0)
      
      # Calculate minimum water level across all data for consistent precipitation baseline
      minWL <- min(plot_data_filtered$water_depth, na.rm = TRUE)
      
      # Create color palette for sites
      sites <- unique(plot_data_filtered$site)
      site_colors <- c(
        "Great Meadow 1" = "black", "Great Meadow 2" = "chartreuse4", "Great Meadow 3" = "green",
        "Great Meadow 4" = "darkorange", "Great Meadow 5" = "deeppink2", "Great Meadow 6" = "purple",
        "Gilmore Meadow" = "darkgray", "Precipitation" = "blue"
      )
      
      # Create the plot
      p <- ggplot(plot_data_filtered, aes(x = doy_h, y = water_depth)) +
        # Water level lines by site
        geom_line(aes(color = site), size = 0.7) +
        
        # Precipitation line (scaled by multiplier of 5 and offset by minWL)
        geom_line(aes(x = doy_h, y = lag_precip * 5 + minWL, color = "Precipitation"), 
                  size = 0.7) +
        
        # Ground level reference
        geom_hline(yintercept = 0, color = 'brown') +
        
        # Facet by year
        facet_wrap(~ year, ncol = 1, scales = "free_y") +
        
        # Colors
        scale_color_manual(values = site_colors, 
                           breaks = c(sites, "Precipitation")) +
        
        # Axes and labels
        labs(y = 'Water Level (cm)', x = 'Date') +
        scale_x_continuous(
          breaks = c(121, 152, 182, 213, 244, 274),
          labels = c('May-01', 'Jun-01', 'Jul-01', 'Aug-01', 'Sep-01', 'Oct-01')
        ) +
        
        # Secondary axis for precipitation
        scale_y_continuous(
          sec.axis = sec_axis(~ .,
                              breaks = c(minWL, minWL + 10),
                              name = 'Hourly Precip. (cm)',
                              labels = c('0', '2'))
        ) +
        
        # Theme
        theme_bw() +
        theme(
          plot.title = element_text(hjust = 0.5),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          axis.text.y.right = element_text(color = 'blue'),
          axis.title.y.right = element_text(color = 'blue'),
          strip.text = element_text(size = 11),
          legend.position = "bottom",
          legend.title = element_blank()
        )
      
      # Save the plot
      ggsave(file, plot = p, width = 12, height = 8, dpi = 300, bg = "white")
    }
  )
  
}

# Run app
shinyApp(ui, server)




#10/16/25 significance testing -------------------------------------------------

library(shiny)
library(tidyverse)
library(lubridate)
library(patchwork)
library(shinyWidgets)
library(DT)
library(bslib)

# Format water level stats for WL table output 
wl_stats <- read.csv("data/processed_data/gm_gl_wl_stats.csv") %>% 
  select(year,
         stat, 
         `Gilmore Meadow` = gilmore.meadow, 
         `Great Meadow 1` = great.meadow.1, 
         `Great Meadow 2` = great.meadow.2, 
         `Great Meadow 3` = great.meadow.3, 
         `Great Meadow 4` = great.meadow.4, 
         `Great Meadow 5` = great.meadow.5, 
         `Great Meadow 6` = great.meadow.6) %>% 
  pivot_longer(cols = -c(year, stat), names_to = "site", values_to = "value") %>% 
  pivot_wider(names_from = stat, values_from = value) %>%
  arrange(site, year) %>% 
  mutate(
    wetland = case_when(
      grepl("Great Meadow", site, ignore.case = TRUE) ~ "Great Meadow",
      grepl("Gilmore Meadow", site, ignore.case = TRUE) ~ "Gilmore Meadow",
      TRUE ~ "Unknown"
    )
  )

# Function to calculate significance tests between wetlands
calculate_wetland_significance <- function(data, selected_years, selected_sites, alpha = 0.05) {
  # Filter data for selected years and sites
  filtered_data <- data %>%
    filter(year %in% selected_years, site %in% selected_sites) %>%
    mutate(site_group = ifelse(grepl("Great Meadow", site), "Great Meadow", site))
  
  # Check both wetlands are present
  wetlands_present <- unique(filtered_data$site_group)
  if (length(wetlands_present) < 2 || !all(c("Great Meadow", "Gilmore Meadow") %in% wetlands_present)) {
    return(NULL)
  }
  
  # Get numeric stat columns
  stat_cols <- filtered_data %>% select(where(is.numeric), -year) %>% names()
  
  # Step 1: average each variable by wetland × year
  yearly_means <- filtered_data %>%
    group_by(year, site_group) %>%
    summarise(across(all_of(stat_cols), mean, na.rm = TRUE), .groups = "drop")
  
  # Step 2: run t-tests across wetlands for each variable
  t_test_results <- map_dfr(stat_cols, function(var) {
    tryCatch({
      test <- t.test(as.formula(paste(var, "~ site_group")), data = yearly_means)
      data.frame(
        variable = var,
        p_value = test$p.value,
        significant = test$p.value < alpha,
        stringsAsFactors = FALSE
      )
    }, error = function(e) {
      data.frame(
        variable = var,
        p_value = NA,
        significant = FALSE,
        stringsAsFactors = FALSE
      )
    })
  })
  
  return(t_test_results)
}


# Example test values
selected_years <- c(2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024)
selected_sites <- c("Gilmore Meadow", "Great Meadow 1", "Great Meadow 2", "Great Meadow 3", 
                    "Great Meadow 4", "Great Meadow 5", "Great Meadow 6")

# Run the function
test_results <- calculate_wetland_significance(wl_stats, selected_years, selected_sites, alpha = 0.05)

# View the output
test_results







#9/29/25 Prior to significance test change

#### Hydrograph Visualizer Shiny Dashboard #### 

#---------------------------------------------#
####        Load Required Packages         ####
#---------------------------------------------#

library(shiny)
library(tidyverse)
library(lubridate)
library(patchwork)
library(shinyWidgets)
library(DT)
library(bslib)

#-------------------------------------------#
####    Read & Prepare Processed Data    ####
#-------------------------------------------#

# Great Meadow data
gm <- read.csv("data/processed_data/great_meadow_well_data_2024_20250915.csv") %>%
  mutate(date = as.Date(date),
         timestamp = as_datetime(timestamp),
         site = paste("Great Meadow", plot.num),
         water.depth = case_when(
           year == 2016 & doy_h == 159.12 & plot.num == 3 & water.depth < -120 ~ NA_real_,
           year == 2017 & doy_h == 215.02 & plot.num == 6 & water.depth < -115 ~ NA_real_,
           year == 2021 & plot.num == 3 & doy == 224 & water.depth > 400 ~ NA_real_,
           year == 2021 & plot.num == 3 & doy == 225 & water.depth > 400 ~ NA_real_,
           TRUE ~ water.depth
         )) %>%
  mutate(siteyear = paste(site, year, sep = "_"))

# Gilmore Meadow data 
gl <- read.csv("data/raw_data/hydrology_data/gilmore_well_prec_data_2013-2024.csv") %>%
  rename(water.depth = GILM_WL) %>%
  mutate(
    site = "Gilmore Meadow",
    
    # Try to parse full datetime, fallback to date only + midnight
    timestamp_parsed = mdy_hm(timestamp),
    timestamp_parsed = if_else(
      is.na(timestamp_parsed),
      mdy(timestamp),
      timestamp_parsed
    ),
    timestamp_parsed = as.POSIXct(timestamp_parsed),
    timestamp = timestamp_parsed,
    
    # Standardize Date column
    Date = mdy(Date)
  ) %>%
  select(timestamp, 
         date = Date,
         doy,
         year = Year,
         precip.cm = precip_cm,
         water.depth,
         lag.precip,
         hr,
         doy_h,
         site)

# Need to select Great Meadow precip data as default for all data since Gilmore Meadow has precip data too
# Create precipitation lookup from Great Meadow data
gm_precip_lookup <- gm %>%
  select(timestamp, precip.cm, lag.precip) %>%
  distinct()

# Replace Gilmore precip with Great Meadow precip for matching timestamps
gl_with_gm_precip <- gl %>%
  select(-precip.cm, -lag.precip) %>%  # Remove original precip columns
  left_join(gm_precip_lookup, by = "timestamp") %>%
  # If no match found, keep original Gilmore precip values as fallback
  left_join(
    gl %>% select(timestamp, orig_precip_cm = precip.cm, orig_lag_precip = lag.precip),
    by = "timestamp"
  ) %>%
  mutate(
    precip.cm = coalesce(precip.cm, orig_precip_cm),
    lag.precip = coalesce(lag.precip, orig_lag_precip)
  ) %>%
  select(-orig_precip_cm, -orig_lag_precip)

# Combine Great Meadow and Gilmore Meadow datasets
all_data <- bind_rows(gm, gl_with_gm_precip) %>% 
  filter(year >= 2016 & year <= 2024) %>% 
  select(timestamp, 
         date, 
         year, 
         doy, 
         hr, 
         doy_h,
         precip_cm = precip.cm,
         lag_precip = lag.precip,
         water_depth = water.depth,
         site)

# Format water level stats for WL table output 
wl_stats <- read.csv("data/processed_data/gm_gl_wl_stats.csv") %>% 
  select(year,
         stat, 
         `Gilmore Meadow` = gilmore.meadow, 
         `Great Meadow 1` = great.meadow.1, 
         `Great Meadow 2` = great.meadow.2, 
         `Great Meadow 3` = great.meadow.3, 
         `Great Meadow 4` = great.meadow.4, 
         `Great Meadow 5` = great.meadow.5, 
         `Great Meadow 6` = great.meadow.6) %>% 
  pivot_longer(cols = -c(year, stat), names_to = "site", values_to = "value") %>% 
  pivot_wider(names_from = stat, values_from = value) %>%
  arrange(site, year) %>% 
  mutate(
    wetland = case_when(
      grepl("Great Meadow", site, ignore.case = TRUE) ~ "Great Meadow",
      grepl("Gilmore Meadow", site, ignore.case = TRUE) ~ "Gilmore Meadow",
      TRUE ~ "Unknown"
    )
  )

#-----------------------#
####    Functions    ####
#-----------------------#

# Function to calculate significance tests between wetlands
calculate_wetland_significance <- function(data, selected_years, selected_sites, alpha = 0.05) {
  # Filter data for selected years and sites
  filtered_data <- data %>%
    filter(year %in% selected_years, site %in% selected_sites) %>%
    mutate(site_group = ifelse(grepl("Great Meadow", site), "Great Meadow", site))
  
  # Check if both wetlands are represented in the selected sites
  wetlands_present <- unique(filtered_data$site_group)
  
  # Only run tests if both wetlands are present
  if (length(wetlands_present) < 2 || !all(c("Great Meadow", "Gilmore Meadow") %in% wetlands_present)) {
    return(NULL)  # return NULL if we can't compare between wetlands
  }
  
  # Get all numeric stat columns
  stat_cols <- filtered_data %>% select(where(is.numeric), -year) %>% names()
  
  # Run t-tests for each variable and store results
  t_test_results <- map_dfr(stat_cols, function(var) {
    formula <- as.formula(paste(var, "~ site_group"))
    tryCatch({
      test <- t.test(formula, data = filtered_data) # perform t-test
      data.frame(
        variable = var,
        p_value = test$p.value,
        significant = test$p.value < alpha,
        stringsAsFactors = FALSE
      )
    }, error = function(e) {
      data.frame(
        variable = var,
        p_value = NA,
        significant = FALSE,
        stringsAsFactors = FALSE
      )
    })
  })
  
  return(t_test_results)
}


#----------------#
####    UI    ####
#----------------#

ui <- page_fluid(
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#1B365D",
    secondary = "#4C6D9A",    
    success = "#2E86C1",
    info = "#3498db",
    warning = "#f39c12",
    danger = "#e74c3c",
    base_font = font_google("Open Sans"),
    heading_font = font_google("Open Sans", wght = c(400, 700))
  ),
  
  # Custom CSS for additional styling
  tags$head(
    tags$style(HTML("
      .content-section {
        margin: 30px 0;
        padding: 25px;
        border-radius: 15px;
        box-shadow: 0 4px 12px rgba(0,0,0,0.1);
        background: linear-gradient(135deg, #ffffff 0%, #f8f9fa 100%);
        border: 2px solid #1B365D;
      }
      
      .section-title {
        color: #1B365D;
        font-weight: 600;
        margin-bottom: 20px;
        padding-bottom: 10px;
        border-bottom: 2px solid #4C6D9A;
      }
      
      .sidebar-custom {
        background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%);
        border-radius: 10px;
        padding: 20px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.08);
        border: 1px solid #dee2e6;
      }
      
      .main-title {
        background: linear-gradient(135deg, #1B365D 0%, #4C6D9A 100%);
        color: white;
        padding: 30px;
        margin: -15px -15px 30px -15px;
        text-align: center;
        border-radius: 0 0 20px 20px;
        box-shadow: 0 4px 12px rgba(0,0,0,0.15);
      }
      
      .brush-info-section {
        background: linear-gradient(135deg, #e8f4f8 0%, #f0f8ff 100%);
        border-radius: 12px;
        padding: 20px;
        margin: 20px 0;
        border: 1px solid #4C6D9A;
      }
      
      .stats-main {
        background: linear-gradient(135deg, #f8f9fa 0%, #ffffff 100%);
      }
      
      .dataTables_wrapper {
        font-size: 0.85rem !important;
      }

      .dataTables_wrapper table {
        font-size: 0.8rem !important;
      }

      .dataTables_wrapper .dataTables_info,
      .dataTables_wrapper .dataTables_paginate {
        font-size: 0.75rem !important;
      }
      
    .significance-info h5 {
      background-color: #fff3cd;
      color: #856404;
      padding: 6px 10px;
      border-radius: 4px;
      display: inline-block;
      margin-bottom: 10px;
    }
    
    .significance-info p {
      margin-bottom: 8px;
      font-size: 0.9rem;
    }
    
    .significance-info ul li {
      font-size: 0.85rem;
      color: #856404;
    }
    
    .significance-info .note {
      margin-top: 5px;
      margin-bottom: 10px;
      font-size: 0.8rem;
      font-style: italic;
      color: #6c757d;
    }
      
    "))
  ),
  
  # Main title with gradient background
  div(class = "main-title",
      h1("Wetland Hydrograph Visualizer", 
         style = "margin: 0; font-size: 2rem; text-shadow: 2px 2px 4px rgba(0,0,0,0.3)")
  ),
  
  # First section: Hydrographs 
  div(class = "content-section",
      layout_sidebar(
        sidebar = sidebar(
          class = "sidebar-custom",
          width = 300,
          h4("Hydrograph Controls", style = "color: #1B365D; margin-bottom: 20px;"),
          
          pickerInput("selected_sites", 
                      label = div(icon("map-marker"), "Select Site(s):"),
                      choices = sort(unique(all_data$site)),
                      selected = c("Great Meadow 1"),
                      multiple = TRUE,
                      options = list(
                        `actions-box` = TRUE,
                        `deselect-all-text` = "Clear all",
                        `select-all-text` = "Select all",
                        `none-selected-text` = "Choose site(s)",
                        `live-search` = TRUE,
                        style = "btn-outline-primary"
                      )),
          
          div(style = "margin-bottom: 15px;",
              pickerInput("year", 
                          label = div(icon("calendar"), "Select Year(s):"),
                          choices = sort(unique(all_data$year)),
                          selected = 2024,
                          multiple = TRUE,
                          options = list(
                            `actions-box` = TRUE,
                            `deselect-all-text` = "Clear all",
                            `select-all-text` = "Select all",
                            `none-selected-text` = "Choose year(s)",
                            `live-search` = TRUE,
                            style = "btn-outline-primary"
                          ))),
          
          br(),
          div(style = "padding: 10px; background-color: #e8f4f8; border-radius: 8px; border-left: 4px solid #3498db;",
              p(icon("info-circle"), " Use the brush tool (+) by clicking and dragging with your cursor to select data on the hydrograph and view below.", 
                style = "margin: 0; font-size: 0.9rem; color: #2c3e50;")),
          
          div(style = "margin-top: 10px; text-align: center;",
              downloadButton("download_plot", "Download Hydrograph", 
                             class = "btn-primary btn-sm", 
                             icon = icon("image"))),
          
          div(style = "margin-top: 15px; text-align: center;",
              downloadButton("download_brush", "Download Selected Data", 
                             class = "btn-primary btn-sm", 
                             icon = icon("download")))
        ),
        
        card(
          full_screen = TRUE,
          card_header(
            class = "bg-primary text-white",
            "Hydrographs by Year"
          ),
          plotOutput("hydrograph", height = "600px", 
                     brush = brushOpts(id = "hydro_brush", fill = "#4C6D9A", opacity = 0.3))
        )
      )
  ),
  
  # Selected data points section 
  div(class = "brush-info-section",
      div(class = "row",
          div(class = "col-12",
              card(
                card_header(
                  class = "bg-success text-white",
                  "Selected Data from Hydrograph:"
                ),
                tableOutput("brush_info")
              )
          )
      )
  ),
  
  # Second section: Water Level Stats
  div(class = "content-section stats-main",
      layout_sidebar(
        sidebar = sidebar(
          class = "sidebar-custom",
          width = 300,
          h4("Statistics Controls", style = "color: #1B365D; margin-bottom: 20px;"),
          
          pickerInput("stats_site", 
                      label = div(icon("map-marker"), "Select Site(s):"),
                      choices = sort(unique(wl_stats$site)),
                      selected = "Great Meadow 1",
                      multiple = TRUE,
                      options = list(
                        `actions-box` = TRUE,
                        `deselect-all-text` = "Clear all",
                        `select-all-text` = "Select all",
                        `none-selected-text` = "Choose site(s)",
                        `live-search` = TRUE,
                        style = "btn-outline-primary"
                      )),
          
          div(style = "margin-bottom: 15px;",
              pickerInput("stats_year", 
                          label = div(icon("calendar"), "Select Years:"),
                          choices = sort(unique(wl_stats$year)),
                          selected = c(2024, 2023, 2022, 2021),
                          multiple = TRUE,
                          options = list(
                            `actions-box` = TRUE,
                            `deselect-all-text` = "Clear all",
                            `select-all-text` = "Select all",
                            `none-selected-text` = "Choose year(s)",
                            `live-search` = TRUE,
                            style = "btn-outline-primary"
                          ))),
          
          div(style = "margin-bottom: 15px;",
              radioButtons("time_summary", "Summarize Stats:",
                           choices = c("Each Year" = "year", "Average Across Years" = "multi"),
                           selected = "year",
                           inline = TRUE)),
          
          br(),
          div(style = "padding: 10px; background-color: #e8f4f8; border-radius: 8px; border-left: 4px solid #3498db;",
              p(icon("info-circle"), " Growing season statistics calculated from May through October.", 
                style = "margin: 0; font-size: 0.9rem; color: #2c3e50;")),
          
          div(style = "margin-top: 15px; text-align: center;",
              downloadButton("download_stats", "Download Table", 
                             class = "btn-primary btn-sm", 
                             icon = icon("download")))
        ),
        
        card(
          full_screen = TRUE,
          card_header(
            class = "bg-primary text-white",
            "Growing Season Water Level Statistics"
          ),
          
          div(style = "padding: 10px;",
              uiOutput("significance_info"),
              dataTableOutput("wl_stats"))
        )
      )
  )
)

#--------------------#
####    SERVER    ####
#--------------------#

server <- function(input, output, session) {
  
  # Reactive data for plotting
  plot_data <- reactive({
    req(input$year, input$selected_sites)
    
    all_data %>%
      filter(year %in% input$year, doy > 134, doy < 275) %>%
      filter(site %in% input$selected_sites)
  })
  
  output$hydrograph <- renderPlot({
    plot_data_filtered <- plot_data()
    req(nrow(plot_data_filtered) > 0)
    
    # Calculate minimum water level across all data for consistent precipitation baseline
    minWL <- min(plot_data_filtered$water_depth, na.rm = TRUE)
    
    # Color palette for sites
    sites <- unique(plot_data_filtered$site)
    site_colors <- c(
      "Great Meadow 1" = "black", "Great Meadow 2" = "chartreuse4", "Great Meadow 3" = "green",
      "Great Meadow 4" = "darkorange", "Great Meadow 5" = "deeppink2", "Great Meadow 6" = "purple",
      "Gilmore Meadow" = "darkgray", "Precipitation" = "blue"
    )
    
    # Create the plot
    ggplot(plot_data_filtered, aes(x = doy_h, y = water_depth)) +
      # Water level lines by site
      geom_line(aes(color = site), size = 0.7) +
      
      # Precip line (scaled by multiplier of 5 and offset by minWL)
      geom_line(aes(x = doy_h, y = lag_precip * 5 + minWL, color = "Precipitation"), 
                size = 0.7) +
      
      # Ground level reference
      geom_hline(yintercept = 0, color = 'brown') +
      
      # Facet by year
      facet_wrap(~ year, ncol = 1, scales = "free_y") +
      
      # Colors
      scale_color_manual(values = site_colors, 
                         breaks = c(sites, "Precipitation")) +
      
      # Axes and labels
      labs(y = 'Water Level (cm)', x = 'Date') +
      scale_x_continuous(
        breaks = c(121, 152, 182, 213, 244, 274),
        labels = c('May-01', 'Jun-01', 'Jul-01', 'Aug-01', 'Sep-01', 'Oct-01')
      ) +
      
      # Secondary axis for precip
      scale_y_continuous(
        sec.axis = sec_axis(~ .,
                            breaks = c(minWL, minWL + 10),
                            name = 'Hourly Precip. (cm)',
                            labels = c('0', '2'))
      ) +
      
      # Theme
      theme_bw() +
      theme(
        plot.title = element_text(hjust = 0.5),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.text.y.right = element_text(color = 'blue'),
        axis.title.y.right = element_text(color = 'blue'),
        strip.text = element_text(size = 11),
        legend.position = "bottom",
        legend.title = element_blank()
      )
  })
  
  # Reactive for processed brushed data - with site columns
  processed_brush_data <- reactive({
    req(input$hydro_brush)
    
    plot_data_filtered <- plot_data()
    req(nrow(plot_data_filtered) > 0)
    
    # Get brushed points using brushedPoints()
    selected_data <- brushedPoints(
      plot_data_filtered,
      brush = input$hydro_brush,
      xvar = "doy_h",
      yvar = "water_depth"
    ) %>%
      select(timestamp, year, doy_h, site, water_depth, lag_precip) %>%
      arrange(year, doy_h) %>%
      mutate(
        doy_h = round(doy_h, 2),
        water_depth = round(water_depth, 2),
        lag_precip = round(lag_precip, 3),
        timestamp = format(timestamp, "%Y-%m-%d %H:%M:%S")
      )
    
    if (nrow(selected_data) == 0) {
      return(data.frame(Message = "No points selected - try brushing over the water level lines"))
    }
    
    # Create the base data with unique timestamp/precip combinations
    base_data <- selected_data %>%
      group_by(year, doy_h, timestamp, lag_precip) %>%
      summarise(.groups = 'drop') %>%
      arrange(year, doy_h)
    
    # Create water depth columns for each site
    water_depth_data <- selected_data %>%
      group_by(year, doy_h, timestamp, site) %>%
      summarise(water_depth = mean(water_depth, na.rm = TRUE), .groups = 'drop') %>%
      pivot_wider(names_from = site, values_from = water_depth, names_prefix = "")
    
    # Join the data together
    result_data <- base_data %>%
      left_join(water_depth_data, by = c("year", "doy_h", "timestamp")) %>%
      rename(
        Year = year,
        Timestamp = timestamp,
        `Day of Year` = doy_h,
        `Precipitation (cm)` = lag_precip
      )
    
    # Add "Water Depth (cm)" suffix to site columns
    site_columns <- intersect(names(result_data), input$selected_sites)
    if (length(site_columns) > 0) {
      result_data <- result_data %>%
        rename_with(~ paste(.x, "Water Depth (cm)"), .cols = all_of(site_columns))
    }
    
    result_data %>% arrange(Year, `Day of Year`)
  })
  
  # Reactive for significance testing results - checks if both wetlands are selected
  significance_results <- reactive({
    req(input$stats_year, input$stats_site)
    
    # Check if both wetlands are represented in selected sites
    selected_wetlands <- wl_stats %>%
      filter(site %in% input$stats_site) %>%
      pull(wetland) %>%
      unique()
    
    # Only calculate significance if both wetlands are represented
    if (length(selected_wetlands) >= 2 && all(c("Great Meadow", "Gilmore Meadow") %in% selected_wetlands)) {
      calculate_wetland_significance(wl_stats, input$stats_year, input$stats_site, alpha = 0.05)
    } else {
      NULL
    }
  })
  
  # Reactive to determine if we should show significance info
  show_significance_info <- reactive({
    req(input$stats_site, input$time_summary)
    
    if (input$time_summary != "multi") return(FALSE)
    
    selected_wetlands <- wl_stats %>%
      filter(site %in% input$stats_site) %>%
      pull(wetland) %>%
      unique()
    
    return(length(selected_wetlands) >= 2 && all(c("Great Meadow", "Gilmore Meadow") %in% selected_wetlands))
  })
  
  
  # Setting up average WL stats and calculating stats output tables
  filtered_stats <- reactive({
    req(input$stats_site, input$stats_year, input$time_summary)
    
    data <- wl_stats %>%
      filter(site %in% input$stats_site, year %in% input$stats_year)
    
    if (input$time_summary == "year") {
      # ---- Option 1: Per-Year Summary ----
      data %>%
        mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
        select(
          Year = year,
          Site = site,
          `Mean Water Level (cm)` = WL_mean,
          `SD Water Level (cm)` = WL_sd,
          `Minimum Water Level (cm)` = WL_min,
          `Maximum Water Level (cm)` = WL_max,
          `Maximum Hourly Increase (cm)` = max_inc,
          `Maximum Hourly Decrease (cm)` = max_dec,
          `Growing Season Change (cm)` = GS_change,
          `GS % Surface Water` = prop_over_0cm,
          `GS % Within 30cm` = prop_bet_0_neg30cm,
          `GS % Over 30cm Deep` = prop_under_neg30cm
        )
    } else {
      # ---- Option 2: Average Across Years ----
      # Per-site summary
      data_site <- data %>%
        group_by(site, wetland) %>%
        summarise(
          Year = paste0(min(year), "–", max(year)),
          `Mean Water Level (cm)` = round(mean(WL_mean, na.rm = TRUE), 2),
          `SD Water Level (cm)` = round(mean(WL_sd, na.rm = TRUE), 2),
          `Minimum Water Level (cm)` = round(mean(WL_min, na.rm = TRUE), 2),
          `Maximum Water Level (cm)` = round(mean(WL_max, na.rm = TRUE), 2),
          `Maximum Hourly Increase (cm)` = round(mean(max_inc, na.rm = TRUE), 2),
          `Maximum Hourly Decrease (cm)` = round(mean(max_dec, na.rm = TRUE), 2),
          `Growing Season Change (cm)` = round(mean(GS_change, na.rm = TRUE), 2),
          `GS % Surface Water` = round(mean(prop_over_0cm, na.rm = TRUE), 2),
          `GS % Within 30cm` = round(mean(prop_bet_0_neg30cm, na.rm = TRUE), 2),
          `GS % Over 30cm Deep` = round(mean(prop_under_neg30cm, na.rm = TRUE), 2),
          .groups = "drop"
        )
      
      # Per-wetland "All Sites" summary
      data_wetland <- data %>%
        group_by(wetland) %>%
        summarise(
          site = "All Sites",
          Year = paste0(min(year), "–", max(year)),
          `Mean Water Level (cm)` = round(mean(WL_mean, na.rm = TRUE), 2),
          `SD Water Level (cm)` = round(mean(WL_sd, na.rm = TRUE), 2),
          `Minimum Water Level (cm)` = round(mean(WL_min, na.rm = TRUE), 2),
          `Maximum Water Level (cm)` = round(mean(WL_max, na.rm = TRUE), 2),
          `Maximum Hourly Increase (cm)` = round(mean(max_inc, na.rm = TRUE), 2),
          `Maximum Hourly Decrease (cm)` = round(mean(max_dec, na.rm = TRUE), 2),
          `Growing Season Change (cm)` = round(mean(GS_change, na.rm = TRUE), 2),
          `GS % Surface Water` = round(mean(prop_over_0cm, na.rm = TRUE), 2),
          `GS % Within 30cm` = round(mean(prop_bet_0_neg30cm, na.rm = TRUE), 2),
          `GS % Over 30cm Deep` = round(mean(prop_under_neg30cm, na.rm = TRUE), 2),
          .groups = "drop"
        )
      
      bind_rows(data_site, data_wetland) %>%
        distinct() %>% 
        rename(Site = site,
               Wetland = wetland)
    }
  })
  
  # Reactive table output for selected data from hydrograph
  output$brush_info <- renderTable({
    processed_brush_data()
  })
  
  # Output for significance information display
  output$significance_info <- renderUI({
    if (show_significance_info()) {
      sig_results <- significance_results()
      
      if (!is.null(sig_results)) {
        sig_vars <- sig_results %>% 
          filter(significant) %>% 
          pull(variable)
        
        if (length(sig_vars) > 0) {
          # Map variable names to display names
          var_display_names <- c(
            "WL_mean" = "Mean Water Level",
            "WL_sd" = "SD Water Level", 
            "WL_min" = "Minimum Water Level",
            "WL_max" = "Maximum Water Level",
            "max_inc" = "Maximum Hourly Increase",
            "max_dec" = "Maximum Hourly Decrease",
            "GS_change" = "Growing Season Change",
            "prop_over_0cm" = "GS % Surface Water",
            "prop_bet_0_neg30cm" = "GS % Within 30cm",
            "prop_under_neg30cm" = "GS % Over 30cm Deep"
          )
          
          sig_display_names <- var_display_names[sig_vars]
          sig_display_names <- sig_display_names[!is.na(sig_display_names)]
          
          div(class = "significance-info",
              h5(icon("asterisk"), " Statistical Significance"),
              p("Yellow highlighted variables show significant differences (p < 0.05) between Great Meadow and Gilmore Meadow wetlands:"),
              tags$ul(
                lapply(sig_display_names, function(name) {
                  tags$li(name)
                })
              ),
              p(class = "note", "Statistical tests compare averages across all selected years and sites within each wetland.")
          )
        }
      }
    }
  })
  
  # reactive table output for WL stats with significance highlighting
  output$wl_stats <- DT::renderDataTable({
    data <- filtered_stats()
    
    # Reorder so "All Sites" rows come first
    data <- data %>%
      dplyr::arrange(desc(Site == "All Sites"), Site)
    
    dt <- datatable(
      data,
      rownames = FALSE,
      options = list(pageLength = 10, scrollX = TRUE)
    ) %>%
      formatStyle(
        columns = names(data),  # apply to all columns
        valueColumns = "Site",
        backgroundColor = styleEqual("All Sites", "#d9edf7"), # light blue highlight
        fontWeight = styleEqual("All Sites", "bold")
      )
    
    # Add significance highlighting only when appropriate
    if (show_significance_info()) {
      sig_results <- significance_results()
      
      if (!is.null(sig_results)) {
        # Map variable names to column names in the table
        var_mapping <- c(
          "WL_mean" = "Mean Water Level (cm)",
          "WL_sd" = "SD Water Level (cm)", 
          "WL_min" = "Minimum Water Level (cm)",
          "WL_max" = "Maximum Water Level (cm)",
          "max_inc" = "Maximum Hourly Increase (cm)",
          "max_dec" = "Maximum Hourly Decrease (cm)",
          "GS_change" = "Growing Season Change (cm)",
          "prop_over_0cm" = "GS % Surface Water",
          "prop_bet_0_neg30cm" = "GS % Within 30cm",
          "prop_under_neg30cm" = "GS % Over 30cm Deep"
        )
        
        # Apply highlighting for significant variables
        for (var_name in names(var_mapping)) {
          col_name <- var_mapping[var_name]
          sig_row <- sig_results[sig_results$variable == var_name, ]
          
          if (nrow(sig_row) > 0 && sig_row$significant) {
            dt <- dt %>%
              formatStyle(
                col_name,
                valueColumns = "Site",
                backgroundColor = styleEqual("All Sites", "#fff3cd"), # light yellow highlight
                fontWeight = styleEqual("All Sites", "bold")
              )
          }
        }
      }
    }
    
    dt
  })
  
  # Download handler for brushed data
  output$download_brush <- downloadHandler(
    filename = function() {
      paste("hydrograph_selected_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(processed_brush_data(), file, row.names = FALSE)
    }
  )
  
  # Download handler for water level stats
  output$download_stats <- downloadHandler(
    filename = function() {
      if (input$time_summary == "multi") {
        paste("wetland_averaged_stats_", Sys.Date(), ".csv", sep = "")
      } else {
        paste("site_stats_", Sys.Date(), ".csv", sep = "")
      }
    },
    content = function(file) {
      write.csv(filtered_stats(), file, row.names = FALSE)
    }
  )
  
  # Download handler for hydrograph plot
  output$download_plot <- downloadHandler(
    filename = function() {
      sites_label <- paste(input$selected_sites, collapse = "_")
      years_label <- paste(input$year, collapse = "_")
      paste0("hydrograph_", sites_label, "_", years_label, "_", Sys.Date(), ".png")
    },
    content = function(file) {
      # Recreate the plot
      plot_data_filtered <- plot_data()
      req(nrow(plot_data_filtered) > 0)
      
      # Calculate minimum water level across all data for consistent precipitation baseline
      minWL <- min(plot_data_filtered$water_depth, na.rm = TRUE)
      
      # Create color palette for sites
      sites <- unique(plot_data_filtered$site)
      site_colors <- c(
        "Great Meadow 1" = "black", "Great Meadow 2" = "chartreuse4", "Great Meadow 3" = "green",
        "Great Meadow 4" = "darkorange", "Great Meadow 5" = "deeppink2", "Great Meadow 6" = "purple",
        "Gilmore Meadow" = "darkgray", "Precipitation" = "blue"
      )
      
      # Create the plot
      p <- ggplot(plot_data_filtered, aes(x = doy_h, y = water_depth)) +
        # Water level lines by site
        geom_line(aes(color = site), size = 0.7) +
        
        # Precipitation line (scaled by multiplier of 5 and offset by minWL)
        geom_line(aes(x = doy_h, y = lag_precip * 5 + minWL, color = "Precipitation"), 
                  size = 0.7) +
        
        # Ground level reference
        geom_hline(yintercept = 0, color = 'brown') +
        
        # Facet by year
        facet_wrap(~ year, ncol = 1, scales = "free_y") +
        
        # Colors
        scale_color_manual(values = site_colors, 
                           breaks = c(sites, "Precipitation")) +
        
        # Axes and labels
        labs(y = 'Water Level (cm)', x = 'Date') +
        scale_x_continuous(
          breaks = c(121, 152, 182, 213, 244, 274),
          labels = c('May-01', 'Jun-01', 'Jul-01', 'Aug-01', 'Sep-01', 'Oct-01')
        ) +
        
        # Secondary axis for precipitation
        scale_y_continuous(
          sec.axis = sec_axis(~ .,
                              breaks = c(minWL, minWL + 10),
                              name = 'Hourly Precip. (cm)',
                              labels = c('0', '2'))
        ) +
        
        # Theme
        theme_bw() +
        theme(
          plot.title = element_text(hjust = 0.5),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          axis.text.y.right = element_text(color = 'blue'),
          axis.title.y.right = element_text(color = 'blue'),
          strip.text = element_text(size = 11),
          legend.position = "bottom",
          legend.title = element_blank()
        )
      
      # Save the plot
      ggsave(file, plot = p, width = 12, height = 8, dpi = 300, bg = "white")
    }
  )
  
}

# Run app
shinyApp(ui, server)








#9/16/25 (update annotations and moved sig info css styling)
#### Hydrograph Visualizer Shiny Dashboard #### 

#---------------------------------------------#
####        Load Required Packages         ####
#---------------------------------------------#

library(shiny)
library(tidyverse)
library(lubridate)
library(patchwork)
library(shinyWidgets)
library(DT)
library(bslib)

#-------------------------------------------#
####    Read & Prepare Processed Data    ####
#-------------------------------------------#

# Great Meadow data
gm <- read.csv("data/processed_data/great_meadow_well_data_2024_20250915.csv") %>%
  mutate(date = as.Date(date),
         timestamp = as_datetime(timestamp),
         site = paste("Great Meadow", plot.num),
         water.depth = case_when(
           year == 2016 & doy_h == 159.12 & plot.num == 3 & water.depth < -120 ~ NA_real_,
           year == 2017 & doy_h == 215.02 & plot.num == 6 & water.depth < -115 ~ NA_real_,
           year == 2021 & plot.num == 3 & doy == 224 & water.depth > 400 ~ NA_real_,
           year == 2021 & plot.num == 3 & doy == 225 & water.depth > 400 ~ NA_real_,
           TRUE ~ water.depth
         )) %>%
  mutate(siteyear = paste(site, year, sep = "_"))

# Gilmore Meadow data 
gl <- read.csv("data/raw_data/hydrology_data/gilmore_well_prec_data_2013-2024.csv") %>%
  rename(water.depth = GILM_WL) %>%
  mutate(
    site = "Gilmore Meadow",
    
    # Try to parse full datetime, fallback to date only + midnight
    timestamp_parsed = mdy_hm(timestamp),
    timestamp_parsed = if_else(
      is.na(timestamp_parsed),
      mdy(timestamp),
      timestamp_parsed
    ),
    timestamp_parsed = as.POSIXct(timestamp_parsed),
    timestamp = timestamp_parsed,
    
    # Standardize Date column
    Date = mdy(Date)
  ) %>%
  select(timestamp, 
         date = Date,
         doy,
         year = Year,
         precip.cm = precip_cm,
         water.depth,
         lag.precip,
         hr,
         doy_h,
         site)

# Need to select Great Meadow precip data as default for all data since Gilmore Meadow has precip data too
# Create precipitation lookup from Great Meadow data
gm_precip_lookup <- gm %>%
  select(timestamp, precip.cm, lag.precip) %>%
  distinct()

# Replace Gilmore precip with Great Meadow precip for matching timestamps
gl_with_gm_precip <- gl %>%
  select(-precip.cm, -lag.precip) %>%  # Remove original precip columns
  left_join(gm_precip_lookup, by = "timestamp") %>%
  # If no match found, keep original Gilmore precip values as fallback
  left_join(
    gl %>% select(timestamp, orig_precip_cm = precip.cm, orig_lag_precip = lag.precip),
    by = "timestamp"
  ) %>%
  mutate(
    precip.cm = coalesce(precip.cm, orig_precip_cm),
    lag.precip = coalesce(lag.precip, orig_lag_precip)
  ) %>%
  select(-orig_precip_cm, -orig_lag_precip)

# Combine Great Meadow and Gilmore Meadow datasets
all_data <- bind_rows(gm, gl_with_gm_precip) %>% 
  filter(year >= 2016 & year <= 2024) %>% 
  select(timestamp, 
         date, 
         year, 
         doy, 
         hr, 
         doy_h,
         precip_cm = precip.cm,
         lag_precip = lag.precip,
         water_depth = water.depth,
         site)

# Format water level stats for wl table output 
wl_stats <- read.csv("data/processed_data/gm_gl_wl_stats.csv") %>% 
  select(year,
         stat, 
         `Gilmore Meadow` = gilmore.meadow, 
         `Great Meadow 1` = great.meadow.1, 
         `Great Meadow 2` = great.meadow.2, 
         `Great Meadow 3` = great.meadow.3, 
         `Great Meadow 4` = great.meadow.4, 
         `Great Meadow 5` = great.meadow.5, 
         `Great Meadow 6` = great.meadow.6) %>% 
  pivot_longer(cols = -c(year, stat), names_to = "site", values_to = "value") %>% 
  pivot_wider(names_from = stat, values_from = value) %>%
  arrange(site, year) %>% 
  mutate(
    wetland = case_when(
      grepl("Great Meadow", site, ignore.case = TRUE) ~ "Great Meadow",
      grepl("Gilmore Meadow", site, ignore.case = TRUE) ~ "Gilmore Meadow",
      TRUE ~ "Unknown"
    )
  )

#-----------------------#
####    Functions    ####
#-----------------------#

# Function to calculate significance tests between wetlands
calculate_wetland_significance <- function(data, selected_years, selected_sites, alpha = 0.05) {
  # Filter data for selected years and sites
  filtered_data <- data %>%
    filter(year %in% selected_years, site %in% selected_sites) %>%
    mutate(site_group = ifelse(grepl("Great Meadow", site), "Great Meadow", site))
  
  # Check if both wetlands are represented in the selected sites
  wetlands_present <- unique(filtered_data$site_group)
  
  # Only run tests if both wetlands are present
  if (length(wetlands_present) < 2 || !all(c("Great Meadow", "Gilmore Meadow") %in% wetlands_present)) {
    return(NULL)  # return NULL if we can't compare between wetlands
  }
  
  # Get all numeric stat columns
  stat_cols <- filtered_data %>% select(where(is.numeric), -year) %>% names()
  
  # Run t-tests for each variable and store results
  t_test_results <- map_dfr(stat_cols, function(var) {
    formula <- as.formula(paste(var, "~ site_group"))
    tryCatch({
      test <- t.test(formula, data = filtered_data) # perform t-test
      data.frame(
        variable = var,
        p_value = test$p.value,
        significant = test$p.value < alpha,
        stringsAsFactors = FALSE
      )
    }, error = function(e) {
      data.frame(
        variable = var,
        p_value = NA,
        significant = FALSE,
        stringsAsFactors = FALSE
      )
    })
  })
  
  return(t_test_results)
}


#----------------#
####    UI    ####
#----------------#

ui <- page_fluid(
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#1B365D",
    secondary = "#4C6D9A",    
    success = "#2E86C1",
    info = "#3498db",
    warning = "#f39c12",
    danger = "#e74c3c",
    base_font = font_google("Open Sans"),
    heading_font = font_google("Open Sans", wght = c(400, 700))
  ),
  
  # Custom CSS for additional styling
  tags$head(
    tags$style(HTML("
      .content-section {
        margin: 30px 0;
        padding: 25px;
        border-radius: 15px;
        box-shadow: 0 4px 12px rgba(0,0,0,0.1);
        background: linear-gradient(135deg, #ffffff 0%, #f8f9fa 100%);
        border: 2px solid #1B365D;
      }
      
      .section-title {
        color: #1B365D;
        font-weight: 600;
        margin-bottom: 20px;
        padding-bottom: 10px;
        border-bottom: 2px solid #4C6D9A;
      }
      
      .sidebar-custom {
        background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%);
        border-radius: 10px;
        padding: 20px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.08);
        border: 1px solid #dee2e6;
      }
      
      .main-title {
        background: linear-gradient(135deg, #1B365D 0%, #4C6D9A 100%);
        color: white;
        padding: 30px;
        margin: -15px -15px 30px -15px;
        text-align: center;
        border-radius: 0 0 20px 20px;
        box-shadow: 0 4px 12px rgba(0,0,0,0.15);
      }
      
      .brush-info-section {
        background: linear-gradient(135deg, #e8f4f8 0%, #f0f8ff 100%);
        border-radius: 12px;
        padding: 20px;
        margin: 20px 0;
        border: 1px solid #4C6D9A;
      }
      
      .stats-main {
        background: linear-gradient(135deg, #f8f9fa 0%, #ffffff 100%);
      }
      
      .dataTables_wrapper {
        font-size: 0.85rem !important;
      }

      .dataTables_wrapper table {
        font-size: 0.8rem !important;
      }

      .dataTables_wrapper .dataTables_info,
      .dataTables_wrapper .dataTables_paginate {
        font-size: 0.75rem !important;
      }
      
    "))
  ),
  
  # Main title with gradient background
  div(class = "main-title",
      h1("Wetland Hydrograph Visualizer", 
         style = "margin: 0; font-size: 2rem; text-shadow: 2px 2px 4px rgba(0,0,0,0.3)")
  ),
  
  # First section: Hydrographs 
  div(class = "content-section",
      layout_sidebar(
        sidebar = sidebar(
          class = "sidebar-custom",
          width = 300,
          h4("Hydrograph Controls", style = "color: #1B365D; margin-bottom: 20px;"),
          
          pickerInput("selected_sites", 
                      label = div(icon("map-marker"), "Select Site(s):"),
                      choices = sort(unique(all_data$site)),
                      selected = c("Great Meadow 1"),
                      multiple = TRUE,
                      options = list(
                        `actions-box` = TRUE,
                        `deselect-all-text` = "Clear all",
                        `select-all-text` = "Select all",
                        `none-selected-text` = "Choose site(s)",
                        `live-search` = TRUE,
                        style = "btn-outline-primary"
                      )),
          
          div(style = "margin-bottom: 15px;",
              pickerInput("year", 
                          label = div(icon("calendar"), "Select Year(s):"),
                          choices = sort(unique(all_data$year)),
                          selected = 2024,
                          multiple = TRUE,
                          options = list(
                            `actions-box` = TRUE,
                            `deselect-all-text` = "Clear all",
                            `select-all-text` = "Select all",
                            `none-selected-text` = "Choose year(s)",
                            `live-search` = TRUE,
                            style = "btn-outline-primary"
                          ))),
          
          br(),
          div(style = "padding: 10px; background-color: #e8f4f8; border-radius: 8px; border-left: 4px solid #3498db;",
              p(icon("info-circle"), " Use the brush tool (+) by clicking and dragging with your cursor to select data on the hydrograph and view below.", 
                style = "margin: 0; font-size: 0.9rem; color: #2c3e50;")),
          
          div(style = "margin-top: 10px; text-align: center;",
              downloadButton("download_plot", "Download Hydrograph", 
                             class = "btn-primary btn-sm", 
                             icon = icon("image"))),
          
          div(style = "margin-top: 15px; text-align: center;",
              downloadButton("download_brush", "Download Selected Data", 
                             class = "btn-primary btn-sm", 
                             icon = icon("download")))
        ),
        
        card(
          full_screen = TRUE,
          card_header(
            class = "bg-primary text-white",
            "Hydrographs by Year"
          ),
          plotOutput("hydrograph", height = "600px", 
                     brush = brushOpts(id = "hydro_brush", fill = "#4C6D9A", opacity = 0.3))
        )
      )
  ),
  
  # Selected data points section 
  div(class = "brush-info-section",
      div(class = "row",
          div(class = "col-12",
              card(
                card_header(
                  class = "bg-success text-white",
                  "Selected Data from Hydrograph:"
                ),
                tableOutput("brush_info")
              )
          )
      )
  ),
  
  # Second section: Water Level Stats
  div(class = "content-section stats-main",
      layout_sidebar(
        sidebar = sidebar(
          class = "sidebar-custom",
          width = 300,
          h4("Statistics Controls", style = "color: #1B365D; margin-bottom: 20px;"),
          
          pickerInput("stats_site", 
                      label = div(icon("map-marker"), "Select Site(s):"),
                      choices = sort(unique(wl_stats$site)),
                      selected = "Great Meadow 1",
                      multiple = TRUE,
                      options = list(
                        `actions-box` = TRUE,
                        `deselect-all-text` = "Clear all",
                        `select-all-text` = "Select all",
                        `none-selected-text` = "Choose site(s)",
                        `live-search` = TRUE,
                        style = "btn-outline-primary"
                      )),
          
          div(style = "margin-bottom: 15px;",
              pickerInput("stats_year", 
                          label = div(icon("calendar"), "Select Years:"),
                          choices = sort(unique(wl_stats$year)),
                          selected = c(2024, 2023, 2022, 2021),
                          multiple = TRUE,
                          options = list(
                            `actions-box` = TRUE,
                            `deselect-all-text` = "Clear all",
                            `select-all-text` = "Select all",
                            `none-selected-text` = "Choose year(s)",
                            `live-search` = TRUE,
                            style = "btn-outline-primary"
                          ))),
          
          div(style = "margin-bottom: 15px;",
              radioButtons("time_summary", "Summarize Stats:",
                           choices = c("Each Year" = "year", "Average Across Years" = "multi"),
                           selected = "year",
                           inline = TRUE)),
          
          br(),
          div(style = "padding: 10px; background-color: #e8f4f8; border-radius: 8px; border-left: 4px solid #3498db;",
              p(icon("info-circle"), " Growing season statistics calculated from May through October.", 
                style = "margin: 0; font-size: 0.9rem; color: #2c3e50;")),
          
          div(style = "margin-top: 15px; text-align: center;",
              downloadButton("download_stats", "Download Table", 
                             class = "btn-primary btn-sm", 
                             icon = icon("download")))
        ),
        
        card(
          full_screen = TRUE,
          card_header(
            class = "bg-primary text-white",
            "Growing Season Water Level Statistics"
          ),
          
          div(style = "padding: 10px;",
              uiOutput("significance_info"),
              dataTableOutput("wl_stats"))
        )
      )
  )
)

#--------------------#
####    SERVER    ####
#--------------------#

server <- function(input, output, session) {
  
  # Reactive data for plotting
  plot_data <- reactive({
    req(input$year, input$selected_sites)
    
    all_data %>%
      filter(year %in% input$year, doy > 134, doy < 275) %>%
      filter(site %in% input$selected_sites)
  })
  
  output$hydrograph <- renderPlot({
    plot_data_filtered <- plot_data()
    req(nrow(plot_data_filtered) > 0)
    
    # Calculate minimum water level across all data for consistent precipitation baseline
    minWL <- min(plot_data_filtered$water_depth, na.rm = TRUE)
    
    # Color palette for sites
    sites <- unique(plot_data_filtered$site)
    site_colors <- c(
      "Great Meadow 1" = "black", "Great Meadow 2" = "chartreuse4", "Great Meadow 3" = "green",
      "Great Meadow 4" = "darkorange", "Great Meadow 5" = "deeppink2", "Great Meadow 6" = "purple",
      "Gilmore Meadow" = "darkgray", "Precipitation" = "blue"
    )
    
    # Create the plot
    ggplot(plot_data_filtered, aes(x = doy_h, y = water_depth)) +
      # Water level lines by site
      geom_line(aes(color = site), size = 0.7) +
      
      # Precip line (scaled by multiplier of 5 and offset by minWL)
      geom_line(aes(x = doy_h, y = lag_precip * 5 + minWL, color = "Precipitation"), 
                size = 0.7) +
      
      # Ground level reference
      geom_hline(yintercept = 0, color = 'brown') +
      
      # Facet by year
      facet_wrap(~ year, ncol = 1, scales = "free_y") +
      
      # Colors
      scale_color_manual(values = site_colors, 
                         breaks = c(sites, "Precipitation")) +
      
      # Axes and labels
      labs(y = 'Water Level (cm)', x = 'Date') +
      scale_x_continuous(
        breaks = c(121, 152, 182, 213, 244, 274),
        labels = c('May-01', 'Jun-01', 'Jul-01', 'Aug-01', 'Sep-01', 'Oct-01')
      ) +
      
      # Secondary axis for precip
      scale_y_continuous(
        sec.axis = sec_axis(~ .,
                            breaks = c(minWL, minWL + 10),
                            name = 'Hourly Precip. (cm)',
                            labels = c('0', '2'))
      ) +
      
      # Theme
      theme_bw() +
      theme(
        plot.title = element_text(hjust = 0.5),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.text.y.right = element_text(color = 'blue'),
        axis.title.y.right = element_text(color = 'blue'),
        strip.text = element_text(size = 11),
        legend.position = "bottom",
        legend.title = element_blank()
      )
  })
  
  # Reactive for processed brushed data - with site columns
  processed_brush_data <- reactive({
    req(input$hydro_brush)
    
    plot_data_filtered <- plot_data()
    req(nrow(plot_data_filtered) > 0)
    
    # Get brushed points using brushedPoints()
    selected_data <- brushedPoints(
      plot_data_filtered,
      brush = input$hydro_brush,
      xvar = "doy_h",
      yvar = "water_depth"
    ) %>%
      select(timestamp, year, doy_h, site, water_depth, lag_precip) %>%
      arrange(year, doy_h) %>%
      mutate(
        doy_h = round(doy_h, 2),
        water_depth = round(water_depth, 2),
        lag_precip = round(lag_precip, 3),
        timestamp = format(timestamp, "%Y-%m-%d %H:%M:%S")
      )
    
    if (nrow(selected_data) == 0) {
      return(data.frame(Message = "No points selected - try brushing over the water level lines"))
    }
    
    # Create the base data with unique timestamp/precip combinations
    base_data <- selected_data %>%
      group_by(year, doy_h, timestamp, lag_precip) %>%
      summarise(.groups = 'drop') %>%
      arrange(year, doy_h)
    
    # Create water depth columns for each site
    water_depth_data <- selected_data %>%
      group_by(year, doy_h, timestamp, site) %>%
      summarise(water_depth = mean(water_depth, na.rm = TRUE), .groups = 'drop') %>%
      pivot_wider(names_from = site, values_from = water_depth, names_prefix = "")
    
    # Join the data together
    result_data <- base_data %>%
      left_join(water_depth_data, by = c("year", "doy_h", "timestamp")) %>%
      rename(
        Year = year,
        Timestamp = timestamp,
        `Day of Year` = doy_h,
        `Precipitation (cm)` = lag_precip
      )
    
    # Add "Water Depth (cm)" suffix to site columns
    site_columns <- intersect(names(result_data), input$selected_sites)
    if (length(site_columns) > 0) {
      result_data <- result_data %>%
        rename_with(~ paste(.x, "Water Depth (cm)"), .cols = all_of(site_columns))
    }
    
    result_data %>% arrange(Year, `Day of Year`)
  })
  
  # Reactive for significance testing results - checks if both wetlands are selected
  significance_results <- reactive({
    req(input$stats_year, input$stats_site)
    
    # Check if both wetlands are represented in selected sites
    selected_wetlands <- wl_stats %>%
      filter(site %in% input$stats_site) %>%
      pull(wetland) %>%
      unique()
    
    # Only calculate significance if both wetlands are represented
    if (length(selected_wetlands) >= 2 && all(c("Great Meadow", "Gilmore Meadow") %in% selected_wetlands)) {
      calculate_wetland_significance(wl_stats, input$stats_year, input$stats_site, alpha = 0.05)
    } else {
      NULL
    }
  })
  
  # Reactive to determine if we should show significance info
  show_significance_info <- reactive({
    req(input$stats_site, input$time_summary)
    
    if (input$time_summary != "multi") return(FALSE)
    
    selected_wetlands <- wl_stats %>%
      filter(site %in% input$stats_site) %>%
      pull(wetland) %>%
      unique()
    
    return(length(selected_wetlands) >= 2 && all(c("Great Meadow", "Gilmore Meadow") %in% selected_wetlands))
  })
  
  
  # Setting up average WL stats and calculating stats output tables
  filtered_stats <- reactive({
    req(input$stats_site, input$stats_year, input$time_summary)
    
    data <- wl_stats %>%
      filter(site %in% input$stats_site, year %in% input$stats_year)
    
    if (input$time_summary == "year") {
      # ---- Option 1: Per-Year Summary ----
      data %>%
        mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
        select(
          Year = year,
          Site = site,
          `Mean Water Level (cm)` = WL_mean,
          `SD Water Level (cm)` = WL_sd,
          `Minimum Water Level (cm)` = WL_min,
          `Maximum Water Level (cm)` = WL_max,
          `Maximum Hourly Increase (cm)` = max_inc,
          `Maximum Hourly Decrease (cm)` = max_dec,
          `Growing Season Change (cm)` = GS_change,
          `GS % Surface Water` = prop_over_0cm,
          `GS % Within 30cm` = prop_bet_0_neg30cm,
          `GS % Over 30cm Deep` = prop_under_neg30cm
        )
    } else {
      # ---- Option 2: Average Across Years ----
      # Per-site summary
      data_site <- data %>%
        group_by(site, wetland) %>%
        summarise(
          Year = paste0(min(year), "–", max(year)),
          `Mean Water Level (cm)` = round(mean(WL_mean, na.rm = TRUE), 2),
          `SD Water Level (cm)` = round(mean(WL_sd, na.rm = TRUE), 2),
          `Minimum Water Level (cm)` = round(mean(WL_min, na.rm = TRUE), 2),
          `Maximum Water Level (cm)` = round(mean(WL_max, na.rm = TRUE), 2),
          `Maximum Hourly Increase (cm)` = round(mean(max_inc, na.rm = TRUE), 2),
          `Maximum Hourly Decrease (cm)` = round(mean(max_dec, na.rm = TRUE), 2),
          `Growing Season Change (cm)` = round(mean(GS_change, na.rm = TRUE), 2),
          `GS % Surface Water` = round(mean(prop_over_0cm, na.rm = TRUE), 2),
          `GS % Within 30cm` = round(mean(prop_bet_0_neg30cm, na.rm = TRUE), 2),
          `GS % Over 30cm Deep` = round(mean(prop_under_neg30cm, na.rm = TRUE), 2),
          .groups = "drop"
        )
      
      # Per-wetland "All Sites" summary
      data_wetland <- data %>%
        group_by(wetland) %>%
        summarise(
          site = "All Sites",
          Year = paste0(min(year), "–", max(year)),
          `Mean Water Level (cm)` = round(mean(WL_mean, na.rm = TRUE), 2),
          `SD Water Level (cm)` = round(mean(WL_sd, na.rm = TRUE), 2),
          `Minimum Water Level (cm)` = round(mean(WL_min, na.rm = TRUE), 2),
          `Maximum Water Level (cm)` = round(mean(WL_max, na.rm = TRUE), 2),
          `Maximum Hourly Increase (cm)` = round(mean(max_inc, na.rm = TRUE), 2),
          `Maximum Hourly Decrease (cm)` = round(mean(max_dec, na.rm = TRUE), 2),
          `Growing Season Change (cm)` = round(mean(GS_change, na.rm = TRUE), 2),
          `GS % Surface Water` = round(mean(prop_over_0cm, na.rm = TRUE), 2),
          `GS % Within 30cm` = round(mean(prop_bet_0_neg30cm, na.rm = TRUE), 2),
          `GS % Over 30cm Deep` = round(mean(prop_under_neg30cm, na.rm = TRUE), 2),
          .groups = "drop"
        )
      
      bind_rows(data_site, data_wetland) %>%
        distinct() %>% 
        rename(Site = site,
               Wetland = wetland)
    }
  })
  
  # Reactive table output for selected data from hydrograph
  output$brush_info <- renderTable({
    processed_brush_data()
  })
  
  # Output for significance information display
  output$significance_info <- renderUI({
    if (show_significance_info()) {
      sig_results <- significance_results()
      
      if (!is.null(sig_results)) {
        sig_vars <- sig_results %>% 
          filter(significant) %>% 
          pull(variable)
        
        if (length(sig_vars) > 0) {
          # Map variable names to display names
          var_display_names <- c(
            "WL_mean" = "Mean Water Level",
            "WL_sd" = "SD Water Level", 
            "WL_min" = "Minimum Water Level",
            "WL_max" = "Maximum Water Level",
            "max_inc" = "Maximum Hourly Increase",
            "max_dec" = "Maximum Hourly Decrease",
            "GS_change" = "Growing Season Change",
            "prop_over_0cm" = "GS % Surface Water",
            "prop_bet_0_neg30cm" = "GS % Within 30cm",
            "prop_under_neg30cm" = "GS % Over 30cm Deep"
          )
          
          sig_display_names <- var_display_names[sig_vars]
          sig_display_names <- sig_display_names[!is.na(sig_display_names)]
          
          div(class = "significance-info",
              h5(icon("asterisk"), " Statistical Significance", 
                 style = "background-color: #fff3cd; 
                color: #856404; 
                padding: 6px 10px; 
                border-radius: 4px; 
                display: inline-block; 
                margin-bottom: 10px;"),
              p("Yellow highlighted variables show significant differences (p < 0.05) between Great Meadow and Gilmore Meadow wetlands:", 
                style = "margin-bottom: 8px; font-size: 0.9rem;"),
              tags$ul(
                lapply(sig_display_names, function(name) {
                  tags$li(name, style = "font-size: 0.85rem; color: #856404;")
                })
              ),
              p("Statistical tests compare averages across all selected years and sites within each wetland.", 
                style = "margin-top: 5px; margin-bottom: 10px; font-size: 0.8rem; font-style: italic; color: #6c757d;")
          )
        }
      }
    }
  })
  
  # reactive table output for WL stats with significance highlighting
  output$wl_stats <- DT::renderDataTable({
    data <- filtered_stats()
    
    # Reorder so "All Sites" rows come first
    data <- data %>%
      dplyr::arrange(desc(Site == "All Sites"), Site)
    
    dt <- datatable(
      data,
      rownames = FALSE,
      options = list(pageLength = 10, scrollX = TRUE)
    ) %>%
      formatStyle(
        columns = names(data),  # apply to all columns
        valueColumns = "Site",
        backgroundColor = styleEqual("All Sites", "#d9edf7"), # light blue
        fontWeight = styleEqual("All Sites", "bold")
      )
    
    # Add significance highlighting only when appropriate
    if (show_significance_info()) {
      sig_results <- significance_results()
      
      if (!is.null(sig_results)) {
        # Map variable names to column names in the table
        var_mapping <- c(
          "WL_mean" = "Mean Water Level (cm)",
          "WL_sd" = "SD Water Level (cm)", 
          "WL_min" = "Minimum Water Level (cm)",
          "WL_max" = "Maximum Water Level (cm)",
          "max_inc" = "Maximum Hourly Increase (cm)",
          "max_dec" = "Maximum Hourly Decrease (cm)",
          "GS_change" = "Growing Season Change (cm)",
          "prop_over_0cm" = "GS % Surface Water",
          "prop_bet_0_neg30cm" = "GS % Within 30cm",
          "prop_under_neg30cm" = "GS % Over 30cm Deep"
        )
        
        # Apply highlighting for significant variables
        for (var_name in names(var_mapping)) {
          col_name <- var_mapping[var_name]
          sig_row <- sig_results[sig_results$variable == var_name, ]
          
          if (nrow(sig_row) > 0 && sig_row$significant) {
            dt <- dt %>%
              formatStyle(
                col_name,
                valueColumns = "Site",
                backgroundColor = styleEqual("All Sites", "#fff3cd"),
                fontWeight = styleEqual("All Sites", "bold")
              )
          }
        }
      }
    }
    
    dt
  })
  
  # Download handler for brushed data
  output$download_brush <- downloadHandler(
    filename = function() {
      paste("hydrograph_selected_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(processed_brush_data(), file, row.names = FALSE)
    }
  )
  
  # Download handler for water level stats
  output$download_stats <- downloadHandler(
    filename = function() {
      if (input$time_summary == "multi") {
        paste("wetland_averaged_stats_", Sys.Date(), ".csv", sep = "")
      } else {
        paste("site_stats_", Sys.Date(), ".csv", sep = "")
      }
    },
    content = function(file) {
      write.csv(filtered_stats(), file, row.names = FALSE)
    }
  )
  
  # Download handler for hydrograph plot
  output$download_plot <- downloadHandler(
    filename = function() {
      sites_label <- paste(input$selected_sites, collapse = "_")
      years_label <- paste(input$year, collapse = "_")
      paste0("hydrograph_", sites_label, "_", years_label, "_", Sys.Date(), ".png")
    },
    content = function(file) {
      # Recreate the plot
      plot_data_filtered <- plot_data()
      req(nrow(plot_data_filtered) > 0)
      
      # Calculate minimum water level across all data for consistent precipitation baseline
      minWL <- min(plot_data_filtered$water_depth, na.rm = TRUE)
      
      # Create color palette for sites
      sites <- unique(plot_data_filtered$site)
      site_colors <- c(
        "Great Meadow 1" = "black", "Great Meadow 2" = "chartreuse4", "Great Meadow 3" = "green",
        "Great Meadow 4" = "darkorange", "Great Meadow 5" = "deeppink2", "Great Meadow 6" = "purple",
        "Gilmore Meadow" = "darkgray", "Precipitation" = "blue"
      )
      
      # Create the plot
      p <- ggplot(plot_data_filtered, aes(x = doy_h, y = water_depth)) +
        # Water level lines by site
        geom_line(aes(color = site), size = 0.7) +
        
        # Precipitation line (scaled by multiplier of 5 and offset by minWL)
        geom_line(aes(x = doy_h, y = lag_precip * 5 + minWL, color = "Precipitation"), 
                  size = 0.7) +
        
        # Ground level reference
        geom_hline(yintercept = 0, color = 'brown') +
        
        # Facet by year
        facet_wrap(~ year, ncol = 1, scales = "free_y") +
        
        # Colors
        scale_color_manual(values = site_colors, 
                           breaks = c(sites, "Precipitation")) +
        
        # Axes and labels
        labs(y = 'Water Level (cm)', x = 'Date') +
        scale_x_continuous(
          breaks = c(121, 152, 182, 213, 244, 274),
          labels = c('May-01', 'Jun-01', 'Jul-01', 'Aug-01', 'Sep-01', 'Oct-01')
        ) +
        
        # Secondary axis for precipitation
        scale_y_continuous(
          sec.axis = sec_axis(~ .,
                              breaks = c(minWL, minWL + 10),
                              name = 'Hourly Precip. (cm)',
                              labels = c('0', '2'))
        ) +
        
        # Theme
        theme_bw() +
        theme(
          plot.title = element_text(hjust = 0.5),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          axis.text.y.right = element_text(color = 'blue'),
          axis.title.y.right = element_text(color = 'blue'),
          strip.text = element_text(size = 11),
          legend.position = "bottom",
          legend.title = element_blank()
        )
      
      # Save the plot
      ggsave(file, plot = p, width = 12, height = 8, dpi = 300, bg = "white")
    }
  )
  
}

# Run app
shinyApp(ui, server)




#9/16/25 (prior to Gilmore precip fix)--------------------------------------------------------

library(shiny)
library(tidyverse)
library(lubridate)
library(patchwork)
library(shinyWidgets)
library(DT)
library(bslib)

# Read & prepare processed data
gm <- read.csv("data/processed_data/great_meadow_well_data_2024_20250915.csv") %>%
  mutate(date = as.Date(date),
         timestamp = as_datetime(timestamp),
         site = paste("Great Meadow", plot.num),
         water.depth = case_when(
           year == 2016 & doy_h == 159.12 & plot.num == 3 & water.depth < -120 ~ NA_real_,
           year == 2017 & doy_h == 215.02 & plot.num == 6 & water.depth < -115 ~ NA_real_,
           year == 2021 & plot.num == 3 & doy == 224 & water.depth > 400 ~ NA_real_,
           year == 2021 & plot.num == 3 & doy == 225 & water.depth > 400 ~ NA_real_,
           TRUE ~ water.depth
         )) %>%
  mutate(siteyear = paste(site, year, sep = "_"))


gl <- read.csv("data/raw_data/hydrology_data/gilmore_well_prec_data_2013-2024.csv") %>%
  rename(water.depth = GILM_WL) %>%
  mutate(
    site = "Gilmore Meadow",
    
    # Try to parse full datetime, fallback to date only + midnight
    timestamp_parsed = mdy_hm(timestamp),
    timestamp_parsed = if_else(
      is.na(timestamp_parsed),
      mdy(timestamp),
      timestamp_parsed
    ),
    timestamp_parsed = as.POSIXct(timestamp_parsed),
    timestamp = timestamp_parsed,
    
    # Standardize Date column too
    Date = mdy(Date)
  ) %>%
  select(timestamp, 
         date = Date,
         doy,
         year = Year,
         precip_cm,
         water.depth,
         lag.precip,
         hr,
         doy_h,
         site)

all_data <- bind_rows(gm, gl) %>% 
  filter(year >= 2016 & year <= 2024) %>% 
  select(timestamp, 
         date, 
         year, 
         doy, 
         hr, 
         doy_h,
         precip_cm,
         lag_precip = lag.precip,
         water_depth = water.depth,
         site)

wl_stats <- read.csv("data/processed_data/gm_gl_wl_stats.csv") %>% 
  select(year,
         stat, 
         `Gilmore Meadow` = gilmore.meadow, 
         `Great Meadow 1` = great.meadow.1, 
         `Great Meadow 2` = great.meadow.2, 
         `Great Meadow 3` = great.meadow.3, 
         `Great Meadow 4` = great.meadow.4, 
         `Great Meadow 5` = great.meadow.5, 
         `Great Meadow 6` = great.meadow.6) %>% 
  pivot_longer(cols = -c(year, stat), names_to = "site", values_to = "value") %>% 
  pivot_wider(names_from = stat, values_from = value) %>%
  arrange(site, year) %>% 
  mutate(
    wetland = case_when(
      grepl("Great Meadow", site, ignore.case = TRUE) ~ "Great Meadow",
      grepl("Gilmore Meadow", site, ignore.case = TRUE) ~ "Gilmore Meadow",
      TRUE ~ "Unknown"
    )
  )


# Function to calculate significance tests between wetlands
calculate_wetland_significance <- function(data, selected_years, selected_sites, alpha = 0.05) {
  # Filter data for selected years and sites
  filtered_data <- data %>%
    filter(year %in% selected_years, site %in% selected_sites) %>%
    mutate(site_group = ifelse(grepl("Great Meadow", site), "Great Meadow", site))
  
  # Check if we have both wetlands represented in the selected sites
  wetlands_present <- unique(filtered_data$site_group)
  
  # Only run tests if both wetlands are present
  if (length(wetlands_present) < 2 || !all(c("Great Meadow", "Gilmore Meadow") %in% wetlands_present)) {
    return(NULL)  # return NULL if we can't compare between wetlands
  }
  
  # Get all numeric stat columns
  stat_cols <- filtered_data %>% select(where(is.numeric), -year) %>% names()
  
  # Run t-tests for each variable and store results
  t_test_results <- map_dfr(stat_cols, function(var) {
    formula <- as.formula(paste(var, "~ site_group"))
    tryCatch({
      test <- t.test(formula, data = filtered_data) # perform t-test
      data.frame(
        variable = var,
        p_value = test$p.value,
        significant = test$p.value < alpha,
        stringsAsFactors = FALSE
      )
    }, error = function(e) {
      data.frame(
        variable = var,
        p_value = NA,
        significant = FALSE,
        stringsAsFactors = FALSE
      )
    })
  })
  
  return(t_test_results)
}


# UI 
ui <- page_fluid(
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#1B365D",
    secondary = "#4C6D9A",    
    success = "#2E86C1",
    info = "#3498db",
    warning = "#f39c12",
    danger = "#e74c3c",
    base_font = font_google("Open Sans"),
    heading_font = font_google("Open Sans", wght = c(400, 700))
  ),
  
  # Custom CSS for additional styling
  tags$head(
    tags$style(HTML("
      .content-section {
        margin: 30px 0;
        padding: 25px;
        border-radius: 15px;
        box-shadow: 0 4px 12px rgba(0,0,0,0.1);
        background: linear-gradient(135deg, #ffffff 0%, #f8f9fa 100%);
        border: 2px solid #1B365D;
      }
      
      .section-title {
        color: #1B365D;
        font-weight: 600;
        margin-bottom: 20px;
        padding-bottom: 10px;
        border-bottom: 2px solid #4C6D9A;
      }
      
      .sidebar-custom {
        background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%);
        border-radius: 10px;
        padding: 20px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.08);
        border: 1px solid #dee2e6;
      }
      
      .main-title {
        background: linear-gradient(135deg, #1B365D 0%, #4C6D9A 100%);
        color: white;
        padding: 30px;
        margin: -15px -15px 30px -15px;
        text-align: center;
        border-radius: 0 0 20px 20px;
        box-shadow: 0 4px 12px rgba(0,0,0,0.15);
      }
      
      .brush-info-section {
        background: linear-gradient(135deg, #e8f4f8 0%, #f0f8ff 100%);
        border-radius: 12px;
        padding: 20px;
        margin: 20px 0;
        border: 1px solid #4C6D9A;
      }
      
      .stats-main {
        background: linear-gradient(135deg, #f8f9fa 0%, #ffffff 100%);
      }
      
      .dataTables_wrapper {
        font-size: 0.85rem !important;
      }

      .dataTables_wrapper table {
        font-size: 0.8rem !important;
      }

      .dataTables_wrapper .dataTables_info,
      .dataTables_wrapper .dataTables_paginate {
        font-size: 0.75rem !important;
      }
      
    "))
  ),
  
  # Main title with gradient background
  div(class = "main-title",
      h1("Wetland Hydrograph Visualizer", 
         style = "margin: 0; font-size: 2rem; text-shadow: 2px 2px 4px rgba(0,0,0,0.3)")
  ),
  
  # First section: Hydrographs 
  div(class = "content-section",
      layout_sidebar(
        sidebar = sidebar(
          class = "sidebar-custom",
          width = 300,
          h4("Hydrograph Controls", style = "color: #1B365D; margin-bottom: 20px;"),
          
          pickerInput("selected_sites", 
                      label = div(icon("map-marker"), "Select Site(s):"),
                      choices = sort(unique(all_data$site)),
                      selected = c("Great Meadow 1"),
                      multiple = TRUE,
                      options = list(
                        `actions-box` = TRUE,
                        `deselect-all-text` = "Clear all",
                        `select-all-text` = "Select all",
                        `none-selected-text` = "Choose site(s)",
                        `live-search` = TRUE,
                        style = "btn-outline-primary"
                      )),
          
          div(style = "margin-bottom: 15px;",
              pickerInput("year", 
                          label = div(icon("calendar"), "Select Year(s):"),
                          choices = sort(unique(all_data$year)),
                          selected = 2024,
                          multiple = TRUE,
                          options = list(
                            `actions-box` = TRUE,
                            `deselect-all-text` = "Clear all",
                            `select-all-text` = "Select all",
                            `none-selected-text` = "Choose year(s)",
                            `live-search` = TRUE,
                            style = "btn-outline-primary"
                          ))),
          
          br(),
          div(style = "padding: 10px; background-color: #e8f4f8; border-radius: 8px; border-left: 4px solid #3498db;",
              p(icon("info-circle"), " Use the brush tool (+) by clicking and dragging with your cursor to select data on the hydrograph and view below.", 
                style = "margin: 0; font-size: 0.9rem; color: #2c3e50;")),
          
          div(style = "margin-top: 10px; text-align: center;",
              downloadButton("download_plot", "Download Hydrograph", 
                             class = "btn-primary btn-sm", 
                             icon = icon("image"))),
          
          div(style = "margin-top: 15px; text-align: center;",
              downloadButton("download_brush", "Download Selected Data", 
                             class = "btn-primary btn-sm", 
                             icon = icon("download")))
        ),
        
        card(
          full_screen = TRUE,
          card_header(
            class = "bg-primary text-white",
            "Hydrographs by Year"
          ),
          plotOutput("hydrograph", height = "600px", 
                     brush = brushOpts(id = "hydro_brush", fill = "#4C6D9A", opacity = 0.3))
        )
      )
  ),
  
  # Selected data points section with improved styling
  div(class = "brush-info-section",
      div(class = "row",
          div(class = "col-12",
              card(
                card_header(
                  class = "bg-success text-white",
                  "Selected Data from Hydrograph:"
                ),
                tableOutput("brush_info")
              )
          )
      )
  ),
  
  # Second section: Water Level Stats with improved styling
  div(class = "content-section stats-main",
      layout_sidebar(
        sidebar = sidebar(
          class = "sidebar-custom",
          width = 300,
          h4("Statistics Controls", style = "color: #1B365D; margin-bottom: 20px;"),
          
          pickerInput("stats_site", 
                      label = div(icon("map-marker"), "Select Site(s):"),
                      choices = sort(unique(wl_stats$site)),
                      selected = "Great Meadow 1",
                      multiple = TRUE,
                      options = list(
                        `actions-box` = TRUE,
                        `deselect-all-text` = "Clear all",
                        `select-all-text` = "Select all",
                        `none-selected-text` = "Choose site(s)",
                        `live-search` = TRUE,
                        style = "btn-outline-primary"
                      )),
          
          div(style = "margin-bottom: 15px;",
              pickerInput("stats_year", 
                          label = div(icon("calendar"), "Select Years:"),
                          choices = sort(unique(wl_stats$year)),
                          selected = c(2024, 2023, 2022, 2021),
                          multiple = TRUE,
                          options = list(
                            `actions-box` = TRUE,
                            `deselect-all-text` = "Clear all",
                            `select-all-text` = "Select all",
                            `none-selected-text` = "Choose year(s)",
                            `live-search` = TRUE,
                            style = "btn-outline-primary"
                          ))),
          
          div(style = "margin-bottom: 15px;",
              radioButtons("time_summary", "Summarize Stats:",
                           choices = c("Each Year" = "year", "Average Across Years" = "multi"),
                           selected = "year",
                           inline = TRUE)),
          
          br(),
          div(style = "padding: 10px; background-color: #e8f4f8; border-radius: 8px; border-left: 4px solid #3498db;",
              p(icon("info-circle"), " Growing season statistics calculated from May through October.", 
                style = "margin: 0; font-size: 0.9rem; color: #2c3e50;")),
          
          div(style = "margin-top: 15px; text-align: center;",
              downloadButton("download_stats", "Download Table", 
                             class = "btn-primary btn-sm", 
                             icon = icon("download")))
        ),
        
        card(
          full_screen = TRUE,
          card_header(
            class = "bg-primary text-white",
            "Growing Season Water Level Statistics"
          ),
          
          div(style = "padding: 10px;",
              uiOutput("significance_info"),
              dataTableOutput("wl_stats"))
        )
      )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive data for plotting
  plot_data <- reactive({
    req(input$year, input$selected_sites)
    
    all_data %>%
      filter(year %in% input$year, doy > 134, doy < 275) %>%
      filter(site %in% input$selected_sites)
  })
  
  output$hydrograph <- renderPlot({
    plot_data_filtered <- plot_data()
    req(nrow(plot_data_filtered) > 0)
    
    # Calculate minimum water level across all data for consistent precipitation baseline
    minWL <- min(plot_data_filtered$water_depth, na.rm = TRUE)
    
    # Color palette for sites
    sites <- unique(plot_data_filtered$site)
    site_colors <- c(
      "Great Meadow 1" = "black", "Great Meadow 2" = "chartreuse4", "Great Meadow 3" = "green",
      "Great Meadow 4" = "darkorange", "Great Meadow 5" = "deeppink2", "Great Meadow 6" = "purple",
      "Gilmore Meadow" = "darkgray", "Precipitation" = "blue"
    )
    
    # Create the plot
    ggplot(plot_data_filtered, aes(x = doy_h, y = water_depth)) +
      # Water level lines by site
      geom_line(aes(color = site), size = 0.7) +
      
      # Precipitation line (scaled by multiplier of 5 and offset by minWL)
      geom_line(aes(x = doy_h, y = lag_precip * 5 + minWL, color = "Precipitation"), 
                size = 0.7) +
      
      # Ground level reference
      geom_hline(yintercept = 0, color = 'brown') +
      
      # Facet by year
      facet_wrap(~ year, ncol = 1, scales = "free_y") +
      
      # Colors
      scale_color_manual(values = site_colors, 
                         breaks = c(sites, "Precipitation")) +
      
      # Axes and labels
      labs(y = 'Water Level (cm)', x = 'Date') +
      scale_x_continuous(
        breaks = c(121, 152, 182, 213, 244, 274),
        labels = c('May-01', 'Jun-01', 'Jul-01', 'Aug-01', 'Sep-01', 'Oct-01')
      ) +
      
      # Secondary axis for precipitation
      scale_y_continuous(
        sec.axis = sec_axis(~ .,
                            breaks = c(minWL, minWL + 10),
                            name = 'Hourly Precip. (cm)',
                            labels = c('0', '2'))
      ) +
      
      # Theme
      theme_bw() +
      theme(
        plot.title = element_text(hjust = 0.5),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.text.y.right = element_text(color = 'blue'),
        axis.title.y.right = element_text(color = 'blue'),
        strip.text = element_text(size = 11),
        legend.position = "bottom",
        legend.title = element_blank()
      )
  })
  
  # Reactive for processed brushed data - with site columns
  processed_brush_data <- reactive({
    req(input$hydro_brush)
    
    plot_data_filtered <- plot_data()
    req(nrow(plot_data_filtered) > 0)
    
    # Get brushed points using the simple approach
    selected_data <- brushedPoints(
      plot_data_filtered,
      brush = input$hydro_brush,
      xvar = "doy_h",
      yvar = "water_depth"
    ) %>%
      select(timestamp, year, doy_h, site, water_depth, lag_precip) %>%
      arrange(year, doy_h) %>%
      mutate(
        doy_h = round(doy_h, 2),
        water_depth = round(water_depth, 2),
        lag_precip = round(lag_precip, 3),
        timestamp = format(timestamp, "%Y-%m-%d %H:%M:%S")
      )
    
    if (nrow(selected_data) == 0) {
      return(data.frame(Message = "No points selected - try brushing over the water level lines"))
    }
    
    # Create the base data with unique timestamp/precipitation combinations
    base_data <- selected_data %>%
      group_by(year, doy_h, timestamp, lag_precip) %>%
      summarise(.groups = 'drop') %>%
      arrange(year, doy_h)
    
    # Create water depth columns for each site
    water_depth_data <- selected_data %>%
      group_by(year, doy_h, timestamp, site) %>%
      summarise(water_depth = mean(water_depth, na.rm = TRUE), .groups = 'drop') %>%
      pivot_wider(names_from = site, values_from = water_depth, names_prefix = "")
    
    # Join the data together
    result_data <- base_data %>%
      left_join(water_depth_data, by = c("year", "doy_h", "timestamp")) %>%
      rename(
        Year = year,
        Timestamp = timestamp,
        `Day of Year` = doy_h,
        `Precipitation (cm)` = lag_precip
      )
    
    # Add " Water Depth (cm)" suffix to site columns
    site_columns <- intersect(names(result_data), input$selected_sites)
    if (length(site_columns) > 0) {
      result_data <- result_data %>%
        rename_with(~ paste(.x, "Water Depth (cm)"), .cols = all_of(site_columns))
    }
    
    result_data %>% arrange(Year, `Day of Year`)
  })
  
  # Reactive for significance testing results - now checks if both wetlands are selected
  significance_results <- reactive({
    req(input$stats_year, input$stats_site)
    
    # Check if both wetlands are represented in selected sites
    selected_wetlands <- wl_stats %>%
      filter(site %in% input$stats_site) %>%
      pull(wetland) %>%
      unique()
    
    # Only calculate significance if both wetlands are represented
    if (length(selected_wetlands) >= 2 && all(c("Great Meadow", "Gilmore Meadow") %in% selected_wetlands)) {
      calculate_wetland_significance(wl_stats, input$stats_year, input$stats_site, alpha = 0.05)
    } else {
      NULL
    }
  })
  
  # Reactive to determine if we should show significance info
  show_significance_info <- reactive({
    req(input$stats_site, input$time_summary)
    
    if (input$time_summary != "multi") return(FALSE)
    
    selected_wetlands <- wl_stats %>%
      filter(site %in% input$stats_site) %>%
      pull(wetland) %>%
      unique()
    
    return(length(selected_wetlands) >= 2 && all(c("Great Meadow", "Gilmore Meadow") %in% selected_wetlands))
  })
  
  
  # setting up average WL stats and calculating stats output tables
  filtered_stats <- reactive({
    req(input$stats_site, input$stats_year, input$time_summary)
    
    data <- wl_stats %>%
      filter(site %in% input$stats_site, year %in% input$stats_year)
    
    if (input$time_summary == "year") {
      # ---- Option 1: Per-Year Summary ----
      data %>%
        mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
        select(
          Year = year,
          Site = site,
          `Mean Water Level (cm)` = WL_mean,
          `SD Water Level (cm)` = WL_sd,
          `Minimum Water Level (cm)` = WL_min,
          `Maximum Water Level (cm)` = WL_max,
          `Maximum Hourly Increase (cm)` = max_inc,
          `Maximum Hourly Decrease (cm)` = max_dec,
          `Growing Season Change (cm)` = GS_change,
          `GS % Surface Water` = prop_over_0cm,
          `GS % Within 30cm` = prop_bet_0_neg30cm,
          `GS % Over 30cm Deep` = prop_under_neg30cm
        )
    } else {
      # ---- Option 2: Average Across Years ----
      # Per-site summary
      data_site <- data %>%
        group_by(site, wetland) %>%
        summarise(
          Year = paste0(min(year), "–", max(year)),
          `Mean Water Level (cm)` = round(mean(WL_mean, na.rm = TRUE), 2),
          `SD Water Level (cm)` = round(mean(WL_sd, na.rm = TRUE), 2),
          `Minimum Water Level (cm)` = round(mean(WL_min, na.rm = TRUE), 2),
          `Maximum Water Level (cm)` = round(mean(WL_max, na.rm = TRUE), 2),
          `Maximum Hourly Increase (cm)` = round(mean(max_inc, na.rm = TRUE), 2),
          `Maximum Hourly Decrease (cm)` = round(mean(max_dec, na.rm = TRUE), 2),
          `Growing Season Change (cm)` = round(mean(GS_change, na.rm = TRUE), 2),
          `GS % Surface Water` = round(mean(prop_over_0cm, na.rm = TRUE), 2),
          `GS % Within 30cm` = round(mean(prop_bet_0_neg30cm, na.rm = TRUE), 2),
          `GS % Over 30cm Deep` = round(mean(prop_under_neg30cm, na.rm = TRUE), 2),
          .groups = "drop"
        )
      
      # Per-wetland "All Sites" summary
      data_wetland <- data %>%
        group_by(wetland) %>%
        summarise(
          site = "All Sites",
          Year = paste0(min(year), "–", max(year)),
          `Mean Water Level (cm)` = round(mean(WL_mean, na.rm = TRUE), 2),
          `SD Water Level (cm)` = round(mean(WL_sd, na.rm = TRUE), 2),
          `Minimum Water Level (cm)` = round(mean(WL_min, na.rm = TRUE), 2),
          `Maximum Water Level (cm)` = round(mean(WL_max, na.rm = TRUE), 2),
          `Maximum Hourly Increase (cm)` = round(mean(max_inc, na.rm = TRUE), 2),
          `Maximum Hourly Decrease (cm)` = round(mean(max_dec, na.rm = TRUE), 2),
          `Growing Season Change (cm)` = round(mean(GS_change, na.rm = TRUE), 2),
          `GS % Surface Water` = round(mean(prop_over_0cm, na.rm = TRUE), 2),
          `GS % Within 30cm` = round(mean(prop_bet_0_neg30cm, na.rm = TRUE), 2),
          `GS % Over 30cm Deep` = round(mean(prop_under_neg30cm, na.rm = TRUE), 2),
          .groups = "drop"
        )
      
      bind_rows(data_site, data_wetland) %>%
        distinct() %>% 
        rename(Site = site,
               Wetland = wetland)
    }
  })
  
  # reactive table output for selected data from hydrograph
  output$brush_info <- renderTable({
    processed_brush_data()
  })
  
  # Output for significance information display
  output$significance_info <- renderUI({
    if (show_significance_info()) {
      sig_results <- significance_results()
      
      if (!is.null(sig_results)) {
        sig_vars <- sig_results %>% 
          filter(significant) %>% 
          pull(variable)
        
        if (length(sig_vars) > 0) {
          # Map variable names to display names
          var_display_names <- c(
            "WL_mean" = "Mean Water Level",
            "WL_sd" = "SD Water Level", 
            "WL_min" = "Minimum Water Level",
            "WL_max" = "Maximum Water Level",
            "max_inc" = "Maximum Hourly Increase",
            "max_dec" = "Maximum Hourly Decrease",
            "GS_change" = "Growing Season Change",
            "prop_over_0cm" = "GS % Surface Water",
            "prop_bet_0_neg30cm" = "GS % Within 30cm",
            "prop_under_neg30cm" = "GS % Over 30cm Deep"
          )
          
          sig_display_names <- var_display_names[sig_vars]
          sig_display_names <- sig_display_names[!is.na(sig_display_names)]
          
          div(class = "significance-info",
              h5(icon("asterisk"), " Statistical Significance", 
                 style = "background-color: #fff3cd; 
                color: #856404; 
                padding: 6px 10px; 
                border-radius: 4px; 
                display: inline-block; 
                margin-bottom: 10px;"),
              p("Yellow highlighted variables show significant differences (p < 0.05) between Great Meadow and Gilmore Meadow wetlands:", 
                style = "margin-bottom: 8px; font-size: 0.9rem;"),
              tags$ul(
                lapply(sig_display_names, function(name) {
                  tags$li(name, style = "font-size: 0.85rem; color: #856404;")
                })
              ),
              p("Statistical tests compare averages across all selected years and sites within each wetland.", 
                style = "margin-top: 5px; margin-bottom: 10px; font-size: 0.8rem; font-style: italic; color: #6c757d;")
          )
        }
      }
    }
  })
  
  # reactive table output for WL stats with significance highlighting
  output$wl_stats <- DT::renderDataTable({
    data <- filtered_stats()
    
    # Reorder so "All Sites" rows come first
    data <- data %>%
      dplyr::arrange(desc(Site == "All Sites"), Site)
    
    dt <- datatable(
      data,
      rownames = FALSE,
      options = list(pageLength = 10, scrollX = TRUE)
    ) %>%
      formatStyle(
        columns = names(data),  # apply to all columns
        valueColumns = "Site",
        backgroundColor = styleEqual("All Sites", "#d9edf7"), # light blue
        fontWeight = styleEqual("All Sites", "bold")
      )
    
    # Add significance highlighting only when appropriate
    if (show_significance_info()) {
      sig_results <- significance_results()
      
      if (!is.null(sig_results)) {
        # Map variable names to column names in the table
        var_mapping <- c(
          "WL_mean" = "Mean Water Level (cm)",
          "WL_sd" = "SD Water Level (cm)", 
          "WL_min" = "Minimum Water Level (cm)",
          "WL_max" = "Maximum Water Level (cm)",
          "max_inc" = "Maximum Hourly Increase (cm)",
          "max_dec" = "Maximum Hourly Decrease (cm)",
          "GS_change" = "Growing Season Change (cm)",
          "prop_over_0cm" = "GS % Surface Water",
          "prop_bet_0_neg30cm" = "GS % Within 30cm",
          "prop_under_neg30cm" = "GS % Over 30cm Deep"
        )
        
        # Apply highlighting for significant variables
        for (var_name in names(var_mapping)) {
          col_name <- var_mapping[var_name]
          sig_row <- sig_results[sig_results$variable == var_name, ]
          
          if (nrow(sig_row) > 0 && sig_row$significant) {
            dt <- dt %>%
              formatStyle(
                col_name,
                valueColumns = "Site",
                backgroundColor = styleEqual("All Sites", "#fff3cd"),
                fontWeight = styleEqual("All Sites", "bold")
              )
          }
        }
      }
    }
    
    dt
  })
  
  # Download handler for brushed data
  output$download_brush <- downloadHandler(
    filename = function() {
      paste("hydrograph_selected_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(processed_brush_data(), file, row.names = FALSE)
    }
  )
  
  # Download handler for water level stats
  output$download_stats <- downloadHandler(
    filename = function() {
      if (input$time_summary == "multi") {
        paste("wetland_averaged_stats_", Sys.Date(), ".csv", sep = "")
      } else {
        paste("site_stats_", Sys.Date(), ".csv", sep = "")
      }
    },
    content = function(file) {
      write.csv(filtered_stats(), file, row.names = FALSE)
    }
  )
  
  # Download handler for hydrograph plot
  output$download_plot <- downloadHandler(
    filename = function() {
      sites_label <- paste(input$selected_sites, collapse = "_")
      years_label <- paste(input$year, collapse = "_")
      paste0("hydrograph_", sites_label, "_", years_label, "_", Sys.Date(), ".png")
    },
    content = function(file) {
      # Recreate the plot
      plot_data_filtered <- plot_data()
      req(nrow(plot_data_filtered) > 0)
      
      # Calculate minimum water level across all data for consistent precipitation baseline
      minWL <- min(plot_data_filtered$water_depth, na.rm = TRUE)
      
      # Create color palette for sites
      sites <- unique(plot_data_filtered$site)
      site_colors <- c(
        "Great Meadow 1" = "black", "Great Meadow 2" = "chartreuse4", "Great Meadow 3" = "green",
        "Great Meadow 4" = "darkorange", "Great Meadow 5" = "deeppink2", "Great Meadow 6" = "purple",
        "Gilmore Meadow" = "darkgray", "Precipitation" = "blue"
      )
      
      # Create the plot
      p <- ggplot(plot_data_filtered, aes(x = doy_h, y = water_depth)) +
        # Water level lines by site
        geom_line(aes(color = site), size = 0.7) +
        
        # Precipitation line (scaled by multiplier of 5 and offset by minWL)
        geom_line(aes(x = doy_h, y = lag_precip * 5 + minWL, color = "Precipitation"), 
                  size = 0.7) +
        
        # Ground level reference
        geom_hline(yintercept = 0, color = 'brown') +
        
        # Facet by year
        facet_wrap(~ year, ncol = 1, scales = "free_y") +
        
        # Colors
        scale_color_manual(values = site_colors, 
                           breaks = c(sites, "Precipitation")) +
        
        # Axes and labels
        labs(y = 'Water Level (cm)', x = 'Date') +
        scale_x_continuous(
          breaks = c(121, 152, 182, 213, 244, 274),
          labels = c('May-01', 'Jun-01', 'Jul-01', 'Aug-01', 'Sep-01', 'Oct-01')
        ) +
        
        # Secondary axis for precipitation
        scale_y_continuous(
          sec.axis = sec_axis(~ .,
                              breaks = c(minWL, minWL + 10),
                              name = 'Hourly Precip. (cm)',
                              labels = c('0', '2'))
        ) +
        
        # Theme
        theme_bw() +
        theme(
          plot.title = element_text(hjust = 0.5),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          axis.text.y.right = element_text(color = 'blue'),
          axis.title.y.right = element_text(color = 'blue'),
          strip.text = element_text(size = 11),
          legend.position = "bottom",
          legend.title = element_blank()
        )
      
      # Save the plot
      ggsave(file, plot = p, width = 12, height = 8, dpi = 300, bg = "white")
    }
  )
  
}

# Run app
shinyApp(ui, server)











#-------------------------------------------------------------------------------


#9/8/25 experimenting with precipitation line scaling 
library(shiny)
library(tidyverse)
library(lubridate)
library(patchwork)
library(shinyWidgets)
library(DT)
library(bslib)

# Read & prepare processed data
gm <- read.csv("data/processed_data/great_meadow_well_data_2024_20250715.csv") %>%
  rename(Date = date, Year = year, precip_cm = precip.cm) %>%
  mutate(Date = as.Date(Date),
         timestamp = as_datetime(timestamp),
         site = paste("Great Meadow", plot.num),
         water.depth = case_when(
           Year == 2016 & doy_h == 159.12 & plot.num == 3 & water.depth < -120 ~ NA_real_,
           Year == 2017 & doy_h == 215.02 & plot.num == 6 & water.depth < -115 ~ NA_real_,
           Year == 2021 & plot.num == 3 & doy == 224 & water.depth > 400 ~ NA_real_,
           Year == 2021 & plot.num == 3 & doy == 225 & water.depth > 400 ~ NA_real_,
           TRUE ~ water.depth
         )) %>%
  mutate(siteyear = paste(site, Year, sep = "_"))

gl <- read.csv("data/raw_data/hydrology_data/gilmore_well_prec_data_2013-2024.csv") %>%
  rename(water.depth = GILM_WL) %>%
  mutate(
    site = "Gilmore Meadow",
    
    # Try to parse full datetime, fallback to date only + midnight
    timestamp_parsed = mdy_hm(timestamp),
    timestamp_parsed = if_else(
      is.na(timestamp_parsed),
      mdy(timestamp),
      timestamp_parsed
    ),
    timestamp_parsed = as.POSIXct(timestamp_parsed),
    timestamp = timestamp_parsed,
    
    # Standardize Date column too
    Date = mdy(Date)
  ) %>%
  select(timestamp, 
         Date,
         doy,
         Year,
         precip_cm,
         water.depth,
         lag.precip,
         hr,
         doy_h,
         site)

all_data <- bind_rows(gm, gl) %>% 
  filter(Year >= 2016 & Year <= 2023) %>% 
  select(timestamp, 
         date = Date, 
         year = Year, 
         doy, 
         hr, 
         doy_h,
         precip_cm,
         lag_precip = lag.precip,
         water_depth = water.depth,
         site)

wl_stats <- read.csv("data/processed_data/gm_gl_wl_stats.csv") %>% 
  select(year,
         stat, 
         `Gilmore Meadow` = gilmore.meadow, 
         `Great Meadow 1` = great.meadow.1, 
         `Great Meadow 2` = great.meadow.2, 
         `Great Meadow 3` = great.meadow.3, 
         `Great Meadow 4` = great.meadow.4, 
         `Great Meadow 5` = great.meadow.5, 
         `Great Meadow 6` = great.meadow.6) %>% 
  pivot_longer(cols = -c(year, stat), names_to = "site", values_to = "value") %>% 
  pivot_wider(names_from = stat, values_from = value) %>%
  arrange(site, year) %>% 
  mutate(
    wetland = case_when(
      grepl("Great Meadow", site, ignore.case = TRUE) ~ "Great Meadow",
      grepl("Gilmore Meadow", site, ignore.case = TRUE) ~ "Gilmore Meadow",
      TRUE ~ "Unknown"
    )
  )


# Function to calculate significance tests between wetlands
calculate_wetland_significance <- function(data, selected_years, selected_sites, alpha = 0.05) {
  # Filter data for selected years and sites
  filtered_data <- data %>%
    filter(year %in% selected_years, site %in% selected_sites) %>%
    mutate(site_group = ifelse(grepl("Great Meadow", site), "Great Meadow", site))
  
  # Check if we have both wetlands represented in the selected sites
  wetlands_present <- unique(filtered_data$site_group)
  
  # Only run tests if both wetlands are present
  if (length(wetlands_present) < 2 || !all(c("Great Meadow", "Gilmore Meadow") %in% wetlands_present)) {
    return(NULL)  # Return NULL if we can't compare between wetlands
  }
  
  # Get all numeric stat columns
  stat_cols <- filtered_data %>% select(where(is.numeric), -year) %>% names()
  
  # Run t-tests for each variable and store results
  t_test_results <- map_dfr(stat_cols, function(var) {
    formula <- as.formula(paste(var, "~ site_group"))
    tryCatch({
      test <- t.test(formula, data = filtered_data)
      data.frame(
        variable = var,
        p_value = test$p.value,
        significant = test$p.value < alpha,
        stringsAsFactors = FALSE
      )
    }, error = function(e) {
      data.frame(
        variable = var,
        p_value = NA,
        significant = FALSE,
        stringsAsFactors = FALSE
      )
    })
  })
  
  return(t_test_results)
}


# UI 
ui <- page_fluid(
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#1B365D",
    secondary = "#4C6D9A",    
    success = "#2E86C1",
    info = "#3498db",
    warning = "#f39c12",
    danger = "#e74c3c",
    base_font = font_google("Open Sans"),
    heading_font = font_google("Open Sans", wght = c(400, 700))
  ),
  
  # Custom CSS for additional styling
  tags$head(
    tags$style(HTML("
      .content-section {
        margin: 30px 0;
        padding: 25px;
        border-radius: 15px;
        box-shadow: 0 4px 12px rgba(0,0,0,0.1);
        background: linear-gradient(135deg, #ffffff 0%, #f8f9fa 100%);
        border: 2px solid #1B365D;
      }
      
      .section-title {
        color: #1B365D;
        font-weight: 600;
        margin-bottom: 20px;
        padding-bottom: 10px;
        border-bottom: 2px solid #4C6D9A;
      }
      
      .sidebar-custom {
        background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%);
        border-radius: 10px;
        padding: 20px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.08);
        border: 1px solid #dee2e6;
      }
      
      .main-title {
        background: linear-gradient(135deg, #1B365D 0%, #4C6D9A 100%);
        color: white;
        padding: 30px;
        margin: -15px -15px 30px -15px;
        text-align: center;
        border-radius: 0 0 20px 20px;
        box-shadow: 0 4px 12px rgba(0,0,0,0.15);
      }
      
      .brush-info-section {
        background: linear-gradient(135deg, #e8f4f8 0%, #f0f8ff 100%);
        border-radius: 12px;
        padding: 20px;
        margin: 20px 0;
        border: 1px solid #4C6D9A;
      }
      
      .stats-main {
        background: linear-gradient(135deg, #f8f9fa 0%, #ffffff 100%);
      }
      
      .dataTables_wrapper {
        font-size: 0.85rem !important;
      }

      .dataTables_wrapper table {
        font-size: 0.8rem !important;
      }

      .dataTables_wrapper .dataTables_info,
      .dataTables_wrapper .dataTables_paginate {
        font-size: 0.75rem !important;
      }
      
    "))
  ),
  
  # Main title with gradient background
  div(class = "main-title",
      h1("Wetland Hydrograph Visualizer", 
         style = "margin: 0; font-size: 2rem; text-shadow: 2px 2px 4px rgba(0,0,0,0.3)")
  ),
  
  # First section: Hydrographs 
  div(class = "content-section",
      layout_sidebar(
        sidebar = sidebar(
          class = "sidebar-custom",
          width = 300,
          h4("Hydrograph Controls", style = "color: #1B365D; margin-bottom: 20px;"),
          
          pickerInput("selected_sites", 
                      label = div(icon("map-marker"), "Select Site(s):"),
                      choices = sort(unique(all_data$site)),
                      selected = c("Great Meadow 1", "Gilmore Meadow"),
                      multiple = TRUE,
                      options = list(
                        `actions-box` = TRUE,
                        `deselect-all-text` = "Clear all",
                        `select-all-text` = "Select all",
                        `none-selected-text` = "Choose site(s)",
                        `live-search` = TRUE,
                        style = "btn-outline-primary"
                      )),
          
          div(style = "margin-bottom: 15px;",
              pickerInput("year", 
                          label = div(icon("calendar"), "Select Year(s):"),
                          choices = sort(unique(all_data$year)),
                          selected = 2023,
                          multiple = TRUE,
                          options = list(
                            `actions-box` = TRUE,
                            `deselect-all-text` = "Clear all",
                            `select-all-text` = "Select all",
                            `none-selected-text` = "Choose year(s)",
                            `live-search` = TRUE,
                            style = "btn-outline-primary"
                          ))),
          
          br(),
          div(style = "padding: 15px; background-color: #e8f4f8; border-radius: 8px; border-left: 4px solid #3498db;",
              p(icon("info-circle"), " Use the brush tool to select data points on the hydrograph for a detailed view of the data.", 
                style = "margin: 0; font-size: 0.9rem; color: #2c3e50;")),
          
          div(style = "margin-top: 15px; text-align: center;",
              downloadButton("download_brush", "Download Selected Data", 
                             class = "btn-primary btn-sm", 
                             icon = icon("download")))
        ),
        
        card(
          full_screen = TRUE,
          card_header(
            class = "bg-primary text-white",
            "Hydrographs by Year"
          ),
          plotOutput("hydrograph", height = "600px", 
                     brush = brushOpts(id = "hydro_brush", fill = "#4C6D9A", opacity = 0.3))
        )
      )
  ),
  
  # Selected data points section with improved styling
  div(class = "brush-info-section",
      div(class = "row",
          div(class = "col-12",
              card(
                card_header(
                  class = "bg-success text-white",
                  "Selected Data from Hydrograph:"
                ),
                tableOutput("brush_info")
              )
          )
      )
  ),
  
  # Second section: Water Level Stats with improved styling
  div(class = "content-section stats-main",
      layout_sidebar(
        sidebar = sidebar(
          class = "sidebar-custom",
          width = 300,
          h4("Statistics Controls", style = "color: #1B365D; margin-bottom: 20px;"),
          
          pickerInput("stats_site", 
                      label = div(icon("map-marker"), "Select Site(s):"),
                      choices = sort(unique(wl_stats$site)),
                      selected = "Great Meadow 1",
                      multiple = TRUE,
                      options = list(
                        `actions-box` = TRUE,
                        `deselect-all-text` = "Clear all",
                        `select-all-text` = "Select all",
                        `none-selected-text` = "Choose site(s)",
                        `live-search` = TRUE,
                        style = "btn-outline-primary"
                      )),
          
          div(style = "margin-bottom: 15px;",
              pickerInput("stats_year", 
                          label = div(icon("calendar"), "Select Years:"),
                          choices = sort(unique(wl_stats$year)),
                          selected = c(2023, 2022, 2021, 2020),
                          multiple = TRUE,
                          options = list(
                            `actions-box` = TRUE,
                            `deselect-all-text` = "Clear all",
                            `select-all-text` = "Select all",
                            `none-selected-text` = "Choose year(s)",
                            `live-search` = TRUE,
                            style = "btn-outline-primary"
                          ))),
          
          div(style = "margin-bottom: 15px;",
              radioButtons("time_summary", "Summarize Stats:",
                           choices = c("Each Year" = "year", "Average Across Years" = "multi"),
                           selected = "year",
                           inline = TRUE)),
          
          br(),
          div(style = "padding: 15px; background-color: #e8f4f8; border-radius: 8px; border-left: 4px solid #3498db;",
              p(icon("info-circle"), " Growing Season statistics calculated from May through October.", 
                style = "margin: 0; font-size: 0.9rem; color: #2c3e50;")),
          
          div(style = "margin-top: 15px; text-align: center;",
              downloadButton("download_stats", "Download Table", 
                             class = "btn-primary btn-sm", 
                             icon = icon("download")))
        ),
        
        card(
          full_screen = TRUE,
          card_header(
            class = "bg-primary text-white",
            "Growing Season Water Level Statistics"
          ),
          
          div(style = "padding: 10px;",
              uiOutput("significance_info"),
              dataTableOutput("wl_stats"))
        )
      )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive data for plotting
  plot_data <- reactive({
    req(input$year, input$selected_sites)
    
    all_data %>%
      filter(year %in% input$year, doy > 134, doy < 275) %>%
      filter(site %in% input$selected_sites)
  })
  
  output$hydrograph <- renderPlot({
    plot_data_filtered <- plot_data()
    req(nrow(plot_data_filtered) > 0)
    
    # Calculate minimum water level across all data for consistent precipitation baseline
    minWL <- min(plot_data_filtered$water_depth, na.rm = TRUE)
    
    # Color palette for sites
    sites <- unique(plot_data_filtered$site)
    site_colors <- c(
      "Great Meadow 1" = "black", "Great Meadow 2" = "chartreuse4", "Great Meadow 3" = "green",
      "Great Meadow 4" = "darkorange", "Great Meadow 5" = "deeppink2", "Great Meadow 6" = "purple",
      "Gilmore Meadow" = "darkgray", "Precipitation" = "blue"
    )
    
    # Create the plot
    ggplot(plot_data_filtered, aes(x = doy_h, y = water_depth)) +
      # Water level lines by site
      geom_line(aes(color = site), size = 0.7) +
      
      # Precipitation line (scaled by multiplier of 5 and offset by minWL)
      geom_line(aes(x = doy_h, y = lag_precip * 5 + minWL, color = "Precipitation"), 
                size = 0.7) +
      
      # Ground level reference
      geom_hline(yintercept = 0, color = 'brown') +
      
      # Facet by year
      facet_wrap(~ year, ncol = 1, scales = "free_y") +
      
      # Colors
      scale_color_manual(values = site_colors, 
                         breaks = c(sites, "Precipitation")) +
      
      # Axes and labels
      labs(y = 'Water Level (cm)', x = 'Date') +
      scale_x_continuous(
        breaks = c(121, 152, 182, 213, 244, 274),
        labels = c('May-01', 'Jun-01', 'Jul-01', 'Aug-01', 'Sep-01', 'Oct-01')
      ) +
      
      # Secondary axis for precipitation
      scale_y_continuous(
        sec.axis = sec_axis(~ .,
                            breaks = c(minWL, minWL + 10),
                            name = 'Hourly Precip. (cm)',
                            labels = c('0', '2'))
      ) +
      
      # Theme
      theme_bw() +
      theme(
        plot.title = element_text(hjust = 0.5),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.text.y.right = element_text(color = 'blue'),
        axis.title.y.right = element_text(color = 'blue'),
        strip.text = element_text(size = 11),
        legend.position = "bottom",
        legend.title = element_blank()
      )
  })
  
  # Reactive for processed brushed data
  processed_brush_data <- reactive({
    req(input$hydro_brush)
    
    plot_data_filtered <- plot_data()
    req(nrow(plot_data_filtered) > 0)
    
    selected_data <- brushedPoints(
      plot_data_filtered,
      brush = input$hydro_brush,
      xvar = "doy_h",
      yvar = "water_depth"
    ) %>%
      select(site, year, doy_h, timestamp, water_depth, lag_precip) %>%
      arrange(year, doy_h) %>%
      mutate(
        doy_h = round(doy_h, 2),
        water_depth = round(water_depth, 2),
        lag_precip = round(lag_precip, 3),
        timestamp = format(timestamp, "%Y-%m-%d %H:%M:%S")
      )
    
    if (nrow(selected_data) == 0) {
      return(data.frame(Message = "No points selected - try brushing over the water level lines"))
    }
    
    # Create the base data with timestamp and precipitation
    base_data <- selected_data %>%
      mutate(doy_h_rounded = round(doy_h, 1)) %>%
      group_by(year, doy_h_rounded) %>%
      summarise(
        timestamp = first(timestamp),
        doy_h_orig = first(doy_h),
        lag_precip = first(lag_precip),
        .groups = 'drop'
      )
    
    # Create water depth data for each site, taking the mean of duplicates
    water_depth_data <- selected_data %>%
      mutate(doy_h_rounded = round(doy_h, 1)) %>%
      group_by(year, doy_h_rounded, site) %>%
      summarise(water_depth = mean(water_depth, na.rm = TRUE), .groups = 'drop') %>%
      pivot_wider(names_from = site, values_from = water_depth, names_prefix = "")
    
    # Join the data together
    result_data <- base_data %>%
      left_join(water_depth_data, by = c("year", "doy_h_rounded")) %>%
      select(-doy_h_rounded) %>%
      rename(
        `Year` = year, 
        `Timestamp` = timestamp,
        `Day of Year` = doy_h_orig,
        `Precipitation (cm)` = lag_precip
      )
    
    # Add " Water Depth (cm)" suffix to site columns
    site_columns <- intersect(names(result_data), input$selected_sites)
    if (length(site_columns) > 0) {
      result_data <- result_data %>%
        rename_with(~ paste(.x, "Water Depth (cm)"), .cols = all_of(site_columns))
    }
    
    result_data %>% arrange(Year, `Day of Year`)
  })
  
  # Reactive for significance testing results - now checks if both wetlands are selected
  significance_results <- reactive({
    req(input$stats_year, input$stats_site)
    
    # Check if both wetlands are represented in selected sites
    selected_wetlands <- wl_stats %>%
      filter(site %in% input$stats_site) %>%
      pull(wetland) %>%
      unique()
    
    # Only calculate significance if both wetlands are represented
    if (length(selected_wetlands) >= 2 && all(c("Great Meadow", "Gilmore Meadow") %in% selected_wetlands)) {
      calculate_wetland_significance(wl_stats, input$stats_year, input$stats_site, alpha = 0.05)
    } else {
      NULL
    }
  })
  
  # Reactive to determine if we should show significance info
  show_significance_info <- reactive({
    req(input$stats_site, input$time_summary)
    
    if (input$time_summary != "multi") return(FALSE)
    
    selected_wetlands <- wl_stats %>%
      filter(site %in% input$stats_site) %>%
      pull(wetland) %>%
      unique()
    
    return(length(selected_wetlands) >= 2 && all(c("Great Meadow", "Gilmore Meadow") %in% selected_wetlands))
  })
  
  
  # setting up average WL stats and calculating stats output tables
  filtered_stats <- reactive({
    req(input$stats_site, input$stats_year, input$time_summary)
    
    data <- wl_stats %>%
      filter(site %in% input$stats_site, year %in% input$stats_year)
    
    if (input$time_summary == "year") {
      # ---- Option 1: Per-Year Summary ----
      data %>%
        mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
        select(
          Year = year,
          Site = site,
          `Mean Water Level (cm)` = WL_mean,
          `SD Water Level (cm)` = WL_sd,
          `Minimum Water Level (cm)` = WL_min,
          `Maximum Water Level (cm)` = WL_max,
          `Maximum Hourly Increase (cm)` = max_inc,
          `Maximum Hourly Decrease (cm)` = max_dec,
          `Growing Season Change (cm)` = GS_change,
          `GS % Surface Water` = prop_over_0cm,
          `GS % Within 30cm` = prop_bet_0_neg30cm,
          `GS % Over 30cm Deep` = prop_under_neg30cm
        )
    } else {
      # ---- Option 2: Average Across Years ----
      # Per-site summary
      data_site <- data %>%
        group_by(site, wetland) %>%
        summarise(
          Year = paste0(min(year), "–", max(year)),
          `Mean Water Level (cm)` = round(mean(WL_mean, na.rm = TRUE), 2),
          `SD Water Level (cm)` = round(mean(WL_sd, na.rm = TRUE), 2),
          `Minimum Water Level (cm)` = round(mean(WL_min, na.rm = TRUE), 2),
          `Maximum Water Level (cm)` = round(mean(WL_max, na.rm = TRUE), 2),
          `Maximum Hourly Increase (cm)` = round(mean(max_inc, na.rm = TRUE), 2),
          `Maximum Hourly Decrease (cm)` = round(mean(max_dec, na.rm = TRUE), 2),
          `Growing Season Change (cm)` = round(mean(GS_change, na.rm = TRUE), 2),
          `GS % Surface Water` = round(mean(prop_over_0cm, na.rm = TRUE), 2),
          `GS % Within 30cm` = round(mean(prop_bet_0_neg30cm, na.rm = TRUE), 2),
          `GS % Over 30cm Deep` = round(mean(prop_under_neg30cm, na.rm = TRUE), 2),
          .groups = "drop"
        )
      
      # Per-wetland "All Sites" summary
      data_wetland <- data %>%
        group_by(wetland) %>%
        summarise(
          site = "All Sites",
          Year = paste0(min(year), "–", max(year)),
          `Mean Water Level (cm)` = round(mean(WL_mean, na.rm = TRUE), 2),
          `SD Water Level (cm)` = round(mean(WL_sd, na.rm = TRUE), 2),
          `Minimum Water Level (cm)` = round(mean(WL_min, na.rm = TRUE), 2),
          `Maximum Water Level (cm)` = round(mean(WL_max, na.rm = TRUE), 2),
          `Maximum Hourly Increase (cm)` = round(mean(max_inc, na.rm = TRUE), 2),
          `Maximum Hourly Decrease (cm)` = round(mean(max_dec, na.rm = TRUE), 2),
          `Growing Season Change (cm)` = round(mean(GS_change, na.rm = TRUE), 2),
          `GS % Surface Water` = round(mean(prop_over_0cm, na.rm = TRUE), 2),
          `GS % Within 30cm` = round(mean(prop_bet_0_neg30cm, na.rm = TRUE), 2),
          `GS % Over 30cm Deep` = round(mean(prop_under_neg30cm, na.rm = TRUE), 2),
          .groups = "drop"
        )
      
      bind_rows(data_site, data_wetland) %>%
        distinct() %>% 
        rename(Site = site,
               Wetland = wetland)
    }
  })
  
  # reactive table output for selected data from hydrograph
  output$brush_info <- renderTable({
    processed_brush_data()
  })
  
  # Output for significance information display
  output$significance_info <- renderUI({
    if (show_significance_info()) {
      sig_results <- significance_results()
      
      if (!is.null(sig_results)) {
        sig_vars <- sig_results %>% 
          filter(significant) %>% 
          pull(variable)
        
        if (length(sig_vars) > 0) {
          # Map variable names to display names
          var_display_names <- c(
            "WL_mean" = "Mean Water Level",
            "WL_sd" = "SD Water Level", 
            "WL_min" = "Minimum Water Level",
            "WL_max" = "Maximum Water Level",
            "max_inc" = "Maximum Hourly Increase",
            "max_dec" = "Maximum Hourly Decrease",
            "GS_change" = "Growing Season Change",
            "prop_over_0cm" = "GS % Surface Water",
            "prop_bet_0_neg30cm" = "GS % Within 30cm",
            "prop_under_neg30cm" = "GS % Over 30cm Deep"
          )
          
          sig_display_names <- var_display_names[sig_vars]
          sig_display_names <- sig_display_names[!is.na(sig_display_names)]
          
          div(class = "significance-info",
              h5(icon("exclamation-triangle"), " Statistical Significance", 
                 style = "color: #856404; margin-bottom: 10px;"),
              p("Highlighted variables show significant differences (p < 0.05) between Great Meadow and Gilmore Meadow wetlands:", 
                style = "margin-bottom: 8px; font-size: 0.9rem;"),
              tags$ul(
                lapply(sig_display_names, function(name) {
                  tags$li(name, style = "font-size: 0.85rem; color: #856404;")
                })
              ),
              p("Statistical tests compare averages across all selected years and sites within each wetland.", 
                style = "margin-top: 5px; margin-bottom: 10px; font-size: 0.8rem; font-style: italic; color: #6c757d;")
          )
        }
      }
    }
  })
  
  # reactive table output for WL stats with significance highlighting
  output$wl_stats <- DT::renderDataTable({
    data <- filtered_stats()
    
    dt <- datatable(
      data,
      options = list(pageLength = 10, scrollX = TRUE)
    ) %>%
      formatStyle(
        'Site',
        target = 'cell',
        fontWeight = styleEqual("All Sites", "bold")
      )
    
    # Add significance highlighting only when appropriate
    if (show_significance_info()) {
      sig_results <- significance_results()
      
      if (!is.null(sig_results)) {
        # Map variable names to column names in the table
        var_mapping <- c(
          "WL_mean" = "Mean Water Level (cm)",
          "WL_sd" = "SD Water Level (cm)", 
          "WL_min" = "Minimum Water Level (cm)",
          "WL_max" = "Maximum Water Level (cm)",
          "max_inc" = "Maximum Hourly Increase (cm)",
          "max_dec" = "Maximum Hourly Decrease (cm)",
          "GS_change" = "Growing Season Change (cm)",
          "prop_over_0cm" = "GS % Surface Water",
          "prop_bet_0_neg30cm" = "GS % Within 30cm",
          "prop_under_neg30cm" = "GS % Over 30cm Deep"
        )
        
        # Apply highlighting for significant variables
        for (var_name in names(var_mapping)) {
          col_name <- var_mapping[var_name]
          sig_row <- sig_results[sig_results$variable == var_name, ]
          
          if (nrow(sig_row) > 0 && sig_row$significant) {
            dt <- dt %>%
              formatStyle(
                col_name,
                valueColumns = "Site",
                backgroundColor = styleEqual("All Sites", "#fff3cd"),
                fontWeight = styleEqual("All Sites", "bold")
              )
          }
        }
      }
    }
    
    dt
  })
  
  # Download handler for brushed data
  output$download_brush <- downloadHandler(
    filename = function() {
      paste("hydrograph_selected_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(processed_brush_data(), file, row.names = FALSE)
    }
  )
  
  # Download handler for water level stats
  output$download_stats <- downloadHandler(
    filename = function() {
      if (input$time_summary == "multi") {
        paste("wetland_averaged_stats_", Sys.Date(), ".csv", sep = "")
      } else {
        paste("site_stats_", Sys.Date(), ".csv", sep = "")
      }
    },
    content = function(file) {
      write.csv(filtered_stats(), file, row.names = FALSE)
    }
  )
  
}

# Run app
shinyApp(ui, server)

















library(shiny)
library(tidyverse)
library(lubridate)
library(patchwork)
library(shinyWidgets)
library(DT)
library(bslib)

# Read & prepare processed data
gm <- read.csv("data/processed_data/great_meadow_well_data_2024_20250715.csv") %>%
  rename(Date = date, Year = year, precip_cm = precip.cm) %>%
  mutate(Date = as.Date(Date),
         timestamp = as_datetime(timestamp),
         site = paste("Great Meadow", plot.num),
         water.depth = case_when(
           Year == 2016 & doy_h == 159.12 & plot.num == 3 & water.depth < -120 ~ NA_real_,
           Year == 2017 & doy_h == 215.02 & plot.num == 6 & water.depth < -115 ~ NA_real_,
           Year == 2021 & plot.num == 3 & doy == 224 & water.depth > 400 ~ NA_real_,
           Year == 2021 & plot.num == 3 & doy == 225 & water.depth > 400 ~ NA_real_,
           TRUE ~ water.depth
         )) %>%
  mutate(siteyear = paste(site, Year, sep = "_"))

gl <- read.csv("data/raw_data/hydrology_data/gilmore_well_prec_data_2013-2024.csv") %>%
  rename(water.depth = GILM_WL) %>%
  mutate(
    site = "Gilmore Meadow",
    
    # Try to parse full datetime, fallback to date only + midnight
    timestamp_parsed = mdy_hm(timestamp),
    timestamp_parsed = if_else(
      is.na(timestamp_parsed),
      mdy(timestamp),
      timestamp_parsed
    ),
    timestamp_parsed = as.POSIXct(timestamp_parsed),
    timestamp = timestamp_parsed,
    
    # Standardize Date column too
    Date = mdy(Date)
  ) %>%
  select(timestamp, 
         Date,
         doy,
         Year,
         precip_cm,
         water.depth,
         lag.precip,
         hr,
         doy_h,
         site)

all_data <- bind_rows(gm, gl) %>% 
  filter(Year >= 2016 & Year <= 2023) %>% 
  select(timestamp, 
         date = Date, 
         year = Year, 
         doy, 
         hr, 
         doy_h,
         precip_cm,
         lag_precip = lag.precip,
         water_depth = water.depth,
         site)

wl_stats <- read.csv("data/processed_data/gm_gl_wl_stats.csv") %>% 
  select(year,
         stat, 
         `Gilmore Meadow` = gilmore.meadow, 
         `Great Meadow 1` = great.meadow.1, 
         `Great Meadow 2` = great.meadow.2, 
         `Great Meadow 3` = great.meadow.3, 
         `Great Meadow 4` = great.meadow.4, 
         `Great Meadow 5` = great.meadow.5, 
         `Great Meadow 6` = great.meadow.6) %>% 
  pivot_longer(cols = -c(year, stat), names_to = "site", values_to = "value") %>% 
  pivot_wider(names_from = stat, values_from = value) %>%
  arrange(site, year) %>% 
  mutate(
    wetland = case_when(
      grepl("Great Meadow", site, ignore.case = TRUE) ~ "Great Meadow",
      grepl("Gilmore Meadow", site, ignore.case = TRUE) ~ "Gilmore Meadow",
      TRUE ~ "Unknown"
    )
  )


# Function to calculate significance tests between wetlands
calculate_wetland_significance <- function(data, selected_years, selected_sites, alpha = 0.05) {
  # Filter data for selected years and sites
  filtered_data <- data %>%
    filter(year %in% selected_years, site %in% selected_sites) %>%
    mutate(site_group = ifelse(grepl("Great Meadow", site), "Great Meadow", site))
  
  # Check if we have both wetlands represented in the selected sites
  wetlands_present <- unique(filtered_data$site_group)
  
  # Only run tests if both wetlands are present
  if (length(wetlands_present) < 2 || !all(c("Great Meadow", "Gilmore Meadow") %in% wetlands_present)) {
    return(NULL)  # Return NULL if we can't compare between wetlands
  }
  
  # Get all numeric stat columns
  stat_cols <- filtered_data %>% select(where(is.numeric), -year) %>% names()
  
  # Run t-tests for each variable and store results
  t_test_results <- map_dfr(stat_cols, function(var) {
    formula <- as.formula(paste(var, "~ site_group"))
    tryCatch({
      test <- t.test(formula, data = filtered_data)
      data.frame(
        variable = var,
        p_value = test$p.value,
        significant = test$p.value < alpha,
        stringsAsFactors = FALSE
      )
    }, error = function(e) {
      data.frame(
        variable = var,
        p_value = NA,
        significant = FALSE,
        stringsAsFactors = FALSE
      )
    })
  })
  
  return(t_test_results)
}


# UI 
ui <- page_fluid(
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#1B365D",
    secondary = "#4C6D9A",    
    success = "#2E86C1",
    info = "#3498db",
    warning = "#f39c12",
    danger = "#e74c3c",
    base_font = font_google("Open Sans"),
    heading_font = font_google("Open Sans", wght = c(400, 700))
  ),
  
  # Custom CSS for additional styling
  tags$head(
    tags$style(HTML("
      .content-section {
        margin: 30px 0;
        padding: 25px;
        border-radius: 15px;
        box-shadow: 0 4px 12px rgba(0,0,0,0.1);
        background: linear-gradient(135deg, #ffffff 0%, #f8f9fa 100%);
        border: 2px solid #1B365D;
      }
      
      .section-title {
        color: #1B365D;
        font-weight: 600;
        margin-bottom: 20px;
        padding-bottom: 10px;
        border-bottom: 2px solid #4C6D9A;
      }
      
      .sidebar-custom {
        background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%);
        border-radius: 10px;
        padding: 20px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.08);
        border: 1px solid #dee2e6;
      }
      
      .main-title {
        background: linear-gradient(135deg, #1B365D 0%, #4C6D9A 100%);
        color: white;
        padding: 30px;
        margin: -15px -15px 30px -15px;
        text-align: center;
        border-radius: 0 0 20px 20px;
        box-shadow: 0 4px 12px rgba(0,0,0,0.15);
      }
      
      .brush-info-section {
        background: linear-gradient(135deg, #e8f4f8 0%, #f0f8ff 100%);
        border-radius: 12px;
        padding: 20px;
        margin: 20px 0;
        border: 1px solid #4C6D9A;
      }
      
      .stats-main {
        background: linear-gradient(135deg, #f8f9fa 0%, #ffffff 100%);
      }
      
      .dataTables_wrapper {
        font-size: 0.85rem !important;
      }

      .dataTables_wrapper table {
        font-size: 0.8rem !important;
      }

      .dataTables_wrapper .dataTables_info,
      .dataTables_wrapper .dataTables_paginate {
        font-size: 0.75rem !important;
      }
      
    "))
  ),
  
  # Main title with gradient background
  div(class = "main-title",
      h1("Wetland Hydrograph Visualizer", 
         style = "margin: 0; font-size: 2rem; text-shadow: 2px 2px 4px rgba(0,0,0,0.3)")
  ),
  
  # First section: Hydrographs 
  div(class = "content-section",
      layout_sidebar(
        sidebar = sidebar(
          class = "sidebar-custom",
          width = 300,
          h4("Hydrograph Controls", style = "color: #1B365D; margin-bottom: 20px;"),
          
          pickerInput("selected_sites", 
                      label = div(icon("map-marker"), "Select Site(s):"),
                      choices = sort(unique(all_data$site)),
                      selected = c("Great Meadow 1", "Gilmore Meadow"),
                      multiple = TRUE,
                      options = list(
                        `actions-box` = TRUE,
                        `deselect-all-text` = "Clear all",
                        `select-all-text` = "Select all",
                        `none-selected-text` = "Choose site(s)",
                        `live-search` = TRUE,
                        style = "btn-outline-primary"
                      )),
          
          div(style = "margin-bottom: 15px;",
              pickerInput("year", 
                          label = div(icon("calendar"), "Select Year(s):"),
                          choices = sort(unique(all_data$year)),
                          selected = 2023,
                          multiple = TRUE,
                          options = list(
                            `actions-box` = TRUE,
                            `deselect-all-text` = "Clear all",
                            `select-all-text` = "Select all",
                            `none-selected-text` = "Choose year(s)",
                            `live-search` = TRUE,
                            style = "btn-outline-primary"
                          ))),
          
          br(),
          div(style = "padding: 15px; background-color: #e8f4f8; border-radius: 8px; border-left: 4px solid #3498db;",
              p(icon("info-circle"), " Use the brush tool to select data points on the hydrograph for a detailed view of the data.", 
                style = "margin: 0; font-size: 0.9rem; color: #2c3e50;")),
          
          div(style = "margin-top: 15px; text-align: center;",
              downloadButton("download_brush", "Download Selected Data", 
                             class = "btn-primary btn-sm", 
                             icon = icon("download")))
        ),
        
        card(
          full_screen = TRUE,
          card_header(
            class = "bg-primary text-white",
            "Hydrographs by Year"
          ),
          plotOutput("hydrograph", height = "600px", 
                     brush = brushOpts(id = "hydro_brush", fill = "#4C6D9A", opacity = 0.3))
        )
      )
  ),
  
  # Selected data points section with improved styling
  div(class = "brush-info-section",
      div(class = "row",
          div(class = "col-12",
              card(
                card_header(
                  class = "bg-success text-white",
                  "Selected Data from Hydrograph:"
                ),
                tableOutput("brush_info")
              )
          )
      )
  ),
  
  # Second section: Water Level Stats with improved styling
  div(class = "content-section stats-main",
      layout_sidebar(
        sidebar = sidebar(
          class = "sidebar-custom",
          width = 300,
          h4("Statistics Controls", style = "color: #1B365D; margin-bottom: 20px;"),
          
          pickerInput("stats_site", 
                      label = div(icon("map-marker"), "Select Site(s):"),
                      choices = sort(unique(wl_stats$site)),
                      selected = "Great Meadow 1",
                      multiple = TRUE,
                      options = list(
                        `actions-box` = TRUE,
                        `deselect-all-text` = "Clear all",
                        `select-all-text` = "Select all",
                        `none-selected-text` = "Choose site(s)",
                        `live-search` = TRUE,
                        style = "btn-outline-primary"
                      )),
          
          div(style = "margin-bottom: 15px;",
              pickerInput("stats_year", 
                          label = div(icon("calendar"), "Select Years:"),
                          choices = sort(unique(wl_stats$year)),
                          selected = c(2023, 2022, 2021, 2020),
                          multiple = TRUE,
                          options = list(
                            `actions-box` = TRUE,
                            `deselect-all-text` = "Clear all",
                            `select-all-text` = "Select all",
                            `none-selected-text` = "Choose year(s)",
                            `live-search` = TRUE,
                            style = "btn-outline-primary"
                          ))),
          
          div(style = "margin-bottom: 15px;",
              radioButtons("time_summary", "Summarize Stats:",
                           choices = c("Each Year" = "year", "Average Across Years" = "multi"),
                           selected = "year",
                           inline = TRUE)),
          
          br(),
          div(style = "padding: 15px; background-color: #e8f4f8; border-radius: 8px; border-left: 4px solid #3498db;",
              p(icon("info-circle"), " Growing Season statistics calculated from May through October.", 
                style = "margin: 0; font-size: 0.9rem; color: #2c3e50;")),
          
          div(style = "margin-top: 15px; text-align: center;",
              downloadButton("download_stats", "Download Table", 
                             class = "btn-primary btn-sm", 
                             icon = icon("download")))
        ),
        
        card(
          full_screen = TRUE,
          card_header(
            class = "bg-primary text-white",
            "Growing Season Water Level Statistics"
          ),
          
          div(style = "padding: 10px;",
              uiOutput("significance_info"),
              dataTableOutput("wl_stats"))
        )
      )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive data for plotting
  plot_data <- reactive({
    req(input$year, input$selected_sites)
    
    all_data %>%
      filter(year %in% input$year, doy > 134, doy < 275) %>%
      filter(site %in% input$selected_sites)
  })
  
  output$hydrograph <- renderPlot({
    plot_data_filtered <- plot_data()
    req(nrow(plot_data_filtered) > 0)
    
    # Calculate minimum water level across all data for consistent precipitation baseline
    minWL <- min(plot_data_filtered$water_depth, na.rm = TRUE)
    
    # Color palette for sites
    sites <- unique(plot_data_filtered$site)
    site_colors <- c(
      "Great Meadow 1" = "black", "Great Meadow 2" = "chartreuse4", "Great Meadow 3" = "green",
      "Great Meadow 4" = "darkorange", "Great Meadow 5" = "deeppink2", "Great Meadow 6" = "purple",
      "Gilmore Meadow" = "darkgray", "Precipitation" = "blue"
    )
    
    # Create the plot
    ggplot(plot_data_filtered, aes(x = doy_h, y = water_depth)) +
      # Water level lines by site
      geom_line(aes(color = site), size = 0.7) +
      
      # Precipitation line (scaled by multiplier of 5 and offset by minWL)
      geom_line(aes(x = doy_h, y = lag_precip * 5 + minWL, color = "Precipitation"), 
                size = 0.7) +
      
      # Ground level reference
      geom_hline(yintercept = 0, color = 'brown') +
      
      # Facet by year
      facet_wrap(~ year, ncol = 1, scales = "free_y") +
      
      # Colors
      scale_color_manual(values = site_colors, 
                         breaks = c(sites, "Precipitation")) +
      
      # Axes and labels
      labs(y = 'Water Level (cm)', x = 'Date') +
      scale_x_continuous(
        breaks = c(121, 152, 182, 213, 244, 274),
        labels = c('May-01', 'Jun-01', 'Jul-01', 'Aug-01', 'Sep-01', 'Oct-01')
      ) +
      
      # Secondary axis for precipitation
      scale_y_continuous(
        sec.axis = sec_axis(~ .,
                            breaks = c(minWL, minWL + 10),
                            name = 'Hourly Precip. (cm)',
                            labels = c('0', '2'))
      ) +
      
      # Theme
      theme_bw() +
      theme(
        plot.title = element_text(hjust = 0.5),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.text.y.right = element_text(color = 'blue'),
        axis.title.y.right = element_text(color = 'blue'),
        strip.text = element_text(size = 11),
        legend.position = "bottom",
        legend.title = element_blank()
      )
  })
  
  # Reactive for processed brushed data - simplified approach like the example
  processed_brush_data <- reactive({
    req(input$hydro_brush)
    
    plot_data_filtered <- plot_data()
    req(nrow(plot_data_filtered) > 0)
    
    # Simple approach: just show the brushed points as rows (like the example)
    selected_data <- brushedPoints(
      plot_data_filtered,
      brush = input$hydro_brush,
      xvar = "doy_h",
      yvar = "water_depth"
    ) %>%
      select(timestamp, year, doy_h, site, water_depth, lag_precip) %>%
      arrange(year, doy_h) %>%
      mutate(
        doy_h = round(doy_h, 2),
        water_depth = round(water_depth, 2),
        lag_precip = round(lag_precip, 3),
        timestamp = format(timestamp, "%Y-%m-%d %H:%M:%S")
      ) %>%
      rename(
        Timestamp = timestamp,
        Year = year,
        `Day of Year` = doy_h,
        Site = site,
        `Water Depth (cm)` = water_depth,
        `Precipitation (cm)` = lag_precip
      )
    
    if (nrow(selected_data) == 0) {
      return(data.frame(Message = "No points selected - try brushing over the water level lines"))
    }
    
    return(selected_data)
  })
  
  # Reactive for significance testing results - now checks if both wetlands are selected
  significance_results <- reactive({
    req(input$stats_year, input$stats_site)
    
    # Check if both wetlands are represented in selected sites
    selected_wetlands <- wl_stats %>%
      filter(site %in% input$stats_site) %>%
      pull(wetland) %>%
      unique()
    
    # Only calculate significance if both wetlands are represented
    if (length(selected_wetlands) >= 2 && all(c("Great Meadow", "Gilmore Meadow") %in% selected_wetlands)) {
      calculate_wetland_significance(wl_stats, input$stats_year, input$stats_site, alpha = 0.05)
    } else {
      NULL
    }
  })
  
  # Reactive to determine if we should show significance info
  show_significance_info <- reactive({
    req(input$stats_site, input$time_summary)
    
    if (input$time_summary != "multi") return(FALSE)
    
    selected_wetlands <- wl_stats %>%
      filter(site %in% input$stats_site) %>%
      pull(wetland) %>%
      unique()
    
    return(length(selected_wetlands) >= 2 && all(c("Great Meadow", "Gilmore Meadow") %in% selected_wetlands))
  })
  
  
  # setting up average WL stats and calculating stats output tables
  filtered_stats <- reactive({
    req(input$stats_site, input$stats_year, input$time_summary)
    
    data <- wl_stats %>%
      filter(site %in% input$stats_site, year %in% input$stats_year)
    
    if (input$time_summary == "year") {
      # ---- Option 1: Per-Year Summary ----
      data %>%
        mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
        select(
          Year = year,
          Site = site,
          `Mean Water Level (cm)` = WL_mean,
          `SD Water Level (cm)` = WL_sd,
          `Minimum Water Level (cm)` = WL_min,
          `Maximum Water Level (cm)` = WL_max,
          `Maximum Hourly Increase (cm)` = max_inc,
          `Maximum Hourly Decrease (cm)` = max_dec,
          `Growing Season Change (cm)` = GS_change,
          `GS % Surface Water` = prop_over_0cm,
          `GS % Within 30cm` = prop_bet_0_neg30cm,
          `GS % Over 30cm Deep` = prop_under_neg30cm
        )
    } else {
      # ---- Option 2: Average Across Years ----
      # Per-site summary
      data_site <- data %>%
        group_by(site, wetland) %>%
        summarise(
          Year = paste0(min(year), "–", max(year)),
          `Mean Water Level (cm)` = round(mean(WL_mean, na.rm = TRUE), 2),
          `SD Water Level (cm)` = round(mean(WL_sd, na.rm = TRUE), 2),
          `Minimum Water Level (cm)` = round(mean(WL_min, na.rm = TRUE), 2),
          `Maximum Water Level (cm)` = round(mean(WL_max, na.rm = TRUE), 2),
          `Maximum Hourly Increase (cm)` = round(mean(max_inc, na.rm = TRUE), 2),
          `Maximum Hourly Decrease (cm)` = round(mean(max_dec, na.rm = TRUE), 2),
          `Growing Season Change (cm)` = round(mean(GS_change, na.rm = TRUE), 2),
          `GS % Surface Water` = round(mean(prop_over_0cm, na.rm = TRUE), 2),
          `GS % Within 30cm` = round(mean(prop_bet_0_neg30cm, na.rm = TRUE), 2),
          `GS % Over 30cm Deep` = round(mean(prop_under_neg30cm, na.rm = TRUE), 2),
          .groups = "drop"
        )
      
      # Per-wetland "All Sites" summary
      data_wetland <- data %>%
        group_by(wetland) %>%
        summarise(
          site = "All Sites",
          Year = paste0(min(year), "–", max(year)),
          `Mean Water Level (cm)` = round(mean(WL_mean, na.rm = TRUE), 2),
          `SD Water Level (cm)` = round(mean(WL_sd, na.rm = TRUE), 2),
          `Minimum Water Level (cm)` = round(mean(WL_min, na.rm = TRUE), 2),
          `Maximum Water Level (cm)` = round(mean(WL_max, na.rm = TRUE), 2),
          `Maximum Hourly Increase (cm)` = round(mean(max_inc, na.rm = TRUE), 2),
          `Maximum Hourly Decrease (cm)` = round(mean(max_dec, na.rm = TRUE), 2),
          `Growing Season Change (cm)` = round(mean(GS_change, na.rm = TRUE), 2),
          `GS % Surface Water` = round(mean(prop_over_0cm, na.rm = TRUE), 2),
          `GS % Within 30cm` = round(mean(prop_bet_0_neg30cm, na.rm = TRUE), 2),
          `GS % Over 30cm Deep` = round(mean(prop_under_neg30cm, na.rm = TRUE), 2),
          .groups = "drop"
        )
      
      bind_rows(data_site, data_wetland) %>%
        distinct() %>% 
        rename(Site = site,
               Wetland = wetland)
    }
  })
  
  # reactive table output for selected data from hydrograph
  output$brush_info <- renderTable({
    processed_brush_data()
  })
  
  # Output for significance information display
  output$significance_info <- renderUI({
    if (show_significance_info()) {
      sig_results <- significance_results()
      
      if (!is.null(sig_results)) {
        sig_vars <- sig_results %>% 
          filter(significant) %>% 
          pull(variable)
        
        if (length(sig_vars) > 0) {
          # Map variable names to display names
          var_display_names <- c(
            "WL_mean" = "Mean Water Level",
            "WL_sd" = "SD Water Level", 
            "WL_min" = "Minimum Water Level",
            "WL_max" = "Maximum Water Level",
            "max_inc" = "Maximum Hourly Increase",
            "max_dec" = "Maximum Hourly Decrease",
            "GS_change" = "Growing Season Change",
            "prop_over_0cm" = "GS % Surface Water",
            "prop_bet_0_neg30cm" = "GS % Within 30cm",
            "prop_under_neg30cm" = "GS % Over 30cm Deep"
          )
          
          sig_display_names <- var_display_names[sig_vars]
          sig_display_names <- sig_display_names[!is.na(sig_display_names)]
          
          div(class = "significance-info",
              h5(icon("exclamation-triangle"), " Statistical Significance", 
                 style = "color: #856404; margin-bottom: 10px;"),
              p("Highlighted variables show significant differences (p < 0.05) between Great Meadow and Gilmore Meadow wetlands:", 
                style = "margin-bottom: 8px; font-size: 0.9rem;"),
              tags$ul(
                lapply(sig_display_names, function(name) {
                  tags$li(name, style = "font-size: 0.85rem; color: #856404;")
                })
              ),
              p("Statistical tests compare averages across all selected years and sites within each wetland.", 
                style = "margin-top: 5px; margin-bottom: 10px; font-size: 0.8rem; font-style: italic; color: #6c757d;")
          )
        }
      }
    }
  })
  
  # reactive table output for WL stats with significance highlighting
  output$wl_stats <- DT::renderDataTable({
    data <- filtered_stats()
    
    dt <- datatable(
      data,
      options = list(pageLength = 10, scrollX = TRUE)
    ) %>%
      formatStyle(
        'Site',
        target = 'cell',
        fontWeight = styleEqual("All Sites", "bold")
      )
    
    # Add significance highlighting only when appropriate
    if (show_significance_info()) {
      sig_results <- significance_results()
      
      if (!is.null(sig_results)) {
        # Map variable names to column names in the table
        var_mapping <- c(
          "WL_mean" = "Mean Water Level (cm)",
          "WL_sd" = "SD Water Level (cm)", 
          "WL_min" = "Minimum Water Level (cm)",
          "WL_max" = "Maximum Water Level (cm)",
          "max_inc" = "Maximum Hourly Increase (cm)",
          "max_dec" = "Maximum Hourly Decrease (cm)",
          "GS_change" = "Growing Season Change (cm)",
          "prop_over_0cm" = "GS % Surface Water",
          "prop_bet_0_neg30cm" = "GS % Within 30cm",
          "prop_under_neg30cm" = "GS % Over 30cm Deep"
        )
        
        # Apply highlighting for significant variables
        for (var_name in names(var_mapping)) {
          col_name <- var_mapping[var_name]
          sig_row <- sig_results[sig_results$variable == var_name, ]
          
          if (nrow(sig_row) > 0 && sig_row$significant) {
            dt <- dt %>%
              formatStyle(
                col_name,
                valueColumns = "Site",
                backgroundColor = styleEqual("All Sites", "#fff3cd"),
                fontWeight = styleEqual("All Sites", "bold")
              )
          }
        }
      }
    }
    
    dt
  })
  
  # Download handler for brushed data
  output$download_brush <- downloadHandler(
    filename = function() {
      paste("hydrograph_selected_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(processed_brush_data(), file, row.names = FALSE)
    }
  )
  
  # Download handler for water level stats
  output$download_stats <- downloadHandler(
    filename = function() {
      if (input$time_summary == "multi") {
        paste("wetland_averaged_stats_", Sys.Date(), ".csv", sep = "")
      } else {
        paste("site_stats_", Sys.Date(), ".csv", sep = "")
      }
    },
    content = function(file) {
      write.csv(filtered_stats(), file, row.names = FALSE)
    }
  )
  
}

# Run app
shinyApp(ui, server)













library(shiny)
library(tidyverse)
library(lubridate)
library(patchwork)
library(shinyWidgets)
library(DT)
library(bslib)

# Read & prepare processed data
gm <- read.csv("data/processed_data/great_meadow_well_data_2024_20250715.csv") %>%
  rename(Date = date, Year = year, precip_cm = precip.cm) %>%
  mutate(Date = as.Date(Date),
         timestamp = as_datetime(timestamp),
         site = paste("Great Meadow", plot.num),
         water.depth = case_when(
           Year == 2016 & doy_h == 159.12 & plot.num == 3 & water.depth < -120 ~ NA_real_,
           Year == 2017 & doy_h == 215.02 & plot.num == 6 & water.depth < -115 ~ NA_real_,
           Year == 2021 & plot.num == 3 & doy == 224 & water.depth > 400 ~ NA_real_,
           Year == 2021 & plot.num == 3 & doy == 225 & water.depth > 400 ~ NA_real_,
           TRUE ~ water.depth
         )) %>%
  mutate(siteyear = paste(site, Year, sep = "_"))

gl <- read.csv("data/raw_data/hydrology_data/gilmore_well_prec_data_2013-2024.csv") %>%
  rename(water.depth = GILM_WL) %>%
  mutate(
    site = "Gilmore Meadow",
    
    # Try to parse full datetime, fallback to date only + midnight
    timestamp_parsed = mdy_hm(timestamp),
    timestamp_parsed = if_else(
      is.na(timestamp_parsed),
      mdy(timestamp),
      timestamp_parsed
    ),
    timestamp_parsed = as.POSIXct(timestamp_parsed),
    timestamp = timestamp_parsed,
    
    # Standardize Date column too
    Date = mdy(Date)
  ) %>%
  select(timestamp, 
         Date,
         doy,
         Year,
         precip_cm,
         water.depth,
         lag.precip,
         hr,
         doy_h,
         site)

all_data <- bind_rows(gm, gl) %>% 
  filter(Year >= 2016 & Year <= 2023) %>% 
  select(timestamp, 
         date = Date, 
         year = Year, 
         doy, 
         hr, 
         doy_h,
         precip_cm,
         lag_precip = lag.precip,
         water_depth = water.depth,
         site)

wl_stats <- read.csv("data/processed_data/gm_gl_wl_stats.csv") %>% 
  select(year,
         stat, 
         `Gilmore Meadow` = gilmore.meadow, 
         `Great Meadow 1` = great.meadow.1, 
         `Great Meadow 2` = great.meadow.2, 
         `Great Meadow 3` = great.meadow.3, 
         `Great Meadow 4` = great.meadow.4, 
         `Great Meadow 5` = great.meadow.5, 
         `Great Meadow 6` = great.meadow.6) %>% 
  pivot_longer(cols = -c(year, stat), names_to = "site", values_to = "value") %>% 
  pivot_wider(names_from = stat, values_from = value) %>%
  arrange(site, year) %>% 
  mutate(
    wetland = case_when(
      grepl("Great Meadow", site, ignore.case = TRUE) ~ "Great Meadow",
      grepl("Gilmore Meadow", site, ignore.case = TRUE) ~ "Gilmore Meadow",
      TRUE ~ "Unknown"
    )
  )


# Function to calculate significance tests between wetlands
calculate_wetland_significance <- function(data, selected_years, selected_sites, alpha = 0.05) {
  # Filter data for selected years and sites
  filtered_data <- data %>%
    filter(year %in% selected_years, site %in% selected_sites) %>%
    mutate(site_group = ifelse(grepl("Great Meadow", site), "Great Meadow", site))
  
  # Check if we have both wetlands represented in the selected sites
  wetlands_present <- unique(filtered_data$site_group)
  
  # Only run tests if both wetlands are present
  if (length(wetlands_present) < 2 || !all(c("Great Meadow", "Gilmore Meadow") %in% wetlands_present)) {
    return(NULL)  # Return NULL if we can't compare between wetlands
  }
  
  # Get all numeric stat columns
  stat_cols <- filtered_data %>% select(where(is.numeric), -year) %>% names()
  
  # Run t-tests for each variable and store results
  t_test_results <- map_dfr(stat_cols, function(var) {
    formula <- as.formula(paste(var, "~ site_group"))
    tryCatch({
      test <- t.test(formula, data = filtered_data)
      data.frame(
        variable = var,
        p_value = test$p.value,
        significant = test$p.value < alpha,
        stringsAsFactors = FALSE
      )
    }, error = function(e) {
      data.frame(
        variable = var,
        p_value = NA,
        significant = FALSE,
        stringsAsFactors = FALSE
      )
    })
  })
  
  return(t_test_results)
}


# UI 
ui <- page_fluid(
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#1B365D",
    secondary = "#4C6D9A",    
    success = "#2E86C1",
    info = "#3498db",
    warning = "#f39c12",
    danger = "#e74c3c",
    base_font = font_google("Open Sans"),
    heading_font = font_google("Open Sans", wght = c(400, 700))
  ),
  
  # Custom CSS for additional styling
  tags$head(
    tags$style(HTML("
      .content-section {
        margin: 30px 0;
        padding: 25px;
        border-radius: 15px;
        box-shadow: 0 4px 12px rgba(0,0,0,0.1);
        background: linear-gradient(135deg, #ffffff 0%, #f8f9fa 100%);
        border: 2px solid #1B365D;
      }
      
      .section-title {
        color: #1B365D;
        font-weight: 600;
        margin-bottom: 20px;
        padding-bottom: 10px;
        border-bottom: 2px solid #4C6D9A;
      }
      
      .sidebar-custom {
        background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%);
        border-radius: 10px;
        padding: 20px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.08);
        border: 1px solid #dee2e6;
      }
      
      .main-title {
        background: linear-gradient(135deg, #1B365D 0%, #4C6D9A 100%);
        color: white;
        padding: 30px;
        margin: -15px -15px 30px -15px;
        text-align: center;
        border-radius: 0 0 20px 20px;
        box-shadow: 0 4px 12px rgba(0,0,0,0.15);
      }
      
      .brush-info-section {
        background: linear-gradient(135deg, #e8f4f8 0%, #f0f8ff 100%);
        border-radius: 12px;
        padding: 20px;
        margin: 20px 0;
        border: 1px solid #4C6D9A;
      }
      
      .stats-main {
        background: linear-gradient(135deg, #f8f9fa 0%, #ffffff 100%);
      }
      
      .dataTables_wrapper {
        font-size: 0.85rem !important;
      }

      .dataTables_wrapper table {
        font-size: 0.8rem !important;
      }

      .dataTables_wrapper .dataTables_info,
      .dataTables_wrapper .dataTables_paginate {
        font-size: 0.75rem !important;
      }
      
    "))
  ),
  
  # Main title with gradient background
  div(class = "main-title",
      h1("Wetland Hydrograph Visualizer", 
         style = "margin: 0; font-size: 2rem; text-shadow: 2px 2px 4px rgba(0,0,0,0.3)")
  ),
  
  # First section: Hydrographs 
  div(class = "content-section",
      layout_sidebar(
        sidebar = sidebar(
          class = "sidebar-custom",
          width = 300,
          h4("Hydrograph Controls", style = "color: #1B365D; margin-bottom: 20px;"),
          
          pickerInput("selected_sites", 
                      label = div(icon("map-marker"), "Select Site(s):"),
                      choices = sort(unique(all_data$site)),
                      selected = c("Great Meadow 1", "Gilmore Meadow"),
                      multiple = TRUE,
                      options = list(
                        `actions-box` = TRUE,
                        `deselect-all-text` = "Clear all",
                        `select-all-text` = "Select all",
                        `none-selected-text` = "Choose site(s)",
                        `live-search` = TRUE,
                        style = "btn-outline-primary"
                      )),
          
          div(style = "margin-bottom: 15px;",
              pickerInput("year", 
                          label = div(icon("calendar"), "Select Year(s):"),
                          choices = sort(unique(all_data$year)),
                          selected = 2023,
                          multiple = TRUE,
                          options = list(
                            `actions-box` = TRUE,
                            `deselect-all-text` = "Clear all",
                            `select-all-text` = "Select all",
                            `none-selected-text` = "Choose year(s)",
                            `live-search` = TRUE,
                            style = "btn-outline-primary"
                          ))),
          
          br(),
          div(style = "padding: 15px; background-color: #e8f4f8; border-radius: 8px; border-left: 4px solid #3498db;",
              p(icon("info-circle"), " Use the brush tool to select data points on the hydrograph for a detailed view of the data.", 
                style = "margin: 0; font-size: 0.9rem; color: #2c3e50;")),
          
          
          div(style = "margin-top: 10px; text-align: center;",
              downloadButton("download_plot", "Download Hydrograph", 
                             class = "btn-primary btn-sm", 
                             icon = icon("image"))),
          
          div(style = "margin-top: 10px; text-align: center;",
              downloadButton("download_brush", "Download Selected Data", 
                             class = "btn-primary btn-sm", 
                             icon = icon("download")))
        ),
        
        card(
          full_screen = TRUE,
          card_header(
            class = "bg-primary text-white",
            "Hydrographs by Year"
          ),
          plotOutput("hydrograph", height = "600px", 
                     brush = brushOpts(id = "hydro_brush", fill = "#4C6D9A", opacity = 0.3))
        )
      )
  ),
  
  # Selected data points section with improved styling
  div(class = "brush-info-section",
      div(class = "row",
          div(class = "col-12",
              card(
                card_header(
                  class = "bg-success text-white",
                  "Selected Data from Hydrograph:"
                ),
                tableOutput("brush_info")
              )
          )
      )
  ),
  
  # Second section: Water Level Stats with improved styling
  div(class = "content-section stats-main",
      layout_sidebar(
        sidebar = sidebar(
          class = "sidebar-custom",
          width = 300,
          h4("Statistics Controls", style = "color: #1B365D; margin-bottom: 20px;"),
          
          pickerInput("stats_site", 
                      label = div(icon("map-marker"), "Select Site(s):"),
                      choices = sort(unique(wl_stats$site)),
                      selected = "Great Meadow 1",
                      multiple = TRUE,
                      options = list(
                        `actions-box` = TRUE,
                        `deselect-all-text` = "Clear all",
                        `select-all-text` = "Select all",
                        `none-selected-text` = "Choose site(s)",
                        `live-search` = TRUE,
                        style = "btn-outline-primary"
                      )),
          
          div(style = "margin-bottom: 15px;",
              pickerInput("stats_year", 
                          label = div(icon("calendar"), "Select Years:"),
                          choices = sort(unique(wl_stats$year)),
                          selected = c(2023, 2022, 2021, 2020),
                          multiple = TRUE,
                          options = list(
                            `actions-box` = TRUE,
                            `deselect-all-text` = "Clear all",
                            `select-all-text` = "Select all",
                            `none-selected-text` = "Choose year(s)",
                            `live-search` = TRUE,
                            style = "btn-outline-primary"
                          ))),
          
          div(style = "margin-bottom: 15px;",
              radioButtons("time_summary", "Summarize Stats:",
                           choices = c("Each Year" = "year", "Average Across Years" = "multi"),
                           selected = "year",
                           inline = TRUE)),
          
          br(),
          div(style = "padding: 15px; background-color: #e8f4f8; border-radius: 8px; border-left: 4px solid #3498db;",
              p(icon("info-circle"), " Growing Season statistics calculated from May through October.", 
                style = "margin: 0; font-size: 0.9rem; color: #2c3e50;")),
          
          div(style = "margin-top: 15px; text-align: center;",
              downloadButton("download_stats", "Download Table", 
                             class = "btn-primary btn-sm", 
                             icon = icon("download")))
        ),
        
        card(
          full_screen = TRUE,
          card_header(
            class = "bg-primary text-white",
            "Growing Season Water Level Statistics"
          ),
          
          div(style = "padding: 10px;",
              uiOutput("significance_info"),
              dataTableOutput("wl_stats"))
        )
      )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive data for plotting
  plot_data <- reactive({
    req(input$year, input$selected_sites)
    
    all_data %>%
      filter(year %in% input$year, doy > 134, doy < 275) %>%
      filter(site %in% input$selected_sites)
  })
  
  output$hydrograph <- renderPlot({
    plot_data_filtered <- plot_data()
    req(nrow(plot_data_filtered) > 0)
    
    # Calculate water level range across all data for consistent precipitation baseline
    minWL <- min(plot_data_filtered$water_depth, na.rm = TRUE)
    maxWL <- max(plot_data_filtered$water_depth, na.rm = TRUE)
    water_range_size <- maxWL - minWL
    
    # Use a smaller portion of the range for precipitation
    precip_height <- water_range_size * 0.15  # 15% of water range
    precip_scale <- precip_height / max(plot_data_filtered$lag_precip, na.rm = TRUE)
    
    # Position precipitation at the bottom
    precip_baseline <- minWL - (water_range_size * 0.05)  # 5% below minimum
    
    # Color palette for sites
    sites <- unique(plot_data_filtered$site)
    site_colors <- c(
      "Great Meadow 1" = "black", "Great Meadow 2" = "chartreuse4", "Great Meadow 3" = "green",
      "Great Meadow 4" = "darkorange", "Great Meadow 5" = "deeppink2", "Great Meadow 6" = "purple",
      "Gilmore Meadow" = "darkgray", "Precipitation" = "blue"
    )
    
    # Create the plot
    ggplot(plot_data_filtered, aes(x = doy_h, y = water_depth)) +
      # Water level lines by site
      geom_line(aes(color = site), size = 0.7) +
      
      # Precipitation line (scaled by multiplier of 5 and offset by minWL)
      geom_line(aes(x = doy_h, y = lag_precip * precip_scale + precip_baseline, 
                    color = "Precipitation"), 
                size = 0.7) +
      
      # Ground level reference
      geom_hline(yintercept = 0, color = 'brown') +
      
      # Facet by year
      facet_wrap(~ year, ncol = 1, scales = "free_y") +
      
      # Colors
      scale_color_manual(values = site_colors, 
                         breaks = c(sites, "Precipitation")) +
      
      # Axes and labels
      labs(y = 'Water Level (cm)', x = 'Date') +
      scale_x_continuous(
        breaks = c(121, 152, 182, 213, 244, 274),
        labels = c('May-01', 'Jun-01', 'Jul-01', 'Aug-01', 'Sep-01', 'Oct-01')
      ) +
      
      # Secondary axis for precipitation
      scale_y_continuous(
        sec.axis = sec_axis(~ (. - precip_baseline) / precip_scale,
                            name = 'Hourly Precip. (cm)',
                            breaks = seq(0, max(plot_data_filtered$lag_precip, na.rm = TRUE), 
                                         by = 0.5),
                            labels = function(x) sprintf("%.1f", x))
      ) +
      
      # Theme
      theme_bw() +
      theme(
        plot.title = element_text(hjust = 0.5),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.text.y.right = element_text(color = 'blue'),
        axis.title.y.right = element_text(color = 'blue'),
        strip.text = element_text(size = 11),
        legend.position = "bottom",
        legend.title = element_blank()
      )
  })
  
  # Reactive for processed brushed data
  processed_brush_data <- reactive({
    req(input$hydro_brush)
    
    plot_data_filtered <- plot_data()
    req(nrow(plot_data_filtered) > 0)
    
    selected_data <- brushedPoints(
      plot_data_filtered,
      brush = input$hydro_brush,
      xvar = "doy_h",
      yvar = "water_depth"
    ) %>%
      select(site, year, doy_h, timestamp, water_depth, lag_precip) %>%
      arrange(year, doy_h) %>%
      mutate(
        doy_h = round(doy_h, 2),
        water_depth = round(water_depth, 2),
        lag_precip = round(lag_precip, 3),
        timestamp = format(timestamp, "%Y-%m-%d %H:%M:%S")
      )
    
    if (nrow(selected_data) == 0) {
      return(data.frame(Message = "No points selected - try brushing over the water level lines"))
    }
    
    # Create the base data with timestamp and precipitation
    base_data <- selected_data %>%
      mutate(doy_h_rounded = round(doy_h, 1)) %>%
      group_by(year, doy_h_rounded) %>%
      summarise(
        timestamp = first(timestamp),
        doy_h_orig = first(doy_h),
        lag_precip = first(lag_precip),
        .groups = 'drop'
      )
    
    # Create water depth data for each site, taking the mean of duplicates
    water_depth_data <- selected_data %>%
      mutate(doy_h_rounded = round(doy_h, 1)) %>%
      group_by(year, doy_h_rounded, site) %>%
      summarise(water_depth = mean(water_depth, na.rm = TRUE), .groups = 'drop') %>%
      pivot_wider(names_from = site, values_from = water_depth, names_prefix = "")
    
    # Join the data together
    result_data <- base_data %>%
      left_join(water_depth_data, by = c("year", "doy_h_rounded")) %>%
      select(-doy_h_rounded) %>%
      rename(
        `Year` = year, 
        `Timestamp` = timestamp,
        `Day of Year` = doy_h_orig,
        `Precipitation (cm)` = lag_precip
      )
    
    # Add " Water Depth (cm)" suffix to site columns
    site_columns <- intersect(names(result_data), input$selected_sites)
    if (length(site_columns) > 0) {
      result_data <- result_data %>%
        rename_with(~ paste(.x, "Water Depth (cm)"), .cols = all_of(site_columns))
    }
    
    result_data %>% arrange(Year, `Day of Year`)
  })
  
  # Reactive for significance testing results - now checks if both wetlands are selected
  significance_results <- reactive({
    req(input$stats_year, input$stats_site)
    
    # Check if both wetlands are represented in selected sites
    selected_wetlands <- wl_stats %>%
      filter(site %in% input$stats_site) %>%
      pull(wetland) %>%
      unique()
    
    # Only calculate significance if both wetlands are represented
    if (length(selected_wetlands) >= 2 && all(c("Great Meadow", "Gilmore Meadow") %in% selected_wetlands)) {
      calculate_wetland_significance(wl_stats, input$stats_year, input$stats_site, alpha = 0.05)
    } else {
      NULL
    }
  })
  
  # Reactive to determine if we should show significance info
  show_significance_info <- reactive({
    req(input$stats_site, input$time_summary)
    
    if (input$time_summary != "multi") return(FALSE)
    
    selected_wetlands <- wl_stats %>%
      filter(site %in% input$stats_site) %>%
      pull(wetland) %>%
      unique()
    
    return(length(selected_wetlands) >= 2 && all(c("Great Meadow", "Gilmore Meadow") %in% selected_wetlands))
  })
  
  
  # setting up average WL stats and calculating stats output tables
  filtered_stats <- reactive({
    req(input$stats_site, input$stats_year, input$time_summary)
    
    data <- wl_stats %>%
      filter(site %in% input$stats_site, year %in% input$stats_year)
    
    if (input$time_summary == "year") {
      # ---- Option 1: Per-Year Summary ----
      data %>%
        mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
        select(
          Year = year,
          Site = site,
          `Mean Water Level (cm)` = WL_mean,
          `SD Water Level (cm)` = WL_sd,
          `Minimum Water Level (cm)` = WL_min,
          `Maximum Water Level (cm)` = WL_max,
          `Maximum Hourly Increase (cm)` = max_inc,
          `Maximum Hourly Decrease (cm)` = max_dec,
          `Growing Season Change (cm)` = GS_change,
          `GS % Surface Water` = prop_over_0cm,
          `GS % Within 30cm` = prop_bet_0_neg30cm,
          `GS % Over 30cm Deep` = prop_under_neg30cm
        )
    } else {
      # ---- Option 2: Average Across Years ----
      # Per-site summary
      data_site <- data %>%
        group_by(site, wetland) %>%
        summarise(
          Year = paste0(min(year), "–", max(year)),
          `Mean Water Level (cm)` = round(mean(WL_mean, na.rm = TRUE), 2),
          `SD Water Level (cm)` = round(mean(WL_sd, na.rm = TRUE), 2),
          `Minimum Water Level (cm)` = round(mean(WL_min, na.rm = TRUE), 2),
          `Maximum Water Level (cm)` = round(mean(WL_max, na.rm = TRUE), 2),
          `Maximum Hourly Increase (cm)` = round(mean(max_inc, na.rm = TRUE), 2),
          `Maximum Hourly Decrease (cm)` = round(mean(max_dec, na.rm = TRUE), 2),
          `Growing Season Change (cm)` = round(mean(GS_change, na.rm = TRUE), 2),
          `GS % Surface Water` = round(mean(prop_over_0cm, na.rm = TRUE), 2),
          `GS % Within 30cm` = round(mean(prop_bet_0_neg30cm, na.rm = TRUE), 2),
          `GS % Over 30cm Deep` = round(mean(prop_under_neg30cm, na.rm = TRUE), 2),
          .groups = "drop"
        )
      
      # Per-wetland "All Sites" summary
      data_wetland <- data %>%
        group_by(wetland) %>%
        summarise(
          site = "All Sites",
          Year = paste0(min(year), "–", max(year)),
          `Mean Water Level (cm)` = round(mean(WL_mean, na.rm = TRUE), 2),
          `SD Water Level (cm)` = round(mean(WL_sd, na.rm = TRUE), 2),
          `Minimum Water Level (cm)` = round(mean(WL_min, na.rm = TRUE), 2),
          `Maximum Water Level (cm)` = round(mean(WL_max, na.rm = TRUE), 2),
          `Maximum Hourly Increase (cm)` = round(mean(max_inc, na.rm = TRUE), 2),
          `Maximum Hourly Decrease (cm)` = round(mean(max_dec, na.rm = TRUE), 2),
          `Growing Season Change (cm)` = round(mean(GS_change, na.rm = TRUE), 2),
          `GS % Surface Water` = round(mean(prop_over_0cm, na.rm = TRUE), 2),
          `GS % Within 30cm` = round(mean(prop_bet_0_neg30cm, na.rm = TRUE), 2),
          `GS % Over 30cm Deep` = round(mean(prop_under_neg30cm, na.rm = TRUE), 2),
          .groups = "drop"
        )
      
      bind_rows(data_site, data_wetland) %>%
        distinct() %>% 
        rename(Site = site,
               Wetland = wetland)
    }
  })
  
  # reactive table output for selected data from hydrograph
  output$brush_info <- renderTable({
    processed_brush_data()
  })
  
  # Output for significance information display
  output$significance_info <- renderUI({
    if (show_significance_info()) {
      sig_results <- significance_results()
      
      if (!is.null(sig_results)) {
        sig_vars <- sig_results %>% 
          filter(significant) %>% 
          pull(variable)
        
        if (length(sig_vars) > 0) {
          # Map variable names to display names
          var_display_names <- c(
            "WL_mean" = "Mean Water Level",
            "WL_sd" = "SD Water Level", 
            "WL_min" = "Minimum Water Level",
            "WL_max" = "Maximum Water Level",
            "max_inc" = "Maximum Hourly Increase",
            "max_dec" = "Maximum Hourly Decrease",
            "GS_change" = "Growing Season Change",
            "prop_over_0cm" = "GS % Surface Water",
            "prop_bet_0_neg30cm" = "GS % Within 30cm",
            "prop_under_neg30cm" = "GS % Over 30cm Deep"
          )
          
          sig_display_names <- var_display_names[sig_vars]
          sig_display_names <- sig_display_names[!is.na(sig_display_names)]
          
          div(class = "significance-info",
              h5(icon("exclamation-triangle"), " Statistical Significance", 
                 style = "color: #856404; margin-bottom: 10px;"),
              p("Highlighted variables show significant differences (p < 0.05) between Great Meadow and Gilmore Meadow wetlands:", 
                style = "margin-bottom: 8px; font-size: 0.9rem;"),
              tags$ul(
                lapply(sig_display_names, function(name) {
                  tags$li(name, style = "font-size: 0.85rem; color: #856404;")
                })
              ),
              p("Statistical tests compare averages across all selected years and sites within each wetland.", 
                style = "margin-top: 5px; margin-bottom: 10px; font-size: 0.8rem; font-style: italic; color: #6c757d;")
          )
        }
      }
    }
  })
  
  # reactive table output for WL stats with significance highlighting
  output$wl_stats <- DT::renderDataTable({
    data <- filtered_stats()
    
    dt <- datatable(
      data,
      options = list(pageLength = 10, scrollX = TRUE)
    ) %>%
      formatStyle(
        'Site',
        target = 'cell',
        fontWeight = styleEqual("All Sites", "bold")
      )
    
    # Add significance highlighting only when appropriate
    if (show_significance_info()) {
      sig_results <- significance_results()
      
      if (!is.null(sig_results)) {
        # Map variable names to column names in the table
        var_mapping <- c(
          "WL_mean" = "Mean Water Level (cm)",
          "WL_sd" = "SD Water Level (cm)", 
          "WL_min" = "Minimum Water Level (cm)",
          "WL_max" = "Maximum Water Level (cm)",
          "max_inc" = "Maximum Hourly Increase (cm)",
          "max_dec" = "Maximum Hourly Decrease (cm)",
          "GS_change" = "Growing Season Change (cm)",
          "prop_over_0cm" = "GS % Surface Water",
          "prop_bet_0_neg30cm" = "GS % Within 30cm",
          "prop_under_neg30cm" = "GS % Over 30cm Deep"
        )
        
        # Apply highlighting for significant variables
        for (var_name in names(var_mapping)) {
          col_name <- var_mapping[var_name]
          sig_row <- sig_results[sig_results$variable == var_name, ]
          
          if (nrow(sig_row) > 0 && sig_row$significant) {
            dt <- dt %>%
              formatStyle(
                col_name,
                valueColumns = "Site",
                backgroundColor = styleEqual("All Sites", "#fff3cd"),
                fontWeight = styleEqual("All Sites", "bold")
              )
          }
        }
      }
    }
    
    dt
  })
  
  # Download handler for brushed data
  output$download_brush <- downloadHandler(
    filename = function() {
      paste("hydrograph_selected_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(processed_brush_data(), file, row.names = FALSE)
    }
  )
  
  # Download handler for water level stats
  output$download_stats <- downloadHandler(
    filename = function() {
      if (input$time_summary == "multi") {
        paste("wetland_averaged_stats_", Sys.Date(), ".csv", sep = "")
      } else {
        paste("site_stats_", Sys.Date(), ".csv", sep = "")
      }
    },
    content = function(file) {
      write.csv(filtered_stats(), file, row.names = FALSE)
    }
  )
  
  # Download handler for hydrograph plot
  output$download_plot <- downloadHandler(
    filename = function() {
      sites_label <- paste(input$selected_sites, collapse = "_")
      years_label <- paste(input$year, collapse = "_")
      paste0("hydrograph_", sites_label, "_", years_label, "_", Sys.Date(), ".png")
    },
    content = function(file) {
      # Recreate the plot
      plot_data_filtered <- plot_data()
      req(nrow(plot_data_filtered) > 0)
      
      # Calculate minimum water level across all data for consistent precipitation baseline
      minWL <- min(plot_data_filtered$water_depth, na.rm = TRUE)
      
      # Create color palette for sites
      sites <- unique(plot_data_filtered$site)
      site_colors <- c(
        "Great Meadow 1" = "black", "Great Meadow 2" = "red", "Great Meadow 3" = "green",
        "Great Meadow 4" = "orange", "Great Meadow 5" = "purple", "Great Meadow 6" = "brown",
        "Gilmore Meadow" = "darkgray", "Precipitation" = "blue"
      )
      
      # Create the plot
      p <- ggplot(plot_data_filtered, aes(x = doy_h, y = water_depth)) +
        # Water level lines by site
        geom_line(aes(color = site), size = 0.7) +
        
        # Precipitation line (scaled by multiplier of 5 and offset by minWL)
        geom_line(aes(x = doy_h, y = lag_precip * 5 + minWL, color = "Precipitation"), 
                  size = 0.7) +
        
        # Ground level reference
        geom_hline(yintercept = 0, color = 'brown') +
        
        # Facet by year
        facet_wrap(~ year, ncol = 1, scales = "free_y") +
        
        # Colors
        scale_color_manual(values = site_colors, 
                           breaks = c(sites, "Precipitation")) +
        
        # Axes and labels
        labs(y = 'Water Level (cm)', x = 'Date') +
        scale_x_continuous(
          breaks = c(121, 152, 182, 213, 244, 274),
          labels = c('May-01', 'Jun-01', 'Jul-01', 'Aug-01', 'Sep-01', 'Oct-01')
        ) +
        
        # Secondary axis for precipitation
        scale_y_continuous(
          sec.axis = sec_axis(~ .,
                              breaks = c(minWL, minWL + 10),
                              name = 'Hourly Precip. (cm)',
                              labels = c('0', '2'))
        ) +
        
        # Theme
        theme_bw() +
        theme(
          plot.title = element_text(hjust = 0.5),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          axis.text.y.right = element_text(color = 'blue'),
          axis.title.y.right = element_text(color = 'blue'),
          strip.text = element_text(size = 11),
          legend.position = "bottom",
          legend.title = element_blank()
        )
      
      # Save the plot
      ggsave(file, plot = p, width = 12, height = 8, dpi = 300, bg = "white")
    }
  )
  
}

# Run app
shinyApp(ui, server)














####----------------------------------------------------------------------------



library(shiny)
library(tidyverse)
library(lubridate)
library(patchwork)
library(shinyWidgets)
library(DT)
library(bslib)

# Read & prepare processed data
gm <- read.csv("data/processed_data/great_meadow_well_data_2024_20250715.csv") %>%
  rename(Date = date, Year = year, precip_cm = precip.cm) %>%
  mutate(Date = as.Date(Date),
         timestamp = as_datetime(timestamp),
         site = paste("Great Meadow", plot.num),
         water.depth = case_when(
           Year == 2016 & doy_h == 159.12 & plot.num == 3 & water.depth < -120 ~ NA_real_,
           Year == 2017 & doy_h == 215.02 & plot.num == 6 & water.depth < -115 ~ NA_real_,
           Year == 2021 & plot.num == 3 & doy == 224 & water.depth > 400 ~ NA_real_,
           Year == 2021 & plot.num == 3 & doy == 225 & water.depth > 400 ~ NA_real_,
           TRUE ~ water.depth
         )) %>%
  mutate(siteyear = paste(site, Year, sep = "_"))

gl <- read.csv("data/raw_data/hydrology_data/gilmore_well_prec_data_2013-2024.csv") %>%
  rename(water.depth = GILM_WL) %>%
  mutate(
    site = "Gilmore Meadow",
    
    # Try to parse full datetime, fallback to date only + midnight
    timestamp_parsed = mdy_hm(timestamp),
    timestamp_parsed = if_else(
      is.na(timestamp_parsed),
      mdy(timestamp),
      timestamp_parsed
    ),
    timestamp_parsed = as.POSIXct(timestamp_parsed),
    timestamp = timestamp_parsed,
    
    # Standardize Date column too
    Date = mdy(Date)
  ) %>%
  select(timestamp, 
         Date,
         doy,
         Year,
         precip_cm,
         water.depth,
         lag.precip,
         hr,
         doy_h,
         site)

all_data <- bind_rows(gm, gl) %>% 
  filter(Year >= 2016 & Year <= 2023) %>% 
  select(timestamp, 
         date = Date, 
         year = Year, 
         doy, 
         hr, 
         doy_h,
         precip_cm,
         lag_precip = lag.precip,
         water_depth = water.depth,
         site)

wl_stats <- read.csv("data/processed_data/gm_gl_wl_stats.csv") %>% 
  select(year,
         stat, 
         `Gilmore Meadow` = gilmore.meadow, 
         `Great Meadow 1` = great.meadow.1, 
         `Great Meadow 2` = great.meadow.2, 
         `Great Meadow 3` = great.meadow.3, 
         `Great Meadow 4` = great.meadow.4, 
         `Great Meadow 5` = great.meadow.5, 
         `Great Meadow 6` = great.meadow.6) %>% 
  pivot_longer(cols = -c(year, stat), names_to = "site", values_to = "value") %>% 
  pivot_wider(names_from = stat, values_from = value) %>%
  arrange(site, year) %>% 
  mutate(
    wetland = case_when(
      grepl("Great Meadow", site, ignore.case = TRUE) ~ "Great Meadow",
      grepl("Gilmore Meadow", site, ignore.case = TRUE) ~ "Gilmore Meadow",
      TRUE ~ "Unknown"
    )
  )


# Function to calculate significance tests between wetlands
calculate_wetland_significance <- function(data, selected_years, selected_sites, alpha = 0.05) {
  # Filter data for selected years and sites
  filtered_data <- data %>%
    filter(year %in% selected_years, site %in% selected_sites) %>%
    mutate(site_group = ifelse(grepl("Great Meadow", site), "Great Meadow", site))
  
  # Check if we have both wetlands represented in the selected sites
  wetlands_present <- unique(filtered_data$site_group)
  
  # Only run tests if both wetlands are present
  if (length(wetlands_present) < 2 || !all(c("Great Meadow", "Gilmore Meadow") %in% wetlands_present)) {
    return(NULL)  # Return NULL if we can't compare between wetlands
  }
  
  # Get all numeric stat columns
  stat_cols <- filtered_data %>% select(where(is.numeric), -year) %>% names()
  
  # Run t-tests for each variable and store results
  t_test_results <- map_dfr(stat_cols, function(var) {
    formula <- as.formula(paste(var, "~ site_group"))
    tryCatch({
      test <- t.test(formula, data = filtered_data)
      data.frame(
        variable = var,
        p_value = test$p.value,
        significant = test$p.value < alpha,
        stringsAsFactors = FALSE
      )
    }, error = function(e) {
      data.frame(
        variable = var,
        p_value = NA,
        significant = FALSE,
        stringsAsFactors = FALSE
      )
    })
  })
  
  return(t_test_results)
}


# UI 
ui <- page_fluid(
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#1B365D",
    secondary = "#4C6D9A",    
    success = "#2E86C1",
    info = "#3498db",
    warning = "#f39c12",
    danger = "#e74c3c",
    base_font = font_google("Open Sans"),
    heading_font = font_google("Open Sans", wght = c(400, 700))
  ),
  
  # Custom CSS for additional styling
  tags$head(
    tags$style(HTML("
      .content-section {
        margin: 30px 0;
        padding: 25px;
        border-radius: 15px;
        box-shadow: 0 4px 12px rgba(0,0,0,0.1);
        background: linear-gradient(135deg, #ffffff 0%, #f8f9fa 100%);
        border: 2px solid #1B365D;
      }
      
      .section-title {
        color: #1B365D;
        font-weight: 600;
        margin-bottom: 20px;
        padding-bottom: 10px;
        border-bottom: 2px solid #4C6D9A;
      }
      
      .sidebar-custom {
        background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%);
        border-radius: 10px;
        padding: 20px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.08);
        border: 1px solid #dee2e6;
      }
      
      .main-title {
        background: linear-gradient(135deg, #1B365D 0%, #4C6D9A 100%);
        color: white;
        padding: 30px;
        margin: -15px -15px 30px -15px;
        text-align: center;
        border-radius: 0 0 20px 20px;
        box-shadow: 0 4px 12px rgba(0,0,0,0.15);
      }
      
      .brush-info-section {
        background: linear-gradient(135deg, #e8f4f8 0%, #f0f8ff 100%);
        border-radius: 12px;
        padding: 20px;
        margin: 20px 0;
        border: 1px solid #4C6D9A;
      }
      
      .stats-main {
        background: linear-gradient(135deg, #f8f9fa 0%, #ffffff 100%);
      }
      
      .dataTables_wrapper {
        font-size: 0.85rem !important;
      }

      .dataTables_wrapper table {
        font-size: 0.8rem !important;
      }

      .dataTables_wrapper .dataTables_info,
      .dataTables_wrapper .dataTables_paginate {
        font-size: 0.75rem !important;
      }
      
    "))
  ),
  
  # Main title with gradient background
  div(class = "main-title",
      h1("Wetland Hydrograph Visualizer", 
         style = "margin: 0; font-size: 2rem; text-shadow: 2px 2px 4px rgba(0,0,0,0.3)")
  ),
  
  # First section: Hydrographs 
  div(class = "content-section",
      layout_sidebar(
        sidebar = sidebar(
          class = "sidebar-custom",
          width = 300,
          h4("Hydrograph Controls", style = "color: #1B365D; margin-bottom: 20px;"),
          
          pickerInput("selected_sites", 
                      label = div(icon("map-marker"), "Select Site(s):"),
                      choices = sort(unique(all_data$site)),
                      selected = c("Great Meadow 1", "Gilmore Meadow"),
                      multiple = TRUE,
                      options = list(
                        `actions-box` = TRUE,
                        `deselect-all-text` = "Clear all",
                        `select-all-text` = "Select all",
                        `none-selected-text` = "Choose site(s)",
                        `live-search` = TRUE,
                        style = "btn-outline-primary"
                      )),
          
          div(style = "margin-bottom: 15px;",
              pickerInput("year", 
                          label = div(icon("calendar"), "Select Year(s):"),
                          choices = sort(unique(all_data$year)),
                          selected = 2023,
                          multiple = TRUE,
                          options = list(
                            `actions-box` = TRUE,
                            `deselect-all-text` = "Clear all",
                            `select-all-text` = "Select all",
                            `none-selected-text` = "Choose year(s)",
                            `live-search` = TRUE,
                            style = "btn-outline-primary"
                          ))),
          
          br(),
          div(style = "padding: 15px; background-color: #e8f4f8; border-radius: 8px; border-left: 4px solid #3498db;",
              p(icon("info-circle"), " Use the brush tool to select data points on the hydrograph for a detailed view of the data.", 
                style = "margin: 0; font-size: 0.9rem; color: #2c3e50;")),
          
          div(style = "margin-top: 15px; text-align: center;",
              downloadButton("download_brush", "Download Selected Data", 
                             class = "btn-primary btn-sm", 
                             icon = icon("download")))
        ),
        
        card(
          full_screen = TRUE,
          card_header(
            class = "bg-primary text-white",
            "Hydrographs by Year"
          ),
          plotOutput("hydrograph", height = "600px", 
                     brush = brushOpts(id = "hydro_brush", fill = "#4C6D9A", opacity = 0.3))
        )
      )
  ),
  
  # Selected data points section with improved styling
  div(class = "brush-info-section",
      div(class = "row",
          div(class = "col-12",
              card(
                card_header(
                  class = "bg-success text-white",
                  "Selected Data from Hydrograph:"
                ),
                tableOutput("brush_info")
              )
          )
      )
  ),
  
  # Second section: Water Level Stats with improved styling
  div(class = "content-section stats-main",
      layout_sidebar(
        sidebar = sidebar(
          class = "sidebar-custom",
          width = 300,
          h4("Statistics Controls", style = "color: #1B365D; margin-bottom: 20px;"),
          
          pickerInput("stats_site", 
                      label = div(icon("map-marker"), "Select Site(s):"),
                      choices = sort(unique(wl_stats$site)),
                      selected = "Great Meadow 1",
                      multiple = TRUE,
                      options = list(
                        `actions-box` = TRUE,
                        `deselect-all-text` = "Clear all",
                        `select-all-text` = "Select all",
                        `none-selected-text` = "Choose site(s)",
                        `live-search` = TRUE,
                        style = "btn-outline-primary"
                      )),
          
          div(style = "margin-bottom: 15px;",
              pickerInput("stats_year", 
                          label = div(icon("calendar"), "Select Years:"),
                          choices = sort(unique(wl_stats$year)),
                          selected = c(2023, 2022, 2021, 2020),
                          multiple = TRUE,
                          options = list(
                            `actions-box` = TRUE,
                            `deselect-all-text` = "Clear all",
                            `select-all-text` = "Select all",
                            `none-selected-text` = "Choose year(s)",
                            `live-search` = TRUE,
                            style = "btn-outline-primary"
                          ))),
          
          div(style = "margin-bottom: 15px;",
              radioButtons("time_summary", "Summarize Stats:",
                           choices = c("Each Year" = "year", "Average Across Years" = "multi"),
                           selected = "year",
                           inline = TRUE)),
          
          br(),
          div(style = "padding: 15px; background-color: #e8f4f8; border-radius: 8px; border-left: 4px solid #3498db;",
              p(icon("info-circle"), " Growing Season statistics calculated from May through October.", 
                style = "margin: 0; font-size: 0.9rem; color: #2c3e50;")),
          
          div(style = "margin-top: 15px; text-align: center;",
              downloadButton("download_stats", "Download Table", 
                             class = "btn-primary btn-sm", 
                             icon = icon("download")))
        ),
        
        card(
          full_screen = TRUE,
          card_header(
            class = "bg-primary text-white",
            "Growing Season Water Level Statistics"
          ),
          
          div(style = "padding: 10px;",
              uiOutput("significance_info"),
              dataTableOutput("wl_stats"))
        )
      )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive data for plotting
  plot_data <- reactive({
    req(input$year, input$selected_sites)
    
    all_data %>%
      filter(year %in% input$year, doy > 134, doy < 275) %>%
      filter(site %in% input$selected_sites)
  })
  
  output$hydrograph <- renderPlot({
    plot_data_filtered <- plot_data()
    req(nrow(plot_data_filtered) > 0)
    
    # Calculate dynamic scaling for precipitation
    precip_range <- range(plot_data_filtered$lag_precip, na.rm = TRUE)
    water_range <- range(plot_data_filtered$water_depth, na.rm = TRUE)
    
    # Scale precipitation to use about 20% of the water level range
    precip_scale_factor <- diff(water_range) * 0.2 / diff(precip_range)
    precip_offset <- min(water_range, na.rm = TRUE)
    
    # Color palette for sites
    sites <- unique(plot_data_filtered$site)
    site_colors <- c(
      "Great Meadow 1" = "black", "Great Meadow 2" = "chartreuse4", "Great Meadow 3" = "green",
      "Great Meadow 4" = "darkorange", "Great Meadow 5" = "deeppink2", "Great Meadow 6" = "purple",
      "Gilmore Meadow" = "darkgray", "Precipitation" = "blue"
    )
    
    # Create the plot
    ggplot(plot_data_filtered, aes(x = doy_h, y = water_depth)) +
      # Water level lines by site
      geom_line(aes(color = site), size = 0.7) +
      
      # Precipitation line (dynamically scaled)
      geom_line(aes(x = doy_h, y = lag_precip * precip_scale_factor + precip_offset, 
                    color = "Precipitation"), 
                size = 0.7) +
      
      # Ground level reference
      geom_hline(yintercept = 0, color = 'brown') +
      
      # Facet by year
      facet_wrap(~ year, ncol = 1, scales = "free_y") +
      
      # Colors
      scale_color_manual(values = site_colors, 
                         breaks = c(sites, "Precipitation")) +
      
      # Axes and labels
      labs(y = 'Water Level (cm)', x = 'Date') +
      scale_x_continuous(
        breaks = c(121, 152, 182, 213, 244, 274),
        labels = c('May-01', 'Jun-01', 'Jul-01', 'Aug-01', 'Sep-01', 'Oct-01')
      ) +
      
      # Dynamic secondary axis for precipitation
      scale_y_continuous(
        sec.axis = sec_axis(~ (. - precip_offset) / precip_scale_factor,
                            name = 'Hourly Precip. (cm)',
                            labels = function(x) sprintf("%.1f", x))
      ) +
      
      # Theme
      theme_bw() +
      theme(
        plot.title = element_text(hjust = 0.5),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.text.y.right = element_text(color = 'blue'),
        axis.title.y.right = element_text(color = 'blue'),
        strip.text = element_text(size = 11),
        legend.position = "bottom",
        legend.title = element_blank()
      )
  })
  
  # Reactive for processed brushed data
  processed_brush_data <- reactive({
    req(input$hydro_brush)
    
    plot_data_filtered <- plot_data()
    req(nrow(plot_data_filtered) > 0)
    
    selected_data <- brushedPoints(
      plot_data_filtered,
      brush = input$hydro_brush,
      xvar = "doy_h",
      yvar = "water_depth"
    ) %>%
      select(site, year, doy_h, timestamp, water_depth, lag_precip) %>%
      arrange(year, doy_h) %>%
      mutate(
        doy_h = round(doy_h, 2),
        water_depth = round(water_depth, 2),
        lag_precip = round(lag_precip, 3),
        timestamp = format(timestamp, "%Y-%m-%d %H:%M:%S")
      )
    
    if (nrow(selected_data) == 0) {
      return(data.frame(Message = "No points selected - try brushing over the water level lines"))
    }
    
    # Create the base data with timestamp and precipitation
    base_data <- selected_data %>%
      mutate(doy_h_rounded = round(doy_h, 1)) %>%
      group_by(year, doy_h_rounded) %>%
      summarise(
        timestamp = first(timestamp),
        doy_h_orig = first(doy_h),
        lag_precip = first(lag_precip),
        .groups = 'drop'
      )
    
    # Create water depth data for each site, taking the mean of duplicates
    water_depth_data <- selected_data %>%
      mutate(doy_h_rounded = round(doy_h, 1)) %>%
      group_by(year, doy_h_rounded, site) %>%
      summarise(water_depth = mean(water_depth, na.rm = TRUE), .groups = 'drop') %>%
      pivot_wider(names_from = site, values_from = water_depth, names_prefix = "")
    
    # Join the data together
    result_data <- base_data %>%
      left_join(water_depth_data, by = c("year", "doy_h_rounded")) %>%
      select(-doy_h_rounded) %>%
      rename(
        `Year` = year, 
        `Timestamp` = timestamp,
        `Day of Year` = doy_h_orig,
        `Precipitation (cm)` = lag_precip
      )
    
    # Add " Water Depth (cm)" suffix to site columns
    site_columns <- intersect(names(result_data), input$selected_sites)
    if (length(site_columns) > 0) {
      result_data <- result_data %>%
        rename_with(~ paste(.x, "Water Depth (cm)"), .cols = all_of(site_columns))
    }
    
    result_data %>% arrange(Year, `Day of Year`)
  })
  
  # Reactive for significance testing results - now checks if both wetlands are selected
  significance_results <- reactive({
    req(input$stats_year, input$stats_site)
    
    # Check if both wetlands are represented in selected sites
    selected_wetlands <- wl_stats %>%
      filter(site %in% input$stats_site) %>%
      pull(wetland) %>%
      unique()
    
    # Only calculate significance if both wetlands are represented
    if (length(selected_wetlands) >= 2 && all(c("Great Meadow", "Gilmore Meadow") %in% selected_wetlands)) {
      calculate_wetland_significance(wl_stats, input$stats_year, input$stats_site, alpha = 0.05)
    } else {
      NULL
    }
  })
  
  # Reactive to determine if we should show significance info
  show_significance_info <- reactive({
    req(input$stats_site, input$time_summary)
    
    if (input$time_summary != "multi") return(FALSE)
    
    selected_wetlands <- wl_stats %>%
      filter(site %in% input$stats_site) %>%
      pull(wetland) %>%
      unique()
    
    return(length(selected_wetlands) >= 2 && all(c("Great Meadow", "Gilmore Meadow") %in% selected_wetlands))
  })
  
  
  # setting up average WL stats and calculating stats output tables
  filtered_stats <- reactive({
    req(input$stats_site, input$stats_year, input$time_summary)
    
    data <- wl_stats %>%
      filter(site %in% input$stats_site, year %in% input$stats_year)
    
    if (input$time_summary == "year") {
      # ---- Option 1: Per-Year Summary ----
      data %>%
        mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
        select(
          Year = year,
          Site = site,
          `Mean Water Level (cm)` = WL_mean,
          `SD Water Level (cm)` = WL_sd,
          `Minimum Water Level (cm)` = WL_min,
          `Maximum Water Level (cm)` = WL_max,
          `Maximum Hourly Increase (cm)` = max_inc,
          `Maximum Hourly Decrease (cm)` = max_dec,
          `Growing Season Change (cm)` = GS_change,
          `GS % Surface Water` = prop_over_0cm,
          `GS % Within 30cm` = prop_bet_0_neg30cm,
          `GS % Over 30cm Deep` = prop_under_neg30cm
        )
    } else {
      # ---- Option 2: Average Across Years ----
      # Per-site summary
      data_site <- data %>%
        group_by(site, wetland) %>%
        summarise(
          Year = paste0(min(year), "–", max(year)),
          `Mean Water Level (cm)` = round(mean(WL_mean, na.rm = TRUE), 2),
          `SD Water Level (cm)` = round(mean(WL_sd, na.rm = TRUE), 2),
          `Minimum Water Level (cm)` = round(mean(WL_min, na.rm = TRUE), 2),
          `Maximum Water Level (cm)` = round(mean(WL_max, na.rm = TRUE), 2),
          `Maximum Hourly Increase (cm)` = round(mean(max_inc, na.rm = TRUE), 2),
          `Maximum Hourly Decrease (cm)` = round(mean(max_dec, na.rm = TRUE), 2),
          `Growing Season Change (cm)` = round(mean(GS_change, na.rm = TRUE), 2),
          `GS % Surface Water` = round(mean(prop_over_0cm, na.rm = TRUE), 2),
          `GS % Within 30cm` = round(mean(prop_bet_0_neg30cm, na.rm = TRUE), 2),
          `GS % Over 30cm Deep` = round(mean(prop_under_neg30cm, na.rm = TRUE), 2),
          .groups = "drop"
        )
      
      # Per-wetland "All Sites" summary
      data_wetland <- data %>%
        group_by(wetland) %>%
        summarise(
          site = "All Sites",
          Year = paste0(min(year), "–", max(year)),
          `Mean Water Level (cm)` = round(mean(WL_mean, na.rm = TRUE), 2),
          `SD Water Level (cm)` = round(mean(WL_sd, na.rm = TRUE), 2),
          `Minimum Water Level (cm)` = round(mean(WL_min, na.rm = TRUE), 2),
          `Maximum Water Level (cm)` = round(mean(WL_max, na.rm = TRUE), 2),
          `Maximum Hourly Increase (cm)` = round(mean(max_inc, na.rm = TRUE), 2),
          `Maximum Hourly Decrease (cm)` = round(mean(max_dec, na.rm = TRUE), 2),
          `Growing Season Change (cm)` = round(mean(GS_change, na.rm = TRUE), 2),
          `GS % Surface Water` = round(mean(prop_over_0cm, na.rm = TRUE), 2),
          `GS % Within 30cm` = round(mean(prop_bet_0_neg30cm, na.rm = TRUE), 2),
          `GS % Over 30cm Deep` = round(mean(prop_under_neg30cm, na.rm = TRUE), 2),
          .groups = "drop"
        )
      
      bind_rows(data_site, data_wetland) %>%
        distinct() %>% 
        rename(Site = site,
               Wetland = wetland)
    }
  })
  
  # reactive table output for selected data from hydrograph
  output$brush_info <- renderTable({
    processed_brush_data()
  })
  
  # Output for significance information display
  output$significance_info <- renderUI({
    if (show_significance_info()) {
      sig_results <- significance_results()
      
      if (!is.null(sig_results)) {
        sig_vars <- sig_results %>% 
          filter(significant) %>% 
          pull(variable)
        
        if (length(sig_vars) > 0) {
          # Map variable names to display names
          var_display_names <- c(
            "WL_mean" = "Mean Water Level",
            "WL_sd" = "SD Water Level", 
            "WL_min" = "Minimum Water Level",
            "WL_max" = "Maximum Water Level",
            "max_inc" = "Maximum Hourly Increase",
            "max_dec" = "Maximum Hourly Decrease",
            "GS_change" = "Growing Season Change",
            "prop_over_0cm" = "GS % Surface Water",
            "prop_bet_0_neg30cm" = "GS % Within 30cm",
            "prop_under_neg30cm" = "GS % Over 30cm Deep"
          )
          
          sig_display_names <- var_display_names[sig_vars]
          sig_display_names <- sig_display_names[!is.na(sig_display_names)]
          
          div(class = "significance-info",
              h5(icon("exclamation-triangle"), " Statistical Significance", 
                 style = "color: #856404; margin-bottom: 10px;"),
              p("Highlighted variables show significant differences (p < 0.05) between Great Meadow and Gilmore Meadow wetlands:", 
                style = "margin-bottom: 8px; font-size: 0.9rem;"),
              tags$ul(
                lapply(sig_display_names, function(name) {
                  tags$li(name, style = "font-size: 0.85rem; color: #856404;")
                })
              ),
              p("Statistical tests compare averages across all selected years and sites within each wetland.", 
                style = "margin-top: 5px; margin-bottom: 10px; font-size: 0.8rem; font-style: italic; color: #6c757d;")
          )
        }
      }
    }
  })
  
  # reactive table output for WL stats with significance highlighting
  output$wl_stats <- DT::renderDataTable({
    data <- filtered_stats()
    
    dt <- datatable(
      data,
      options = list(pageLength = 10, scrollX = TRUE)
    ) %>%
      formatStyle(
        'Site',
        target = 'cell',
        fontWeight = styleEqual("All Sites", "bold")
      )
    
    # Add significance highlighting only when appropriate
    if (show_significance_info()) {
      sig_results <- significance_results()
      
      if (!is.null(sig_results)) {
        # Map variable names to column names in the table
        var_mapping <- c(
          "WL_mean" = "Mean Water Level (cm)",
          "WL_sd" = "SD Water Level (cm)", 
          "WL_min" = "Minimum Water Level (cm)",
          "WL_max" = "Maximum Water Level (cm)",
          "max_inc" = "Maximum Hourly Increase (cm)",
          "max_dec" = "Maximum Hourly Decrease (cm)",
          "GS_change" = "Growing Season Change (cm)",
          "prop_over_0cm" = "GS % Surface Water",
          "prop_bet_0_neg30cm" = "GS % Within 30cm",
          "prop_under_neg30cm" = "GS % Over 30cm Deep"
        )
        
        # Apply highlighting for significant variables
        for (var_name in names(var_mapping)) {
          col_name <- var_mapping[var_name]
          sig_row <- sig_results[sig_results$variable == var_name, ]
          
          if (nrow(sig_row) > 0 && sig_row$significant) {
            dt <- dt %>%
              formatStyle(
                col_name,
                valueColumns = "Site",
                backgroundColor = styleEqual("All Sites", "#fff3cd"),
                fontWeight = styleEqual("All Sites", "bold")
              )
          }
        }
      }
    }
    
    dt
  })
  
  # Download handler for brushed data
  output$download_brush <- downloadHandler(
    filename = function() {
      paste("hydrograph_selected_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(processed_brush_data(), file, row.names = FALSE)
    }
  )
  
  # Download handler for water level stats
  output$download_stats <- downloadHandler(
    filename = function() {
      if (input$time_summary == "multi") {
        paste("wetland_averaged_stats_", Sys.Date(), ".csv", sep = "")
      } else {
        paste("site_stats_", Sys.Date(), ".csv", sep = "")
      }
    },
    content = function(file) {
      write.csv(filtered_stats(), file, row.names = FALSE)
    }
  )
  
}

# Run app
shinyApp(ui, server)








####----------------------------------------------------------------------------






#9/8/25 APP prior to multiple select sites for hydrographs

library(shiny)
library(tidyverse)
library(lubridate)
library(patchwork)
library(shinyWidgets)
library(DT)
library(bslib)

# Read & prepare processed data
gm <- read.csv("data/processed_data/great_meadow_well_data_2024_20250715.csv") %>%
  rename(Date = date, Year = year, precip_cm = precip.cm) %>%
  mutate(Date = as.Date(Date),
         timestamp = as_datetime(timestamp),
         site = paste("Great Meadow", plot.num),
         water.depth = case_when(
           Year == 2016 & doy_h == 159.12 & plot.num == 3 & water.depth < -120 ~ NA_real_,
           Year == 2017 & doy_h == 215.02 & plot.num == 6 & water.depth < -115 ~ NA_real_,
           Year == 2021 & plot.num == 3 & doy == 224 & water.depth > 400 ~ NA_real_,
           Year == 2021 & plot.num == 3 & doy == 225 & water.depth > 400 ~ NA_real_,
           TRUE ~ water.depth
         )) %>%
  mutate(siteyear = paste(site, Year, sep = "_"))

gl <- read.csv("data/raw_data/hydrology_data/gilmore_well_prec_data_2013-2024.csv") %>%
  rename(water.depth = GILM_WL) %>%
  mutate(
    site = "Gilmore Meadow",
    
    # Try to parse full datetime, fallback to date only + midnight
    timestamp_parsed = mdy_hm(timestamp),
    timestamp_parsed = if_else(
      is.na(timestamp_parsed),
      mdy(timestamp),
      timestamp_parsed
    ),
    timestamp_parsed = as.POSIXct(timestamp_parsed),
    timestamp = timestamp_parsed,
    
    # Standardize Date column too
    Date = mdy(Date)
  ) %>%
  select(timestamp, 
         Date,
         doy,
         Year,
         precip_cm,
         water.depth,
         lag.precip,
         hr,
         doy_h,
         site)

all_data <- bind_rows(gm, gl) %>% 
  filter(Year >= 2016 & Year <= 2023) %>% 
  select(timestamp, 
         date = Date, 
         year = Year, 
         doy, 
         hr, 
         doy_h,
         precip_cm,
         lag_precip = lag.precip,
         water_depth = water.depth,
         site)

wl_stats <- read.csv("data/processed_data/gm_gl_wl_stats.csv") %>% 
  select(year,
         stat, 
         `Gilmore Meadow` = gilmore.meadow, 
         `Great Meadow 1` = great.meadow.1, 
         `Great Meadow 2` = great.meadow.2, 
         `Great Meadow 3` = great.meadow.3, 
         `Great Meadow 4` = great.meadow.4, 
         `Great Meadow 5` = great.meadow.5, 
         `Great Meadow 6` = great.meadow.6) %>% 
  pivot_longer(cols = -c(year, stat), names_to = "site", values_to = "value") %>% 
  pivot_wider(names_from = stat, values_from = value) %>%
  arrange(site, year) %>% 
  mutate(
    wetland = case_when(
      grepl("Great Meadow", site, ignore.case = TRUE) ~ "Great Meadow",
      grepl("Gilmore Meadow", site, ignore.case = TRUE) ~ "Gilmore Meadow",
      TRUE ~ "Unknown"
    )
  )


# Function to calculate significance tests between wetlands
calculate_wetland_significance <- function(data, selected_years, selected_sites, alpha = 0.05) {
  # Filter data for selected years and sites
  filtered_data <- data %>%
    filter(year %in% selected_years, site %in% selected_sites) %>%
    mutate(site_group = ifelse(grepl("Great Meadow", site), "Great Meadow", site))
  
  # Check if we have both wetlands represented in the selected sites
  wetlands_present <- unique(filtered_data$site_group)
  
  # Only run tests if both wetlands are present
  if (length(wetlands_present) < 2 || !all(c("Great Meadow", "Gilmore Meadow") %in% wetlands_present)) {
    return(NULL)  # Return NULL if we can't compare between wetlands
  }
  
  # Get all numeric stat columns
  stat_cols <- filtered_data %>% select(where(is.numeric), -year) %>% names()
  
  # Run t-tests for each variable and store results
  t_test_results <- map_dfr(stat_cols, function(var) {
    formula <- as.formula(paste(var, "~ site_group"))
    tryCatch({
      test <- t.test(formula, data = filtered_data)
      data.frame(
        variable = var,
        p_value = test$p.value,
        significant = test$p.value < alpha,
        stringsAsFactors = FALSE
      )
    }, error = function(e) {
      data.frame(
        variable = var,
        p_value = NA,
        significant = FALSE,
        stringsAsFactors = FALSE
      )
    })
  })
  
  return(t_test_results)
}


# UI 
ui <- page_fluid(
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#1B365D",
    secondary = "#4C6D9A",    
    success = "#2E86C1",
    info = "#3498db",
    warning = "#f39c12",
    danger = "#e74c3c",
    base_font = font_google("Open Sans"),
    heading_font = font_google("Open Sans", wght = c(400, 700))
  ),
  
  # Custom CSS for additional styling
  tags$head(
    tags$style(HTML("
      .content-section {
        margin: 30px 0;
        padding: 25px;
        border-radius: 15px;
        box-shadow: 0 4px 12px rgba(0,0,0,0.1);
        background: linear-gradient(135deg, #ffffff 0%, #f8f9fa 100%);
        border: 2px solid #1B365D;
      }
      
      .section-title {
        color: #1B365D;
        font-weight: 600;
        margin-bottom: 20px;
        padding-bottom: 10px;
        border-bottom: 2px solid #4C6D9A;
      }
      
      .sidebar-custom {
        background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%);
        border-radius: 10px;
        padding: 20px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.08);
        border: 1px solid #dee2e6;
      }
      
      .main-title {
        background: linear-gradient(135deg, #1B365D 0%, #4C6D9A 100%);
        color: white;
        padding: 30px;
        margin: -15px -15px 30px -15px;
        text-align: center;
        border-radius: 0 0 20px 20px;
        box-shadow: 0 4px 12px rgba(0,0,0,0.15);
      }
      
      .brush-info-section {
        background: linear-gradient(135deg, #e8f4f8 0%, #f0f8ff 100%);
        border-radius: 12px;
        padding: 20px;
        margin: 20px 0;
        border: 1px solid #4C6D9A;
      }
      
      .stats-main {
        background: linear-gradient(135deg, #f8f9fa 0%, #ffffff 100%);
      }
      
      .dataTables_wrapper {
        font-size: 0.85rem !important;
      }

      .dataTables_wrapper table {
        font-size: 0.8rem !important;
      }

      .dataTables_wrapper .dataTables_info,
      .dataTables_wrapper .dataTables_paginate {
        font-size: 0.75rem !important;
      }
      
    "))
  ),
  
  # Main title with gradient background
  div(class = "main-title",
      h1("Wetland Hydrograph Visualizer", 
         style = "margin: 0; font-size: 2rem; text-shadow: 2px 2px 4px rgba(0,0,0,0.3)")
  ),
  
  # First section: Hydrographs 
  div(class = "content-section",
      layout_sidebar(
        sidebar = sidebar(
          class = "sidebar-custom",
          width = 300,
          h4("Hydrograph Controls", style = "color: #1B365D; margin-bottom: 20px;"),
          
          selectInput("gm_site", 
                      label = div(icon("map-marker"), "Select a Great Meadow Site:"),
                      choices = sort(unique(all_data$site[grepl("Great Meadow", all_data$site)])),
                      selected = "Great Meadow 1"),
          
          div(style = "margin-bottom: 15px;",
              pickerInput("year", 
                          label = div(icon("calendar"), "Select Year(s):"),
                          choices = sort(unique(all_data$year)),
                          selected = 2023,
                          multiple = TRUE,
                          options = list(
                            `actions-box` = TRUE,
                            `deselect-all-text` = "Clear all",
                            `select-all-text` = "Select all",
                            `none-selected-text` = "Choose year(s)",
                            `live-search` = TRUE,
                            style = "btn-outline-primary"
                          ))),
          
          br(),
          div(style = "padding: 15px; background-color: #e8f4f8; border-radius: 8px; border-left: 4px solid #3498db;",
              p(icon("info-circle"), " Use the brush tool to select data points on the hydrograph for a detailed view of the data.", 
                style = "margin: 0; font-size: 0.9rem; color: #2c3e50;")),
          
          div(style = "margin-top: 15px; text-align: center;",
              downloadButton("download_brush", "Download Selected Data", 
                             class = "btn-primary btn-sm", 
                             icon = icon("download")))
        ),
        
        card(
          full_screen = TRUE,
          card_header(
            class = "bg-primary text-white",
            "Hydrographs by Year"
          ),
          plotOutput("hydrograph", height = "600px", 
                     brush = brushOpts(id = "hydro_brush", fill = "#4C6D9A", opacity = 0.3))
        )
      )
  ),
  
  # Selected data points section with improved styling
  div(class = "brush-info-section",
      div(class = "row",
          div(class = "col-12",
              card(
                card_header(
                  class = "bg-success text-white",
                  "Selected Data from Hydrograph:"
                ),
                tableOutput("brush_info")
              )
          )
      )
  ),
  
  # Second section: Water Level Stats with improved styling
  div(class = "content-section stats-main",
      layout_sidebar(
        sidebar = sidebar(
          class = "sidebar-custom",
          width = 300,
          h4("Statistics Controls", style = "color: #1B365D; margin-bottom: 20px;"),
          
          pickerInput("stats_site", 
                      label = div(icon("map-marker"), "Select Site(s):"),
                      choices = sort(unique(wl_stats$site)),
                      selected = "Great Meadow 1",
                      multiple = TRUE,
                      options = list(
                        `actions-box` = TRUE,
                        `deselect-all-text` = "Clear all",
                        `select-all-text` = "Select all",
                        `none-selected-text` = "Choose site(s)",
                        `live-search` = TRUE,
                        style = "btn-outline-primary"
                      )),
          
          div(style = "margin-bottom: 15px;",
              pickerInput("stats_year", 
                          label = div(icon("calendar"), "Select Years:"),
                          choices = sort(unique(wl_stats$year)),
                          selected = c(2023, 2022, 2021, 2020),
                          multiple = TRUE,
                          options = list(
                            `actions-box` = TRUE,
                            `deselect-all-text` = "Clear all",
                            `select-all-text` = "Select all",
                            `none-selected-text` = "Choose year(s)",
                            `live-search` = TRUE,
                            style = "btn-outline-primary"
                          ))),
          
          div(style = "margin-bottom: 15px;",
              radioButtons("time_summary", "Summarize Stats:",
                           choices = c("Each Year" = "year", "Average Across Years" = "multi"),
                           selected = "year",
                           inline = TRUE)),
          
          br(),
          div(style = "padding: 15px; background-color: #e8f4f8; border-radius: 8px; border-left: 4px solid #3498db;",
              p(icon("info-circle"), " Growing Season statistics calculated from May through October.", 
                style = "margin: 0; font-size: 0.9rem; color: #2c3e50;")),
          
          div(style = "margin-top: 15px; text-align: center;",
              downloadButton("download_stats", "Download Table", 
                             class = "btn-primary btn-sm", 
                             icon = icon("download")))
        ),
        
        card(
          full_screen = TRUE,
          card_header(
            class = "bg-primary text-white",
            "Growing Season Water Level Statistics"
          ),
          
          div(style = "padding: 10px;",
              uiOutput("significance_info"),
              dataTableOutput("wl_stats"))
        )
      )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive data for plotting
  plot_data <- reactive({
    req(input$year, input$gm_site)
    
    all_data %>%
      filter(year %in% input$year, doy > 134, doy < 275) %>%
      filter(site %in% c(input$gm_site, "Gilmore Meadow"))
  })
  
  output$hydrograph <- renderPlot({
    plot_data_filtered <- plot_data()
    req(nrow(plot_data_filtered) > 0)
    
    # Calculate minimum water level across all data for consistent precipitation baseline
    minWL <- min(plot_data_filtered$water_depth, na.rm = TRUE)
    
    # Create the plot using simplified approach
    ggplot(plot_data_filtered, aes(x = doy_h, y = water_depth)) +
      # Water level lines by site
      geom_line(data = plot_data_filtered %>% filter(site == input$gm_site),
                aes(color = input$gm_site), size = 0.7) +
      geom_line(data = plot_data_filtered %>% filter(site == "Gilmore Meadow"),
                aes(color = "Gilmore Meadow"), size = 0.7) +
      
      # Precipitation line (scaled by multiplier of 5 and offset by minWL)
      geom_line(aes(x = doy_h, y = lag_precip * 5 + minWL, color = "Precipitation"), 
                size = 0.7) +
      
      # Ground level reference
      geom_hline(yintercept = 0, color = 'brown') +
      
      # Facet by year
      facet_wrap(~ year, ncol = 1, scales = "free_y") +
      
      # Colors
      scale_color_manual(values = c(
        setNames("black", input$gm_site),
        "Gilmore Meadow" = "darkgray",
        "Precipitation" = "blue"
      )) +
      
      # Axes and labels
      labs(y = 'Water Level (cm)', x = 'Date') +
      scale_x_continuous(
        breaks = c(121, 152, 182, 213, 244, 274),
        labels = c('May-01', 'Jun-01', 'Jul-01', 'Aug-01', 'Sep-01', 'Oct-01')
      ) +
      
      # Secondary axis for precipitation
      scale_y_continuous(
        sec.axis = sec_axis(~ .,
                            breaks = c(minWL, minWL + 10),
                            name = 'Hourly Precip. (cm)',
                            labels = c('0', '2'))
      ) +
      
      # Theme
      theme_bw() +
      theme(
        plot.title = element_text(hjust = 0.5),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.text.y.right = element_text(color = 'blue'),
        axis.title.y.right = element_text(color = 'blue'),
        strip.text = element_text(size = 11),
        legend.position = "bottom",
        legend.title = element_blank()
      )
  })
  
  # Reactive for processed brushed data
  processed_brush_data <- reactive({
    req(input$hydro_brush)
    
    plot_data_filtered <- plot_data()
    req(nrow(plot_data_filtered) > 0)
    
    selected_data <- brushedPoints(
      plot_data_filtered,
      brush = input$hydro_brush,
      xvar = "doy_h",
      yvar = "water_depth"
    ) %>%
      select(site, year, doy_h, timestamp, water_depth, lag_precip) %>%
      arrange(year, doy_h) %>%
      mutate(
        doy_h = round(doy_h, 2),
        water_depth = round(water_depth, 2),
        lag_precip = round(lag_precip, 3),
        timestamp = format(timestamp, "%Y-%m-%d %H:%M:%S")
      )
    
    if (nrow(selected_data) == 0) {
      return(data.frame(Message = "No points selected - try brushing over the water level lines"))
    }
    
    selected_data %>%
      mutate(doy_h_rounded = round(doy_h, 1)) %>%
      group_by(year, doy_h_rounded) %>%
      summarise(
        timestamp = first(timestamp),
        doy_h = first(doy_h),
        lag_precip = first(lag_precip),
        great_meadow_depth = ifelse(any(site == input$gm_site), 
                                    water_depth[site == input$gm_site][1], NA),
        gilmore_depth = ifelse(any(site == "Gilmore Meadow"), 
                               water_depth[site == "Gilmore Meadow"][1], NA),
        .groups = 'drop'
      ) %>%
      select(-doy_h_rounded) %>%
      rename(
        `Year` = year, 
        `Timestamp` = timestamp,
        `Day of Year` = doy_h,
        `Precipitation (cm)` = lag_precip
      ) %>%
      rename_with(~ paste(input$gm_site, "Water Depth (cm)"), .cols = great_meadow_depth) %>%
      rename_with(~ "Gilmore Meadow Water Depth (cm)", .cols = gilmore_depth) %>%
      arrange(Year, `Day of Year`)
  })
  
  # Reactive for significance testing results - now checks if both wetlands are selected
  significance_results <- reactive({
    req(input$stats_year, input$stats_site)
    
    # Check if both wetlands are represented in selected sites
    selected_wetlands <- wl_stats %>%
      filter(site %in% input$stats_site) %>%
      pull(wetland) %>%
      unique()
    
    # Only calculate significance if both wetlands are represented
    if (length(selected_wetlands) >= 2 && all(c("Great Meadow", "Gilmore Meadow") %in% selected_wetlands)) {
      calculate_wetland_significance(wl_stats, input$stats_year, input$stats_site, alpha = 0.05)
    } else {
      NULL
    }
  })
  
  # Reactive to determine if we should show significance info
  show_significance_info <- reactive({
    req(input$stats_site, input$time_summary)
    
    if (input$time_summary != "multi") return(FALSE)
    
    selected_wetlands <- wl_stats %>%
      filter(site %in% input$stats_site) %>%
      pull(wetland) %>%
      unique()
    
    return(length(selected_wetlands) >= 2 && all(c("Great Meadow", "Gilmore Meadow") %in% selected_wetlands))
  })
  
  
  # setting up average WL stats and calculating stats output tables
  filtered_stats <- reactive({
    req(input$stats_site, input$stats_year, input$time_summary)
    
    data <- wl_stats %>%
      filter(site %in% input$stats_site, year %in% input$stats_year)
    
    if (input$time_summary == "year") {
      # ---- Option 1: Per-Year Summary ----
      data %>%
        mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
        select(
          Year = year,
          Site = site,
          `Mean Water Level (cm)` = WL_mean,
          `SD Water Level (cm)` = WL_sd,
          `Minimum Water Level (cm)` = WL_min,
          `Maximum Water Level (cm)` = WL_max,
          `Maximum Hourly Increase (cm)` = max_inc,
          `Maximum Hourly Decrease (cm)` = max_dec,
          `Growing Season Change (cm)` = GS_change,
          `GS % Surface Water` = prop_over_0cm,
          `GS % Within 30cm` = prop_bet_0_neg30cm,
          `GS % Over 30cm Deep` = prop_under_neg30cm
        )
    } else {
      # ---- Option 2: Average Across Years ----
      # Per-site summary
      data_site <- data %>%
        group_by(site, wetland) %>%
        summarise(
          Year = paste0(min(year), "–", max(year)),
          `Mean Water Level (cm)` = round(mean(WL_mean, na.rm = TRUE), 2),
          `SD Water Level (cm)` = round(mean(WL_sd, na.rm = TRUE), 2),
          `Minimum Water Level (cm)` = round(mean(WL_min, na.rm = TRUE), 2),
          `Maximum Water Level (cm)` = round(mean(WL_max, na.rm = TRUE), 2),
          `Maximum Hourly Increase (cm)` = round(mean(max_inc, na.rm = TRUE), 2),
          `Maximum Hourly Decrease (cm)` = round(mean(max_dec, na.rm = TRUE), 2),
          `Growing Season Change (cm)` = round(mean(GS_change, na.rm = TRUE), 2),
          `GS % Surface Water` = round(mean(prop_over_0cm, na.rm = TRUE), 2),
          `GS % Within 30cm` = round(mean(prop_bet_0_neg30cm, na.rm = TRUE), 2),
          `GS % Over 30cm Deep` = round(mean(prop_under_neg30cm, na.rm = TRUE), 2),
          .groups = "drop"
        )
      
      # Per-wetland "All Sites" summary
      data_wetland <- data %>%
        group_by(wetland) %>%
        summarise(
          site = "All Sites",
          Year = paste0(min(year), "–", max(year)),
          `Mean Water Level (cm)` = round(mean(WL_mean, na.rm = TRUE), 2),
          `SD Water Level (cm)` = round(mean(WL_sd, na.rm = TRUE), 2),
          `Minimum Water Level (cm)` = round(mean(WL_min, na.rm = TRUE), 2),
          `Maximum Water Level (cm)` = round(mean(WL_max, na.rm = TRUE), 2),
          `Maximum Hourly Increase (cm)` = round(mean(max_inc, na.rm = TRUE), 2),
          `Maximum Hourly Decrease (cm)` = round(mean(max_dec, na.rm = TRUE), 2),
          `Growing Season Change (cm)` = round(mean(GS_change, na.rm = TRUE), 2),
          `GS % Surface Water` = round(mean(prop_over_0cm, na.rm = TRUE), 2),
          `GS % Within 30cm` = round(mean(prop_bet_0_neg30cm, na.rm = TRUE), 2),
          `GS % Over 30cm Deep` = round(mean(prop_under_neg30cm, na.rm = TRUE), 2),
          .groups = "drop"
        )
      
      bind_rows(data_site, data_wetland) %>%
        distinct() %>% 
        rename(Site = site,
               Wetland = wetland)
    }
  })
  
  # reactive table output for selected data from hydrograph
  output$brush_info <- renderTable({
    processed_brush_data()
  })
  
  # Output for significance information display
  output$significance_info <- renderUI({
    if (show_significance_info()) {
      sig_results <- significance_results()
      
      if (!is.null(sig_results)) {
        sig_vars <- sig_results %>% 
          filter(significant) %>% 
          pull(variable)
        
        if (length(sig_vars) > 0) {
          # Map variable names to display names
          var_display_names <- c(
            "WL_mean" = "Mean Water Level",
            "WL_sd" = "SD Water Level", 
            "WL_min" = "Minimum Water Level",
            "WL_max" = "Maximum Water Level",
            "max_inc" = "Maximum Hourly Increase",
            "max_dec" = "Maximum Hourly Decrease",
            "GS_change" = "Growing Season Change",
            "prop_over_0cm" = "GS % Surface Water",
            "prop_bet_0_neg30cm" = "GS % Within 30cm",
            "prop_under_neg30cm" = "GS % Over 30cm Deep"
          )
          
          sig_display_names <- var_display_names[sig_vars]
          sig_display_names <- sig_display_names[!is.na(sig_display_names)]
          
          div(class = "significance-info",
              h5(icon("exclamation-triangle"), " Statistical Significance", 
                 style = "color: #856404; margin-bottom: 10px;"),
              p("Highlighted variables show significant differences (p < 0.05) between Great Meadow and Gilmore Meadow wetlands:", 
                style = "margin-bottom: 8px; font-size: 0.9rem;"),
              tags$ul(
                lapply(sig_display_names, function(name) {
                  tags$li(name, style = "font-size: 0.85rem; color: #856404;")
                })
              ),
              p("Statistical tests compare averages across all selected years and sites within each wetland.", 
                style = "margin-top: 5px; margin-bottom: 10px; font-size: 0.8rem; font-style: italic; color: #6c757d;")
          )
        }
      }
    }
  })
  
  # reactive table output for WL stats with significance highlighting
  output$wl_stats <- DT::renderDataTable({
    data <- filtered_stats()
    
    dt <- datatable(
      data,
      options = list(pageLength = 10, scrollX = TRUE)
    ) %>%
      formatStyle(
        'Site',
        target = 'cell',
        fontWeight = styleEqual("All Sites", "bold")
      )
    
    # Add significance highlighting only when appropriate
    if (show_significance_info()) {
      sig_results <- significance_results()
      
      if (!is.null(sig_results)) {
        # Map variable names to column names in the table
        var_mapping <- c(
          "WL_mean" = "Mean Water Level (cm)",
          "WL_sd" = "SD Water Level (cm)", 
          "WL_min" = "Minimum Water Level (cm)",
          "WL_max" = "Maximum Water Level (cm)",
          "max_inc" = "Maximum Hourly Increase (cm)",
          "max_dec" = "Maximum Hourly Decrease (cm)",
          "GS_change" = "Growing Season Change (cm)",
          "prop_over_0cm" = "GS % Surface Water",
          "prop_bet_0_neg30cm" = "GS % Within 30cm",
          "prop_under_neg30cm" = "GS % Over 30cm Deep"
        )
        
        # Apply highlighting for significant variables
        for (var_name in names(var_mapping)) {
          col_name <- var_mapping[var_name]
          sig_row <- sig_results[sig_results$variable == var_name, ]
          
          if (nrow(sig_row) > 0 && sig_row$significant) {
            dt <- dt %>%
              formatStyle(
                col_name,
                valueColumns = "Site",
                backgroundColor = styleEqual("All Sites", "#fff3cd"),
                fontWeight = styleEqual("All Sites", "bold")
              )
          }
        }
      }
    }
    
    dt
  })
  
  # Download handler for brushed data
  output$download_brush <- downloadHandler(
    filename = function() {
      paste("hydrograph_selected_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(processed_brush_data(), file, row.names = FALSE)
    }
  )
  
  # Download handler for water level stats
  output$download_stats <- downloadHandler(
    filename = function() {
      if (input$time_summary == "multi") {
        paste("wetland_averaged_stats_", Sys.Date(), ".csv", sep = "")
      } else {
        paste("site_stats_", Sys.Date(), ".csv", sep = "")
      }
    },
    content = function(file) {
      write.csv(filtered_stats(), file, row.names = FALSE)
    }
  )
  
}

# Run app
shinyApp(ui, server)







#-------------------------------------------------------------------------------





#### APP CODE PRIOR TO SIGNIFICANCE TESTING ADITION

library(shiny)
library(tidyverse)
library(lubridate)
library(patchwork)
library(shinyWidgets)
library(DT)
library(bslib)

# Read & prepare your processed data (from your current script)
gm <- read.csv("data/processed_data/great_meadow_well_data_2024_20250715.csv") %>%
  rename(Date = date, Year = year, precip_cm = precip.cm) %>%
  mutate(Date = as.Date(Date),
         timestamp = as_datetime(timestamp),
         site = paste("Great Meadow", plot.num),
         water.depth = case_when(
           Year == 2016 & doy_h == 159.12 & plot.num == 3 & water.depth < -120 ~ NA_real_,
           Year == 2017 & doy_h == 215.02 & plot.num == 6 & water.depth < -115 ~ NA_real_,
           Year == 2021 & plot.num == 3 & doy == 224 & water.depth > 400 ~ NA_real_,
           Year == 2021 & plot.num == 3 & doy == 225 & water.depth > 400 ~ NA_real_,
           TRUE ~ water.depth
         )) %>%
  mutate(siteyear = paste(site, Year, sep = "_"))

gl <- read.csv("data/raw_data/hydrology_data/gilmore_well_prec_data_2013-2024.csv") %>%
  rename(water.depth = GILM_WL) %>%
  mutate(
    site = "Gilmore Meadow",
    
    # Try to parse full datetime, fallback to date only + midnight
    timestamp_parsed = mdy_hm(timestamp),
    timestamp_parsed = if_else(
      is.na(timestamp_parsed),
      mdy(timestamp),
      timestamp_parsed
    ),
    timestamp_parsed = as.POSIXct(timestamp_parsed),
    timestamp = timestamp_parsed,
    
    # Standardize Date column too
    Date = mdy(Date)
  ) %>%
  select(timestamp, 
         Date,
         doy,
         Year,
         precip_cm,
         water.depth,
         lag.precip,
         hr,
         doy_h,
         site)

all_data <- bind_rows(gm, gl) %>% 
  filter(Year >= 2016 & Year <= 2023) %>% 
  select(timestamp, 
         date = Date, 
         year = Year, 
         doy, 
         hr, 
         doy_h,
         precip_cm,
         lag_precip = lag.precip,
         water_depth = water.depth,
         site)

wl_stats <- read.csv("data/processed_data/gm_gl_wl_stats.csv") %>% 
  select(year,
         stat, 
         `Gilmore Meadow` = gilmore.meadow, 
         `Great Meadow 1` = great.meadow.1, 
         `Great Meadow 2` = great.meadow.2, 
         `Great Meadow 3` = great.meadow.3, 
         `Great Meadow 4` = great.meadow.4, 
         `Great Meadow 5` = great.meadow.5, 
         `Great Meadow 6` = great.meadow.6) %>% 
  pivot_longer(cols = -c(year, stat), names_to = "site", values_to = "value") %>% 
  pivot_wider(names_from = stat, values_from = value) %>%
  arrange(site, year) %>% 
  mutate(
    wetland = case_when(
      grepl("Great Meadow", site, ignore.case = TRUE) ~ "Great Meadow",
      grepl("Gilmore Meadow", site, ignore.case = TRUE) ~ "Gilmore Meadow",
      TRUE ~ "Unknown"
    )
  )


# UI 
ui <- page_fluid(
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#1B365D",
    secondary = "#4C6D9A",    
    success = "#2E86C1",
    info = "#3498db",
    warning = "#f39c12",
    danger = "#e74c3c",
    base_font = font_google("Open Sans"),
    heading_font = font_google("Roboto", wght = c(400, 700))
  ),
  
  # Custom CSS for additional styling
  tags$head(
    tags$style(HTML("
      .content-section {
        margin: 30px 0;
        padding: 25px;
        border-radius: 15px;
        box-shadow: 0 4px 12px rgba(0,0,0,0.1);
        background: linear-gradient(135deg, #ffffff 0%, #f8f9fa 100%);
        border: 2px solid #1B365D;
      }
      
      .section-title {
        color: #1B365D;
        font-weight: 600;
        margin-bottom: 20px;
        padding-bottom: 10px;
        border-bottom: 2px solid #4C6D9A;
      }
      
      .sidebar-custom {
        background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%);
        border-radius: 10px;
        padding: 20px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.08);
        border: 1px solid #dee2e6;
      }
      
      .main-title {
        background: linear-gradient(135deg, #1B365D 0%, #4C6D9A 100%);
        color: white;
        padding: 30px;
        margin: -15px -15px 30px -15px;
        text-align: center;
        border-radius: 0 0 20px 20px;
        box-shadow: 0 4px 12px rgba(0,0,0,0.15);
      }
      
      .brush-info-section {
        background: linear-gradient(135deg, #e8f4f8 0%, #f0f8ff 100%);
        border-radius: 12px;
        padding: 20px;
        margin: 20px 0;
        border: 1px solid #4C6D9A;
      }
      
      .stats-main {
        background: linear-gradient(135deg, #f8f9fa 0%, #ffffff 100%);
      }
      
      .dataTables_wrapper {
        font-size: 0.85rem !important;
      }

      .dataTables_wrapper table {
        font-size: 0.8rem !important;
      }

      .dataTables_wrapper .dataTables_info,
      .dataTables_wrapper .dataTables_paginate {
        font-size: 0.75rem !important;
      }
      
    "))
  ),
  
  # Main title with gradient background
  div(class = "main-title",
      h1("Wetland Hydrograph Visualizer", 
         style = "margin: 0; font-size: 2rem; text-shadow: 2px 2px 4px rgba(0,0,0,0.3)")
  ),
  
  # First section: Hydrographs 
  div(class = "content-section",
      layout_sidebar(
        sidebar = sidebar(
          class = "sidebar-custom",
          width = 300,
          h4("Hydrograph Controls", style = "color: #1B365D; margin-bottom: 20px;"),
          
          selectInput("gm_site", 
                      label = div(icon("map-marker"), "Select a Great Meadow Site:"),
                      choices = sort(unique(all_data$site[grepl("Great Meadow", all_data$site)])),
                      selected = "Great Meadow 1"),
          
          div(style = "margin-bottom: 15px;",
              pickerInput("year", 
                          label = div(icon("calendar"), "Select Year(s):"),
                          choices = sort(unique(all_data$year)),
                          selected = 2023,
                          multiple = TRUE,
                          options = list(
                            `actions-box` = TRUE,
                            `deselect-all-text` = "Clear all",
                            `select-all-text` = "Select all",
                            `none-selected-text` = "Choose year(s)",
                            `live-search` = TRUE,
                            style = "btn-outline-primary"
                          ))),
          
          br(),
          div(style = "padding: 15px; background-color: #e8f4f8; border-radius: 8px; border-left: 4px solid #3498db;",
              p(icon("info-circle"), " Use the brush tool to select data points on the hydrograph for a detailed view of the data.", 
                style = "margin: 0; font-size: 0.9rem; color: #2c3e50;")),
          
          div(style = "margin-top: 15px; text-align: center;",
              downloadButton("download_brush", "Download Selected Data", 
                             class = "btn-primary btn-sm", 
                             icon = icon("download")))
        ),
        
        card(
          full_screen = TRUE,
          card_header(
            class = "bg-primary text-white",
            "Hydrographs by Year"
          ),
          plotOutput("hydrograph", height = "600px", 
                     brush = brushOpts(id = "hydro_brush", fill = "#4C6D9A", opacity = 0.3))
        )
      )
  ),
  
  # Selected data points section with improved styling
  div(class = "brush-info-section",
      div(class = "row",
          div(class = "col-12",
              card(
                card_header(
                  class = "bg-success text-white",
                  "Selected Data from Hydrograph:"
                ),
                tableOutput("brush_info")
              )
          )
      )
  ),
  
  # Second section: Water Level Stats with improved styling
  div(class = "content-section stats-main",
      layout_sidebar(
        sidebar = sidebar(
          class = "sidebar-custom",
          width = 300,
          h4("Statistics Controls", style = "color: #1B365D; margin-bottom: 20px;"),
          
          pickerInput("stats_site", 
                      label = div(icon("map-marker"), "Select Site(s):"),
                      choices = sort(unique(wl_stats$site)),
                      selected = "Great Meadow 1",
                      multiple = TRUE,
                      options = list(
                        `actions-box` = TRUE,
                        `deselect-all-text` = "Clear all",
                        `select-all-text` = "Select all",
                        `none-selected-text` = "Choose site(s)",
                        `live-search` = TRUE,
                        style = "btn-outline-primary"
                      )),
          
          div(style = "margin-bottom: 15px;",
              pickerInput("stats_year", 
                          label = div(icon("calendar"), "Select Years:"),
                          choices = sort(unique(wl_stats$year)),
                          selected = c(2023, 2022, 2021, 2020),
                          multiple = TRUE,
                          options = list(
                            `actions-box` = TRUE,
                            `deselect-all-text` = "Clear all",
                            `select-all-text` = "Select all",
                            `none-selected-text` = "Choose year(s)",
                            `live-search` = TRUE,
                            style = "btn-outline-primary"
                          ))),
          
          div(style = "margin-bottom: 15px;",
              radioButtons("time_summary", "Summarize Stats:",
                           choices = c("Each Year" = "year", "Average Across Years" = "multi"),
                           selected = "year",
                           inline = TRUE)),
          
          br(),
          div(style = "padding: 15px; background-color: #e8f4f8; border-radius: 8px; border-left: 4px solid #3498db;",
              p(icon("info-circle"), " Growing Season statistics calculated from May through October.", 
                style = "margin: 0; font-size: 0.9rem; color: #2c3e50;")),
          
          div(style = "margin-top: 15px; text-align: center;",
              downloadButton("download_stats", "Download Table", 
                             class = "btn-primary btn-sm", 
                             icon = icon("download")))
        ),
        
        card(
          full_screen = TRUE,
          card_header(
            class = "bg-primary text-white",
            "Growing Season Water Level Statistics"
          ),
          div(style = "padding: 10px;",
              dataTableOutput("wl_stats"))
        )
      )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive data for plotting
  plot_data <- reactive({
    req(input$year, input$gm_site)
    
    all_data %>%
      filter(year %in% input$year, doy > 134, doy < 275) %>%
      filter(site %in% c(input$gm_site, "Gilmore Meadow"))
  })
  
  output$hydrograph <- renderPlot({
    plot_data_filtered <- plot_data()
    req(nrow(plot_data_filtered) > 0)
    
    # Calculate minimum water level across all data for consistent precipitation baseline
    minWL <- min(plot_data_filtered$water_depth, na.rm = TRUE)
    
    # Create the plot using your simplified approach
    ggplot(plot_data_filtered, aes(x = doy_h, y = water_depth)) +
      # Water level lines by site
      geom_line(data = plot_data_filtered %>% filter(site == input$gm_site),
                aes(color = input$gm_site), size = 0.7) +
      geom_line(data = plot_data_filtered %>% filter(site == "Gilmore Meadow"),
                aes(color = "Gilmore Meadow"), size = 0.7) +
      
      # Precipitation line (scaled by multiplier of 5 and offset by minWL)
      geom_line(aes(x = doy_h, y = lag_precip * 5 + minWL, color = "Precipitation"), 
                size = 0.7) +
      
      # Ground level reference
      geom_hline(yintercept = 0, color = 'brown') +
      
      # Facet by year
      facet_wrap(~ year, ncol = 1, scales = "free_y") +
      
      # Colors
      scale_color_manual(values = c(
        setNames("black", input$gm_site),
        "Gilmore Meadow" = "darkgray",
        "Precipitation" = "blue"
      )) +
      
      # Axes and labels
      labs(y = 'Water Level (cm)', x = 'Date') +
      scale_x_continuous(
        breaks = c(121, 152, 182, 213, 244, 274),
        labels = c('May-01', 'Jun-01', 'Jul-01', 'Aug-01', 'Sep-01', 'Oct-01')
      ) +
      
      # Secondary axis for precipitation
      scale_y_continuous(
        sec.axis = sec_axis(~ .,
                            breaks = c(minWL, minWL + 10),
                            name = 'Hourly Precip. (cm)',
                            labels = c('0', '2'))
      ) +
      
      # Theme
      theme_bw() +
      theme(
        plot.title = element_text(hjust = 0.5),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.text.y.right = element_text(color = 'blue'),
        axis.title.y.right = element_text(color = 'blue'),
        strip.text = element_text(size = 11),
        legend.position = "bottom",
        legend.title = element_blank()
      )
  })
  
  # Reactive for processed brushed data
  processed_brush_data <- reactive({
    req(input$hydro_brush)
    
    plot_data_filtered <- plot_data()
    req(nrow(plot_data_filtered) > 0)
    
    selected_data <- brushedPoints(
      plot_data_filtered,
      brush = input$hydro_brush,
      xvar = "doy_h",
      yvar = "water_depth"
    ) %>%
      select(site, year, doy_h, timestamp, water_depth, lag_precip) %>%
      arrange(year, doy_h) %>%
      mutate(
        doy_h = round(doy_h, 2),
        water_depth = round(water_depth, 2),
        lag_precip = round(lag_precip, 3),
        timestamp = format(timestamp, "%Y-%m-%d %H:%M:%S")
      )
    
    if (nrow(selected_data) == 0) {
      return(data.frame(Message = "No points selected - try brushing over the water level lines"))
    }
    
    selected_data %>%
      mutate(doy_h_rounded = round(doy_h, 1)) %>%
      group_by(year, doy_h_rounded) %>%
      summarise(
        timestamp = first(timestamp),
        doy_h = first(doy_h),
        lag_precip = first(lag_precip),
        great_meadow_depth = ifelse(any(site == input$gm_site), 
                                    water_depth[site == input$gm_site][1], NA),
        gilmore_depth = ifelse(any(site == "Gilmore Meadow"), 
                               water_depth[site == "Gilmore Meadow"][1], NA),
        .groups = 'drop'
      ) %>%
      select(-doy_h_rounded) %>%
      rename(
        `Year` = year, 
        `Timestamp` = timestamp,
        `Day of Year` = doy_h,
        `Precipitation (cm)` = lag_precip
      ) %>%
      rename_with(~ paste(input$gm_site, "Water Depth (cm)"), .cols = great_meadow_depth) %>%
      rename_with(~ "Gilmore Meadow Water Depth (cm)", .cols = gilmore_depth) %>%
      arrange(Year, `Day of Year`)
  })
  
  
  # setting up average WL stats and calculating stats output tables
  filtered_stats <- reactive({
    req(input$stats_site, input$stats_year, input$time_summary)
    
    data <- wl_stats %>%
      filter(site %in% input$stats_site, year %in% input$stats_year)
    
    if (input$time_summary == "year") {
      # ---- Option 1: Per-Year Summary ----
      data %>%
        mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
        select(
          Year = year,
          Site = site,
          `Mean Water Level (cm)` = WL_mean,
          `SD Water Level (cm)` = WL_sd,
          `Minimum Water Level (cm)` = WL_min,
          `Maximum Water Level (cm)` = WL_max,
          `Maximum Hourly Increase (cm)` = max_inc,
          `Maximum Hourly Decrease (cm)` = max_dec,
          `Growing Season Change (cm)` = GS_change,
          `GS % Surface Water` = prop_over_0cm,
          `GS % Within 30cm` = prop_bet_0_neg30cm,
          `GS % Over 30cm Deep` = prop_under_neg30cm
        )
    } else {
      # ---- Option 2: Average Across Years ----
      # Per-site summary
      data_site <- data %>%
        group_by(site, wetland) %>%
        summarise(
          Year = paste0(min(year), "–", max(year)),
          `Mean Water Level (cm)` = round(mean(WL_mean, na.rm = TRUE), 2),
          `SD Water Level (cm)` = round(mean(WL_sd, na.rm = TRUE), 2),
          `Minimum Water Level (cm)` = round(mean(WL_min, na.rm = TRUE), 2),
          `Maximum Water Level (cm)` = round(mean(WL_max, na.rm = TRUE), 2),
          `Maximum Hourly Increase (cm)` = round(mean(max_inc, na.rm = TRUE), 2),
          `Maximum Hourly Decrease (cm)` = round(mean(max_dec, na.rm = TRUE), 2),
          `Growing Season Change (cm)` = round(mean(GS_change, na.rm = TRUE), 2),
          `GS % Surface Water` = round(mean(prop_over_0cm, na.rm = TRUE), 2),
          `GS % Within 30cm` = round(mean(prop_bet_0_neg30cm, na.rm = TRUE), 2),
          `GS % Over 30cm Deep` = round(mean(prop_under_neg30cm, na.rm = TRUE), 2),
          .groups = "drop"
        )
      
      # Per-wetland "All Sites" summary
      data_wetland <- data %>%
        group_by(wetland) %>%
        summarise(
          site = "All Sites",
          Year = paste0(min(year), "–", max(year)),
          `Mean Water Level (cm)` = round(mean(WL_mean, na.rm = TRUE), 2),
          `SD Water Level (cm)` = round(mean(WL_sd, na.rm = TRUE), 2),
          `Minimum Water Level (cm)` = round(mean(WL_min, na.rm = TRUE), 2),
          `Maximum Water Level (cm)` = round(mean(WL_max, na.rm = TRUE), 2),
          `Maximum Hourly Increase (cm)` = round(mean(max_inc, na.rm = TRUE), 2),
          `Maximum Hourly Decrease (cm)` = round(mean(max_dec, na.rm = TRUE), 2),
          `Growing Season Change (cm)` = round(mean(GS_change, na.rm = TRUE), 2),
          `GS % Surface Water` = round(mean(prop_over_0cm, na.rm = TRUE), 2),
          `GS % Within 30cm` = round(mean(prop_bet_0_neg30cm, na.rm = TRUE), 2),
          `GS % Over 30cm Deep` = round(mean(prop_under_neg30cm, na.rm = TRUE), 2),
          .groups = "drop"
        )
      
      bind_rows(data_site, data_wetland) %>%
        distinct() %>% 
        rename(Site = site,
               Wetland = wetland)
    }
  })
  
  # reactive table output for selected data from hydrograph
  output$brush_info <- renderTable({
    processed_brush_data()
  })
  
  # reactive table output for WL stats 
  output$wl_stats <- DT::renderDataTable({
    datatable(
      filtered_stats(),
      options = list(pageLength = 10, scrollX = TRUE)
    ) %>%
      formatStyle(
        'Site',
        target = 'cell',
        fontWeight = styleEqual("All Sites", "bold")
      )
  })
  
  
  # Download handler for brushed data
  output$download_brush <- downloadHandler(
    filename = function() {
      paste("hydrograph_selected_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(processed_brush_data(), file, row.names = FALSE)
    }
  )
  
  # Download handler for water level stats
  output$download_stats <- downloadHandler(
    filename = function() {
      if (input$time_summary == "multi") {
        paste("wetland_averaged_stats_", Sys.Date(), ".csv", sep = "")
      } else {
        paste("site_stats_", Sys.Date(), ".csv", sep = "")
      }
    },
    content = function(file) {
      write.csv(filtered_stats(), file, row.names = FALSE)
    }
  )
  
}

# Run app
shinyApp(ui, server)




####----------------------------------------------------------------------------



library(shiny)
library(tidyverse)
library(lubridate)
library(patchwork)
library(shinyWidgets)
library(DT)
library(bslib)

# Read & prepare your processed data (from your current script)
gm <- read.csv("data/processed_data/great_meadow_well_data_2024_20250715.csv") %>%
  rename(Date = date, Year = year, precip_cm = precip.cm) %>%
  mutate(Date = as.Date(Date),
         timestamp = as_datetime(timestamp),
         site = paste("Great Meadow", plot.num),
         water.depth = case_when(
           Year == 2016 & doy_h == 159.12 & plot.num == 3 & water.depth < -120 ~ NA_real_,
           Year == 2017 & doy_h == 215.02 & plot.num == 6 & water.depth < -115 ~ NA_real_,
           Year == 2021 & plot.num == 3 & doy == 224 & water.depth > 400 ~ NA_real_,
           Year == 2021 & plot.num == 3 & doy == 225 & water.depth > 400 ~ NA_real_,
           TRUE ~ water.depth
         )) %>%
  mutate(siteyear = paste(site, Year, sep = "_"))

gl <- read.csv("data/raw_data/hydrology_data/gilmore_well_prec_data_2013-2024.csv") %>%
  rename(water.depth = GILM_WL) %>%
  mutate(
    site = "Gilmore Meadow",
    
    # Try to parse full datetime, fallback to date only + midnight
    timestamp_parsed = mdy_hm(timestamp),
    timestamp_parsed = if_else(
      is.na(timestamp_parsed),
      mdy(timestamp),
      timestamp_parsed
    ),
    timestamp_parsed = as.POSIXct(timestamp_parsed),
    timestamp = timestamp_parsed,
    
    # Standardize Date column too
    Date = mdy(Date)
  ) %>%
  select(timestamp, 
         Date,
         doy,
         Year,
         precip_cm,
         water.depth,
         lag.precip,
         hr,
         doy_h,
         site)

all_data <- bind_rows(gm, gl) %>% 
  filter(Year >= 2016 & Year <= 2023) %>% 
  select(timestamp, 
         date = Date, 
         year = Year, 
         doy, 
         hr, 
         doy_h,
         precip_cm,
         lag_precip = lag.precip,
         water_depth = water.depth,
         site)

wl_stats <- read.csv("data/processed_data/gm_gl_wl_stats.csv") %>% 
  select(year,
         stat, 
         `Gilmore Meadow` = gilmore.meadow, 
         `Great Meadow 1` = great.meadow.1, 
         `Great Meadow 2` = great.meadow.2, 
         `Great Meadow 3` = great.meadow.3, 
         `Great Meadow 4` = great.meadow.4, 
         `Great Meadow 5` = great.meadow.5, 
         `Great Meadow 6` = great.meadow.6) %>% 
  pivot_longer(cols = -c(year, stat), names_to = "site", values_to = "value") %>% 
  pivot_wider(names_from = stat, values_from = value) %>%
  arrange(site, year) %>% 
  mutate(
    wetland = case_when(
      grepl("Great Meadow", site, ignore.case = TRUE) ~ "Great Meadow",
      grepl("Gilmore Meadow", site, ignore.case = TRUE) ~ "Gilmore Meadow",
      TRUE ~ "Unknown"
    )
  )


# UI 
ui <- page_fluid(
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#1B365D",
    secondary = "#4C6D9A",    
    success = "#2E86C1",
    info = "#3498db",
    warning = "#f39c12",
    danger = "#e74c3c",
    base_font = font_google("Open Sans"),
    heading_font = font_google("Roboto", wght = c(400, 700))
  ),
  
  # Custom CSS for additional styling
  tags$head(
    tags$style(HTML("
      .content-section {
        margin: 30px 0;
        padding: 25px;
        border-radius: 15px;
        box-shadow: 0 4px 12px rgba(0,0,0,0.1);
        background: linear-gradient(135deg, #ffffff 0%, #f8f9fa 100%);
        border: 2px solid #1B365D;
      }
      
      .section-title {
        color: #1B365D;
        font-weight: 600;
        margin-bottom: 20px;
        padding-bottom: 10px;
        border-bottom: 2px solid #4C6D9A;
      }
      
      .sidebar-custom {
        background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%);
        border-radius: 10px;
        padding: 20px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.08);
        border: 1px solid #dee2e6;
      }
      
      .main-title {
        background: linear-gradient(135deg, #1B365D 0%, #4C6D9A 100%);
        color: white;
        padding: 30px;
        margin: -15px -15px 30px -15px;
        text-align: center;
        border-radius: 0 0 20px 20px;
        box-shadow: 0 4px 12px rgba(0,0,0,0.15);
      }
      
      .brush-info-section {
        background: linear-gradient(135deg, #e8f4f8 0%, #f0f8ff 100%);
        border-radius: 12px;
        padding: 20px;
        margin: 20px 0;
        border: 1px solid #4C6D9A;
      }
      
      .stats-main {
        background: linear-gradient(135deg, #f8f9fa 0%, #ffffff 100%);
      }
      
      .dataTables_wrapper {
        font-size: 0.85rem !important;
      }

      .dataTables_wrapper table {
        font-size: 0.8rem !important;
      }

      .dataTables_wrapper .dataTables_info,
      .dataTables_wrapper .dataTables_paginate {
        font-size: 0.75rem !important;
      }
      
    "))
  ),
  
  # Main title with gradient background
  div(class = "main-title",
      h1("Wetland Hydrograph Visualizer", 
         style = "margin: 0; font-size: 2rem; text-shadow: 2px 2px 4px rgba(0,0,0,0.3)")
  ),
  
  # First section: Hydrographs 
  div(class = "content-section",
      layout_sidebar(
        sidebar = sidebar(
          class = "sidebar-custom",
          width = 300,
          h4("Hydrograph Controls", style = "color: #1B365D; margin-bottom: 20px;"),
          
          selectInput("gm_site", 
                      label = div(icon("map-marker"), "Select a Great Meadow Site:"),
                      choices = sort(unique(all_data$site[grepl("Great Meadow", all_data$site)])),
                      selected = "Great Meadow 1"),
          
          div(style = "margin-bottom: 15px;",
              pickerInput("year", 
                          label = div(icon("calendar"), "Select Year(s):"),
                          choices = sort(unique(all_data$year)),
                          selected = 2023,
                          multiple = TRUE,
                          options = list(
                            `actions-box` = TRUE,
                            `deselect-all-text` = "Clear all",
                            `select-all-text` = "Select all",
                            `none-selected-text` = "Choose year(s)",
                            `live-search` = TRUE,
                            style = "btn-outline-primary"
                          ))),
          
          br(),
          div(style = "padding: 15px; background-color: #e8f4f8; border-radius: 8px; border-left: 4px solid #3498db;",
              p(icon("info-circle"), " Use the brush tool to select data points on the hydrograph for a detailed view of the data.", 
                style = "margin: 0; font-size: 0.9rem; color: #2c3e50;")),
          
          div(style = "margin-top: 15px; text-align: center;",
              downloadButton("download_brush", "Download Selected Data", 
                             class = "btn-primary btn-sm", 
                             icon = icon("download")))
        ),
        
        card(
          full_screen = TRUE,
          card_header(
            class = "bg-primary text-white",
            "Hydrographs by Year"
          ),
          plotOutput("hydrograph", height = "600px", 
                     brush = brushOpts(id = "hydro_brush", fill = "#4C6D9A", opacity = 0.3))
        )
      )
  ),
  
  # Selected data points section with improved styling
  div(class = "brush-info-section",
      div(class = "row",
          div(class = "col-12",
              card(
                card_header(
                  class = "bg-success text-white",
                  "Selected Data from Hydrograph:"
                ),
                tableOutput("brush_info")
              )
          )
      )
  ),
  
  # Second section: Water Level Stats with improved styling
  div(class = "content-section stats-main",
      layout_sidebar(
        sidebar = sidebar(
          class = "sidebar-custom",
          width = 300,
          h4("Statistics Controls", style = "color: #1B365D; margin-bottom: 20px;"),
          
          pickerInput("stats_site", 
                      label = div(icon("map-marker"), "Select Site(s):"),
                      choices = sort(unique(wl_stats$site)),
                      selected = "Great Meadow 1",
                      multiple = TRUE,
                      options = list(
                        `actions-box` = TRUE,
                        `deselect-all-text` = "Clear all",
                        `select-all-text` = "Select all",
                        `none-selected-text` = "Choose site(s)",
                        `live-search` = TRUE,
                        style = "btn-outline-primary"
                      )),
          
          div(style = "margin-bottom: 15px;",
              pickerInput("stats_year", 
                          label = div(icon("calendar"), "Select Years:"),
                          choices = sort(unique(wl_stats$year)),
                          selected = c(2023, 2022, 2021, 2020),
                          multiple = TRUE,
                          options = list(
                            `actions-box` = TRUE,
                            `deselect-all-text` = "Clear all",
                            `select-all-text` = "Select all",
                            `none-selected-text` = "Choose year(s)",
                            `live-search` = TRUE,
                            style = "btn-outline-primary"
                          ))),
          
          div(style = "margin-bottom: 15px;",
              radioButtons("time_summary", "Summarize Stats:",
                           choices = c("Each Year" = "year", "Average Across Years" = "multi"),
                           selected = "year",
                           inline = TRUE)),
          
          br(),
          div(style = "padding: 15px; background-color: #e8f4f8; border-radius: 8px; border-left: 4px solid #3498db;",
              p(icon("info-circle"), " Growing Season statistics calculated from May through October.", 
                style = "margin: 0; font-size: 0.9rem; color: #2c3e50;")),
          
          div(style = "margin-top: 15px; text-align: center;",
              downloadButton("download_stats", "Download Table", 
                             class = "btn-primary btn-sm", 
                             icon = icon("download")))
        ),
        
        card(
          full_screen = TRUE,
          card_header(
            class = "bg-primary text-white",
            "Growing Season Water Level Statistics"
          ),
          div(style = "padding: 10px;",
              dataTableOutput("wl_stats"))
        )
      )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive data for plotting
  plot_data <- reactive({
    req(input$year, input$gm_site)
    
    all_data %>%
      filter(year %in% input$year, doy > 134, doy < 275) %>%
      filter(site %in% c(input$gm_site, "Gilmore Meadow"))
  })
  
  output$hydrograph <- renderPlot({
    plot_data_filtered <- plot_data()
    req(nrow(plot_data_filtered) > 0)
    
    # Calculate minimum water level across all data for consistent precipitation baseline
    minWL <- min(plot_data_filtered$water_depth, na.rm = TRUE)
    
    # Create the plot using your simplified approach
    ggplot(plot_data_filtered, aes(x = doy_h, y = water_depth)) +
      # Water level lines by site
      geom_line(data = plot_data_filtered %>% filter(site == input$gm_site),
                aes(color = input$gm_site), size = 0.7) +
      geom_line(data = plot_data_filtered %>% filter(site == "Gilmore Meadow"),
                aes(color = "Gilmore Meadow"), size = 0.7) +
      
      # Precipitation line (scaled by multiplier of 5 and offset by minWL)
      geom_line(aes(x = doy_h, y = lag_precip * 5 + minWL, color = "Precipitation"), 
                size = 0.7) +
      
      # Ground level reference
      geom_hline(yintercept = 0, color = 'brown') +
      
      # Facet by year
      facet_wrap(~ year, ncol = 1, scales = "free_y") +
      
      # Colors
      scale_color_manual(values = c(
        setNames("black", input$gm_site),
        "Gilmore Meadow" = "darkgray",
        "Precipitation" = "blue"
      )) +
      
      # Axes and labels
      labs(y = 'Water Level (cm)', x = 'Date') +
      scale_x_continuous(
        breaks = c(121, 152, 182, 213, 244, 274),
        labels = c('May-01', 'Jun-01', 'Jul-01', 'Aug-01', 'Sep-01', 'Oct-01')
      ) +
      
      # Secondary axis for precipitation
      scale_y_continuous(
        sec.axis = sec_axis(~ .,
                            breaks = c(minWL, minWL + 10),
                            name = 'Hourly Precip. (cm)',
                            labels = c('0', '2'))
      ) +
      
      # Theme
      theme_bw() +
      theme(
        plot.title = element_text(hjust = 0.5),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.text.y.right = element_text(color = 'blue'),
        axis.title.y.right = element_text(color = 'blue'),
        strip.text = element_text(size = 11),
        legend.position = "bottom",
        legend.title = element_blank()
      )
  })
  
  # Reactive for processed brushed data
  processed_brush_data <- reactive({
    req(input$hydro_brush)
    
    plot_data_filtered <- plot_data() %>%
      filter(doy > 134 & doy < 275) %>%
      filter(site %in% c(input$gm_site, "Gilmore Meadow"))
    
    selected_data <- brushedPoints(
      plot_data_filtered[,c('timestamp','year','doy_h', 'water_depth', 'precip_cm', 'site')],
      brush = input$hydro_brush,
      xvar = "doy_h",
      yvar = "water_depth"
    ) %>%
      select(site, year, doy_h, timestamp, water_depth, precip_cm) %>%
      arrange(year, doy_h) %>%
      mutate(
        doy_h = round(doy_h, 2),
        water_depth = round(water_depth, 2),
        precip_cm = round(precip_cm, 3),
        timestamp = format(timestamp, "%Y-%m-%d %H:%M:%S")
      )
    
    if (nrow(selected_data) == 0) {
      return(data.frame(Message = "No points selected - try brushing over the water level lines"))
    }
    
    selected_data %>%
      mutate(doy_h_rounded = round(doy_h, 1)) %>%
      group_by(year, doy_h_rounded) %>%
      summarise(
        timestamp = first(timestamp),
        doy_h = first(doy_h),
        precip_cm = first(precip_cm),
        great_meadow_depth = ifelse(any(site == input$gm_site), 
                                    water_depth[site == input$gm_site][1], NA),
        gilmore_depth = ifelse(any(site == "Gilmore Meadow"), 
                               water_depth[site == "Gilmore Meadow"][1], NA),
        .groups = 'drop'
      ) %>%
      select(-doy_h_rounded) %>%
      rename(
        `Year` = year, 
        `Timestamp` = timestamp,
        `Day of Year` = doy_h,
        `Precipitation (cm)` = precip_cm
      ) %>%
      rename_with(~ paste(input$gm_site, "Water Depth (cm)"), .cols = great_meadow_depth) %>%
      rename_with(~ "Gilmore Meadow Water Depth (cm)", .cols = gilmore_depth) %>%
      arrange(Year, `Day of Year`)
  })
  
  
  # setting up average WL stats and calculating stats output tables
  filtered_stats <- reactive({
    req(input$stats_site, input$stats_year, input$time_summary)
    
    data <- wl_stats %>%
      filter(site %in% input$stats_site, year %in% input$stats_year)
    
    if (input$time_summary == "year") {
      # ---- Option 1: Per-Year Summary ----
      data %>%
        mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
        select(
          Year = year,
          Site = site,
          `Mean Water Level (cm)` = WL_mean,
          `SD Water Level (cm)` = WL_sd,
          `Minimum Water Level (cm)` = WL_min,
          `Maximum Water Level (cm)` = WL_max,
          `Maximum Hourly Increase (cm)` = max_inc,
          `Maximum Hourly Decrease (cm)` = max_dec,
          `Growing Season Change (cm)` = GS_change,
          `GS % Surface Water` = prop_over_0cm,
          `GS % Within 30cm` = prop_bet_0_neg30cm,
          `GS % Over 30cm Deep` = prop_under_neg30cm
        )
    } else {
      # ---- Option 2: Average Across Years ----
      # Per-site summary
      data_site <- data %>%
        group_by(site, wetland) %>%
        summarise(
          Year = paste0(min(year), "–", max(year)),
          `Mean Water Level (cm)` = round(mean(WL_mean, na.rm = TRUE), 2),
          `SD Water Level (cm)` = round(mean(WL_sd, na.rm = TRUE), 2),
          `Minimum Water Level (cm)` = round(mean(WL_min, na.rm = TRUE), 2),
          `Maximum Water Level (cm)` = round(mean(WL_max, na.rm = TRUE), 2),
          `Maximum Hourly Increase (cm)` = round(mean(max_inc, na.rm = TRUE), 2),
          `Maximum Hourly Decrease (cm)` = round(mean(max_dec, na.rm = TRUE), 2),
          `Growing Season Change (cm)` = round(mean(GS_change, na.rm = TRUE), 2),
          `GS % Surface Water` = round(mean(prop_over_0cm, na.rm = TRUE), 2),
          `GS % Within 30cm` = round(mean(prop_bet_0_neg30cm, na.rm = TRUE), 2),
          `GS % Over 30cm Deep` = round(mean(prop_under_neg30cm, na.rm = TRUE), 2),
          .groups = "drop"
        )
      
      # Per-wetland "All Sites" summary
      data_wetland <- data %>%
        group_by(wetland) %>%
        summarise(
          site = "All Sites",
          Year = paste0(min(year), "–", max(year)),
          `Mean Water Level (cm)` = round(mean(WL_mean, na.rm = TRUE), 2),
          `SD Water Level (cm)` = round(mean(WL_sd, na.rm = TRUE), 2),
          `Minimum Water Level (cm)` = round(mean(WL_min, na.rm = TRUE), 2),
          `Maximum Water Level (cm)` = round(mean(WL_max, na.rm = TRUE), 2),
          `Maximum Hourly Increase (cm)` = round(mean(max_inc, na.rm = TRUE), 2),
          `Maximum Hourly Decrease (cm)` = round(mean(max_dec, na.rm = TRUE), 2),
          `Growing Season Change (cm)` = round(mean(GS_change, na.rm = TRUE), 2),
          `GS % Surface Water` = round(mean(prop_over_0cm, na.rm = TRUE), 2),
          `GS % Within 30cm` = round(mean(prop_bet_0_neg30cm, na.rm = TRUE), 2),
          `GS % Over 30cm Deep` = round(mean(prop_under_neg30cm, na.rm = TRUE), 2),
          .groups = "drop"
        )
      
      bind_rows(data_site, data_wetland) %>%
        distinct() %>% 
        rename(Site = site,
               Wetland = wetland)
    }
  })
  
  # reactive table output for selected data from hydrograph
  output$brush_info <- renderTable({
    processed_brush_data()
  })
  
  # reactive table output for WL stats 
  output$wl_stats <- DT::renderDataTable({
    datatable(
      filtered_stats(),
      options = list(pageLength = 10, scrollX = TRUE)
    ) %>%
      formatStyle(
        'Site',
        target = 'cell',
        fontWeight = styleEqual("All Sites", "bold")
      )
  })
  
  
  # Download handler for brushed data
  output$download_brush <- downloadHandler(
    filename = function() {
      paste("hydrograph_selected_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(processed_brush_data(), file, row.names = FALSE)
    }
  )
  
  # Download handler for water level stats
  output$download_stats <- downloadHandler(
    filename = function() {
      if (input$time_summary == "multi") {
        paste("wetland_averaged_stats_", Sys.Date(), ".csv", sep = "")
      } else {
        paste("site_stats_", Sys.Date(), ".csv", sep = "")
      }
    },
    content = function(file) {
      write.csv(filtered_stats(), file, row.names = FALSE)
    }
  )
  
}

# Run app
shinyApp(ui, server)





# ------------------------------------------------------------------------------




library(shiny)
library(tidyverse)
library(lubridate)
library(patchwork)
library(shinyWidgets)
library(DT)
library(bslib)

# Read & prepare your processed data (from your current script)
gm <- read.csv("data/processed_data/great_meadow_well_data_2024_20250715.csv") %>%
  rename(Date = date, Year = year, precip_cm = precip.cm) %>%
  mutate(Date = as.Date(Date),
         timestamp = as_datetime(timestamp),
         site = paste("Great Meadow", plot.num),
         water.depth = case_when(
           Year == 2016 & doy_h == 159.12 & plot.num == 3 & water.depth < -120 ~ NA_real_,
           Year == 2017 & doy_h == 215.02 & plot.num == 6 & water.depth < -115 ~ NA_real_,
           Year == 2021 & plot.num == 3 & doy == 224 & water.depth > 400 ~ NA_real_,
           Year == 2021 & plot.num == 3 & doy == 225 & water.depth > 400 ~ NA_real_,
           TRUE ~ water.depth
         )) %>%
  mutate(siteyear = paste(site, Year, sep = "_"))

gl <- read.csv("data/raw_data/hydrology_data/gilmore_well_prec_data_2013-2024.csv") %>%
  rename(water.depth = GILM_WL) %>%
  mutate(
    site = "Gilmore Meadow",
    
    # Try to parse full datetime, fallback to date only + midnight
    timestamp_parsed = mdy_hm(timestamp),
    timestamp_parsed = if_else(
      is.na(timestamp_parsed),
      mdy(timestamp),
      timestamp_parsed
    ),
    timestamp_parsed = as.POSIXct(timestamp_parsed),
    timestamp = timestamp_parsed,
    
    # Standardize Date column too
    Date = mdy(Date)
  ) %>%
  select(timestamp, 
         Date,
         doy,
         Year,
         precip_cm,
         water.depth,
         lag.precip,
         hr,
         doy_h,
         site)

all_data <- bind_rows(gm, gl) %>% 
  filter(Year >= 2016 & Year <= 2023) %>% 
  select(timestamp, 
         date = Date, 
         year = Year, 
         doy, 
         hr, 
         doy_h, 
         lag_precip = lag.precip,
         water_depth = water.depth,
         precip_cm,
         site)

wl_stats <- read.csv("data/processed_data/gm_gl_wl_stats.csv") %>% 
  select(year,
         stat, 
         `Gilmore Meadow` = gilmore.meadow, 
         `Great Meadow 1` = great.meadow.1, 
         `Great Meadow 2` = great.meadow.2, 
         `Great Meadow 3` = great.meadow.3, 
         `Great Meadow 4` = great.meadow.4, 
         `Great Meadow 5` = great.meadow.5, 
         `Great Meadow 6` = great.meadow.6) %>% 
  pivot_longer(cols = -c(year, stat), names_to = "site", values_to = "value") %>% 
  pivot_wider(names_from = stat, values_from = value) %>%
  arrange(site, year) %>% 
  mutate(
    wetland = case_when(
      grepl("Great Meadow", site, ignore.case = TRUE) ~ "Great Meadow",
      grepl("Gilmore Meadow", site, ignore.case = TRUE) ~ "Gilmore Meadow",
      TRUE ~ "Unknown"
    )
  )

# Simple plotting function based on your example
plot_hydro_site_year <- function(df, site, years = 2016:2023){
  minWL <- min(df$water_depth, na.rm = TRUE)
  
  df <- df %>% 
    filter(year %in% years) %>%
    filter(doy > 134 & doy < 275) %>%
    filter(site %in% c(site, "Gilmore Meadow")) %>%
    droplevels()
  
  p <- ggplot(df, aes(x = doy_h, y = water_depth)) +
    # Water level lines by site
    geom_line(data = df %>% filter(site == !!site), 
              aes(color = site), size = 0.7) +
    geom_line(data = df %>% filter(site == "Gilmore Meadow"), 
              aes(color = "Gilmore Meadow"), size = 0.7) +
    
    # Precipitation line (scaled by multiplier of 5 and offset by minWL)
    geom_line(aes(x = doy_h, y = lag_precip * 5 + minWL, color = "Precipitation"), 
              size = 0.7) +
    
    # Ground level reference
    geom_hline(yintercept = 0, col = 'brown') +
    
    # Facet by year
    facet_wrap(~ year, nrow = length(unique(df$year))) +
    
    # Colors
    scale_color_manual(values = c(
      setNames("black", site),
      "Gilmore Meadow" = "darkgray",
      "Precipitation" = "blue"
    )) +
    
    # Axes and labels
    labs(title = site, y = 'Water Level (cm)\n', x = 'Date') +
    scale_x_continuous(
      breaks = c(121, 152, 182, 213, 244, 274),
      labels = c('May-01', 'Jun-01', 'Jul-01', 'Aug-01', 'Sep-01', 'Oct-01')
    ) +
    
    # Secondary axis for precipitation
    scale_y_continuous(
      sec.axis = sec_axis(~ .,
                          breaks = c(minWL, minWL + 10),
                          name = 'Hourly Precip. (cm)\n',
                          labels = c('0', '2'))
    ) +
    
    # Theme
    theme_bw() +
    theme(
      plot.title = element_text(hjust = 0.5),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      axis.text.y.right = element_text(color = 'blue'),
      axis.title.y.right = element_text(color = 'blue'),
      strip.text = element_text(size = 11),
      legend.position = "bottom",
      legend.title = element_blank()
    )
  
  return(p)
}

# UI 
ui <- page_fluid(
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#1B365D"
  ),
  
  titlePanel("Wetland Hydrograph Visualizer"),
  
  layout_sidebar(
    sidebar = sidebar(
      width = 300,
      h4("Hydrograph Controls"),
      
      selectInput("gm_site", 
                  label = "Select a Great Meadow Site:",
                  choices = sort(unique(all_data$site[grepl("Great Meadow", all_data$site)])),
                  selected = "Great Meadow 1"),
      
      pickerInput("year", 
                  label = "Select Year(s):",
                  choices = sort(unique(all_data$year)),
                  selected = 2023,
                  multiple = TRUE,
                  options = list(`actions-box` = TRUE))
    ),
    
    card(
      full_screen = TRUE,
      card_header("Hydrographs by Year"),
      plotOutput("hydroPlot", height = "600px", 
                 brush = brushOpts(id = "plot_brush"))
    )
  ),
  
  card(
    card_header("Selected Data from Hydrograph:"),
    tableOutput("info")
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive data for plotting
  plot_data <- reactive({
    req(input$year, input$gm_site)
    
    all_data %>%
      filter(year %in% input$year)
  })
  
  # Render hydrograph plot
  plotInput <- reactive({
    plot_hydro_site_year(
      df = plot_data(),
      site = input$gm_site,
      years = input$year
    )
  })
  
  output$hydroPlot <- renderPlot({
    plotInput()
  })  
  
  # Brushed points output - using precip_cm for output (like the example)
  output$info <- renderTable({
    req(input$plot_brush)
    
    plot_data_filtered <- plot_data() %>%
      filter(doy > 134 & doy < 275) %>%
      filter(site %in% c(input$gm_site, "Gilmore Meadow"))
    
    brushedPoints(
      plot_data_filtered[,c('timestamp','year','doy_h', 'water_depth', 'precip_cm', 'site')], 
      input$plot_brush, 
      xvar = "doy_h", 
      yvar = "water_depth"
    ) %>%
      select(timestamp, year, doy_h, site, water_depth, precip_cm) %>%
      arrange(year, doy_h)
  }, rownames = TRUE)
}

# Run the application 
shinyApp(ui = ui, server = server)





#-------------------------------------------------------------------------------






# app code version 8/11/25 - hydrograph selected data pulls from underlying data format,
# not what presented in app

library(shiny)
library(tidyverse)
library(lubridate)
library(patchwork)
library(shinyWidgets)
library(DT)
library(bslib)

# Read & prepare your processed data (from your current script)
gm <- read.csv("data/processed_data/great_meadow_well_data_2024_20250715.csv") %>%
  rename(Date = date, Year = year, precip_cm = precip.cm) %>%
  mutate(Date = as.Date(Date),
         timestamp = as_datetime(timestamp),
         site = paste("Great Meadow", plot.num),
         water.depth = case_when(
           Year == 2016 & doy_h == 159.12 & plot.num == 3 & water.depth < -120 ~ NA_real_,
           Year == 2017 & doy_h == 215.02 & plot.num == 6 & water.depth < -115 ~ NA_real_,
           Year == 2021 & plot.num == 3 & doy == 224 & water.depth > 400 ~ NA_real_,
           Year == 2021 & plot.num == 3 & doy == 225 & water.depth > 400 ~ NA_real_,
           TRUE ~ water.depth
         )) %>%
  mutate(siteyear = paste(site, Year, sep = "_"))

gl <- read.csv("data/raw_data/hydrology_data/gilmore_well_prec_data_2013-2024.csv") %>%
  rename(water.depth = GILM_WL) %>%
  mutate(
    site = "Gilmore Meadow",
    
    # Try to parse full datetime, fallback to date only + midnight
    timestamp_parsed = mdy_hm(timestamp),
    timestamp_parsed = if_else(
      is.na(timestamp_parsed),
      mdy(timestamp),
      timestamp_parsed
    ),
    timestamp_parsed = as.POSIXct(timestamp_parsed),
    timestamp = timestamp_parsed,
    
    # Standardize Date column too
    Date = mdy(Date)
  ) %>%
  select(timestamp, 
         Date,
         doy,
         Year,
         precip_cm,
         water.depth,
         lag.precip,
         hr,
         doy_h,
         site)

all_data <- bind_rows(gm, gl) %>% 
  filter(Year >= 2016 & Year <= 2023) %>% 
  select(timestamp, 
         date = Date, 
         year = Year, 
         doy, 
         hr, 
         doy_h, 
         lag_precip = lag.precip,
         water_depth = water.depth,
         site)

wl_stats <- read.csv("data/processed_data/gm_gl_wl_stats.csv") %>% 
  select(year,
         stat, 
         `Gilmore Meadow` = gilmore.meadow, 
         `Great Meadow 1` = great.meadow.1, 
         `Great Meadow 2` = great.meadow.2, 
         `Great Meadow 3` = great.meadow.3, 
         `Great Meadow 4` = great.meadow.4, 
         `Great Meadow 5` = great.meadow.5, 
         `Great Meadow 6` = great.meadow.6) %>% 
  pivot_longer(cols = -c(year, stat), names_to = "site", values_to = "value") %>% 
  pivot_wider(names_from = stat, values_from = value) %>%
  arrange(site, year) %>% 
  mutate(
    wetland = case_when(
      grepl("Great Meadow", site, ignore.case = TRUE) ~ "Great Meadow",
      grepl("Gilmore Meadow", site, ignore.case = TRUE) ~ "Gilmore Meadow",
      TRUE ~ "Unknown"
    )
  )


# UI 
ui <- page_fluid(
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#1B365D",
    secondary = "#4C6D9A",    
    success = "#2E86C1",
    info = "#3498db",
    warning = "#f39c12",
    danger = "#e74c3c",
    base_font = font_google("Open Sans"),
    heading_font = font_google("Roboto", wght = c(400, 700))
  ),
  
  # Custom CSS for additional styling
  tags$head(
    tags$style(HTML("
      .content-section {
        margin: 30px 0;
        padding: 25px;
        border-radius: 15px;
        box-shadow: 0 4px 12px rgba(0,0,0,0.1);
        background: linear-gradient(135deg, #ffffff 0%, #f8f9fa 100%);
        border: 2px solid #1B365D;
      }
      
      .section-title {
        color: #1B365D;
        font-weight: 600;
        margin-bottom: 20px;
        padding-bottom: 10px;
        border-bottom: 2px solid #4C6D9A;
      }
      
      .sidebar-custom {
        background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%);
        border-radius: 10px;
        padding: 20px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.08);
        border: 1px solid #dee2e6;
      }
      
      .main-title {
        background: linear-gradient(135deg, #1B365D 0%, #4C6D9A 100%);
        color: white;
        padding: 30px;
        margin: -15px -15px 30px -15px;
        text-align: center;
        border-radius: 0 0 20px 20px;
        box-shadow: 0 4px 12px rgba(0,0,0,0.15);
      }
      
      .brush-info-section {
        background: linear-gradient(135deg, #e8f4f8 0%, #f0f8ff 100%);
        border-radius: 12px;
        padding: 20px;
        margin: 20px 0;
        border: 1px solid #4C6D9A;
      }
      
      .stats-main {
        background: linear-gradient(135deg, #f8f9fa 0%, #ffffff 100%);
      }
      
      .dataTables_wrapper {
        font-size: 0.85rem !important;
      }

      .dataTables_wrapper table {
        font-size: 0.8rem !important;
      }

      .dataTables_wrapper .dataTables_info,
      .dataTables_wrapper .dataTables_paginate {
        font-size: 0.75rem !important;
      }
      
    "))
  ),
  
  # Main title with gradient background
  div(class = "main-title",
      h1("Wetland Hydrograph Visualizer", 
         style = "margin: 0; font-size: 2rem; text-shadow: 2px 2px 4px rgba(0,0,0,0.3)")
  ),
  
  # First section: Hydrographs 
  div(class = "content-section",
      layout_sidebar(
        sidebar = sidebar(
          class = "sidebar-custom",
          width = 300,
          h4("Hydrograph Controls", style = "color: #1B365D; margin-bottom: 20px;"),
          
          selectInput("gm_site", 
                      label = div(icon("map-marker"), "Select a Great Meadow Site:"),
                      choices = sort(unique(all_data$site[grepl("Great Meadow", all_data$site)])),
                      selected = "Great Meadow 1"),
          
          div(style = "margin-bottom: 15px;",
              pickerInput("year", 
                          label = div(icon("calendar"), "Select Year(s):"),
                          choices = sort(unique(all_data$year)),
                          selected = 2023,
                          multiple = TRUE,
                          options = list(
                            `actions-box` = TRUE,
                            `deselect-all-text` = "Clear all",
                            `select-all-text` = "Select all",
                            `none-selected-text` = "Choose year(s)",
                            `live-search` = TRUE,
                            style = "btn-outline-primary"
                          ))),
          
          br(),
          div(style = "padding: 15px; background-color: #e8f4f8; border-radius: 8px; border-left: 4px solid #3498db;",
              p(icon("info-circle"), " Use the brush tool to select data points on the hydrograph for a detailed view of the data.", 
                style = "margin: 0; font-size: 0.9rem; color: #2c3e50;")),
          
          div(style = "margin-top: 15px; text-align: center;",
              downloadButton("download_brush", "Download Selected Data", 
                             class = "btn-primary btn-sm", 
                             icon = icon("download")))
        ),
        
        card(
          full_screen = TRUE,
          card_header(
            class = "bg-primary text-white",
            "Hydrographs by Year"
          ),
          plotOutput("hydrograph", height = "600px", 
                     brush = brushOpts(id = "hydro_brush", fill = "#4C6D9A", opacity = 0.3))
        )
      )
  ),
  
  # Selected data points section with improved styling
  div(class = "brush-info-section",
      div(class = "row",
          div(class = "col-12",
              card(
                card_header(
                  class = "bg-success text-white",
                  "Selected Data from Hydrograph:"
                ),
                tableOutput("brush_info")
              )
          )
      )
  ),
  
  # Second section: Water Level Stats with improved styling
  div(class = "content-section stats-main",
      layout_sidebar(
        sidebar = sidebar(
          class = "sidebar-custom",
          width = 300,
          h4("Statistics Controls", style = "color: #1B365D; margin-bottom: 20px;"),
          
          pickerInput("stats_site", 
                      label = div(icon("map-marker"), "Select Site(s):"),
                      choices = sort(unique(wl_stats$site)),
                      selected = "Great Meadow 1",
                      multiple = TRUE,
                      options = list(
                        `actions-box` = TRUE,
                        `deselect-all-text` = "Clear all",
                        `select-all-text` = "Select all",
                        `none-selected-text` = "Choose site(s)",
                        `live-search` = TRUE,
                        style = "btn-outline-primary"
                      )),
          
          div(style = "margin-bottom: 15px;",
              pickerInput("stats_year", 
                          label = div(icon("calendar"), "Select Years:"),
                          choices = sort(unique(wl_stats$year)),
                          selected = c(2023, 2022, 2021, 2020),
                          multiple = TRUE,
                          options = list(
                            `actions-box` = TRUE,
                            `deselect-all-text` = "Clear all",
                            `select-all-text` = "Select all",
                            `none-selected-text` = "Choose year(s)",
                            `live-search` = TRUE,
                            style = "btn-outline-primary"
                          ))),
          
          div(style = "margin-bottom: 15px;",
              radioButtons("time_summary", "Summarize Stats:",
                           choices = c("Each Year" = "year", "Average Across Years" = "multi"),
                           selected = "year",
                           inline = TRUE)),
          
          br(),
          div(style = "padding: 15px; background-color: #e8f4f8; border-radius: 8px; border-left: 4px solid #3498db;",
              p(icon("info-circle"), " Growing Season statistics calculated from May through October.", 
                style = "margin: 0; font-size: 0.9rem; color: #2c3e50;")),
          
          div(style = "margin-top: 15px; text-align: center;",
              downloadButton("download_stats", "Download Table", 
                             class = "btn-primary btn-sm", 
                             icon = icon("download")))
        ),
        
        card(
          full_screen = TRUE,
          card_header(
            class = "bg-primary text-white",
            "Growing Season Water Level Statistics"
          ),
          div(style = "padding: 10px;",
              dataTableOutput("wl_stats"))
        )
      )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive data for plotting
  plot_data <- reactive({
    req(input$year, input$gm_site)
    
    all_data %>%
      filter(year %in% input$year, doy > 134, doy < 275) %>%
      filter(site %in% c(input$gm_site, "Gilmore Meadow"))
  })
  
  output$hydrograph <- renderPlot({
    plot_data_filtered <- plot_data()
    req(nrow(plot_data_filtered) > 0)
    
    # Calculate precipitation offset for each year
    precip_data <- plot_data_filtered %>%
      group_by(year) %>%
      summarise(
        min_water = min(c(
          water_depth[site == input$gm_site & !is.na(water_depth)],
          water_depth[site == "Gilmore Meadow" & !is.na(water_depth)]
        ), na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      left_join(plot_data_filtered, by = "year") %>%
      mutate(precip_y = lag_precip * 5 + min_water)
    
    global_min_water <- min(plot_data_filtered$water_depth, na.rm = TRUE)
    
    # Create the plot using facet_wrap instead of patchwork
    ggplot() +
      # Water level lines
      geom_line(data = plot_data_filtered %>% filter(site == input$gm_site),
                aes(x = doy_h, y = water_depth, color = input$gm_site), 
                size = 0.7) +
      geom_line(data = plot_data_filtered %>% filter(site == "Gilmore Meadow"),
                aes(x = doy_h, y = water_depth, color = "Gilmore Meadow"), 
                size = 0.7) +
      # Precipitation line
      geom_line(data = precip_data,
                aes(x = doy_h, y = precip_y, color = "Precipitation"), size = 0.7) +
      # Ground level reference
      geom_hline(yintercept = 0, color = "brown") +
      
      # Facet by year
      facet_wrap(~ year, ncol = 1, scales = "free_y") +
      
      # Styling
      scale_color_manual(values = c(
        setNames("black", input$gm_site),
        "Gilmore Meadow" = "darkgray",
        "Precipitation" = "blue"
      )) +
      labs(title = NULL,
           x = "Date", y = "Water Level (cm)") +
      scale_x_continuous(
        breaks = c(121, 152, 182, 213, 244, 274),
        labels = c('May-01', 'Jun-01', 'Jul-01', 'Aug-01', 'Sep-01', 'Oct-01')) +
      scale_y_continuous(
        name = "Water Level (cm)",
        sec.axis = sec_axis(~ (. - global_min_water) / 5,
                            name = "Hourly Precip. (cm)",
                            breaks = seq(0, 8, by = 2))) +
      theme_bw() +
      theme(legend.position = "bottom",
            legend.title = element_blank(),
            strip.text = element_text(size = 11),
            plot.title = element_text(hjust = 0.5))
  })
  
  # Selected data output table - Now with separate columns for each site
  output$brush_info <- renderTable({
    req(input$hydro_brush)
    
    plot_data_filtered <- plot_data()
    req(nrow(plot_data_filtered) > 0)
    
    # Use brushedPoints function which works properly with faceted plots
    selected_data <- brushedPoints(
      plot_data_filtered,
      brush = input$hydro_brush,
      xvar = "doy_h",
      yvar = "water_depth"
    ) %>%
      select(site, year, doy_h, timestamp, water_depth, lag_precip) %>%
      arrange(year, doy_h) %>%
      # Round for better display
      mutate(
        doy_h = round(doy_h, 2),
        water_depth = round(water_depth, 2),
        lag_precip = round(lag_precip, 3),
        timestamp = format(timestamp, "%Y-%m-%d %H:%M:%S")
      )
    
    if(nrow(selected_data) == 0) {
      data.frame(Message = "No points selected - try brushing over the water level lines")
    } else {
      # Create a proper time grouping to match data points from both sites
      # Group by rounded doy_h to combine data from both sites at similar times
      pivoted_data <- selected_data %>%
        # Round doy_h slightly to group nearby times together
        mutate(doy_h_rounded = round(doy_h, 1)) %>%
        # Group by the rounded time
        group_by(year, doy_h_rounded) %>%
        summarise(
          # Use the first timestamp and precipitation value in each group
          timestamp = first(timestamp),
          doy_h = first(doy_h),
          lag_precip = first(lag_precip),
          # Create separate columns for each site's water depth
          great_meadow_depth = ifelse(any(site == input$gm_site), 
                                      water_depth[site == input$gm_site][1], NA),
          gilmore_depth = ifelse(any(site == "Gilmore Meadow"), 
                                 water_depth[site == "Gilmore Meadow"][1], NA),
          .groups = 'drop'
        ) %>%
        # Remove the grouping column and rename for display
        select(-doy_h_rounded) %>%
        rename(
          `Year` = year, 
          `Timestamp` = timestamp,
          `Day of Year` = doy_h,
          `Precipitation (cm)` = lag_precip
        ) %>%
        # Create dynamic column names based on selected site
        rename_with(~ paste(input$gm_site, "Water Depth (cm)"), .cols = great_meadow_depth) %>%
        rename_with(~ "Gilmore Meadow Water Depth (cm)", .cols = gilmore_depth) %>%
        # Arrange by year and day
        arrange(Year, `Day of Year`)
      
      pivoted_data
    }
  })
  
  # setting up average WL stats and calculating stats output tables
  filtered_stats <- reactive({
    req(input$stats_site, input$stats_year, input$time_summary)
    
    data <- wl_stats %>%
      filter(site %in% input$stats_site, year %in% input$stats_year)
    
    if (input$time_summary == "year") {
      # ---- Option 1: Per-Year Summary ----
      data %>%
        mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
        select(
          Year = year,
          Site = site,
          `Mean Water Level (cm)` = WL_mean,
          `SD Water Level (cm)` = WL_sd,
          `Minimum Water Level (cm)` = WL_min,
          `Maximum Water Level (cm)` = WL_max,
          `Maximum Hourly Increase (cm)` = max_inc,
          `Maximum Hourly Decrease (cm)` = max_dec,
          `Growing Season Change (cm)` = GS_change,
          `GS % Surface Water` = prop_over_0cm,
          `GS % Within 30cm` = prop_bet_0_neg30cm,
          `GS % Over 30cm Deep` = prop_under_neg30cm
        )
    } else {
      # ---- Option 2: Average Across Years ----
      # Per-site summary
      data_site <- data %>%
        group_by(site, wetland) %>%
        summarise(
          Year = paste0(min(year), "–", max(year)),
          `Mean Water Level (cm)` = round(mean(WL_mean, na.rm = TRUE), 2),
          `SD Water Level (cm)` = round(mean(WL_sd, na.rm = TRUE), 2),
          `Minimum Water Level (cm)` = round(mean(WL_min, na.rm = TRUE), 2),
          `Maximum Water Level (cm)` = round(mean(WL_max, na.rm = TRUE), 2),
          `Maximum Hourly Increase (cm)` = round(mean(max_inc, na.rm = TRUE), 2),
          `Maximum Hourly Decrease (cm)` = round(mean(max_dec, na.rm = TRUE), 2),
          `Growing Season Change (cm)` = round(mean(GS_change, na.rm = TRUE), 2),
          `GS % Surface Water` = round(mean(prop_over_0cm, na.rm = TRUE), 2),
          `GS % Within 30cm` = round(mean(prop_bet_0_neg30cm, na.rm = TRUE), 2),
          `GS % Over 30cm Deep` = round(mean(prop_under_neg30cm, na.rm = TRUE), 2),
          .groups = "drop"
        )
      
      # Per-wetland "All Sites" summary
      data_wetland <- data %>%
        group_by(wetland) %>%
        summarise(
          site = "All Sites",
          Year = paste0(min(year), "–", max(year)),
          `Mean Water Level (cm)` = round(mean(WL_mean, na.rm = TRUE), 2),
          `SD Water Level (cm)` = round(mean(WL_sd, na.rm = TRUE), 2),
          `Minimum Water Level (cm)` = round(mean(WL_min, na.rm = TRUE), 2),
          `Maximum Water Level (cm)` = round(mean(WL_max, na.rm = TRUE), 2),
          `Maximum Hourly Increase (cm)` = round(mean(max_inc, na.rm = TRUE), 2),
          `Maximum Hourly Decrease (cm)` = round(mean(max_dec, na.rm = TRUE), 2),
          `Growing Season Change (cm)` = round(mean(GS_change, na.rm = TRUE), 2),
          `GS % Surface Water` = round(mean(prop_over_0cm, na.rm = TRUE), 2),
          `GS % Within 30cm` = round(mean(prop_bet_0_neg30cm, na.rm = TRUE), 2),
          `GS % Over 30cm Deep` = round(mean(prop_under_neg30cm, na.rm = TRUE), 2),
          .groups = "drop"
        )
      
      bind_rows(data_site, data_wetland) %>%
        distinct() %>% 
        rename(Site = site,
               Wetland = wetland)
    }
  })
  
  # WL stats table output
  output$wl_stats <- DT::renderDataTable({
    datatable(
      filtered_stats(),
      options = list(pageLength = 10, scrollX = TRUE)
    ) %>%
      formatStyle(
        'Site',
        target = 'cell',
        fontWeight = styleEqual("All Sites", "bold")
      )
  })
  
  
  # Download handler for brushed data
  output$download_brush <- downloadHandler(
    filename = function() {
      paste("hydrograph_selected_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      # Get the brushed data (you'll need this reactive from your existing brush logic)
      brushed_data <- brushedPoints(plot_data(), input$hydro_brush,
                                    xvar = "doy_h", yvar = "water_depth")
      write.csv(brushed_data, file, row.names = FALSE)
    }
  )
  
  # Download handler for water level stats
  output$download_stats <- downloadHandler(
    filename = function() {
      if (input$time_summary == "multi") {
        paste("wetland_averaged_stats_", Sys.Date(), ".csv", sep = "")
      } else {
        paste("site_stats_", Sys.Date(), ".csv", sep = "")
      }
    },
    content = function(file) {
      write.csv(filtered_stats(), file, row.names = FALSE)
    }
  )
  
}

# Run app
shinyApp(ui, server)






#-------------------------------------------------------------------------------




# creates average wl summary by site and wetland using a toggle tool
library(shiny)
library(tidyverse)
library(lubridate)
library(patchwork)
library(shinyWidgets)
library(DT)
library(bslib)

# Read & prepare your processed data (from your current script)
gm <- read.csv("data/processed_data/great_meadow_well_data_2024_20250715.csv") %>%
  rename(Date = date, Year = year, precip_cm = precip.cm) %>%
  mutate(Date = as.Date(Date),
         timestamp = as_datetime(timestamp),
         site = paste("Great Meadow", plot.num),
         water.depth = case_when(
           Year == 2016 & doy_h == 159.12 & plot.num == 3 & water.depth < -120 ~ NA_real_,
           Year == 2017 & doy_h == 215.02 & plot.num == 6 & water.depth < -115 ~ NA_real_,
           Year == 2021 & plot.num == 3 & doy == 224 & water.depth > 400 ~ NA_real_,
           Year == 2021 & plot.num == 3 & doy == 225 & water.depth > 400 ~ NA_real_,
           TRUE ~ water.depth
         )) %>%
  mutate(siteyear = paste(site, Year, sep = "_"))

gl <- read.csv("data/raw_data/hydrology_data/gilmore_well_prec_data_2013-2024.csv") %>%
  rename(water.depth = GILM_WL) %>%
  mutate(
    site = "Gilmore Meadow",
    
    # Try to parse full datetime, fallback to date only + midnight
    timestamp_parsed = mdy_hm(timestamp),
    timestamp_parsed = if_else(
      is.na(timestamp_parsed),
      mdy(timestamp),
      timestamp_parsed
    ),
    timestamp_parsed = as.POSIXct(timestamp_parsed),
    timestamp = timestamp_parsed,
    
    # Standardize Date column too
    Date = mdy(Date)
  ) %>%
  select(timestamp, 
         Date,
         doy,
         Year,
         precip_cm,
         water.depth,
         lag.precip,
         hr,
         doy_h,
         site)

all_data <- bind_rows(gm, gl) %>% 
  filter(Year >= 2016 & Year <= 2023) %>% 
  select(timestamp, 
         date = Date, 
         year = Year, 
         doy, 
         hr, 
         doy_h, 
         lag_precip = lag.precip,
         water_depth = water.depth,
         site)

wl_stats <- read.csv("data/processed_data/gm_gl_wl_stats.csv") %>% 
  select(year,
         stat, 
         `Gilmore Meadow` = gilmore.meadow, 
         `Great Meadow 1` = great.meadow.1, 
         `Great Meadow 2` = great.meadow.2, 
         `Great Meadow 3` = great.meadow.3, 
         `Great Meadow 4` = great.meadow.4, 
         `Great Meadow 5` = great.meadow.5, 
         `Great Meadow 6` = great.meadow.6) %>% 
  pivot_longer(cols = -c(year, stat), names_to = "site", values_to = "value") %>% 
  pivot_wider(names_from = stat, values_from = value) %>%
  arrange(site, year) %>% 
  mutate(
    wetland = case_when(
      grepl("Great Meadow", site, ignore.case = TRUE) ~ "Great Meadow",
      grepl("Gilmore Meadow", site, ignore.case = TRUE) ~ "Gilmore Meadow",
      TRUE ~ "Unknown"
    )
  )


# UI 
ui <- page_fluid(
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#1B365D",
    secondary = "#4C6D9A",    
    success = "#2E86C1",
    info = "#3498db",
    warning = "#f39c12",
    danger = "#e74c3c",
    base_font = font_google("Open Sans"),
    heading_font = font_google("Roboto", wght = c(400, 700))
  ),
  
  # Custom CSS for additional styling
  tags$head(
    tags$style(HTML("
      .content-section {
        margin: 30px 0;
        padding: 25px;
        border-radius: 15px;
        box-shadow: 0 4px 12px rgba(0,0,0,0.1);
        background: linear-gradient(135deg, #ffffff 0%, #f8f9fa 100%);
        border: 2px solid #1B365D;
      }
      
      .section-title {
        color: #1B365D;
        font-weight: 600;
        margin-bottom: 20px;
        padding-bottom: 10px;
        border-bottom: 2px solid #4C6D9A;
      }
      
      .sidebar-custom {
        background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%);
        border-radius: 10px;
        padding: 20px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.08);
        border: 1px solid #dee2e6;
      }
      
      .main-title {
        background: linear-gradient(135deg, #1B365D 0%, #4C6D9A 100%);
        color: white;
        padding: 30px;
        margin: -15px -15px 30px -15px;
        text-align: center;
        border-radius: 0 0 20px 20px;
        box-shadow: 0 4px 12px rgba(0,0,0,0.15);
      }
      
      .brush-info-section {
        background: linear-gradient(135deg, #e8f4f8 0%, #f0f8ff 100%);
        border-radius: 12px;
        padding: 20px;
        margin: 20px 0;
        border: 1px solid #4C6D9A;
      }
      
      .stats-main {
        background: linear-gradient(135deg, #f8f9fa 0%, #ffffff 100%);
      }
      
      .dataTables_wrapper {
        font-size: 0.85rem !important;
      }

      .dataTables_wrapper table {
        font-size: 0.8rem !important;
      }

      .dataTables_wrapper .dataTables_info,
      .dataTables_wrapper .dataTables_paginate {
        font-size: 0.75rem !important;
      }
      
    "))
  ),
  
  # Main title with gradient background
  div(class = "main-title",
      h1("Wetland Hydrograph Visualizer", 
         style = "margin: 0; font-size: 2rem; text-shadow: 2px 2px 4px rgba(0,0,0,0.3)")
  ),
  
  # First section: Hydrographs 
  div(class = "content-section",
      layout_sidebar(
        sidebar = sidebar(
          class = "sidebar-custom",
          width = 300,
          h4("Hydrograph Controls", style = "color: #1B365D; margin-bottom: 20px;"),
          
          selectInput("gm_site", 
                      label = div(icon("map-marker"), "Select a Great Meadow Site:"),
                      choices = sort(unique(all_data$site[grepl("Great Meadow", all_data$site)])),
                      selected = "Great Meadow 1"),
          
          div(style = "margin-bottom: 15px;",
          pickerInput("year", 
                      label = div(icon("calendar"), "Select Year(s):"),
                      choices = sort(unique(all_data$year)),
                      selected = 2023,
                      multiple = TRUE,
                      options = list(
                        `actions-box` = TRUE,
                        `deselect-all-text` = "Clear all",
                        `select-all-text` = "Select all",
                        `none-selected-text` = "Choose year(s)",
                        `live-search` = TRUE,
                        style = "btn-outline-primary"
                      ))),
          
          br(),
          div(style = "padding: 15px; background-color: #e8f4f8; border-radius: 8px; border-left: 4px solid #3498db;",
              p(icon("info-circle"), " Use the brush tool to select data points on the hydrograph for a detailed view of the data.", 
                style = "margin: 0; font-size: 0.9rem; color: #2c3e50;")),
          
          div(style = "margin-top: 15px; text-align: center;",
              downloadButton("download_brush", "Download Selected Data", 
                             class = "btn-primary btn-sm", 
                             icon = icon("download")))
        ),
        
        card(
          full_screen = TRUE,
          card_header(
            class = "bg-primary text-white",
            "Hydrographs by Year"
          ),
          plotOutput("hydrograph", height = "600px", 
                     brush = brushOpts(id = "hydro_brush", fill = "#4C6D9A", opacity = 0.3))
        )
      )
  ),
  
  # Selected data points section with improved styling
  div(class = "brush-info-section",
      div(class = "row",
          div(class = "col-12",
              card(
                card_header(
                  class = "bg-success text-white",
                  "Selected Data from Hydrograph:"
                ),
                tableOutput("brush_info")
              )
          )
      )
  ),
  
  # Second section: Water Level Stats with improved styling
  div(class = "content-section stats-main",
      layout_sidebar(
        sidebar = sidebar(
          class = "sidebar-custom",
          width = 300,
          h4("Statistics Controls", style = "color: #1B365D; margin-bottom: 20px;"),
          
          pickerInput("stats_site", 
                      label = div(icon("map-marker"), "Select Site(s):"),
                      choices = sort(unique(wl_stats$site)),
                      selected = "Great Meadow 1",
                      multiple = TRUE,
                      options = list(
                        `actions-box` = TRUE,
                        `deselect-all-text` = "Clear all",
                        `select-all-text` = "Select all",
                        `none-selected-text` = "Choose site(s)",
                        `live-search` = TRUE,
                        style = "btn-outline-primary"
                      )),
          
          div(style = "margin-bottom: 15px;",
          pickerInput("stats_year", 
                      label = div(icon("calendar"), "Select Years:"),
                      choices = sort(unique(wl_stats$year)),
                      selected = c(2023, 2022),
                      multiple = TRUE,
                      options = list(
                        `actions-box` = TRUE,
                        `deselect-all-text` = "Clear all",
                        `select-all-text` = "Select all",
                        `none-selected-text` = "Choose year(s)",
                        `live-search` = TRUE,
                        style = "btn-outline-primary"
                      ))),
          
          div(style = "margin-bottom: 15px;",
          radioButtons("summary_level", "Summary Type:",
                       choices = c("By Site" = "site", "Average by Wetland" = "wetland"),
                       selected = "site",
                       inline = TRUE)),
          
          br(),
          div(style = "padding: 15px; background-color: #e8f4f8; border-radius: 8px; border-left: 4px solid #3498db;",
              p(icon("info-circle"), " Growing Season statistics calculated from May through October.", 
                style = "margin: 0; font-size: 0.9rem; color: #2c3e50;")),
          
          div(style = "margin-top: 15px; text-align: center;",
              downloadButton("download_stats", "Download Table", 
                             class = "btn-primary btn-sm", 
                             icon = icon("download")))
        ),
        
        card(
          full_screen = TRUE,
          card_header(
            class = "bg-primary text-white",
            "Growing Season Water Level Statistics"
          ),
          div(style = "padding: 10px;",
              dataTableOutput("wl_stats"))
        )
      )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive data for plotting
  plot_data <- reactive({
    req(input$year, input$gm_site)
    
    all_data %>%
      filter(year %in% input$year, doy > 134, doy < 275) %>%
      filter(site %in% c(input$gm_site, "Gilmore Meadow"))
  })
  
  output$hydrograph <- renderPlot({
    plot_data_filtered <- plot_data()
    req(nrow(plot_data_filtered) > 0)
    
    # Calculate precipitation offset for each year
    precip_data <- plot_data_filtered %>%
      group_by(year) %>%
      summarise(
        min_water = min(c(
          water_depth[site == input$gm_site & !is.na(water_depth)],
          water_depth[site == "Gilmore Meadow" & !is.na(water_depth)]
        ), na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      left_join(plot_data_filtered, by = "year") %>%
      mutate(precip_y = lag_precip * 5 + min_water)
    
    global_min_water <- min(plot_data_filtered$water_depth, na.rm = TRUE)
    
    # Create the plot using facet_wrap instead of patchwork
    ggplot() +
      # Water level lines
      geom_line(data = plot_data_filtered %>% filter(site == input$gm_site),
                aes(x = doy_h, y = water_depth, color = input$gm_site), 
                size = 0.7) +
      geom_line(data = plot_data_filtered %>% filter(site == "Gilmore Meadow"),
                aes(x = doy_h, y = water_depth, color = "Gilmore Meadow"), 
                size = 0.7) +
      # Precipitation line
      geom_line(data = precip_data,
                aes(x = doy_h, y = precip_y, color = "Precipitation"), size = 0.7) +
      # Ground level reference
      geom_hline(yintercept = 0, color = "brown") +
      
      # Facet by year
      facet_wrap(~ year, ncol = 1, scales = "free_y") +
      
      # Styling
      scale_color_manual(values = c(
        setNames("black", input$gm_site),
        "Gilmore Meadow" = "darkgray",
        "Precipitation" = "blue"
      )) +
      labs(title = NULL,
           x = "Date", y = "Water Level (cm)") +
      scale_x_continuous(
        breaks = c(121, 152, 182, 213, 244, 274),
        labels = c('May-01', 'Jun-01', 'Jul-01', 'Aug-01', 'Sep-01', 'Oct-01')) +
      scale_y_continuous(
        name = "Water Level (cm)",
        sec.axis = sec_axis(~ (. - global_min_water) / 5,
                            name = "Hourly Precip. (cm)",
                            breaks = seq(0, 8, by = 2))) +
      theme_bw() +
      theme(legend.position = "bottom",
            legend.title = element_blank(),
            strip.text = element_text(size = 11),
            plot.title = element_text(hjust = 0.5))
  })
  
  # Selected data output table - Now with separate columns for each site
  output$brush_info <- renderTable({
    req(input$hydro_brush)
    
    plot_data_filtered <- plot_data()
    req(nrow(plot_data_filtered) > 0)
    
    # Use brushedPoints function which works properly with faceted plots
    selected_data <- brushedPoints(
      plot_data_filtered,
      brush = input$hydro_brush,
      xvar = "doy_h",
      yvar = "water_depth"
    ) %>%
      select(site, year, doy_h, timestamp, water_depth, lag_precip) %>%
      arrange(year, doy_h) %>%
      # Round for better display
      mutate(
        doy_h = round(doy_h, 2),
        water_depth = round(water_depth, 2),
        lag_precip = round(lag_precip, 3),
        timestamp = format(timestamp, "%Y-%m-%d %H:%M:%S")
      )
    
    if(nrow(selected_data) == 0) {
      data.frame(Message = "No points selected - try brushing over the water level lines")
    } else {
      # Create a proper time grouping to match data points from both sites
      # Group by rounded doy_h to combine data from both sites at similar times
      pivoted_data <- selected_data %>%
        # Round doy_h slightly to group nearby times together
        mutate(doy_h_rounded = round(doy_h, 1)) %>%
        # Group by the rounded time
        group_by(year, doy_h_rounded) %>%
        summarise(
          # Use the first timestamp and precipitation value in each group
          timestamp = first(timestamp),
          doy_h = first(doy_h),
          lag_precip = first(lag_precip),
          # Create separate columns for each site's water depth
          great_meadow_depth = ifelse(any(site == input$gm_site), 
                                      water_depth[site == input$gm_site][1], NA),
          gilmore_depth = ifelse(any(site == "Gilmore Meadow"), 
                                 water_depth[site == "Gilmore Meadow"][1], NA),
          .groups = 'drop'
        ) %>%
        # Remove the grouping column and rename for display
        select(-doy_h_rounded) %>%
        rename(
          `Year` = year, 
          `Timestamp` = timestamp,
          `Day of Year` = doy_h,
          `Precipitation (cm)` = lag_precip
        ) %>%
        # Create dynamic column names based on selected site
        rename_with(~ paste(input$gm_site, "Water Depth (cm)"), .cols = great_meadow_depth) %>%
        rename_with(~ "Gilmore Meadow Water Depth (cm)", .cols = gilmore_depth) %>%
        # Arrange by year and day
        arrange(Year, `Day of Year`)
      
      pivoted_data
    }
  })
  
  # setting up average WL stats and calculating stats output tables
  filtered_stats <- reactive({
    req(input$stats_site, input$stats_year)
    
    data <- wl_stats %>%
      filter(site %in% input$stats_site, year %in% input$stats_year)
    
    if (input$summary_level == "wetland") {
      data %>%
        group_by(wetland) %>%
        summarise(
          Year = paste0(min(year), "–", max(year)),
          `Mean Water Level` = mean(WL_mean, na.rm = TRUE),
          `SD Water Level` = mean(WL_sd, na.rm = TRUE),
          `Minimum Water Level` = mean(WL_min, na.rm = TRUE),
          `Maximum Water Level` = mean(WL_max, na.rm = TRUE),
          `Maximum Hourly Increase` = mean(max_inc, na.rm = TRUE),
          `Maximum Hourly Decrease` = mean(max_dec, na.rm = TRUE),
          `Growing Season Change` = mean(GS_change, na.rm = TRUE),
          `GS % Surface Water` = mean(prop_over_0cm, na.rm = TRUE),
          `GS % Within 30cm` = mean(prop_bet_0_neg30cm, na.rm = TRUE),
          `GS % Over 30cm Deep` = mean(prop_under_neg30cm, na.rm = TRUE),
          `GS % Complete Data` = mean(prop_GS_comp, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
        rename(Wetland = wetland)
    } else {
      data %>%
        mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
        select(
          Year = year,
          Site = site,
          `Mean Water Level` = WL_mean,
          `SD Water Level` = WL_sd,
          `Minimum Water Level` = WL_min,
          `Maximum Water Level` = WL_max,
          `Maximum Hourly Increase` = max_inc,
          `Maximum Hourly Decrease` = max_dec,
          `Growing Season Change` = GS_change,
          `GS % Surface Water` = prop_over_0cm,
          `GS % Within 30cm` = prop_bet_0_neg30cm,
          `GS % Over 30cm Deep` = prop_under_neg30cm,
          `GS % Complete Data` = prop_GS_comp
        )
    }
  })
  
  # WL stats table output
  output$wl_stats <- DT::renderDataTable({
    filtered_stats()
  }, options = list(pageLength = 10, scrollX = TRUE))
  
  
  # Download handler for brushed data
  output$download_brush <- downloadHandler(
    filename = function() {
      paste("hydrograph_selected_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      # Get the brushed data (you'll need this reactive from your existing brush logic)
      brushed_data <- brushedPoints(plot_data(), input$hydro_brush,
                                    xvar = "doy_h", yvar = "water_depth")
      write.csv(brushed_data, file, row.names = FALSE)
    }
  )
  
  # Download handler for water level stats
  output$download_stats <- downloadHandler(
    filename = function() {
      if (input$summary_level == "wetland") {
        paste("wetland_averaged_stats_", Sys.Date(), ".csv", sep = "")
      } else {
        paste("site_stats_", Sys.Date(), ".csv", sep = "")
      }
    },
    content = function(file) {
      write.csv(filtered_stats(), file, row.names = FALSE)
    }
  )
  
}

# Run app
shinyApp(ui, server)





#-------------------------------------------------------------------------------


# creates average wl summary stats by site
library(shiny)
library(tidyverse)
library(lubridate)
library(patchwork)
library(shinyWidgets)
library(DT)
library(bslib)

# Read & prepare your processed data (from your current script)
gm <- read.csv("data/processed_data/great_meadow_well_data_2024_20250715.csv") %>%
  rename(Date = date, Year = year, precip_cm = precip.cm) %>%
  mutate(Date = as.Date(Date),
         timestamp = as_datetime(timestamp),
         site = paste("Great Meadow", plot.num),
         water.depth = case_when(
           Year == 2016 & doy_h == 159.12 & plot.num == 3 & water.depth < -120 ~ NA_real_,
           Year == 2017 & doy_h == 215.02 & plot.num == 6 & water.depth < -115 ~ NA_real_,
           Year == 2021 & plot.num == 3 & doy == 224 & water.depth > 400 ~ NA_real_,
           Year == 2021 & plot.num == 3 & doy == 225 & water.depth > 400 ~ NA_real_,
           TRUE ~ water.depth
         )) %>%
  mutate(siteyear = paste(site, Year, sep = "_"))

gl <- read.csv("data/raw_data/hydrology_data/gilmore_well_prec_data_2013-2024.csv") %>%
  rename(water.depth = GILM_WL) %>%
  mutate(
    site = "Gilmore Meadow",
    
    # Try to parse full datetime, fallback to date only + midnight
    timestamp_parsed = mdy_hm(timestamp),
    timestamp_parsed = if_else(
      is.na(timestamp_parsed),
      mdy(timestamp),
      timestamp_parsed
    ),
    timestamp_parsed = as.POSIXct(timestamp_parsed),
    timestamp = timestamp_parsed,
    
    # Standardize Date column too
    Date = mdy(Date)
  ) %>%
  select(timestamp, 
         Date,
         doy,
         Year,
         precip_cm,
         water.depth,
         lag.precip,
         hr,
         doy_h,
         site)

all_data <- bind_rows(gm, gl) %>% 
  filter(Year >= 2016 & Year <= 2023) %>% 
  select(timestamp, 
         date = Date, 
         year = Year, 
         doy, 
         hr, 
         doy_h, 
         lag_precip = lag.precip,
         water_depth = water.depth,
         site)

wl_stats <- read.csv("data/processed_data/gm_gl_wl_stats.csv") %>% 
  select(year,
         stat, 
         `Gilmore Meadow` = gilmore.meadow, 
         `Great Meadow 1` = great.meadow.1, 
         `Great Meadow 2` = great.meadow.2, 
         `Great Meadow 3` = great.meadow.3, 
         `Great Meadow 4` = great.meadow.4, 
         `Great Meadow 5` = great.meadow.5, 
         `Great Meadow 6` = great.meadow.6) %>% 
  pivot_longer(cols = -c(year, stat), names_to = "site", values_to = "value") %>% 
  pivot_wider(names_from = stat, values_from = value) %>%
  arrange(site, year)


# UI 
ui <- page_fluid(
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#1B365D",
    secondary = "#4C6D9A",    
    success = "#2E86C1",
    info = "#3498db",
    warning = "#f39c12",
    danger = "#e74c3c",
    base_font = font_google("Open Sans"),
    heading_font = font_google("Roboto", wght = c(400, 700))
  ),
  
  # Custom CSS for additional styling
  tags$head(
    tags$style(HTML("
      .content-section {
        margin: 30px 0;
        padding: 25px;
        border-radius: 15px;
        box-shadow: 0 4px 12px rgba(0,0,0,0.1);
        background: linear-gradient(135deg, #ffffff 0%, #f8f9fa 100%);
        border: 2px solid #1B365D;
      }
      
      .section-title {
        color: #1B365D;
        font-weight: 600;
        margin-bottom: 20px;
        padding-bottom: 10px;
        border-bottom: 2px solid #4C6D9A;
      }
      
      .sidebar-custom {
        background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%);
        border-radius: 10px;
        padding: 20px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.08);
        border: 1px solid #dee2e6;
      }
      
      .main-title {
        background: linear-gradient(135deg, #1B365D 0%, #4C6D9A 100%);
        color: white;
        padding: 30px;
        margin: -15px -15px 30px -15px;
        text-align: center;
        border-radius: 0 0 20px 20px;
        box-shadow: 0 4px 12px rgba(0,0,0,0.15);
      }
      
      .brush-info-section {
        background: linear-gradient(135deg, #e8f4f8 0%, #f0f8ff 100%);
        border-radius: 12px;
        padding: 20px;
        margin: 20px 0;
        border: 1px solid #4C6D9A;
      }
      
      .stats-main {
        background: linear-gradient(135deg, #f8f9fa 0%, #ffffff 100%);
      }
      
      .dataTables_wrapper {
        font-size: 0.85rem !important;
      }

      .dataTables_wrapper table {
        font-size: 0.8rem !important;
      }

      .dataTables_wrapper .dataTables_info,
      .dataTables_wrapper .dataTables_paginate {
        font-size: 0.75rem !important;
      }
      
    "))
  ),
  
  # Main title with gradient background
  div(class = "main-title",
      h1("Wetland Hydrograph Visualizer", 
         style = "margin: 0; font-size: 2rem; text-shadow: 2px 2px 4px rgba(0,0,0,0.3)")
  ),
  
  # First section: Hydrographs 
  div(class = "content-section",
      layout_sidebar(
        sidebar = sidebar(
          class = "sidebar-custom",
          width = 300,
          h4("Hydrograph Controls", style = "color: #1B365D; margin-bottom: 20px;"),
          
          selectInput("gm_site", 
                      label = div(icon("map-marker"), "Select a Great Meadow Site:"),
                      choices = sort(unique(all_data$site[grepl("Great Meadow", all_data$site)])),
                      selected = "Great Meadow 1"),
          
          div(style = "margin-bottom: 15px;",
              pickerInput("year", 
                          label = div(icon("calendar"), "Select Year(s):"),
                          choices = sort(unique(all_data$year)),
                          selected = 2023,
                          multiple = TRUE,
                          options = list(
                            `actions-box` = TRUE,
                            `deselect-all-text` = "Clear all",
                            `select-all-text` = "Select all",
                            `none-selected-text` = "Choose year(s)",
                            `live-search` = TRUE,
                            style = "btn-outline-primary"
                          ))),
          
          br(),
          div(style = "padding: 15px; background-color: #e8f4f8; border-radius: 8px; border-left: 4px solid #3498db;",
              p(icon("info-circle"), " Use the brush tool to select data points on the hydrograph for a detailed view of the data.", 
                style = "margin: 0; font-size: 0.9rem; color: #2c3e50;")),
          
          div(style = "margin-top: 15px; text-align: center;",
              downloadButton("download_brush", "Download Selected Data", 
                             class = "btn-primary btn-sm", 
                             icon = icon("download")))
        ),
        
        card(
          full_screen = TRUE,
          card_header(
            class = "bg-primary text-white",
            "Hydrographs by Year"
          ),
          plotOutput("hydrograph", height = "600px", 
                     brush = brushOpts(id = "hydro_brush", fill = "#4C6D9A", opacity = 0.3))
        )
      )
  ),
  
  # Selected data points section with improved styling
  div(class = "brush-info-section",
      div(class = "row",
          div(class = "col-12",
              card(
                card_header(
                  class = "bg-success text-white",
                  "Selected Data from Hydrograph:"
                ),
                tableOutput("brush_info")
              )
          )
      )
  ),
  
  # Second section: Water Level Stats with improved styling
  div(class = "content-section stats-main",
      layout_sidebar(
        sidebar = sidebar(
          class = "sidebar-custom",
          width = 300,
          h4("Statistics Controls", style = "color: #1B365D; margin-bottom: 20px;"),
          
          pickerInput("stats_site", 
                      label = div(icon("map-marker"), "Select Site(s):"),
                      choices = sort(unique(wl_stats$site)),
                      selected = "Great Meadow 1",
                      multiple = TRUE,
                      options = list(
                        `actions-box` = TRUE,
                        `deselect-all-text` = "Clear all",
                        `select-all-text` = "Select all",
                        `none-selected-text` = "Choose site(s)",
                        `live-search` = TRUE,
                        style = "btn-outline-primary"
                      )),
          
          div(style = "margin-bottom: 15px;",
              pickerInput("stats_year", 
                          label = div(icon("calendar"), "Select Years:"),
                          choices = sort(unique(wl_stats$year)),
                          selected = c(2023, 2022),
                          multiple = TRUE,
                          options = list(
                            `actions-box` = TRUE,
                            `deselect-all-text` = "Clear all",
                            `select-all-text` = "Select all",
                            `none-selected-text` = "Choose year(s)",
                            `live-search` = TRUE,
                            style = "btn-outline-primary"
                          ))),
          
          br(),
          div(style = "padding: 15px; background-color: #e8f4f8; border-radius: 8px; border-left: 4px solid #3498db;",
              p(icon("info-circle"), " Growing Season statistics calculated from May through October.", 
                style = "margin: 0; font-size: 0.9rem; color: #2c3e50;")),
          
          div(style = "margin-top: 15px; text-align: center;",
              downloadButton("download_stats", "Download Table", 
                             class = "btn-primary btn-sm", 
                             icon = icon("download"))),
          br(),
          radioButtons("stats_summary_type", "Summary Type:",
                       choices = c("By Year" = "yearly", "Average Across Years" = "average"),
                       inline = TRUE)
          
        ),
        
        card(
          full_screen = TRUE,
          card_header(
            class = "bg-primary text-white",
            "Growing Season Water Level Statistics"
          ),
          div(style = "padding: 10px;",
              dataTableOutput("wl_stats"))
        )
      )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive data for plotting
  plot_data <- reactive({
    req(input$year, input$gm_site)
    
    all_data %>%
      filter(year %in% input$year, doy > 134, doy < 275) %>%
      filter(site %in% c(input$gm_site, "Gilmore Meadow"))
  })
  
  output$hydrograph <- renderPlot({
    plot_data_filtered <- plot_data()
    req(nrow(plot_data_filtered) > 0)
    
    # Calculate precipitation offset for each year
    precip_data <- plot_data_filtered %>%
      group_by(year) %>%
      summarise(
        min_water = min(c(
          water_depth[site == input$gm_site & !is.na(water_depth)],
          water_depth[site == "Gilmore Meadow" & !is.na(water_depth)]
        ), na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      left_join(plot_data_filtered, by = "year") %>%
      mutate(precip_y = lag_precip * 5 + min_water)
    
    global_min_water <- min(plot_data_filtered$water_depth, na.rm = TRUE)
    
    # Create the plot using facet_wrap instead of patchwork
    ggplot() +
      # Water level lines
      geom_line(data = plot_data_filtered %>% filter(site == input$gm_site),
                aes(x = doy_h, y = water_depth, color = input$gm_site), 
                size = 0.7) +
      geom_line(data = plot_data_filtered %>% filter(site == "Gilmore Meadow"),
                aes(x = doy_h, y = water_depth, color = "Gilmore Meadow"), 
                size = 0.7) +
      # Precipitation line
      geom_line(data = precip_data,
                aes(x = doy_h, y = precip_y, color = "Precipitation"), size = 0.7) +
      # Ground level reference
      geom_hline(yintercept = 0, color = "brown") +
      
      # Facet by year
      facet_wrap(~ year, ncol = 1, scales = "free_y") +
      
      # Styling
      scale_color_manual(values = c(
        setNames("black", input$gm_site),
        "Gilmore Meadow" = "darkgray",
        "Precipitation" = "blue"
      )) +
      labs(title = NULL,
           x = "Date", y = "Water Level (cm)") +
      scale_x_continuous(
        breaks = c(121, 152, 182, 213, 244, 274),
        labels = c('May-01', 'Jun-01', 'Jul-01', 'Aug-01', 'Sep-01', 'Oct-01')) +
      scale_y_continuous(
        name = "Water Level (cm)",
        sec.axis = sec_axis(~ (. - global_min_water) / 5,
                            name = "Hourly Precip. (cm)",
                            breaks = seq(0, 8, by = 2))) +
      theme_bw() +
      theme(legend.position = "bottom",
            legend.title = element_blank(),
            strip.text = element_text(size = 11),
            plot.title = element_text(hjust = 0.5))
  })
  
  # Selected data output table - Now with separate columns for each site
  output$brush_info <- renderTable({
    req(input$hydro_brush)
    
    plot_data_filtered <- plot_data()
    req(nrow(plot_data_filtered) > 0)
    
    # Use brushedPoints function which works properly with faceted plots
    selected_data <- brushedPoints(
      plot_data_filtered,
      brush = input$hydro_brush,
      xvar = "doy_h",
      yvar = "water_depth"
    ) %>%
      select(site, year, doy_h, timestamp, water_depth, lag_precip) %>%
      arrange(year, doy_h) %>%
      # Round for better display
      mutate(
        doy_h = round(doy_h, 2),
        water_depth = round(water_depth, 2),
        lag_precip = round(lag_precip, 3),
        timestamp = format(timestamp, "%Y-%m-%d %H:%M:%S")
      )
    
    if(nrow(selected_data) == 0) {
      data.frame(Message = "No points selected - try brushing over the water level lines")
    } else {
      # Create a proper time grouping to match data points from both sites
      # Group by rounded doy_h to combine data from both sites at similar times
      pivoted_data <- selected_data %>%
        # Round doy_h slightly to group nearby times together
        mutate(doy_h_rounded = round(doy_h, 1)) %>%
        # Group by the rounded time
        group_by(year, doy_h_rounded) %>%
        summarise(
          # Use the first timestamp and precipitation value in each group
          timestamp = first(timestamp),
          doy_h = first(doy_h),
          lag_precip = first(lag_precip),
          # Create separate columns for each site's water depth
          great_meadow_depth = ifelse(any(site == input$gm_site), 
                                      water_depth[site == input$gm_site][1], NA),
          gilmore_depth = ifelse(any(site == "Gilmore Meadow"), 
                                 water_depth[site == "Gilmore Meadow"][1], NA),
          .groups = 'drop'
        ) %>%
        # Remove the grouping column and rename for display
        select(-doy_h_rounded) %>%
        rename(
          `Year` = year, 
          `Timestamp` = timestamp,
          `Day of Year` = doy_h,
          `Precipitation (cm)` = lag_precip
        ) %>%
        # Create dynamic column names based on selected site
        rename_with(~ paste(input$gm_site, "Water Depth (cm)"), .cols = great_meadow_depth) %>%
        rename_with(~ "Gilmore Meadow Water Depth (cm)", .cols = gilmore_depth) %>%
        # Arrange by year and day
        arrange(Year, `Day of Year`)
      
      pivoted_data
    }
  })
  
  # setting up average WL stats and WL average stats
  filtered_stats <- reactive({
    req(input$stats_site, input$stats_year)
    
    data <- wl_stats %>%
      filter(site %in% input$stats_site, year %in% input$stats_year)
    
    if (input$stats_summary_type == "average") {
      data <- data %>%
        group_by(site) %>%
        summarise(
          year = paste0(min(year), "–", max(year)),
          WL_mean = mean(WL_mean, na.rm = TRUE),
          WL_sd = mean(WL_sd, na.rm = TRUE),
          WL_min = mean(WL_min, na.rm = TRUE),
          WL_max = mean(WL_max, na.rm = TRUE),
          max_inc = mean(max_inc, na.rm = TRUE),
          max_dec = mean(max_dec, na.rm = TRUE),
          GS_change = mean(GS_change, na.rm = TRUE),
          prop_over_0cm = mean(prop_over_0cm, na.rm = TRUE),
          prop_bet_0_neg30cm = mean(prop_bet_0_neg30cm, na.rm = TRUE),
          prop_under_neg30cm = mean(prop_under_neg30cm, na.rm = TRUE),
          prop_GS_comp = mean(prop_GS_comp, na.rm = TRUE),
          .groups = "drop"
        )
    }
    
    data
  })
  
  
  # WL stats output table
  output$wl_stats <- DT::renderDataTable({
    filtered_stats() %>%
      mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
      select(
        Year = year,
        Site = site,
        `Mean Water Level` = WL_mean,
        `SD Water Level` = WL_sd,
        `Minimum Water Level` = WL_min,
        `Maximum Water Level` = WL_max,
        `Maximum Hourly Increase` = max_inc,
        `Maximum Hourly Decrease` = max_dec,
        `Growing Season Change` = GS_change,
        `GS % Surface Water` = prop_over_0cm,
        `GS % Within 30cm` = prop_bet_0_neg30cm,
        `GS % Over 30cm Deep` = prop_under_neg30cm,
        `GS % Complete Data` = prop_GS_comp
      )
  }, options = list(pageLength = 10, scrollX = TRUE))
  
  
  # Download handler for brushed data
  output$download_brush <- downloadHandler(
    filename = function() {
      paste("hydrograph_selected_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      # Get the brushed data (you'll need this reactive from your existing brush logic)
      brushed_data <- brushedPoints(plot_data(), input$hydro_brush,
                                    xvar = "doy_h", yvar = "water_depth")
      write.csv(brushed_data, file, row.names = FALSE)
    }
  )
  
  # Download handler for water level stats
  output$download_stats <- downloadHandler(
    filename = function() {
      paste("water_level_stats_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filtered_stats(), file, row.names = FALSE)
    }
  )
  
  
}

# Run app
shinyApp(ui, server)



#-------------------------------------------------------------------------------


library(shiny)
library(tidyverse)
library(lubridate)
library(patchwork)
library(shinyWidgets)
library(DT)
library(bslib)

# Read & prepare your processed data (from your current script)
gm <- read.csv("data/processed_data/great_meadow_well_data_2024_20250715.csv") %>%
  rename(Date = date, Year = year, precip_cm = precip.cm) %>%
  mutate(Date = as.Date(Date),
         timestamp = as_datetime(timestamp),
         site = paste("Great Meadow", plot.num),
         water.depth = case_when(
           Year == 2016 & doy_h == 159.12 & plot.num == 3 & water.depth < -120 ~ NA_real_,
           Year == 2017 & doy_h == 215.02 & plot.num == 6 & water.depth < -115 ~ NA_real_,
           Year == 2021 & plot.num == 3 & doy == 224 & water.depth > 400 ~ NA_real_,
           Year == 2021 & plot.num == 3 & doy == 225 & water.depth > 400 ~ NA_real_,
           TRUE ~ water.depth
         )) %>%
  mutate(siteyear = paste(site, Year, sep = "_"))

gl <- read.csv("data/raw_data/hydrology_data/gilmore_well_prec_data_2013-2024.csv") %>%
  rename(water.depth = GILM_WL) %>%
  mutate(
    site = "Gilmore Meadow",
    
    # Try to parse full datetime, fallback to date only + midnight
    timestamp_parsed = mdy_hm(timestamp),
    timestamp_parsed = if_else(
      is.na(timestamp_parsed),
      mdy(timestamp),
      timestamp_parsed
    ),
    timestamp_parsed = as.POSIXct(timestamp_parsed),
    timestamp = timestamp_parsed,
    
    # Standardize Date column too
    Date = mdy(Date)
  ) %>%
  select(timestamp, 
         Date,
         doy,
         Year,
         precip_cm,
         water.depth,
         lag.precip,
         hr,
         doy_h,
         site)

all_data <- bind_rows(gm, gl) %>% 
  filter(Year >= 2016 & Year <= 2023) %>% 
  select(timestamp, 
         date = Date, 
         year = Year, 
         doy, 
         hr, 
         doy_h, 
         lag_precip = lag.precip,
         water_depth = water.depth,
         site)

wl_stats <- read.csv("data/processed_data/gm_gl_wl_stats.csv") %>% 
  select(year,
         stat, 
         `Gilmore Meadow` = gilmore.meadow, 
         `Great Meadow 1` = great.meadow.1, 
         `Great Meadow 2` = great.meadow.2, 
         `Great Meadow 3` = great.meadow.3, 
         `Great Meadow 4` = great.meadow.4, 
         `Great Meadow 5` = great.meadow.5, 
         `Great Meadow 6` = great.meadow.6) %>% 
  pivot_longer(cols = -c(year, stat), names_to = "site", values_to = "value") %>% 
  pivot_wider(names_from = stat, values_from = value) %>%
  arrange(site, year)


# UI 
ui <- page_fluid(
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#1B365D",
    secondary = "#4C6D9A",    
    success = "#2E86C1",
    info = "#3498db",
    warning = "#f39c12",
    danger = "#e74c3c",
    base_font = font_google("Open Sans"),
    heading_font = font_google("Roboto", wght = c(400, 700))
  ),
  
  # Custom CSS for additional styling
  tags$head(
    tags$style(HTML("
      .content-section {
        margin: 30px 0;
        padding: 25px;
        border-radius: 15px;
        box-shadow: 0 4px 12px rgba(0,0,0,0.1);
        background: linear-gradient(135deg, #ffffff 0%, #f8f9fa 100%);
        border: 2px solid #1B365D;
      }
      
      .section-title {
        color: #1B365D;
        font-weight: 600;
        margin-bottom: 20px;
        padding-bottom: 10px;
        border-bottom: 2px solid #4C6D9A;
      }
      
      .sidebar-custom {
        background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%);
        border-radius: 10px;
        padding: 20px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.08);
        border: 1px solid #dee2e6;
      }
      
      .main-title {
        background: linear-gradient(135deg, #1B365D 0%, #4C6D9A 100%);
        color: white;
        padding: 30px;
        margin: -15px -15px 30px -15px;
        text-align: center;
        border-radius: 0 0 20px 20px;
        box-shadow: 0 4px 12px rgba(0,0,0,0.15);
      }
      
      .brush-info-section {
        background: linear-gradient(135deg, #e8f4f8 0%, #f0f8ff 100%);
        border-radius: 12px;
        padding: 20px;
        margin: 20px 0;
        border: 1px solid #4C6D9A;
      }
      
      .stats-main {
        background: linear-gradient(135deg, #f8f9fa 0%, #ffffff 100%);
      }
      
      .dataTables_wrapper {
        font-size: 0.85rem !important;
      }

      .dataTables_wrapper table {
        font-size: 0.8rem !important;
      }

      .dataTables_wrapper .dataTables_info,
      .dataTables_wrapper .dataTables_paginate {
        font-size: 0.75rem !important;
      }
      
    "))
  ),
  
  # Main title with gradient background
  div(class = "main-title",
      h1("Wetland Hydrograph Visualizer", 
         style = "margin: 0; font-size: 2rem; text-shadow: 2px 2px 4px rgba(0,0,0,0.3)")
  ),
  
  # First section: Hydrographs 
  div(class = "content-section",
      layout_sidebar(
        sidebar = sidebar(
          class = "sidebar-custom",
          width = 300,
          h4("Hydrograph Controls", style = "color: #1B365D; margin-bottom: 20px;"),
          
          selectInput("gm_site", 
                      label = div(icon("map-marker"), "Select a Great Meadow Site:"),
                      choices = sort(unique(all_data$site[grepl("Great Meadow", all_data$site)])),
                      selected = "Great Meadow 1"),
          
          div(style = "margin-bottom: 15px;",
              pickerInput("year", 
                          label = div(icon("calendar"), "Select Year(s):"),
                          choices = sort(unique(all_data$year)),
                          selected = 2023,
                          multiple = TRUE,
                          options = list(
                            `actions-box` = TRUE,
                            `deselect-all-text` = "Clear all",
                            `select-all-text` = "Select all",
                            `none-selected-text` = "Choose year(s)",
                            `live-search` = TRUE,
                            style = "btn-outline-primary"
                          ))),
          
          br(),
          div(style = "padding: 15px; background-color: #e8f4f8; border-radius: 8px; border-left: 4px solid #3498db;",
              p(icon("info-circle"), " Use the brush tool to select data points on the hydrograph for a detailed view of the data.", 
                style = "margin: 0; font-size: 0.9rem; color: #2c3e50;")),
          
          div(style = "margin-top: 15px; text-align: center;",
              downloadButton("download_brush", "Download Selected Data", 
                             class = "btn-primary btn-sm", 
                             icon = icon("download")))
        ),
        
        card(
          full_screen = TRUE,
          card_header(
            class = "bg-primary text-white",
            "Hydrographs by Year"
          ),
          plotOutput("hydrograph", height = "600px", 
                     brush = brushOpts(id = "hydro_brush", fill = "#4C6D9A", opacity = 0.3))
        )
      )
  ),
  
  # Selected data points section with improved styling
  div(class = "brush-info-section",
      div(class = "row",
          div(class = "col-12",
              card(
                card_header(
                  class = "bg-success text-white",
                  "Selected Data from Hydrograph:"
                ),
                tableOutput("brush_info")
              )
          )
      )
  ),
  
  # Second section: Water Level Stats with improved styling
  div(class = "content-section stats-main",
      layout_sidebar(
        sidebar = sidebar(
          class = "sidebar-custom",
          width = 300,
          h4("Statistics Controls", style = "color: #1B365D; margin-bottom: 20px;"),
          
          pickerInput("stats_site", 
                      label = div(icon("map-marker"), "Select Site(s):"),
                      choices = sort(unique(wl_stats$site)),
                      selected = "Great Meadow 1",
                      multiple = TRUE,
                      options = list(
                        `actions-box` = TRUE,
                        `deselect-all-text` = "Clear all",
                        `select-all-text` = "Select all",
                        `none-selected-text` = "Choose site(s)",
                        `live-search` = TRUE,
                        style = "btn-outline-primary"
                      )),
          
          div(style = "margin-bottom: 15px;",
              pickerInput("stats_year", 
                          label = div(icon("calendar"), "Select Years:"),
                          choices = sort(unique(wl_stats$year)),
                          selected = c(2023, 2022),
                          multiple = TRUE,
                          options = list(
                            `actions-box` = TRUE,
                            `deselect-all-text` = "Clear all",
                            `select-all-text` = "Select all",
                            `none-selected-text` = "Choose year(s)",
                            `live-search` = TRUE,
                            style = "btn-outline-primary"
                          ))),
          
          br(),
          div(style = "padding: 15px; background-color: #e8f4f8; border-radius: 8px; border-left: 4px solid #3498db;",
              p(icon("info-circle"), " Growing Season statistics calculated from May through October.", 
                style = "margin: 0; font-size: 0.9rem; color: #2c3e50;")),
          
          div(style = "margin-top: 15px; text-align: center;",
              downloadButton("download_stats", "Download Table", 
                             class = "btn-primary btn-sm", 
                             icon = icon("download")))
        ),
        
        card(
          full_screen = TRUE,
          card_header(
            class = "bg-primary text-white",
            "Growing Season Water Level Statistics"
          ),
          div(style = "padding: 10px;",
              dataTableOutput("wl_stats"))
        )
      )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive data for plotting
  plot_data <- reactive({
    req(input$year, input$gm_site)
    
    all_data %>%
      filter(year %in% input$year, doy > 134, doy < 275) %>%
      filter(site %in% c(input$gm_site, "Gilmore Meadow"))
  })
  
  output$hydrograph <- renderPlot({
    plot_data_filtered <- plot_data()
    req(nrow(plot_data_filtered) > 0)
    
    # Calculate precipitation offset for each year
    precip_data <- plot_data_filtered %>%
      group_by(year) %>%
      summarise(
        min_water = min(c(
          water_depth[site == input$gm_site & !is.na(water_depth)],
          water_depth[site == "Gilmore Meadow" & !is.na(water_depth)]
        ), na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      left_join(plot_data_filtered, by = "year") %>%
      mutate(precip_y = lag_precip * 5 + min_water)
    
    global_min_water <- min(plot_data_filtered$water_depth, na.rm = TRUE)
    
    # Create the plot using facet_wrap instead of patchwork
    ggplot() +
      # Water level lines
      geom_line(data = plot_data_filtered %>% filter(site == input$gm_site),
                aes(x = doy_h, y = water_depth, color = input$gm_site), 
                size = 0.7) +
      geom_line(data = plot_data_filtered %>% filter(site == "Gilmore Meadow"),
                aes(x = doy_h, y = water_depth, color = "Gilmore Meadow"), 
                size = 0.7) +
      # Precipitation line
      geom_line(data = precip_data,
                aes(x = doy_h, y = precip_y, color = "Precipitation"), size = 0.7) +
      # Ground level reference
      geom_hline(yintercept = 0, color = "brown") +
      
      # Facet by year
      facet_wrap(~ year, ncol = 1, scales = "free_y") +
      
      # Styling
      scale_color_manual(values = c(
        setNames("black", input$gm_site),
        "Gilmore Meadow" = "darkgray",
        "Precipitation" = "blue"
      )) +
      labs(title = NULL,
           x = "Date", y = "Water Level (cm)") +
      scale_x_continuous(
        breaks = c(121, 152, 182, 213, 244, 274),
        labels = c('May-01', 'Jun-01', 'Jul-01', 'Aug-01', 'Sep-01', 'Oct-01')) +
      scale_y_continuous(
        name = "Water Level (cm)",
        sec.axis = sec_axis(~ (. - global_min_water) / 5,
                            name = "Hourly Precip. (cm)",
                            breaks = seq(0, 8, by = 2))) +
      theme_bw() +
      theme(legend.position = "bottom",
            legend.title = element_blank(),
            strip.text = element_text(size = 11),
            plot.title = element_text(hjust = 0.5))
  })
  
  # Selected data output table - CORRECTED WIDE FORMAT
  output$brush_info <- renderTable({
    req(input$hydro_brush)
    
    plot_data_filtered <- plot_data()
    req(nrow(plot_data_filtered) > 0)
    
    # Use brushedPoints function which works properly with faceted plots
    selected_data <- brushedPoints(
      plot_data_filtered,
      brush = input$hydro_brush,
      xvar = "doy_h",
      yvar = "water_depth"
    )
    
    if(nrow(selected_data) == 0) {
      data.frame(Message = "No points selected - try brushing over the water level lines")
    } else {
      # Create wide format with separate columns for each site
      wide_data <- selected_data %>%
        select(year, doy_h, timestamp, water_depth, site) %>%
        # Round for better display
        mutate(
          doy_h = round(doy_h, 2),
          water_depth = round(water_depth, 2),
          timestamp = format(timestamp, "%Y-%m-%d %H:%M:%S")
        ) %>%
        # Group by time points that are very close together (within 0.1 doy_h)
        mutate(time_group = round(doy_h * 10) / 10) %>%
        group_by(year, time_group) %>%
        summarise(
          timestamp = first(timestamp),
          doy_h = first(doy_h),
          .groups = 'drop'
        ) %>%
        # Get the water level data for each site at these time points
        left_join(
          selected_data %>%
            select(year, doy_h, water_depth, site) %>%
            mutate(
              time_group = round(doy_h * 10) / 10,
              water_depth = round(water_depth, 2)
            ) %>%
            group_by(year, time_group, site) %>%
            summarise(water_depth = first(water_depth), .groups = 'drop') %>%
            pivot_wider(
              names_from = site,
              values_from = water_depth,
              names_glue = "{tolower(gsub(' ', '_', site))}_wl"
            ),
          by = c("year", "time_group")
        ) %>%
        select(-time_group) %>%
        rename(
          Year = year,
          Timestamp = timestamp,
          `Day of Year` = doy_h
        ) %>%
        arrange(Year, `Day of Year`)
      
      wide_data
    }
  })
  
  # WL stats output table
  output$wl_stats <- DT::renderDataTable({
    req(input$stats_site, input$stats_year)
    
    wl_stats %>%
      filter(site %in% input$stats_site, year %in% input$stats_year) %>% 
      mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
      select(
        Year = year,
        Site = site,
        `Mean Water Level` = WL_mean,
        `SD Water Level` = WL_sd,
        `Minimum Water Level` = WL_min,
        `Maximum Water Level` = WL_max,
        `Maximum Hourly Increase` = max_inc,
        `Maximum Hourly Decrease` = max_dec,
        `Growing Season Change` = GS_change,
        `GS % Surface Water` = prop_over_0cm,
        `GS % Within 30cm` = prop_bet_0_neg30cm,
        `GS % Over 30cm Deep` = prop_under_neg30cm,
        `GS % Complete Data` = prop_GS_comp
      )
  }, options = list(pageLength = 10, scrollX = TRUE))
  
  # Download handler for brushed data - UPDATED TO DOWNLOAD WIDE FORMAT
  output$download_brush <- downloadHandler(
    filename = function() {
      paste("hydrograph_selected_data_wide_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      # Get the same wide format data as displayed in the table
      plot_data_filtered <- plot_data()
      
      if(is.null(input$hydro_brush) || nrow(plot_data_filtered) == 0) {
        # Create empty file if no data
        write.csv(data.frame(Message = "No data selected"), file, row.names = FALSE)
        return()
      }
      
      selected_data <- brushedPoints(
        plot_data_filtered,
        brush = input$hydro_brush,
        xvar = "doy_h",
        yvar = "water_depth"
      )
      
      if(nrow(selected_data) == 0) {
        write.csv(data.frame(Message = "No points selected"), file, row.names = FALSE)
      } else {
        # Use the same wide format logic as the table display
        wide_data <- selected_data %>%
          select(year, doy_h, timestamp, water_depth, site) %>%
          mutate(
            doy_h = round(doy_h, 2),
            water_depth = round(water_depth, 2),
            timestamp = format(timestamp, "%Y-%m-%d %H:%M:%S")
          ) %>%
          mutate(time_group = round(doy_h * 10) / 10) %>%
          group_by(year, time_group) %>%
          summarise(
            timestamp = first(timestamp),
            doy_h = first(doy_h),
            .groups = 'drop'
          ) %>%
          left_join(
            selected_data %>%
              select(year, doy_h, water_depth, site) %>%
              mutate(
                time_group = round(doy_h * 10) / 10,
                water_depth = round(water_depth, 2)
              ) %>%
              group_by(year, time_group, site) %>%
              summarise(water_depth = first(water_depth), .groups = 'drop') %>%
              pivot_wider(
                names_from = site,
                values_from = water_depth,
                names_glue = "{tolower(gsub(' ', '_', site))}_wl"
              ),
            by = c("year", "time_group")
          ) %>%
          select(-time_group) %>%
          rename(
            Year = year,
            Timestamp = timestamp,
            Day_of_Year = doy_h
          ) %>%
          arrange(Year, Day_of_Year)
        
        write.csv(wide_data, file, row.names = FALSE)
      }
    }
  )
  
  # Download handler for water level stats
  output$download_stats <- downloadHandler(
    filename = function() {
      paste("water_level_stats_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      # Use your filtered stats data
      filtered_stats <- wl_stats %>%
        filter(site %in% input$stats_site,
               year %in% input$stats_year)
      write.csv(filtered_stats, file, row.names = FALSE)
    }
  )
  
}

# Run app
shinyApp(ui, server)






### app with pivot wider dataset

library(shiny)
library(tidyverse)
library(lubridate)
library(patchwork)
library(shinyWidgets)
library(DT)
library(bslib)

# Read and prepare your processed data
gm <- read.csv("data/processed_data/great_meadow_well_data_2024_20250715.csv") %>%
  rename(Date = date, Year = year, precip_cm = precip.cm) %>%
  mutate(Date = as.Date(Date),
         timestamp = as_datetime(timestamp),
         site = paste("Great Meadow", plot.num),
         water.depth = case_when(
           Year == 2016 & doy_h == 159.12 & plot.num == 3 & water.depth < -120 ~ NA_real_,
           Year == 2017 & doy_h == 215.02 & plot.num == 6 & water.depth < -115 ~ NA_real_,
           Year == 2021 & plot.num == 3 & doy == 224 & water.depth > 400 ~ NA_real_,
           Year == 2021 & plot.num == 3 & doy == 225 & water.depth > 400 ~ NA_real_,
           TRUE ~ water.depth
         )) %>%
  mutate(siteyear = paste(site, Year, sep = "_"))

gl <- read.csv("data/raw_data/hydrology_data/gilmore_well_prec_data_2013-2024.csv") %>%
  rename(water.depth = GILM_WL) %>%
  mutate(
    site = "Gilmore Meadow",
    
    # Try to parse full datetime, fallback to date only + midnight
    timestamp_parsed = mdy_hm(timestamp),
    timestamp_parsed = if_else(
      is.na(timestamp_parsed),
      mdy(timestamp),
      timestamp_parsed
    ),
    timestamp_parsed = as.POSIXct(timestamp_parsed),
    timestamp = timestamp_parsed,
    
    # Standardize Date column too
    Date = mdy(Date)
  ) %>%
  select(timestamp, 
         Date,
         doy,
         Year,
         precip_cm,
         water.depth,
         lag.precip,
         hr,
         doy_h,
         site)

# combine all well data together
all_data <- bind_rows(gm, gl) %>% 
  filter(Year >= 2016 & Year <= 2023) %>% 
  select(timestamp, 
         date = Date, 
         year = Year, 
         doy, 
         hr, 
         doy_h, 
         lag_precip = lag.precip,
         water_depth = water.depth,
         site)

# # Re-format data
# # 1. Separate metadata per timestamp
# timestamp_meta <- all_data %>%
#   select(timestamp, Date, Year, doy, hr, doy_h, lag.precip) %>%
#   distinct()
# 
# # 2. Pivot water level wide
# wl_wide <- all_data %>%
#   select(timestamp, site, water.depth) %>%
#   pivot_wider(names_from = site, values_from = water.depth) %>%
#   left_join(timestamp_meta, by = "timestamp") %>%
#   relocate(timestamp, Date, Year, doy, hr, doy_h, lag.precip) %>% 
#   rename(date = Date,
#          year = Year,
#          lag_precip = lag.precip,
#          gilmore_meadow = `Gilmore Meadow`, 
#          great_meadow_1 = `Great Meadow 1`, 
#          great_meadow_2 = `Great Meadow 2`,
#          great_meadow_3 = `Great Meadow 3`,
#          great_meadow_4 = `Great Meadow 4`,
#          great_meadow_5 = `Great Meadow 5`,
#          great_meadow_6 = `Great Meadow 6`)


# Read and prepare wl stats
wl_stats <- read.csv("data/processed_data/gm_gl_wl_stats.csv") %>% 
  select(year,
         stat, 
         `Gilmore Meadow` = gilmore.meadow, 
         `Great Meadow 1` = great.meadow.1, 
         `Great Meadow 2` = great.meadow.2, 
         `Great Meadow 3` = great.meadow.3, 
         `Great Meadow 4` = great.meadow.4, 
         `Great Meadow 5` = great.meadow.5, 
         `Great Meadow 6` = great.meadow.6) %>% 
  pivot_longer(cols = -c(year, stat), names_to = "site", values_to = "value") %>% 
  pivot_wider(names_from = stat, values_from = value) %>%
  arrange(site, year)


# UI 
ui <- page_fluid(
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#1B365D",
    secondary = "#4C6D9A",    
    success = "#2E86C1",
    info = "#3498db",
    warning = "#f39c12",
    danger = "#e74c3c",
    base_font = font_google("Open Sans"),
    heading_font = font_google("Roboto", wght = c(400, 700))
  ),
  
  # Custom CSS for additional styling
  tags$head(
    tags$style(HTML("
      .content-section {
        margin: 30px 0;
        padding: 25px;
        border-radius: 15px;
        box-shadow: 0 4px 12px rgba(0,0,0,0.1);
        background: linear-gradient(135deg, #ffffff 0%, #f8f9fa 100%);
        border: 2px solid #1B365D;
      }
      
      .section-title {
        color: #1B365D;
        font-weight: 600;
        margin-bottom: 20px;
        padding-bottom: 10px;
        border-bottom: 2px solid #4C6D9A;
      }
      
      .sidebar-custom {
        background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%);
        border-radius: 10px;
        padding: 20px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.08);
        border: 1px solid #dee2e6;
      }
      
      .main-title {
        background: linear-gradient(135deg, #1B365D 0%, #4C6D9A 100%);
        color: white;
        padding: 30px;
        margin: -15px -15px 30px -15px;
        text-align: center;
        border-radius: 0 0 20px 20px;
        box-shadow: 0 4px 12px rgba(0,0,0,0.15);
      }
      
      .brush-info-section {
        background: linear-gradient(135deg, #e8f4f8 0%, #f0f8ff 100%);
        border-radius: 12px;
        padding: 20px;
        margin: 20px 0;
        border: 1px solid #4C6D9A;
      }
      
      .stats-main {
        background: linear-gradient(135deg, #f8f9fa 0%, #ffffff 100%);
      }
      
      .dataTables_wrapper {
        font-size: 0.85rem !important;
      }

      .dataTables_wrapper table {
        font-size: 0.8rem !important;
      }

      .dataTables_wrapper .dataTables_info,
      .dataTables_wrapper .dataTables_paginate {
        font-size: 0.75rem !important;
      }
      
    "))
  ),
  
  # Main title with gradient background
  div(class = "main-title",
      h1("Wetland Hydrograph Visualizer", 
         style = "margin: 0; font-size: 2rem; text-shadow: 2px 2px 4px rgba(0,0,0,0.3)")
  ),
  
  # First section: Hydrographs 
  div(class = "content-section",
      layout_sidebar(
        sidebar = sidebar(
          class = "sidebar-custom",
          width = 300,
          h4("Hydrograph Controls", style = "color: #1B365D; margin-bottom: 20px;"),
          
          selectInput("gm_site", 
                      label = div(icon("map-marker"), "Select a Great Meadow Site:"),
                      choices = sort(unique(all_data$site[grepl("Great Meadow", all_data$site)])),
                      selected = "Great Meadow 1"),
          
          div(style = "margin-bottom: 15px;",
              pickerInput("year", 
                          label = div(icon("calendar"), "Select Year(s):"),
                          choices = sort(unique(all_data$year)),
                          selected = 2023,
                          multiple = TRUE,
                          options = list(
                            `actions-box` = TRUE,
                            `deselect-all-text` = "Clear all",
                            `select-all-text` = "Select all",
                            `none-selected-text` = "Choose year(s)",
                            `live-search` = TRUE,
                            style = "btn-outline-primary"
                          ))),
          
          br(),
          div(style = "padding: 15px; background-color: #e8f4f8; border-radius: 8px; border-left: 4px solid #3498db;",
              p(icon("info-circle"), " Use the brush tool to select data points on the hydrograph for a detailed view of the data.", 
                style = "margin: 0; font-size: 0.9rem; color: #2c3e50;")),
          
          div(style = "margin-top: 15px; text-align: center;",
              downloadButton("download_brush", "Download Selected Data", 
                             class = "btn-primary btn-sm", 
                             icon = icon("download")))
        ),
        
        card(
          full_screen = TRUE,
          card_header(
            class = "bg-primary text-white",
            "Hydrographs by Year"
          ),
          plotOutput("hydrograph", height = "600px", 
                     brush = brushOpts(id = "hydro_brush", fill = "#4C6D9A", opacity = 0.3))
        )
      )
  ),
  
  # Selected data points section with improved styling
  div(class = "brush-info-section",
      div(class = "row",
          div(class = "col-12",
              card(
                card_header(
                  class = "bg-success text-white",
                  "Selected Data from Hydrograph:"
                ),
                tableOutput("brush_info")
              )
          )
      )
  ),
  
  # Second section: Water Level Stats with improved styling
  div(class = "content-section stats-main",
      layout_sidebar(
        sidebar = sidebar(
          class = "sidebar-custom",
          width = 300,
          h4("Statistics Controls", style = "color: #1B365D; margin-bottom: 20px;"),
          
          pickerInput("stats_site", 
                      label = div(icon("map-marker"), "Select Site(s):"),
                      choices = sort(unique(wl_stats$site)),
                      selected = "Great Meadow 1",
                      multiple = TRUE,
                      options = list(
                        `actions-box` = TRUE,
                        `deselect-all-text` = "Clear all",
                        `select-all-text` = "Select all",
                        `none-selected-text` = "Choose site(s)",
                        `live-search` = TRUE,
                        style = "btn-outline-primary"
                      )),
          
          div(style = "margin-bottom: 15px;",
              pickerInput("stats_year", 
                          label = div(icon("calendar"), "Select Years:"),
                          choices = sort(unique(wl_stats$year)),
                          selected = c(2023, 2022),
                          multiple = TRUE,
                          options = list(
                            `actions-box` = TRUE,
                            `deselect-all-text` = "Clear all",
                            `select-all-text` = "Select all",
                            `none-selected-text` = "Choose year(s)",
                            `live-search` = TRUE,
                            style = "btn-outline-primary"
                          ))),
          
          br(),
          div(style = "padding: 15px; background-color: #e8f4f8; border-radius: 8px; border-left: 4px solid #3498db;",
              p(icon("info-circle"), " Growing Season statistics calculated from May through October.", 
                style = "margin: 0; font-size: 0.9rem; color: #2c3e50;")),
          
          div(style = "margin-top: 15px; text-align: center;",
              downloadButton("download_stats", "Download Table", 
                             class = "btn-primary btn-sm", 
                             icon = icon("download")))
        ),
        
        card(
          full_screen = TRUE,
          card_header(
            class = "bg-primary text-white",
            "Growing Season Water Level Statistics"
          ),
          div(style = "padding: 10px;",
              dataTableOutput("wl_stats"))
        )
      )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive data for plotting
  plot_data <- reactive({
    req(input$year, input$gm_site)
    
    all_data %>%
      filter(year %in% input$year, doy > 134, doy < 275) %>%
      filter(site %in% c(input$gm_site, "Gilmore Meadow"))
  })
  
  output$hydrograph <- renderPlot({
    plot_data_filtered <- plot_data()
    req(nrow(plot_data_filtered) > 0)
    
    # Calculate precipitation offset for each year
    precip_data <- plot_data_filtered %>%
      group_by(year) %>%
      summarise(
        min_water = min(c(
          water_depth[site == input$gm_site & !is.na(water_depth)],
          water_depth[site == "Gilmore Meadow" & !is.na(water_depth)]
        ), na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      left_join(plot_data_filtered, by = "year") %>%
      mutate(precip_y = lag_precip * 5 + min_water)
    
    global_min_water <- min(plot_data_filtered$water_depth, na.rm = TRUE)
    
    # Create the plot using facet_wrap instead of patchwork
    ggplot() +
      # Water level lines
      geom_line(data = plot_data_filtered %>% filter(site == input$gm_site),
                aes(x = doy_h, y = water_depth, color = input$gm_site), 
                size = 0.7) +
      geom_line(data = plot_data_filtered %>% filter(site == "Gilmore Meadow"),
                aes(x = doy_h, y = water_depth, color = "Gilmore Meadow"), 
                size = 0.7) +
      # Precipitation line
      geom_line(data = precip_data,
                aes(x = doy_h, y = precip_y, color = "Precipitation"), size = 0.7) +
      # Ground level reference
      geom_hline(yintercept = 0, color = "brown") +
      
      # Facet by year
      facet_wrap(~ year, ncol = 1, scales = "free_y") +
      
      # Styling
      scale_color_manual(values = c(
        setNames("black", input$gm_site),
        "Gilmore Meadow" = "darkgray",
        "Precipitation" = "blue"
      )) +
      labs(title = NULL,
           x = "Date", y = "Water Level (cm)") +
      scale_x_continuous(
        breaks = c(121, 152, 182, 213, 244, 274),
        labels = c('May-01', 'Jun-01', 'Jul-01', 'Aug-01', 'Sep-01', 'Oct-01')) +
      scale_y_continuous(
        name = "Water Level (cm)",
        sec.axis = sec_axis(~ (. - global_min_water) / 5,
                            name = "Hourly Precip. (cm)",
                            breaks = seq(0, 8, by = 2))) +
      theme_bw() +
      theme(legend.position = "bottom",
            legend.title = element_blank(),
            strip.text = element_text(size = 11),
            plot.title = element_text(hjust = 0.5))
  })
  
  # Selected data output table - Now with separate columns for each site
  output$brush_info <- renderTable({
    brush <- input$hydro_brush
    if (is.null(brush)) return(NULL)
    
    filtered_long <- all_data %>%
      filter(doy_h >= brush$xmin, doy_h <= brush$xmax,
             water_depth >= brush$ymin, water_depth <= brush$ymax,
             site %in% c("Gilmore Meadow", input$gm_site)) %>%
      select(timestamp, site, water_depth) %>%
      distinct()
    
    wide_selected <- filtered_long %>%
      pivot_wider(names_from = site, values_from = water_depth)
    
    wide_selected
  })
  
  
  # WL stats output table
  output$wl_stats <- DT::renderDataTable({
    req(input$stats_site, input$stats_year)
    
    wl_stats %>%
      filter(site %in% input$stats_site, year %in% input$stats_year) %>% 
      mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
      select(
        Year = year,
        Site = site,
        `Mean Water Level` = WL_mean,
        `SD Water Level` = WL_sd,
        `Minimum Water Level` = WL_min,
        `Maximum Water Level` = WL_max,
        `Maximum Hourly Increase` = max_inc,
        `Maximum Hourly Decrease` = max_dec,
        `Growing Season Change` = GS_change,
        `GS % Surface Water` = prop_over_0cm,
        `GS % Within 30cm` = prop_bet_0_neg30cm,
        `GS % Over 30cm Deep` = prop_under_neg30cm,
        `GS % Complete Data` = prop_GS_comp
      )
  }, options = list(pageLength = 10, scrollX = TRUE))
  
  # Download handler for brushed data
  output$download_brush <- downloadHandler(
    filename = function() {
      paste0("selected_hydrograph_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      d <- event_data("plotly_selected", source = "subset_data")
      
      if (is.null(d)) {
        return(NULL)
      }
      
      selected_timestamps <- as.POSIXct(d$x, origin = "1970-01-01", tz = "America/New_York")
      
      filtered_long <- all_data %>%
        filter(timestamp %in% selected_timestamps,
               site %in% c("Gilmore Meadow", input$gm_site)) %>%
        select(timestamp, site, water_depth) %>%
        distinct()
      
      wide_selected <- filtered_long %>%
        pivot_wider(names_from = site, values_from = water_depth)
      
      write.csv(wide_selected, file, row.names = FALSE)
    }
  )
  
  # Download handler for water level stats
  output$download_stats <- downloadHandler(
    filename = function() {
      paste("water_level_stats_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      # Use your filtered stats data
      filtered_stats <- wl_stats %>%
        filter(site %in% input$stats_site,
               year %in% input$stats_year)
      write.csv(filtered_stats, file, row.names = FALSE)
    }
  )
  
}

# Run app
shinyApp(ui, server)











#### More complex style and design version 7/30/25

library(shiny)
library(tidyverse)
library(lubridate)
library(patchwork)
library(shinyWidgets)
library(DT)
library(bslib)

# Read & prepare your processed data (from your current script)
gm <- read.csv("data/processed_data/great_meadow_well_data_2024_20250715.csv") %>%
  rename(Date = date, Year = year, precip_cm = precip.cm) %>%
  mutate(Date = as.Date(Date),
         timestamp = as_datetime(timestamp),
         site = paste("Great Meadow", plot.num),
         water.depth = case_when(
           Year == 2016 & doy_h == 159.12 & plot.num == 3 & water.depth < -120 ~ NA_real_,
           Year == 2017 & doy_h == 215.02 & plot.num == 6 & water.depth < -115 ~ NA_real_,
           Year == 2021 & plot.num == 3 & doy == 224 & water.depth > 400 ~ NA_real_,
           Year == 2021 & plot.num == 3 & doy == 225 & water.depth > 400 ~ NA_real_,
           TRUE ~ water.depth
         )) %>%
  mutate(siteyear = paste(site, Year, sep = "_"))

gl <- read.csv("data/raw_data/hydrology_data/gilmore_well_prec_data_2013-2024.csv") %>%
  rename(water.depth = GILM_WL) %>%
  mutate(
    site = "Gilmore Meadow",
    
    # Try to parse full datetime, fallback to date only + midnight
    timestamp_parsed = mdy_hm(timestamp),
    timestamp_parsed = if_else(
      is.na(timestamp_parsed),
      mdy(timestamp),
      timestamp_parsed
    ),
    timestamp_parsed = as.POSIXct(timestamp_parsed),
    timestamp = timestamp_parsed,
    
    # Standardize Date column too
    Date = mdy(Date)
  ) %>%
  select(timestamp, 
         Date,
         doy,
         Year,
         precip_cm,
         water.depth,
         lag.precip,
         hr,
         doy_h,
         site)

all_data <- bind_rows(gm, gl) %>% 
  filter(Year >= 2016 & Year <= 2023) 

wl_stats <- read.csv("data/processed_data/gm_gl_wl_stats.csv") %>% 
  select(year,
         stat, 
         `Gilmore Meadow` = gilmore.meadow, 
         `Great Meadow 1` = great.meadow.1, 
         `Great Meadow 2` = great.meadow.2, 
         `Great Meadow 3` = great.meadow.3, 
         `Great Meadow 4` = great.meadow.4, 
         `Great Meadow 5` = great.meadow.5, 
         `Great Meadow 6` = great.meadow.6) %>% 
  pivot_longer(cols = -c(year, stat), names_to = "site", values_to = "value") %>% 
  pivot_wider(names_from = stat, values_from = value) %>%
  arrange(site, year)


# UI with improved styling
ui <- page_fluid(
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#2C5F41",
    secondary = "#5A9B7C", 
    success = "#27ae60",
    info = "#3498db",
    warning = "#f39c12",
    danger = "#e74c3c",
    base_font = font_google("Open Sans"),
    heading_font = font_google("Roboto", wght = c(400, 700))
  ),
  
  # Custom CSS for additional styling
  tags$head(
    tags$style(HTML("
      .content-section {
        margin: 30px 0;
        padding: 25px;
        border-radius: 15px;
        box-shadow: 0 4px 12px rgba(0,0,0,0.1);
        background: linear-gradient(135deg, #ffffff 0%, #f8f9fa 100%);
        border-left: 5px solid #2C5F41;
      }
      
      .section-title {
        color: #2C5F41;
        font-weight: 600;
        margin-bottom: 20px;
        padding-bottom: 10px;
        border-bottom: 2px solid #5A9B7C;
      }
      
      .sidebar-custom {
        background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%);
        border-radius: 10px;
        padding: 20px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.08);
        border: 1px solid #dee2e6;
      }
      
      .main-title {
        background: linear-gradient(135deg, #2C5F41 0%, #5A9B7C 100%);
        color: white;
        padding: 30px;
        margin: -15px -15px 30px -15px;
        text-align: center;
        border-radius: 0 0 20px 20px;
        box-shadow: 0 4px 12px rgba(0,0,0,0.15);
      }
      
      .brush-info-section {
        background: linear-gradient(135deg, #e8f4f8 0%, #f0f8ff 100%);
        border-radius: 12px;
        padding: 20px;
        margin: 20px 0;
        border: 1px solid #5A9B7C;
      }
      
      .stats-main {
        background: linear-gradient(135deg, #f8f9fa 0%, #ffffff 100%);
      }
      
    "))
  ),
  
  # Main title with gradient background
  div(class = "main-title",
      h1("Wetland Hydrograph Visualizer", 
         style = "margin: 0; font-size: 2.5rem; text-shadow: 2px 2px 4px rgba(0,0,0,0.3)")
  ),
  
  # First section: Hydrograph with improved styling
  div(class = "content-section",
      h2("Wetland Hydrograph Analysis", class = "section-title"),
      layout_sidebar(
        sidebar = sidebar(
          class = "sidebar-custom",
          width = 300,
          h4("Hydrograph Controls", style = "color: #2C5F41; margin-bottom: 20px;"),
          
          selectInput("gm_site", 
                      label = div(icon("map-marker"), "Select a Great Meadow Site:"),
                      choices = sort(unique(all_data$site[grepl("Great Meadow", all_data$site)])),
                      selected = "Great Meadow 1"),
          
          br(),
          pickerInput("year", 
                      label = div(icon("calendar"), "Select Year(s):"),
                      choices = sort(unique(all_data$Year)),
                      selected = 2023,
                      multiple = TRUE,
                      options = list(
                        `actions-box` = TRUE,
                        `deselect-all-text` = "Clear all",
                        `select-all-text` = "Select all",
                        `none-selected-text` = "Choose year(s)",
                        `live-search` = TRUE,
                        style = "btn-outline-primary"
                      )),
          
          br(),
          div(style = "padding: 15px; background-color: #e8f4f8; border-radius: 8px; border-left: 4px solid #3498db;",
              p(icon("info-circle"), " Use the brush tool to select data points on the chart for detailed analysis.", 
                style = "margin: 0; font-size: 0.9rem; color: #2c3e50;"))
        ),
        
        card(
          full_screen = TRUE,
          card_header(
            class = "bg-primary text-white",
            "Hydrographs by Year"
          ),
          plotOutput("hydrograph", height = "600px", 
                     brush = brushOpts(id = "hydro_brush", fill = "#5A9B7C", opacity = 0.3))
        )
      )
  ),
  
  # Selected data points section with improved styling
  div(class = "brush-info-section",
      div(class = "row",
          div(class = "col-12",
              h3("Selected Data Points:", 
                 style = "color: #2C5F41; text-align: center; margin-bottom: 20px;"),
              card(
                card_header(
                  class = "bg-info text-white",
                  "Detailed View of Brushed Data"
                ),
                tableOutput("brush_info")
              )
          )
      )
  ),
  
  # Second section: Water Level Stats with improved styling
  div(class = "content-section stats-main",
      h2("Water Level Statistics", class = "section-title"),
      layout_sidebar(
        sidebar = sidebar(
          class = "sidebar-custom",
          width = 300,
          h4("Statistics Controls", style = "color: #2C5F41; margin-bottom: 20px;"),
          
          pickerInput("stats_site", 
                      label = div(icon("map-marker"), "Select Site(s):"),
                      choices = sort(unique(wl_stats$site)),
                      selected = "Great Meadow 1",
                      multiple = TRUE,
                      options = list(
                        `actions-box` = TRUE,
                        `deselect-all-text` = "Clear all",
                        `select-all-text` = "Select all",
                        `none-selected-text` = "Choose site(s)",
                        `live-search` = TRUE,
                        style = "btn-outline-primary"
                      )),
          
          br(),
          
          pickerInput("stats_year", 
                      label = div(icon("calendar"), "Select Years:"),
                      choices = sort(unique(wl_stats$year)),
                      selected = c(2022, 2023),
                      multiple = TRUE,
                      options = list(
                        `actions-box` = TRUE,
                        `deselect-all-text` = "Clear all",
                        `select-all-text` = "Select all",
                        `none-selected-text` = "Choose year(s)",
                        `live-search` = TRUE,
                        style = "btn-outline-primary"
                      )),
          
          br(),
          div(style = "padding: 15px; background-color: #e8f4f8; border-radius: 8px; border-left: 4px solid #3498db;",
              p(icon("info-circle"), " Growing Season statistics calculated from May through October.", 
                style = "margin: 0; font-size: 0.9rem; color: #2c3e50;"))
        ),
        
        card(
          full_screen = TRUE,
          card_header(
            class = "bg-primary text-white",
            "Growing Season Water Level Statistics"
          ),
          div(style = "padding: 10px;",
              dataTableOutput("wl_stats"))
        )
      )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive data for plotting
  plot_data <- reactive({
    req(input$year, input$gm_site)
    
    all_data %>%
      filter(Year %in% input$year, doy > 134, doy < 275) %>%
      filter(site %in% c(input$gm_site, "Gilmore Meadow"))
  })
  
  output$hydrograph <- renderPlot({
    plot_data_filtered <- plot_data()
    req(nrow(plot_data_filtered) > 0)
    
    # Calculate precipitation offset for each year
    precip_data <- plot_data_filtered %>%
      group_by(Year) %>%
      summarise(
        min_water = min(c(
          water.depth[site == input$gm_site & !is.na(water.depth)],
          water.depth[site == "Gilmore Meadow" & !is.na(water.depth)]
        ), na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      left_join(plot_data_filtered, by = "Year") %>%
      mutate(precip_y = lag.precip * 5 + min_water)
    
    global_min_water <- min(plot_data_filtered$water.depth, na.rm = TRUE)
    
    # Create the plot using facet_wrap instead of patchwork
    ggplot() +
      # Water level lines
      geom_line(data = plot_data_filtered %>% filter(site == input$gm_site),
                aes(x = doy_h, y = water.depth, color = input$gm_site), 
                size = 0.7) +
      geom_line(data = plot_data_filtered %>% filter(site == "Gilmore Meadow"),
                aes(x = doy_h, y = water.depth, color = "Gilmore Meadow"), 
                size = 0.7) +
      # Precipitation line
      geom_line(data = precip_data,
                aes(x = doy_h, y = precip_y, color = "Precipitation"), size = 0.7) +
      # Ground level reference
      geom_hline(yintercept = 0, color = "brown") +
      
      # Facet by year
      facet_wrap(~ Year, ncol = 1, scales = "free_y") +
      
      # Styling
      scale_color_manual(values = c(
        setNames("black", input$gm_site),
        "Gilmore Meadow" = "darkgray",
        "Precipitation" = "blue"
      )) +
      labs(title = NULL,
           x = "Date", y = "Water Level (cm)") +
      scale_x_continuous(
        breaks = c(121, 152, 182, 213, 244, 274),
        labels = c('May-01', 'Jun-01', 'Jul-01', 'Aug-01', 'Sep-01', 'Oct-01')) +
      scale_y_continuous(
        name = "Water Level (cm)",
        sec.axis = sec_axis(~ (. - global_min_water) / 5,
                            name = "Hourly Precip. (cm)",
                            breaks = seq(0, 8, by = 2))) +
      theme_bw() +
      theme(legend.position = "bottom",
            legend.title = element_blank(),
            strip.text = element_text(size = 11),
            plot.title = element_text(hjust = 0.5))
  })
  
  # Selected data output table - Now with separate columns for each site
  output$brush_info <- renderTable({
    req(input$hydro_brush)
    
    plot_data_filtered <- plot_data()
    req(nrow(plot_data_filtered) > 0)
    
    # Use brushedPoints function which works properly with faceted plots
    selected_data <- brushedPoints(
      plot_data_filtered,
      brush = input$hydro_brush,
      xvar = "doy_h",
      yvar = "water.depth"
    ) %>%
      select(site, Year, doy_h, timestamp, water.depth, precip_cm) %>%
      arrange(Year, doy_h) %>%
      # Round for better display
      mutate(
        doy_h = round(doy_h, 2),
        water.depth = round(water.depth, 2),
        precip_cm = round(precip_cm, 3),
        timestamp = format(timestamp, "%Y-%m-%d %H:%M:%S")
      )
    
    if(nrow(selected_data) == 0) {
      data.frame(Message = "No points selected - try brushing over the water level lines")
    } else {
      # Create a proper time grouping to match data points from both sites
      # Group by rounded doy_h to combine data from both sites at similar times
      pivoted_data <- selected_data %>%
        # Round doy_h slightly to group nearby times together
        mutate(doy_h_rounded = round(doy_h, 1)) %>%
        # Group by the rounded time
        group_by(Year, doy_h_rounded) %>%
        summarise(
          # Use the first timestamp and precipitation value in each group
          timestamp = first(timestamp),
          doy_h = first(doy_h),
          precip_cm = first(precip_cm),
          # Create separate columns for each site's water depth
          great_meadow_depth = ifelse(any(site == input$gm_site), 
                                      water.depth[site == input$gm_site][1], NA),
          gilmore_depth = ifelse(any(site == "Gilmore Meadow"), 
                                 water.depth[site == "Gilmore Meadow"][1], NA),
          .groups = 'drop'
        ) %>%
        # Remove the grouping column and rename for display
        select(-doy_h_rounded) %>%
        rename(
          `Timestamp` = timestamp,
          `Day of Year` = doy_h,
          `Precipitation (cm)` = precip_cm
        ) %>%
        # Create dynamic column names based on selected site
        rename_with(~ paste(input$gm_site, "Water Depth (cm)"), .cols = great_meadow_depth) %>%
        rename_with(~ "Gilmore Meadow Water Depth (cm)", .cols = gilmore_depth) %>%
        # Arrange by year and day
        arrange(Year, `Day of Year`)
      
      pivoted_data
    }
  })
  
  # WL stats output table
  output$wl_stats <- DT::renderDataTable({
    req(input$stats_site, input$stats_year)
    
    wl_stats %>%
      filter(site %in% input$stats_site, year %in% input$stats_year) %>% 
      mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
      select(
        Year = year,
        Site = site,
        `Mean Water Level` = WL_mean,
        `SD Water Level` = WL_sd,
        `Minimum Water Level` = WL_min,
        `Maximum Water Level` = WL_max,
        `Maximum Hourly Increase` = max_inc,
        `Maximum Hourly Decrease` = max_dec,
        `Growing Season Change` = GS_change,
        `GS % Surface Water` = prop_over_0cm,
        `GS % Within 30cm` = prop_bet_0_neg30cm,
        `GS % Over 30cm Deep` = prop_under_neg30cm,
        `GS % Complete Data` = prop_GS_comp
      )
  }, options = list(pageLength = 10, scrollX = TRUE))
}

# Run app
shinyApp(ui, server)





#### BASIC HYDROGRAPH VISUALIZER

library(shiny)
library(tidyverse)
library(lubridate)
library(patchwork)
library(shinyWidgets)
library(DT)

# Read & prepare your processed data (from your current script)
gm <- read.csv("data/processed_data/great_meadow_well_data_2024_20250715.csv") %>%
  rename(Date = date, Year = year, precip_cm = precip.cm) %>%
  mutate(Date = as.Date(Date),
         timestamp = as_datetime(timestamp),
         site = paste("Great Meadow", plot.num),
         water.depth = case_when(
           Year == 2016 & doy_h == 159.12 & plot.num == 3 & water.depth < -120 ~ NA_real_,
           Year == 2017 & doy_h == 215.02 & plot.num == 6 & water.depth < -115 ~ NA_real_,
           Year == 2021 & plot.num == 3 & doy == 224 & water.depth > 400 ~ NA_real_,
           Year == 2021 & plot.num == 3 & doy == 225 & water.depth > 400 ~ NA_real_,
           TRUE ~ water.depth
         )) %>%
  mutate(siteyear = paste(site, Year, sep = "_"))

gl <- read.csv("data/raw_data/hydrology_data/gilmore_well_prec_data_2013-2024.csv") %>%
  rename(water.depth = GILM_WL) %>%
  mutate(
    site = "Gilmore Meadow",
    
    # Try to parse full datetime, fallback to date only + midnight
    timestamp_parsed = mdy_hm(timestamp),
    timestamp_parsed = if_else(
      is.na(timestamp_parsed),
      mdy(timestamp),
      timestamp_parsed
    ),
    timestamp_parsed = as.POSIXct(timestamp_parsed),
    timestamp = timestamp_parsed,
    
    # Standardize Date column too
    Date = mdy(Date)
  ) %>%
  select(timestamp, 
         Date,
         doy,
         Year,
         precip_cm,
         water.depth,
         lag.precip,
         hr,
         doy_h,
         site)

all_data <- bind_rows(gm, gl) %>% 
  filter(Year >= 2016 & Year <= 2023) 

wl_stats <- read.csv("data/processed_data/gm_gl_wl_stats.csv") %>% 
  select(year,
         stat, 
         `Gilmore Meadow` = gilmore.meadow, 
         `Great Meadow 1` = great.meadow.1, 
         `Great Meadow 2` = great.meadow.2, 
         `Great Meadow 3` = great.meadow.3, 
         `Great Meadow 4` = great.meadow.4, 
         `Great Meadow 5` = great.meadow.5, 
         `Great Meadow 6` = great.meadow.6) %>% 
  pivot_longer(cols = -c(year, stat), names_to = "site", values_to = "value") %>% 
  pivot_wider(names_from = stat, values_from = value) %>%
  arrange(site, year)


# UI
ui <- fluidPage(
  titlePanel("Wetland Hydrograph Visualizer"),
  
  # First section: Hydrograph with its own sidebar
  sidebarLayout(
    sidebarPanel(
      h4("Hydrograph Controls"),
      pickerInput("year", "Select Year(s):",
                  choices = sort(unique(all_data$Year)),
                  selected = 2023,
                  multiple = TRUE,
                  options = list(`actions-box` = TRUE,
                                 `deselect-all-text` = "Clear all",
                                 `select-all-text` = "Select all",
                                 `none-selected-text` = "Choose year(s)")),
      selectInput("gm_site", "Select Great Meadow Site(s) for Hydrograph:",
                  choices = sort(unique(all_data$site[grepl("Great Meadow", all_data$site)])),
                  selected = "Great Meadow 1")
    ),
    mainPanel(
      plotOutput("hydrograph", height = "600px", brush = brushOpts(id = "hydro_brush"))
    )
  ),
  
  # Selected data points section
  fluidRow(
    column(width = 10, offset = 1, align = "center",
           h4("Selected Data Points:"),
           tableOutput("brush_info"))
  ),
  
  # Second section: Water Level Stats with its own sidebar
  h3("Water Level Statistics", style = "text-align: center; margin-top: 40px; margin-bottom: 20px;"),
  sidebarLayout(
    sidebarPanel(
      h4("Statistics Controls"),
      pickerInput("stats_site", "Select Site(s) for Water Level Stats:",
                  choices = sort(unique(wl_stats$site)),
                  selected = "Great Meadow 1",
                  multiple = TRUE,
                  options = list(
                    `actions-box` = TRUE,
                    `deselect-all-text` = "Clear all",
                    `select-all-text` = "Select all",
                    `none-selected-text` = "Choose site(s)"
                  )),
      pickerInput("stats_year", "Select Year(s) for Stats:",
                  choices = sort(unique(wl_stats$year)),
                  selected = c(2022, 2023),
                  multiple = TRUE,
                  options = list(`actions-box` = TRUE,
                                 `deselect-all-text` = "Clear all",
                                 `select-all-text` = "Select all",
                                 `none-selected-text` = "Choose year(s)"))
    ),
    mainPanel(
      dataTableOutput("wl_stats")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive data for plotting
  plot_data <- reactive({
    req(input$year, input$gm_site)
    
    all_data %>%
      filter(Year %in% input$year, doy > 134, doy < 275) %>%
      filter(site %in% c(input$gm_site, "Gilmore Meadow"))
  })
  
  output$hydrograph <- renderPlot({
    plot_data_filtered <- plot_data()
    req(nrow(plot_data_filtered) > 0)
    
    # Calculate precipitation offset for each year
    precip_data <- plot_data_filtered %>%
      group_by(Year) %>%
      summarise(
        min_water = min(c(
          water.depth[site == input$gm_site & !is.na(water.depth)],
          water.depth[site == "Gilmore Meadow" & !is.na(water.depth)]
        ), na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      left_join(plot_data_filtered, by = "Year") %>%
      mutate(precip_y = lag.precip * 5 + min_water)
    
    global_min_water <- min(plot_data_filtered$water.depth, na.rm = TRUE)
    
    # Create the plot using facet_wrap instead of patchwork
    ggplot() +
      # Water level lines
      geom_line(data = plot_data_filtered %>% filter(site == input$gm_site),
                aes(x = doy_h, y = water.depth, color = input$gm_site), 
                size = 0.7) +
      geom_line(data = plot_data_filtered %>% filter(site == "Gilmore Meadow"),
                aes(x = doy_h, y = water.depth, color = "Gilmore Meadow"), 
                size = 0.7) +
      # Precipitation line
      geom_line(data = precip_data,
                aes(x = doy_h, y = precip_y, color = "Precipitation"), size = 0.7) +
      # Ground level reference
      geom_hline(yintercept = 0, color = "brown") +
      
      # Facet by year
      facet_wrap(~ Year, ncol = 1, scales = "free_y") +
      
      # Styling
      scale_color_manual(values = c(
        setNames("black", input$gm_site),
        "Gilmore Meadow" = "darkgray",
        "Precipitation" = "blue"
      )) +
      labs(title = "Hydrographs by Year",
           x = "Date", y = "Water Level (cm)") +
      scale_x_continuous(
        breaks = c(121, 152, 182, 213, 244, 274),
        labels = c('May-01', 'Jun-01', 'Jul-01', 'Aug-01', 'Sep-01', 'Oct-01')) +
      scale_y_continuous(
        name = "Water Level (cm)",
        sec.axis = sec_axis(~ (. - global_min_water) / 5,
                            name = "Hourly Precip. (cm)",
                            breaks = seq(0, 8, by = 2))) +
      theme_bw() +
      theme(legend.position = "bottom",
            legend.title = element_blank(),
            strip.text = element_text(size = 11),
            plot.title = element_text(hjust = 0.5))
  })
  
  # Selected data output table - Now with separate columns for each site
  output$brush_info <- renderTable({
    req(input$hydro_brush)
    
    plot_data_filtered <- plot_data()
    req(nrow(plot_data_filtered) > 0)
    
    # Use brushedPoints function which works properly with faceted plots
    selected_data <- brushedPoints(
      plot_data_filtered,
      brush = input$hydro_brush,
      xvar = "doy_h",
      yvar = "water.depth"
    ) %>%
      select(site, Year, doy_h, timestamp, water.depth, precip_cm) %>%
      arrange(Year, doy_h) %>%
      # Round for better display
      mutate(
        doy_h = round(doy_h, 2),
        water.depth = round(water.depth, 2),
        precip_cm = round(precip_cm, 3),
        timestamp = format(timestamp, "%Y-%m-%d %H:%M:%S")
      )
    
    if(nrow(selected_data) == 0) {
      data.frame(Message = "No points selected - try brushing over the water level lines")
    } else {
      # Create a proper time grouping to match data points from both sites
      # Group by rounded doy_h to combine data from both sites at similar times
      pivoted_data <- selected_data %>%
        # Round doy_h slightly to group nearby times together
        mutate(doy_h_rounded = round(doy_h, 1)) %>%
        # Group by the rounded time
        group_by(Year, doy_h_rounded) %>%
        summarise(
          # Use the first timestamp and precipitation value in each group
          timestamp = first(timestamp),
          doy_h = first(doy_h),
          precip_cm = first(precip_cm),
          # Create separate columns for each site's water depth
          great_meadow_depth = ifelse(any(site == input$gm_site), 
                                      water.depth[site == input$gm_site][1], NA),
          gilmore_depth = ifelse(any(site == "Gilmore Meadow"), 
                                 water.depth[site == "Gilmore Meadow"][1], NA),
          .groups = 'drop'
        ) %>%
        # Remove the grouping column and rename for display
        select(-doy_h_rounded) %>%
        rename(
          `Timestamp` = timestamp,
          `Day of Year` = doy_h,
          `Precipitation (cm)` = precip_cm
        ) %>%
        # Create dynamic column names based on selected site
        rename_with(~ paste(input$gm_site, "Water Depth (cm)"), .cols = great_meadow_depth) %>%
        rename_with(~ "Gilmore Meadow Water Depth (cm)", .cols = gilmore_depth) %>%
        # Arrange by year and day
        arrange(Year, `Day of Year`)
      
      pivoted_data
    }
  })
  
  # WL stats output table
  output$wl_stats <- DT::renderDataTable({
    req(input$stats_site, input$stats_year)
    
    wl_stats %>%
      filter(site %in% input$stats_site, year %in% input$stats_year) %>% 
      mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
      select(
        Year = year,
        Site = site,
        `Mean Water Level` = WL_mean,
        `SD Water Level` = WL_sd,
        `Minimum Water Level` = WL_min,
        `Maximum Water Level` = WL_max,
        `Maximum Hourly Increase` = max_inc,
        `Maximum Hourly Decrease` = max_dec,
        `Growing Season Change` = GS_change,
        `GS % Surface Water` = prop_over_0cm,
        `GS % Within 30cm` = prop_bet_0_neg30cm,
        `GS % Over 30cm Deep` = prop_under_neg30cm,
        `GS % Complete Data` = prop_GS_comp
      )
  }, options = list(pageLength = 10, scrollX = TRUE))
}

# Run app
shinyApp(ui, server)





#-------------------------------------------------------------------------------


library(shiny)
library(tidyverse)
library(lubridate)
library(patchwork)
library(shinyWidgets)
library(DT)

# Read & prepare your processed data (from your current script)
gm <- read.csv("data/processed_data/great_meadow_well_data_2024_20250715.csv") %>%
  rename(Date = date, Year = year, precip_cm = precip.cm) %>%
  mutate(Date = as.Date(Date),
         timestamp = as_datetime(timestamp),
         site = paste("Great Meadow", plot.num),
         water.depth = case_when(
           Year == 2016 & doy_h == 159.12 & plot.num == 3 & water.depth < -120 ~ NA_real_,
           Year == 2017 & doy_h == 215.02 & plot.num == 6 & water.depth < -115 ~ NA_real_,
           Year == 2021 & plot.num == 3 & doy == 224 & water.depth > 400 ~ NA_real_,
           Year == 2021 & plot.num == 3 & doy == 225 & water.depth > 400 ~ NA_real_,
           TRUE ~ water.depth
         )) %>%
  mutate(siteyear = paste(site, Year, sep = "_"))

gl <- read.csv("data/raw_data/hydrology_data/gilmore_well_prec_data_2013-2024.csv") %>%
  rename(water.depth = GILM_WL) %>%
  select(-timestamp) %>%  # drop the all-NA column
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"),
         site = "Gilmore Meadow",
         siteyear = paste(site, Year, sep = "_"))

all_data <- bind_rows(gm, gl) %>% 
  filter(Year >= 2016 & Year <= 2023) 

wl_stats <- read.csv("data/processed_data/gm_gl_wl_stats.csv") %>% 
  select(year,
         stat, 
         Gilmore Meadow = gilmore.meadow, 
         Great Meadow 1 = great.meadow.1, 
         Great Meadow 2 = great.meadow.2, 
         Great Meadow 3 = great.meadow.3, 
         Great Meadow 4 = great.meadow.4, 
         Great Meadow 5 = great.meadow.5, 
         Great Meadow 6 = great.meadow.6) %>% 
  pivot_longer(cols = -c(year, stat), names_to = "site", values_to = "value") %>% 
  pivot_wider(names_from = stat, values_from = value) %>%
  arrange(site, year)

# UI
ui <- fluidPage(
  titlePanel("Wetland Hydrograph Viewer"),
  sidebarLayout(
    sidebarPanel(
      pickerInput("year", "Select Year(s):",
                  choices = sort(unique(all_data$Year)),
                  selected = 2023,
                  multiple = TRUE,
                  options = list(actions-box = TRUE,
                                 deselect-all-text = "Clear all",
                                 select-all-text = "Select all",
                                 none-selected-text = "Choose year(s)")),
      selectInput("gm_site", "Select Great Meadow Plot for Hydrograph:",
                  choices = sort(unique(all_data$site[grepl("Great Meadow", all_data$site)])),
                  selected = "Great Meadow 1"),
      
      pickerInput("site", "Select Site(s) for Water Level Stats:",
                  choices = sort(unique(wl_stats$site)),
                  selected = "Great Meadow 1",
                  multiple = TRUE,
                  options = list(
                    actions-box = TRUE,
                    deselect-all-text = "Clear all",
                    select-all-text = "Select all",
                    none-selected-text = "Choose site(s)"
                  ))
    ),
    mainPanel(
      plotOutput("hydrograph", height = "600px")
    )
  ),
  
  fluidRow(
    column(width = 10, offset = 1, align = "center",
           h4("Growing Season Water Level Statistics"),
           dataTableOutput("wl_stats")
    )
  )
)

# Server
server <- function(input, output, session) {
  output$hydrograph <- renderPlot({
    req(input$year, input$gm_site)
    
    plot_list <- lapply(input$year, function(yr) {
      plot_data_year <- all_data %>%
        filter(Year == yr, doy > 134, doy < 275)
      
      # Calculate min water level for consistent precipitation offset
      min_gm <- plot_data_year %>%
        filter(site == input$gm_site, !is.na(water.depth)) %>%
        pull(water.depth) %>% 
        min(na.rm = TRUE)
      
      min_gil <- plot_data_year %>%
        filter(site == "Gilmore Meadow", !is.na(water.depth)) %>%
        pull(water.depth) %>% 
        min(na.rm = TRUE)
      
      minWL <- min(min_gm, min_gil)
      
      # Create individual hydrograph plot
      ggplot() +
        geom_line(data = plot_data_year %>% filter(site == input$gm_site),
                  aes(x = doy_h, y = water.depth, color = input$gm_site), size = 0.7) +
        geom_line(data = plot_data_year %>% filter(site == "Gilmore Meadow"),
                  aes(x = doy_h, y = water.depth, color = "Gilmore Meadow"), size = 0.7) +
        geom_line(data = plot_data_year,
                  aes(x = doy_h, y = lag.precip * 5 + minWL),
                  color = "blue", size = 0.7) +
        geom_hline(yintercept = 0, color = "brown") +
        scale_color_manual(values = setNames(c("black", "darkgray"), 
                                             c(input$gm_site, "Gilmore Meadow"))) +
        labs(title = paste("Hydrograph for", yr),
             x = "Date", y = "Water Level (cm)") +
        scale_x_continuous(
          breaks = c(121, 152, 182, 213, 244, 274),
          labels = c('May-01', 'Jun-01', 'Jul-01', 'Aug-01', 'Sep-01', 'Oct-01')) +
        scale_y_continuous(
          name = "Water Level (cm)",
          sec.axis = sec_axis(~ (. - minWL) / 5,
                              name = "Hourly Precip (cm)",
                              breaks = seq(0, 8, by = 2))
        ) +
        theme_bw() +
        theme(legend.position = "bottom",
              legend.title = element_blank(),
              strip.text = element_text(size = 11),
              plot.title = element_text(hjust = 0.5))
    })
    
    # Combine plots vertically using patchwork
    wrap_plots(plot_list, ncol = 1)
  })
  
  output$wl_stats <- DT::renderDataTable({
    req(input$site, input$year)
    
    wl_stats %>%
      filter(site %in% input$site, year %in% input$year) %>% 
      mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
      select(
        Year = year,
        Site = site,
        Mean Water Level = WL_mean,
        SD Water Level = WL_sd,
        Minimum Water Level = WL_min,
        Maximum Water Level = WL_max,
        Maximum Hourly Increase = max_inc,
        Maximum Hourly Decrease = max_dec,
        Growing Season Change = GS_change,
        GS % Complete Data = prop_GS_comp,
        GS % Surface Water = prop_over_0cm,
        GS % Within 30cm = prop_bet_0_neg30cm,
        GS % Over 30cm Deep = prop_under_neg30cm
      )
  })
  
  
}



# Run app
shinyApp(ui, server)




--------------------------------------------------------------------------------



# Load packages
library(shiny)
library(tidyverse)
library(lubridate)
library(patchwork)
library(shinyWidgets)
library(DT)
library(plotly)

# Read & prepare your processed data (from your current script)
gm <- read.csv("data/processed_data/great_meadow_well_data_2024_20250715.csv") %>%
  rename(Date = date, Year = year, precip_cm = precip.cm) %>%
  mutate(Date = as.Date(Date),
         timestamp = as_datetime(timestamp),
         site = paste("Great Meadow", plot.num),
         water.depth = case_when(
           Year == 2016 & doy_h == 159.12 & plot.num == 3 & water.depth < -120 ~ NA_real_,
           Year == 2017 & doy_h == 215.02 & plot.num == 6 & water.depth < -115 ~ NA_real_,
           Year == 2021 & plot.num == 3 & doy == 224 & water.depth > 400 ~ NA_real_,
           Year == 2021 & plot.num == 3 & doy == 225 & water.depth > 400 ~ NA_real_,
           TRUE ~ water.depth
         )) %>%
  mutate(siteyear = paste(site, Year, sep = "_"))

gl <- read.csv("data/raw_data/hydrology_data/gilmore_well_prec_data_2013-2024.csv") %>%
  rename(water.depth = GILM_WL) %>%
  select(-timestamp) %>%  # drop the all-NA column
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"),
         site = "Gilmore Meadow",
         siteyear = paste(site, Year, sep = "_"))

all_data <- bind_rows(gm, gl) %>% 
  filter(Year >= 2016 & Year <= 2023) 

wl_stats <- read.csv("data/processed_data/gm_gl_wl_stats.csv") %>% 
  select(year,
         stat, 
         `Gilmore Meadow` = gilmore.meadow, 
         `Great Meadow 1` = great.meadow.1, 
         `Great Meadow 2` = great.meadow.2, 
         `Great Meadow 3` = great.meadow.3, 
         `Great Meadow 4` = great.meadow.4, 
         `Great Meadow 5` = great.meadow.5, 
         `Great Meadow 6` = great.meadow.6) %>% 
  pivot_longer(cols = -c(year, stat), names_to = "site", values_to = "value") %>% 
  pivot_wider(names_from = stat, values_from = value) %>%
  arrange(site, year)

# UI
ui <- fluidPage(
  titlePanel("Wetland Hydrograph Viewer"),
  sidebarLayout(
    sidebarPanel(
      pickerInput("year", "Select Year(s):",
                  choices = sort(unique(all_data$Year)),
                  selected = 2023,
                  multiple = TRUE,
                  options = list(`actions-box` = TRUE,
                                 `deselect-all-text` = "Clear all",
                                 `select-all-text` = "Select all",
                                 `none-selected-text` = "Choose year(s)")),
      selectInput("gm_site", "Select Great Meadow Plot for Hydrograph:",
                  choices = sort(unique(all_data$site[grepl("Great Meadow", all_data$site)])),
                  selected = "Great Meadow 1"),
      
      pickerInput("site", "Select Site(s) for Water Level Stats:",
                  choices = sort(unique(wl_stats$site)),
                  selected = "Great Meadow 1",
                  multiple = TRUE,
                  options = list(
                    `actions-box` = TRUE,
                    `deselect-all-text` = "Clear all",
                    `select-all-text` = "Select all",
                    `none-selected-text` = "Choose site(s)"
                  ))
    ),
    mainPanel(
      plotlyOutput("hydrograph", height = "600px"),
      br(),
      downloadButton("download_points", "Download Selected Points"),
      br(), br(),
      DTOutput("selected_points"),
      br(), br(),
      fluidRow(
        column(width = 10, offset = 1,
               div(style = "text-align: center;",
                   h4("Growing Season Water Level Statistics")),
               DTOutput("wl_stats"))
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  output$hydrograph <- renderPlot({
    req(input$year, input$gm_site)
    
    plot_data <- all_data %>%
      filter(Year %in% input$year,
             doy > 134, doy < 275,
             site %in% c(input$gm_site, "Gilmore Meadow")) %>%
      mutate(row_id = row_number())  # assign temporary row id
    
    # Store selected plot data in a reactive for reuse
    plot_data_reactive <- reactive({
      plot_data
    })
    
    p <- ggplot(plot_data, aes(x = doy_h, y = water.depth, color = site, key = row_id)) +
      geom_line(size = 0.7) +
      geom_point(size = 1.5, alpha = 0.7) +
      geom_hline(yintercept = 0, color = "brown") +
      scale_color_manual(values = c("black", "darkgray")) +
      labs(title = "Interactive Hydrograph", x = "DOY", y = "Water Level (cm)") +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    ggplotly(p, tooltip = c("x", "y", "color", "key")) %>%
      layout(dragmode = "select")
  })
  
  output$selected_points <- renderDataTable({
    sel <- event_data("plotly_selected")
    if (is.null(sel)) return(NULL)
    
    ids <- sel$key
    plot_data_reactive() %>% filter(row_id %in% ids)
  })
  
  output$download_points <- downloadHandler(
    filename = function() {
      paste0("selected_points_", Sys.Date(), ".csv")
    },
    content = function(file) {
      sel <- event_data("plotly_selected")
      if (is.null(sel)) return()
      
      ids <- sel$key
      data_out <- plot_data_reactive() %>% filter(row_id %in% ids)
      
      write.csv(data_out, file, row.names = FALSE)
    }
  )
  
  output$wl_stats <- DT::renderDataTable({
    req(input$site, input$year)
    
    wl_stats %>%
      filter(site %in% input$site, year %in% input$year) %>% 
      mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
      select(
        Year = year,
        Site = site,
        `Mean Water Level` = WL_mean,
        `SD Water Level` = WL_sd,
        `Minimum Water Level` = WL_min,
        `Maximum Water Level` = WL_max,
        `Maximum Hourly Increase` = max_inc,
        `Maximum Hourly Decrease` = max_dec,
        `Growing Season Change` = GS_change,
        `GS % Complete Data` = prop_GS_comp,
        `GS % Surface Water` = prop_over_0cm,
        `GS % Within 30cm` = prop_bet_0_neg30cm,
        `GS % Over 30cm Deep` = prop_under_neg30cm
      )
  })
  
  
}



# Run app
shinyApp(ui, server)




--------------------------------------------------------------------------------
  
  
  
  
  
  # Load packages
  library(shiny)
library(tidyverse)
library(lubridate)
library(patchwork)
library(shinyWidgets)
library(DT)

# Read & prepare your processed data
gm <- read.csv("data/processed_data/great_meadow_well_data_2024_20250715.csv") %>%
  rename(Date = date, Year = year, precip_cm = precip.cm) %>%
  mutate(Date = as.Date(Date),
         timestamp = as_datetime(timestamp),
         site = paste("Great Meadow", plot.num),
         water.depth = case_when(
           Year == 2016 & doy_h == 159.12 & plot.num == 3 & water.depth < -120 ~ NA_real_,
           Year == 2017 & doy_h == 215.02 & plot.num == 6 & water.depth < -115 ~ NA_real_,
           Year == 2021 & plot.num == 3 & doy == 224 & water.depth > 400 ~ NA_real_,
           Year == 2021 & plot.num == 3 & doy == 225 & water.depth > 400 ~ NA_real_,
           TRUE ~ water.depth
         )) %>%
  mutate(siteyear = paste(site, Year, sep = "_"))

gl <- read.csv("data/raw_data/hydrology_data/gilmore_well_prec_data_2013-2024.csv") %>%
  rename(water.depth = GILM_WL) %>%
  select(-timestamp) %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"),
         site = "Gilmore Meadow",
         siteyear = paste(site, Year, sep = "_"))

all_data <- bind_rows(gm, gl) %>%
  filter(Year >= 2016 & Year <= 2023)

wl_stats <- read.csv("data/processed_data/gm_gl_wl_stats.csv") %>%
  select(year,
         stat,
         `Gilmore Meadow` = gilmore.meadow,
         `Great Meadow 1` = great.meadow.1,
         `Great Meadow 2` = great.meadow.2,
         `Great Meadow 3` = great.meadow.3,
         `Great Meadow 4` = great.meadow.4,
         `Great Meadow 5` = great.meadow.5,
         `Great Meadow 6` = great.meadow.6) %>%
  pivot_longer(cols = -c(year, stat), names_to = "site", values_to = "value") %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  arrange(site, year)

# UI
ui <- fluidPage(
  titlePanel("Wetland Hydrograph Viewer"),
  sidebarLayout(
    sidebarPanel(
      pickerInput("year", "Select Year(s):",
                  choices = sort(unique(all_data$Year)),
                  selected = 2023,
                  multiple = TRUE,
                  options = list(`actions-box` = TRUE,
                                 `deselect-all-text` = "Clear all",
                                 `select-all-text` = "Select all",
                                 `none-selected-text` = "Choose year(s)")),
      selectInput("gm_site", "Select Great Meadow Plot for Hydrograph:",
                  choices = sort(unique(all_data$site[grepl("Great Meadow", all_data$site)])),
                  selected = "Great Meadow 1"),
      
      pickerInput("site", "Select Site(s) for Water Level Stats:",
                  choices = sort(unique(wl_stats$site)),
                  selected = "Great Meadow 1",
                  multiple = TRUE,
                  options = list(
                    `actions-box` = TRUE,
                    `deselect-all-text` = "Clear all",
                    `select-all-text` = "Select all",
                    `none-selected-text` = "Choose site(s)"
                  )),
      br(),
      downloadButton("download_points", "Download Selected Points")
    ),
    mainPanel(
      plotOutput("hydrograph", height = "600px", brush = brushOpts(id = "plot_brush")),
      br(),
      h4("Selected Points from Hydrograph"),
      DTOutput("selected_points"),
      br()
    ),
    fluidRow(
      column(width = 10, offset = 1, align = "center",
             h4("Growing Season Water Level Statistics"),
             dataTableOutput("wl_stats"))
    )
  )
)

# Server
server <- function(input, output, session) {
  output$hydrograph <- renderPlot({
    req(input$year, input$gm_site)
    
    plot_list <- lapply(input$year, function(yr) {
      plot_data_year <- all_data %>%
        filter(Year == yr, doy > 134, doy < 275)
      
      min_gm <- plot_data_year %>%
        filter(site == input$gm_site, !is.na(water.depth)) %>%
        pull(water.depth) %>%
        min(na.rm = TRUE)
      
      min_gil <- plot_data_year %>%
        filter(site == "Gilmore Meadow", !is.na(water.depth)) %>%
        pull(water.depth) %>%
        min(na.rm = TRUE)
      
      minWL <- min(min_gm, min_gil)
      
      ggplot() +
        geom_line(data = plot_data_year %>% filter(site == input$gm_site),
                  aes(x = doy_h, y = water.depth, color = input$gm_site), size = 0.7) +
        geom_line(data = plot_data_year %>% filter(site == "Gilmore Meadow"),
                  aes(x = doy_h, y = water.depth, color = "Gilmore Meadow"), size = 0.7) +
        geom_line(data = plot_data_year,
                  aes(x = doy_h, y = lag.precip * 5 + minWL),
                  color = "blue", size = 0.7) +
        geom_hline(yintercept = 0, color = "brown") +
        scale_color_manual(values = setNames(c("black", "darkgray"),
                                             c(input$gm_site, "Gilmore Meadow"))) +
        labs(title = paste("Hydrograph for", yr),
             x = "Date", y = "Water Level (cm)") +
        scale_x_continuous(
          breaks = c(121, 152, 182, 213, 244, 274),
          labels = c('May-01', 'Jun-01', 'Jul-01', 'Aug-01', 'Sep-01', 'Oct-01')) +
        scale_y_continuous(
          name = "Water Level (cm)",
          sec.axis = sec_axis(~ (. - minWL) / 5,
                              name = "Hourly Precip (cm)",
                              breaks = seq(0, 8, by = 2))
        ) +
        theme_bw() +
        theme(legend.position = "bottom",
              legend.title = element_blank(),
              plot.title = element_text(hjust = 0.5))
    })
    
    wrap_plots(plot_list, ncol = 1)
  })
  
  output$wl_stats <- DT::renderDataTable({
    req(input$site, input$year)
    
    wl_stats %>%
      filter(site %in% input$site, year %in% input$year) %>%
      mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
      select(
        Year = year,
        Site = site,
        `Mean Water Level` = WL_mean,
        `SD Water Level` = WL_sd,
        `Minimum Water Level` = WL_min,
        `Maximum Water Level` = WL_max,
        `Maximum Hourly Increase` = max_inc,
        `Maximum Hourly Decrease` = max_dec,
        `Growing Season Change` = GS_change,
        `GS % Complete Data` = prop_GS_comp,
        `GS % Surface Water` = prop_over_0cm,
        `GS % Within 30cm` = prop_bet_0_neg30cm,
        `GS % Over 30cm Deep` = prop_under_neg30cm
      )
  })
  
  # Reactive: Filtered dataset for brushing
  brushed_data <- reactive({
    req(input$plot_brush)
    
    brushedPoints(
      all_data %>% filter(site %in% c("Gilmore Meadow", input$gm_site),
                          Year %in% input$year,
                          doy > 134, doy < 275),
      input$plot_brush,
      xvar = "doy_h",
      yvar = "water.depth"
    )
  })
  
  output$selected_points <- renderDT({
    req(brushed_data())
    brushed_data() %>%
      select(Date, site, Year, doy, doy_h, water.depth, lag.precip) %>%
      arrange(site, Year, doy_h) %>%
      datatable(options = list(pageLength = 10))
  })
  
  # Download handler
  output$download_points <- downloadHandler(
    filename = function() {
      paste0("selected_points_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(brushed_data(), file, row.names = FALSE)
    }
  )
}

# Run the app
shinyApp(ui, server)




#----------------------------------------------------------------------------
  



library(shiny)
library(tidyverse)
library(lubridate)
library(patchwork)
library(shinyWidgets)
library(DT)
library(shinythemes)
library(shinydashboard)

# Read & prepare your processed data (from your current script)
gm <- read.csv("data/processed_data/great_meadow_well_data_2024_20250715.csv") %>%
  rename(Date = date, Year = year, precip_cm = precip.cm) %>%
  mutate(Date = as.Date(Date),
         timestamp = as_datetime(timestamp),
         site = paste("Great Meadow", plot.num),
         water.depth = case_when(
           Year == 2016 & doy_h == 159.12 & plot.num == 3 & water.depth < -120 ~ NA_real_,
           Year == 2017 & doy_h == 215.02 & plot.num == 6 & water.depth < -115 ~ NA_real_,
           Year == 2021 & plot.num == 3 & doy == 224 & water.depth > 400 ~ NA_real_,
           Year == 2021 & plot.num == 3 & doy == 225 & water.depth > 400 ~ NA_real_,
           TRUE ~ water.depth
         )) %>%
  mutate(siteyear = paste(site, Year, sep = "_"))

gl <- read.csv("data/raw_data/hydrology_data/gilmore_well_prec_data_2013-2024.csv") %>%
  rename(water.depth = GILM_WL) %>%
  mutate(
    site = "Gilmore Meadow",
    timestamp = mdy_hm(timestamp),
    timestamp = if_else(is.na(timestamp), as.POSIXct(mdy(Date)), timestamp),
    timestamp = as.POSIXct(timestamp, tz = "UTC"),
    Date = as.Date(mdy(Date))
  ) %>%
  select(timestamp, 
         Date,
         doy,
         Year,
         precip_cm,
         water.depth,
         lag.precip,
         hr,
         doy_h,
         site)

all_data <- bind_rows(gm, gl) %>% 
  filter(Year >= 2016 & Year <= 2023) 

wl_stats <- read.csv("data/processed_data/gm_gl_wl_stats.csv") %>% 
  select(year,
         stat, 
         `Gilmore Meadow` = gilmore.meadow, 
         `Great Meadow 1` = great.meadow.1, 
         `Great Meadow 2` = great.meadow.2, 
         `Great Meadow 3` = great.meadow.3, 
         `Great Meadow 4` = great.meadow.4, 
         `Great Meadow 5` = great.meadow.5, 
         `Great Meadow 6` = great.meadow.6) %>% 
  pivot_longer(cols = -c(year, stat), names_to = "site", values_to = "value") %>% 
  pivot_wider(names_from = stat, values_from = value) %>%
  arrange(site, year)


# UI
ui <- fluidPage(
  theme = shinytheme("flatly"),  # Try "flatly", "cosmo", "cerulean", etc.
  
  tags$head(
    tags$style(HTML("
      .shiny-input-container {
        margin-bottom: 15px;
      }
      .box {
        box-shadow: 2px 2px 8px rgba(0,0,0,0.1);
        border-radius: 10px;
      }
      .box-title {
        font-weight: bold;
        font-size: 18px;
      }
    "))
  ),
  
  titlePanel(tagList(icon("tint"), "Wetland Hydrograph Visualizer")),
  br(),
  
  fluidRow(
    box(width = 3, title = "Hydrograph Controls", solidHeader = TRUE, status = "primary",
        pickerInput("year", "Select Year(s):", choices = NULL, 
                    multiple = TRUE, options = list(`actions-box` = TRUE)),
        selectInput("gm_site", "Select Site to Compare to Gilmore Meadow:", choices = NULL)
    ),
    
    box(width = 9, title = "Hydrograph", solidHeader = TRUE, status = "info",
        plotOutput("hydrograph", height = "450px",
                   brush = brushOpts(id = "plot_brush", direction = "x"))
    )
  ),
  
  fluidRow(
    box(width = 12, title = "Selected Data Points", solidHeader = TRUE, status = "warning",
        tableOutput("brush_info")
    )
  ),
  
  fluidRow(
    box(width = 3, title = "Statistics Controls", solidHeader = TRUE, status = "success",
        pickerInput("site_stats", "Select Site(s) for Stats:", choices = NULL, 
                    multiple = TRUE, options = list(`actions-box` = TRUE)),
        pickerInput("year_stats", "Select Year(s) for Stats:", choices = NULL, 
                    multiple = TRUE, options = list(`actions-box` = TRUE))
    ),
    
    box(width = 9, title = "Water Level Statistics", solidHeader = TRUE, status = "success",
        dataTableOutput("wl_stats")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Populate input choices
  observe({
    updatePickerInput(session, "year", choices = sort(unique(all_data$Year)))
    updateSelectInput(session, "gm_site", choices = unique(all_data$site[all_data$site != "Gilmore Meadow"]))
    updatePickerInput(session, "site_stats", choices = unique(all_data$site))
    updatePickerInput(session, "year_stats", choices = sort(unique(all_data$Year)))
  })
  
  # Reactive: Filtered hydrograph data
  plot_data <- reactive({
    req(input$year, input$gm_site)
    all_data %>%
      filter(
        Year %in% input$year,
        doy > 134, doy < 275,
        site %in% c(input$gm_site, "Gilmore Meadow")
      )
  })
  
  # Reactive: Precip offset (scaled to water depth)
  precip_data <- reactive({
    d <- plot_data()
    
    d %>%
      group_by(Year) %>%
      summarise(
        min_water = min(water.depth[!is.na(water.depth)], na.rm = TRUE),
        .groups = "drop"
      ) %>%
      left_join(d, by = "Year") %>%
      mutate(precip_y = lag.precip * 5 + min_water)
  })
  
  # Hydrograph Plot
  output$hydrograph <- renderPlot({
    d <- plot_data()
    p <- precip_data()
    req(nrow(d) > 0)
    
    global_min <- min(d$water.depth, na.rm = TRUE)
    
    ggplot() +
      geom_line(data = d %>% filter(site == input$gm_site),
                aes(x = doy_h, y = water.depth, color = input$gm_site),
                size = 0.7) +
      geom_line(data = d %>% filter(site == "Gilmore Meadow"),
                aes(x = doy_h, y = water.depth, color = "Gilmore Meadow"),
                size = 0.7) +
      geom_line(data = p,
                aes(x = doy_h, y = precip_y),
                color = "blue", size = 0.7) +
      geom_hline(yintercept = 0, color = "brown") +
      facet_wrap(~ Year, ncol = 1, scales = "free_y") +
      scale_color_manual(
        values = setNames(
          c("black", "darkgray"),
          c(input$gm_site, "Gilmore Meadow")
        )
      ) +
      scale_x_continuous(
        breaks = c(121, 152, 182, 213, 244, 274),
        labels = c('May-01', 'Jun-01', 'Jul-01', 'Aug-01', 'Sep-01', 'Oct-01')
      ) +
      scale_y_continuous(
        name = "Water Level (cm)",
        sec.axis = sec_axis(~ (. - global_min) / 5,
                            name = "Hourly Precip. (cm)",
                            breaks = seq(0, 8, by = 2))
      ) +
      labs(title = "Hydrographs by Year",
           x = "Day of Year",
           y = "Water Level (cm)") +
      theme_bw() +
      theme(
        legend.position = "bottom",
        legend.title = element_blank(),
        strip.text = element_text(size = 11),
        plot.title = element_text(hjust = 0.5)
      )
  })
  
  # Table: Selected points via brush
  output$brush_info <- renderTable({
    d <- plot_data()
    brushedPoints(d, input$plot_brush) %>%
      select(site, timestamp, Year, water.depth, lag.precip) %>%
      arrange(timestamp)
  })
  
  # Stats: Calculate water level summaries
  output$wl_stats <- renderDataTable({
    req(input$site_stats, input$year_stats)
    
    stats_data <- all_data %>%
      filter(site %in% input$site_stats, Year %in% input$year_stats)
    
    calc_WL_stats(stats_data)
  })
}


# Run app
shinyApp(ui, server) 



#-------------------------------------------------------------------------------



library(shiny)
library(tidyverse)
library(lubridate)
library(patchwork)
library(shinyWidgets)
library(DT)
library(bslib)

# Read & prepare your processed data (from your current script)
gm <- read.csv("data/processed_data/great_meadow_well_data_2024_20250715.csv") %>%
  rename(Date = date, Year = year, precip_cm = precip.cm) %>%
  mutate(Date = as.Date(Date),
         timestamp = as_datetime(timestamp),
         site = paste("Great Meadow", plot.num),
         water.depth = case_when(
           Year == 2016 & doy_h == 159.12 & plot.num == 3 & water.depth < -120 ~ NA_real_,
           Year == 2017 & doy_h == 215.02 & plot.num == 6 & water.depth < -115 ~ NA_real_,
           Year == 2021 & plot.num == 3 & doy == 224 & water.depth > 400 ~ NA_real_,
           Year == 2021 & plot.num == 3 & doy == 225 & water.depth > 400 ~ NA_real_,
           TRUE ~ water.depth
         )) %>%
  mutate(siteyear = paste(site, Year, sep = "_"))

gl <- read.csv("data/raw_data/hydrology_data/gilmore_well_prec_data_2013-2024.csv") %>%
  rename(water.depth = GILM_WL) %>%
  mutate(
    site = "Gilmore Meadow",
    
    # Try to parse full datetime, fallback to date only + midnight
    timestamp_parsed = mdy_hm(timestamp),
    timestamp_parsed = if_else(
      is.na(timestamp_parsed),
      mdy(timestamp),
      timestamp_parsed
    ),
    timestamp_parsed = as.POSIXct(timestamp_parsed),
    timestamp = timestamp_parsed,
    
    # Standardize Date column too
    Date = mdy(Date)
  ) %>%
  select(timestamp, 
         Date,
         doy,
         Year,
         precip_cm,
         water.depth,
         lag.precip,
         hr,
         doy_h,
         site)

all_data <- bind_rows(gm, gl) %>% 
  filter(Year >= 2016 & Year <= 2023) 

wl_stats <- read.csv("data/processed_data/gm_gl_wl_stats.csv") %>% 
  select(year,
         stat, 
         `Gilmore Meadow` = gilmore.meadow, 
         `Great Meadow 1` = great.meadow.1, 
         `Great Meadow 2` = great.meadow.2, 
         `Great Meadow 3` = great.meadow.3, 
         `Great Meadow 4` = great.meadow.4, 
         `Great Meadow 5` = great.meadow.5, 
         `Great Meadow 6` = great.meadow.6) %>% 
  pivot_longer(cols = -c(year, stat), names_to = "site", values_to = "value") %>% 
  pivot_wider(names_from = stat, values_from = value) %>%
  arrange(site, year)


# UI with improved styling
ui <- page_fluid(
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#2C5F41",
    secondary = "#5A9B7C", 
    success = "#27ae60",
    info = "#3498db",
    warning = "#f39c12",
    danger = "#e74c3c",
    base_font = font_google("Open Sans"),
    heading_font = font_google("Roboto", wght = c(400, 700))
  ),
  
  # Custom CSS for additional styling
  tags$head(
    tags$style(HTML("
      .content-section {
        margin: 30px 0;
        padding: 25px;
        border-radius: 15px;
        box-shadow: 0 4px 12px rgba(0,0,0,0.1);
        background: linear-gradient(135deg, #ffffff 0%, #f8f9fa 100%);
        border-left: 5px solid #2C5F41;
      }
      
      .section-title {
        color: #2C5F41;
        font-weight: 600;
        margin-bottom: 20px;
        padding-bottom: 10px;
        border-bottom: 2px solid #5A9B7C;
      }
      
      .sidebar-custom {
        background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%);
        border-radius: 10px;
        padding: 20px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.08);
        border: 1px solid #dee2e6;
      }
      
      .main-title {
        background: linear-gradient(135deg, #2C5F41 0%, #5A9B7C 100%);
        color: white;
        padding: 30px;
        margin: -15px -15px 30px -15px;
        text-align: center;
        border-radius: 0 0 20px 20px;
        box-shadow: 0 4px 12px rgba(0,0,0,0.15);
      }
      
      .brush-info-section {
        background: linear-gradient(135deg, #e8f4f8 0%, #f0f8ff 100%);
        border-radius: 12px;
        padding: 20px;
        margin: 20px 0;
        border: 1px solid #5A9B7C;
      }
      
      .stats-main {
        background: linear-gradient(135deg, #f8f9fa 0%, #ffffff 100%);
      }
    "))
  ),
  
  # Main title with gradient background
  div(class = "main-title",
      h1("Wetland Hydrograph Visualizer", 
         style = "margin: 0; font-size: 2.5rem; text-shadow: 2px 2px 4px rgba(0,0,0,0.3)")
  ),
  
  # First section: Hydrograph with improved styling
  div(class = "content-section",
      h2("Wetland Hydrograph Analysis", class = "section-title"),
      layout_sidebar(
        sidebar = sidebar(
          class = "sidebar-custom",
          width = 300,
          h4("Chart Controls", style = "color: #2C5F41; margin-bottom: 20px;"),
          
          selectInput("gm_site", 
                      label = div(icon("map-marker"), "Select a Great Meadow Site:"),
                      choices = sort(unique(all_data$site[grepl("Great Meadow", all_data$site)])),
                      selected = "Great Meadow 1"),
          
          br(),
          pickerInput("year", 
                      label = div(icon("calendar"), "Select Year(s):"),
                      choices = sort(unique(all_data$Year)),
                      selected = 2023,
                      multiple = TRUE,
                      options = list(
                        `actions-box` = TRUE,
                        `deselect-all-text` = "Clear all",
                        `select-all-text` = "Select all",
                        `none-selected-text` = "Choose year(s)",
                        `live-search` = TRUE,
                        style = "btn-outline-primary"
                      )),
          
          br(),
          div(style = "padding: 15px; background-color: #e8f4f8; border-radius: 8px; border-left: 4px solid #3498db;",
              p(icon("info-circle"), " Use the brush tool to select data points on the chart for detailed analysis.", 
                style = "margin: 0; font-size: 0.9rem; color: #2c3e50;"))
        ),
        
        card(
          full_screen = TRUE,
          card_header(
            class = "bg-primary text-white",
            "Hydrographs by Year"
          ),
          plotOutput("hydrograph", height = "600px", 
                     brush = brushOpts(id = "hydro_brush", fill = "#5A9B7C", opacity = 0.3))
        )
      )
  ),
  
  # Selected data points section with improved styling
  div(class = "brush-info-section",
      div(class = "row",
          div(class = "col-12",
              h3("Selected Data Points:", 
                 style = "color: #2C5F41; text-align: center; margin-bottom: 20px;"),
              card(
                card_header(
                  class = "bg-info text-white",
                  "Detailed View of Brushed Data"
                ),
                tableOutput("brush_info")
              )
          )
      )
  ),
  
  # Second section: Water Level Stats with improved styling
  div(class = "content-section stats-main",
      h2("Water Level Statistics", class = "section-title"),
      layout_sidebar(
        sidebar = sidebar(
          class = "sidebar-custom",
          width = 300,
          h4("Statistics Controls", style = "color: #2C5F41; margin-bottom: 20px;"),
          
          pickerInput("stats_site", 
                      label = div(icon("map-marker"), "Select Site(s):"),
                      choices = sort(unique(wl_stats$site)),
                      selected = "Great Meadow 1",
                      multiple = TRUE,
                      options = list(
                        `actions-box` = TRUE,
                        `deselect-all-text` = "Clear all",
                        `select-all-text` = "Select all",
                        `none-selected-text` = "Choose site(s)",
                        `live-search` = TRUE,
                        style = "btn-outline-success"
                      )),
          
          br(),
          
          pickerInput("stats_year", 
                      label = div(icon("calendar"), "Select Years:"),
                      choices = sort(unique(wl_stats$year)),
                      selected = c(2022, 2023),
                      multiple = TRUE,
                      options = list(
                        `actions-box` = TRUE,
                        `deselect-all-text` = "Clear all",
                        `select-all-text` = "Select all",
                        `none-selected-text` = "Choose year(s)",
                        `live-search` = TRUE,
                        style = "btn-outline-success"
                      )),
          
          br(),
          div(style = "padding: 15px; background-color: #f0f8f0; border-radius: 8px; border-left: 4px solid #27ae60;",
              p(icon("chart-bar"), " Growing Season statistics calculated from May through October.", 
                style = "margin: 0; font-size: 0.9rem; color: #2c3e50;"))
        ),
        
        card(
          full_screen = TRUE,
          card_header(
            class = "bg-success text-white",
            "Growing Season Water Level Statistics"
          ),
          div(style = "padding: 10px;",
              dataTableOutput("wl_stats"))
        )
      )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive data for plotting
  plot_data <- reactive({
    req(input$year, input$gm_site)
    
    all_data %>%
      filter(Year %in% input$year, doy > 134, doy < 275) %>%
      filter(site %in% c(input$gm_site, "Gilmore Meadow"))
  })
  
  output$hydrograph <- renderPlot({
    plot_data_filtered <- plot_data()
    req(nrow(plot_data_filtered) > 0)
    
    # Calculate precipitation offset for each year
    precip_data <- plot_data_filtered %>%
      group_by(Year) %>%
      summarise(
        min_water = min(c(
          water.depth[site == input$gm_site & !is.na(water.depth)],
          water.depth[site == "Gilmore Meadow" & !is.na(water.depth)]
        ), na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      left_join(plot_data_filtered, by = "Year") %>%
      mutate(precip_y = lag.precip * 5 + min_water)
    
    global_min_water <- min(plot_data_filtered$water.depth, na.rm = TRUE)
    
    # Create the plot using facet_wrap instead of patchwork
    ggplot() +
      # Water level lines
      geom_line(data = plot_data_filtered %>% filter(site == input$gm_site),
                aes(x = doy_h, y = water.depth, color = input$gm_site), 
                size = 0.7) +
      geom_line(data = plot_data_filtered %>% filter(site == "Gilmore Meadow"),
                aes(x = doy_h, y = water.depth, color = "Gilmore Meadow"), 
                size = 0.7) +
      # Precipitation line
      geom_line(data = precip_data,
                aes(x = doy_h, y = precip_y, color = "Precipitation"), size = 0.7) +
      # Ground level reference
      geom_hline(yintercept = 0, color = "brown") +
      
      # Facet by year
      facet_wrap(~ Year, ncol = 1, scales = "free_y") +
      
      # Styling
      scale_color_manual(values = c(
        setNames("black", input$gm_site),
        "Gilmore Meadow" = "darkgray",
        "Precipitation" = "blue"
      )) +
      labs(title = "Hydrographs by Year",
           x = "Date", y = "Water Level (cm)") +
      scale_x_continuous(
        breaks = c(121, 152, 182, 213, 244, 274),
        labels = c('May-01', 'Jun-01', 'Jul-01', 'Aug-01', 'Sep-01', 'Oct-01')) +
      scale_y_continuous(
        name = "Water Level (cm)",
        sec.axis = sec_axis(~ (. - global_min_water) / 5,
                            name = "Hourly Precip. (cm)",
                            breaks = seq(0, 8, by = 2))) +
      theme_bw() +
      theme(legend.position = "bottom",
            legend.title = element_blank(),
            strip.text = element_text(size = 11),
            plot.title = element_text(hjust = 0.5))
  })
  
  # Selected data output table - Now with separate columns for each site
  output$brush_info <- renderTable({
    req(input$hydro_brush)
    
    plot_data_filtered <- plot_data()
    req(nrow(plot_data_filtered) > 0)
    
    # Use brushedPoints function which works properly with faceted plots
    selected_data <- brushedPoints(
      plot_data_filtered,
      brush = input$hydro_brush,
      xvar = "doy_h",
      yvar = "water.depth"
    ) %>%
      select(site, Year, doy_h, timestamp, water.depth, precip_cm) %>%
      arrange(Year, doy_h) %>%
      # Round for better display
      mutate(
        doy_h = round(doy_h, 2),
        water.depth = round(water.depth, 2),
        precip_cm = round(precip_cm, 3),
        timestamp = format(timestamp, "%Y-%m-%d %H:%M:%S")
      )
    
    if(nrow(selected_data) == 0) {
      data.frame(Message = "No points selected - try brushing over the water level lines")
    } else {
      # Create a proper time grouping to match data points from both sites
      # Group by rounded doy_h to combine data from both sites at similar times
      pivoted_data <- selected_data %>%
        # Round doy_h slightly to group nearby times together
        mutate(doy_h_rounded = round(doy_h, 1)) %>%
        # Group by the rounded time
        group_by(Year, doy_h_rounded) %>%
        summarise(
          # Use the first timestamp and precipitation value in each group
          timestamp = first(timestamp),
          doy_h = first(doy_h),
          precip_cm = first(precip_cm),
          # Create separate columns for each site's water depth
          great_meadow_depth = ifelse(any(site == input$gm_site), 
                                      water.depth[site == input$gm_site][1], NA),
          gilmore_depth = ifelse(any(site == "Gilmore Meadow"), 
                                 water.depth[site == "Gilmore Meadow"][1], NA),
          .groups = 'drop'
        ) %>%
        # Remove the grouping column and rename for display
        select(-doy_h_rounded) %>%
        rename(
          `Timestamp` = timestamp,
          `Day of Year` = doy_h,
          `Precipitation (cm)` = precip_cm
        ) %>%
        # Create dynamic column names based on selected site
        rename_with(~ paste(input$gm_site, "Water Depth (cm)"), .cols = great_meadow_depth) %>%
        rename_with(~ "Gilmore Meadow Water Depth (cm)", .cols = gilmore_depth) %>%
        # Arrange by year and day
        arrange(Year, `Day of Year`)
      
      pivoted_data
    }
  })
  
  # WL stats output table
  output$wl_stats <- DT::renderDataTable({
    req(input$stats_site, input$stats_year)
    
    wl_stats %>%
      filter(site %in% input$stats_site, year %in% input$stats_year) %>% 
      mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
      select(
        Year = year,
        Site = site,
        `Mean Water Level` = WL_mean,
        `SD Water Level` = WL_sd,
        `Minimum Water Level` = WL_min,
        `Maximum Water Level` = WL_max,
        `Maximum Hourly Increase` = max_inc,
        `Maximum Hourly Decrease` = max_dec,
        `Growing Season Change` = GS_change,
        `GS % Surface Water` = prop_over_0cm,
        `GS % Within 30cm` = prop_bet_0_neg30cm,
        `GS % Over 30cm Deep` = prop_under_neg30cm,
        `GS % Complete Data` = prop_GS_comp
      )
  }, options = list(pageLength = 10, scrollX = TRUE))
}

# Run app
shinyApp(ui, server)




