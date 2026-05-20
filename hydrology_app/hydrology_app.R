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
library(ggtext)

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

# Prepare aggregated data for time series plots
wl_test <- wl_stats %>%
  group_by(wetland, year) %>%
  summarise(
    across(
      c(WL_mean, WL_sd, WL_min, WL_max, max_inc, max_dec, GS_change,
        prop_over_0cm, prop_bet_0_neg30cm, prop_under_neg30cm),
      list(avg = ~mean(.x, na.rm = TRUE),
           se  = ~sd(.x, na.rm = TRUE) / sqrt(n())),
      .names = "{.col}_{.fn}"
    ),
    
    n_sites = n_distinct(site),
    .groups = "drop"
  )

# Compute grand means
grand_means <- wl_test %>%
  group_by(wetland) %>%
  summarise(
    across(
      ends_with("_avg"),
      ~ mean(.x, na.rm = TRUE),
      .names = "{.col}_grand"
    ),
    .groups = "drop"
  )


#-----------------------#
####    Constants    ####
#-----------------------#

# Site color palette (defined once)
SITE_COLORS <- c(
  "Great Meadow 1" = "#000066", "Great Meadow 2" = "darkgreen", "Great Meadow 3" = "green",
  "Great Meadow 4" = "darkorange", "Great Meadow 5" = "deeppink2", "Great Meadow 6" = "purple",
  "Gilmore Meadow 1" = "chocolate4",   "Great Meadow (Average)" = "black",
  "Gilmore Meadow (Average)" = "darkgray", "Precipitation" = "blue"
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
    summarise(across(all_of(stat_cols), \(x) mean(x, na.rm = TRUE)), .groups = "drop")
  
  map_dfr(stat_cols, function(var) {
    tryCatch({
      # reshape to wide format (one row per year)
      wide_data <- yearly_means %>%
        select(year, site_group, all_of(var)) %>%
        tidyr::pivot_wider(names_from = site_group, values_from = all_of(var)) %>%
        drop_na()  # ensure complete pairs
      # paired t-test
      test <- t.test(wide_data[["Great Meadow"]], wide_data[["Gilmore Meadow"]], paired = TRUE)
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

# Time series plotting function for statistics
plot_wl_metric <- function(data, grand_data, metric, y_label, title, sig_results = NULL) {
  
  # Filter grand_data to only include wetlands present in data
  wetlands_in_data <- unique(as.character(data$wetland))
  grand_data <- grand_data %>%
    filter(wetland %in% wetlands_in_data)
  
  # Enforce consistent ordering - Great Meadow first
  data$wetland <- factor(data$wetland, 
                         levels = c("Great Meadow", "Gilmore Meadow"))
  grand_data$wetland <- factor(grand_data$wetland, 
                               levels = c("Great Meadow", "Gilmore Meadow"))
  
  # Sort wetlands_in_data to ensure Great Meadow is first
  wetlands_in_data <- sort(wetlands_in_data, decreasing = TRUE)  # This will put Great before Gilmore
  
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
          ymax = .data[[avg_col]] + .data[[se_col]]),
      width = 0, alpha = 0.85
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
          color = grand_colors,
          linewidth = 1
        )
      )
    )
  
  return(p)
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
      color: white; 
      padding: 30px; 
      margin: -15px -15px 0 -15px;  
      text-align: center;
      box-shadow: 0 2px 8px rgba(0,0,0,0.1);
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
      color: #1B365D;
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
      background-color: #1B365D;  /* Navy background on hover */
      color: white;  /* White text on hover */
      border: 1px solid #1B365D;
      transform: translateY(-1px);  /* Slight lift effect */
      box-shadow: 0 2px 4px rgba(27, 54, 93, 0.2);
    }
    .nav-bar-container {
      display: flex;
      justify-content: center;
      align-items: center;
      flex-wrap: wrap;
    }
  "))
  ),
  
  # Main title
  div(class = "main-title",
      h1("Wetland Hydrology Dashboard", 
         style = "margin: 0; font-size: 2rem; text-shadow: 2px 2px 4px rgba(0,0,0,0.3)")
  ),
  
  # Navigation bar - separate from header, spans full width
  div(class = "nav-bar",
      div(class = "nav-bar-container",
          tags$a(href = "#hydrographs", "Hydrographs"),
          tags$a(href = "#stats-plots", "Statistics Plots"),
          tags$a(href = "#stats-table", "Statistics Table"),
          tags$a(href = "#about", "About")
      )
  ),
  
  #--------------------------------#
  ####   Hydrographs Section    ####
  #--------------------------------#
  
  div(id = "hydrographs", class = "content-section",
      layout_sidebar(
        sidebar = sidebar(
          class = "sidebar-custom", width = 300,
          h4("Hydrograph Controls", style = "color: #1B365D; margin-bottom: 20px;"),
          
          create_picker_input(
            "selected_sites", "Select Site(s):", 
            list(
              "Individual Sites" = sort(unique(all_data$site)),
              "Wetland Averages" = c(
                "Gilmore Meadow (Average)",
                "Great Meadow (Average)"
              )
            ),
            "Great Meadow 1",
            none_text = "Choose site(s)"
          ),
          
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
  
  #------------------------------------------#
  ####  Water Level Stats Plots Section   ####
  #------------------------------------------#
  
  div(id = "stats-plots", class = "content-section",
      layout_sidebar(
        sidebar = sidebar(
          class = "sidebar-custom", width = 300,
          h4("Plot Controls", style = "color: #1B365D; margin-bottom: 20px;"),
          
          pickerInput("selected_metric", 
                      label = div(icon("chart-line"), "Select Statistic(s):"),
                      choices = c(
                        "Mean Water Level (cm)" = "WL_mean",
                        "SD Water Level (cm)" = "WL_sd",
                        "Minimum Water Level (cm)" = "WL_min",
                        "Maximum Water Level (cm)" = "WL_max",
                        "Maximum Hourly Increase (cm)" = "max_inc",
                        "Maximum Hourly Decrease (cm)" = "max_dec",
                        "Growing Season Change (cm)" = "GS_change",
                        "GS % Surface Water" = "prop_over_0cm",
                        "GS % Within 30cm" = "prop_bet_0_neg30cm",
                        "GS % Over 30cm Deep" = "prop_under_neg30cm"
                      ),
                      selected = "WL_mean",
                      multiple = TRUE,
                      options = c(PICKER_OPTIONS, list(`none-selected-text` = "Choose metric(s)"))),
          
          checkboxGroupInput(
            "ts_wetland",
            div("Select Wetland(s):"),
            choices = c("Great Meadow", "Gilmore Meadow"),
            selected = c("Great Meadow", "Gilmore Meadow")
          ),
          
          conditionalPanel(
            condition = "input.ts_wetland.length == 2",
            div(style = "padding: 10px; background-color: #fff3cd; border-radius: 8px; border-left: 4px solid #856404; margin-top: 10px;",
                p(HTML(paste0(as.character(icon("asterisk")), " <strong>Significance Testing:</strong><br>",
                              "Compares grand means between Great Meadow and Gilmore Meadow wetlands. Significant differences (p < 0.05) are highlighted.")), 
                  style = "margin: 0; font-size: 0.9rem; color: #856404;"))
          ),
          
          div(style = "padding: 10px; background-color: #e8f4f8; border-radius: 8px; border-left: 4px solid #3498db;",
              p(icon("info-circle"), "Points show annual means ± SE across sites within each wetland (Gilmore Meadow has one site and therefore no SE). Dashed lines represent grand means across all years.", 
                style = "margin: 0; font-size: 0.9rem; color: #2c3e50;")),
          
          div(style = "margin-top: 15px; text-align: center;",
              downloadButton("download_timeseries", "Download Plot", 
                             class = "btn-primary btn-sm", icon = icon("download"))),
          
          div(style = "margin-top: 15px; text-align: center;",
              tags$a(href = "#about",
                     class = "btn btn-primary btn-sm", icon("info-circle"),
                     "About"))
        ),
        
        card(
          full_screen = TRUE,
          card_header(class = "bg-primary text-white", "Water Level Statistics Over Time"),
          uiOutput("timeseries_plots")
        )
      )
  ),
  
  #--------------------------------------#
  ####   Water Level Stats Section    ####
  #--------------------------------------#
  
  div(id = "stats-table", class = "content-section",
      layout_sidebar(
        sidebar = sidebar(
          class = "sidebar-custom", width = 300,
          h4("Table Controls", style = "color: #1B365D; margin-bottom: 20px;"),
          
          create_picker_input("stats_site", "Select Site(s):", 
                              unique(wl_stats$site), 
                              c("Great Meadow 1", "Gilmore Meadow 1"),
                              none_text = "Choose site(s)"),
          
          create_picker_input("stats_year", "Select Years:", 
                              unique(wl_stats$year), 
                              selected = tail(sort(unique(wl_stats$year)), 4),
                              none_text = "Choose year(s)"),
          
          radioButtons("time_summary", "Summarize Water Level Statistics By:",
                       choices = c("Site per Year" = "year",
                                   "Site Averaged Across Years" = "multi",
                                   "Wetland per Year" = "wetland_year",
                                   "Wetland Averaged Across Years" = "all_sites"),
                       selected = "year"),
          
          br(),
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
          card_header(class = "bg-primary text-white", "Water Level Statistics"),
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
  
  # Significance results for time series plot - only when both wetlands selected
  timeseries_significance <- reactive({
    req(input$ts_wetland)
    
    # Only run significance test if both wetlands are selected
    if (length(input$ts_wetland) == 2 && 
        all(c("Great Meadow", "Gilmore Meadow") %in% input$ts_wetland)) {
      
      all_sites <- unique(wl_stats$site)
      all_years <- unique(wl_stats$year)
      
      calculate_wetland_significance(wl_stats, all_years, all_sites, alpha = 0.05)
    } else {
      NULL  # Return NULL if both wetlands aren't selected
    }
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
           "wetland_year" = {
             # Wetland per Year (NEW)
             data %>%
               group_by(wetland, year) %>%
               summarise(
                 across(c(WL_mean, WL_sd, WL_min, WL_max, max_inc, max_dec, GS_change,
                          prop_over_0cm, prop_bet_0_neg30cm, prop_under_neg30cm), 
                        ~ round(mean(.x, na.rm = TRUE), 2)),
                 .groups = "drop"
               ) %>%
               rename(Year = year, Wetland = wetland, `Mean Water Level (cm)` = WL_mean,
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
    • For comparison with significance testing, both Great Meadow and Gilmore Meadow and at least four years of data must be selected. Otherwise, results display without significance testing.<br>
    • Significance testing compares the grand means of each statistic between wetlands for all selected years and sites.
  </div>")
    
    if (show_significance_info()) {
      sig_results <- significance_results()
      
      if (!is.null(sig_results)) {
        sig_vars <- sig_results %>% filter(significant) %>% pull(variable)
        
        if (length(sig_vars) > 0) {
          # At least one significant result
          sig_display_names <- VAR_MAPPING[sig_vars]
          sig_display_names <- sig_display_names[!is.na(sig_display_names)]
          
          # Get p-values for significant variables
          sig_pvals <- sig_results %>% 
            filter(significant) %>% 
            select(variable, p_value)
          
          sig_list <- sapply(sig_display_names, function(name) {
            var_code <- names(VAR_MAPPING)[VAR_MAPPING == name]
            pval <- sig_pvals %>% filter(variable == var_code) %>% pull(p_value)
            paste0("<li>", name, " (p = ", sprintf("%.3f", pval), ")</li>")
          })
          
          return(tagList(
            base_note,
            div(
              style = "margin-bottom: 15px;",
              div(
                style = 'background-color:#fff3cd; padding:8px 12px; border-left:4px solid #856404; border-radius:4px;',
                HTML(sprintf(
                  "<strong style='color:#856404; font-size:13px;'>%s Significance Testing</strong><br>
              <span style='color:#333333; font-size:12px;'>Highlighted variables show significant differences (p < 0.05) between Great Meadow and Gilmore Meadow wetlands:</span><br>
              <ul style='margin-top:8px; margin-bottom:5px; color:#333333; font-size:12px;'>%s</ul>",
                  as.character(icon("asterisk")),
                  paste(sig_list, collapse = "")
                ))
              )
            )
          ))
        } else {
          # No significant results found
          return(tagList(
            base_note,
            div(
              style = "margin-bottom: 15px;",
              div(
                style = 'background-color:#f5f5f5; padding:8px 12px; border-left:4px solid #6c757d; border-radius:4px;',
                HTML(sprintf(
                  "<strong style='color:#495057; font-size:13px;'>No Statistically Significant Differences</strong><br>
              <span style='color:#666666; font-size:12px;'>No variables show significant differences (p < 0.05) between Great Meadow and Gilmore Meadow wetlands for the selected sites and years.</span>"
                ))
              )
            )
          ))
        }
      }
    }
    
    return(base_note)
  })
  
  # Render water level stats table with significance highlighting
  output$wl_stats <- DT::renderDataTable({
    data <- filtered_stats()
    
    # Conditional sorting based on what columns exist
    if ("Site" %in% names(data)) {
      data <- data %>% arrange(desc(Site == "All Sites"), Site)
    } else if ("Wetland" %in% names(data) && "Year" %in% names(data)) {
      data <- data %>% arrange(Wetland, Year)
    } else {
      data <- data %>% arrange(Wetland)
    }
    
    dt <- datatable(data, rownames = FALSE,
                    options = list(pageLength = 10, scrollX = TRUE)) %>%
      formatStyle(columns = names(data), valueColumns = if ("Site" %in% names(data)) "Site" else "Wetland")
    
    if (show_significance_info()) {
      sig_results <- significance_results()
      
      if (!is.null(sig_results)) {
        for (var_name in names(VAR_MAPPING)) {
          col_name <- VAR_MAPPING[var_name]
          sig_row <- sig_results[sig_results$variable == var_name, ]
          
          if (nrow(sig_row) > 0 && sig_row$significant) {
            dt <- dt %>%
              formatStyle(col_name, 
                          valueColumns = if ("Site" %in% names(data)) "Site" else "Wetland",
                          backgroundColor = styleEqual("All Sites", "#fff3cd"))
          }
        }
      }
    }
    
    dt
  })
  
  ## NEW SECTION ---------------------------------------------------------------
  
  # Render time series plot based on selected metric
  # Render stacked time series plots
  output$timeseries_plots <- renderUI({
    req(input$selected_metric, input$ts_wetland)
    
    metric_labels <- c(
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
    
    # Get significance results only if both wetlands selected
    sig_results <- if (length(input$ts_wetland) == 2 && 
                       all(c("Great Meadow", "Gilmore Meadow") %in% input$ts_wetland)) {
      timeseries_significance()
    } else {
      NULL
    }
    
    # Create a plot for each selected metric
    plot_outputs <- lapply(seq_along(input$selected_metric), function(i) {
      metric <- input$selected_metric[i]
      output_id <- paste0("plot_", metric)
      
      # Generate the plot
      output[[output_id]] <- renderPlot({
        # Filter data by selected wetlands
        filtered_wl_test <- wl_test %>%
          filter(wetland %in% input$ts_wetland)
        
        filtered_grand_means <- grand_means %>%
          filter(wetland %in% input$ts_wetland)
        
        metric_label <- metric_labels[metric]
        
        plot_wl_metric(
          filtered_wl_test,
          filtered_grand_means,
          metric = metric,
          y_label = metric_label,
          title = paste(metric_label, "Over Time"),
          sig_results = sig_results
        )
      })
      
      # Create significance info for this metric
      sig_info_output <- if (!is.null(sig_results)) {
        sig_row <- sig_results %>% filter(variable == metric)
        
        if (nrow(sig_row) > 0) {
          if (sig_row$significant) {
            div(
              style = "margin-top: 0px; margin-bottom: 5px;",
              div(
                style = 'background-color:#fff3cd; padding:5px; border-left:4px solid #856404; border-radius:4px;',
                HTML(sprintf(
                  "<strong style='color:#856404; font-size:13px;'>%s Statistically Significant (p = %.3f)</strong>",
                  as.character(icon("asterisk")),
                  sig_row$p_value
                ))
              )
            )
          } else {
            div(
              style = "margin-top: 0px; margin-bottom: 5px;",
              div(
                style = 'background-color:#f5f5f5; padding:5px; border-left:4px solid #6c757d; border-radius:4px;',
                HTML(sprintf(
                  "<strong style='color:#495057; font-size:13px;'>Not Statistically Significant (p = %.3f)</strong>",
                  sig_row$p_value
                ))
              )
            )
          }
        }
      } else {
        NULL
      }
      
      # Return the plot with significance info
      tagList(
        sig_info_output,
        plotOutput(output_id, height = "600px")
      )
    })
    
    tagList(plot_outputs)
  })
  
  
  ## ---------------------------------------------------------------------------
  
  
  # Download handlers
  output$download_brush <- downloadHandler(
    filename = function() paste("hydrograph_selected_data_", Sys.Date(), ".csv", sep = ""),
    content = function(file) write.csv(processed_brush_data(), file, row.names = FALSE)
  )
  
  output$download_stats <- downloadHandler(
    filename = function() {
      suffix <- switch(input$time_summary,
                       "year" = "site_per_year_stats",
                       "multi" = "site_averaged_stats",
                       "wetland_year" = "wetland_per_year_stats",
                       "all_sites" = "wetland_averaged_significance_stats"
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
  
  # Download handler for time series plot
  output$download_timeseries <- downloadHandler(
    filename = function() {
      metrics <- paste(input$selected_metric, collapse = "_")
      wetlands <- paste(input$ts_wetland, collapse = "_")
      paste0("timeseries_", metrics, "_", wetlands, "_", Sys.Date(), ".png")
    },
    content = function(file) {
      # Filter data by selected wetlands
      filtered_wl_test <- wl_test %>%
        filter(wetland %in% input$ts_wetland)
      
      filtered_grand_means <- grand_means %>%
        filter(wetland %in% input$ts_wetland)
      
      metric_labels <- c(
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
      
      # Get significance results
      sig_results <- if (length(input$ts_wetland) == 2 && 
                         all(c("Great Meadow", "Gilmore Meadow") %in% input$ts_wetland)) {
        isolate(timeseries_significance())
      } else {
        NULL
      }
      
      # Create all selected plots
      plots <- lapply(input$selected_metric, function(metric) {
        metric_label <- metric_labels[metric]
        
        p <- plot_wl_metric(filtered_wl_test, filtered_grand_means, metric, 
                            metric_label, paste(metric_label, "Over Time"),
                            sig_results = sig_results)
        
        # Add p-value annotation if available
        if (!is.null(sig_results)) {
          sig_row <- sig_results %>% filter(variable == metric)
          
          if (nrow(sig_row) > 0 && !is.na(sig_row$p_value)) {
            label_text <- if (sig_row$significant) {
              sprintf("p = %.3f *", sig_row$p_value)
            } else {
              sprintf("p = %.3f (ns)", sig_row$p_value)
            }
            
            p <- p + 
              annotate("text", 
                       x = Inf, y = Inf, 
                       label = label_text,
                       hjust = 1.05, vjust = 1.5,
                       size = 4.5,
                       fontface = "bold",
                       color = "black")
          }
        }
        
        p
      })
      
      # Stack plots vertically using patchwork
      combined_plot <- wrap_plots(plots, ncol = 1)
      
      # Save with height adjusted for number of plots
      ggsave(file, plot = combined_plot, device = "png", 
             width = 12, height = 8 * length(input$selected_metric), dpi = 300, bg = "white")
    }
  )
  
}

# Run app
shinyApp(ui, server)
