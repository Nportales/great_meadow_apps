#### Biodiversity Data R Shiny app - iNat and ebird data ####

#---------------------------------------------#
####        Load Required Packages         ####
#---------------------------------------------#

library(shiny)
library(bslib)
library(dplyr)
library(plotly)
library(purrr)
library(forcats)
library(RColorBrewer)
library(viridisLite)

#-------------------------------------------#
####    Read & Prepare Processed Data    ####
#-------------------------------------------#

# For data exploration, app building, or debugging, temporarily set working directory to app folder
# setwd("biodiversity_app")

# iNat data
inat_data <- read.csv("data/inat_greatmeadow_20260326.csv")

# eBird data
eBird_data <- read.csv("data/ebird_greatmeadow_20260324.csv")

eBird_tax <- read.csv("data/eBird_taxonomy_v2025.csv")

# merge eBird data
merged_data <- full_join(
  eBird_data, 
  eBird_tax, 
  by = c("TAXONOMIC.ORDER" = "TAXON_ORDER")
) %>% 
  filter(!is.na(SCIENTIFIC.NAME))

#----------------#
####    UI    ####
#----------------#

ui <- page_fluid(
  theme = bs_theme(
    bootswatch = "flatly",
    primary = "#2E8B57", 
    base_font = font_google("Open Sans")
  ),
  
  # Custom CSS for styled header
  tags$head(
    tags$style(HTML("
      .main-title {
        background: linear-gradient(135deg, #2E8B57 0%, #3CB371 100%);
        color: white;
        padding: 30px 30px 10px 30px;;
        margin: -15px -15px 20px -15px;
        text-align: center;
        border-radius: 0 0 20px 20px;
        box-shadow: 0 4px 12px rgba(0,0,0,0.15);
      }
      .main-title h1 {
        margin: 0;
        font-size: 2rem;
        text-shadow: 2px 2px 4px rgba(0,0,0,0.3);
      }
      .main-title p {
        margin-top: 10px;
        font-size: 1.1rem;
        color: #e0f2e9;
      }
      
      .card {
      margin: 2px !important;   /* tighter margins */
      }
      
      .card-body {
      padding: 6px !important;  /* tighter padding inside */
      }
      
      .card.dark-border {
      border: 1px solid #bbb;   /* darker */
      }
      
      "))
  ),
  
  # Main title styled like hydrology app
  div(class = "main-title",
      h1("Biodiversity of Great Meadow"),
      p("Summary of Participatory Science Observations")
  ),
  
  #### Side-by-side sections ####
  layout_columns(
    col_widths = c(6, 6),  # equal width columns
    
    #### iNaturalist Section ####
    div(class = "p-3 border border-medium rounded",
        h3("iNaturalist", class = "text-center mb-3", style = "color: #4a4a4a;"),
        
        # Stats with icons ABOVE the pie chart
        div(
          style = "display: flex; gap: 10px; justify-content: center; margin-bottom: 10px;",
          
          div(style = "flex: 1;",
              card(
                class = "text-center mb-0 dark-border",
                card_body(
                  div(class = "d-flex align-items-center justify-content-center",
                      icon("binoculars", style = "font-size: 1.2rem; color: #28a745; margin-right: 6px;"),
                      div(
                        h4(textOutput("inat_observations"), class = "mb-0 text-primary", style = "font-size: 1.1rem; font-weight: bold;"),
                        div("Observations", class = "text-muted small")
                      )
                  )
                )
              )
          ),
          
          div(style = "flex: 1;",
              card(
                class = "text-center mb-0 dark-border",
                card_body(
                  div(class = "d-flex align-items-center justify-content-center",
                      icon("users", style = "font-size: 1.2rem; color: #17a2b8; margin-right: 6px;"),
                      div(
                        h4(textOutput("inat_people"), class = "mb-0 text-info", style = "font-size: 1.1rem; font-weight: bold;"),
                        div("Observers", class = "text-muted small")
                      )
                  )
                )
              )
          )
        ),
        
        # Pie chart for species by taxa BELOW stats
        div(class = "text-center",
            plotlyOutput("spp_plot", height = "250px"),
            div(id = "spp_legend", class = "mt-2")
        )
    ),
    
    #### eBird Section ####
    div(class = "p-3 border border-medium rounded",
        h3("eBird", class = "text-center mb-3", style = "color: #4a4a4a;"),
        
        # Stats with icons ABOVE the pie chart
        div(
          style = "display: flex; gap: 10px; justify-content: center; margin-bottom: 10px;",
          
          div(style = "flex: 1;",
              card(
                class = "text-center mb-0 dark-border",
                card_body(
                  div(class = "d-flex align-items-center justify-content-center",
                      icon("clipboard-list", style = "font-size: 1.2rem; color: #28a745; margin-right: 6px;"),
                      div(
                        h4(textOutput("ebird_checklists"), class = "mb-0 text-primary", style = "font-size: 1.1rem; font-weight: bold;"),
                        div("Checklists", class = "text-muted small")
                      )
                  )
                )
              )
          ),
          
          div(style = "flex: 1;",
              card(
                class = "text-center mb-0 dark-border",
                card_body(
                  div(class = "d-flex align-items-center justify-content-center",
                      icon("users", style = "font-size: 1.2rem; color: #17a2b8; margin-right: 6px;"),
                      div(
                        h4(textOutput("ebird_observers"), class = "mb-0 text-info", style = "font-size: 1.1rem; font-weight: bold;"),
                        div("Observers", class = "text-muted small")
                      )
                  )
                )
              )
          )
        ),
        
        # Pie chart for species by taxonomic group BELOW stats
        div(class = "text-center",
            plotlyOutput("ebird_spp_plot", height = "250px"),
            div(id = "ebird_spp_legend", class = "mt-2")
        )
    )
  ),
  
  # Footer
  hr(),
  div(
    class = "text-center text-muted small mt-4 mb-3",  
    p("Data sourced from iNaturalist and eBird • Last updated: ", Sys.Date())
  )
)

#--------------------#
####    SERVER    ####
#--------------------#

server <- function(input, output, session) {
  
  # Function to generate a palette for any number of categories
  generate_palette <- function(n_colors, palette_type = "qualitative") {
    if(n_colors <= 8) {
      rev(viridis(n_colors, option = "D", end = 0.85))
    } else {
      # Use multiple viridis family palettes - colorblind friendly
      base_colors <- c(
        rev(viridis(8, option = "D", end = 0.85)),  # Classic viridis (purple-teal)
        rocket(6, begin = 0.4),
        rev(cividis(6))
      )
      base_colors[1:min(n_colors, length(base_colors))]
    }
  }
  
  # Function to assign colors ensuring "Other" gets a neutral color
  get_chart_colors <- function(categories, palette_type = "qualitative") {
    n_cats <- length(categories)
    colors <- generate_palette(n_cats, palette_type)
    
    # If there's an "Other" category, assign it a neutral gray
    other_idx <- grep("Other", categories, ignore.case = TRUE)
    if(length(other_idx) > 0) {
      colors[other_idx] <- "#CCCCCC"  # Light gray for "Other"
    }
    
    return(colors)
  }
  
  # Helper function to create legend
  create_legend <- function(categories, colors) {
    legend_items <- map2_chr(categories, colors, function(cat, col) {
      paste0('<span style="display: inline-block; margin-right: 10px; margin-bottom: 3px;">',
             '<span style="display: inline-block; width: 10px; height: 10px; ',
             'background-color: ', col, '; margin-right: 4px; border-radius: 2px;"></span>',
             '<span style="font-size: 11px;">', cat, '</span></span>')
    })
    paste(legend_items, collapse = "")
  }
  
  # ---- Species pie chart for iNaturalist ----
  spp_summary <- reactive({
    inat_data %>%
      filter(
        !is.na(taxon_species_name), taxon_species_name != "",
        !is.na(iconic_taxon_name), iconic_taxon_name != "") %>%
      distinct(taxon_species_name, iconic_taxon_name) %>%
      count(iconic_taxon_name) %>%
      mutate(percentage = round(n / sum(n) * 100, 1)) %>%
      arrange(desc(n))
  })
  
  output$spp_plot <- renderPlotly({
    data <- spp_summary()
    total_spp <- sum(data$n)
    chart_colors <- get_chart_colors(data$iconic_taxon_name)
    
    plot_ly(data, 
            labels = ~iconic_taxon_name, 
            values = ~n,
            type = "pie", 
            hole = 0.6,
            marker = list(colors = chart_colors,
                          line = list(color = "white", width = 2)),
            textinfo = "none",
            hovertemplate = "<b>%{label}</b><br>Species: %{value} (%{percent})<extra></extra>") %>%
      layout(
        showlegend = FALSE,
        margin = list(t = 10, b = 10, l = 10, r = 10),
        paper_bgcolor = "transparent",  # Remove white background
        plot_bgcolor = "transparent", 
        annotations = list(
          list(x = 0.5, y = 0.5, 
               text = paste0("<b>", formatC(total_spp, big.mark = ","), "</b><br>",
                             "<span style='font-size:14px'>Species</span>"),
               showarrow = FALSE,
               font = list(size = 20))
        )
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # Create legend for iNat species
  observe({
    data <- spp_summary()
    chart_colors <- get_chart_colors(data$iconic_taxon_name)
    legend_html <- create_legend(data$iconic_taxon_name, chart_colors)
    insertUI(selector = "#spp_legend", 
             ui = div(HTML(legend_html), class = "text-center"),
             immediate = TRUE)
  })
  
  # ---- eBird species pie chart ----
  ebird_spp_summary <- reactive({
    df <- merged_data %>%
      filter(CATEGORY.x == "species",
             !is.na(SCIENTIFIC.NAME), SCIENTIFIC.NAME != "", 
             !is.na(SPECIES_GROUP), SPECIES_GROUP != "") %>%
      distinct(SCIENTIFIC.NAME, SPECIES_GROUP) %>%
      count(SPECIES_GROUP) %>%
      mutate(SPECIES_GROUP = ifelse(n <= 2, "Other (1–2 species groups)", SPECIES_GROUP)) %>%
      group_by(SPECIES_GROUP) %>%
      summarise(n = sum(n), .groups = "drop") %>%
      mutate(percentage = round(n / sum(n) * 100, 1)) %>%
      arrange(desc(n))
    
    # Separate "Other" from the rest and reorder
    other_row <- df[df$SPECIES_GROUP == "Other (1–2 species groups)", ]
    main_rows <- df[df$SPECIES_GROUP != "Other (1–2 species groups)", ]
    
    # Combine with "Other" at the end
    final_df <- bind_rows(main_rows, other_row) %>%
      mutate(SPECIES_GROUP = factor(SPECIES_GROUP, levels = SPECIES_GROUP))
    
    return(final_df)
  })
  
  output$ebird_spp_plot <- renderPlotly({
    data <- ebird_spp_summary()
    total_spp <- sum(data$n)
    chart_colors <- get_chart_colors(data$SPECIES_GROUP)
    
    plot_ly(data, 
            labels = ~SPECIES_GROUP, 
            values = ~n,
            type = "pie", 
            hole = 0.6,
            marker = list(colors = chart_colors,
                          line = list(color = "white", width = 2)),
            textinfo = "none",
            hovertemplate = "<b>%{label}</b><br>Species: %{value} (%{percent})<extra></extra>") %>%
      layout(
        showlegend = FALSE,
        margin = list(t = 10, b = 10, l = 10, r = 10),
        paper_bgcolor = "transparent",  # Remove white background
        plot_bgcolor = "transparent", 
        annotations = list(
          list(x = 0.5, y = 0.5, 
               text = paste0("<b>", formatC(total_spp, big.mark = ","), "</b><br>",
                             "<span style='font-size:14px'>Species</span>"),
               showarrow = FALSE,
               font = list(size = 20))
        )
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # Create legend for eBird species
  observe({
    data <- ebird_spp_summary()
    chart_colors <- get_chart_colors(data$SPECIES_GROUP)
    legend_html <- create_legend(data$SPECIES_GROUP, chart_colors)
    insertUI(selector = "#ebird_spp_legend", 
             ui = div(HTML(legend_html), class = "text-center"),
             immediate = TRUE)
  })
  
  # ---- iNaturalist stats ----
  inat_summary <- reactive({
    list(
      n_observations = n_distinct(inat_data$uuid),
      n_people = n_distinct(inat_data$user_id)
    )
  })
  
  output$inat_observations <- renderText({
    formatC(inat_summary()$n_observations, big.mark = ",")
  })
  
  output$inat_people <- renderText({
    formatC(inat_summary()$n_people, big.mark = ",")
  })
  
  ## ---- eBird summaries ----
  ebird_summary <- reactive({
    list(
      n_checklists = n_distinct(eBird_data$SAMPLING.EVENT.IDENTIFIER),
      n_observers = n_distinct(eBird_data$OBSERVER.ID)
    )
  })
  
  output$ebird_checklists <- renderText({
    formatC(ebird_summary()$n_checklists, big.mark = ",")
  })
  
  output$ebird_observers <- renderText({
    formatC(ebird_summary()$n_observers, big.mark = ",")
  })
}

# Run app
shinyApp(ui, server)


