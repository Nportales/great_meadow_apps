
#9/4/25-------------------------------------------------------------------------


## tried a T border and not good - not flexible when changing screen sizes

#### Biodiversity Data ####

#### iNat and ebird data -------------------------------------------------------

library(shiny)
library(bslib)
library(dplyr)
library(plotly)
library(purrr)
library(forcats)
library(RColorBrewer)
library(viridisLite)

# Read & prepare processed data

# iNat data
inat_data <- read.csv("data/raw_data/biodiversity_data/inat_greatmeadow_20250825.csv")

# eBird data
eBird_data <- read.csv("data/raw_data/biodiversity_data/ebird_greatmeadow_20250825.csv")

eBird_tax <- read.csv("data/raw_data/biodiversity_data/eBird_taxonomy_v2024.csv")

# merge eBird data
merged_data <- full_join(
  eBird_data, 
  eBird_tax, 
  by = c("TAXONOMIC.ORDER" = "TAXON_ORDER")
) %>% 
  filter(!is.na(SCIENTIFIC.NAME))


#UI
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
      
      .t-row {
     display: flex;
     border: 1px solid #bbb;      /* full border */
     border-bottom: none;         /* remove bottom border */
     border-top: 2px solid #bbb;  /* stronger top line */
     margin-bottom: 20px;
     border-radius: 6px 6px 0 0;
      }

      .t-section {
      flex: 1;
      padding: 12px;
      }

      .t-section:not(:last-child) {
      border-right: 1px solid #bbb; /* vertical divider */
      }
      
      "))
  ),
  
  # Main title styled like hydrology app
  div(class = "main-title",
      h1("Biodiversity of Great Meadow"),
      p("Summary of Participatory Science Observations")
  ),
  
  #### Side-by-side sections ####
  div(class = "t-row",
      div(class = "t-section",
          h3("iNaturalist", class = "text-center mb-3", style = "color: #4a4a4a;"),
          
          # Stats with icons ABOVE the pie chart
          div(
            style = "display: flex; gap: 10px; justify-content: center; margin-bottom: 10px;",
            
            div(style = "flex: 1;",
                div(class = "card dark-border text-center mb-0", 
                    div(class = "card-body d-flex align-items-center justify-content-center",
                        icon("binoculars", style = "font-size: 1.2rem; color: #28a745; margin-right: 6px;"),
                        div(
                          h4(textOutput("inat_observations"), class = "mb-0 text-primary", style = "font-size: 1.1rem; font-weight: bold;"),
                          div("Observations", class = "text-muted small")
                        )
                    )
                )
            ),
            
            div(style = "flex: 1;",
                div(class = "card dark-border text-center mb-0", 
                    div(class = " card-body d-flex align-items-center justify-content-center",
                        icon("users", style = "font-size: 1.2rem; color: #17a2b8; margin-right: 6px;"),
                        div(
                          h4(textOutput("inat_people"), class = "mb-0 text-info", style = "font-size: 1.1rem; font-weight: bold;"),
                          div("Observers", class = "text-muted small")
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
      div(class = "t-section",
          h3("eBird", class = "text-center mb-3", style = "color: #4a4a4a;"),
          
          # Stats with icons ABOVE the pie chart
          div(
            style = "display: flex; gap: 10px; justify-content: center; margin-bottom: 10px;",
            
            div(style = "flex: 1;",
                div(class = "card dark-border text-center mb-0", 
                    div(class = "card-body d-flex align-items-center justify-content-center",
                        icon("clipboard-list", style = "font-size: 1.2rem; color: #28a745; margin-right: 6px;"),
                        div(
                          h4(textOutput("ebird_checklists"), class = "mb-0 text-primary", style = "font-size: 1.1rem; font-weight: bold;"),
                          div("Checklists", class = "text-muted small")
                        )
                    )
                )
            ),
            
            div(style = "flex: 1;",
                div(class = "card dark-border text-center mb-0", 
                    div(class = "card-body d-flex align-items-center justify-content-center",
                        icon("users", style = "font-size: 1.2rem; color: #17a2b8; margin-right: 6px;"),
                        div(
                          h4(textOutput("ebird_observers"), class = "mb-0 text-info", style = "font-size: 1.1rem; font-weight: bold;"),
                          div("Observers", class = "text-muted small")
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

#SERVER
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
      distinct(taxon_species_name, iconic_taxon_name) %>%
      filter(!is.na(iconic_taxon_name), iconic_taxon_name != "") %>%
      count(iconic_taxon_name) %>%
      arrange(desc(n)) %>%
      mutate(percentage = round(n/sum(n) * 100, 1))
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

shinyApp(ui, server)






#### Biodiversity Data ####

#### iNat and ebird data -------------------------------------------------------

library(shiny)
library(bslib)
library(dplyr)
library(plotly)
library(purrr)
library(forcats)
library(RColorBrewer)
library(viridisLite)

# Read & prepare processed data

# iNat data
inat_data <- read.csv("data/raw_data/biodiversity_data/inat_greatmeadow_20250825.csv")

# eBird data
eBird_data <- read.csv("data/raw_data/biodiversity_data/ebird_greatmeadow_20250825.csv")

eBird_tax <- read.csv("data/raw_data/biodiversity_data/eBird_taxonomy_v2024.csv")

# merge eBird data
merged_data <- full_join(
  eBird_data, 
  eBird_tax, 
  by = c("TAXONOMIC.ORDER" = "TAXON_ORDER")
) %>% 
  filter(!is.na(SCIENTIFIC.NAME))


#UI
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
      
      "))
  ),
  
  # Main title styled like hydrology app
  div(class = "main-title",
      h1("Biodiversity of Great Meadow"),
      p("Summary of participatory science observations")
  ),
  
  #### Side-by-side sections ####
  layout_columns(
    col_widths = c(6, 6),  # Equal width columns
    
    #### iNaturalist Section ####
    div(class = "p-3 border border-light rounded",
        h3("iNaturalist", class = "text-center text-success mb-3"),
        
        # Stats with icons ABOVE the pie chart
        div(class = "row g-0 mb-2",  # Added mb-3 for spacing below stats
            div(class = "col-6",
                card(
                  class = "text-center",
                  card_body(
                    div(class = "d-flex align-items-center justify-content-center",
                        icon("binoculars", style = "font-size: 1.2rem; color: #28a745; margin-right: 6px;"),
                        div(
                          h4(textOutput("inat_observations"), class = "mb-0 text-primary", style = "font-size: 1.1rem;"),
                          div("Observations", class = "text-muted small")
                        )
                    )
                  )
                )
            )
        ),
        div(class = "col-6",
            card(
              class = "text-center", 
              card_body(
                div(class = "d-flex align-items-center justify-content-center",
                    icon("users", style = "font-size: 1.2rem; color: #17a2b8; margin-right: 6px;"),
                    div(
                      h4(textOutput("inat_people"), class = "mb-0 text-info", style = "font-size: 1.1rem;"),
                      div("Observers", class = "text-muted small")
                    )
                    
                )
              )
            )
        ),
        
        # Pie chart for species by taxa BELOW stats
        div(class = "text-center",
            plotlyOutput("spp_plot", height = "350px"),
            div(id = "spp_legend", class = "mt-2")
        )
    ),
    
    #### eBird Section ####
    div(class = "p-3 border border-light rounded",
        h3("eBird", class = "text-center text-primary mb-3"),
        
        # Stats with icons ABOVE the pie chart
        div(class = "row g-0 mb-2",  # Added mb-3 for spacing below stats
            div(class = "col-6",
                card(
                  class = "text-center",
                  card_body(
                    div(class = "d-flex align-items-center justify-content-center",
                        icon("clipboard-list", style = "font-size: 1.2rem; color: #28a745; margin-right: 6px;"),
                        div(
                          h4(textOutput("ebird_checklists"), class = "mb-0 text-primary", style = "font-size: 1.1rem;"),
                          div("Checklists", class = "text-muted small")
                        )
                    )
                  )
                )
            )
        ),
        
        # Observers
        div(class = "col-6",
            card(
              class = "text-center", 
              card_body(
                div(class = "d-flex align-items-center justify-content-center",
                    icon("users", style = "font-size: 1.2rem; color: #17a2b8; margin-right: 6px;"),
                    div(
                      h4(textOutput("ebird_observers"), class = "mb-0 text-info", style = "font-size: 1.1rem;"),
                      div("Observers", class = "text-muted small")
                    )
                )
              )
            )
        ),
        
        # Pie chart for species by taxonomic group BELOW stats
        div(class = "text-center",
            plotlyOutput("ebird_spp_plot", height = "350px"),
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

#SERVER
server <- function(input, output, session) {
  
  # Function to generate a palette for any number of categories
  generate_palette <- function(n_colors, palette_type = "qualitative") {
    if(n_colors <= 8) {
      rev(viridis(n_colors, option = "D", end = 0.85))
    } else {
      # Use multiple viridis family palettes - all colorblind friendly
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
      distinct(taxon_species_name, iconic_taxon_name) %>%
      filter(!is.na(iconic_taxon_name), iconic_taxon_name != "") %>%
      count(iconic_taxon_name) %>%
      arrange(desc(n)) %>%
      mutate(percentage = round(n/sum(n) * 100, 1))
  })
  
  output$spp_plot <- renderPlotly({
    data <- spp_summary()
    total_spp <- sum(data$n)
    chart_colors <- get_chart_colors(data$iconic_taxon_name)
    
    plot_ly(data, 
            labels = ~iconic_taxon_name, 
            values = ~n,
            type = "pie", 
            hole = 0.4,
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
                             "<span style='font-size:12px'>Species</span>"),
               showarrow = FALSE,
               font = list(size = 16))
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
            hole = 0.4,
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
                             "<span style='font-size:12px'>Species</span>"),
               showarrow = FALSE,
               font = list(size = 16))
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

shinyApp(ui, server)




#### Biodiversity Data ####

#### iNat and ebird data -------------------------------------------------------

library(shiny)
library(bslib)
library(dplyr)
library(plotly)
library(purrr)
library(forcats)
library(RColorBrewer)
library(viridisLite)

# Read & prepare processed data

# iNat data
inat_data <- read.csv("data/raw_data/biodiversity_data/inat_greatmeadow_20250825.csv")

# eBird data
eBird_data <- read.csv("data/raw_data/biodiversity_data/ebird_greatmeadow_20250825.csv")

eBird_tax <- read.csv("data/raw_data/biodiversity_data/eBird_taxonomy_v2024.csv")

# merge eBird data
merged_data <- full_join(
  eBird_data, 
  eBird_tax, 
  by = c("TAXONOMIC.ORDER" = "TAXON_ORDER")
) %>% 
  filter(!is.na(SCIENTIFIC.NAME))


#UI
ui <- page_fluid(
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#2E8B57", 
    base_font = font_google("Open Sans"),
    heading_font = font_google("Open Sans", wght = c(400, 700))
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
      
    /* Keep all stat cards same height */
    .card .card-body {
      min-height: 100px;  
      display: flex;
      flex-direction: column;
      justify-content: center; 
      padding: 0.75rem;   
    }
    
    /* Numbers a bit smaller */
    .card h4 {
      font-size: 1.2rem;
      margin-bottom: 0.25rem;
    }

    /* Smaller labels */
    .card .text-muted.small {
      font-size: 0.75rem;
    }

    /* Smaller icons */
    .card i {
      font-size: 1.2rem !important;
      margin-bottom: 6px;
    }
      
    "))
  ),
  
  # Main title styled like hydrology app
  div(class = "main-title",
      h1("Biodiversity of Great Meadow"),
      p("Summary of participatory science observations")
  ),
  
  #### Side-by-side sections ####
  layout_columns(
    col_widths = c(6, 6),  # Equal width columns
    
    #### iNaturalist Section ####
    div(class = "p-3 border border-light rounded",
        h3("iNaturalist", class = "text-center text-primary mb-3"),
        
        # Stats with icons ABOVE the pie chart
        div(class = "row g-2 mb-3",
            
            # Observations
            div(class = "col-6",
                card(
                  class = "text-center",
                  card_body(
                    div(
                      class = "d-flex align-items-center justify-content-center",
                      icon("binoculars", style = "font-size: 1.2rem; color: #28a745; margin-right: 10px;"),
                      div(
                        h4(textOutput("inat_observations"), class = "mb-0 text-primary"),
                        div("Observations", class = "text-muted small")
                      )
                    )
                  )
                )
            ),
            
            # Observers
            div(class = "col-6",
                card(
                  class = "text-center",
                  card_body(
                    div(
                      class = "d-flex align-items-center justify-content-center",
                      icon("users", style = "font-size: 1.2rem; color: #17a2b8; margin-right: 10px;"),
                      div(
                        h4(textOutput("inat_people"), class = "mb-0 text-info"),
                        div("Observers", class = "text-muted small")
                      )
                    )
                  )
                )
            ),
            
            # Pie chart for species by taxa BELOW stats
            div(class = "text-center",
                plotlyOutput("spp_plot", height = "350px"),
                div(id = "spp_legend", class = "mt-2")
            )
        )
    ),
    
    #### eBird Section ####
    div(class = "p-3 border border-light rounded",
        h3("eBird", class = "text-center text-primary mb-3"),
        
        # Stats with icons ABOVE the pie chart
        div(class = "row g-2 mb-3",
            
            # Checklists
            div(class = "col-6",
                card(
                  class = "text-center",
                  card_body(
                    div(
                      class = "d-flex align-items-center justify-content-center",
                      icon("clipboard-list", style = "font-size: 1.2rem; color: #28a745; margin-right: 10px;"),
                      div(
                        h4(textOutput("ebird_checklists"), class = "mb-0 text-primary"),
                        div("Checklists", class = "text-muted small")
                      )
                    )
                  )
                )
            ),
            
            # Observers
            div(class = "col-6",
                card(
                  class = "text-center",
                  card_body(
                    div(
                      class = "d-flex align-items-center justify-content-center",
                      icon("users", style = "font-size: 1.2rem; color: #17a2b8; margin-right: 10px;"),
                      div(
                        h4(textOutput("ebird_observers"), class = "mb-0 text-info"),
                        div("Observers", class = "text-muted small")
                      )
                    )
                  )
                )
            )
        ),
        
        # Pie chart for species by taxonomic group BELOW stats
        div(class = "text-center",
            plotlyOutput("ebird_spp_plot", height = "350px"),
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

#SERVER
server <- function(input, output, session) {
  
  # Function to generate a palette for any number of categories
  generate_palette <- function(n_colors, palette_type = "qualitative") {
    if(n_colors <= 8) {
      rev(viridis(n_colors, option = "D", end = 0.85))
    } else {
      # Use multiple viridis family palettes - all colorblind friendly
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
      distinct(taxon_species_name, iconic_taxon_name) %>%
      filter(!is.na(iconic_taxon_name), iconic_taxon_name != "") %>%
      count(iconic_taxon_name) %>%
      arrange(desc(n)) %>%
      mutate(percentage = round(n/sum(n) * 100, 1))
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
            domain = list(x = c(0.1, 0.9), y = c(0.1, 0.9)),
            marker = list(colors = chart_colors,
                          line = list(color = "white", width = 2)),
            textinfo = "none",
            hovertemplate = "<b>%{label}</b><br>Species: %{value} (%{percent})<extra></extra>") %>%
      layout(
        showlegend = FALSE,
        margin = list(t = 5, b = 5, l = 5, r = 5),
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
            domain = list(x = c(0.1, 0.9), y = c(0.1, 0.9)),
            marker = list(colors = chart_colors,
                          line = list(color = "white", width = 2)),
            textinfo = "none",
            hovertemplate = "<b>%{label}</b><br>Species: %{value} (%{percent})<extra></extra>") %>%
      layout(
        showlegend = FALSE,
        margin = list(t = 5, b = 5, l = 5, r = 5),
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

shinyApp(ui, server)





#9/3/25 ------------------------------------------------------------------------

#### Biodiversity Data ####

#### iNat and ebird data -------------------------------------------------------

library(shiny)
library(bslib)
library(dplyr)
library(plotly)
library(purrr)
library(forcats)
library(RColorBrewer)
library(viridisLite)

# Read & prepare processed data

# iNat data
inat_data <- read.csv("data/raw_data/biodiversity_data/inat_greatmeadow_20250825.csv")

# eBird data
eBird_data <- read.csv("data/raw_data/biodiversity_data/ebird_greatmeadow_20250825.csv")

eBird_tax <- read.csv("data/raw_data/biodiversity_data/eBird_taxonomy_v2024.csv")

# merge eBird data
merged_data <- full_join(
  eBird_data, 
  eBird_tax, 
  by = c("TAXONOMIC.ORDER" = "TAXON_ORDER")
) %>% 
  filter(!is.na(SCIENTIFIC.NAME))


#UI
ui <- page_fluid(
  theme = bs_theme(
    bootswatch = "flatly",
    primary = "#2E8B57", 
    base_font = font_google("Open Sans")
  ),
  
  # Page Header
  div(
    class = "text-center mb-3",
    h2("Biodiversity of Great Meadow", class = "display-4 text-primary"),
    p("Summary of participatory science observations", class = "lead text-muted")
  ),
  
  #### Side-by-side sections ####
  layout_columns(
    col_widths = c(6, 6),  # Equal width columns
    
    #### iNaturalist Section ####
    div(class = "p-3 border border-light rounded",
        h3("iNaturalist", class = "text-center text-success mb-3"),
        
        # Stats with icons ABOVE the pie chart
        div(class = "row g-2 mb-3",  # Added mb-3 for spacing below stats
            div(class = "col-6",
                card(
                  class = "text-center",
                  card_body(
                    div(
                      icon("binoculars", style = "font-size: 1.5rem; color: #28a745; margin-bottom: 8px;"),
                      h4(textOutput("inat_observations"), class = "mb-1 text-primary"),
                      div("Observations", class = "text-muted small")
                    )
                  )
                )
            ),
            div(class = "col-6",
                card(
                  class = "text-center", 
                  card_body(
                    div(
                      icon("users", style = "font-size: 1.5rem; color: #17a2b8; margin-bottom: 8px;"),
                      h4(textOutput("inat_people"), class = "mb-1 text-info"),
                      div("Observers", class = "text-muted small")
                    )
                  )
                )
            )
        ),
        
        # Pie chart for species by taxa BELOW stats
        div(class = "text-center",
            plotlyOutput("spp_plot", height = "350px"),
            div(id = "spp_legend", class = "mt-2")
        )
    ),
    
    #### eBird Section ####
    div(class = "p-3 border border-light rounded",
        h3("eBird", class = "text-center text-primary mb-3"),
        
        # Stats with icons ABOVE the pie chart
        div(class = "row g-2 mb-3",  # Added mb-3 for spacing below stats
            div(class = "col-6",
                card(
                  class = "text-center",
                  card_body(
                    div(
                      icon("clipboard-list", style = "font-size: 1.5rem; color: #28a745; margin-bottom: 8px;"),
                      h4(textOutput("ebird_checklists"), class = "mb-1 text-primary"),
                      div("Checklists", class = "text-muted small")
                    )
                  )
                )
            ),
            div(class = "col-6",
                card(
                  class = "text-center", 
                  card_body(
                    div(
                      icon("users", style = "font-size: 1.5rem; color: #17a2b8; margin-bottom: 8px;"),
                      h4(textOutput("ebird_observers"), class = "mb-1 text-info"),
                      div("Observers", class = "text-muted small")
                    )
                  )
                )
            )
        ),
        
        # Pie chart for species by taxonomic group BELOW stats
        div(class = "text-center",
            plotlyOutput("ebird_spp_plot", height = "350px"),
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

#SERVER
server <- function(input, output, session) {
  
  # Function to generate a palette for any number of categories
  generate_palette <- function(n_colors, palette_type = "qualitative") {
    if(n_colors <= 8) {
      rev(viridis(n_colors, option = "D", end = 0.85))
    } else {
      # Use multiple viridis family palettes - all colorblind friendly
      base_colors <- c(
        rev(viridis(8, option = "D", end = 0.85)),  # Classic viridis (purple-teal)
        rev(viridis(6, option = "C", end = 0.9)),   # Plasma (purple-pink-orange)
        rev(rocket(6))                             
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
      distinct(taxon_species_name, iconic_taxon_name) %>%
      filter(!is.na(iconic_taxon_name), iconic_taxon_name != "") %>%
      count(iconic_taxon_name) %>%
      arrange(desc(n)) %>%
      mutate(percentage = round(n/sum(n) * 100, 1))
  })
  
  output$spp_plot <- renderPlotly({
    data <- spp_summary()
    total_spp <- sum(data$n)
    chart_colors <- get_chart_colors(data$iconic_taxon_name)
    
    plot_ly(data, 
            labels = ~iconic_taxon_name, 
            values = ~n,
            type = "pie", 
            hole = 0.4,
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
                             "<span style='font-size:12px'>Species</span>"),
               showarrow = FALSE,
               font = list(size = 16))
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
            hole = 0.4,
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
                             "<span style='font-size:12px'>Species</span>"),
               showarrow = FALSE,
               font = list(size = 16))
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

shinyApp(ui, server)



#8/29/25 -----------------------------------------------------------------------

#### Biodiversity Data ####

#### iNat and ebird data -------------------------------------------------------

library(shiny)
library(bslib)
library(dplyr)
library(plotly)
library(purrr)
library(forcats)
library(RColorBrewer)
library(viridisLite)

# Read & prepare processed data

# iNat data
inat_data <- read.csv("data/raw_data/biodiversity_data/inat_greatmeadow_20250825.csv")

# eBird data
eBird_data <- read.csv("data/raw_data/biodiversity_data/ebird_greatmeadow_20250825.csv")

eBird_tax <- read.csv("data/raw_data/biodiversity_data/eBird_taxonomy_v2024.csv")

# merge eBird data
merged_data <- full_join(
  eBird_data, 
  eBird_tax, 
  by = c("TAXONOMIC.ORDER" = "TAXON_ORDER")
) %>% 
  filter(!is.na(SCIENTIFIC.NAME))


#UI
ui <- page_fillable(
  theme = bs_theme(
    bootswatch = "flatly",
    primary = "#2E8B57", 
    base_font = font_google("Open Sans")
  ),
  
  # Page Header
  div(
    class = "text-center mb-2",
    h2("Biodiversity of Great Meadow", class = "display-4 text-primary"),
    p("Summary of participatory science observations", class = "lead text-muted")
  ),
  
  #### iNaturalist Section ####
  div(class = "my-3",
      h3("iNaturalist", class = "text-center text-success mb-3"),
      
      # Single pie chart for species by taxa
      div(class = "text-center mb-3",
          plotlyOutput("spp_plot", height = "400px"),
          div(id = "spp_legend", class = "mt-2")
      ),
      
      # Stats with icons underneath
      layout_columns(
        col_widths = c(6, 6),
        
        card(
          class = "text-center",
          card_body(
            div(
              icon("binoculars", style = "font-size: 2rem; color: #28a745; margin-bottom: 10px;"),
              h3(textOutput("inat_observations"), class = "mb-1 text-primary"),
              div("Observations", class = "text-muted")
            )
          )
        ),
        
        card(
          class = "text-center", 
          card_body(
            div(
              icon("users", style = "font-size: 2rem; color: #17a2b8; margin-bottom: 10px;"),
              h3(textOutput("inat_people"), class = "mb-1 text-info"),
              div("Observers", class = "text-muted")
            )
          )
        )
      )
  ),
  
  #### eBird Section ####
  div(class = "my-3",
      h2("eBird", class = "text-center text-primary mb-3"),
      
      # Single pie chart for species by taxonomic group
      div(class = "text-center mb-3",
          plotlyOutput("ebird_spp_plot", height = "400px"),
          div(id = "ebird_spp_legend", class = "mt-2")
      ),
      
      # Stats with icons underneath
      layout_columns(
        col_widths = c(6, 6),
        
        card(
          class = "text-center",
          card_body(
            div(
              icon("clipboard-list", style = "font-size: 2rem; color: #28a745; margin-bottom: 10px;"),
              h3(textOutput("ebird_checklists"), class = "mb-1 text-primary"),
              div("Checklists", class = "text-muted")
            )
          )
        ),
        
        card(
          class = "text-center", 
          card_body(
            div(
              icon("users", style = "font-size: 2rem; color: #17a2b8; margin-bottom: 10px;"),
              h3(textOutput("ebird_observers"), class = "mb-1 text-info"),
              div("Observers", class = "text-muted")
            )
          )
        )
      )
  ),
  
  # Footer
  hr(),
  div(
    class = "text-center text-muted small mt-2",
    p("Data sourced from iNaturalist and eBird • Last updated: ", Sys.Date())
  )
)

server <- function(input, output, session) {
  
  # Function to generate a palette for any number of categories
  generate_palette <- function(n_colors, palette_type = "qualitative") {
    if(n_colors <= 8) {
      rev(viridis(n_colors, option = "D", end = 0.85))
    } else {
      # Use multiple viridis family palettes - all colorblind friendly
      base_colors <- c(
        rev(viridis(8, option = "D", end = 0.85)),  # Classic viridis (purple-teal)
        rev(viridis(6, option = "C", end = 0.9)),   # Plasma (purple-pink-orange)
        rev(rocket(6))                             
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
  
  # Rest of your existing helper functions...
  create_legend <- function(categories, colors) {
    legend_items <- map2_chr(categories, colors, function(cat, col) {
      paste0('<span style="display: inline-block; margin-right: 15px; margin-bottom: 5px;">',
             '<span style="display: inline-block; width: 12px; height: 12px; ',
             'background-color: ', col, '; margin-right: 5px; border-radius: 2px;"></span>',
             '<span style="font-size: 12px;">', cat, '</span></span>')
    })
    paste(legend_items, collapse = "")
  }
  
  # ---- Species pie chart for iNaturalist ----
  spp_summary <- reactive({
    inat_data %>%
      distinct(taxon_species_name, iconic_taxon_name) %>%
      filter(!is.na(iconic_taxon_name), iconic_taxon_name != "") %>%
      count(iconic_taxon_name) %>%
      arrange(desc(n)) %>%
      mutate(percentage = round(n/sum(n) * 100, 1))
  })
  
  output$spp_plot <- renderPlotly({
    data <- spp_summary()
    total_spp <- sum(data$n)
    chart_colors <- get_chart_colors(data$iconic_taxon_name)
    
    plot_ly(data, 
            labels = ~iconic_taxon_name, 
            values = ~n,
            type = "pie", 
            hole = 0.4,
            marker = list(colors = chart_colors,
                          line = list(color = "white", width = 2)),
            textinfo = "none",
            hovertemplate = "<b>%{label}</b><br>Species: %{value} (%{percent})<extra></extra>") %>%
      layout(
        showlegend = FALSE,
        margin = list(t = 40, b = 40, l = 40, r = 40),
        annotations = list(
          list(x = 0.5, y = 0.5, 
               text = paste0("<b>", formatC(total_spp, big.mark = ","), "</b><br>",
                             "<span style='font-size:14px'>Species</span>"),
               showarrow = FALSE,
               font = list(size = 18))
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
            hole = 0.4,
            marker = list(colors = chart_colors,
                          line = list(color = "white", width = 2)),
            textinfo = "none",
            hovertemplate = "<b>%{label}</b><br>Species: %{value} (%{percent})<extra></extra>") %>%
      layout(
        showlegend = FALSE,
        margin = list(t = 40, b = 40, l = 40, r = 40),
        annotations = list(
          list(x = 0.5, y = 0.5, 
               text = paste0("<b>", formatC(total_spp, big.mark = ","), "</b><br>",
                             "<span style='font-size:14px'>Species</span>"),
               showarrow = FALSE,
               font = list(size = 18))
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

shinyApp(ui, server)



#8/29/25 -----------------------------------------------------------------------

#### Biodiversity Data ####

#### iNat and ebird data -------------------------------------------------------

library(shiny)
library(bslib)
library(dplyr)
library(plotly)
library(purrr)

# Read & prepare processed data

# iNat data
inat_data <- read.csv("data/raw_data/biodiversity_data/inat_greatmeadow_20250825.csv")

# eBird data
eBird_data <- read.csv("data/raw_data/biodiversity_data/ebird_greatmeadow_20250825.csv")

eBird_tax <- read.csv("data/raw_data/biodiversity_data/eBird_taxonomy_v2024.csv")

# merge eBird data
merged_data <- full_join(
  eBird_data, 
  eBird_tax, 
  by = c("TAXONOMIC.ORDER" = "TAXON_ORDER")
) %>% 
  filter(!is.na(SCIENTIFIC.NAME))


#UI
ui <- page_fillable(
  theme = bs_theme(
    bootswatch = "flatly",
    primary = "#2E8B57", 
    base_font = font_google("Open Sans")
  ),
  
  # Page Header
  div(
    class = "text-center mb-4",
    h2("Biodiversity of Great Meadow", class = "display-4 text-primary"),
    p("Summary of participatory science observations", class = "lead text-muted")
  ),
  
  #### iNaturalist Section ####
  div(class = "my-5",
      h3("iNaturalist", class = "text-center text-success mb-4"),
      
      # Single pie chart for species by taxa
      div(class = "text-center mb-4",
          plotlyOutput("spp_plot", height = "400px"),
          div(id = "spp_legend", class = "mt-3")
      ),
      
      # Stats with icons underneath
      layout_columns(
        col_widths = c(6, 6),
        
        card(
          class = "text-center",
          card_body(
            div(
              icon("binoculars", style = "font-size: 2rem; color: #28a745; margin-bottom: 10px;"),
              h3(textOutput("inat_observations"), class = "mb-1 text-primary"),
              div("Observations", class = "text-muted")
            )
          )
        ),
        
        card(
          class = "text-center", 
          card_body(
            div(
              icon("users", style = "font-size: 2rem; color: #17a2b8; margin-bottom: 10px;"),
              h3(textOutput("inat_people"), class = "mb-1 text-info"),
              div("Observers", class = "text-muted")
            )
          )
        )
      )
  ),
  
  #### eBird Section ####
  div(class = "my-5",
      h2("eBird", class = "text-center text-primary mb-4"),
      
      # Single pie chart for species by taxonomic group
      div(class = "text-center mb-4",
          plotlyOutput("ebird_spp_plot", height = "400px"),
          div(id = "ebird_spp_legend", class = "mt-3")
      ),
      
      # Stats with icons underneath
      layout_columns(
        col_widths = c(6, 6),
        
        card(
          class = "text-center",
          card_body(
            div(
              icon("clipboard-list", style = "font-size: 2rem; color: #28a745; margin-bottom: 10px;"),
              h3(textOutput("ebird_checklists"), class = "mb-1 text-primary"),
              div("Checklists", class = "text-muted")
            )
          )
        ),
        
        card(
          class = "text-center", 
          card_body(
            div(
              icon("users", style = "font-size: 2rem; color: #17a2b8; margin-bottom: 10px;"),
              h3(textOutput("ebird_observers"), class = "mb-1 text-info"),
              div("Observers", class = "text-muted")
            )
          )
        )
      )
  ),
  
  # Footer
  hr(),
  div(
    class = "text-center text-muted small mt-3",
    p("Data sourced from iNaturalist and eBird • Last updated: ", Sys.Date())
  )
)

server <- function(input, output, session) {
  
  # Define color palettes
  taxon_colors <- c(    
    # Primary naturalist colors
    "#2E8B57", "#1ABC9C","#556B2F", "#4A90E2", "#8E44AD", 
    "#F39C12", "#FF6B35","#E74C3C", "#3498DB", "#9B59B6",
    
    # Secondary vibrant colors
    "#FF5733", "#33FF57", "#3357FF", "#FF33F5", "#F5FF33",
    "#33FFF5", "#F533FF", "#57FF33", "#FF3357", "#5733FF",
    
    # Earthy/natural tones
    "#8B4513", "#2F4F4F", "#27AE60", "#8B008B", "#FF4500",
    "#32CD32", "#FFD700", "#DC143C", "#00CED1", "#9932CC",
    
    # Pastel variations
    "#FFB6C1", "#98FB98", "#87CEEB", "#DDA0DD", "#F0E68C",
    "#AFEEEE", "#DB7093", "#90EE90", "#FFA07A", "#20B2AA",
    
    # Additional distinct colors
    "#CD853F", "#4682B4", "#D2691E", "#B0C4DE", "#F4A460",
    "#6495ED", "#DEB887", "#5F9EA0", "#A0522D", "#2E8B57",
    
    # Final set for very large datasets
    "#7B68EE", "#FA8072", "#FFA500", "#32CD32", "#FF69B4",
    "#00FF7F", "#FF1493", "#1E90FF", "#FFD700", "#ADFF2F"
  )
  
  # Helper function to create legend HTML
  create_legend <- function(categories, colors) {
    legend_items <- map2_chr(categories, colors, function(cat, col) {
      paste0('<span style="display: inline-block; margin-right: 15px; margin-bottom: 5px;">',
             '<span style="display: inline-block; width: 12px; height: 12px; ',
             'background-color: ', col, '; margin-right: 5px; border-radius: 2px;"></span>',
             '<span style="font-size: 12px;">', cat, '</span></span>')
    })
    paste(legend_items, collapse = "")
  }
  
  # ---- Species pie chart for iNaturalist ----
  spp_summary <- reactive({
    inat_data %>%
      distinct(taxon_species_name, iconic_taxon_name) %>%
      filter(!is.na(iconic_taxon_name), iconic_taxon_name != "") %>%
      count(iconic_taxon_name) %>%
      arrange(desc(n)) %>%
      mutate(
        percentage = round(n/sum(n) * 100, 1)
      )
  })
  
  output$spp_plot <- renderPlotly({
    data <- spp_summary()
    total_spp <- sum(data$n)
    
    plot_ly(data, 
            labels = ~iconic_taxon_name, 
            values = ~n,
            type = "pie", 
            hole = 0.4,
            marker = list(colors = taxon_colors[1:nrow(data)],
                          line = list(color = "white", width = 2)),
            textinfo = "none",
            hovertemplate = "<b>%{label}</b><br>Species: %{value} (%{percent})<extra></extra>") %>%
      layout(
        showlegend = FALSE,
        margin = list(t = 40, b = 40, l = 40, r = 40),
        annotations = list(
          list(x = 0.5, y = 0.5, 
               text = paste0("<b>", formatC(total_spp, big.mark = ","), "</b><br>",
                             "<span style='font-size:14px'>Species</span>"),
               showarrow = FALSE,
               font = list(size = 18))
        )
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # Create legend for iNat species
  observe({
    data <- spp_summary()
    legend_html <- create_legend(data$iconic_taxon_name, taxon_colors[1:nrow(data)])
    insertUI(selector = "#spp_legend", 
             ui = div(HTML(legend_html), class = "text-center"),
             immediate = TRUE)
  })
  
  # ---- eBird species pie chart ----
  ebird_spp_summary <- reactive({
    merged_data %>%
      distinct(SCIENTIFIC.NAME, SPECIES_GROUP) %>%
      count(SPECIES_GROUP) %>%
      arrange(desc(n)) %>%
      mutate(
        percentage = round(n/sum(n) * 100, 1)
      )
  })
  
  output$ebird_spp_plot <- renderPlotly({
    data <- ebird_spp_summary()
    total_spp <- sum(data$n)
    
    plot_ly(data, 
            labels = ~SPECIES_GROUP, 
            values = ~n,
            type = "pie", 
            hole = 0.4,
            marker = list(colors = taxon_colors[1:nrow(data)],
                          line = list(color = "white", width = 2)),
            textinfo = "none",
            hovertemplate = "<b>%{label}</b><br>Species: %{value} (%{percent})<extra></extra>") %>%
      layout(
        showlegend = FALSE,
        margin = list(t = 40, b = 40, l = 40, r = 40),
        annotations = list(
          list(x = 0.5, y = 0.5, 
               text = paste0("<b>", formatC(total_spp, big.mark = ","), "</b><br>",
                             "<span style='font-size:14px'>Species</span>"),
               showarrow = FALSE,
               font = list(size = 18))
        )
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # Create legend for eBird species
  observe({
    data <- ebird_spp_summary()
    legend_html <- create_legend(data$SPECIES_GROUP, taxon_colors[1:nrow(data)])
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

shinyApp(ui, server)














#### Biodiversity Data ####

#### iNat and ebird data -------------------------------------------------------

library(shiny)
library(bslib)
library(dplyr)
library(plotly)
library(purrr)
library(forcats)

# Read & prepare processed data

# iNat data
inat_data <- read.csv("data/raw_data/biodiversity_data/inat_greatmeadow_20250825.csv")

# eBird data
eBird_data <- read.csv("data/raw_data/biodiversity_data/ebird_greatmeadow_20250825.csv")

eBird_tax <- read.csv("data/raw_data/biodiversity_data/eBird_taxonomy_v2024.csv")

# merge eBird data
merged_data <- full_join(
  eBird_data, 
  eBird_tax, 
  by = c("TAXONOMIC.ORDER" = "TAXON_ORDER")
) %>% 
  filter(!is.na(SCIENTIFIC.NAME))


#UI
ui <- page_fillable(
  theme = bs_theme(
    bootswatch = "flatly",
    primary = "#2E8B57", 
    base_font = font_google("Open Sans")
  ),
  
  # Page Header
  div(
    class = "text-center mb-4",
    h2("Biodiversity of Great Meadow", class = "display-4 text-primary"),
    p("Summary of participatory science observations", class = "lead text-muted")
  ),
  
  #### iNaturalist Section ####
  div(class = "my-5",
      h3("iNaturalist", class = "text-center text-success mb-4"),
      
      # Single pie chart for species by taxa
      div(class = "text-center mb-4",
          plotlyOutput("spp_plot", height = "400px"),
          div(id = "spp_legend", class = "mt-3")
      ),
      
      # Stats with icons underneath
      layout_columns(
        col_widths = c(6, 6),
        
        card(
          class = "text-center",
          card_body(
            div(
              icon("binoculars", style = "font-size: 2rem; color: #28a745; margin-bottom: 10px;"),
              h3(textOutput("inat_observations"), class = "mb-1 text-primary"),
              div("Observations", class = "text-muted")
            )
          )
        ),
        
        card(
          class = "text-center", 
          card_body(
            div(
              icon("users", style = "font-size: 2rem; color: #17a2b8; margin-bottom: 10px;"),
              h3(textOutput("inat_people"), class = "mb-1 text-info"),
              div("Observers", class = "text-muted")
            )
          )
        )
      )
  ),
  
  #### eBird Section ####
  div(class = "my-5",
      h2("eBird", class = "text-center text-primary mb-4"),
      
      # Single pie chart for species by taxonomic group
      div(class = "text-center mb-4",
          plotlyOutput("ebird_spp_plot", height = "400px"),
          div(id = "ebird_spp_legend", class = "mt-3")
      ),
      
      # Stats with icons underneath
      layout_columns(
        col_widths = c(6, 6),
        
        card(
          class = "text-center",
          card_body(
            div(
              icon("clipboard-list", style = "font-size: 2rem; color: #28a745; margin-bottom: 10px;"),
              h3(textOutput("ebird_checklists"), class = "mb-1 text-primary"),
              div("Checklists", class = "text-muted")
            )
          )
        ),
        
        card(
          class = "text-center", 
          card_body(
            div(
              icon("users", style = "font-size: 2rem; color: #17a2b8; margin-bottom: 10px;"),
              h3(textOutput("ebird_observers"), class = "mb-1 text-info"),
              div("Observers", class = "text-muted")
            )
          )
        )
      )
  ),
  
  # Footer
  hr(),
  div(
    class = "text-center text-muted small mt-3",
    p("Data sourced from iNaturalist and eBird • Last updated: ", Sys.Date())
  )
)

server <- function(input, output, session) {
  
  # Define color palettes
  taxon_colors <- c(    
    # Primary naturalist colors
    
    
    "#556B2F","#1ABC9C", "#90EE90","#4682B4", "#3498DB",
    "#1E90FF", "#3357FF","#F39C12", "#FF6B35","#E74C3C", "#8B4513",
    "#8B008B", "#8E44AD", "#9932CC", "#DDA0DD","#F533FF","#FF33F5", 
    "#FF1493", "#FF69B4", "#FF3357", "#DC143C",
    
    "#4A90E2", "#8E44AD", 
    "#F39C12", "#FF6B35","#E74C3C", "#3498DB", "#9B59B6",
    "#2E8B57","#27AE60",
    
    # Secondary vibrant colors
    "#FF5733", "#33FF57", "#3357FF", "#FF33F5", "#F5FF33",
    "#33FFF5", "#F533FF", "#57FF33", "#FF3357", "#5733FF",
    
    # Earthy/natural tones
    "#8B4513", "#2F4F4F", "#27AE60", "#8B008B", "#FF4500",
    "#32CD32", "#FFD700", "#DC143C", "#00CED1", "#9932CC",
    
    # Pastel variations
    "#FFB6C1", "#98FB98", "#87CEEB", "#DDA0DD", "#F0E68C",
    "#AFEEEE", "#DB7093", "#90EE90", "#FFA07A", "#20B2AA",
    
    # Additional distinct colors
    "#CD853F", "#4682B4", "#D2691E", "#B0C4DE", "#F4A460",
    "#6495ED", "#DEB887", "#5F9EA0", "#A0522D", "#2E8B57",
    
    # Final set for very large datasets
    "#7B68EE", "#FA8072", "#FFA500", "#32CD32", "#FF69B4",
    "#00FF7F", "#FF1493", "#1E90FF", "#FFD700", "#ADFF2F"
  )
  
  # Helper function to create legend HTML
  create_legend <- function(categories, colors) {
    legend_items <- map2_chr(categories, colors, function(cat, col) {
      paste0('<span style="display: inline-block; margin-right: 15px; margin-bottom: 5px;">',
             '<span style="display: inline-block; width: 12px; height: 12px; ',
             'background-color: ', col, '; margin-right: 5px; border-radius: 2px;"></span>',
             '<span style="font-size: 12px;">', cat, '</span></span>')
    })
    paste(legend_items, collapse = "")
  }
  
  # ---- Species pie chart for iNaturalist ----
  spp_summary <- reactive({
    inat_data %>%
      distinct(taxon_species_name, iconic_taxon_name) %>%
      filter(!is.na(iconic_taxon_name), iconic_taxon_name != "") %>%
      count(iconic_taxon_name) %>%
      arrange(desc(n)) %>%
      mutate(
        percentage = round(n/sum(n) * 100, 1)
      )
  })
  
  output$spp_plot <- renderPlotly({
    data <- spp_summary()
    total_spp <- sum(data$n)
    
    plot_ly(data, 
            labels = ~iconic_taxon_name, 
            values = ~n,
            type = "pie", 
            hole = 0.4,
            marker = list(colors = taxon_colors[1:nrow(data)],
                          line = list(color = "white", width = 2)),
            textinfo = "none",
            hovertemplate = "<b>%{label}</b><br>Species: %{value} (%{percent})<extra></extra>") %>%
      layout(
        showlegend = FALSE,
        margin = list(t = 40, b = 40, l = 40, r = 40),
        annotations = list(
          list(x = 0.5, y = 0.5, 
               text = paste0("<b>", formatC(total_spp, big.mark = ","), "</b><br>",
                             "<span style='font-size:14px'>Species</span>"),
               showarrow = FALSE,
               font = list(size = 18))
        )
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # Create legend for iNat species
  observe({
    data <- spp_summary()
    legend_html <- create_legend(data$iconic_taxon_name, taxon_colors[1:nrow(data)])
    insertUI(selector = "#spp_legend", 
             ui = div(HTML(legend_html), class = "text-center"),
             immediate = TRUE)
  })
  
  # ---- eBird species pie chart ----
  ebird_spp_summary <- reactive({
    df <- merged_data %>%
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
    
    plot_ly(data, 
            labels = ~SPECIES_GROUP, 
            values = ~n,
            type = "pie", 
            hole = 0.4,
            marker = list(colors = taxon_colors[1:nrow(data)],
                          line = list(color = "white", width = 2)),
            textinfo = "none",
            hovertemplate = "<b>%{label}</b><br>Species: %{value} (%{percent})<extra></extra>") %>%
      layout(
        showlegend = FALSE,
        margin = list(t = 40, b = 40, l = 40, r = 40),
        annotations = list(
          list(x = 0.5, y = 0.5, 
               text = paste0("<b>", formatC(total_spp, big.mark = ","), "</b><br>",
                             "<span style='font-size:14px'>Species</span>"),
               showarrow = FALSE,
               font = list(size = 18))
        )
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # Create legend for eBird species
  observe({
    data <- ebird_spp_summary()
    legend_html <- create_legend(data$SPECIES_GROUP, taxon_colors[1:nrow(data)])
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

shinyApp(ui, server)










#### Biodiversity Data ####

#### iNat and ebird data -------------------------------------------------------

library(shiny)
library(bslib)
library(dplyr)
library(plotly)
library(purrr)
library(forcats)
library(RColorBrewer)

# Read & prepare processed data

# iNat data
inat_data <- read.csv("data/raw_data/biodiversity_data/inat_greatmeadow_20250825.csv")

# eBird data
eBird_data <- read.csv("data/raw_data/biodiversity_data/ebird_greatmeadow_20250825.csv")

eBird_tax <- read.csv("data/raw_data/biodiversity_data/eBird_taxonomy_v2024.csv")

# merge eBird data
merged_data <- full_join(
  eBird_data, 
  eBird_tax, 
  by = c("TAXONOMIC.ORDER" = "TAXON_ORDER")
) %>% 
  filter(!is.na(SCIENTIFIC.NAME))


#UI
ui <- page_fillable(
  theme = bs_theme(
    bootswatch = "flatly",
    primary = "#2E8B57", 
    base_font = font_google("Open Sans")
  ),
  
  # Page Header
  div(
    class = "text-center mb-4",
    h2("Biodiversity of Great Meadow", class = "display-4 text-primary"),
    p("Summary of participatory science observations", class = "lead text-muted")
  ),
  
  #### iNaturalist Section ####
  div(class = "my-5",
      h3("iNaturalist", class = "text-center text-success mb-4"),
      
      # Single pie chart for species by taxa
      div(class = "text-center mb-4",
          plotlyOutput("spp_plot", height = "400px"),
          div(id = "spp_legend", class = "mt-3")
      ),
      
      # Stats with icons underneath
      layout_columns(
        col_widths = c(6, 6),
        
        card(
          class = "text-center",
          card_body(
            div(
              icon("binoculars", style = "font-size: 2rem; color: #28a745; margin-bottom: 10px;"),
              h3(textOutput("inat_observations"), class = "mb-1 text-primary"),
              div("Observations", class = "text-muted")
            )
          )
        ),
        
        card(
          class = "text-center", 
          card_body(
            div(
              icon("users", style = "font-size: 2rem; color: #17a2b8; margin-bottom: 10px;"),
              h3(textOutput("inat_people"), class = "mb-1 text-info"),
              div("Observers", class = "text-muted")
            )
          )
        )
      )
  ),
  
  #### eBird Section ####
  div(class = "my-5",
      h2("eBird", class = "text-center text-primary mb-4"),
      
      # Single pie chart for species by taxonomic group
      div(class = "text-center mb-4",
          plotlyOutput("ebird_spp_plot", height = "400px"),
          div(id = "ebird_spp_legend", class = "mt-3")
      ),
      
      # Stats with icons underneath
      layout_columns(
        col_widths = c(6, 6),
        
        card(
          class = "text-center",
          card_body(
            div(
              icon("clipboard-list", style = "font-size: 2rem; color: #28a745; margin-bottom: 10px;"),
              h3(textOutput("ebird_checklists"), class = "mb-1 text-primary"),
              div("Checklists", class = "text-muted")
            )
          )
        ),
        
        card(
          class = "text-center", 
          card_body(
            div(
              icon("users", style = "font-size: 2rem; color: #17a2b8; margin-bottom: 10px;"),
              h3(textOutput("ebird_observers"), class = "mb-1 text-info"),
              div("Observers", class = "text-muted")
            )
          )
        )
      )
  ),
  
  # Footer
  hr(),
  div(
    class = "text-center text-muted small mt-3",
    p("Data sourced from iNaturalist and eBird • Last updated: ", Sys.Date())
  )
)

server <- function(input, output, session) {
  
  # Function to generate a palette for any number of categories
  generate_palette <- function(n_colors, palette_type = "qualitative") {
    if(n_colors <= 8) {  # Changed from 9 to 8 since we're removing one color
      # Use reversed PRGn but skip the light middle color
      full_palette <- rev(brewer.pal(9, "PRGn"))
      # Remove the 5th color (middle light color) - it's usually #F7F7F7 or similar
      filtered_palette <- full_palette[-5]  # Remove the middle light color
      filtered_palette[1:min(n_colors, length(filtered_palette))]
    } else if(n_colors <= 11) {
      # Use BrBG for 9-11 categories
      rev(brewer.pal(n_colors, "BrBG"))
    } else {
      # For larger numbers, create a custom palette
      full_prgn <- rev(brewer.pal(9, "PRGn"))
      filtered_prgn <- full_prgn[-5]  # Remove middle light color
      
      base_colors <- c(
        filtered_prgn,                   # Filtered PRGn (8 colors)
        brewer.pal(11, "BrBG"),         # Brown to blue-green (11 colors) 
        brewer.pal(8, "Dark2")[1:4]     # Add some darker colors (4 colors)
      )
      
      if(length(base_colors) < n_colors) {
        additional_needed <- n_colors - length(base_colors)
        additional_colors <- rainbow(additional_needed, start = 0.1, end = 0.9, v = 0.8)
        base_colors <- c(base_colors, additional_colors)
      }
      
      base_colors[1:n_colors]
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
  
  # Rest of your existing helper functions...
  create_legend <- function(categories, colors) {
    legend_items <- map2_chr(categories, colors, function(cat, col) {
      paste0('<span style="display: inline-block; margin-right: 15px; margin-bottom: 5px;">',
             '<span style="display: inline-block; width: 12px; height: 12px; ',
             'background-color: ', col, '; margin-right: 5px; border-radius: 2px;"></span>',
             '<span style="font-size: 12px;">', cat, '</span></span>')
    })
    paste(legend_items, collapse = "")
  }
  
  # ---- Species pie chart for iNaturalist ----
  spp_summary <- reactive({
    inat_data %>%
      distinct(taxon_species_name, iconic_taxon_name) %>%
      filter(!is.na(iconic_taxon_name), iconic_taxon_name != "") %>%
      count(iconic_taxon_name) %>%
      arrange(desc(n)) %>%
      mutate(percentage = round(n/sum(n) * 100, 1))
  })
  
  output$spp_plot <- renderPlotly({
    data <- spp_summary()
    total_spp <- sum(data$n)
    chart_colors <- get_chart_colors(data$iconic_taxon_name)
    
    plot_ly(data, 
            labels = ~iconic_taxon_name, 
            values = ~n,
            type = "pie", 
            hole = 0.4,
            marker = list(colors = chart_colors,
                          line = list(color = "white", width = 2)),
            textinfo = "none",
            hovertemplate = "<b>%{label}</b><br>Species: %{value} (%{percent})<extra></extra>") %>%
      layout(
        showlegend = FALSE,
        margin = list(t = 40, b = 40, l = 40, r = 40),
        annotations = list(
          list(x = 0.5, y = 0.5, 
               text = paste0("<b>", formatC(total_spp, big.mark = ","), "</b><br>",
                             "<span style='font-size:14px'>Species</span>"),
               showarrow = FALSE,
               font = list(size = 18))
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
            hole = 0.4,
            marker = list(colors = chart_colors,
                          line = list(color = "white", width = 2)),
            textinfo = "none",
            hovertemplate = "<b>%{label}</b><br>Species: %{value} (%{percent})<extra></extra>") %>%
      layout(
        showlegend = FALSE,
        margin = list(t = 40, b = 40, l = 40, r = 40),
        annotations = list(
          list(x = 0.5, y = 0.5, 
               text = paste0("<b>", formatC(total_spp, big.mark = ","), "</b><br>",
                             "<span style='font-size:14px'>Species</span>"),
               showarrow = FALSE,
               font = list(size = 18))
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

shinyApp(ui, server)




#8/28/25------------------------------------------------------------------------

#### Biodiversity Data ####

#### iNat and ebird data -------------------------------------------------------

library(shiny)
library(bslib)
library(dplyr)
library(plotly)
library(purrr)

# Read & prepare processed data

# iNat data
inat_data <- read.csv("data/raw_data/biodiversity_data/inat_greatmeadow_20250825.csv")

# eBird data
eBird_data <- read.csv("data/raw_data/biodiversity_data/ebird_greatmeadow_20250825.csv")

# UI
ui <- page_fillable(
  theme = bs_theme(
    bootswatch = "flatly",
    primary = "#2E8B57", 
    base_font = font_google("Open Sans")
  ),
  
  # Header
  div(
    class = "text-center mb-4",
    h1("iNaturalist Biodiversity Dashboard", 
       class = "display-4 text-primary"),
    p("Summary of observations from Great Meadow", 
      class = "lead text-muted")
  ),
  
  # Main content
  layout_columns(
    col_widths = c(4, 4, 4),
    
    # Observations
    div(
      class = "text-center",
      plotlyOutput("obs_plot", height = "300px"),
      div(id = "obs_legend", class = "mt-3")
    ),
    
    # Species
    div(
      class = "text-center",
      plotlyOutput("spp_plot", height = "300px"),
      div(id = "spp_legend", class = "mt-3")
    ),
    
    # People
    div(
      class = "text-center",
      plotlyOutput("ppl_plot", height = "300px"),
      div(id = "ppl_legend", class = "mt-3")
    )
  ),
  
  # Footer
  hr(),
  div(
    class = "text-center text-muted small mt-3",
    p("Data sourced from iNaturalist • Last updated: ", Sys.Date())
  )
)

# SERVER
server <- function(input, output, session) {
  
  # Define color palettes
  quality_colors <- c("research" = "#28a745", "needs_id" = "#ffc107", "casual" = "#6c757d")
  taxon_colors <- c("#FF6B6B", "#4ECDC4", "#45B7D1", "#96CEB4", "#FECA57", 
                    "#FF9FF3", "#54A0FF", "#5F27CD", "#00D2D3", "#FF9F43",
                    "#A8E6CF", "#FFB347", "#87CEEB", "#DDA0DD", "#F0E68C")
  
  # Helper function to create legend HTML
  create_legend <- function(categories, colors) {
    legend_items <- map2_chr(categories, colors, function(cat, col) {
      paste0('<span style="display: inline-block; margin-right: 15px; margin-bottom: 5px;">',
             '<span style="display: inline-block; width: 12px; height: 12px; ',
             'background-color: ', col, '; margin-right: 5px; border-radius: 2px;"></span>',
             '<span style="font-size: 12px;">', cat, '</span></span>')
    })
    paste(legend_items, collapse = "")
  }
  
  # ---- Observations ----
  obs_summary <- reactive({
    inat_data %>%
      distinct(uuid, quality_grade) %>%
      count(quality_grade) %>%
      mutate(
        percentage = round(n/sum(n) * 100, 1)
      )
  })
  
  output$obs_plot <- renderPlotly({
    data <- obs_summary()
    total_obs <- sum(data$n)
    
    plot_ly(data, 
            labels = ~quality_grade, 
            values = ~n,
            type = "pie", 
            hole = 0.6,
            marker = list(colors = quality_colors[data$quality_grade],
                          line = list(color = "white", width = 2)),
            textinfo = "none",
            hovertemplate = "<b>%{label}</b><br>Count: %{value} (%{percent})<extra></extra>") %>%
      layout(
        showlegend = FALSE,
        margin = list(t = 20, b = 20, l = 20, r = 20),
        annotations = list(
          list(x = 0.5, y = 0.5, 
               text = paste0("<b>", formatC(total_obs, big.mark = ","), "</b><br>",
                             "<span style='font-size:12px'>Observations</span>"),
               showarrow = FALSE,
               font = list(size = 16))
        )
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # Create legend for observations
  observe({
    data <- obs_summary()
    legend_html <- create_legend(data$quality_grade, quality_colors[data$quality_grade])
    insertUI(selector = "#obs_legend", 
             ui = div(HTML(legend_html), class = "text-center"),
             immediate = TRUE)
  })
  
  # ---- Species ----
  spp_summary <- reactive({
    inat_data %>%
      distinct(taxon_species_name, iconic_taxon_name) %>%
      count(iconic_taxon_name) %>%
      arrange(desc(n)) %>%
      mutate(
        percentage = round(n/sum(n) * 100, 1)
      )
  })
  
  output$spp_plot <- renderPlotly({
    data <- spp_summary()
    total_spp <- sum(data$n)
    
    plot_ly(data, 
            labels = ~iconic_taxon_name, 
            values = ~n,
            type = "pie", 
            hole = 0.6,
            marker = list(colors = taxon_colors[1:nrow(data)],
                          line = list(color = "white", width = 2)),
            textinfo = "none",
            hovertemplate = "<b>%{label}</b><br>Count: %{value} (%{percent})<extra></extra>") %>%
      layout(
        showlegend = FALSE,
        margin = list(t = 20, b = 20, l = 20, r = 20),
        annotations = list(
          list(x = 0.5, y = 0.5, 
               text = paste0("<b>", formatC(total_spp, big.mark = ","), "</b><br>",
                             "<span style='font-size:12px'>Species</span>"),
               showarrow = FALSE,
               font = list(size = 16))
        )
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # Create legend for species
  observe({
    data <- spp_summary()
    legend_html <- create_legend(data$iconic_taxon_name, taxon_colors[1:nrow(data)])
    insertUI(selector = "#spp_legend", 
             ui = div(HTML(legend_html), class = "text-center"),
             immediate = TRUE)
  })
  
  # ---- People ----
  output$ppl_plot <- renderPlotly({
    n_people <- n_distinct(inat_data$user_id)
    
    plot_ly(labels = "Contributors", 
            values = n_people,
            type = "pie", 
            hole = 0.6,
            marker = list(colors = "#17a2b8",
                          line = list(color = "white", width = 2)),
            textinfo = "none",
            hovertemplate = "<b>Contributors</b><br>Count: %{value}<extra></extra>") %>%
      layout(
        showlegend = FALSE,
        margin = list(t = 20, b = 20, l = 20, r = 20),
        annotations = list(
          list(x = 0.5, y = 0.5, 
               text = paste0("<b>", formatC(n_people, big.mark = ","), "</b><br>",
                             "<span style='font-size:12px'>People</span>"),
               showarrow = FALSE,
               font = list(size = 16))
        )
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # Create legend for people (simple single item)
  observe({
    legend_html <- create_legend("Contributors", "#17a2b8")
    insertUI(selector = "#ppl_legend", 
             ui = div(HTML(legend_html), class = "text-center"),
             immediate = TRUE)
  })
}

shinyApp(ui, server)












#### iNat and ebird data -------------------------------------------------------

library(shiny)
library(bslib)
library(dplyr)
library(plotly)
library(purrr)

# Read & prepare processed data

# iNat data
inat_data <- read.csv("data/raw_data/biodiversity_data/inat_greatmeadow_20250825.csv")

# eBird data
eBird_data <- read.csv("data/raw_data/biodiversity_data/ebird_greatmeadow_20250825.csv")


ui <- page_fillable(
  theme = bs_theme(
    bootswatch = "flatly",
    primary = "#2E8B57", 
    base_font = font_google("Open Sans")
  ),
  
  # Page Header
  div(
    class = "text-center mb-4",
    h2("Biodiversity of Great Meadow", class = "display-4 text-primary"),
    p("Summary of participatory science observations", class = "lead text-muted")
  ),
  
  #### iNaturalist Section ####
  div(class = "my-5",
      h3("iNaturalist", class = "text-center text-success mb-4"),
      layout_columns(
        col_widths = c(4,4,4),
        
        div(class = "text-center",
            plotlyOutput("obs_plot", height = "250px"),
            div(id = "obs_legend", class = "mt-3")
        ),
        
        div(class = "text-center",
            plotlyOutput("spp_plot", height = "250px"),
            div(id = "spp_legend", class = "mt-3")
        ),
        
        div(class = "text-center",
            plotlyOutput("ppl_plot", height = "250px"),
            div(id = "ppl_legend", class = "mt-3")
        )
      )
  ),
  
  #### eBird Section ####
  div(class = "my-5",
      h3("eBird", class = "text-center text-primary mb-4"),
      layout_columns(
        col_widths = c(4,4,4),
        
        div(class = "text-center",
            plotlyOutput("ebird_observations_plot", height = "250px")
        ),
        
        div(class = "text-center",
            plotlyOutput("ebird_spp_plot", height = "250px")
        ),
        
        div(class = "text-center",
            plotlyOutput("ebird_obs_plot", height = "250px")
        )
      )
  ),
  
  # Footer
  hr(),
  div(
    class = "text-center text-muted small mt-3",
    p("Data sourced from iNaturalist and eBird • Last updated: ", Sys.Date())
  )
)

server <- function(input, output, session) {
  
  # Define color palettes
  quality_colors <- c("research" = "#28a745", "needs_id" = "#ffc107", "casual" = "#6c757d")
  taxon_colors <- c("#FF6B6B", "#4ECDC4", "#45B7D1", "#96CEB4", "#FECA57", 
                    "#FF9FF3", "#54A0FF", "#5F27CD", "#00D2D3", "#FF9F43",
                    "#A8E6CF", "#FFB347", "#87CEEB", "#DDA0DD", "#F0E68C")
  
  # Helper function to create legend HTML
  create_legend <- function(categories, colors) {
    legend_items <- map2_chr(categories, colors, function(cat, col) {
      paste0('<span style="display: inline-block; margin-right: 15px; margin-bottom: 5px;">',
             '<span style="display: inline-block; width: 12px; height: 12px; ',
             'background-color: ', col, '; margin-right: 5px; border-radius: 2px;"></span>',
             '<span style="font-size: 12px;">', cat, '</span></span>')
    })
    paste(legend_items, collapse = "")
  }
  
  # ---- Observations ----
  obs_summary <- reactive({
    inat_data %>%
      distinct(uuid, quality_grade) %>%
      count(quality_grade) %>%
      mutate(
        percentage = round(n/sum(n) * 100, 1)
      )
  })
  
  output$obs_plot <- renderPlotly({
    data <- obs_summary()
    total_obs <- sum(data$n)
    
    plot_ly(data, 
            labels = ~quality_grade, 
            values = ~n,
            type = "pie", 
            hole = 0.6,
            marker = list(colors = quality_colors[data$quality_grade],
                          line = list(color = "white", width = 2)),
            textinfo = "none",
            hovertemplate = "<b>%{label}</b><br>Count: %{value} (%{percent})<extra></extra>") %>%
      layout(
        showlegend = FALSE,
        margin = list(t = 20, b = 20, l = 20, r = 20),
        annotations = list(
          list(x = 0.5, y = 0.5, 
               text = paste0("<b>", formatC(total_obs, big.mark = ","), "</b><br>",
                             "<span style='font-size:12px'>Observations</span>"),
               showarrow = FALSE,
               font = list(size = 16))
        )
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # Create legend for observations
  observe({
    data <- obs_summary()
    legend_html <- create_legend(data$quality_grade, quality_colors[data$quality_grade])
    insertUI(selector = "#obs_legend", 
             ui = div(HTML(legend_html), class = "text-center"),
             immediate = TRUE)
  })
  
  # ---- Species ----
  spp_summary <- reactive({
    inat_data %>%
      distinct(taxon_species_name, iconic_taxon_name) %>%
      count(iconic_taxon_name) %>%
      arrange(desc(n)) %>%
      mutate(
        percentage = round(n/sum(n) * 100, 1)
      )
  })
  
  output$spp_plot <- renderPlotly({
    data <- spp_summary()
    total_spp <- sum(data$n)
    
    plot_ly(data, 
            labels = ~iconic_taxon_name, 
            values = ~n,
            type = "pie", 
            hole = 0.6,
            marker = list(colors = taxon_colors[1:nrow(data)],
                          line = list(color = "white", width = 2)),
            textinfo = "none",
            hovertemplate = "<b>%{label}</b><br>Count: %{value} (%{percent})<extra></extra>") %>%
      layout(
        showlegend = FALSE,
        margin = list(t = 20, b = 20, l = 20, r = 20),
        annotations = list(
          list(x = 0.5, y = 0.5, 
               text = paste0("<b>", formatC(total_spp, big.mark = ","), "</b><br>",
                             "<span style='font-size:12px'>Species</span>"),
               showarrow = FALSE,
               font = list(size = 16))
        )
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # Create legend for species
  observe({
    data <- spp_summary()
    legend_html <- create_legend(data$iconic_taxon_name, taxon_colors[1:nrow(data)])
    insertUI(selector = "#spp_legend", 
             ui = div(HTML(legend_html), class = "text-center"),
             immediate = TRUE)
  })
  
  # ---- People ----
  output$ppl_plot <- renderPlotly({
    n_people <- n_distinct(inat_data$user_id)
    
    plot_ly(labels = "Contributors", 
            values = n_people,
            type = "pie", 
            hole = 0.6,
            marker = list(colors = "#17a2b8",
                          line = list(color = "white", width = 2)),
            textinfo = "none",
            hovertemplate = "<b>Contributors</b><br>Count: %{value}<extra></extra>") %>%
      layout(
        showlegend = FALSE,
        margin = list(t = 20, b = 20, l = 20, r = 20),
        annotations = list(
          list(x = 0.5, y = 0.5, 
               text = paste0("<b>", formatC(n_people, big.mark = ","), "</b><br>",
                             "<span style='font-size:12px'>People</span>"),
               showarrow = FALSE,
               font = list(size = 16))
        )
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # Create legend for people
  observe({
    legend_html <- create_legend("Contributors", "#17a2b8")
    insertUI(selector = "#ppl_legend", 
             ui = div(HTML(legend_html), class = "text-center"),
             immediate = TRUE)
  })
  
  ## ---- eBird summaries ----
  ebird_summary <- reactive({
    list(
      n_observations = n_distinct(eBird_data$GLOBAL.UNIQUE.IDENTIFIER),
      n_species = n_distinct(eBird_data$SCIENTIFIC.NAME),
      n_observers = n_distinct(eBird_data$OBSERVER.ID)
    )
  })
  
  # Checklists donut
  output$ebird_observations_plot <- renderPlotly({
    n_observations <- ebird_summary()$n_observations
    plot_ly(labels = "Observations", values = n_observations,
            type = "pie", hole = 0.6,
            marker = list(colors = "#28B463", line = list(color = "white", width = 2)),
            textinfo = "none") %>%
      layout(
        showlegend = FALSE,
        annotations = list(list(x = 0.5, y = 0.5,
                                text = paste0("<b>", formatC(n_observations, big.mark=","), "</b><br>",
                                              "<span style='font-size:12px'>Observations</span>"),
                                showarrow = FALSE,
                                font = list(size = 16)))
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # Species donut
  output$ebird_spp_plot <- renderPlotly({
    n_species <- ebird_summary()$n_species
    plot_ly(labels = "Species", values = n_species,
            type = "pie", hole = 0.6,
            marker = list(colors = "#2E86AB", line = list(color = "white", width = 2)),
            textinfo = "none") %>%
      layout(
        showlegend = FALSE,
        annotations = list(list(x = 0.5, y = 0.5,
                                text = paste0("<b>", formatC(n_species, big.mark=","), "</b><br>",
                                              "<span style='font-size:12px'>Species</span>"),
                                showarrow = FALSE,
                                font = list(size = 16)))
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # Observers donut
  output$ebird_obs_plot <- renderPlotly({
    n_obs <- ebird_summary()$n_observers
    plot_ly(labels = "Observers", values = n_obs,
            type = "pie", hole = 0.6,
            marker = list(colors = "#E67E22", line = list(color = "white", width = 2)),
            textinfo = "none") %>%
      layout(
        showlegend = FALSE,
        annotations = list(list(x = 0.5, y = 0.5,
                                text = paste0("<b>", formatC(n_obs, big.mark=","), "</b><br>",
                                              "<span style='font-size:12px'>Observers</span>"),
                                showarrow = FALSE,
                                font = list(size = 16)))
      ) %>%
      config(displayModeBar = FALSE)
  })
  
}

shinyApp(ui, server) 























#### another option - stats bar for eBird data




#### iNat and ebird data -------------------------------------------------------

library(shiny)
library(bslib)
library(dplyr)
library(plotly)
library(purrr)

# Read & prepare processed data

# iNat data
inat_data <- read.csv("data/raw_data/biodiversity_data/inat_greatmeadow_20250825.csv")

# eBird data
eBird_data <- read.csv("data/raw_data/biodiversity_data/ebird_greatmeadow_20250825.csv")

eBird_tax <- read.csv("data/raw_data/biodiversity_data/eBird_taxonomy_v2024.csv")

# merge eBird data
merged_data <- full_join(
  eBird_data, 
  eBird_tax, 
  by = c("TAXONOMIC.ORDER" = "TAXON_ORDER")
)

#UI
ui <- page_fillable(
  theme = bs_theme(
    bootswatch = "flatly",
    primary = "#2E8B57", 
    base_font = font_google("Open Sans")
  ),
  
  # Page Header
  div(
    class = "text-center mb-4",
    h2("Biodiversity of Great Meadow", class = "display-4 text-primary"),
    p("Summary of participatory science observations", class = "lead text-muted")
  ),
  
  #### iNaturalist Section ####
  div(class = "my-5",
      h3("iNaturalist", class = "text-center text-success mb-4"),
      layout_columns(
        col_widths = c(4,4,4),
        
        div(class = "text-center",
            plotlyOutput("obs_plot", height = "250px"),
            div(id = "obs_legend", class = "mt-3")
        ),
        
        div(class = "text-center",
            plotlyOutput("spp_plot", height = "250px"),
            div(id = "spp_legend", class = "mt-3")
        ),
        
        div(class = "text-center",
            plotlyOutput("ppl_plot", height = "250px"),
            div(id = "ppl_legend", class = "mt-3")
        )
      )
  ),
  
  #### eBird Section ####
  div(class = "my-5",
      h2("eBird", class = "text-center text-primary mb-4"),
      
      # Stats bar
      div(class = "d-flex justify-content-center gap-5",
          
          div(class = "text-center",
              h3(textOutput("ebird_observations"), class = "mb-1 text-primary"),
              div("Observations", class = "text-muted")
          ),
          
          div(class = "text-center",
              h3(textOutput("ebird_species"), class = "mb-1 text-primary"),
              div("Species", class = "text-muted")
          ),
          
          div(class = "text-center",
              h3(textOutput("ebird_observers"), class = "mb-1 text-warning"),
              div("Observers", class = "text-muted")
          )
      )
  ),
  
  # Footer
  hr(),
  div(
    class = "text-center text-muted small mt-3",
    p("Data sourced from iNaturalist and eBird • Last updated: ", Sys.Date())
  )
)

server <- function(input, output, session) {
  
  # Define color palettes
  quality_colors <- c("research" = "#28a745", "needs_id" = "#ffc107", "casual" = "#6c757d")
  taxon_colors <- c("#FF6B6B", "#4ECDC4", "#45B7D1", "#96CEB4", "#FECA57", 
                    "#FF9FF3", "#54A0FF", "#5F27CD", "#00D2D3", "#FF9F43",
                    "#A8E6CF", "#FFB347", "#87CEEB", "#DDA0DD", "#F0E68C")
  
  # Helper function to create legend HTML
  create_legend <- function(categories, colors) {
    legend_items <- map2_chr(categories, colors, function(cat, col) {
      paste0('<span style="display: inline-block; margin-right: 15px; margin-bottom: 5px;">',
             '<span style="display: inline-block; width: 12px; height: 12px; ',
             'background-color: ', col, '; margin-right: 5px; border-radius: 2px;"></span>',
             '<span style="font-size: 12px;">', cat, '</span></span>')
    })
    paste(legend_items, collapse = "")
  }
  
  # ---- Observations ----
  obs_summary <- reactive({
    inat_data %>%
      distinct(uuid, quality_grade) %>%
      count(quality_grade) %>%
      mutate(
        percentage = round(n/sum(n) * 100, 1)
      )
  })
  
  output$obs_plot <- renderPlotly({
    data <- obs_summary()
    total_obs <- sum(data$n)
    
    plot_ly(data, 
            labels = ~quality_grade, 
            values = ~n,
            type = "pie", 
            hole = 0.6,
            marker = list(colors = quality_colors[data$quality_grade],
                          line = list(color = "white", width = 2)),
            textinfo = "none",
            hovertemplate = "<b>%{label}</b><br>Count: %{value} (%{percent})<extra></extra>") %>%
      layout(
        showlegend = FALSE,
        margin = list(t = 20, b = 20, l = 20, r = 20),
        annotations = list(
          list(x = 0.5, y = 0.5, 
               text = paste0("<b>", formatC(total_obs, big.mark = ","), "</b><br>",
                             "<span style='font-size:12px'>Observations</span>"),
               showarrow = FALSE,
               font = list(size = 16))
        )
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # Create legend for observations
  observe({
    data <- obs_summary()
    legend_html <- create_legend(data$quality_grade, quality_colors[data$quality_grade])
    insertUI(selector = "#obs_legend", 
             ui = div(HTML(legend_html), class = "text-center"),
             immediate = TRUE)
  })
  
  # ---- Species ----
  spp_summary <- reactive({
    inat_data %>%
      distinct(taxon_species_name, iconic_taxon_name) %>%
      count(iconic_taxon_name) %>%
      arrange(desc(n)) %>%
      mutate(
        percentage = round(n/sum(n) * 100, 1)
      )
  })
  
  output$spp_plot <- renderPlotly({
    data <- spp_summary()
    total_spp <- sum(data$n)
    
    plot_ly(data, 
            labels = ~iconic_taxon_name, 
            values = ~n,
            type = "pie", 
            hole = 0.6,
            marker = list(colors = taxon_colors[1:nrow(data)],
                          line = list(color = "white", width = 2)),
            textinfo = "none",
            hovertemplate = "<b>%{label}</b><br>Count: %{value} (%{percent})<extra></extra>") %>%
      layout(
        showlegend = FALSE,
        margin = list(t = 20, b = 20, l = 20, r = 20),
        annotations = list(
          list(x = 0.5, y = 0.5, 
               text = paste0("<b>", formatC(total_spp, big.mark = ","), "</b><br>",
                             "<span style='font-size:12px'>Species</span>"),
               showarrow = FALSE,
               font = list(size = 16))
        )
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # Create legend for species
  observe({
    data <- spp_summary()
    legend_html <- create_legend(data$iconic_taxon_name, taxon_colors[1:nrow(data)])
    insertUI(selector = "#spp_legend", 
             ui = div(HTML(legend_html), class = "text-center"),
             immediate = TRUE)
  })
  
  # ---- People ----
  output$ppl_plot <- renderPlotly({
    n_people <- n_distinct(inat_data$user_id)
    
    plot_ly(labels = "Contributors", 
            values = n_people,
            type = "pie", 
            hole = 0.6,
            marker = list(colors = "#17a2b8",
                          line = list(color = "white", width = 2)),
            textinfo = "none",
            hovertemplate = "<b>Contributors</b><br>Count: %{value}<extra></extra>") %>%
      layout(
        showlegend = FALSE,
        margin = list(t = 20, b = 20, l = 20, r = 20),
        annotations = list(
          list(x = 0.5, y = 0.5, 
               text = paste0("<b>", formatC(n_people, big.mark = ","), "</b><br>",
                             "<span style='font-size:12px'>People</span>"),
               showarrow = FALSE,
               font = list(size = 16))
        )
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # Create legend for people
  observe({
    legend_html <- create_legend("Contributors", "#17a2b8")
    insertUI(selector = "#ppl_legend", 
             ui = div(HTML(legend_html), class = "text-center"),
             immediate = TRUE)
  })
  
  ## ---- eBird summaries ----
  ebird_summary <- reactive({
    list(
      n_observations = n_distinct(eBird_data$GLOBAL.UNIQUE.IDENTIFIER),
      n_species = n_distinct(eBird_data$SCIENTIFIC.NAME),
      n_observers = n_distinct(eBird_data$OBSERVER.ID)
    )
  })
  
  output$ebird_observations <- renderText({
    formatC(ebird_summary()$n_observations, big.mark = ",")
  })
  
  output$ebird_species <- renderText({
    formatC(ebird_summary()$n_species, big.mark = ",")
  })
  
  output$ebird_observers <- renderText({
    formatC(ebird_summary()$n_observers, big.mark = ",")
  })
}

shinyApp(ui, server) 













#### iNat and ebird data -------------------------------------------------------

library(shiny)
library(bslib)
library(dplyr)
library(plotly)
library(purrr)

# Read & prepare processed data

# iNat data
inat_data <- read.csv("data/raw_data/biodiversity_data/inat_greatmeadow_20250825.csv")

# eBird data
eBird_data <- read.csv("data/raw_data/biodiversity_data/ebird_greatmeadow_20250825.csv")

eBird_tax <- read.csv("data/raw_data/biodiversity_data/eBird_taxonomy_v2024.csv")

# merge eBird data
merged_data <- full_join(
  eBird_data, 
  eBird_tax, 
  by = c("TAXONOMIC.ORDER" = "TAXON_ORDER")
)

#UI
ui <- page_fillable(
  theme = bs_theme(
    bootswatch = "flatly",
    primary = "#2E8B57", 
    base_font = font_google("Open Sans")
  ),
  
  # Page Header
  div(
    class = "text-center mb-4",
    h2("Biodiversity of Great Meadow", class = "display-4 text-primary"),
    p("Summary of participatory science observations", class = "lead text-muted")
  ),
  
  #### iNaturalist Section ####
  div(class = "my-5",
      h3("iNaturalist", class = "text-center text-success mb-4"),
      
      # Single pie chart for species by taxa
      div(class = "text-center mb-4",
          plotlyOutput("spp_plot", height = "400px"),
          div(id = "spp_legend", class = "mt-3")
      ),
      
      # Stats with icons underneath
      layout_columns(
        col_widths = c(6, 6),
        
        card(
          class = "text-center",
          card_body(
            div(
              icon("binoculars", style = "font-size: 2rem; color: #28a745; margin-bottom: 10px;"),
              h3(textOutput("inat_observations"), class = "mb-1 text-primary"),
              div("Observations", class = "text-muted")
            )
          )
        ),
        
        card(
          class = "text-center", 
          card_body(
            div(
              icon("users", style = "font-size: 2rem; color: #17a2b8; margin-bottom: 10px;"),
              h3(textOutput("inat_people"), class = "mb-1 text-info"),
              div("Contributors", class = "text-muted")
            )
          )
        )
      )
  ),
  
  #### eBird Section ####
  div(class = "my-5",
      h2("eBird", class = "text-center text-primary mb-4"),
      
      # Stats bar
      div(class = "d-flex justify-content-center gap-5",
          
          div(class = "text-center",
              h3(textOutput("ebird_observations"), class = "mb-1 text-primary"),
              div("Observations", class = "text-muted")
          ),
          
          div(class = "text-center",
              h3(textOutput("ebird_species"), class = "mb-1 text-primary"),
              div("Species", class = "text-muted")
          ),
          
          div(class = "text-center",
              h3(textOutput("ebird_observers"), class = "mb-1 text-warning"),
              div("Observers", class = "text-muted")
          )
      )
  ),
  
  # Footer
  hr(),
  div(
    class = "text-center text-muted small mt-3",
    p("Data sourced from iNaturalist and eBird • Last updated: ", Sys.Date())
  )
)

server <- function(input, output, session) {
  
  # Define color palettes
  taxon_colors <- c("#FF6B6B", "#4ECDC4", "#45B7D1", "#96CEB4", "#FECA57", 
                    "#FF9FF3", "#54A0FF", "#5F27CD", "#00D2D3", "#FF9F43",
                    "#A8E6CF", "#FFB347", "#87CEEB", "#DDA0DD", "#F0E68C")
  
  # Helper function to create legend HTML
  create_legend <- function(categories, colors) {
    legend_items <- map2_chr(categories, colors, function(cat, col) {
      paste0('<span style="display: inline-block; margin-right: 15px; margin-bottom: 5px;">',
             '<span style="display: inline-block; width: 12px; height: 12px; ',
             'background-color: ', col, '; margin-right: 5px; border-radius: 2px;"></span>',
             '<span style="font-size: 12px;">', cat, '</span></span>')
    })
    paste(legend_items, collapse = "")
  }
  
  # ---- Species pie chart for iNaturalist ----
  spp_summary <- reactive({
    inat_data %>%
      distinct(taxon_species_name, iconic_taxon_name) %>%
      count(iconic_taxon_name) %>%
      arrange(desc(n)) %>%
      mutate(
        percentage = round(n/sum(n) * 100, 1)
      )
  })
  
  output$spp_plot <- renderPlotly({
    data <- spp_summary()
    total_spp <- sum(data$n)
    
    plot_ly(data, 
            labels = ~iconic_taxon_name, 
            values = ~n,
            type = "pie", 
            hole = 0.4,
            marker = list(colors = taxon_colors[1:nrow(data)],
                          line = list(color = "white", width = 2)),
            textinfo = "none",
            hovertemplate = "<b>%{label}</b><br>Species: %{value} (%{percent})<extra></extra>") %>%
      layout(
        showlegend = FALSE,
        margin = list(t = 40, b = 40, l = 40, r = 40),
        annotations = list(
          list(x = 0.5, y = 0.5, 
               text = paste0("<b>", formatC(total_spp, big.mark = ","), "</b><br>",
                             "<span style='font-size:14px'>Species</span>"),
               showarrow = FALSE,
               font = list(size = 18))
        )
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # Create legend for species
  observe({
    data <- spp_summary()
    legend_html <- create_legend(data$iconic_taxon_name, taxon_colors[1:nrow(data)])
    insertUI(selector = "#spp_legend", 
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
      n_observations = n_distinct(eBird_data$GLOBAL.UNIQUE.IDENTIFIER),
      n_species = n_distinct(eBird_data$SCIENTIFIC.NAME),
      n_observers = n_distinct(eBird_data$OBSERVER.ID)
    )
  })
  
  output$ebird_observations <- renderText({
    formatC(ebird_summary()$n_observations, big.mark = ",")
  })
  
  output$ebird_species <- renderText({
    formatC(ebird_summary()$n_species, big.mark = ",")
  })
  
  output$ebird_observers <- renderText({
    formatC(ebird_summary()$n_observers, big.mark = ",")
  })
}

shinyApp(ui, server)




#-------------------------------------------------------------------------------





#### Biodiversity R shiny app notes ####


#### eBird data ####

eBird_data <- read.csv("data/raw_data/biodiversity_data/ebird_greatmeadow_20250825.csv")

colnames(eBird_data)
unique(eBird_data$OBSERVATION.TYPE)


#### iNat data ####


library(shiny)
library(dplyr)
library(plotly)


# Read & prepare processed data

inat_data <- read.csv("data/raw_data/biodiversity_data/inat_greatmeadow_20250825.csv")


# Example app
ui <- fluidPage(
  fluidRow(
    column(3, plotlyOutput("obs_plot")),
    column(3, plotlyOutput("spp_plot")),
    # column(3, plotlyOutput("id_plot")),
    column(3, plotlyOutput("ppl_plot"))
  )
)

server <- function(input, output, session) {
  
  # ---- Observations ----
  obs_summary <- inat_data %>%
    distinct(uuid, quality_grade) %>%        # unique obs by uuid
    count(quality_grade)
  
  output$obs_plot <- renderPlotly({
    plot_ly(obs_summary, labels = ~quality_grade, values = ~n,
            type = "pie", hole = 0.6) %>%
      layout(title = list(text = paste0(formatC(n_distinct(inat_data$uuid), big.mark = ","),
                                        "<br>OBSERVATIONS"),
                          x = 0.5, y = 0.5))
  })
  
  # ---- Species ----
  spp_summary <- inat_data %>%
    distinct(taxon_species_name, iconic_taxon_name) %>%  # unique species
    count(iconic_taxon_name)
  
  output$spp_plot <- renderPlotly({
    plot_ly(spp_summary, labels = ~iconic_taxon_name, values = ~n,
            type = "pie", hole = 0.6) %>%
      layout(title = list(text = paste0(formatC(n_distinct(inat_data$taxon_species_name), big.mark = ","),
                                        "<br>SPECIES"),
                          x = 0.5, y = 0.5))
  })
  
  # # ---- Identifications ----
  # # Assuming you have a column "ident_type"
  # id_summary <- df %>%
  #   count(ident_type)
  # 
  # output$id_plot <- renderPlotly({
  #   plot_ly(id_summary, labels = ~ident_type, values = ~n,
  #           type = "pie", hole = 0.6) %>%
  #     layout(title = list(text = paste0(formatC(sum(id_summary$n), big.mark = ","),
  #                                       "<br>IDENTIFICATIONS"),
  #                         x = 0.5, y = 0.5))
  # })
  
  # ---- People ----
  # Just total unique users for now
  n_people <- n_distinct(inat_data$user_id)
  
  output$ppl_plot <- renderPlotly({
    plot_ly(labels = "People", values = n_people,
            type = "pie", hole = 0.6) %>%
      layout(title = list(text = paste0(formatC(n_people, big.mark = ","),
                                        "<br>PEOPLE"),
                          x = 0.5, y = 0.5),
             showlegend = FALSE)
  })
}

shinyApp(ui, server)






#### DRAFT 2 -------------------------------------------------------------------



library(shiny)
library(bslib)
library(dplyr)
library(plotly)
library(purrr)

# Read & prepare processed data

inat_data <- read.csv("data/raw_data/biodiversity_data/inat_greatmeadow_20250825.csv")


ui <- page_fillable(
  theme = bs_theme(
    bootswatch = "flatly",
    primary = "#2E8B57", 
    base_font = font_google("Open Sans")
  ),
  
  # Header
  div(
    class = "text-center mb-4",
    h1("iNaturalist Biodiversity Dashboard", 
       class = "display-4 text-primary"),
    p("Summary of observations from Great Meadow", 
      class = "lead text-muted")
  ),
  
  # Main content
  layout_columns(
    col_widths = c(4, 4, 4),
    
    # Observations card
    card(
      card_header(
        icon("binoculars"), " Observations",
        class = "text-center bg-primary text-white"
      ),
      card_body(
        plotlyOutput("obs_plot", height = "250px"),
        div(id = "obs_legend", class = "mt-2"),
        class = "p-2"
      ),
      full_screen = TRUE
    ),
    
    # Species card
    card(
      card_header(
        icon("leaf"), " Species",
        class = "text-center bg-success text-white"
      ),
      card_body(
        plotlyOutput("spp_plot", height = "250px"),
        div(id = "spp_legend", class = "mt-2"),
        class = "p-2"
      ),
      full_screen = TRUE
    ),
    
    # People card
    card(
      card_header(
        icon("users"), " Contributors",
        class = "text-center bg-info text-white"
      ),
      card_body(
        plotlyOutput("ppl_plot", height = "250px"),
        div(id = "ppl_legend", class = "mt-2"),
        class = "p-2"
      ),
      full_screen = TRUE
    )
  ),
  
  # Footer
  hr(),
  div(
    class = "text-center text-muted small mt-3",
    p("Data sourced from iNaturalist • Last updated: ", Sys.Date())
  )
)

server <- function(input, output, session) {
  
  # Define color palettes
  quality_colors <- c("research" = "#28a745", "needs_id" = "#ffc107", "casual" = "#6c757d")
  taxon_colors <- c("#FF6B6B", "#4ECDC4", "#45B7D1", "#96CEB4", "#FECA57", 
                    "#FF9FF3", "#54A0FF", "#5F27CD", "#00D2D3", "#FF9F43")
  
  # Simplified helper function to create legend HTML
  create_legend <- function(categories, colors) {
    legend_items <- map2_chr(categories, colors, function(cat, col) {
      paste0('<span style="display: inline-block; margin-right: 15px; margin-bottom: 5px;">',
             '<span style="display: inline-block; width: 12px; height: 12px; ',
             'background-color: ', col, '; margin-right: 5px; border-radius: 2px;"></span>',
             '<span style="font-size: 12px;">', cat, '</span></span>')
    })
    paste(legend_items, collapse = "")
  }
  
  # ---- Observations ----
  obs_summary <- reactive({
    inat_data %>%
      distinct(uuid, quality_grade) %>%
      count(quality_grade) %>%
      mutate(
        percentage = round(n/sum(n) * 100, 1)
      )
  })
  
  output$obs_plot <- renderPlotly({
    data <- obs_summary()
    total_obs <- sum(data$n)
    
    plot_ly(data, 
            labels = ~quality_grade, 
            values = ~n,
            type = "pie", 
            hole = 0.6,
            marker = list(colors = quality_colors[data$quality_grade],
                          line = list(color = "white", width = 2)),
            textinfo = "none",
            hovertemplate = "<b>%{label}</b><br>Count: %{value} (%{percent})<extra></extra>") %>%
      layout(
        showlegend = FALSE,
        margin = list(t = 20, b = 20, l = 20, r = 20),
        annotations = list(
          list(x = 0.5, y = 0.5, 
               text = paste0("<b>", formatC(total_obs, big.mark = ","), "</b><br>",
                             "<span style='font-size:12px'>Observations</span>"),
               showarrow = FALSE,
               font = list(size = 16))
        )
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # Create legend for observations
  observe({
    data <- obs_summary()
    legend_html <- create_legend(data$quality_grade, quality_colors[data$quality_grade])
    insertUI(selector = "#obs_legend", 
             ui = div(HTML(legend_html), class = "text-center"),
             immediate = TRUE)
  })
  
  # ---- Species ----
  spp_summary <- reactive({
    inat_data %>%
      distinct(taxon_species_name, iconic_taxon_name) %>%
      count(iconic_taxon_name) %>%
      arrange(desc(n)) %>%
      mutate(
        percentage = round(n/sum(n) * 100, 1)
      )
  })
  
  output$spp_plot <- renderPlotly({
    data <- spp_summary()
    total_spp <- sum(data$n)
    
    plot_ly(data, 
            labels = ~iconic_taxon_name, 
            values = ~n,
            type = "pie", 
            hole = 0.6,
            marker = list(colors = taxon_colors[1:nrow(data)],
                          line = list(color = "white", width = 2)),
            textinfo = "none",
            hovertemplate = "<b>%{label}</b><br>Count: %{value} (%{percent})<extra></extra>") %>%
      layout(
        showlegend = FALSE,
        margin = list(t = 20, b = 20, l = 20, r = 20),
        annotations = list(
          list(x = 0.5, y = 0.5, 
               text = paste0("<b>", formatC(total_spp, big.mark = ","), "</b><br>",
                             "<span style='font-size:12px'>Species</span>"),
               showarrow = FALSE,
               font = list(size = 16))
        )
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # Create legend for species
  observe({
    data <- spp_summary()
    legend_html <- create_legend(data$iconic_taxon_name, taxon_colors[1:nrow(data)])
    insertUI(selector = "#spp_legend", 
             ui = div(HTML(legend_html), class = "text-center"),
             immediate = TRUE)
  })
  
  # ---- People ----
  output$ppl_plot <- renderPlotly({
    n_people <- n_distinct(inat_data$user_id)
    
    plot_ly(labels = "Contributors", 
            values = n_people,
            type = "pie", 
            hole = 0.6,
            marker = list(colors = "#17a2b8",
                          line = list(color = "white", width = 2)),
            textinfo = "none",
            hovertemplate = "<b>Contributors</b><br>Count: %{value}<extra></extra>") %>%
      layout(
        showlegend = FALSE,
        margin = list(t = 20, b = 20, l = 20, r = 20),
        annotations = list(
          list(x = 0.5, y = 0.5, 
               text = paste0("<b>", formatC(n_people, big.mark = ","), "</b><br>",
                             "<span style='font-size:12px'>People</span>"),
               showarrow = FALSE,
               font = list(size = 16))
        )
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # Create legend for people (simple single item)
  observe({
    legend_html <- create_legend("Contributors", "#17a2b8")
    insertUI(selector = "#ppl_legend", 
             ui = div(HTML(legend_html), class = "text-center"),
             immediate = TRUE)
  })
}

shinyApp(ui, server)


