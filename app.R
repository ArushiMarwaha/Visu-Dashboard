# ============================================================
# E-COMMERCE CUSTOMER BEHAVIOR — ADVANCED EDA DASHBOARD
# Developed by: Arushi Marwaha 
# Chennai Mathematical Institute | MSc Data Science (MDS202512)
# Enhanced: Interactive Plotly + Dark Mode + Corporate UI
# ============================================================

library(shiny)
library(ggplot2)
library(dplyr)
library(viridis)
library(forcats)
library(maps)
library(scales)
library(reshape2)
library(shinyjs)
library(plotly) 
library(shinyWidgets)

# -----------------------------
# 1. DATA LOAD & PREP
# -----------------------------
# Ensure this matches your filename EXACTLY (case-sensitive on server)
data_file <- "shopping_behavior_updated.csv"

if (file.exists(data_file)) {
  my_data <- read.csv(data_file)
} else {
  # This helps debug the Exit Status 1 error in shinyapps.io logs
  stop("Data file not found. Ensure 'shopping_behavior_updated.csv' is in the same folder as app.R")
}

# Establish global color order based on popularity
global_color_order <- my_data %>%
  count(Color) %>%
  arrange(n) %>%
  pull(Color)

my_data <- my_data %>%
  mutate(
    Season = factor(Season, levels = c("Spring", "Summer", "Fall", "Winter")),
    Promo.Code.Used = as.factor(Promo.Code.Used),
    Location_Lower = tolower(trimws(Location)),
    Gender = as.factor(Gender),
    Color = factor(Color, levels = global_color_order) 
  )

# -----------------------------
# 2. UI SECTION
# -----------------------------
ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(HTML("
      /* CSS Variables for Corporate Theming */
      :root { --accent: #004085; --bg: #f8f9fa; --card-bg: #ffffff; --text: #212529; --border: #dee2e6; }
      body.dark-mode { --accent: #4dabff; --bg: #121212; --card-bg: #1e1e1e; --text: #e0e0e0; --border: #333333; }
      
      body { background-color: var(--bg); color: var(--text); font-family: 'Inter', sans-serif; transition: 0.3s ease; }
      .container-fluid { max-width: 1600px; padding: 0 40px; }

      .well { background-color: transparent !important; border: none !important; box-shadow: none !important; padding: 10px !important; }

      .main-title-container { 
        display: flex; justify-content: space-between; align-items: center;
        background: var(--card-bg); padding: 20px; border-bottom: 3px solid var(--accent); margin-bottom: 30px;
        box-shadow: 0 4px 6px rgba(0,0,0,0.05);
      }
      
      .nav-tabs > li > a { color: var(--text); font-weight: 600; padding: 15px 5px; border: none !important; }
      .nav-tabs > li.active > a { color: var(--accent) !important; border-bottom: 3px solid var(--accent) !important; background: transparent !important; }

      .value-box { 
        background: var(--card-bg); border: 1px solid var(--border); border-radius: 8px; 
        padding: 20px; text-align: center; height: 120px;
        display: flex; flex-direction: column; justify-content: center;
        transition: transform 0.2s;
      }
      .box-title { font-size: 0.75rem; font-weight: 700; color: #6c757d; text-transform: uppercase; letter-spacing: 0.5px; }
      .box-value { font-size: 1.8rem; font-weight: 800; color: var(--accent); }
      
      .content-card { 
        background: var(--card-bg); padding: 30px; border: 1px solid var(--border); 
        border-radius: 8px; margin-bottom: 40px; box-shadow: 0 2px 4px rgba(0,0,0,0.02);
      }
      
      .conclusion-box {
        background-color: rgba(0, 64, 133, 0.07); border-left: 5px solid var(--accent);
        padding: 15px; margin-bottom: 25px; font-size: 0.95em; color: var(--text);
      }

      .btn-reset { 
        background-color: #343a40; color: white; border: none; padding: 12px;
        font-weight: 600; width: 100%; border-radius: 6px; margin-top: 15px; transition: 0.3s;
      }
      .btn-reset:hover { background-color: #23272b; }

      .linkedin-card {
        display: inline-block; padding: 12px 25px; background: #0077b5; color: white;
        border-radius: 50px; text-decoration: none; font-weight: 700; transition: 0.3s;
      }
      
      .section-header { color: var(--accent); font-weight: 700; border-bottom: 2px solid var(--border); padding-bottom: 5px; margin-top: 25px;}
    "))
  ),
  
  div(class = "container-fluid",
      div(class = "main-title-container", 
          h1("Customer Behavior Dashboard", style="margin:0; font-weight: 800;"),
          materialSwitch(inputId = "dark_mode", label = icon("moon"), status = "primary", right = TRUE)
      ),
      
      sidebarLayout(
        sidebarPanel(
          width = 3,
          div(class="developer-info",
              tags$b("Arushi Marwaha", style="font-size: 1.2em;"), br(),
              "MSc Data Science | MDS202512", br(),
              tags$span(style="color: var(--accent);", "Chennai Mathematical Institute")
          ),
          hr(),
          h4("Global Dashboard Controls", style="font-weight:bold;"),
          selectInput("global_season", "Filter by Season:", choices = c("All", levels(my_data$Season))),
          selectInput("global_gender", "Filter by Gender:", choices = c("All", levels(my_data$Gender))),
          actionButton("reset_all", "Reset Graph Views", class = "btn-reset", icon = icon("sync-alt")),
          hr(),
          div(style="font-style: italic; opacity: 0.8; font-size: 0.85em;",
              tags$b("Note:"), "All charts are interactive. Use your mouse to zoom, hover, and pan.")
        ),
        
        mainPanel(
          width = 9,
          fluidRow(
            column(4, div(class = "value-box", div(class = "box-title", "Total Spend (USD)"), uiOutput("vbox_rev"))),
            column(4, div(class = "value-box", div(class = "box-title", "Mean Satisfaction"), uiOutput("vbox_rat"))),
            column(4, div(class = "value-box", div(class = "box-title", "Top Performing Category"), uiOutput("vbox_cat")))
          ),
          br(),
          tabsetPanel(
            id = "main_tabs",
            
            tabPanel(title = span(icon("info-circle"), "Project & Data Overview"), 
                     div(class = "content-card",
                         h2("Project Overview", style="margin-top:0; color: var(--accent);"),
                         fluidRow(
                           column(7,
                                  h4(class="section-header", "Abstract"),
                                  p("This project presents an analysis on consumer behaviour and shopping patterns. By analyzing demographics, transaction history, and customer feedback, we uncover key drivers influencing spending decisions to guide marketing efforts."),
                                  h4(class="section-header", "How to Navigate the Dashboard"),
                                  tags$ul(style="padding-left: 15px;",
                                          tags$li(tags$b("Interactive Charts:"), " Hover over points to see details. Drag to zoom in."),
                                          tags$li(tags$b("Customer Profile:"), " Gender and Age breakdowns."),
                                          tags$li(tags$b("Trends & Sales:"), " Deep dives into Seasonal and Geographic performance.")
                                  )
                           ),
                           column(5,
                                  h4(class="section-header", "Dataset Specifications"),
                                  p("The dataset includes", tags$b("3,900 records"), "with", tags$b("18 features"), "."),
                                  div(style = "overflow-x: auto;", tableOutput("raw_sample_table"))
                           )
                         )
                     )),
            
            tabPanel(title = span(icon("user-friends"), "Customer Profile"),
                     div(class = "content-card",
                         h3("Demographic Analysis & Insights"),
                         div(class="conclusion-box", uiOutput("demo_conclusion")),
                         fluidRow(
                           column(6, plotlyOutput("gender_bar")),
                           column(6, plotlyOutput("age_pyramid"))
                         ),
                         hr(),
                         h3("Interactive Relationship Analysis"),
                         fluidRow(
                           column(6, selectInput("x_var", "Select Feature (X):", choices = c("Age", "Review.Rating", "Previous.Purchases"))),
                           column(6, selectInput("color_var", "Segment Points By:", choices = c("Gender", "Promo.Code.Used", "Season")))
                         ),
                         plotlyOutput("dynamic_scatter", height = "450px"),
                         wellPanel(uiOutput("corr_info"), style="background: var(--bg); border: 1px solid var(--border);")
                     )),
            
            tabPanel(title = span(icon("chart-line"), "Market Trends"),
                     div(class = "content-card",
                         h3("Seasonal Trends & Inventory Insights"),
                         div(class="conclusion-box", uiOutput("season_conclusion")),
                         plotlyOutput("color_heatmap", height = "500px"),
                         hr(),
                         h3("Promo Sensitivity Analysis"),
                         div(class="conclusion-box", uiOutput("promo_conclusion")),
                         plotlyOutput("promo_boxplot", height = "450px")
                     )),
            
            tabPanel(title = span(icon("globe-americas"), "Logistics & Geography"),
                     div(class = "content-card",
                         h3("Regional Sales Efficiency"),
                         div(class="conclusion-box", uiOutput("map_conclusion")),
                         plotlyOutput("us_map_refined", height = "600px"),
                         hr(),
                         h3("Shipping & Payment Distribution"),
                         div(class="conclusion-box", uiOutput("shipping_conclusion")),
                         fluidRow(
                           column(6, plotlyOutput("shipping_plot")),
                           column(6, plotlyOutput("payment_plot"))
                         )
                     )),
            
            tabPanel(title = span(icon("id-card"), "About Me"),
                     div(class = "content-card",
                         h2("Arushi Marwaha", style="margin-top:0; font-weight:700; color:var(--accent);"),
                         p(tags$b("MSc DS, CMI"), style="font-size:1.2em;"),
                         p("Email: arushim.mds2025@cmi.ac.in"),
                         br(),
                         a(href="https://www.linkedin.com/in/arushimarwaha/", target="_blank", class="linkedin-card",
                           icon("linkedin"), " Professional Profile")
                     ))
          )
        )
      )
  )
)

# -----------------------------
# 3. SERVER SECTION
# -----------------------------
server <- function(input, output, session) {
  
  observe({
    if(input$dark_mode) {
      runjs("document.body.classList.add('dark-mode');")
    } else {
      runjs("document.body.classList.remove('dark-mode');")
    }
  })
  
  filtered_df <- reactive({
    df <- my_data
    if (input$global_season != "All") df <- df %>% filter(Season == input$global_season)
    if (input$global_gender != "All") df <- df %>% filter(Gender == input$global_gender)
    return(df)
  })
  
  observeEvent(input$reset_all, {
    updateSelectInput(session, "global_season", selected = "All")
    updateSelectInput(session, "global_gender", selected = "All")
    updateSelectInput(session, "x_var", selected = "Age")
    updateSelectInput(session, "color_var", selected = "Gender")
  })
  
  output$vbox_rev <- renderUI({ div(class = "box-value", dollar(sum(filtered_df()$Purchase.Amount..USD., na.rm = TRUE))) })
  output$vbox_rat <- renderUI({ div(class = "box-value", round(mean(filtered_df()$Review.Rating, na.rm = TRUE), 2)) })
  output$vbox_cat <- renderUI({
    top_cat <- filtered_df() %>% count(Category) %>% arrange(desc(n)) %>% slice(1) %>% pull(Category)
    div(class = "box-value", style="font-size:1.3em;", ifelse(length(top_cat)>0, as.character(top_cat), "N/A"))
  })
  
  output$demo_conclusion <- renderUI({
    df <- filtered_df()
    if (nrow(df) == 0) return("No data available.")
    m_pct <- round(sum(df$Gender == "Male") / nrow(df) * 100, 1)
    f_pct <- round(sum(df$Gender == "Female") / nrow(df) * 100, 1)
    tags$span(tags$b("Profile Insight:"), sprintf(" The segment is %s%% Female and %s%% Male.", f_pct, m_pct))
  })
  
  output$season_conclusion <- renderUI({
    top_color <- filtered_df() %>% count(Color) %>% arrange(desc(n)) %>% slice(1) %>% pull(Color)
    tags$span(tags$b("Seasonal Insight:"), sprintf(" '%s' is the top color preference.", top_color))
  })
  
  output$promo_conclusion <- renderUI({
    p_val <- t.test(Purchase.Amount..USD. ~ Promo.Code.Used, data = filtered_df())$p.value
    tags$span(tags$b("Statistical Insight:"), sprintf(" Promo impact is %s (p = %.4f).", ifelse(p_val < 0.05, "significant", "not significant"), p_val))
  })
  
  output$map_conclusion <- renderUI({
    top_state <- filtered_df() %>% group_by(Location) %>% summarise(m = mean(Purchase.Amount..USD.)) %>% arrange(desc(m)) %>% slice(1)
    tags$span(tags$b("Regional Insight:"), sprintf(" %s leads with an avg spend of %s.", top_state$Location, dollar(top_state$m)))
  })
  
  output$shipping_conclusion <- renderUI({
    top_ship <- filtered_df() %>% count(Shipping.Type) %>% arrange(desc(n)) %>% slice(1) %>% pull(Shipping.Type)
    tags$span(tags$b("Logistic Insight:"), sprintf(" %s is the most used shipping method.", top_ship))
  })
  
  # --- PLOTS WITH CORPORATE COLORS ---
  
  output$gender_bar <- renderPlotly({
    p <- filtered_df() %>% count(Gender) %>%
      ggplot(aes(Gender, n, fill = Gender)) + geom_col(width=0.6) +
      scale_fill_manual(values = c("Female" = "#0056b3", "Male" = "#002752")) + 
      theme_minimal() + labs(title = "Gender Distribution", y = "Count")
    ggplotly(p)
  })
  
  output$age_pyramid <- renderPlotly({
    p <- filtered_df() %>% count(Age, Gender) %>% mutate(n = ifelse(Gender == "Male", -n, n)) %>%
      ggplot(aes(Age, n, fill = Gender)) + geom_col() + coord_flip() +
      scale_fill_manual(values = c("Male" = "#002752", "Female" = "#0056b3")) +
      theme_minimal() + labs(title = "Age Distribution Pyramid", y = "Count")
    ggplotly(p)
  })
  
  output$color_heatmap <- renderPlotly({
    p <- filtered_df() %>% count(Category, Color) %>%
      ggplot(aes(x = Category, y = Color, fill = n)) + geom_tile(color = "white") +
      scale_fill_gradient(low = "#e7f1ff", high = "#004085") + 
      theme_minimal() + labs(title = "Color Popularity by Category")
    ggplotly(p)
  })
  
  output$promo_boxplot <- renderPlotly({
    p <- ggplot(filtered_df(), aes(x = Promo.Code.Used, y = Purchase.Amount..USD., fill = Promo.Code.Used)) +
      geom_boxplot(outlier.color = "#004085") + 
      scale_fill_manual(values = c("Yes" = "#28a745", "No" = "#6c757d")) + 
      theme_minimal() + labs(title = "Promo vs Spending (USD)")
    ggplotly(p)
  })
  
  output$us_map_refined <- renderPlotly({
    states_map <- map_data("state")
    region_df <- filtered_df() %>% group_by(Location_Lower) %>% summarise(Avg_Spend = mean(Purchase.Amount..USD.))
    map_df <- left_join(states_map, region_df, by = c("region" = "Location_Lower"))
    p <- ggplot(map_df, aes(long, lat, group = group, fill = Avg_Spend)) +
      geom_polygon(color = "white", size = 0.1) + 
      scale_fill_gradient(low = "#d0e2ff", high = "#002d62") + 
      theme_void() + labs(title = "Avg Spend per State")
    ggplotly(p) %>% layout(xaxis = list(visible = FALSE), yaxis = list(visible = FALSE))
  })
  
  output$shipping_plot <- renderPlotly({
    p <- ggplot(filtered_df(), aes(x = Category, fill = Shipping.Type)) + geom_bar(position = "fill") + 
      scale_fill_manual(values = c("#002d62", "#004085", "#0056b3", "#0069d9", "#007bff", "#4dabff")) +
      theme_minimal() + theme(axis.text.x = element_text(angle=45))
    ggplotly(p)
  })
  
  output$payment_plot <- renderPlotly({
    p <- ggplot(filtered_df(), aes(x = Payment.Method, y = Purchase.Amount..USD., fill = Payment.Method)) + 
      geom_boxplot() + 
      scale_fill_manual(values = c("#002d62", "#004085", "#0056b3", "#0069d9", "#007bff", "#4dabff")) +
      theme_minimal() + guides(fill="none")
    ggplotly(p)
  })
  
  output$dynamic_scatter <- renderPlotly({
    p <- ggplot(filtered_df(), aes_string(x = input$x_var, y = "Purchase.Amount..USD.", color = input$color_var)) +
      geom_point(alpha = 0.7, size = 2) + 
      geom_smooth(method = "lm", color = "#333333", se = FALSE, linetype = "dashed") + 
      scale_color_manual(values = c("#004085", "#28a745", "#dc3545", "#ffc107", "#17a2b8")) +
      theme_minimal() + labs(title = "Statistical Correlation Analysis")
    ggplotly(p)
  })
  
  output$corr_info <- renderUI({
    val <- cor(filtered_df()[[input$x_var]], filtered_df()$Purchase.Amount..USD., use = "complete.obs")
    h4(paste("Pearson Correlation:", round(val, 3)), style="color: var(--accent);")
  })
  
  output$raw_sample_table <- renderTable({ head(filtered_df() %>% select(Age, Gender, Location, Category, Purchase.Amount..USD.), 6) }, striped=T, hover=T)
}

shinyApp(ui, server)