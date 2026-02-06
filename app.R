# ============================================================
# E-COMMERCE CUSTOMER BEHAVIOR — ADVANCED EDA DASHBOARD
# Developed by: Arushi Marwaha 
# Chennai Mathematical Institute | MSc Data Science (MDS202512)
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
data_file <- "shopping_behavior_updated.csv"

if (file.exists(data_file)) {
  my_data <- read.csv(data_file)
} else {
  stop("Data file not found. Ensure 'shopping_behavior_updated.csv' is in the same folder as app.R")
}

global_color_order <- my_data %>%
  count(Color) %>%
  arrange(n) %>%
  pull(Color)

size_levels <- c("S", "M", "L", "XL")

my_data <- my_data %>%
  mutate(
    Season = factor(Season, levels = c("Spring", "Summer", "Fall", "Winter")),
    Promo.Code.Used = as.factor(Promo.Code.Used),
    Location_Lower = tolower(trimws(Location)),
    Gender = as.factor(Gender),
    Color = factor(Color, levels = global_color_order),
    Size = factor(Size, levels = size_levels)
  )

# -----------------------------
# 2. UI SECTION
# -----------------------------
ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(HTML("
      :root { --accent: #004085; --bg: #f8f9fa; --card-bg: #ffffff; --text: #212529; --border: #dee2e6; --plotly-text: #212529; }
      body.dark-mode { --accent: #4dabff; --bg: #0f111a; --card-bg: #1a1d27; --text: #f0f0f0; --border: #2e323d; --plotly-text: #ffffff; }
      
      body { background-color: var(--bg); color: var(--text); font-family: 'Inter', sans-serif; transition: 0.3s ease; scroll-behavior: smooth; }
      .container-fluid { max-width: 1600px; padding: 0 40px; }
      .well { background-color: transparent !important; border: none !important; box-shadow: none !important; padding: 10px !important; }
      .main-title-container { 
        display: flex; justify-content: space-between; align-items: center;
        background: var(--card-bg); padding: 20px; border-bottom: 3px solid var(--accent); margin-bottom: 30px;
        box-shadow: 0 4px 6px rgba(0,0,0,0.05);
      }
      .nav-tabs > li > a { color: var(--text); font-weight: 600; padding: 15px 12px; border: none !important; }
      .nav-tabs > li.active > a { color: var(--accent) !important; border-bottom: 3px solid var(--accent) !important; background: transparent !important; }
      .value-box { 
        background: var(--card-bg); border: 1px solid var(--border); border-radius: 8px; 
        padding: 20px; text-align: center; height: 120px;
        display: flex; flex-direction: column; justify-content: center;
        transition: transform 0.2s;
      }
      .box-title { font-size: 0.95rem; font-weight: 700; color: #6c757d; text-transform: uppercase; letter-spacing: 0.5px; }
      .box-value { font-size: 2.8rem; font-weight: 800; color: var(--accent); white-space: nowrap; overflow: hidden; text-overflow: ellipsis;}
      .content-card { 
        background: var(--card-bg); padding: 30px; border: 1px solid var(--border); 
        border-radius: 8px; margin-bottom: 40px; box-shadow: 0 2px 4px rgba(0,0,0,0.02);
      }
      .conclusion-box {
        background-color: rgba(77, 171, 255, 0.1); border-left: 5px solid var(--accent);
        padding: 15px; margin-bottom: 25px; font-size: 0.95em; color: var(--text);
      }
      .btn-reset { 
        background-color: #343a40; color: white; border: none; padding: 12px;
        font-weight: 600; width: 100%; border-radius: 6px; margin-top: 15px; transition: 0.3s;
      }
      .btn-reset:hover { background-color: #23272b; }
      .section-header { color: var(--accent); font-weight: 700; border-bottom: 2px solid var(--border); padding-bottom: 5px; margin-top: 25px;}
      
      .table-wrapper { overflow-x: auto; width: 100%; border: 1px solid var(--border); border-radius: 4px; }
      table.table { white-space: nowrap; }
      body.dark-mode table.table { color: #e0e0e0; }

      /* Floating Scroll Buttons */
      .scroll-container {
        position: fixed; bottom: 30px; right: 30px; z-index: 9999;
        display: flex; flex-direction: column; gap: 12px;
      }
      .scroll-circle {
        background: var(--accent); color: white; border: none; width: 50px; height: 50px;
        border-radius: 50%; cursor: pointer; display: flex; align-items: center; justify-content: center;
        box-shadow: 0 4px 12px rgba(0,0,0,0.2); transition: 0.3s cubic-bezier(0.175, 0.885, 0.32, 1.275);
      }
      .scroll-circle:hover { transform: scale(1.15); filter: brightness(1.1); }
      .scroll-circle:active { transform: scale(0.95); }
    ")),
    tags$script(HTML("
      function goUp() { window.scrollTo({top: 0, behavior: 'smooth'}); }
      function goDown() { window.scrollTo({top: document.body.scrollHeight, behavior: 'smooth'}); }
    "))
  ),
  
  # Scroll UI Element
  div(class = "scroll-container",
      tags$button(class = "scroll-circle", onclick = "goUp()", icon("caret-up")),
      tags$button(class = "scroll-circle", onclick = "goDown()", icon("caret-down"))
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
          h4("Interactivity Guide", style="font-weight:bold;"),
          tags$ul(style="padding-left: 15px; font-size: 0.85em; opacity: 0.9;",
                  tags$li("Graphs react instantly to filters."),
                  tags$li("Hover over bars/points for exact values."),
                  tags$li("Drag to zoom; double-click to reset view."),
                  tags$li("Use the floating buttons (bottom-right) for quick up/down scrolling.")
          )
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
                                  h4(class="section-header", icon("file-alt"), " Abstract"),
                                  p("This dashboard provides a comprehensive analytical deep-dive into consumer behavior and transaction patterns. By leveraging demographic data, purchasing history, and customer feedback."),
                                  
                                  h4(class="section-header", icon("directions"), " How to Navigate the Dashboard"),
                                  tags$ul(style="padding-left: 15px; line-height: 1.8;",
                                          tags$li(tags$b("Interactive Charts:"), "Hover over data points for exact values, use the 'drag-to-zoom' feature to focus on specific segments, and double-click to reset the view."),
                                          tags$li(tags$b("Customer Profile:"), " Navigate here for gender and age distribution pyramids, size preferences, and customer loyalty density."),
                                          tags$li(tags$b("Market Trends:"), " Visit this tab to analyze seasonal color popularity via heatmaps and to evaluate promo code effectiveness."),
                                          tags$li(tags$b("Logistics & Geography:"), " Access this section for geospatial insights into regional spend and logistics distribution.")
                                  )
                           ),
                           column(5,
                                   h4(class="section-header", icon("database"), " Dataset Specifications"),
                                   p("The dataset includes", tags$b("3,900 records"), "with", tags$b("18 features"), "."),
                                   div(class = "table-wrapper", tableOutput("raw_sample_table"))
                           )
                         )
                     )),
            
            tabPanel(title = span(icon("user-friends"), "Customer Profile"),
                     div(class = "content-card",
                         h2("Demographic & Behavioral Analytics",style="margin-top:0; color: var(--accent);"),
                         div(class="conclusion-box", uiOutput("demo_conclusion")),
                         fluidRow(
                           column(6, plotlyOutput("gender_bar")),
                           column(6, plotlyOutput("age_pyramid"))
                         ),
                         br(),
                         fluidRow(
                           column(6, plotlyOutput("size_bar")),
                           column(6, plotlyOutput("history_density"))
                         ),
                         hr(),
                         h2("Relationship Analysis",style="margin-top:0; color: var(--accent);"),
                         fluidRow(
                           column(6, selectInput("x_var", "Select Feature (X):", choices = c("Age", "Review.Rating", "Previous.Purchases"))),
                           column(6, selectInput("color_var", "Segment Points By:", choices = c("Gender", "Promo.Code.Used", "Season")))
                         ),
                         plotlyOutput("dynamic_scatter", height = "450px"),
                         wellPanel(uiOutput("corr_info"), style="background: var(--bg); border: 1px solid var(--border);")
                     )),
            
            tabPanel(title = span(icon("chart-line"), "Market Trends"),
                     div(class = "content-card",
                         h2("Seasonal Trends & Inventory Insights",style="margin-top:0; color: var(--accent);"),
                         div(class="conclusion-box", uiOutput("season_conclusion")),
                         plotlyOutput("color_heatmap", height = "500px"),
                         br(),
                         div(class="conclusion-box", uiOutput("revenue_conclusion")),
                         plotlyOutput("revenue_donut", height = "450px"),
                         hr(),
                         h2("Promo Sensitivity Analysis",style="margin-top:0; color: var(--accent);"),
                         div(class="conclusion-box", uiOutput("promo_conclusion")),
                         plotlyOutput("promo_boxplot", height = "450px")
                     )),
            
            tabPanel(title = span(icon("globe-americas"), "Logistics & Geography"),
                     div(class = "content-card",
                         h2("Regional Sales Efficiency",style="margin-top:0; color: var(--accent);"),
                         div(class="conclusion-box", uiOutput("map_conclusion")),
                         plotlyOutput("us_map_refined", height = "600px"),
                         hr(),
                         h2("Shipping & Payment Distribution",style="margin-top:0; color: var(--accent);"),
                         div(class="conclusion-box", uiOutput("shipping_conclusion")),
                         fluidRow(
                           column(6, plotlyOutput("shipping_plot")),
                           column(6, plotlyOutput("payment_plot"))
                         )
                     )),
            
            tabPanel(title = span(icon("file-alt"), "Summary"),
                     div(class = "content-card",
                         h2("Final Conclusions", style="margin-top:0; color: var(--accent);"),
                         p(style="font-style: italic; opacity: 0.7;", "Note: These summaries are dynamically generated for the current selected global parameters."),
                         hr(),
                         fluidRow(
                           column(6, 
                                  h4(class="section-header", "Core Revenue Drivers",style="margin-top:0; color: var(--accent);"),
                                  uiOutput("summary_revenue_text")
                           ),
                           column(6, 
                                  h4(class="section-header", "Customer Behavior & Logistics",style="margin-top:0; color: var(--accent);"),
                                  uiOutput("summary_behavior_text")
                           )
                         )
                     )
            ),
            
            tabPanel(title = span(icon("user-tie"), "About Me"),
                     div(class = "content-card", style="max-width: 1000px; margin: 0 auto; padding: 40px 40px;",
                         div(style="text-align: center; margin-bottom: 40px;",
                             h2("Arushi Marwaha", style="font-weight: 800; font-size: 2.8rem; margin-bottom: 5px;"),
                             p(tags$b("MSc Data Science Student"), " | Chennai Mathematical Institute", 
                               style="font-size: 1.2rem; color: var(--accent);"),
                             div(style="display: flex; justify-content: center; gap: 15px; margin-top: 15px;",
                                 a(href="https://www.linkedin.com/in/arushimarwaha/", target="_blank", class="btn-about",
                                   style="background: #0077b5; color: white; padding: 10px 25px; border-radius: 50px; text-decoration: none; font-weight: 600;",
                                   icon("linkedin"), " LinkedIn"),
                                 a(href="mailto:arushim.mds2025@cmi.ac.in", class="btn-about",
                                   style="background: var(--text); color: var(--card-bg); padding: 10px 25px; border-radius: 50px; text-decoration: none; font-weight: 600; border: 1px solid var(--border);",
                                   icon("envelope"), " Contact Email")
                             )
                         ),
                         fluidRow(
                           column(7,
                                  h4(class="section-header", icon("user"), " Professional Summary"),
                                  p("I am a Data Science graduate student at CMI with a strong academic foundation in mathematics from IISER and research experience at ISI. My journey has given me exposure to both theoretical rigor and applied problem-solving in data-driven domains.", style="line-height: 1.6;"),
                                  h4(class="section-header", icon("briefcase"), " Key Experience"),
                                  div(class="conclusion-box", style="background: transparent; border-left: 3px solid var(--accent); padding-left: 15px;",
                                      tags$b("Turing (Unicorn Startup)"), br(),
                                      "Contributed to multimodal AI projects, enhancing large-scale model capabilities.",
                                      br(), br(),
                                      tags$b("ISI Research Internship"), br(),
                                      "Focused on intensive reading projects that deepened theoretical understanding of matrices."
                                  )
                           ),
                           column(5,
                                  h4(class="section-header", icon("tools"), " Technical Stack"),
                                  div(style="display: flex; flex-wrap: wrap; gap: 8px; margin-bottom: 20px;",
                                      span(style="background: var(--accent); color: white; padding: 5px 12px; border-radius: 4px; font-size: 0.85em;", "Python"),
                                      span(style="background: var(--accent); color: white; padding: 5px 12px; border-radius: 4px; font-size: 0.85em;", "R & Shiny"),
                                      span(style="background: var(--accent); color: white; padding: 5px 12px; border-radius: 4px; font-size: 0.85em;", "SQL"),
                                      span(style="background: var(--accent); color: white; padding: 5px 12px; border-radius: 4px; font-size: 0.85em;", "MATLAB")
                                  ),
                                  h4(class="section-header", icon("lightbulb"), " Core Strengths"),
                                  tags$ul(style="padding-left: 15px; font-size: 0.95em; line-height: 1.8;",
                                          tags$li("Analytical & Mathematical Thinking"),
                                          tags$li("Data-driven Problem Solving"),
                                          tags$li("Attention to Detail"),
                                          tags$li("Clear Insight Communication")
                                  )
                           )
                         ),
                         hr(style="margin: 30px 0; border-top: 1px solid var(--border);"),
                         div(style="text-align: center; opacity: 0.8;",
                             h4("Looking Ahead", style="font-weight: 700;"),
                             p("I enjoy working at the intersection of data, technology, and problem-solving. My goal is to apply my skills to real-world challenges in machine learning and applied mathematics.",
                               style="max-width: 700px; margin: 0 auto 20px;"),
                             p(tags$b("Queries or Collaboration?"), " Reach out at ", 
                               tags$span(style="color: var(--accent); font-weight: 700;", "arushim.mds2025@cmi.ac.in"))
                         )
                     )
            )
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
  
  # HELPER FUNCTION FOR UNIFORM PLOTLY DARK THEME
  apply_dark_theme <- function(p) {
    txt_color <- if(input$dark_mode) "#ffffff" else "#212529"
    grid_color <- if(input$dark_mode) "#3c4048" else "#dee2e6"
    
    p %>% layout(
      font = list(color = txt_color, family = "Inter, sans-serif"),
      plot_bgcolor = "rgba(0,0,0,0)",
      paper_bgcolor = "rgba(0,0,0,0)",
      xaxis = list(
        gridcolor = grid_color,
        zerolinecolor = grid_color,
        tickfont = list(color = txt_color, size = 11),
        title = list(font = list(color = txt_color, size = 13, face = "bold")),
        linecolor = grid_color,
        tickangle = 45  
      ),
      yaxis = list(
        gridcolor = grid_color,
        zerolinecolor = grid_color,
        tickfont = list(color = txt_color, size = 11),
        title = list(font = list(color = txt_color, size = 13, face = "bold")),
        linecolor = grid_color
      ),
      legend = list(
        font = list(color = txt_color, size = 12),
        title = list(font = list(color = txt_color, size = 13, face = "bold")),
        bgcolor = "rgba(0,0,0,0)",
        bordercolor = "rgba(0,0,0,0)",
        itemsizing = "constant"
      ),
      title = list(
        font = list(color = txt_color, size = 16, face = "bold"),
        x = 0.05
      ),
      margin = list(t = 60, b = 80, l = 60, r = 60)
    )
    
  }
  
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
    div(class = "box-value", ifelse(length(top_cat)>0, as.character(top_cat), "N/A"))
  })
  
  # --- SUMMARY TAB ---
  output$summary_revenue_text <- renderUI({
    df <- filtered_df()
    if (nrow(df) == 0) return("Insufficient data.")
    best_cat <- df %>% count(Category) %>% arrange(desc(n)) %>% slice(1) %>% pull(Category)
    avg_spend <- mean(df$Purchase.Amount..USD., na.rm = TRUE)
    total_rev <- sum(df$Purchase.Amount..USD., na.rm = TRUE)
    top_size <- df %>% count(Size) %>% arrange(desc(n)) %>% slice(1) %>% pull(Size)
    tags$ul(
      tags$li(tags$b("Primary Category:"), sprintf(" %s is the most frequently purchased category.", best_cat)),
      tags$li(tags$b("Average Transaction Value (ATV):"), sprintf(" The average customer spends %s per order.", dollar(avg_spend))),
      tags$li(tags$b("Total Market Value:"), sprintf(" The current selection represents a total revenue of %s.", dollar(total_rev))),
      tags$li(tags$b("Sizing Trend:"), sprintf(" The most demanded size is %s.", top_size))
    )
  })
  
  output$summary_behavior_text <- renderUI({
    df <- filtered_df()
    if (nrow(df) == 0) return("Insufficient data.")
    top_ship <- df %>% count(Shipping.Type) %>% arrange(desc(n)) %>% slice(1) %>% pull(Shipping.Type)
    top_pay <- df %>% count(Payment.Method) %>% arrange(desc(n)) %>% slice(1) %>% pull(Payment.Method)
    promo_usage <- round(sum(df$Promo.Code.Used == "Yes") / nrow(df) * 100, 0)
    avg_prev_purch <- round(mean(df$Previous.Purchases, na.rm = TRUE), 1)
    tags$ul(
      tags$li(tags$b("Logistics Preference:"), sprintf(" Customers show a strong preference for %s shipping.", top_ship)),
      tags$li(tags$b("Payment Channels:"), sprintf(" %s is the leading payment method.", top_pay)),
      tags$li(tags$b("Promotion Sensitivity:"), sprintf(" %s%% of transactions involved a promo code.", promo_usage)),
      tags$li(tags$b("Customer Loyalty:"), sprintf(" Customers have an average of %s previous purchases.", avg_prev_purch))
    )
  })
  
  # --- CONCLUSIONS ---
  output$demo_conclusion <- renderUI({
    df <- filtered_df(); if (nrow(df) == 0) return("No data.")
    m_pct <- round(sum(df$Gender == "Male") / nrow(df) * 100, 1); f_pct <- round(sum(df$Gender == "Female") / nrow(df) * 100, 1)
    top_size <- df %>% count(Size) %>% arrange(desc(n)) %>% slice(1) %>% pull(Size)
    avg_hist <- round(mean(df$Previous.Purchases, na.rm = TRUE), 1)
    tags$span(tags$b("Profile Insight:"), sprintf(" Segment: %s%% Female and %s%% Male. ", f_pct, m_pct), tags$b("| Preferred Size:"), sprintf(" %s ", top_size), tags$b("| Retention:"), sprintf(" Average %s previous purchases.", avg_hist))  })
  
  output$season_conclusion <- renderUI({
    if(nrow(filtered_df()) == 0) return("No data.")
    top_color <- filtered_df() %>% count(Color) %>% arrange(desc(n)) %>% slice(1) %>% pull(Color)
    tags$span(tags$b("Seasonal Insight:"), sprintf(" '%s' is the top color preference.", top_color))
  })
  
  output$revenue_conclusion <- renderUI({
    df <- filtered_df()
    if (nrow(df) == 0) return("No data.")
    top_rev_cat <- df %>% group_by(Category) %>% summarise(Total = sum(Purchase.Amount..USD.)) %>% arrange(desc(Total)) %>% slice(1) %>% pull(Category)
    tags$span(tags$b("Revenue Insight:"), sprintf(" '%s' is the top-performing category by total revenue.", top_rev_cat))
  })
  
  output$promo_conclusion <- renderUI({
    df <- filtered_df()
    if(nrow(df) == 0) return("No data.")
    if (sum(df$Promo.Code.Used == "Yes") == 0) return(tags$span(style="color: #d9534f;", tags$b("Sensitivity Insight:"), " Note: No transactions in this segment utilized a promo code."))
    p_val <- t.test(Purchase.Amount..USD. ~ Promo.Code.Used, data = df)$p.value
    tags$span(tags$b("Statistical Insight:"), sprintf(" Promo impact is %s (p = %.4f).", ifelse(p_val < 0.05, "significant", "not significant"), p_val))
  })
  
  output$map_conclusion <- renderUI({
    df_map <- filtered_df() %>% group_by(Location) %>% summarise(m = mean(Purchase.Amount..USD.)) %>% arrange(desc(m))
    if(nrow(df_map) == 0) return("No data.")
    top_state <- slice(df_map, 1)
    tags$span(tags$b("Regional Insight:"), sprintf(" %s leads with an average spend of %s.", top_state$Location, dollar(top_state$m)))
  })
  
  output$shipping_conclusion <- renderUI({
    df <- filtered_df(); if(nrow(df) == 0) return("No data.")
    top_ship <- df %>% count(Shipping.Type) %>% arrange(desc(n)) %>% slice(1) %>% pull(Shipping.Type)
    top_pay <- df %>% count(Payment.Method) %>% arrange(desc(n)) %>% slice(1) %>% pull(Payment.Method)
    tags$span(tags$b("Logistic Insight:"), sprintf(" %s is the most used shipping method. ", top_ship), tags$b("| Payment Insight:"), sprintf(" %s is the preferred payment channel.", top_pay))
  })
  
  # --- THEMED PLOTS ---
  output$gender_bar <- renderPlotly({
    p <- filtered_df() %>% count(Gender) %>% ggplot(aes(Gender, n, fill = Gender)) + geom_col(width=0.6) +
      scale_fill_manual(values = c("Male" = "#002d62", "Female" = "#4dabff")) + theme_minimal() + labs(title = "Gender Distribution")
    apply_dark_theme(ggplotly(p))
  })
  
  output$age_pyramid <- renderPlotly({
    p <- filtered_df() %>% count(Age, Gender) %>% mutate(n = ifelse(Gender == "Male", -n, n)) %>%
      ggplot(aes(Age, n, fill = Gender)) + geom_col() + coord_flip() +
      scale_fill_manual(values = c("Male" = "#002d62", "Female" = "#4dabff")) + theme_minimal() + labs(title = "Age Distribution Pyramid")
    apply_dark_theme(ggplotly(p))
  })
  
  output$size_bar <- renderPlotly({
    p <- filtered_df() %>% count(Size) %>% ggplot(aes(Size, n, fill = Size)) + geom_col(width=0.7) +
      scale_fill_manual(values = c("S" = "#002d62", "M" = "#004085", "L" = "#007bff", "XL" = "#4dabff")) + theme_minimal() + labs(title = "Size Preference Distribution")
    apply_dark_theme(ggplotly(p))
  })
  
  output$history_density <- renderPlotly({
    p <- ggplot(filtered_df(), aes(Previous.Purchases)) + geom_density(fill = "#004085", color = "#4dabff", alpha = 0.6) +
      theme_minimal() + labs(title = "Purchase History Density")
    apply_dark_theme(ggplotly(p))
  })
  
  output$color_heatmap <- renderPlotly({
    p <- filtered_df() %>% count(Category, Color) %>% ggplot(aes(Category, Color, fill = n)) + geom_tile(color = "white") +
      scale_fill_gradient(low = "#e7f1ff", high = "#004085") + theme_minimal() + labs(title = "Color Popularity by Category")
    apply_dark_theme(ggplotly(p))
  })
  
  output$revenue_donut <- renderPlotly({
    df_donut <- filtered_df() %>%
      group_by(Category) %>%
      summarise(Total_Revenue = sum(Purchase.Amount..USD.))
    
    p <- plot_ly(df_donut, labels = ~Category, values = ~Total_Revenue, type = 'pie', hole = 0.6,
                 marker = list(colors = c("#002d62", "#004085", "#0056b3", "#007bff"))) %>%
      layout(title = list(text = "Revenue Contribution by Category", font = list(size = 16, face = "bold")))
    
    apply_dark_theme(p)
  })
  
  output$promo_boxplot <- renderPlotly({
    df <- filtered_df()
    if(sum(df$Promo.Code.Used == "Yes") == 0) { p <- ggplot(df) + labs(title = "No Promo Usage") + theme_minimal() }
    else { p <- ggplot(df, aes(Promo.Code.Used, Purchase.Amount..USD., fill = Promo.Code.Used)) + geom_boxplot(outlier.color = "#4dabff") + 
      scale_fill_manual(values = c("Yes" = "#004085", "No" = "#91caff")) + theme_minimal() + labs(title = "Promo vs Spending") }
    apply_dark_theme(ggplotly(p))
  })
  
  output$us_map_refined <- renderPlotly({
    states_map <- map_data("state")
    region_df <- filtered_df() %>% group_by(Location_Lower) %>% summarise(Avg_Spend = mean(Purchase.Amount..USD.))
    map_df <- left_join(states_map, region_df, by = c("region" = "Location_Lower"))
    p <- ggplot(map_df, aes(long, lat, group = group, fill = Avg_Spend)) +
      geom_polygon(color = "white", size = 0.1) + scale_fill_gradient(low = "#d0e2ff", high = "#004085") + theme_void() + labs(title = "Avg Spend per State")
    apply_dark_theme(ggplotly(p)) %>% layout(xaxis = list(visible = FALSE), yaxis = list(visible = FALSE))
  })
  
  output$shipping_plot <- renderPlotly({
    p <- ggplot(filtered_df(), aes(Category, fill = Shipping.Type)) + geom_bar(position = "fill") + 
      scale_fill_manual(values = c("#002d62", "#004085", "#0056b3", "#007bff", "#4dabff", "#b3d9ff")) + theme_minimal() + theme(axis.text.x = element_text(angle=45)) + labs(title = "Shipping Methods")
    apply_dark_theme(ggplotly(p))
  })
  
  output$payment_plot <- renderPlotly({
    p <- ggplot(filtered_df(), aes(Payment.Method, Purchase.Amount..USD., fill = Payment.Method)) + geom_boxplot() + 
      scale_fill_manual(values = c("#002d62", "#004085", "#0056b3", "#007bff", "#4dabff", "#b3d9ff")) + theme_minimal() + guides(fill="none") + labs(title = "Payment Distribution")
    apply_dark_theme(ggplotly(p))
  })
  
  output$dynamic_scatter <- renderPlotly({
    p <- ggplot(filtered_df(), aes_string(x = input$x_var, y = "Purchase.Amount..USD.", color = input$color_var)) +
      geom_point(alpha = 0.7, size = 2) + geom_smooth(method = "lm", color = "#4dabff", se = FALSE, linetype = "dashed") + 
      scale_color_manual(values = c("#002d62", "#0056b3", "#4dabff", "#91caff", "#c6e3ff")) + theme_minimal() + labs(title = "Correlation Analysis")
    apply_dark_theme(ggplotly(p))
  })
  
  output$corr_info <- renderUI({
    val <- cor(filtered_df()[[input$x_var]], filtered_df()$Purchase.Amount..USD., use = "complete.obs")
    h4(paste("Pearson Correlation:", round(val, 3)), style="color: var(--accent);")
  })
  
  output$raw_sample_table <- renderTable({ head(filtered_df(), 6) }, striped=T, hover=T)
}

shinyApp(ui, server)