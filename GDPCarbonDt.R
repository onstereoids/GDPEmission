library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(plotly)
library(countrycode)
library(maps)
library(rnaturalearth)
library(rnaturalearthdata)

# Load data
data <- read.csv("data_CO2_cleaned_for_map.csv", stringsAsFactors = FALSE)
data$Tahun <- as.numeric(data$Tahun)
data$Emisi <- as.numeric(data$Emisi)

data_ratio <- read.csv("Data CO2 to GDP.csv")
for (col in names(data_ratio)[2:ncol(data_ratio)]) {
  data_ratio[[col]] <- as.numeric(gsub(",", "", data_ratio[[col]]))
}

long_data_ratio <- data_ratio %>% 
  pivot_longer(cols = -Country, names_to = "Tahun", values_to = "Rasio") %>%
  mutate(Tahun = as.numeric(gsub("X", "", Tahun)))

# UI
ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Roboto:wght@300;400;700&display=swap"),
    tags$style(HTML(
      "
      body {
        font-family: 'Roboto', sans-serif;
        background-color: #0d1b2a;
        color: #e0f2f1;
        margin: 0;
      }
      .container-fluid {
        width: 100vw;
        height: 100vh;
      }
      .selectize-input, .form-control {
        font-size: 14px;
        background-color: #1b263b !important;
        color: #e0f2f1 !important;
        border: 1px solid #415a77;
        border-radius: 6px;
      }
      .selectize-dropdown, .selectize-dropdown-content {
        background-color: #1b263b !important;
        color: #e0f2f1 !important;
      }
      .selectize-dropdown .active {
        background-color: #00bcd4 !important;
        color: #ffffff !important;
      }
      .well {
        background-color: #1e293b !important;
        border: none;
      }
      .form-control:focus, .selectize-input:focus {
        border-color: #00bcd4;
        box-shadow: 0 0 5px rgba(0, 188, 212, 0.5);
      }
      .tabbable > .nav > li > a {
        color: #e0f2f1;
        background-color: #1b263b;
        border: 1px solid #415a77;
        border-radius: 6px 6px 0 0;
        margin-right: 4px;
      }
      .tabbable > .nav > li[class='active'] > a {
        background-color: #00bcd4;
        color: white;
      }
      .btn {
        background-color: #0288d1;
        border-color: #0288d1;
        color: white;
        font-weight: bold;
      }
      .btn:hover {
        background-color: #03a9f4;
        border-color: #03a9f4;
      }
      .slider-animate-container {
        background-color: transparent;
      }
      .irs--shiny .irs-bar {
        background-color: #00bcd4;
        border-top: 1px solid #00bcd4;
        border-bottom: 1px solid #00bcd4;
      }
      .irs--shiny .irs-handle {
        border: 1px solid #00bcd4;
        background-color: #00bcd4;
      }
      h2, h3, h4, .title {
        color: #ffffff;
      }
      .plotly html-widget {
        background-color: #0d1b2a !important;
      }
      "
    ))
  ),
  titlePanel("Dashboard Interaktif GDP & Emisi CO2"),
  tabsetPanel(
    tabPanel("Peta Emisi CO2 Global",
             fluidRow(
               column(12,
                      div(style = "background-color: #1b263b; padding: 10px; border-radius: 10px;",
                          plotlyOutput("map_emisi", height = "700px")
                      )
               )
             )
    ),
    tabPanel("Grafik CO2 per GDP",
             sidebarLayout(
               sidebarPanel(
                 selectizeInput("selected_countries", "Pilih hingga 5 negara:",
                                choices = unique(long_data_ratio$Country),
                                selected = NULL,
                                multiple = TRUE,
                                options = list(maxItems = 5)),
                 actionButton("run", "Jalankan", icon = icon("play")),
                 actionButton("reset", "Hapus Pilihan", icon = icon("trash"))
               ),
               mainPanel(
                 sliderInput("tahun_range", "Tahun: (Rentang)",
                             min = min(long_data_ratio$Tahun), max = max(long_data_ratio$Tahun),
                             value = c(min(long_data_ratio$Tahun), max(long_data_ratio$Tahun)), step = 1),
                 plotlyOutput("grafik_ratio_main"),
                 plotlyOutput("grafik_ratio_trend"),
                 verbatimTextOutput("trend_text")
               )
             )
    )
  )
)

# Server
server <- function(input, output, session) {
  selected_countries <- reactiveVal()
  
  observeEvent(input$run, {
    selected_countries(input$selected_countries)
  })
  
  observeEvent(input$reset, {
    updateSelectizeInput(session, "selected_countries", selected = NULL)
    selected_countries(NULL)
  })
  
  df_long_reactive <- reactive({
    req(selected_countries())
    long_data_ratio %>% filter(Country %in% selected_countries())
  })
  
  output$grafik_ratio_main <- renderPlotly({
    df_long <- df_long_reactive()
    df_filtered <- df_long %>% filter(Tahun >= input$tahun_range[1], Tahun <= input$tahun_range[2])
    p1 <- ggplot(df_filtered, aes(x = Tahun, y = Rasio, group = Country, color = Country)) +
      geom_line(size = 1) +
      geom_point(size = 1.8) +
      labs(title = paste("Rasio Emisi CO2 terhadap GDP per Tahun",
                         input$tahun_range[1], "hingga", input$tahun_range[2]),
           x = "Tahun", y = "CO2/GDP") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            panel.background = element_rect(fill = "#0d1b2a"),
            plot.background = element_rect(fill = "#0d1b2a", color = NA),
            legend.background = element_rect(fill = "#0d1b2a"),
            legend.text = element_text(color = "#ffffff"),
            legend.title = element_text(color = "#ffffff"),
            axis.text = element_text(color = "#ffffff"),
            axis.title = element_text(color = "#ffffff"),
            plot.title = element_text(color = "#ffffff"))
    ggplotly(p1)
  })
  
  output$grafik_ratio_trend <- renderPlotly({
    df_long <- df_long_reactive()
    df_filtered <- df_long %>% filter(Tahun >= input$tahun_range[1], Tahun <= input$tahun_range[2])
    p2 <- ggplot(df_filtered, aes(x = Tahun, y = Rasio, group = Country, color = Country)) +
      geom_smooth(method = "lm", se = FALSE, size = 1, alpha = 0.2) +
      labs(title = paste("Trendline Linear CO2/GDP",
                         input$tahun_range[1], "hingga", input$tahun_range[2]),
           x = "Tahun", y = "Trend CO2/GDP") +
      theme_minimal() +
      theme(panel.background = element_rect(fill = "#0d1b2a"),
            plot.background = element_rect(fill = "#0d1b2a", color = NA),
            legend.background = element_rect(fill = "#0d1b2a"),
            legend.text = element_text(color = "#ffffff"),
            legend.title = element_text(color = "#ffffff"),
            axis.text = element_text(color = "#ffffff"),
            axis.title = element_text(color = "#ffffff"),
            plot.title = element_text(color = "#ffffff"))
    ggplotly(p2)
  })
  
  output$trend_text <- renderPrint({
    df_long <- df_long_reactive() %>% filter(Tahun >= input$tahun_range[1], Tahun <= input$tahun_range[2])
    trends <- df_long %>% group_by(Country) %>% summarise(
      Trend = {
        model <- lm(Rasio ~ Tahun, data = .)
        slope <- coef(model)[2]
        if (slope > 0.0001) "Naik" else if (slope < -0.0001) "Turun" else "Netral"
      },
      .groups = "drop"
    )
    cat("\nTrend Emisi CO2 terhadap GDP:")
    for (i in 1:nrow(trends)) {
      cat(paste0("\n- ", trends$Country[i], ": ", trends$Trend[i]))
    }
  })
  
  output$map_emisi <- renderPlotly({
    world_data <- ne_countries(scale = "medium", returnclass = "sf")
    df_map <- data %>%
      filter(Tahun == 2023) %>%
      select(Country, Emisi) %>%
      mutate(iso_a3 = countrycode(Country, "country.name", "iso3c"))
    world_data <- left_join(world_data, df_map, by = c("iso_a3" = "iso_a3"))
    
    z_values <- as.numeric(world_data$Emisi)
    z_min <- min(z_values, na.rm = TRUE)
    z_max <- max(z_values, na.rm = TRUE)
    
    color_scale <- list(
      list(0, '#e0f7fa'),
      list(0.25, '#4dd0e1'),
      list(0.5, '#26c6da'),
      list(0.75, '#00acc1'),
      list(1.0, '#00838f')
    )
    
    norm_z <- (z_values - z_min) / (z_max - z_min)
    norm_z[is.na(norm_z)] <- 0
    
    plot_geo(world_data) %>%
      add_trace(
        z = norm_z,
        colorscale = color_scale,
        zmin = 0, zmax = 1,
        text = ~paste(name, "<br>", ifelse(is.na(z_values), "No data", paste(formatC(z_values, format = "f", big.mark = ",", digits = 0), "ton"))),
        locations = ~iso_a3,
        locationmode = "ISO-3",
        marker = list(line = list(width = 0.5, color = 'gray'))
      ) %>%
      colorbar(title = "Emisi (ton)") %>%
      layout(
        geo = list(showframe = FALSE, showcoastlines = FALSE, projection = list(type = 'equirectangular'), bgcolor = '#0d1b2a'),
        paper_bgcolor = '#0d1b2a',
        plot_bgcolor = '#0d1b2a',
        font = list(color = '#ffffff'),
        title = list(text = "Emisi CO2 per Negara Tahun 2023", font = list(color = '#ffffff'))
      )
  })
}

shinyApp(ui = ui, server = server)