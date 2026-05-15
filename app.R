# ================================================
# NurseNow - Complete MVP Dashboard
# ================================================

library(shiny)
library(leaflet)
library(dplyr)
library(ggplot2)
library(DT)
library(bslib)
library(tigris)
library(sf)

options(tigris_use_cache = TRUE)

# ================== DATA ==================
if (file.exists("zipcode_demand_data_enhanced.csv")) {
  data <- read.csv("zipcode_demand_data_enhanced.csv") %>%
    mutate(zcta = as.character(zcta))
  cat("✅ Loaded Enhanced Data (with CDC)\n")
} else {
  data <- read.csv("zipcode_demand_data.csv") %>%
    mutate(zcta = as.character(zcta))
  cat("✅ Loaded Base Data\n")
}

# Simulated Available Nurses
nurses <- tibble(
  id = 1:12,
  lat = 33.75 + rnorm(12, 0, 0.09),
  lng = -84.39 + rnorm(12, 0, 0.10),
  name = c("Sarah K.", "Michael R.", "Aisha P.", "David L.", "Elena M.", "James T.",
           "Priya S.", "Robert B.", "Fatima N.", "Carlos R.", "Emma W.", "Jamal K."),
  specialty = c("Elderly Care","Wound Care","Medication","Post-Op","Dementia","IV Therapy",
                "Elderly Care","Palliative","Wound Care","Med Admin","Post-Op","Dementia"),
  rating = round(runif(12, 4.4, 4.9), 1),
  years_exp = sample(3:15, 12, replace = TRUE)
)

# ================== UI ==================
ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "darkly", primary = "#00bfff"),
  
  titlePanel("NurseNow — Atlanta On-Demand Nursing Platform"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Filters"),
      sliderInput("score_range", "Demand Score:", min = 0, max = 100, value = c(25, 100)),
      sliderInput("pop_range", "Population:", min = 5000, max = 50000, value = c(8000, 48000)),
      checkboxInput("show_nurses", "Show Available Nurses", value = TRUE),
      actionButton("refresh", "Refresh Dashboard", icon = icon("sync"), class = "btn-primary"),
      
      hr(),
      h5("Summary"),
      verbatimTextOutput("summary_stats")
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel("Live Demand Map",
                 leafletOutput("demand_map", height = "720px")),
        
        tabPanel("Top ZIP Codes",
                 DT::dataTableOutput("top_zips"),
                 br(),
                 downloadButton("export_csv", "📥 Export Top ZIPs to CSV", class = "btn-success")),
        
        tabPanel("Charts",
                 plotOutput("score_histogram", height = "620px")),
        
        tabPanel("Matching Demo",
                 h4("Nurse-Family Matching Demo"),
                 verbatimTextOutput("match_demo"))
      )
    )
  )
)

# ================== SERVER ==================
server <- function(input, output, session) {
  
  filtered <- reactive({
    data %>%
      filter(demand_score >= input$score_range[1],
             demand_score <= input$score_range[2],
             total_pop >= input$pop_range[1],
             total_pop <= input$pop_range[2])
  })
  
  # Demand Map
  output$demand_map <- renderLeaflet({
    df <- filtered()
    pal <- colorNumeric("viridis", domain = data$demand_score)
    
    leaflet() %>%
      addProviderTiles(providers$CartoDB.DarkMatter) %>%
      setView(lng = -84.39, lat = 33.75, zoom = 11) %>%
      
      # Demand Circles
      addCircleMarkers(
        data = df,
        lng = ~lng, lat = ~lat,
        radius = ~sqrt(total_pop)/85,
        color = "#ffffff",
        fillColor = ~pal(demand_score),
        fillOpacity = 0.85,
        weight = 2,
        popup = ~paste0("<b>ZCTA: ", zcta, "</b><br>",
                        "Demand Score: <b>", round(demand_score,1), "/100</b><br>",
                        "Elderly: ", elderly_pct, "%")
      ) %>%
      
      # Available Nurses
      {if (input$show_nurses) {
        addCircleMarkers(.,
                         data = nurses,
                         lng = ~lng, lat = ~lat,
                         radius = 11,
                         color = "#00ff00",
                         fillColor = "#00ff00",
                         fillOpacity = 0.95,
                         weight = 3,
                         popup = ~paste0("<b>", name, "</b><br>", specialty, "<br>⭐ ", rating, " • ", years_exp, " yrs")
        )
      } else .} %>%
      
      addLegend("bottomright", pal = pal, values = data$demand_score, title = "Demand Score")
  })
  
  # Top ZIPs Table
  output$top_zips <- DT::renderDataTable({
    filtered() %>%
      arrange(desc(demand_score)) %>%
      select(ZCTA = zcta, Demand = demand_score, Population = total_pop,
             Elderly = elderly_pct, Loneliness = loneliness_pct, 
             Disability = disability_pct, Poor_Health_Days = poor_health_days)
  }, options = list(pageLength = 15))
  
  # Export
  output$export_csv <- downloadHandler(
    filename = function() { paste0("NurseNow_Top_ZIPs_", Sys.Date(), ".csv") },
    content = function(file) {
      write.csv(filtered() %>% arrange(desc(demand_score)), file, row.names = FALSE)
    }
  )
  
  # Histogram
  output$score_histogram <- renderPlot({
    ggplot(filtered(), aes(x = demand_score)) +
      geom_histogram(bins = 25, fill = "#00bfff", color = "white") +
      theme_minimal(base_size = 14) +
      labs(title = "Demand Score Distribution", x = "Demand Score", y = "Number of ZIP Codes")
  })
  
  # Summary
  output$summary_stats <- renderPrint({
    df <- filtered()
    cat("ZCTAs Shown:", nrow(df), "\n")
    cat("Avg Demand Score:", round(mean(df$demand_score), 1), "\n")
    cat("Available Nurses:", nrow(nurses), "\n")
    cat("Total Population:", format(sum(df$total_pop), big.mark = ","), "\n")
  })
  
  # Matching Demo
  output$match_demo <- renderPrint({
    family <- list(service_type = "Wound Care", distance_miles = 3.5)
    nurse <- list(specialties = c("Wound Care", "Elderly Care"), 
                  rating = 4.8, available_now = TRUE, years_exp = 9)
    
    score <- round(
      (max(0, 100 - family$distance_miles * 8) * 0.30) +
        (ifelse(family$service_type %in% nurse$specialties, 100, 40) * 0.25) +
        (nurse$rating * 20 * 0.20) +
        (ifelse(nurse$available_now, 100, 30) * 0.15) +
        (min(100, nurse$years_exp * 8) * 0.10), 1)
    
    cat("Sample Matching Result:\n")
    cat("Family needs:", family$service_type, "within", family$distance_miles, "miles\n")
    cat("Best Nurse Match Score:", score, "/100\n")
    cat("(Higher score = better match)")
  })
}

# Run the App
shinyApp(ui = ui, server = server)