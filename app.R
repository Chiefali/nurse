library(shiny)
library(leaflet)
library(dplyr)
library(DT)
library(bslib)
library(shinyjs)

# ================== DATA ==================
set.seed(123)

data <- tibble(
  zcta = c("30303","30305","30309","30319","30326","30342","30030","30033","30345","30324"),
  total_pop = sample(12000:48000, 10, replace = TRUE),
  elderly_pct = round(runif(10, 12, 32), 1),
  loneliness_pct = round(runif(10, 20, 42), 1),
  disability_pct = round(runif(10, 14, 34), 1),
  poor_health_days = round(runif(10, 4, 8.5), 1),
  lat = 33.75 + rnorm(10, 0, 0.11),
  lng = -84.39 + rnorm(10, 0, 0.13)
) %>%
  mutate(
    demand_score = round(
      (elderly_pct * 0.3) +
        (loneliness_pct * 0.25) +
        (disability_pct * 0.2) +
        (poor_health_days * 0.15),
      1
    )
  )

nurses <- tibble(
  name = c("Sarah K.", "Michael R.", "Aisha P.", "David L.", "Elena M.", "James T.", "Priya S.", "Robert B."),
  specialty = c("Elderly Care", "Wound Care", "Medication", "Post-Op", "Dementia", "IV Therapy", "Elderly Care", "Palliative"),
  rating = round(runif(8, 4.6, 4.9), 1)
)

# ================== UI ==================
ui <- fluidPage(
  useShinyjs(),
  theme = bs_theme(version = 5, bootswatch = "darkly", primary = "#00bfff"),
  
  titlePanel("CHW - Atlanta Community Health Platform"),
  
  conditionalPanel(
    condition = "!output.logged_in",
    fluidRow(
      column(
        width = 4,
        offset = 4,
        tabsetPanel(
          tabPanel(
            "Login",
            wellPanel(
              h3("Login"),
              textInput("login_user", "Username", value = "demo"),
              passwordInput("login_pass", "Password", value = "123"),
              actionButton("login_btn", "Login", class = "btn-success", width = "100%"),
              br(),
              br(),
              p("Demo login: username = demo, password = 123", style = "color:#aaa;")
            )
          ),
          tabPanel(
            "Sign Up",
            wellPanel(
              h3("Create Account"),
              textInput("signup_name", "Full Name"),
              textInput("signup_email", "Email"),
              passwordInput("signup_pass", "Password"),
              fileInput("headshot", "Upload Headshot", accept = c("image/*")),
              actionButton("signup_btn", "Create Account", class = "btn-primary", width = "100%")
            )
          )
        )
      )
    )
  ),
  
  conditionalPanel(
    condition = "output.logged_in",
    sidebarLayout(
      sidebarPanel(
        h4("Welcome,"),
        textOutput("welcome_name"),
        actionButton("logout_btn", "Logout", class = "btn-danger", width = "100%"),
        hr(),
        actionButton("voice_btn", "🎤 Voice Assistant", class = "btn-info", width = "100%"),
        br(),
        br(),
        actionButton("dial_btn", "📞 Dial-in", class = "btn-success", width = "100%")
      ),
      
      mainPanel(
        tabsetPanel(
          tabPanel("Live Map", leafletOutput("demand_map", height = "680px")),
          tabPanel("Family View", DT::dataTableOutput("top_zips")),
          tabPanel("CHW View", DT::dataTableOutput("chw_table")),
          tabPanel(
            "📝 Feedback",
            h4("Provider & Family Feedback"),
            DT::dataTableOutput("feedback_table"),
            textAreaInput("feedback_text", "Write Feedback", rows = 3),
            actionButton("submit_feedback", "Submit Feedback", class = "btn-primary")
          )
        )
      )
    )
  )
)

# ================== SERVER ==================
server <- function(input, output, session) {
  
  logged_in <- reactiveVal(FALSE)
  user_name <- reactiveVal("")
  
  output$logged_in <- reactive({
    logged_in()
  })
  
  outputOptions(output, "logged_in", suspendWhenHidden = FALSE)
  
  output$welcome_name <- renderText({
    user_name()
  })
  
  feedback_data <- reactiveVal(
    data.frame(
      Date = character(),
      From = character(),
      To = character(),
      Feedback = character(),
      stringsAsFactors = FALSE
    )
  )
  
  observeEvent(input$login_btn, {
    if (input$login_user == "demo" && input$login_pass == "123") {
      logged_in(TRUE)
      user_name(input$login_user)
      showNotification("✅ Login Successful!", type = "message")
    } else {
      showNotification("❌ Wrong username or password. Use demo / 123", type = "error")
    }
  })
  
  observeEvent(input$signup_btn, {
    if (nzchar(input$signup_name) && nzchar(input$signup_email) && nzchar(input$signup_pass)) {
      logged_in(TRUE)
      user_name(input$signup_name)
      showNotification("✅ Account Created Successfully!", type = "message")
    } else {
      showNotification("❌ Please complete all signup fields.", type = "error")
    }
  })
  
  observeEvent(input$logout_btn, {
    logged_in(FALSE)
    user_name("")
    showNotification("Logged out successfully.", type = "message")
  })
  
  output$demand_map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.DarkMatter) %>%
      setView(lng = -84.39, lat = 33.75, zoom = 11) %>%
      addCircleMarkers(
        data = data,
        lng = ~lng,
        lat = ~lat,
        radius = ~sqrt(total_pop) / 90,
        color = "#00bfff",
        fillColor = "#00bfff",
        fillOpacity = 0.8,
        popup = ~paste0(
          "<b>ZCTA:</b> ", zcta,
          "<br><b>Total Population:</b> ", total_pop,
          "<br><b>Demand Score:</b> ", demand_score
        )
      ) %>%
      addCircleMarkers(
        data = nurses,
        lng = -84.39 + rnorm(nrow(nurses), 0, 0.09),
        lat = 33.75 + rnorm(nrow(nurses), 0, 0.08),
        radius = 11,
        color = "#00ff00",
        fillColor = "#00ff00",
        fillOpacity = 0.9,
        popup = ~paste0(
          "<b>Nurse:</b> ", name,
          "<br><b>Specialty:</b> ", specialty,
          "<br><b>Rating:</b> ", rating
        )
      )
  })
  
  output$top_zips <- DT::renderDataTable({
    data %>% arrange(desc(demand_score))
  })
  
  output$chw_table <- DT::renderDataTable({
    nurses
  })
  
  output$feedback_table <- DT::renderDataTable({
    feedback_data()
  })
  
  observeEvent(input$submit_feedback, {
    if (nzchar(input$feedback_text)) {
      new_row <- data.frame(
        Date = as.character(Sys.time()),
        From = user_name(),
        To = "All Providers",
        Feedback = input$feedback_text,
        stringsAsFactors = FALSE
      )
      
      feedback_data(rbind(feedback_data(), new_row))
      updateTextAreaInput(session, "feedback_text", value = "")
      showNotification("✅ Feedback Submitted!", type = "message")
    } else {
      showNotification("Please write feedback before submitting.", type = "warning")
    }
  })
  
  observeEvent(input$voice_btn, {
    showNotification("🎤 Voice Assistant Activated Demo", type = "message")
  })
  
  observeEvent(input$dial_btn, {
    showNotification("📞 Dial-in Connected Simulation", type = "message")
  })
}

shinyApp(ui = ui, server = server)