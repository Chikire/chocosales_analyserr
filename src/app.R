library(shiny)
library(bslib)

# Load dataset once at startup.
data_candidates <- c("data/chocolate_sales_clean.csv", "../data/chocolate_sales_clean.csv")
data_path <- data_candidates[file.exists(data_candidates)][1]

if (is.na(data_path)) {
  stop("Could not find data/chocolate_sales_clean.csv")
}

sales_data <- read.csv(data_path, stringsAsFactors = FALSE)
country_choices <- sort(unique(sales_data$country))

# Define UI for app ----
ui <- page_sidebar(
  title = "Chocosales Sales Analyser",
  sidebar = sidebar(
    sliderInput(
      inputId = "bins",
      label = "Number of bins:",
      min = 5,
      max = 50,
      value = 20
    ),
    selectInput(
      inputId = "country",
      label = "Country:",
      choices = country_choices,
      selected = country_choices,
      multiple = TRUE
    )
  ),
  value_box(
    title = "Value box",
    value = 100,
    showcase = icon("chart-bar"),
    theme = "teal"
  ),
  card("Card"),
  card("Another card"),
  plotOutput(outputId = "distPlot")
)

# Define server logic ----
server <- function(input, output, session) {
  output$distPlot <- renderPlot({
    req(input$country)

    filtered <- sales_data[sales_data$country %in% input$country, , drop = FALSE]
    validate(need(nrow(filtered) > 0, "No data for selected country."))

    x <- filtered$sales
    bins <- seq(min(x), max(x), length.out = input$bins + 1)

    hist(
      x,
      breaks = bins,
      col = "#6B3E26",
      border = "white",
      xlab = "Sales",
      main = "Sales Distribution by Selected Country"
    )
  })
}

shinyApp(ui = ui, server = server)
