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
year_choices <- sort(unique(sales_data$year))

# Define UI for app ----
ui <- page_sidebar(
  title = "Chocosales Sales Analyser",
  sidebar = sidebar(
    selectInput(
      inputId = "input_start_year",
      label = "Start year:",
      choices = year_choices,
      selected = min(year_choices)
    ),
    selectInput(
      inputId = "input_end_year",
      label = "End year:",
      choices = year_choices,
      selected = max(year_choices)
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
  card(
    full_screen = TRUE,
    card_header("Year-over-Year Growth By Country"),
    plotOutput(outputId = "out_yoy_country_plot", height = "260px")
  )
)

# Define server logic ----
server <- function(input, output, session) {
  filtered_sales <- reactive({
    req(input$country, input$input_start_year, input$input_end_year)

    start_year <- as.integer(input$input_start_year)
    end_year <- as.integer(input$input_end_year)
    validate(need(start_year <= end_year, "Start year must be less than or equal to end year."))

    filtered <- sales_data[
      sales_data$country %in% input$country &
        sales_data$year >= start_year &
        sales_data$year <= end_year,
      ,
      drop = FALSE
    ]
    validate(need(nrow(filtered) > 0, "No data for selected filters."))
    filtered
  })

  yoy_by_country <- reactive({
    filtered <- filtered_sales()
    start_year <- as.integer(input$input_start_year)
    end_year <- as.integer(input$input_end_year)

    sales_by_country_year <- aggregate(sales ~ country + year, data = filtered, sum)

    sales_prev <- sales_by_country_year[
      sales_by_country_year$year == start_year,
      c("country", "sales")
    ]
    names(sales_prev)[2] <- "sales_prev"

    sales_curr <- sales_by_country_year[
      sales_by_country_year$year == end_year,
      c("country", "sales")
    ]
    names(sales_curr)[2] <- "sales_curr"

    comparison <- merge(sales_prev, sales_curr, by = "country", all = FALSE)

    if (nrow(comparison) == 0) {
      return(comparison)
    }

    comparison$pct_change <- ifelse(
      comparison$sales_prev == 0,
      NA_real_,
      ((comparison$sales_curr - comparison$sales_prev) / comparison$sales_prev) * 100
    )

    comparison <- comparison[order(comparison$pct_change, decreasing = TRUE), , drop = FALSE]
    rownames(comparison) <- comparison$country
    comparison
  })

  output$out_yoy_country_plot <- renderPlot({
    comparison <- yoy_by_country()

    validate(need(nrow(comparison) > 0, "No YoY comparison data available."))
    validate(need(any(!is.na(comparison$pct_change)), "No YoY comparison data available."))

    plot_data <- comparison[!is.na(comparison$pct_change), , drop = FALSE]
    colors <- ifelse(plot_data$pct_change >= 0, "#0072B2", "#E69F00")

    old_par <- par(no.readonly = TRUE)
    on.exit(par(old_par), add = TRUE)
    par(mar = c(5, 9, 4, 2) + 0.1)

    x_limits <- range(c(plot_data$pct_change, 0))

    bar_positions <- barplot(
      plot_data$pct_change,
      horiz = TRUE,
      names.arg = plot_data$country,
      col = colors,
      border = NA,
      las = 1,
      xlim = x_limits,
      xlab = paste0(
        "Percent change in sales (%) - ",
        input$input_end_year,
        " vs ",
        input$input_start_year
      ),
      main = "Year-over-Year Growth By Country"
    )

    abline(v = 0, col = "#6b7280", lwd = 1)
    text(
      x = plot_data$pct_change,
      y = bar_positions,
      labels = sprintf("%.1f%%", plot_data$pct_change),
      pos = ifelse(plot_data$pct_change >= 0, 4, 2),
      cex = 0.9,
      col = "#374151",
      xpd = TRUE
    )
  })
}

shinyApp(ui = ui, server = server)
