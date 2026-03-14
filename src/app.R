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
      label = "Start year:",      choices = year_choices,
      selected = min(year_choices)
    ),
    selectInput(
      inputId = "input_end_year",
      label = "End year:",
      choices = year_choices,
      selected = max(year_choices)
    ),
    selectizeInput(
      inputId = "country",
      label = "Country:",
      choices = country_choices,
      selected = country_choices,
      multiple = TRUE,
      options = list(plugins = list("remove_button"))
    )
  ),
  uiOutput("out_total_transactions"),
  card(
    full_screen = TRUE,
    card_header("Sales Trend by Country Over Time"),
    plotOutput(outputId = "out_sales_trend_plot", height = "320px")
  ),
  layout_columns(
    col_widths = c(6, 6),
    card(
      full_screen = TRUE,
      card_header("Country Contribution Table"),
      tableOutput(outputId = "out_country_contrib_table")
    ),
    card(
      full_screen = TRUE,
      card_header("Year-over-Year Growth By Country"),
      plotOutput(outputId = "out_yoy_country_plot", height = "260px")
    )
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

  output$out_total_transactions <- renderUI({
    total_transactions <- nrow(filtered_sales())

    value_box(
      title = "Total Transactions",
      value = format(total_transactions, big.mark = ","),
      showcase = icon("bar-chart"),
      theme = "blue"
    )
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

  output$out_sales_trend_plot <- renderPlot({
    filtered <- filtered_sales()

    filtered$quarter_num <- ((filtered$month_num - 1) %/% 3) + 1
    filtered$quarter_label <- paste0(filtered$year, "-Q", filtered$quarter_num)

    trend <- aggregate(sales ~ country + year + quarter_num + quarter_label, data = filtered, sum)
    validate(need(nrow(trend) > 0, "No trend data available for selected filters."))

    trend <- trend[order(trend$year, trend$quarter_num), , drop = FALSE]
    quarter_lookup <- unique(trend[, c("year", "quarter_num", "quarter_label")])
    quarter_lookup <- quarter_lookup[order(quarter_lookup$year, quarter_lookup$quarter_num), , drop = FALSE]

    quarter_levels <- quarter_lookup$quarter_label
    trend$quarter_index <- match(trend$quarter_label, quarter_levels)
    countries <- unique(trend$country)

    y_max <- max(trend$sales, na.rm = TRUE)
    if (!is.finite(y_max) || y_max <= 0) {
      y_max <- 1
    }

    par(mar = c(6, 4, 3, 10))

    plot(
      NA,
      xlim = c(0.7, length(quarter_levels) + 0.3),
      ylim = c(0, y_max * 1.05),
      xlab = "Quarter",
      ylab = "Total Sales (USD)",
      main = "Quarterly Sales Trend by Country",
      xaxt = "n"
    )
    axis(1, at = seq_along(quarter_levels), labels = quarter_levels, las = 2, cex.axis = 0.85, mgp = c(1, 0.5, 0))

    colors <- grDevices::hcl.colors(length(countries), "Dark 3")
    for (i in seq_along(countries)) {
      country_data <- trend[trend$country == countries[i], , drop = FALSE]
      country_data <- country_data[order(country_data$quarter_index), , drop = FALSE]
      lines(country_data$quarter_index, country_data$sales, col = colors[i], lwd = 2)
      points(country_data$quarter_index, country_data$sales, col = colors[i], pch = 16, cex = 0.7)
    }

    legend(
      x = par("usr")[2] + diff(par("usr")[1:2]) * 0.02,
      y = par("usr")[4],
      legend = countries,
      col = colors,
      lwd = 2,
      bty = "n",
      title = "Country",
      xpd = NA
    )
  })

  output$out_country_contrib_table <- renderTable({
    filtered <- filtered_sales()

    summary <- aggregate(sales ~ country, data = filtered, sum)

    rep_summary <- aggregate(sales ~ country + sales_person, data = filtered, sum)
    rep_summary <- rep_summary[order(rep_summary$country, -rep_summary$sales, rep_summary$sales_person), , drop = FALSE]
    top_rep <- rep_summary[!duplicated(rep_summary$country), c("country", "sales_person")]
    names(top_rep)[2] <- "top_sales_rep"

    summary <- merge(summary, top_rep, by = "country", all.x = TRUE)
    summary <- summary[order(summary$sales, decreasing = TRUE), , drop = FALSE]

    total_sales <- sum(summary$sales)
    summary$contribution_pct <- if (total_sales == 0) 0 else (summary$sales / total_sales) * 100

    data.frame(
      Country = summary$country,
      `Top Sales Rep` = summary$top_sales_rep,
      `Total Sales (USD)` = paste0("$", format(round(summary$sales, 0), big.mark = ",", scientific = FALSE)),
      `Contribution (%)` = sprintf("%.1f%%", summary$contribution_pct),
      check.names = FALSE
    )
  }, striped = FALSE, hover = FALSE, bordered = TRUE, spacing = "m")
}

shinyApp(ui = ui, server = server)
