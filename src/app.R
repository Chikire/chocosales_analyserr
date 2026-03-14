# Load required libraries
library(shiny)
library(bslib)

# Load dataset once at startup.
# Try both paths to support running from project root or src/ directory.
data_candidates <- c("data/chocolate_sales_clean.csv", "../data/chocolate_sales_clean.csv")
data_path <- data_candidates[file.exists(data_candidates)][1]

if (is.na(data_path)) {
  stop("Could not find data/chocolate_sales_clean.csv")
}

sales_data <- read.csv(data_path, stringsAsFactors = FALSE)

# Pre-compute filter choices from the loaded data
country_choices <- sort(unique(sales_data$country))
year_choices <- sort(unique(sales_data$year))

# Define UI for app ----
ui <- page_sidebar(
  title = "Chocosales Sales Analyser",
  # Sidebar contains all filter controls
  sidebar = sidebar(
    # Year range filters: users select a start and end year for the analysis period
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
    # Multi-select country filter with remove buttons for easy deselection
    selectizeInput(
      inputId = "country",
      label = "Country:",
      choices = country_choices,
      selected = country_choices,
      multiple = TRUE,
      options = list(plugins = list("remove_button"))
    )
  ),
  # Summary metric: total number of transactions matching current filters
  uiOutput("out_total_transactions"),
  # Full-width card: quarterly sales trend lines per country
  card(
    full_screen = TRUE,
    card_header("Sales Trend by Country Over Time"),
    plotOutput(outputId = "out_sales_trend_plot", height = "320px")
  ),
  # Two-column layout: contribution table (left) and YoY growth chart (right)
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
  ),
  # Footer metadata shown at the bottom of the app
  tags$div(
    style = "text-align: center; color: #6b7280; font-size: 0.9rem; margin-top: 1rem; margin-bottom: 0.5rem;",
    "Author: Chikire Aku-Ibe | Last Updated: 2026-03-13"
  )
)

# Define server logic ----
server <- function(input, output, session) {

  # Reactive: returns rows of sales_data matching the current filter selections.
  # All outputs depend on this reactive to avoid repeated subsetting.
  filtered_sales <- reactive({
    req(input$country, input$input_start_year, input$input_end_year)

    start_year <- as.integer(input$input_start_year)
    end_year <- as.integer(input$input_end_year)
    # Validate year range before filtering
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

  # Render a value box showing the total number of filtered transactions
  output$out_total_transactions <- renderUI({
    total_transactions <- nrow(filtered_sales())

    value_box(
      title = tags$div("Total Transactions (Count)", style = "font-size: 1.3rem;"),
      value = format(total_transactions, big.mark = ","),
      showcase = icon("bar-chart"),
      theme = "blue"
    )
  })

  # Reactive: computes year-over-year percentage change in sales per country
  # between the selected start and end years.
  yoy_by_country <- reactive({
    filtered <- filtered_sales()
    start_year <- as.integer(input$input_start_year)
    end_year <- as.integer(input$input_end_year)

    # Aggregate total sales per country and year
    sales_by_country_year <- aggregate(sales ~ country + year, data = filtered, sum)

    # Extract sales for the start year (baseline)
    sales_prev <- sales_by_country_year[
      sales_by_country_year$year == start_year,
      c("country", "sales")
    ]
    names(sales_prev)[2] <- "sales_prev"

    # Extract sales for the end year (comparison)
    sales_curr <- sales_by_country_year[
      sales_by_country_year$year == end_year,
      c("country", "sales")
    ]
    names(sales_curr)[2] <- "sales_curr"

    # Inner join: only include countries present in both years
    comparison <- merge(sales_prev, sales_curr, by = "country", all = FALSE)

    if (nrow(comparison) == 0) {
      return(comparison)
    }

    # Calculate % change; set to NA where baseline sales were zero to avoid division by zero
    comparison$pct_change <- ifelse(
      comparison$sales_prev == 0,
      NA_real_,
      ((comparison$sales_curr - comparison$sales_prev) / comparison$sales_prev) * 100
    )

    # Sort by % change descending for plot ordering
    comparison <- comparison[order(comparison$pct_change, decreasing = TRUE), , drop = FALSE]
    rownames(comparison) <- comparison$country
    comparison
  })

  # Render horizontal bar chart of YoY % change in sales per country
  output$out_yoy_country_plot <- renderPlot({
    comparison <- yoy_by_country()

    validate(need(nrow(comparison) > 0, "No YoY comparison data available."))
    validate(need(any(!is.na(comparison$pct_change)), "No YoY comparison data available."))

    # Drop rows where % change could not be computed
    plot_data <- comparison[!is.na(comparison$pct_change), , drop = FALSE]
    # Blue for growth, orange for decline
    colors <- ifelse(plot_data$pct_change >= 0, "#0072B2", "#E69F00")

    # Save and restore graphical parameters on exit
    old_par <- par(no.readonly = TRUE)
    on.exit(par(old_par), add = TRUE)
    par(mar = c(5, 9, 4, 2) + 0.1)

    # Ensure x-axis always includes zero for reference
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

    # Draw zero baseline
    abline(v = 0, col = "#6b7280", lwd = 1)
    # Label each bar with its % value
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

  # Render quarterly sales trend line chart, one line per country
  output$out_sales_trend_plot <- renderPlot({
    filtered <- filtered_sales()

    # Derive quarter number (1–4) and a readable label (e.g. "2022-Q3")
    filtered$quarter_num <- ((filtered$month_num - 1) %/% 3) + 1
    filtered$quarter_label <- paste0(filtered$year, "-Q", filtered$quarter_num)

    # Aggregate to quarterly totals per country
    trend <- aggregate(sales ~ country + year + quarter_num + quarter_label, data = filtered, sum)
    validate(need(nrow(trend) > 0, "No trend data available for selected filters."))

    trend <- trend[order(trend$year, trend$quarter_num), , drop = FALSE]

    # Build an ordered sequence of quarter labels for the x-axis
    quarter_lookup <- unique(trend[, c("year", "quarter_num", "quarter_label")])
    quarter_lookup <- quarter_lookup[order(quarter_lookup$year, quarter_lookup$quarter_num), , drop = FALSE]

    quarter_levels <- quarter_lookup$quarter_label
    # Convert quarter labels to integer indices for plotting
    trend$quarter_index <- match(trend$quarter_label, quarter_levels)
    countries <- unique(trend$country)

    y_max <- max(trend$sales, na.rm = TRUE)
    # Fallback y-axis ceiling in case all sales are zero or non-finite
    if (!is.finite(y_max) || y_max <= 0) {
      y_max <- 1
    }

    # Extra right margin to accommodate the outside legend
    par(mar = c(6, 4, 3, 10))

    # Initialise empty plot area
    plot(
      NA,
      xlim = c(0.7, length(quarter_levels) + 0.3),
      ylim = c(0, y_max * 1.05),
      xlab = "Quarter",
      ylab = "Total Sales (USD)",
      main = "Quarterly Sales Trend by Country",
      xaxt = "n"
    )
    # Rotated x-axis labels for readability
    axis(1, at = seq_along(quarter_levels), labels = quarter_levels, las = 2, cex.axis = 0.85, mgp = c(1, 0.5, 0))

    # Assign a distinct colour to each country and draw lines + points
    colors <- grDevices::hcl.colors(length(countries), "Dark 3")
    for (i in seq_along(countries)) {
      country_data <- trend[trend$country == countries[i], , drop = FALSE]
      country_data <- country_data[order(country_data$quarter_index), , drop = FALSE]
      lines(country_data$quarter_index, country_data$sales, col = colors[i], lwd = 2)
      points(country_data$quarter_index, country_data$sales, col = colors[i], pch = 16, cex = 0.7)
    }

    # Place legend outside the right edge of the plot
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

  # Render country contribution table: total sales, top sales rep, and % share per country
  output$out_country_contrib_table <- renderTable({
    filtered <- filtered_sales()

    # Total sales per country
    summary <- aggregate(sales ~ country, data = filtered, sum)

    # Find the top sales rep per country (highest individual sales total)
    rep_summary <- aggregate(sales ~ country + sales_person, data = filtered, sum)
    rep_summary <- rep_summary[order(rep_summary$country, -rep_summary$sales, rep_summary$sales_person), , drop = FALSE]
    top_rep <- rep_summary[!duplicated(rep_summary$country), c("country", "sales_person")]
    names(top_rep)[2] <- "top_sales_rep"

    # Join top rep onto the country summary and sort by total sales descending
    summary <- merge(summary, top_rep, by = "country", all.x = TRUE)
    summary <- summary[order(summary$sales, decreasing = TRUE), , drop = FALSE]

    # Calculate each country's share of total sales; guard against zero-total edge case
    total_sales <- sum(summary$sales)
    summary$contribution_pct <- if (total_sales == 0) 0 else (summary$sales / total_sales) * 100

    # Format columns for display
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
