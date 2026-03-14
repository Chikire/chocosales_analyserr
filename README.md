# chocosales_analyserr

ChocoSales Analyser is a small R Shiny dashboard for exploring global chocolate sales data from 2022 to 2024.

Link to published version on Posit Cloud: https://019ce9e6-afa6-dadf-57a7-a30ce9d213f5.share.connect.posit.cloud/ 

## What this app does

- Filter sales by country
- Choose a start year and end year
- View total transactions, sales trends, year-over-year growth, and country contribution

## Prerequisites

- R installed on your machine
- RStudio (recommended) or VS Code with an R extension

## Install packages

This project includes a lockfile-based environment setup through renv, so that is the best option.

1. Open the project folder in RStudio or VS Code.
2. In an R console, run:

```r
install.packages("renv")
renv::restore()
```

If you prefer a minimal manual setup instead, install the required app packages directly:

```r
install.packages(c("shiny", "bslib"))
```

## Run the application

From the project root directory, start the app with:

```r
shiny::runApp("src/app.R")
```

If your working directory is already src, you can run:

```r
shiny::runApp("app.R")
```

The app will open in your browser (or in the RStudio viewer).

## Troubleshooting

- If you see package errors, run renv::restore() again.
- If data is not found, run the app from the project root so paths resolve correctly.
