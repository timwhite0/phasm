suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(dplyr)
  library(readr)
  library(DT)
  library(markdown)
  library(pdftools)
  library(png)
})

app_root <- getwd()
repo_root <- if (dir.exists(file.path(app_root, "results"))) {
  app_root
} else {
  normalizePath(file.path(app_root, ".."), winslash = "/", mustWork = FALSE)
}
pick_existing <- function(path_suffix) {
  paths <- c(file.path(app_root, path_suffix), file.path(repo_root, path_suffix))
  idx <- which(dir.exists(paths) | file.exists(paths))
  if (length(idx) == 0) return(NA_character_)
  paths[idx[1]]
}
results_root <- pick_existing("results")
if (!is.na(results_root) && dir.exists(results_root)) {
  addResourcePath("results", results_root)
}

plot_roots <- list(
  fitted_batters = file.path(results_root, "plots", "fitted_outcome_curves", "batters"),
  fitted_pitchers_sp = file.path(results_root, "plots", "fitted_outcome_curves", "pitchers", "starters"),
  fitted_pitchers_rp = file.path(results_root, "plots", "fitted_outcome_curves", "pitchers", "relievers"),
  interval_batters = file.path(results_root, "plots", "interval_projections", "batters"),
  interval_pitchers_sp = file.path(results_root, "plots", "interval_projections", "pitchers", "starters"),
  interval_pitchers_rp = file.path(results_root, "plots", "interval_projections", "pitchers", "relievers")
)

list_pdfs <- function(path) {
  if (!dir.exists(path)) return(character(0))
  files <- list.files(path, pattern = "\\.pdf$", full.names = FALSE)
  files[order(files)]
}

outcome_choices <- function(files, prefixes) {
  if (length(files) == 0) return(character(0))
  labels <- files
  for (p in prefixes) {
    labels <- sub(p, "", labels, fixed = TRUE)
  }
  labels <- sub("\\.pdf$", "", labels)
  labels <- gsub("_", " ", labels)
  names(files) <- labels
  files
}

md_table <- function(path, section) {
  if (!file.exists(path)) return(NULL)
  lines <- readLines(path, warn = FALSE)
  start <- which(startsWith(lines, section))
  if (length(start) == 0) return(NULL)
  # find header line with "| Rank |" after section
  idx <- which(grepl("Rank | Player", lines, fixed = TRUE))
  idx <- idx[idx > start][1]
  if (is.na(idx)) return(NULL)
  data_lines <- lines[(idx + 2):length(lines)]
  end <- which(data_lines == "")
  if (length(end) > 0) data_lines <- data_lines[1:(end[1] - 1)]
  data_lines <- data_lines[startsWith(data_lines, "|")]
  if (length(data_lines) == 0) return(NULL)

  parts <- lapply(data_lines, function(s) {
    trimws(strsplit(s, "|", fixed = TRUE)[[1]])
  })
  mat <- do.call(rbind, lapply(parts, function(p) p[2:(length(p) - 1)]))
  # header is in the line idx
  header_parts <- trimws(strsplit(lines[idx], "|", fixed = TRUE)[[1]])
  header <- header_parts[2:(length(header_parts) - 1)]
  df <- as.data.frame(mat, stringsAsFactors = FALSE)
  names(df) <- header
  df
}

ui <- dashboardPage(
  dashboardHeader(title = "PHASM"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("info-circle")),
      menuItem("Hitter Plots", tabName = "plots_hitters", icon = icon("chart-line")),
      menuItem("Starter Plots", tabName = "plots_sp", icon = icon("chart-line")),
      menuItem("Reliever Plots", tabName = "plots_rp", icon = icon("chart-line")),
      menuItem("Composite Hitter Rankings", tabName = "hitters", icon = icon("table")),
      menuItem("Composite Starter Rankings", tabName = "starters", icon = icon("table")),
      menuItem("Composite Reliever Rankings", tabName = "relievers", icon = icon("table"))
    )
  ),
  dashboardBody(
    tags$head(tags$style(HTML("
      .plot-controls .form-group {
        margin-bottom: 0;
      }
      .shiny-image-output img {
        max-width: 100% !important;
        height: auto !important;
        display: block;
      }
    "))),
    tabItems(
      tabItem(
        tabName = "overview",
        fluidRow(
          box(width = 12, uiOutput("readme_html"))
        )
      ),
      tabItem(
        tabName = "plots_hitters",
        fluidRow(
          box(
            width = 12,
            div(
              class = "plot-controls",
              fluidRow(
                column(4, selectInput("hitter_plot_type", "Plot type", choices = c("Fitted outcomes", "Interval projections"))),
                column(8, uiOutput("hitter_outcome_ui"))
              )
            )
          )
        ),
        fluidRow(
          box(
            width = 12,
            imageOutput("hitter_plot_image", height = "auto", width = "100%")
          )
        )
      ),
      tabItem(
        tabName = "plots_sp",
        fluidRow(
          box(
            width = 12,
            div(
              class = "plot-controls",
              fluidRow(
                column(4, selectInput("sp_plot_type", "Plot type", choices = c("Fitted outcomes", "Interval projections"))),
                column(8, uiOutput("sp_outcome_ui"))
              )
            )
          )
        ),
        fluidRow(
          box(
            width = 12,
            imageOutput("sp_plot_image", height = "auto", width = "100%")
          )
        )
      ),
      tabItem(
        tabName = "plots_rp",
        fluidRow(
          box(
            width = 12,
            div(
              class = "plot-controls",
              fluidRow(
                column(4, selectInput("rp_plot_type", "Plot type", choices = c("Fitted outcomes", "Interval projections"))),
                column(8, uiOutput("rp_outcome_ui"))
              )
            )
          )
        ),
        fluidRow(
          box(
            width = 12,
            imageOutput("rp_plot_image", height = "auto", width = "100%")
          )
        )
      ),
      tabItem(
        tabName = "hitters",
        fluidRow(
          box(width = 12, uiOutput("hitter_position_ui"))
        ),
        fluidRow(
          box(width = 12, DTOutput("batters_table"))
        )
      ),
      tabItem(
        tabName = "starters",
        fluidRow(
          box(width = 12, DTOutput("sp_table"))
        )
      ),
      tabItem(
        tabName = "relievers",
        fluidRow(
          box(width = 12, DTOutput("rp_table"))
        )
      )
    )
  )
)

server <- function(input, output, session) {
  output$readme_html <- renderUI({
    tagList(
      tags$h2(tags$strong("PHASM")),
      tags$h3(tags$strong("Probabilistic hierarchical autoregressive sabermetric model")),
      tags$div(style = "height: 12px;"),
      tags$p(
        "PHASM generates probabilistic projections for hitters, starters, and relievers by",
        "combining 2018–2025 performance data with hierarchical, partially pooled effects.",
        "Outputs include outcome-specific projections, composite z-scores, and uncertainty intervals",
        "for the 2026 season."
      ),
      tags$div(style = "height: 12px;"),
      tags$h4(tags$strong("What this does")),
      tags$ul(
        tags$li("Estimates latent player skill trajectories over time with shared year effects."),
        tags$li("Produces 2026 projections and uncertainty intervals for each outcome."),
        tags$li("Builds composite rankings from category z-scores for hitters, starters, and relievers.")
      ),
      tags$div(style = "height: 12px;"),
      tags$h4(tags$strong("Covariates used")),
      tags$ul(
        tags$li("Hitter position indicators."),
        tags$li("Pitcher role indicators (SP/RP) with separate models."),
        tags$li("Reliever leverage indicator (role_leverage)."),
        tags$li("Year effects shared across players.")
      ),
      tags$div(style = "height: 12px;"),
      tags$h4(tags$strong("SP model notes")),
      tags$ul(
        tags$li("Starter outcomes include SO, BB, H, ER, W, and QS."),
        tags$li("Composite starter rankings use z-scores, including Ks (not K/9).")
      ),
      tags$div(style = "height: 12px;"),
      tags$h4(tags$strong("RP model notes")),
      tags$ul(
        tags$li("Reliever outcomes include SO, BB, H, ER, W, and SVHLD."),
        tags$li("role_leverage is a binary covariate that captures high-leverage usage.")
      ),
      tags$div(style = "height: 12px;"),
      tags$p(
        tags$strong("Full model specification: "),
        "Documented in the README on GitHub: ",
        tags$a(
          href = "https://github.com/timwhite0/phasm#readme",
          "https://github.com/timwhite0/phasm#readme",
          target = "_blank",
          rel = "noopener noreferrer"
        )
      ),
      tags$div(style = "height: 12px;"),
      tags$h4(tags$strong("Tabs in this app")),
      tags$ul(
        tags$li("Overview: This summary of the model and app contents."),
        tags$li("Hitter Plots: Fitted outcomes and interval projections for hitters."),
        tags$li("Starter Plots: Fitted outcomes and interval projections for starters."),
        tags$li("Reliever Plots: Fitted outcomes and interval projections for relievers."),
        tags$li("Composite Hitter Rankings: Sortable composite z-score table for hitters."),
        tags$li("Composite Starter Rankings: Sortable composite z-score table for starters."),
        tags$li("Composite Reliever Rankings: Sortable composite z-score table for relievers.")
      )
    )
  })

  output$hitter_outcome_ui <- renderUI({
    type <- input$hitter_plot_type
    if (type == "Fitted outcomes") {
      files <- list_pdfs(plot_roots$fitted_batters)
      choices <- outcome_choices(files, c("latent_fit_top100_"))
      selectInput("hitter_outcome", "Outcome", choices = choices)
    } else {
      files <- list_pdfs(plot_roots$interval_batters)
      choices <- outcome_choices(files, c("projection_intervals_2026_"))
      selectInput("hitter_outcome", "Position", choices = choices)
    }
  })

  output$sp_outcome_ui <- renderUI({
    type <- input$sp_plot_type
    if (type == "Fitted outcomes") {
      files <- list_pdfs(plot_roots$fitted_pitchers_sp)
      choices <- outcome_choices(files, c("sp_latent_fit_top100_", "sp_latent_fit_derived_"))
      selectInput("sp_outcome", "Outcome", choices = choices)
    } else {
      choices <- c("Projection intervals" = "sp_intervals_2026_SP.pdf")
      selectInput("sp_outcome", "Position", choices = c("SP" = "sp_intervals_2026_SP.pdf"))
    }
  })

  output$rp_outcome_ui <- renderUI({
    type <- input$rp_plot_type
    if (type == "Fitted outcomes") {
      files <- list_pdfs(plot_roots$fitted_pitchers_rp)
      choices <- outcome_choices(files, c("rp_latent_fit_top100_", "rp_latent_fit_derived_"))
      selectInput("rp_outcome", "Outcome", choices = choices)
    } else {
      choices <- c("Projection intervals" = "rp_intervals_2026_RP.pdf")
      selectInput("rp_outcome", "Position", choices = c("RP" = "rp_intervals_2026_RP.pdf"))
    }
  })

render_plot_image <- function(file, subdir) {
  if (is.null(file) || file == "") return(NULL)
  pdf_path <- file.path(results_root, subdir, file)
  if (!file.exists(pdf_path)) return(NULL)
  png_path <- tempfile(fileext = ".png")
  tryCatch({
    img <- pdftools::pdf_render_page(pdf_path, page = 1, dpi = 200)
    png::writePNG(img, png_path)
    list(src = png_path, contentType = "image/png")
  }, error = function(e) {
    NULL
  })
}

  output$hitter_plot_image <- renderImage({
    type <- input$hitter_plot_type
    file <- input$hitter_outcome
    subdir <- if (type == "Fitted outcomes") {
      file.path("plots", "fitted_outcome_curves", "batters")
    } else {
      file.path("plots", "interval_projections", "batters")
    }
    render_plot_image(file, subdir)
  }, deleteFile = TRUE)

  output$hitter_position_ui <- renderUI({
    path <- file.path(results_root, "projections", "batters", "composite_projections_2026.csv")
    if (!file.exists(path)) return(NULL)
    df <- read_csv(path, show_col_types = FALSE)
    if (!"position" %in% names(df)) return(NULL)
    choices <- sort(unique(sub("/.*$", "", df$position)))
    selectInput("hitter_position", "Position", choices = c("All", choices), selected = "All")
  })

  output$sp_plot_image <- renderImage({
    type <- input$sp_plot_type
    file <- input$sp_outcome
    subdir <- if (type == "Fitted outcomes") {
      file.path("plots", "fitted_outcome_curves", "pitchers", "starters")
    } else {
      file.path("plots", "interval_projections", "pitchers", "starters")
    }
    render_plot_image(file, subdir)
  }, deleteFile = TRUE)

  output$rp_plot_image <- renderImage({
    type <- input$rp_plot_type
    file <- input$rp_outcome
    subdir <- if (type == "Fitted outcomes") {
      file.path("plots", "fitted_outcome_curves", "pitchers", "relievers")
    } else {
      file.path("plots", "interval_projections", "pitchers", "relievers")
    }
    render_plot_image(file, subdir)
  }, deleteFile = TRUE)

  output$batters_table <- renderDT({
    path <- file.path(results_root, "projections", "batters", "composite_projections_2026.csv")
    if (!file.exists(path)) return(NULL)
    df <- read_csv(path, show_col_types = FALSE)
    if ("playerid" %in% names(df)) df <- df %>% select(-playerid)
    if ("PlayerName" %in% names(df)) df <- df %>% rename(Name = PlayerName)
    if ("PA_atc" %in% names(df)) df <- df %>% select(-PA_atc)
    if ("composite_zscore" %in% names(df)) df <- df %>% rename(`Composite z-score` = composite_zscore)
    z_cols <- names(df)[startsWith(names(df), "z_")]
    if (length(z_cols) > 0) {
      new_names <- sub("^z_", "", z_cols)
      new_names <- paste0(new_names, " z-score")
      names(df)[match(z_cols, names(df))] <- new_names
    }
    if ("position" %in% names(df)) {
      df <- df %>% mutate(position = sub("/.*$", "", position))
    }
    if (all(c("Name", "position", "Composite z-score") %in% names(df))) {
      df <- df %>% select(Name, position, `Composite z-score`, everything())
    } else if (all(c("Name", "Composite z-score") %in% names(df))) {
      df <- df %>% select(Name, `Composite z-score`, everything())
    }
    if (!is.null(input$hitter_position) &&
        input$hitter_position != "All" &&
        "position" %in% names(df)) {
      df <- df %>% filter(position == input$hitter_position)
    }
    score_cols <- names(df)[grepl("z-score$", names(df))]
    if (length(score_cols) > 0) {
      df[score_cols] <- lapply(df[score_cols], function(x) round(as.numeric(x), 2))
    }
    default_order <- if ("Composite z-score" %in% names(df)) {
      list(list(which(names(df) == "Composite z-score") - 1L, "desc"))
    } else {
      list()
    }
    datatable(df, options = list(pageLength = 25, scrollX = TRUE, order = default_order), rownames = FALSE)
  })

  output$sp_table <- renderDT({
    path <- file.path(results_root, "projections", "pitchers", "sp_composite_projections_2026.csv")
    if (!file.exists(path)) return(NULL)
    df <- read_csv(path, show_col_types = FALSE)
    if ("playerid" %in% names(df)) df <- df %>% select(-playerid)
    if ("role" %in% names(df)) df <- df %>% select(-role)
    if ("PlayerName" %in% names(df)) df <- df %>% rename(Name = PlayerName)
    if ("composite" %in% names(df)) df <- df %>% rename(`Composite z-score` = composite)
    z_cols <- names(df)[startsWith(names(df), "z_")]
    if (length(z_cols) > 0) {
      new_names <- sub("^z_", "", z_cols)
      new_names <- paste0(new_names, " z-score")
      names(df)[match(z_cols, names(df))] <- new_names
    }
    # keep only name + z-scores for pitchers
    keep_cols <- c("Name", "Composite z-score", names(df)[grepl("z-score$", names(df))])
    keep_cols <- keep_cols[keep_cols %in% names(df)]
    df <- df %>% select(all_of(keep_cols))
    if (all(c("Name", "Composite z-score") %in% names(df))) {
      df <- df %>% select(Name, `Composite z-score`, everything())
    }
    score_cols <- names(df)[grepl("z-score$", names(df))]
    if (length(score_cols) > 0) {
      df[score_cols] <- lapply(df[score_cols], function(x) round(as.numeric(x), 2))
    }
    default_order <- if ("Composite z-score" %in% names(df)) {
      list(list(which(names(df) == "Composite z-score") - 1L, "desc"))
    } else {
      list()
    }
    datatable(df, options = list(pageLength = 25, scrollX = TRUE, order = default_order), rownames = FALSE)
  })

  output$rp_table <- renderDT({
    path <- file.path(results_root, "projections", "pitchers", "rp_composite_projections_2026.csv")
    if (!file.exists(path)) return(NULL)
    df <- read_csv(path, show_col_types = FALSE)
    if ("playerid" %in% names(df)) df <- df %>% select(-playerid)
    if ("role" %in% names(df)) df <- df %>% select(-role)
    if ("PlayerName" %in% names(df)) df <- df %>% rename(Name = PlayerName)
    if ("composite" %in% names(df)) df <- df %>% rename(`Composite z-score` = composite)
    z_cols <- names(df)[startsWith(names(df), "z_")]
    if (length(z_cols) > 0) {
      new_names <- sub("^z_", "", z_cols)
      new_names <- paste0(new_names, " z-score")
      names(df)[match(z_cols, names(df))] <- new_names
    }
    # keep only name + z-scores for pitchers
    keep_cols <- c("Name", "Composite z-score", names(df)[grepl("z-score$", names(df))])
    keep_cols <- keep_cols[keep_cols %in% names(df)]
    df <- df %>% select(all_of(keep_cols))
    if (all(c("Name", "Composite z-score") %in% names(df))) {
      df <- df %>% select(Name, `Composite z-score`, everything())
    }
    score_cols <- names(df)[grepl("z-score$", names(df))]
    if (length(score_cols) > 0) {
      df[score_cols] <- lapply(df[score_cols], function(x) round(as.numeric(x), 2))
    }
    default_order <- if ("Composite z-score" %in% names(df)) {
      list(list(which(names(df) == "Composite z-score") - 1L, "desc"))
    } else {
      list()
    }
    datatable(df, options = list(pageLength = 25, scrollX = TRUE, order = default_order), rownames = FALSE)
  })
}

shinyApp(ui, server)
