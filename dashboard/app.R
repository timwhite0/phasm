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

pick_col <- function(df, candidates) {
  hit <- candidates[candidates %in% names(df)]
  if (length(hit) == 0) return(NULL)
  hit[[1]]
}

read_atc_pa <- function() {
  atc_path <- file.path(repo_root, "data", "atc_pa_projections_2026.csv")
  if (!file.exists(atc_path)) return(NULL)
  atc <- read_csv(atc_path, show_col_types = FALSE)
  id_col <- pick_col(atc, c("playerid", "PlayerId", "player_id"))
  pa_col <- pick_col(atc, c("PA", "pa"))
  if (is.null(id_col) || is.null(pa_col)) return(NULL)
  atc %>%
    transmute(
      playerid = as.character(.data[[id_col]]),
      PA_atc = as.numeric(.data[[pa_col]])
    ) %>%
    filter(!is.na(playerid), !is.na(PA_atc))
}

read_atc_ip <- function() {
  atc_path <- file.path(repo_root, "data", "atc_ip_projections_2026.csv")
  if (!file.exists(atc_path)) return(NULL)
  atc <- read_csv(atc_path, show_col_types = FALSE)
  id_col <- pick_col(atc, c("playerid", "PlayerId", "player_id"))
  ip_col <- pick_col(atc, c("IP", "ip"))
  if (is.null(id_col) || is.null(ip_col)) return(NULL)
  atc %>%
    transmute(
      playerid = as.character(.data[[id_col]]),
      IP_atc = as.numeric(.data[[ip_col]])
    ) %>%
    filter(!is.na(playerid), !is.na(IP_atc))
}

outcome_choices <- function(files, prefixes) {
  if (length(files) == 0) return(character(0))
  labels <- files
  for (p in prefixes) {
    labels <- sub(p, "", labels, fixed = TRUE)
  }
  labels <- sub("\\.pdf$", "", labels)
  labels <- gsub("_", " ", labels)
  labels <- gsub("BB9", "BB/9", labels)
  labels <- gsub("K9", "K/9", labels)
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
      menuItem("Hitter Projections", tabName = "hitter_projections", icon = icon("table")),
      menuItem("Starter Projections", tabName = "sp_projections", icon = icon("table")),
      menuItem("Reliever Projections", tabName = "rp_projections", icon = icon("table")),
      menuItem("Hitter Composite Rankings", tabName = "hitters", icon = icon("table")),
      menuItem("Starter Composite Rankings", tabName = "starters", icon = icon("table")),
      menuItem("Reliever Composite Rankings", tabName = "relievers", icon = icon("table"))
    )
  ),
  dashboardBody(
    tags$head(tags$style(HTML("
      .skin-blue .main-header .navbar,
      .skin-blue .main-header .logo {
        background-color: #667078 !important;
      }
      .skin-blue .main-header .logo:hover {
        background-color: #667078 !important;
      }
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
      ),
      tabItem(
        tabName = "hitter_projections",
        fluidRow(
          box(width = 12, uiOutput("hitter_proj_outcome_ui"))
        ),
        fluidRow(
          box(width = 12, uiOutput("hitter_proj_position_ui"))
        ),
        fluidRow(
          box(width = 12, DTOutput("hitter_proj_table"))
        )
      ),
      tabItem(
        tabName = "sp_projections",
        fluidRow(
          box(width = 12, uiOutput("sp_proj_outcome_ui"))
        ),
        fluidRow(
          box(width = 12, DTOutput("sp_proj_table"))
        )
      ),
      tabItem(
        tabName = "rp_projections",
        fluidRow(
          box(width = 12, uiOutput("rp_proj_outcome_ui"))
        ),
        fluidRow(
          box(width = 12, DTOutput("rp_proj_table"))
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
        tags$li("Produces 2026 projections and uncertainty intervals (5th/95th percentiles) for each outcome."),
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
        tags$li("role_leverage is a binary covariate that captures high-leverage usage."),
        tags$li("RP priors default to empirical-Bayes summaries from results/prior_predictive/rp_prior_summary.csv.")
      ),
      tags$div(style = "height: 12px;"),
      tags$h4(tags$strong("Empirical-Bayes prior flow (all models)")),
      tags$ul(
        tags$li("EB summaries are fit on 2013-2017 data for hitters, starters, and relievers."),
        tags$li("Default summary files are batter_prior_summary.csv, sp_prior_summary.csv, and rp_prior_summary.csv in results/prior_predictive/."),
        tags$li("Main fits use EB posterior means as prior centers and EB posterior sds as prior scales."),
        tags$li("If an EB summary is missing or invalid, that model falls back to legacy priors with a log message."),
        tags$li("Full details and file paths are in the GitHub README.")
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
        tags$li("Starter Plots: Fitted outcomes for W/QS plus derived ERA/K/9/BB/9/WHIP; interval projections by role."),
        tags$li("Reliever Plots: Fitted outcomes for W/SVHLD plus derived ERA/K/9/BB/9/WHIP; interval projections by role."),
        tags$li("Hitter Projections: Outcome-specific posterior mean and quantiles (with PA scaling for counts)."),
        tags$li("Starter Projections: Outcome-specific posterior mean and quantiles (with IP scaling)."),
        tags$li("Reliever Projections: Outcome-specific posterior mean and quantiles (with IP scaling)."),
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
    req(file, nzchar(file))
    subdir <- if (type == "Fitted outcomes") {
      file.path("plots", "fitted_outcome_curves", "batters")
    } else {
      file.path("plots", "interval_projections", "batters")
    }
    out <- render_plot_image(file, subdir)
    req(!is.null(out))
    out
  }, deleteFile = TRUE)

  output$hitter_position_ui <- renderUI({
    path <- file.path(results_root, "projections", "batters", "composite_projections_2026.csv")
    if (!file.exists(path)) return(NULL)
    df <- read_csv(path, show_col_types = FALSE)
    if (!"position" %in% names(df)) return(NULL)
    choices <- sort(unique(sub("/.*$", "", df$position)))
    selectInput("hitter_position", "Position", choices = c("All", choices), selected = "All")
  })

  output$hitter_proj_outcome_ui <- renderUI({
    path <- file.path(results_root, "projections", "batters", "category_projections_2026.csv")
    if (!file.exists(path)) return(NULL)
    df <- read_csv(path, show_col_types = FALSE)
    outcomes <- sub("_mean$", "", names(df)[grepl("_mean$", names(df))])
    outcomes <- outcomes[order(outcomes)]
    selectInput("hitter_proj_outcome", "Outcome", choices = outcomes)
  })

  output$hitter_proj_position_ui <- renderUI({
    path <- file.path(results_root, "projections", "batters", "category_projections_2026.csv")
    if (!file.exists(path)) return(NULL)
    df <- read_csv(path, show_col_types = FALSE)
    if (!"position" %in% names(df)) return(NULL)
    choices <- sort(unique(sub("/.*$", "", df$position)))
    selectInput("hitter_proj_position", "Position", choices = c("All", choices), selected = "All")
  })

  output$sp_proj_outcome_ui <- renderUI({
    choices <- c("ERA", "K/9", "WHIP", "BB/9", "W", "Ks", "QS")
    selectInput("sp_proj_outcome", "Outcome", choices = choices)
  })

  output$rp_proj_outcome_ui <- renderUI({
    choices <- c("ERA", "K/9", "WHIP", "BB/9", "W", "Ks", "SVHLD")
    selectInput("rp_proj_outcome", "Outcome", choices = choices)
  })

  output$sp_plot_image <- renderImage({
    type <- input$sp_plot_type
    file <- input$sp_outcome
    req(file, nzchar(file))
    subdir <- if (type == "Fitted outcomes") {
      file.path("plots", "fitted_outcome_curves", "pitchers", "starters")
    } else {
      file.path("plots", "interval_projections", "pitchers", "starters")
    }
    out <- render_plot_image(file, subdir)
    req(!is.null(out))
    out
  }, deleteFile = TRUE)

  output$rp_plot_image <- renderImage({
    type <- input$rp_plot_type
    file <- input$rp_outcome
    req(file, nzchar(file))
    subdir <- if (type == "Fitted outcomes") {
      file.path("plots", "fitted_outcome_curves", "pitchers", "relievers")
    } else {
      file.path("plots", "interval_projections", "pitchers", "relievers")
    }
    out <- render_plot_image(file, subdir)
    req(!is.null(out))
    out
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
      df <- df %>% rename(Position = position) %>% select(Name, Position, `Composite z-score`, everything())
    } else if (all(c("Name", "Composite z-score") %in% names(df))) {
      df <- df %>% select(Name, `Composite z-score`, everything())
    }
    if ("Composite z-score" %in% names(df)) {
      df <- df %>% mutate(.comp_raw = as.numeric(`Composite z-score`))
    }
    score_cols <- names(df)[grepl("z-score$", names(df))]
    if (!is.null(input$hitter_position) &&
        input$hitter_position != "All" &&
        "Position" %in% names(df)) {
      df <- df %>% filter(Position == input$hitter_position)
    }
    if ("Composite z-score" %in% names(df) && ".comp_raw" %in% names(df)) {
      df <- df %>% mutate(Rank = dense_rank(desc(.comp_raw)))
      df <- df %>% select(Rank, everything(), - .comp_raw)
    }
    default_order <- if ("Composite z-score" %in% names(df)) {
      list(list(which(names(df) == "Composite z-score") - 1L, "desc"))
    } else {
      list()
    }
    dt <- datatable(df, options = list(pageLength = 25, scrollX = TRUE, order = default_order), rownames = FALSE)
    if (length(score_cols) > 0) {
      dt <- formatRound(dt, columns = score_cols, digits = 2)
    }
    dt
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
    if ("Composite z-score" %in% names(df)) {
      df <- df %>% mutate(.comp_raw = as.numeric(`Composite z-score`))
    }
    score_cols <- names(df)[grepl("z-score$", names(df))]
    if ("Composite z-score" %in% names(df) && ".comp_raw" %in% names(df)) {
      df <- df %>% mutate(Rank = dense_rank(desc(.comp_raw)))
      df <- df %>% select(Rank, everything(), - .comp_raw)
    }
    default_order <- if ("Composite z-score" %in% names(df)) {
      list(list(which(names(df) == "Composite z-score") - 1L, "desc"))
    } else {
      list()
    }
    dt <- datatable(df, options = list(pageLength = 25, scrollX = TRUE, order = default_order), rownames = FALSE)
    if (length(score_cols) > 0) {
      dt <- formatRound(dt, columns = score_cols, digits = 2)
    }
    dt
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
    if ("Composite z-score" %in% names(df)) {
      df <- df %>% mutate(.comp_raw = as.numeric(`Composite z-score`))
    }
    score_cols <- names(df)[grepl("z-score$", names(df))]
    if ("Composite z-score" %in% names(df) && ".comp_raw" %in% names(df)) {
      df <- df %>% mutate(Rank = dense_rank(desc(.comp_raw)))
      df <- df %>% select(Rank, everything(), - .comp_raw)
    }
    default_order <- if ("Composite z-score" %in% names(df)) {
      list(list(which(names(df) == "Composite z-score") - 1L, "desc"))
    } else {
      list()
    }
    dt <- datatable(df, options = list(pageLength = 25, scrollX = TRUE, order = default_order), rownames = FALSE)
    if (length(score_cols) > 0) {
      dt <- formatRound(dt, columns = score_cols, digits = 2)
    }
    dt
  })

  output$hitter_proj_table <- renderDT({
    req(input$hitter_proj_outcome)
    path <- file.path(results_root, "projections", "batters", "category_projections_2026.csv")
    if (!file.exists(path)) return(NULL)
    df <- read_csv(path, show_col_types = FALSE)
    outcome <- input$hitter_proj_outcome
    mean_col <- paste0(outcome, "_mean")
    p05_col <- paste0(outcome, "_p05")
    p95_col <- paste0(outcome, "_p95")
    if (!all(c(mean_col, p05_col, p95_col) %in% names(df))) return(NULL)
    df <- df %>% mutate(playerid = as.character(playerid))
    if ("position" %in% names(df)) {
      df <- df %>% mutate(Position = sub("/.*$", "", position))
    }
    count_outcomes <- c("H", "R", "RBI", "HR", "SB")
    if (outcome %in% count_outcomes) {
      mean_col <- paste0(outcome, "_mean_t")
      p05_col <- paste0(outcome, "_p05_t")
      p95_col <- paste0(outcome, "_p95_t")
    }
    if (!all(c(mean_col, p05_col, p95_col) %in% names(df))) return(NULL)
    df <- df %>%
      transmute(
        Name = PlayerName,
        Position = if ("Position" %in% names(df)) Position else NA_character_,
        `0.05 quantile` = .data[[p05_col]],
        `Posterior mean` = .data[[mean_col]],
        `0.95 quantile` = .data[[p95_col]]
      )
    if (!is.null(input$hitter_proj_position) &&
        input$hitter_proj_position != "All" &&
        "Position" %in% names(df)) {
      df <- df %>% filter(Position == input$hitter_proj_position)
    }
    if ("Position" %in% names(df)) {
      df <- df %>% select(Name, Position, `0.05 quantile`, `Posterior mean`, `0.95 quantile`)
    }
    if (outcome %in% c("AVG", "OBP", "SLG")) {
      df <- df %>%
        mutate(
          `0.05 quantile` = formatC(as.numeric(`0.05 quantile`), format = "f", digits = 3),
          `Posterior mean` = formatC(as.numeric(`Posterior mean`), format = "f", digits = 3),
          `0.95 quantile` = formatC(as.numeric(`0.95 quantile`), format = "f", digits = 3)
        )
    } else {
      df <- df %>%
        mutate(
          `0.05 quantile` = round(as.numeric(`0.05 quantile`), 0),
          `Posterior mean` = round(as.numeric(`Posterior mean`), 0),
          `0.95 quantile` = round(as.numeric(`0.95 quantile`), 0)
        )
    }
    order_dir <- if (outcome %in% c("ERA", "WHIP", "BB/9")) "asc" else "desc"
    default_order <- list(list(which(names(df) == "Posterior mean") - 1L, order_dir))
    datatable(df, options = list(pageLength = 25, scrollX = TRUE, order = default_order), rownames = FALSE)
  })

  output$sp_proj_table <- renderDT({
    req(input$sp_proj_outcome)
    path <- file.path(results_root, "projections", "pitchers", "sp_category_projections_2026.csv")
    if (!file.exists(path)) return(NULL)
    df <- read_csv(path, show_col_types = FALSE)
    outcome <- input$sp_proj_outcome
    needed <- c(
      "ERA_mean", "ERA_p05", "ERA_p95",
      "K9_mean", "K9_p05", "K9_p95",
      "WHIP_mean", "WHIP_p05", "WHIP_p95",
      "BB9_mean", "BB9_p05", "BB9_p95",
      "Ks_mean", "Ks_p05", "Ks_p95",
      "W_mean_t", "W_p05_t", "W_p95_t",
      "QS_mean_t", "QS_p05_t", "QS_p95_t"
    )
    if (!all(needed %in% names(df))) return(NULL)

    map <- list(
      "ERA" = c("ERA_mean", "ERA_p05", "ERA_p95"),
      "K/9" = c("K9_mean", "K9_p05", "K9_p95"),
      "WHIP" = c("WHIP_mean", "WHIP_p05", "WHIP_p95"),
      "BB/9" = c("BB9_mean", "BB9_p05", "BB9_p95"),
      "W" = c("W_mean_t", "W_p05_t", "W_p95_t"),
      "Ks" = c("Ks_mean", "Ks_p05", "Ks_p95"),
      "QS" = c("QS_mean_t", "QS_p05_t", "QS_p95_t")
    )
    cols <- map[[outcome]]
    df <- df %>%
      transmute(
        Name = PlayerName,
        `0.05 quantile` = .data[[cols[2]]],
        `Posterior mean` = .data[[cols[1]]],
        `0.95 quantile` = .data[[cols[3]]]
      )
    order_dir <- if (outcome %in% c("ERA", "WHIP", "BB/9")) "asc" else "desc"
    default_order <- list(list(which(names(df) == "Posterior mean") - 1L, order_dir))
    dt <- datatable(df, options = list(pageLength = 25, scrollX = TRUE, order = default_order), rownames = FALSE)
    if (outcome %in% c("ERA", "K/9", "WHIP", "BB/9")) {
      dt <- formatRound(dt, columns = c("0.05 quantile", "Posterior mean", "0.95 quantile"), digits = 2)
    } else {
      dt <- formatRound(dt, columns = c("0.05 quantile", "Posterior mean", "0.95 quantile"), digits = 0)
    }
    dt
  })

  output$rp_proj_table <- renderDT({
    req(input$rp_proj_outcome)
    path <- file.path(results_root, "projections", "pitchers", "rp_category_projections_2026.csv")
    if (!file.exists(path)) return(NULL)
    df <- read_csv(path, show_col_types = FALSE)
    outcome <- input$rp_proj_outcome
    needed <- c(
      "ERA_mean", "ERA_p05", "ERA_p95",
      "K9_mean", "K9_p05", "K9_p95",
      "WHIP_mean", "WHIP_p05", "WHIP_p95",
      "BB9_mean", "BB9_p05", "BB9_p95",
      "Ks_mean", "Ks_p05", "Ks_p95",
      "W_mean_t", "W_p05_t", "W_p95_t",
      "SVHLD_mean_t", "SVHLD_p05_t", "SVHLD_p95_t"
    )
    if (!all(needed %in% names(df))) return(NULL)

    map <- list(
      "ERA" = c("ERA_mean", "ERA_p05", "ERA_p95"),
      "K/9" = c("K9_mean", "K9_p05", "K9_p95"),
      "WHIP" = c("WHIP_mean", "WHIP_p05", "WHIP_p95"),
      "BB/9" = c("BB9_mean", "BB9_p05", "BB9_p95"),
      "W" = c("W_mean_t", "W_p05_t", "W_p95_t"),
      "Ks" = c("Ks_mean", "Ks_p05", "Ks_p95"),
      "SVHLD" = c("SVHLD_mean_t", "SVHLD_p05_t", "SVHLD_p95_t")
    )
    cols <- map[[outcome]]
    df <- df %>%
      transmute(
        Name = PlayerName,
        `0.05 quantile` = .data[[cols[2]]],
        `Posterior mean` = .data[[cols[1]]],
        `0.95 quantile` = .data[[cols[3]]]
      )
    order_dir <- if (outcome %in% c("ERA", "WHIP", "BB/9")) "asc" else "desc"
    default_order <- list(list(which(names(df) == "Posterior mean") - 1L, order_dir))
    dt <- datatable(df, options = list(pageLength = 25, scrollX = TRUE, order = default_order), rownames = FALSE)
    if (outcome %in% c("ERA", "K/9", "WHIP", "BB/9")) {
      dt <- formatRound(dt, columns = c("0.05 quantile", "Posterior mean", "0.95 quantile"), digits = 2)
    } else {
      dt <- formatRound(dt, columns = c("0.05 quantile", "Posterior mean", "0.95 quantile"), digits = 0)
    }
    dt
  })
}

shinyApp(ui, server)
