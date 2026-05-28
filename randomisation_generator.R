# ══════════════════════════════════════════════════════════════════════════════
# randomisation_generator.R — WebTrials
#
# Shiny module: UI + Server for admin randomisation file generator.
# All randomisation METHODS are defined in randomisation_methods.R
# ══════════════════════════════════════════════════════════════════════════════

library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
library(ggplot2)
library(rdrop2)

source("randomisation_methods.R")   # loads RAND_METHODS registry + functions

# ── Build rand-*.csv data.frame ───────────────────────────────────────────────
build_rand_df <- function(site, sequence) {
  data.frame(site          = site,
             Randomization = seq_along(sequence),
             Treatment     = sequence,
             stringsAsFactors = FALSE)
}

# ── Upload rand-*.csv to Dropbox ──────────────────────────────────────────────
save_rand_file <- function(df, site, drop_folder) {
  filename <- paste0("rand-", site, ".csv")
  write.csv(df, filename, row.names = FALSE)
  drop_upload(filename, path = drop_folder)
  unlink(filename)
  message("[RAND] Uploaded: ", drop_folder, "/", filename)
  invisible(TRUE)
}

# ══════════════════════════════════════════════════════════════════════════════
# UI MODULE
# ══════════════════════════════════════════════════════════════════════════════
randGenUI <- function(id) {
  ns <- NS(id)
  method_choices <- setNames(names(RAND_METHODS),
                              sapply(RAND_METHODS, `[[`, "label"))
  tagList(
    tags$style(HTML("
      .rg-card  { background:var(--surface); border:1px solid var(--border);
                  border-radius:10px; padding:22px 24px; margin-bottom:18px;
                  box-shadow:0 1px 4px rgba(0,0,0,.07); color:var(--text); }
      .rg-title { font-size:13px; font-weight:700; text-transform:uppercase;
                  letter-spacing:.5px; color:var(--teal); margin-bottom:14px; }
      .rg-badge { display:inline-block; padding:3px 10px; border-radius:99px;
                  font-size:11px; font-weight:600; margin-right:4px; }
      .rg-info  { background:#e0f2fe; color:#075985; }
      .rg-warn  { background:#fef3c7; color:#92400e; }
      .rg-ok    { background:#dcfce7; color:#166534; }
      .method-desc { font-size:13px; color:var(--text-muted); line-height:1.6;
                     background:var(--surface-2);
                     border-left:3px solid var(--teal);
                     padding:10px 14px; border-radius:0 8px 8px 0;
                     margin:10px 0 4px 0; }
      .method-ref  { font-size:11px; color:var(--text-muted);
                     font-style:italic; margin-bottom:12px; }
      .compliance-banner { background:linear-gradient(135deg,#0f2744,#1a3a5c);
                           color:#e0f2fe; border-radius:8px;
                           padding:10px 16px; font-size:11px;
                           margin-bottom:16px; }
      .compliance-banner b { color:#5eead4; }
    ")),

    div(class="compliance-banner",
      icon("shield-halved"), " ",
      tags$b("ICH E9 / ICH E6(R3): "),
      "Sequences generated server-side with reproducible seed. ",
      "All parameters recorded in audit trail. ",
      tags$b("Edit methods in: "), tags$code("randomisation_methods.R")
    ),

    fluidRow(

      # ── LEFT: Parameters ─────────────────────────────────────────────────
      column(5,
        div(class="rg-card",
          div(class="rg-title", icon("sliders"), " Parameters"),

          checkboxGroupInput(ns("sites"), "Sites:",
            choices  = c("Site A"="a", "Site B"="b", "Site C"="c"),
            selected = c("a","b","c"), inline = TRUE),

          hr(style="border-color:var(--border); margin:12px 0;"),

          fluidRow(
            column(6, numericInput(ns("n_arms"), "Number of arms:", 2, min=2, max=6)),
            column(6, numericInput(ns("n_per_site"), "N per site:", 100, min=10, max=2000, step=10))
          ),

          div(style="margin-bottom:12px;",
            tags$label(class="control-label", "Treatment names"),
            uiOutput(ns("trt_inputs"))
          ),

          hr(style="border-color:var(--border); margin:12px 0;"),

          selectInput(ns("method"), "Randomisation Method:", choices = method_choices),
          uiOutput(ns("method_info")),
          uiOutput(ns("method_params")),

          hr(style="border-color:var(--border); margin:12px 0;"),

          fluidRow(
            column(8, numericInput(ns("seed"), "Random seed:", 42, min=1, max=999999)),
            column(4, br(), checkboxInput(ns("use_seed"), "Use seed", TRUE))
          ),
          tags$small(style="color:var(--text-muted);",
            "Same seed + same parameters = identical sequence (reproducibility, ICH E9 §2.3)"),

          br(), br(),
          actionButton(ns("btn_preview"), icon("eye"),
            label = " Preview",
            style = "background:var(--teal);color:#fff;border:none;
                     font-weight:600;padding:9px 18px;border-radius:6px;"),
          tags$span(" "),
          actionButton(ns("btn_generate"), icon("upload"),
            label = " Generate & Upload",
            style = "background:var(--navy);color:#fff;border:none;
                     font-weight:600;padding:9px 18px;border-radius:6px;"),

          uiOutput(ns("gen_msg"))
        )
      ),

      # ── RIGHT: Preview + Summary ─────────────────────────────────────────
      column(7,
        div(class="rg-card",
          div(class="rg-title", icon("table"), " Preview"),
          uiOutput(ns("summary_badges")),
          DT::dataTableOutput(ns("preview_tbl"))
        ),
        div(class="rg-card",
          div(class="rg-title", icon("chart-bar"), " Allocation"),
          plotOutput(ns("alloc_plot"), height="220px")
        )
      )
    )
  )
}

# ══════════════════════════════════════════════════════════════════════════════
# SERVER MODULE
# ══════════════════════════════════════════════════════════════════════════════
randGenServer <- function(id, actor, drop_folder, append_audit_fn) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Dynamic treatment name inputs ────────────────────────────────────────
    output$trt_inputs <- renderUI({
      n <- max(2L, min(6L, as.integer(input$n_arms) %||% 2L))
      defaults <- paste0("Treatment - ", LETTERS[1:6])
      lapply(seq_len(n), function(i)
        textInput(ns(paste0("trt_", i)), label=NULL,
                  value=defaults[i], placeholder=defaults[i]))
    })

    get_treatments <- reactive({
      n <- max(2L, min(6L, as.integer(input$n_arms) %||% 2L))
      trts <- sapply(seq_len(n), function(i) {
        v <- input[[paste0("trt_", i)]]
        if (is.null(v) || trimws(v)=="") paste0("Treatment - ", LETTERS[i])
        else trimws(v)
      })
      unique(trts)
    })

    # ── Method description + reference ───────────────────────────────────────
    output$method_info <- renderUI({
      m <- RAND_METHODS[[input$method]]
      if (is.null(m)) return(NULL)
      tagList(
        div(class="method-desc", m$description),
        div(class="method-ref", icon("book-open"), " ", m$reference)
      )
    })

    # ── Dynamic method-specific parameters ────────────────────────────────────
    output$method_params <- renderUI({
      m <- RAND_METHODS[[input$method]]
      if (is.null(m)) return(NULL)
      m$ui_fn(ns, get_treatments())
    })

    # ── Core: generate sequences for all selected sites ───────────────────────
    do_generate <- reactive({
      req(input$sites, get_treatments(), input$method)
      m    <- RAND_METHODS[[input$method]]; req(m)
      trts <- get_treatments()
      n    <- as.integer(input$n_per_site) %||% 100L

      if (isTRUE(input$use_seed)) set.seed(as.integer(input$seed) %||% 42L)

      params <- tryCatch(m$get_params(input, trts), error=function(e) list())

      result <- list()
      for (site in input$sites) {
        seq_out <- m$fn(n, trts, params)
        result[[site]] <- build_rand_df(site, seq_out)
      }
      result
    })

    # ── Reactively update preview on input changes ────────────────────────────
    preview_data <- reactiveVal(NULL)

    observeEvent(input$btn_preview, {
      d <- tryCatch(do_generate(), error=function(e){
        showNotification(paste("Error:", e$message), type="error"); NULL
      })
      preview_data(d)
    })

    # Auto-preview (debounced 600ms)
    observe({
      input$method; input$n_per_site; input$n_arms; input$sites
      input$seed; input$use_seed
      invalidateLater(600)
      isolate({
        d <- tryCatch(do_generate(), error=function(e) NULL)
        preview_data(d)
      })
    })

    # ── Summary badges ────────────────────────────────────────────────────────
    output$summary_badges <- renderUI({
      d <- preview_data(); if (is.null(d)) return(NULL)
      combined <- bind_rows(d)
      tagList(
        tags$span(class="rg-badge rg-info", paste(length(d), "site(s)")),
        tags$span(class="rg-badge rg-ok",   paste(nrow(combined), "total slots")),
        tags$span(class="rg-badge rg-warn", paste(length(unique(combined$Treatment)), "arms")),
        br(), br()
      )
    })

    # ── Preview table ─────────────────────────────────────────────────────────
    output$preview_tbl <- DT::renderDataTable({
      d <- preview_data(); if (is.null(d)) return(NULL)
      bind_rows(d) |>
        mutate(site = paste0("Site ", toupper(site))) |>
        rename(Site="site", `Rand #`="Randomization", Treatment="Treatment")
    },
    rownames=FALSE,
    options=list(pageLength=15, scrollX=TRUE, dom="lftip"))

    # ── Allocation bar chart ───────────────────────────────────────────────────
    output$alloc_plot <- renderPlot({
      d <- preview_data(); if (is.null(d)) return(NULL)
      bind_rows(d) |>
        mutate(site = paste0("Site ", toupper(site))) |>
        group_by(site, Treatment) |>
        summarise(n = n(), .groups = "drop") |>
        ggplot(aes(x = Treatment, y = n, fill = site)) +
        geom_col(position = "dodge", width = 0.65) +
        geom_text(aes(label = n), position = position_dodge(0.65),
                  vjust = -0.4, size = 3.8, fontface = "bold",
                  color = "#1e293b") +
        scale_fill_brewer(palette = "Set2") +
        labs(x = NULL, y = "Slots", fill = "Site",
             title = paste(RAND_METHODS[[input$method]]$label, "— Allocation")) +
        theme_minimal(base_size = 12) +
        theme(panel.grid.major.x = element_blank(),
              plot.title  = element_text(size = 11, face = "bold", colour = "#0f2744"),
              axis.text   = element_text(colour = "#334155"),
              axis.title  = element_text(colour = "#334155"),
              legend.text = element_text(colour = "#334155"))
    }, bg = "transparent")

    # ── Generate & Upload ─────────────────────────────────────────────────────
    observeEvent(input$btn_generate, {
      d <- tryCatch(do_generate(), error=function(e){
        output$gen_msg <- renderUI(
          div(style="color:#dc2626;margin-top:10px;",
              icon("circle-xmark"), " ", e$message)); NULL
      })
      if (is.null(d)) return()
      preview_data(d)

      errors <- c()
      uploaded <- c()
      for (site in names(d)) {
        tryCatch({
          save_rand_file(d[[site]], site, drop_folder)
          uploaded <- c(uploaded, paste0("rand-", site, ".csv"))
        }, error=function(e) {
          errors <<- c(errors, paste0(site, ": ", e$message))
        })
      }

      # Audit entry
      tryCatch(
        append_audit_fn("RAND_GENERATE", "rand_files", actor(),
          paste0("method=", input$method,
                 " sites=",  paste(names(d), collapse=","),
                 " n=",      input$n_per_site,
                 " arms=",   paste(get_treatments(), collapse="|"),
                 " seed=",   if(isTRUE(input$use_seed)) input$seed else "none")),
        error=function(e) NULL
      )

      if (length(errors)==0) {
        output$gen_msg <- renderUI(
          div(style="color:#059669;margin-top:10px;font-weight:600;",
              icon("circle-check"), " Uploaded: ",
              paste(uploaded, collapse=", ")))
      } else {
        output$gen_msg <- renderUI(
          div(style="color:#dc2626;margin-top:10px;",
              icon("circle-xmark"), " Errors: ", paste(errors, collapse="; ")))
      }
    })
  })
}
