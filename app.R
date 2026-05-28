library(shiny)
library(shinymanager)
library(shinydashboard)
library(rdrop2)
library(dplyr)
library(rmarkdown)
library(shinyWidgets)
library(shinyalert)
library(shinyjs)
library(ggplot2)
library(sodium)    # XSalsa20-Poly1305 encryption — install.packages("sodium")
library(bcrypt)    # bcrypt password hashing  — install.packages("bcrypt")
library(digest)    # SHA-256 audit chain      — install.packages("digest")
library(DT)        # data tables              — install.packages("DT")

source("user_management.R")
source("user_management_ui.R")
source("randomisation_generator.R")

# ── Config ────────────────────────────────────────────────────────────────────
tokenfile   <- 'data/droptoken.RDS'
logfile     <- "log.csv"
drop.folder <- "dropbox_data"
drop_auth(rdstoken = tokenfile)

# Credentials loaded from AES-256 encrypted file in Dropbox
# Falls back to Credentials.csv on first run (migrates + encrypts automatically)
# Plain text passwords are never stored — bcrypt(cost=12) hashed
credentials_enc <- load_credentials()  # used internally by make_check_credentials()

sites <- c("a", "b", "c")

# ── Dropbox helpers ───────────────────────────────────────────────────────────
addLog <- function(user, site, file = file.path(drop.folder, logfile)) {
  time.stamp <- date()
  tmplog <- rbind(data.frame(login = user, timestamp = time.stamp),
                  drop_read_csv(file))
  write.csv(tmplog, file = logfile, row.names = FALSE)
  drop_upload(logfile, path = drop.folder)
  paste("You are logged in as ", user, "\non ", time.stamp,
        "\nfrom site ", site, ".", sep = "")
}

addPID <- function(site, screenID, age, sex, randID, treatment, file, timestamp, user) {
  tmpdat <- rbind(
    drop_read_csv(file.path(drop.folder, file)),
    data.frame(site = site, screeningID = screenID, age = age, sex = sex,
               randomizationID = randID, Treatment = treatment,
               Date = timestamp, user = user)
  )
  write.csv(tmpdat, file = file, row.names = FALSE)
  drop_upload(file, path = drop.folder)
  return(tmpdat)
}

existQ <- function(site, pid, age, sex, user) {
  data.file <- paste0("data-", site, ".csv")
  tmp.dat <- drop_read_csv(file.path(drop.folder, data.file))
  tmp  <- tmp.dat[tmp.dat$screeningID == pid & tmp.dat$age == age & tmp.dat$sex == sex, ]
  tmp2 <- tmp.dat[tmp.dat$screeningID == pid & (tmp.dat$age != age | tmp.dat$sex != sex), ]
  list(exist  = nrow(tmp) == 1,
       data   = if (nrow(tmp) == 1) tmp else NULL,
       exist2 = nrow(tmp2) >= 1)
}

getTreatment <- function(site, PID) {
  rand.file <- paste0("rand-", site, ".csv")
  data.file <- paste0("data-", site, ".csv")
  output    <- list()
  tmp.data  <- drop_read_csv(file.path(drop.folder, data.file))
  is.assigned <- PID %in% tmp.data[, 2]
  output$Assigned <- is.assigned
  npid     <- nrow(tmp.data)
  tmp.rand <- drop_read_csv(file.path(drop.folder, rand.file))
  n.rand   <- nrow(tmp.rand)
  if (!is.assigned && (npid + 1 <= n.rand)) {
    output$site            <- site
    output$screeningID     <- PID
    output$randomizationID <- tmp.rand[npid + 1, 2]
    output$Treatment       <- tmp.rand[npid + 1, 3]
  } else {
    output$Treatment <- NULL
  }
  return(output)
}

check_screening_ID <- function(id) {
  if (is.na(id) || !is.numeric(id)) return(FALSE)
  xx <- unlist(strsplit(as.character(id), ""))
  xx[1] == "1" && length(xx) == 5
}
check_age <- function(age) !is.null(age) && !is.na(age) && is.numeric(age)

# ── Load all rand + data from Dropbox (for dashboard) ────────────────────────
load_all_data <- function() {
  rand_list <- lapply(sites, function(s) {
    df <- drop_read_csv(file.path(drop.folder, paste0("rand-", s, ".csv")))
    df$site <- s
    df
  })
  data_list <- lapply(sites, function(s) {
    df <- drop_read_csv(file.path(drop.folder, paste0("data-", s, ".csv")))
    df$site <- s
    df
  })
  list(
    rand = bind_rows(rand_list),
    data = bind_rows(data_list)
  )
}

# ── Shared CSS (external file: www/styles.css) ───────────────────────────────
role_css <- tags$head(
  tags$link(rel = "stylesheet",
            href = "https://fonts.googleapis.com/css2?family=DM+Sans:wght@300;400;500;600;700&family=DM+Mono:wght@400;500&display=swap"),
  tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
  tags$script(src = "theme.js")
)

# ── Regular-user UI ───────────────────────────────────────────────────────────
rand_ui <- fluidPage(
  role_css,
  # Dark/light toggle — injected by theme.js into this div
  tags$div(id = "theme-toggle-fixed"),
  titlePanel(tagList(tags$h1("WebTrials"), verbatimTextOutput("auth_output"))),
  sidebarLayout(
    sidebarPanel(
      useShinyjs(),
      uiOutput("reset_input"),
      actionButton("validate.button", "Validate"),
      useShinyalert()
    ),
    mainPanel()
  )
)

# ── Admin dashboard UI ────────────────────────────────────────────────────────
dashboard_ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(
    title = "WebTrials",
    titleWidth = 220,
    tags$li(class = "dropdown", uiOutput("role_badge_dash"))
  ),
  dashboardSidebar(width = 220,
    sidebarMenu(
      menuItem("Randomisation Form", tabName = "rand_form",  icon = icon("clipboard-list")),
      menuItem("Dashboard",          tabName = "dashboard",  icon = icon("chart-bar")),
      menuItem("User Management",    tabName = "user_mgmt",  icon = icon("users-gear")),
      menuItem("Rand Generator",     tabName = "rand_gen",   icon = icon("dice"))
    )
  ),
  dashboardBody(
    role_css,
    tabItems(

      # ── Randomisation form tab ──────────────────────────────────────────
      tabItem(tabName = "rand_form",
        fluidPage(
          titlePanel(tagList(tags$h1("WebTrials"), verbatimTextOutput("auth_output_dash"))),
          sidebarLayout(
            sidebarPanel(
              useShinyjs(),
              uiOutput("reset_input_dash"),
              actionButton("validate.button.dash", "Validate")
            ),
            mainPanel()
          )
        )
      ),

      # ── Dashboard tab ────────────────────────────────────────────────────
      tabItem(tabName = "dashboard",
        fluidRow(
          column(12,
            div(style = "display:flex; align-items:center; justify-content:space-between; margin-bottom:8px;",
              h2(style="margin:0;", "Randomisation Dashboard"),
              actionButton("refresh_dash", "🔄 Refresh", class="btn btn-sm btn-default")
            )
          )
        ),

        # ── Site selector ─────────────────────────────────────────────────
        fluidRow(
          column(12,
            radioGroupButtons("site_filter", label = "Site:",
              choices = c("All" = "all", "Site A" = "a", "Site B" = "b", "Site C" = "c"),
              selected = "all", status = "primary", size = "sm"
            )
          )
        ),

        # ── Summary stat boxes ────────────────────────────────────────────
        fluidRow(
          valueBoxOutput("vbox_total_rand",    width = 3),
          valueBoxOutput("vbox_total_enrolled",width = 3),
          valueBoxOutput("vbox_remaining",     width = 3),
          valueBoxOutput("vbox_pct",           width = 3)
        ),

        # ── Treatment allocation: rand vs used ────────────────────────────
        fluidRow(
          box(title = "Treatment Slots: Available vs Used", status = "primary",
              solidHeader = TRUE, width = 6,
              plotOutput("plot_treatment_slots", height = "320px")),
          box(title = "Enrolled by Treatment", status = "info",
              solidHeader = TRUE, width = 6,
              plotOutput("plot_enrolled_treatment", height = "320px"))
        ),

        # ── Age histogram + sex breakdown ─────────────────────────────────
        fluidRow(
          box(title = "Age Distribution of Enrolled Patients", status = "warning",
              solidHeader = TRUE, width = 6,
              plotOutput("plot_age_hist", height = "300px")),
          box(title = "Enrolled by Site", status = "success",
              solidHeader = TRUE, width = 6,
              plotOutput("plot_by_site", height = "300px"))
        ),

        # ── Data table ────────────────────────────────────────────────────
        fluidRow(
          box(title = "Enrolled Patients", status = "primary",
              solidHeader = TRUE, width = 12, collapsible = TRUE, collapsed = TRUE,
              DT::dataTableOutput("table_enrolled"))
        )
      ),

      # ── User Management tab (admin only) ────────────────────────────────────
      tabItem(tabName = "user_mgmt",
        userMgmtUI("um")
      ),

      # ── Randomisation Generator tab (admin only) ──────────────────────────
      tabItem(tabName = "rand_gen",
        randGenUI("rg")
      )
    )
  )
)

# ── Root UI ───────────────────────────────────────────────────────────────────
ui <- secure_app(uiOutput("root_ui"))

# ── Server ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  values       <- reactiveValues()
  login_logged <- reactiveVal(FALSE)

  res_auth <- secure_server(check_credentials = make_check_credentials())

  # shinymanager flattens user_info list into res_auth directly:
  # res_auth$user, res_auth$site, res_auth$admin, res_auth$email
  is_admin <- reactive({
    req(res_auth$user)
    isTRUE(as.logical(res_auth$admin))
  })

  # Session ID for audit trail (CFR Part 11)
  session_id_r <- reactive({ session$token })

  # User Management module (admin only)
  observe({
    req(is_admin())
    userMgmtServer("um",
                   actor      = reactive(res_auth$user),
                   session_id = session_id_r)
  })

  # Randomisation Generator module (admin only)
  observe({
    req(is_admin())
    randGenServer("rg",
                  actor         = reactive(res_auth$user),
                  drop_folder   = drop.folder,
                  append_audit_fn = append_audit)
  })

  # Root UI switch
  output$root_ui <- renderUI({
    req(res_auth$user)
    # Signal JS to re-apply saved theme and re-inject toggle after UI swap
    session$sendCustomMessage("__themeReapply__", list())
    if (is_admin()) dashboard_ui else rand_ui
  })

  # Role badge
  badge <- reactive({
    req(res_auth$user)
    cls <- if (is_admin()) "role-badge role-admin" else "role-badge role-user"
    lbl <- if (is_admin()) tagList(icon("shield-halved"), " Admin") else tagList(icon("user"), " User")
    tags$span(class = cls, lbl)
  })
  output$role_badge_dash <- renderUI({ badge() })

  # Login log
  log_msg <- reactive({
    req(res_auth$user)
    if (!login_logged()) {
      login_logged(TRUE)
      addLog(user = res_auth$user, site = res_auth$site)
    } else {
      paste0("You are logged in as ", res_auth$user,
             "\nfrom site ", res_auth$site, ".")
    }
  })
  output$auth_output      <- renderText({ log_msg() })
  output$auth_output_dash <- renderText({ log_msg() })

  # ── Randomisation form ────────────────────────────────────────────────────
  reset_form <- function(sfx = "") {
    pid_id <- paste0("PID", sfx)
    sex_id <- paste0("sex", sfx)
    age_id <- paste0("age", sfx)
    output[[paste0("reset_input", sfx)]] <- renderUI({
      div(
        numericInput(pid_id, "Screening ID (1XXXX)", value = NULL),
        radioButtons(sex_id, "Sex:", c("Male","Female","Other"), inline = TRUE),
        numericInput(age_id, "Age:", value = NULL, min = 18, max = 60)
      )
    })
  }
  reset_form(""); reset_form("_dash")

  observe({
    for (sfx in c("", "_dash")) {
      local({
        s   <- sfx
        btn <- paste0("validate.button", if (s == "") "" else ".dash")
        sex <- paste0("sex", s)
        observe({ if (is.null(input[[sex]])) shinyjs::disable(btn) else shinyjs::enable(btn) })
      })
    }
  })

  handle_validate <- function(sfx = "") {
    values$site      <- res_auth$site
    values$user      <- res_auth$user
    values$data.file <- paste0("data-", values$site, ".csv")
    values$PID       <- input[[paste0("PID", sfx)]]
    values$age       <- input[[paste0("age", sfx)]]
    values$sex       <- input[[paste0("sex", sfx)]]
    values$check.screenID <- check_screening_ID(values$PID)
    values$check.age      <- check_age(values$age)

    if (values$check.screenID && values$check.age && between(values$age, 18, 60)) {
      ask_confirmation(
        inputId = "validate.confirm", type = "warning",
        title = "Please confirm your input!",
        text = tags$div(
          tags$p(), tags$b("Screening ID: PLT-"), values$site, "-", values$PID,
          tags$p(), tags$b("Age: "), values$age, tags$b("  Sex: "), values$sex,
          tags$p(), "Click OK to proceed or Cancel to go back."
        ),
        btn_labels = c("Cancel","OK"), btn_colors = c("#FE642E","#04B404"), html = TRUE
      )
    } else {
      ask_confirmation(
        inputId = "checking.input", type = "error",
        title = "Please check your inputs!",
        text = "Screening ID must be 5 digits and Age must be between 18 and 60 years.",
        btn_labels = c(NULL,"OK")
      )
    }
  }

  observeEvent(input$validate.button,      { handle_validate("") })
  observeEvent(input$validate.button.dash, { handle_validate("_dash") })

  observeEvent(input$validate.confirm, {
    req(isTRUE(input$validate.confirm))
    ec <- existQ(pid = values$PID, age = values$age,
                 sex = values$sex, site = values$site, user = res_auth$user)
    values$exist.data   <- ec$data
    values$exist.check  <- ec$exist
    values$exist2.check <- ec$exist2
    tl <- getTreatment(values$site, values$PID)
    values$assigned        <- tl$Assigned
    values$randomizationID <- tl$randomizationID
    values$treatment       <- tl$Treatment
    values$timestamp       <- date()

    if (values$exist.check) {
      ask_confirmation(
        inputId = "exist.info", type = "success",
        title = "This patient has been randomized!",
        text = tags$div(
          tags$b("Registered by: "), values$exist.data$user,
          tags$p(), tags$b("Screening ID: PLT-"), values$exist.data$site, "-", values$exist.data$screeningID,
          tags$p(), tags$b("Age: "), values$exist.data$age, tags$b("  Sex: "), values$exist.data$sex,
          tags$p(), tags$b("Subject No: PLT-"), values$exist.data$site, "-",
          sprintf("%03d", as.numeric(values$exist.data$randomizationID)),
          tags$p(), tags$b("Treatment: "), values$exist.data$Treatment,
          tags$p(), tags$b("Date: "), values$exist.data$Date, tags$p(),
          tagList(downloadButton("downloadPDF", "Download PDF"))
        ),
        btn_labels = c(NULL,"OK"), btn_colors = c("#04B404","#04B404"), html = TRUE
      )
    } else if (values$exist2.check && values$assigned) {
      ask_confirmation(
        inputId = "exist2.info", type = "warning",
        title = "Patient found but age/sex mismatch.",
        text = "Please check your inputs.",
        btn_labels = c(NULL,"OK"), btn_colors = c("#FE642E","#04B404")
      )
    } else if (!values$assigned) {
      values$newdata <- addPID(
        site = values$site, screenID = values$PID, age = values$age,
        sex = values$sex, randID = values$randomizationID,
        treatment = values$treatment, file = values$data.file,
        timestamp = values$timestamp, user = res_auth$user
      )
      ask_confirmation(
        inputId = "add.new", type = "success", title = values$treatment,
        text = tags$div(
          tags$p(), tags$b("Screening ID: PLT-"), values$site, "-", values$PID,
          tags$p(), tags$b("Age: "), values$age, tags$b("  Sex: "), values$sex,
          tags$p(), tags$b("Subject No: PLT-"), values$site, "-",
          sprintf("%03d", as.numeric(values$randomizationID)),
          tags$p(), tags$b("Treatment: "), values$treatment,
          tags$p(), tags$b("Date: "), values$timestamp, tags$p(),
          tagList(downloadButton("downloadPDF2", "Download PDF"))
        ),
        btn_labels = c(NULL,"OK"), btn_colors = c("#FE642E","#04B404"), html = TRUE
      )
      # refresh dashboard data after new enrolment
      dash_trigger(isolate(dash_trigger()) + 1)
    }
  })

  output$downloadPDF <- downloadHandler(
    filename = function() paste0("PLT-", values$site, "-", values$PID, ".pdf"),
    content  = function(file) {
      src <- normalizePath("report.Rmd"); owd <- setwd(tempdir()); on.exit(setwd(owd))
      file.copy(src, "report.Rmd")
      file.rename(render("report.Rmd", pdf_document(latex_engine="xelatex")), file)
    }
  )
  output$downloadPDF2 <- downloadHandler(
    filename = function() paste0("PLT-", values$site, "-", values$PID, ".pdf"),
    content  = function(file) {
      src <- normalizePath("report2.Rmd"); owd <- setwd(tempdir()); on.exit(setwd(owd))
      file.copy(src, "report2.Rmd")
      file.rename(render("report2.Rmd", pdf_document(latex_engine="xelatex")), file)
    }
  )

  observeEvent(input$exist.info,  { if (!input$exist.info)  { reset_form(""); reset_form("_dash") } })
  observeEvent(input$exist2.info, { if (!input$exist2.info) { reset_form(""); reset_form("_dash") } })
  observeEvent(input$add.new,     { if (!input$add.new)     { reset_form(""); reset_form("_dash") } })

  # ── Dashboard data ────────────────────────────────────────────────────────
  dash_trigger <- reactiveVal(0)
  observeEvent(input$refresh_dash, { dash_trigger(dash_trigger() + 1) })

  dash_data <- reactive({
    dash_trigger()   # depend on trigger for manual refresh
    req(is_admin())
    load_all_data()
  })

  # Filtered by site selector
  filtered_rand <- reactive({
    df <- dash_data()$rand
    if (input$site_filter != "all") df <- df[df$site == input$site_filter, ]
    df
  })
  filtered_data <- reactive({
    df <- dash_data()$data
    if (input$site_filter != "all") df <- df[df$site == input$site_filter, ]
    df
  })

  # ── Value boxes ───────────────────────────────────────────────────────────
  output$vbox_total_rand <- renderValueBox({
    valueBox(nrow(filtered_rand()), "Total Randomisation Slots",
             icon = icon("list-ol"), color = "blue")
  })
  output$vbox_total_enrolled <- renderValueBox({
    valueBox(nrow(filtered_data()), "Patients Enrolled",
             icon = icon("user-check"), color = "green")
  })
  output$vbox_remaining <- renderValueBox({
    rem <- nrow(filtered_rand()) - nrow(filtered_data())
    valueBox(rem, "Slots Remaining",
             icon = icon("hourglass-half"), color = "yellow")
  })
  output$vbox_pct <- renderValueBox({
    pct <- if (nrow(filtered_rand()) > 0)
      round(100 * nrow(filtered_data()) / nrow(filtered_rand()), 1) else 0
    valueBox(paste0(pct, "%"), "Enrolment Progress",
             icon = icon("percent"), color = "purple")
  })

  # ── Plot: treatment slots available vs used ───────────────────────────────
  output$plot_treatment_slots <- renderPlot({
    rand_df <- filtered_rand()
    data_df <- filtered_data()

    # Count slots per treatment in randomisation list
    slots <- rand_df |>
      group_by(Treatment) |>
      summarise(total = n(), .groups = "drop")

    # Count used per treatment
    used <- data_df |>
      group_by(Treatment) |>
      summarise(used = n(), .groups = "drop")

    df <- left_join(slots, used, by = "Treatment") |>
      mutate(used = replace(used, is.na(used), 0),
             remaining = total - used) |>
      tidyr::pivot_longer(c(used, remaining),
                          names_to = "status", values_to = "n") |>
      mutate(status = factor(status, levels = c("remaining","used"),
                             labels = c("Remaining","Used")))

    ggplot(df, aes(x = reorder(Treatment, -n), y = n, fill = status)) +
      geom_col(width = 0.6) +
      geom_text(aes(label = n), position = position_stack(vjust = 0.5),
                size = 4, fontface = "bold", color = "white") +
      scale_fill_manual(values = c("Used" = "#2980b9", "Remaining" = "#bdc3c7")) +
      labs(x = NULL, y = "Number of slots", fill = NULL) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "top",
            panel.grid.major.x = element_blank())
  })

  # ── Plot: enrolled patients by treatment ──────────────────────────────────
  output$plot_enrolled_treatment <- renderPlot({
    df <- filtered_data()
    if (nrow(df) == 0) {
      ggplot() + annotate("text", x=0.5, y=0.5, label="No data", size=6) +
        theme_void(); return()
    }
    df |>
      group_by(Treatment) |>
      summarise(n = n(), .groups = "drop") |>
      ggplot(aes(x = reorder(Treatment, n), y = n, fill = Treatment)) +
      geom_col(width = 0.6, show.legend = FALSE) +
      geom_text(aes(label = n), hjust = -0.3, size = 5, fontface = "bold") +
      coord_flip() +
      scale_fill_brewer(palette = "Set2") +
      labs(x = NULL, y = "Patients") +
      theme_minimal(base_size = 13) +
      theme(panel.grid.major.y = element_blank())
  })

  # ── Plot: age histogram ───────────────────────────────────────────────────
  output$plot_age_hist <- renderPlot({
    df <- filtered_data()
    if (nrow(df) == 0) {
      ggplot() + annotate("text", x=0.5, y=0.5, label="No data", size=6) +
        theme_void(); return()
    }
    ggplot(df, aes(x = age, fill = Treatment)) +
      geom_histogram(binwidth = 5, color = "white", alpha = 0.85) +
      scale_fill_brewer(palette = "Set2") +
      labs(x = "Age (years)", y = "Count", fill = "Treatment") +
      theme_minimal(base_size = 13) +
      theme(legend.position = "bottom")
  })

  # ── Plot: enrolled by site ────────────────────────────────────────────────
  output$plot_by_site <- renderPlot({
    df <- filtered_data()
    if (nrow(df) == 0) {
      ggplot() + annotate("text", x=0.5, y=0.5, label="No data", size=6) +
        theme_void(); return()
    }
    df |>
      group_by(site, Treatment) |>
      summarise(n = n(), .groups = "drop") |>
      mutate(site = paste0("Site ", toupper(site))) |>
      ggplot(aes(x = site, y = n, fill = Treatment)) +
      geom_col(width = 0.55, position = "dodge") +
      geom_text(aes(label = n), position = position_dodge(width = 0.55),
                vjust = -0.4, size = 4, fontface = "bold") +
      scale_fill_brewer(palette = "Set2") +
      labs(x = NULL, y = "Patients", fill = "Treatment") +
      theme_minimal(base_size = 13) +
      theme(legend.position = "bottom",
            panel.grid.major.x = element_blank())
  })

  # ── Data table ────────────────────────────────────────────────────────────
  output$table_enrolled <- DT::renderDataTable({
    df <- filtered_data() |>
      mutate(site = paste0("Site ", toupper(site))) |>
      select(Site = site, `Screening ID` = screeningID,
             Age = age, Sex = sex, Treatment, Date)
    DT::datatable(df, options = list(pageLength = 15, scrollX = TRUE),
                  rownames = FALSE)
  })
}

shinyApp(ui, server)
