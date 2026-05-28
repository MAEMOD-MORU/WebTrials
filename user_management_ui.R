# ══════════════════════════════════════════════════════════════════════════════
# user_management_ui.R
# Admin-only User Management UI module for WebTrials
# ══════════════════════════════════════════════════════════════════════════════

# ── UI module ──────────────────────────────────────────────────────────────────
userMgmtUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$style(HTML("
      .um-card { background: var(--surface); border: 1px solid var(--border);
                 border-radius: 10px; padding: 22px 24px; margin-bottom: 20px;
                 box-shadow: 0 1px 4px rgba(0,0,0,.07); }
      .um-section-title { font-size: 13px; font-weight: 700; text-transform: uppercase;
                          letter-spacing: 0.5px; color: var(--teal); margin-bottom: 14px; }
      .um-badge-active   { display:inline-block; padding:2px 10px; border-radius:99px;
                           background:#dcfce7; color:#166534; font-size:11px; font-weight:600; }
      .um-badge-inactive { display:inline-block; padding:2px 10px; border-radius:99px;
                           background:#fee2e2; color:#991b1b; font-size:11px; font-weight:600; }
      .um-badge-admin    { display:inline-block; padding:2px 10px; border-radius:99px;
                           background:#fef3c7; color:#92400e; font-size:11px; font-weight:600; }
      .um-badge-user     { display:inline-block; padding:2px 10px; border-radius:99px;
                           background:#e0f2fe; color:#075985; font-size:11px; font-weight:600; }
      .pw-meter { height:5px; border-radius:99px; margin-top:6px;
                  transition: width .3s, background .3s; }
      .um-alert-ok  { background:#f0fdf4; border:1px solid #86efac;
                      color:#166534; border-radius:8px; padding:10px 14px; margin-top:10px; }
      .um-alert-err { background:#fef2f2; border:1px solid #fca5a5;
                      color:#991b1b; border-radius:8px; padding:10px 14px; margin-top:10px; }
      .compliance-banner { background: linear-gradient(135deg,#0f2744,#1a3a5c);
                           color:#e0f2fe; border-radius:8px; padding:10px 16px;
                           font-size:11px; margin-bottom:16px; }
      .compliance-banner b { color:#5eead4; }
    ")),

    # ── Compliance notice ─────────────────────────────────────────────────────
    div(class="compliance-banner",
      icon("shield-halved"), " ",
      tags$b("Compliance: "),
      "Passwords are hashed (bcrypt-12). Credentials stored AES-256 encrypted. ",
      "All actions logged to tamper-evident audit trail. Accounts are disabled, never deleted. ",
      tags$b("Frameworks: "), "ICH E6(R3) · 21 CFR Part 11 · EMA Annex 11 · GDPR · PDPA"
    ),

    fluidRow(

      # ── LEFT: Create user ───────────────────────────────────────────────────
      column(5,
        div(class="um-card",
          div(class="um-section-title", icon("user-plus"), " Create New User"),
          textInput(ns("new_user"),  "Username", placeholder="3-20 chars, letters/digits/_"),
          textInput(ns("new_email"), "Email", placeholder="user@hospital.org"),
          selectInput(ns("new_site"), "Site",
                      choices = c("a","b","c"),   # populated from credentials at runtime
                      selected = "a"),
          checkboxInput(ns("new_admin"), "Grant Admin access", value = FALSE),
          tags$hr(style="border-color:var(--border);margin:14px 0;"),
          tags$div(class="um-section-title", style="font-size:12px;", icon("lock"), " Password"),
          tags$div(style="position:relative;",
            passwordInput(ns("new_pw1"), "Password", placeholder="Min 8 chars"),
            # Password strength meter
            tags$div(id=ns("pw_meter_wrap"), style="margin-top:-8px; margin-bottom:8px;",
              tags$div(id=ns("pw_meter"), class="pw-meter",
                       style="width:0%; background:#e2e8f0;"),
              tags$small(id=ns("pw_hint"), style="color:var(--text-muted); font-size:11px;",
                "Requires: uppercase · lowercase · digit · special char · min 8 chars")
            )
          ),
          passwordInput(ns("new_pw2"), "Confirm Password"),
          tags$br(),
          actionButton(ns("btn_create"), "Create User",
                       class="btn btn-primary",
                       style="background:var(--navy);border:none;width:100%;font-weight:600;padding:10px;"),
          uiOutput(ns("create_msg"))
        )
      ),

      # ── RIGHT: User list + actions ──────────────────────────────────────────
      column(7,
        div(class="um-card",
          div(class="um-section-title", icon("users"), " User Accounts"),
          div(style="display:flex;gap:8px;margin-bottom:12px;",
            checkboxInput(ns("show_inactive"), "Show disabled accounts", value=FALSE),
            actionButton(ns("btn_refresh_users"), icon("rotate"), class="btn btn-sm btn-default")
          ),
          DT::dataTableOutput(ns("user_table")),
        ),

        # ── Edit panel (appears when a row is selected) ────────────────────
        uiOutput(ns("edit_panel"))
      )
    ),

    # ── Audit trail section ───────────────────────────────────────────────────
    fluidRow(
      column(12,
        div(class="um-card",
          div(class="um-section-title", icon("clipboard-list"), " Audit Trail",
              tags$small(style="font-weight:400;text-transform:none;color:var(--text-muted);",
                " — tamper-evident log (ICH E6 R3 § 5.5.3 / 21 CFR 11.10e)")),
          div(style="display:flex;gap:8px;margin-bottom:10px;",
            downloadButton(ns("dl_audit"), "Export Audit Log (.csv)",
                           style="background:var(--teal);color:#fff;border:none;font-size:13px;"),
            actionButton(ns("btn_refresh_audit"), icon("rotate"), class="btn btn-sm btn-default")
          ),
          DT::dataTableOutput(ns("audit_table"))
        )
      )
    )
  )
}

# ── Server module ─────────────────────────────────────────────────────────────
userMgmtServer <- function(id, actor, session_id = reactive("")) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Password strength meter (JS) ────────────────────────────────────────
    observeEvent(input$new_pw1, {
      pw <- input$new_pw1
      score <- 0
      if (nchar(pw) >= 8)          score <- score + 1
      if (grepl("[A-Z]", pw))      score <- score + 1
      if (grepl("[a-z]", pw))      score <- score + 1
      if (grepl("[0-9]", pw))      score <- score + 1
      if (grepl("[^A-Za-z0-9]",pw))score <- score + 1
      pct <- score * 20
      col <- switch(as.character(score),
        "0"="#e2e8f0","1"="#ef4444","2"="#f97316","3"="#eab308",
        "4"="#22c55e","5"="#0d9488")
      shinyjs::runjs(sprintf(
        'document.getElementById("%s").style.width="%d%%";
         document.getElementById("%s").style.background="%s";',
        ns("pw_meter"), pct, ns("pw_meter"), col
      ))
    })

    # ── Reactive credential store ────────────────────────────────────────────
    cred_trigger <- reactiveVal(0)
    creds_r <- reactive({
      cred_trigger()
      tryCatch(load_credentials(), error=function(e) NULL)
    })

    # ── User table ───────────────────────────────────────────────────────────
    output$user_table <- DT::renderDataTable({
      df <- creds_r()
      if (is.null(df)) return(data.frame(Message="No data"))

      if (!isTRUE(input$show_inactive)) df <- df[isTRUE(df$active) | df$active==TRUE, ]

      df |>
        mutate(
          Status = ifelse(isTRUE(active) | active==TRUE,
                          '<span class="um-badge-active">Active</span>',
                          '<span class="um-badge-inactive">Disabled</span>'),
          Role   = ifelse(isTRUE(admin) | admin==TRUE,
                          '<span class="um-badge-admin">Admin</span>',
                          '<span class="um-badge-user">User</span>')
        ) |>
        select(Username=user, Site=site, Email=email, Role, Status,
               Created=created_at, `Last Modified`=last_modified)
    },
    escape = FALSE,
    selection = "single",
    rownames = FALSE,
    options = list(pageLength=10, scrollX=TRUE,
                   columnDefs=list(list(className="dt-center",targets=c(2,3,4))))
    )

    # ── Edit panel ────────────────────────────────────────────────────────────
    selected_user <- reactive({
      req(input$user_table_rows_selected)
      df <- creds_r()
      if (is.null(df)) return(NULL)
      if (!isTRUE(input$show_inactive)) df <- df[isTRUE(df$active)|df$active==TRUE, ]
      df[input$user_table_rows_selected, ]
    })

    output$edit_panel <- renderUI({
      su <- selected_user()
      if (is.null(su)) return(NULL)

      is_active <- isTRUE(su$active)

      div(class="um-card", style="margin-top:14px;",
        div(class="um-section-title", icon("user-pen"),
            paste0(" Edit: ", su$user)),
        fluidRow(
          column(4, selectInput(ns("edit_site"), "Site",
                                choices=c("a","b","c"), selected=su$site)),
          column(4, checkboxInput(ns("edit_admin"), "Admin", value=isTRUE(su$admin))),
          column(4, textInput(ns("edit_email"), "Email", value=su$email))
        ),
        fluidRow(
          column(6, passwordInput(ns("edit_pw1"), "New Password (leave blank = no change)")),
          column(6, passwordInput(ns("edit_pw2"), "Confirm New Password"))
        ),
        div(style="display:flex;gap:8px;flex-wrap:wrap;margin-top:8px;",
          actionButton(ns("btn_save_edit"), "Save Changes",
                       style="background:var(--navy);color:#fff;border:none;font-weight:600;"),
          if (is_active)
            actionButton(ns("btn_disable"), paste0("Disable ", su$user),
                         style="background:#dc2626;color:#fff;border:none;font-weight:600;")
          else
            actionButton(ns("btn_enable"), paste0("Re-enable ", su$user),
                         style="background:#059669;color:#fff;border:none;font-weight:600;")
        ),
        uiOutput(ns("edit_msg"))
      )
    })

    # ── Create user ───────────────────────────────────────────────────────────
    observeEvent(input$btn_create, {
      if (input$new_pw1 != input$new_pw2) {
        output$create_msg <- renderUI(div(class="um-alert-err", "Passwords do not match."))
        return()
      }
      res <- create_user(
        username       = trimws(input$new_user),
        plain_password = input$new_pw1,
        site           = input$new_site,
        role_admin     = input$new_admin,
        email          = trimws(input$new_email),
        actor          = actor(),
        session_id     = session_id()
      )
      if (res$ok) {
        output$create_msg <- renderUI(div(class="um-alert-ok", icon("circle-check"), " ", res$msg))
        cred_trigger(cred_trigger() + 1)
        # Clear form
        updateTextInput(session, "new_user",  value="")
        updateTextInput(session, "new_email", value="")
        updateTextInput(session, "new_pw1",   value="")
        updateTextInput(session, "new_pw2",   value="")
      } else {
        output$create_msg <- renderUI(div(class="um-alert-err", icon("circle-xmark"), " ", res$msg))
      }
    })

    # ── Save edits ────────────────────────────────────────────────────────────
    observeEvent(input$btn_save_edit, {
      su <- selected_user(); req(su)
      msg_parts <- c()

      # Modify site/role/email
      res <- modify_user(su$user,
                         site       = input$edit_site,
                         role_admin = input$edit_admin,
                         email      = input$edit_email,
                         actor      = actor(),
                         session_id = session_id())
      msg_parts <- c(msg_parts, res$msg)

      # Change password if provided
      if (nchar(trimws(input$edit_pw1)) > 0) {
        if (input$edit_pw1 != input$edit_pw2) {
          output$edit_msg <- renderUI(div(class="um-alert-err", "Passwords do not match."))
          return()
        }
        res2 <- change_password(su$user, input$edit_pw1, actor(), session_id())
        msg_parts <- c(msg_parts, res2$msg)
      }

      cred_trigger(cred_trigger() + 1)
      cls <- if (all(sapply(list(res), `[[`, "ok"))) "um-alert-ok" else "um-alert-err"
      output$edit_msg <- renderUI(div(class=cls,
        icon(if(cls=="um-alert-ok") "circle-check" else "circle-xmark"),
        " ", paste(msg_parts, collapse=" | ")))
    })

    # ── Disable / Enable ──────────────────────────────────────────────────────
    observeEvent(input$btn_disable, {
      su <- selected_user(); req(su)
      res <- disable_user(su$user, actor(), session_id())
      cred_trigger(cred_trigger() + 1)
      cls <- if(res$ok) "um-alert-ok" else "um-alert-err"
      output$edit_msg <- renderUI(div(class=cls, res$msg))
    })
    observeEvent(input$btn_enable, {
      su <- selected_user(); req(su)
      res <- enable_user(su$user, actor(), session_id())
      cred_trigger(cred_trigger() + 1)
      cls <- if(res$ok) "um-alert-ok" else "um-alert-err"
      output$edit_msg <- renderUI(div(class=cls, res$msg))
    })

    # ── Refresh triggers ──────────────────────────────────────────────────────
    observeEvent(input$btn_refresh_users, { cred_trigger(cred_trigger() + 1) })
    observeEvent(input$btn_refresh_audit, { audit_trigger(audit_trigger() + 1) })

    # ── Audit trail ───────────────────────────────────────────────────────────
    audit_trigger <- reactiveVal(0)
    audit_r <- reactive({
      audit_trigger()
      tryCatch(get_audit_trail(), error=function(e) NULL)
    })

    output$audit_table <- DT::renderDataTable({
      df <- audit_r()
      if (is.null(df) || nrow(df)==0) return(data.frame(Message="No audit records yet."))
      df[order(df$timestamp, decreasing=TRUE), ]
    },
    rownames=FALSE, selection="none",
    options=list(pageLength=10, scrollX=TRUE,
                 columnDefs=list(list(width="160px", targets=0)))
    )

    output$dl_audit <- downloadHandler(
      filename = function() paste0("audit_trail_", format(Sys.Date(),"%Y%m%d"), ".csv"),
      content  = function(file) {
        df <- audit_r()
        if (!is.null(df)) write.csv(df, file, row.names=FALSE)
        append_audit("EXPORT_AUDIT", "audit_trail", actor(), "CSV export", session_id())
      }
    )

  })
}
