# ══════════════════════════════════════════════════════════════════════════════
# randomisation_methods.R — WebTrials
#
# ┌─────────────────────────────────────────────────────────────────────────┐
# │  ADD / EDIT / REMOVE randomisation methods HERE                         │
# │                                                                         │
# │  Each method must be registered in RAND_METHODS list at the bottom.     │
# │  The generator UI will automatically pick up any registered method.     │
# └─────────────────────────────────────────────────────────────────────────┘
#
# HOW TO ADD A NEW METHOD:
#   1. Write a function: my_method(n, treatments, params) -> character vector
#   2. Write a params UI function: my_method_ui(ns, treatments) -> tagList/NULL
#   3. Add entry to RAND_METHODS list at the bottom of this file
#
# HOW TO REMOVE A METHOD:
#   Comment out or delete its entry from RAND_METHODS (keep the function)
#
# HOW TO EDIT A METHOD:
#   Edit the function directly — no changes needed elsewhere
# ══════════════════════════════════════════════════════════════════════════════


# ══════════════════════════════════════════════════════════════════════════════
# METHOD 1 — Simple Randomization
# ══════════════════════════════════════════════════════════════════════════════
#
# Each participant has equal probability of assignment.
# Ref: ICH E9 §2.3; Rosenberger & Lachin (2016) Ch.2
#
# params: (none)
# ─────────────────────────────────────────────────────────────────────────────
method_simple <- function(n, treatments, params = list()) {
  sample(rep(treatments, ceiling(n / length(treatments))), n)
}

method_simple_ui <- function(ns, treatments) {
  NULL   # no extra parameters needed
}


# ══════════════════════════════════════════════════════════════════════════════
# METHOD 2 — Block Randomization (Permuted-Block)
# ══════════════════════════════════════════════════════════════════════════════
#
# Balanced within each block. Variable block sizes reduce predictability.
# Ref: ICH E9 §2.3.3; Matts & Lachin (1988)
#
# params:
#   block_sizes : integer vector — multiples of k (number of arms)
#                 default: c(2k, 3k, 4k)
# ─────────────────────────────────────────────────────────────────────────────
method_block <- function(n, treatments, params = list()) {
  k <- length(treatments)
  block_sizes <- params$block_sizes
  if (is.null(block_sizes) || length(block_sizes) == 0)
    block_sizes <- c(k * 2L, k * 3L, k * 4L)
  block_sizes <- as.integer(block_sizes)
  # Each block size must be a multiple of k; silently fix if not
  block_sizes <- unique(block_sizes[block_sizes %% k == 0])
  if (length(block_sizes) == 0) block_sizes <- c(k * 2L)

  result <- c()
  while (length(result) < n) {
    bs    <- sample(block_sizes, 1)
    reps  <- bs / k
    block <- sample(rep(treatments, reps))
    result <- c(result, block)
  }
  result[seq_len(n)]
}

method_block_ui <- function(ns, treatments) {
  k <- length(treatments)
  choices <- setNames(
    c(k*2L, k*3L, k*4L),
    paste0(c(k*2, k*3, k*4), " slots  (", c("2k","3k","4k"), ")")
  )
  tagList(
    tags$label(class = "control-label", "Block sizes (must be multiples of arms k)"),
    checkboxGroupInput(ns("block_sizes"),
                       label    = NULL,
                       choices  = choices,
                       selected = c(k*2L, k*3L, k*4L),
                       inline   = TRUE),
    tags$small(style = "color:var(--text-muted);",
      paste0("k = ", k, " arms. Variable block sizes improve unpredictability (ICH E9)."))
  )
}


# ══════════════════════════════════════════════════════════════════════════════
# METHOD 4 — Response-Adaptive Randomization (Thompson Sampling / Bayesian RAR)
# ══════════════════════════════════════════════════════════════════════════════
#
# Allocation probability shifts toward better-performing arms using
# Bayesian Beta-Binomial conjugate model updated at each assignment.
# Ref: Thompson (1933); Berry et al. (2010); FDA RAR Guidance (2019)
#
# params:
#   min_initial : integer — equal allocation until this many per arm
#   prior_alpha : numeric — Beta prior shape α (successes + 1)
#   prior_beta  : numeric — Beta prior shape β (failures  + 1)
#   true_p      : numeric vector — simulated success probabilities per arm
#                 (used for preview/simulation; replace with real data in analysis)
# ─────────────────────────────────────────────────────────────────────────────
method_adaptive <- function(n, treatments, params = list()) {
  k           <- length(treatments)
  min_initial <- as.integer(params$min_initial %||% 5L)
  pa          <- as.numeric(params$prior_alpha  %||% 1)
  pb          <- as.numeric(params$prior_beta   %||% 1)
  true_p      <- params$true_p
  if (is.null(true_p) || length(true_p) != k) true_p <- rep(0.5, k)

  alpha  <- rep(pa, k)
  beta_v <- rep(pb, k)
  counts <- rep(0L, k)
  result <- character(n)

  for (i in seq_len(n)) {
    # Burn-in: equal allocation until min_initial per arm
    if (min(counts) < min_initial) {
      arm <- which.min(counts)
    } else {
      # Thompson Sampling: sample θ from Beta(α, β) for each arm
      theta <- sapply(seq_len(k), function(j) rbeta(1, alpha[j], beta_v[j]))
      arm   <- which.max(theta)
    }

    result[i]    <- treatments[arm]
    counts[arm]  <- counts[arm] + 1L

    # Simulate binary outcome and update posterior
    outcome      <- rbinom(1, 1, true_p[arm])
    alpha[arm]   <- alpha[arm]  + outcome
    beta_v[arm]  <- beta_v[arm] + (1L - outcome)
  }

  result
}

method_adaptive_ui <- function(ns, treatments) {
  k <- length(treatments)
  tagList(
    fluidRow(
      column(4, numericInput(ns("rar_initial"), "Burn-in per arm:", 5, min=2, max=50)),
      column(4, numericInput(ns("rar_alpha"),   "Prior α (Beta):", 1, min=0.1, max=20, step=0.1)),
      column(4, numericInput(ns("rar_beta"),    "Prior β (Beta):", 1, min=0.1, max=20, step=0.1))
    ),
    textInput(ns("rar_true_p"),
              label       = "Simulated success probabilities (comma-separated, one per arm):",
              value       = paste(rep(0.5, k), collapse=","),
              placeholder = paste0("e.g. ", paste(rep(0.5, k), collapse=","))),
    tags$small(style="color:var(--text-muted);",
      "These probabilities are used for simulation/preview only.",
      " In a live trial, outcomes are entered as data accumulate.",
      " α=β=1 = uniform (non-informative) prior.")
  )
}


# ══════════════════════════════════════════════════════════════════════════════
# ── HOW TO ADD YOUR OWN METHOD ────────────────────────────────────────────────
# ══════════════════════════════════════════════════════════════════════════════
#
# Example skeleton — copy, rename, and fill in:
#
# method_mymethod <- function(n, treatments, params = list()) {
#   # params$my_param <- params$my_param %||% default_value
#   # ... your algorithm ...
#   result   # must return character vector of length n
# }
#
# method_mymethod_ui <- function(ns, treatments) {
#   tagList(
#     numericInput(ns("my_param"), "My parameter:", value=10, min=1)
#     # NULL if no parameters needed
#   )
# }
#
# Then add to RAND_METHODS below:
# mymethod = list(
#   label       = "My Custom Method",
#   description = "One-line description for display in UI.",
#   reference   = "Author (Year); Guideline §X.X",
#   fn          = method_mymethod,
#   ui_fn       = method_mymethod_ui,
#   get_params  = function(input, treatments) list(my_param = input$my_param)
# )


# ══════════════════════════════════════════════════════════════════════════════
# RAND_METHODS REGISTRY
# ══════════════════════════════════════════════════════════════════════════════
# Edit this list to add, remove, or reorder methods shown in the UI.
# Keys (e.g. "simple") are used internally; labels are shown to users.
# ══════════════════════════════════════════════════════════════════════════════

RAND_METHODS <- list(

  simple = list(
    label       = "Simple Randomization",
    description = "Equal allocation probability. No balancing. Suitable for large trials (n > 200).",
    reference   = "ICH E9 §2.3; Rosenberger & Lachin (2016)",
    fn          = method_simple,
    ui_fn       = method_simple_ui,
    get_params  = function(input, treatments) list()
  ),

  block = list(
    label       = "Block Randomization (Permuted-Block)",
    description = "Balanced within blocks. Variable block sizes (2k/3k/4k) reduce predictability. Best for n < 200.",
    reference   = "ICH E9 §2.3.3; Matts & Lachin (1988)",
    fn          = method_block,
    ui_fn       = method_block_ui,
    get_params  = function(input, treatments) {
      bs <- as.integer(input$block_sizes)
      list(block_sizes = if (length(bs) > 0) bs else NULL)
    }
  ),

  adaptive = list(
    label       = "Response-Adaptive (Thompson Sampling)",
    description = "Bayesian RAR: allocation shifts toward better-performing arms as outcomes accumulate.",
    reference   = "Thompson (1933); FDA RAR Guidance (2019); Berry et al. (2010)",
    fn          = method_adaptive,
    ui_fn       = method_adaptive_ui,
    get_params  = function(input, treatments) {
      k      <- length(treatments)
      true_p_str <- trimws(input$rar_true_p %||% "")
      true_p <- tryCatch(
        as.numeric(strsplit(true_p_str, ",")[[1]]),
        error = function(e) rep(0.5, k)
      )
      if (length(true_p) != k || any(is.na(true_p))) true_p <- rep(0.5, k)
      list(min_initial = input$rar_initial %||% 5,
           prior_alpha = input$rar_alpha   %||% 1,
           prior_beta  = input$rar_beta    %||% 1,
           true_p      = true_p)
    }
  )

  # ── Paste your new method entry here (add comma after previous entry) ───────
  # ,mymethod = list(
  #   label      = "My Method",
  #   description = "...",
  #   reference  = "...",
  #   fn         = method_mymethod,
  #   ui_fn      = method_mymethod_ui,
  #   get_params = function(input, treatments) list(...)
  # )

)

# Null-coalesce helper (safe to source multiple times)
if (!exists("%||%", mode="function"))
  `%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !is.na(a[1])) a else b
