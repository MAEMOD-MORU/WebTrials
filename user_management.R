# ══════════════════════════════════════════════════════════════════════════════
# user_management.R — WebTrials
#
# Regulatory: ICH E6(R3), 21 CFR Part 11, EMA Annex 11, GDPR, PDPA
#
# Requires (all have Windows binaries — no compilation needed):
#   sodium  >= 1.4.0  : XSalsa20-Poly1305 authenticated encryption
#   bcrypt  >= 1.2.0  : bcrypt password hashing (adaptive, salted)
#   digest            : SHA-256 hash chain for audit trail
#   dplyr, rdrop2     : already in project
#
# Install: install.packages(c("sodium", "bcrypt", "digest", "DT"))
#
# Security:
#   Passwords  : bcrypt::hashpw() cost=12 — adaptive, GPU-resistant
#   Encryption : sodium::data_encrypt()   — XSalsa20-Poly1305 (authenticated)
#   Key        : 256-bit random, stored in data/cred.key (never in Dropbox)
#   Audit      : append-only SHA-256 hash chain (tamper-evident, CFR 11.10e)
#   Accounts   : disabled, never deleted (preserves audit trail)
# ══════════════════════════════════════════════════════════════════════════════

library(sodium)   # XSalsa20-Poly1305 encryption
library(bcrypt)   # bcrypt password hashing
library(digest)   # SHA-256 audit chain
library(dplyr)
library(rdrop2)

CRED_KEYFILE  <- "data/cred.key"
CRED_FILE_ENC <- "credentials.enc"
AUDIT_FILE    <- "audit_trail.enc"

# ── Key management ─────────────────────────────────────────────────────────────
load_or_create_key <- function() {
  dir.create("data", showWarnings = FALSE, recursive = TRUE)
  if (file.exists(CRED_KEYFILE)) {
    readRDS(CRED_KEYFILE)
  } else {
    key <- sodium::random(32)   # 256-bit random key
    saveRDS(key, CRED_KEYFILE)
    message("[KEY] New 256-bit key created: ", CRED_KEYFILE)
    message("[KEY] Back this file up securely — losing it = losing all credentials!")
    key
  }
}

.cred_key <- load_or_create_key()

# ── Encrypt / Decrypt (XSalsa20-Poly1305 authenticated encryption) ─────────────
# sodium::data_encrypt provides both confidentiality AND integrity/authenticity.
# Any tampering of the ciphertext causes decryption to fail outright.
wt_encrypt <- function(obj, key = .cred_key) {
  msg   <- serialize(obj, NULL)          # R object → raw bytes
  nonce <- sodium::random(24)            # 192-bit random nonce
  ct    <- sodium::data_encrypt(msg, key, nonce)
  list(nonce          = nonce,
       ct             = ct,
       schema_version = "3.0",
       created        = format(Sys.time(), tz = "UTC"))
}

wt_decrypt <- function(bundle, key = .cred_key) {
  # data_decrypt throws on any tampering — built-in MAC verification
  msg <- sodium::data_decrypt(bundle$ct, key, bundle$nonce)
  unserialize(msg)
}

# ── Save encrypted to Dropbox + local cache ────────────────────────────────────
# Write to a temp file named exactly as the target so Dropbox keeps that name.
save_encrypted <- function(obj, filename, key = .cred_key) {
  bundle    <- wt_encrypt(obj, key)
  tmp_named <- file.path(tempdir(), filename)
  saveRDS(bundle, tmp_named)
  drop_upload(tmp_named, path = drop.folder)
  # Local cache
  dir.create("data", showWarnings = FALSE)
  file.copy(tmp_named, file.path("data", filename), overwrite = TRUE)
  unlink(tmp_named)
  invisible(TRUE)
}

# ── Load encrypted from Dropbox (local cache fallback) ────────────────────────
load_encrypted <- function(filename, key = .cred_key) {
  local_cache <- file.path("data", filename)
  tryCatch({
    tmp <- tempfile(fileext = ".enc")
    drop_download(file.path(drop.folder, filename),
                  local_path = tmp, overwrite = TRUE)
    bundle <- readRDS(tmp)
    unlink(tmp)
    wt_decrypt(bundle, key)
  }, error = function(e) {
    if (file.exists(local_cache)) {
      message("[CACHE] Dropbox unavailable — using local cache: ", filename)
      tryCatch(wt_decrypt(readRDS(local_cache), key), error = function(e2) NULL)
    } else {
      NULL
    }
  })
}

# ── Password hashing — bcrypt (cost=12) ───────────────────────────────────────
# bcrypt is adaptive, salted, and resistant to GPU brute-force attacks.
# cost=12 ≈ 250ms per hash on modern hardware (CFR Part 11 compliant).
hash_password <- function(plain) {
  bcrypt::hashpw(as.character(plain), bcrypt::gensalt(log_rounds = 12))
}

verify_password <- function(plain, stored) {
  if (is.null(stored) || length(stored) == 0) return(FALSE)
  stored_str <- as.character(stored[1])
  if (is.na(stored_str) || nchar(stored_str) == 0) return(FALSE)

  plain_str <- as.character(plain)

  # Debug: show hash prefix (never log full hash or password)
  message("[AUTH] Hash prefix: ", substr(stored_str, 1, 10), "...")

  # bcrypt hash must start with $2 — if not, hash was stored incorrectly
  if (!startsWith(stored_str, "$2")) {
    message("[AUTH] ERROR: stored value is not a bcrypt hash! Starts with: ",
            substr(stored_str, 1, 4))
    return(FALSE)
  }

  tryCatch(
    isTRUE(bcrypt::checkpw(plain_str, stored_str)),
    error = function(e) {
      message("[AUTH] bcrypt verify error: ", conditionMessage(e))
      FALSE
    }
  )
}

# ── Password policy ────────────────────────────────────────────────────────────
check_password_policy <- function(pw) {
  e <- c()
  if (nchar(pw) < 8)              e <- c(e, "At least 8 characters")
  if (!grepl("[A-Z]", pw))        e <- c(e, "At least one uppercase letter")
  if (!grepl("[a-z]", pw))        e <- c(e, "At least one lowercase letter")
  if (!grepl("[0-9]", pw))        e <- c(e, "At least one digit")
  if (!grepl("[^A-Za-z0-9]", pw)) e <- c(e, "At least one special character (!@#$...)")
  list(ok = length(e) == 0, errors = e)
}

# ── Migrate from plain-text CSV ────────────────────────────────────────────────
.migrate_csv <- function(raw) {
  if (!"site"  %in% names(raw)) raw$site  <- "default"
  if (!"admin" %in% names(raw)) raw$admin <- FALSE
  if (!"email" %in% names(raw)) raw$email <- ""
  raw$admin <- as.logical(raw$admin)

  message("[INIT] Hashing ", nrow(raw),
          " passwords with bcrypt (cost=12) — ~0.3s per user...")
  raw$password_hash <- sapply(as.character(raw$password), hash_password)
  raw$password      <- NULL
  raw$active        <- TRUE
  raw$created_at    <- format(Sys.time(), tz = "UTC")
  raw$created_by    <- "SYSTEM_MIGRATION"
  raw$last_modified <- format(Sys.time(), tz = "UTC")
  raw$modified_by   <- "SYSTEM_MIGRATION"

  save_encrypted(raw, CRED_FILE_ENC)
  message("[INIT] Done! Encrypted credentials saved to Dropbox as '", CRED_FILE_ENC, "'")
  message("[INIT] You may now delete Credentials.csv from Dropbox.")
  raw
}

# ── Load credentials ───────────────────────────────────────────────────────────
load_credentials <- function() {

  # 1. Normal path — encrypted file in Dropbox
  creds <- load_encrypted(CRED_FILE_ENC)
  if (!is.null(creds)) return(creds)

  message("[INIT] No encrypted credentials found. Looking for Credentials.csv...")

  raw <- NULL

  # 2a. Dropbox subfolder
  raw <- tryCatch({
    df <- drop_read_csv(file.path(drop.folder, "Credentials.csv"))
    message("[INIT] Found: Dropbox/", drop.folder, "/Credentials.csv")
    df
  }, error = function(e) NULL)

  # 2b. Dropbox root
  if (is.null(raw)) {
    raw <- tryCatch({
      df <- drop_read_csv("Credentials.csv")
      message("[INIT] Found: Dropbox root /Credentials.csv")
      df
    }, error = function(e) NULL)
  }

  # 2c. Local files
  for (p in c("Credentials.csv", "data/Credentials.csv")) {
    if (is.null(raw) && file.exists(p)) {
      raw <- tryCatch({
        df <- read.csv(p, stringsAsFactors = FALSE)
        message("[INIT] Found locally: ", p)
        df
      }, error = function(e) NULL)
    }
  }

  if (is.null(raw)) {
    stop(paste0(
      "No credentials found.\n",
      "Place Credentials.csv (columns: user,password,site,admin,email) in:\n",
      "  Dropbox: ", drop.folder, "/Credentials.csv\n",
      "  OR Local: Credentials.csv (same folder as app.R)"
    ))
  }

  .migrate_csv(raw)
}

save_credentials <- function(creds) save_encrypted(creds, CRED_FILE_ENC)

# ── Audit trail — append-only SHA-256 hash chain ──────────────────────────────
append_audit <- function(action, target_user, actor, details = "", session_id = "") {
  existing  <- tryCatch(load_encrypted(AUDIT_FILE), error = function(e) NULL)
  if (is.null(existing)) existing <- data.frame()

  prev_hash <- if (nrow(existing) > 0) tail(existing$chain_hash, 1) else "GENESIS"
  payload   <- paste(action, target_user, actor,
                     format(Sys.time(), tz = "UTC"), details)
  this_hash <- digest::digest(paste0(prev_hash, payload), algo = "sha256")

  entry <- data.frame(
    timestamp   = format(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    action      = action,
    target_user = target_user,
    actor       = actor,
    session_id  = session_id,
    details     = details,
    chain_hash  = this_hash,
    stringsAsFactors = FALSE
  )
  save_encrypted(bind_rows(existing, entry), AUDIT_FILE)
  invisible(entry)
}

# ── CRUD operations ────────────────────────────────────────────────────────────
create_user <- function(username, plain_password, site, role_admin, email,
                        actor, session_id = "") {
  if (!grepl("^[a-zA-Z0-9_]{3,20}$", username))
    return(list(ok=FALSE, msg="Username: 3-20 chars, letters/digits/underscore only"))
  pw <- check_password_policy(plain_password)
  if (!pw$ok)
    return(list(ok=FALSE, msg=paste("Password policy:", paste(pw$errors, collapse="; "))))
  creds <- load_credentials()
  if (username %in% creds$user)
    return(list(ok=FALSE, msg="Username already exists"))
  new_row <- data.frame(
    user=username, password_hash=hash_password(plain_password),
    site=site, admin=role_admin, email=email, active=TRUE,
    created_at=format(Sys.time(),tz="UTC"), created_by=actor,
    last_modified=format(Sys.time(),tz="UTC"), modified_by=actor,
    stringsAsFactors=FALSE)
  save_credentials(bind_rows(creds, new_row))
  append_audit("CREATE", username, actor,
               paste0("site=",site," admin=",role_admin," email=",email), session_id)
  list(ok=TRUE, msg=paste0("User '", username, "' created successfully."))
}

disable_user <- function(username, actor, session_id = "") {
  creds <- load_credentials()
  idx <- which(creds$user == username)
  if (!length(idx))    return(list(ok=FALSE, msg="User not found"))
  if (username==actor) return(list(ok=FALSE, msg="Cannot disable your own account"))
  creds$active[idx]        <- FALSE
  creds$last_modified[idx] <- format(Sys.time(),tz="UTC")
  creds$modified_by[idx]   <- actor
  save_credentials(creds)
  append_audit("DISABLE", username, actor, "Account disabled", session_id)
  list(ok=TRUE, msg=paste0("'", username, "' disabled."))
}

enable_user <- function(username, actor, session_id = "") {
  creds <- load_credentials()
  idx <- which(creds$user == username)
  if (!length(idx)) return(list(ok=FALSE, msg="User not found"))
  creds$active[idx]        <- TRUE
  creds$last_modified[idx] <- format(Sys.time(),tz="UTC")
  creds$modified_by[idx]   <- actor
  save_credentials(creds)
  append_audit("ENABLE", username, actor, "Account re-enabled", session_id)
  list(ok=TRUE, msg=paste0("'", username, "' re-enabled."))
}

change_password <- function(username, new_plain, actor, session_id = "") {
  pw <- check_password_policy(new_plain)
  if (!pw$ok)
    return(list(ok=FALSE, msg=paste("Password:", paste(pw$errors, collapse="; "))))
  creds <- load_credentials()
  idx <- which(creds$user == username)
  if (!length(idx)) return(list(ok=FALSE, msg="User not found"))
  creds$password_hash[idx] <- hash_password(new_plain)
  creds$last_modified[idx] <- format(Sys.time(),tz="UTC")
  creds$modified_by[idx]   <- actor
  save_credentials(creds)
  append_audit("PWCHANGE", username, actor, "Password changed", session_id)
  list(ok=TRUE, msg="Password updated successfully.")
}

modify_user <- function(username, site=NULL, role_admin=NULL, email=NULL,
                        actor, session_id="") {
  creds <- load_credentials()
  idx <- which(creds$user == username)
  if (!length(idx)) return(list(ok=FALSE, msg="User not found"))
  changes <- c()
  if (!is.null(site))       { creds$site[idx]  <- site;       changes <- c(changes, paste0("site=",site)) }
  if (!is.null(role_admin)) { creds$admin[idx] <- role_admin; changes <- c(changes, paste0("admin=",role_admin)) }
  if (!is.null(email))      { creds$email[idx] <- email;      changes <- c(changes, paste0("email=",email)) }
  creds$last_modified[idx] <- format(Sys.time(),tz="UTC")
  creds$modified_by[idx]   <- actor
  save_credentials(creds)
  append_audit("MODIFY", username, actor, paste(changes, collapse="; "), session_id)
  list(ok=TRUE, msg="User updated.")
}

# ── shinymanager credential checker ───────────────────────────────────────────
# shinymanager::check_credentials() requires a data.frame with column "user"
# and "password". We build a temporary data.frame with plaintext-equivalent
# by using a custom check_credentials that verifies bcrypt at runtime.
#
# The correct pattern: pass a custom function directly to secure_server().
# shinymanager calls it as: check_fn(user, password) and expects:
#   - FALSE on failure
#   - data.frame (1 row) on success — extra columns become res_auth$<col>
# ── Build shinymanager-compatible credentials data.frame ──────────────────────
# shinymanager::check_credentials(df) compares password column directly.
# We work around this by putting a DUMMY password in the df and overriding
# the check with a custom wrapper that calls bcrypt::checkpw before returning.
make_check_credentials <- function() {
  # shinymanager custom checker API (from datastorm-open/shinymanager source):
  # Must return: list(result = TRUE,  user_info = list(...))  on success
  #          or: list(result = FALSE, user_info = list())     on failure
  # The user_info list fields become res_auth$<field> in the server.
  function(user, password) {
    fail <- list(result = FALSE, user_info = list())

    creds <- tryCatch(load_credentials(), error = function(e) {
      message("[AUTH] Error: ", conditionMessage(e)); NULL
    })
    if (is.null(creds)) return(fail)

    row <- creds[creds$user == user, ]
    if (nrow(row) == 0) {
      message("[AUTH] User not found: ", user); return(fail)
    }
    if (!isTRUE(as.logical(row$active[1]))) {
      message("[AUTH] Account disabled: ", user); return(fail)
    }
    if (!verify_password(password, row$password_hash[1])) {
      message("[AUTH] Wrong password: ", user); return(fail)
    }

    message("[AUTH] Login OK: ", user)
    list(
      result    = TRUE,
      user_info = list(
        user  = row$user[1],
        site  = row$site[1],
        admin = as.logical(row$admin[1]),
        email = row$email[1]
      )
    )
  }
}


get_user_info <- function(username) {
  if (exists(username, envir = .wt_auth_cache))
    get(username, envir = .wt_auth_cache)
  else
    NULL
}

get_audit_trail <- function() {
  tryCatch(load_encrypted(AUDIT_FILE), error = function(e) NULL)
}
