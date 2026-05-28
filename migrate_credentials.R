# ══════════════════════════════════════════════════════════════════════════════
# migrate_credentials.R
#
# Run this ONCE before starting the app for the first time.
# Reads Credentials.csv → hashes passwords with bcrypt → encrypts with sodium
# → saves credentials.enc to Dropbox and data/credentials.enc locally.
#
# Usage (in RStudio console):
#   source("migrate_credentials.R")
#
# After success: Credentials.csv can be deleted from Dropbox.
# ══════════════════════════════════════════════════════════════════════════════

library(sodium)
library(bcrypt)
library(digest)
library(rdrop2)

# ── Config — must match app.R ─────────────────────────────────────────────────
tokenfile     <- "data/droptoken.RDS"
drop.folder   <- "dropbox_data"
CRED_KEYFILE  <- "data/cred.key"
CRED_FILE_ENC <- "credentials.enc"

drop_auth(rdstoken = tokenfile)

# ── Key ───────────────────────────────────────────────────────────────────────
dir.create("data", showWarnings = FALSE)

if (file.exists(CRED_KEYFILE)) {
  cat("[KEY] Loading existing key from:", CRED_KEYFILE, "\n")
  .key <- readRDS(CRED_KEYFILE)
} else {
  .key <- sodium::random(32)
  saveRDS(.key, CRED_KEYFILE)
  cat("[KEY] New 256-bit key created:", CRED_KEYFILE, "\n")
  cat("[KEY] *** Back this file up securely! Losing it = losing all credentials ***\n")
}

# ── Encrypt helpers ───────────────────────────────────────────────────────────
wt_encrypt <- function(obj) {
  msg   <- serialize(obj, NULL)
  nonce <- sodium::random(24)
  ct    <- sodium::data_encrypt(msg, .key, nonce)
  list(nonce=nonce, ct=ct, schema_version="3.0",
       created=format(Sys.time(), tz="UTC"))
}

save_enc <- function(obj, filename) {
  bundle    <- wt_encrypt(obj)
  tmp_named <- file.path(tempdir(), filename)
  saveRDS(bundle, tmp_named)
  drop_upload(tmp_named, path = drop.folder)
  file.copy(tmp_named, file.path("data", filename), overwrite = TRUE)
  unlink(tmp_named)
  cat("[OK] Saved encrypted file:", filename, "\n")
}

# ── Read CSV ──────────────────────────────────────────────────────────────────
cat("\n[1/4] Reading Credentials.csv...\n")

raw <- NULL

# Try Dropbox first
raw <- tryCatch({
  df <- drop_read_csv(file.path(drop.folder, "Credentials.csv"))
  cat("      Found in Dropbox:", drop.folder, "/Credentials.csv\n")
  df
}, error = function(e) NULL)

# Try local
if (is.null(raw)) {
  for (p in c("Credentials.csv", "data/Credentials.csv")) {
    if (file.exists(p)) {
      raw <- read.csv(p, stringsAsFactors = FALSE)
      cat("      Found locally:", p, "\n")
      break
    }
  }
}

if (is.null(raw)) stop("Credentials.csv not found. Place it next to this script or in Dropbox.")

cat("      Found", nrow(raw), "users:", paste(raw$user, collapse=", "), "\n")

# ── Validate columns ──────────────────────────────────────────────────────────
cat("\n[2/4] Validating columns...\n")
if (!"user"     %in% names(raw)) stop("Missing column: user")
if (!"password" %in% names(raw)) stop("Missing column: password")
if (!"site"     %in% names(raw)) raw$site  <- "default"
if (!"admin"    %in% names(raw)) raw$admin <- FALSE
if (!"email"    %in% names(raw)) raw$email <- ""
raw$admin <- as.logical(raw$admin)
cat("      Columns OK\n")

# ── Hash passwords ────────────────────────────────────────────────────────────
cat("\n[3/4] Hashing passwords with bcrypt (cost=12)...\n")
cat("      This takes ~0.3s per user\n")

raw$password_hash <- sapply(seq_len(nrow(raw)), function(i) {
  cat("      Hashing user:", raw$user[i], "...\n")
  bcrypt::hashpw(as.character(raw$password[i]), bcrypt::gensalt(log_rounds=12))
})

raw$password      <- NULL    # remove plain text
raw$active        <- TRUE
raw$created_at    <- format(Sys.time(), tz="UTC")
raw$created_by    <- "SYSTEM_MIGRATION"
raw$last_modified <- format(Sys.time(), tz="UTC")
raw$modified_by   <- "SYSTEM_MIGRATION"

cat("      All passwords hashed.\n")

# ── Encrypt & upload ──────────────────────────────────────────────────────────
cat("\n[4/4] Encrypting and uploading to Dropbox...\n")
save_enc(raw, CRED_FILE_ENC)

# ── Summary ───────────────────────────────────────────────────────────────────
cat("\n══════════════════════════════════════════\n")
cat(" Migration complete!\n\n")
cat(" Users migrated:\n")
for (i in seq_len(nrow(raw))) {
  role <- if (isTRUE(raw$admin[i])) "Admin" else "User"
  cat(sprintf("   %-12s  site=%-8s  %s\n", raw$user[i], raw$site[i], role))
}
cat("\n Files created:\n")
cat("   Dropbox:", drop.folder, "/", CRED_FILE_ENC, "\n")
cat("   Local:   data/", CRED_FILE_ENC, "\n")
cat("   Local:   data/cred.key  (keep this safe!)\n")
cat("\n Next steps:\n")
cat("   1. Verify app login works\n")
cat("   2. Delete Credentials.csv from Dropbox (passwords are now hashed)\n")
cat("══════════════════════════════════════════\n")
