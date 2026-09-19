# 00c_download_fertility.R — the outcome variable.
# HFD for register-based TFR (long series), UN WPP 1950- to fill the gaps.
# Same defensive pattern as 00_download.R: browser UA, validate, never trust
# a 200 response to mean you got data.

root <- path.expand("~/Desktop/the archive/names~fertility/data/fertility")
dir.create(root, recursive = TRUE, showWarnings = FALSE)
UA <- paste("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7)",
            "AppleWebKit/537.36 (KHTML, like Gecko)",
            "Chrome/140.0.0.0 Safari/537.36")
options(HTTPUserAgent = UA)

try_get <- function(url, path, min_bytes = 2e4) {
  ok <- tryCatch({
    download.file(url, path, mode = "wb", quiet = TRUE, method = "libcurl"); TRUE
  }, error = function(e) FALSE)
  if (ok && file.exists(path)) {
    sz <- file.size(path)
    hd <- tryCatch(readChar(path, 200, useBytes = TRUE), error = function(e) "")
    if (grepl("^\\s*<", hd) || sz < min_bytes) { unlink(path); ok <- FALSE }
  }
  if (ok) message("   OK  ", basename(path), "  ", round(file.size(path)/1e3), " KB")
  ok
}

# ---- HFD: period TFR, all countries, one 95 KB zip --------------------------
message("HFD tfr.zip ...")
hfd <- file.path(root, "hfd_tfr.zip")
if (!try_get("https://www.humanfertility.org/File/Download/Files/zip/tfr.zip",
             hfd, min_bytes = 3e4)) {
  message("   FAILED. HFD may require a (free, no-affiliation) login.\n",
          "   Register at https://www.humanfertility.org, then download\n",
          "   'By statistic' -> tfr.zip and save it to data/fertility/hfd_tfr.zip")
} else {
  unzip(hfd, exdir = file.path(root, "hfd_tfr"))
  message("   unzipped: ", length(list.files(file.path(root, "hfd_tfr"))), " files")
}

# ---- UN WPP: demographic indicators, 1950- ----------------------------------
# The 2024 site reorganised its paths, so try the known candidates in order.
message("UN WPP demographic indicators ...")
cands <- c(
  paste0("https://population.un.org/wpp/assets/Excel%20Files/",
         "1_Indicator%20(Standard)/CSV_FILES/",
         "WPP2024_Demographic_Indicators_Medium.csv.gz"),
  paste0("https://population.un.org/wpp/Download/Files/",
         "1_Indicator%20(Standard)/CSV_FILES/",
         "WPP2024_Demographic_Indicators_Medium.csv.gz"),
  paste0("https://population.un.org/wpp/assets/Excel%20Files/",
         "1_Indicator%20(Standard)/CSV_FILES/",
         "WPP2022_Demographic_Indicators_Medium.zip")
)
wpp <- file.path(root, "wpp_demographic_indicators.csv.gz")
hit <- FALSE
for (u in cands) if (!hit) hit <- try_get(u, wpp, min_bytes = 1e6)
if (!hit) {
  message("   FAILED on all candidates. Grab it by hand:\n",
          "   https://population.un.org/wpp/downloads  ->  CSV format\n",
          "   -> 'Demographic Indicators' (medium variant)\n",
          "   Save as data/fertility/wpp_demographic_indicators.csv.gz")
}

message("\nScript A needs at least one of these two. HFD is preferred for the\n",
        "pre-1950 years, which is exactly where our deep countries live.")
