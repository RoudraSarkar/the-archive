# 00_download.R — fetch the remaining name files
# Writes into names~fertility/data/<country>/.
#
# NOTE: both hosts reject plain R downloads.
#   * data.govt.nz sits behind bot protection ("Pardon Our Interruption"),
#     which returns an HTML challenge page under a .csv filename.
#   * data.ssb.no returned 503 to R's default agent while serving the same
#     URL fine to a browser.
# So: send a browser user-agent, retry on failure, and VALIDATE what came
# back — the dangerous failure mode is a small HTML file that looks like a
# successful download.

root <- "~/Desktop/the archive/names~fertility/data"
UA   <- paste("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7)",
              "AppleWebKit/537.36 (KHTML, like Gecko)",
              "Chrome/140.0.0.0 Safari/537.36")
options(HTTPUserAgent = UA)

get <- function(url, path, min_bytes = 5e4, tries = 4) {
  path <- path.expand(file.path(root, path))
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  for (i in seq_len(tries)) {
    message("-> ", basename(path), "  (attempt ", i, ")")
    ok <- tryCatch({
      download.file(url, path, mode = "wb", quiet = TRUE, method = "libcurl")
      TRUE
    }, error = function(e) { message("   ", conditionMessage(e)); FALSE })

    if (ok && file.exists(path)) {
      sz <- file.size(path)
      hd <- tryCatch(readChar(path, 200, useBytes = TRUE), error = function(e) "")
      if (grepl("^\\s*<", hd)) {
        message("   REJECTED: HTML, not data (bot check).")
        unlink(path); ok <- FALSE
      } else if (sz < min_bytes) {
        message("   REJECTED: only ", sz, " bytes.")
        unlink(path); ok <- FALSE
      } else {
        message("   OK  ", round(sz / 1e6, 2), " MB")
        return(invisible(TRUE))
      }
    }
    if (i < tries) Sys.sleep(3 * i)
  }
  message("   FAILED after ", tries, " attempts: ", basename(path))
  invisible(FALSE)
}

# ---- Norway ----------------------------------------------------------------
# SSB StatBank table 10467, 1880-2025, 1,974 names. Wide: one row per name,
# one column per year. Name codes prefixed 1 = girls, 2 = boys.
no <- paste0("https://data.ssb.no/api/pxwebapi/v2-beta/tables/10467/data",
             "?lang=en&outputFormat=csv",
             "&valuecodes%5BTid%5D=*",
             "&valuecodes%5BFornavn%5D=*",
             "&valuecodes%5BContentsCode%5D=")

ok_pct <- get(paste0(no, "PersonerProsent"),
              "norway/no_names_pct_1880_2025.csv", min_bytes = 1e6)
ok_cnt <- get(paste0(no, "Personer"),
              "norway/no_names_counts_1880_2025.csv", min_bytes = 3e5)

# Fallback: the older, stable v0 API. POST only, so it needs httr.
if (!ok_pct && requireNamespace("httr", quietly = TRUE)) {
  message("\nFalling back to the v0 API ...")
  meta <- httr::content(httr::GET("https://data.ssb.no/api/v0/en/table/10467",
                                  httr::user_agent(UA)))
  nm <- unlist(meta$variables[[1]]$values)
  body <- list(
    query = list(
      list(code = "Fornavn",
           selection = list(filter = "item", values = as.list(nm))),
      list(code = "ContentsCode",
           selection = list(filter = "item", values = list("PersonerProsent")))),
    response = list(format = "csv2"))
  r <- httr::POST("https://data.ssb.no/api/v0/en/table/10467",
                  httr::user_agent(UA), body = body, encode = "json")
  if (httr::status_code(r) == 200) {
    writeBin(httr::content(r, "raw"),
             path.expand(file.path(root, "norway/no_names_pct_1880_2025.csv")))
    message("v0 fallback OK")
  } else {
    message("v0 fallback also failed, status ", httr::status_code(r))
  }
}

# ---- New Zealand -----------------------------------------------------------
# DIA via data.govt.nz. Sex and NUMBER of birth registrations per first name,
# 1900 onward. Years grouped by REGISTRATION date, not birth date.
# This one will probably fail: the host runs a JavaScript bot challenge that a
# scripted request cannot pass. If it does, open this URL in Chrome and save
# the file as  data/new zealand/nz_baby_names_1900_2025.csv
get(paste0("https://catalogue.data.govt.nz/dataset/",
           "01ee87cd-ecf8-44a1-ad33-b376a689e597/resource/",
           "0b0b326c-d720-480f-8f86-bf2d221c7d3f/download/",
           "baby-names-1900-to-2025.csv"),
    "new zealand/nz_baby_names_1900_2025.csv", min_bytes = 2e5)

message("\nNetherlands is deliberately absent - see DATA_CHECKLIST.md.")
