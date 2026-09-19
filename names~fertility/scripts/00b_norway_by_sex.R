# 00b_norway_by_sex.R — re-pull Norway split by sex.
#
# The v0 fallback in 00_download.R returned name LABELS, not the coded values,
# and the code prefix (1 = girls, 2 = boys) was the only thing carrying sex.
# Five names sit on both lists, so sex cannot be recovered after the fact.
# Fix: ask for the girls' codes and the boys' codes in two separate requests.
#
# Output is ISO-8859-1, like everything from this API. Read it with
#   read.csv(..., fileEncoding = "latin1")

library(httr)
root <- path.expand("~/Desktop/the archive/names~fertility/data/norway")
UA   <- paste("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7)",
              "AppleWebKit/537.36 (KHTML, like Gecko)",
              "Chrome/140.0.0.0 Safari/537.36")
api  <- "https://data.ssb.no/api/v0/en/table/10467"

meta  <- content(GET(api, user_agent(UA)))
codes <- unlist(meta$variables[[1]]$values)

pull <- function(prefix, out) {
  sel  <- codes[substr(codes, 1, 1) == prefix]
  body <- list(
    query = list(
      list(code = "Fornavn",
           selection = list(filter = "item", values = as.list(sel))),
      list(code = "ContentsCode",
           selection = list(filter = "item",
                            values = list("PersonerProsent", "Personer")))),
    response = list(format = "csv2"))
  r <- POST(api, user_agent(UA), body = body, encode = "json")
  if (status_code(r) == 200) {
    writeBin(content(r, "raw"), file.path(root, out))
    message("OK  ", out, "  ", length(sel), " names, ",
            round(file.size(file.path(root, out)) / 1e6, 2), " MB")
  } else {
    message("FAILED ", out, "  status ", status_code(r))
  }
}

pull("1", "no_names_girls_1880_2025.csv")
pull("2", "no_names_boys_1880_2025.csv")

message("\nBoth files carry two content rows per name-year: ",
        "'Per cent of born persons' (complete 1880-2025, within-sex) and ",
        "'Born persons' (null in the early years).")
