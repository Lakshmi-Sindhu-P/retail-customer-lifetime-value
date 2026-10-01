# 01 - Load and clean ----------------------------------------------------------
# Source: UCI Online Retail II, sheet "Year 2010-2011".

#' Download the raw dataset if it is not already present
fetch_data <- function() {
  dir.create("data/raw", showWarnings = FALSE, recursive = TRUE)
  dest <- file.path("data/raw", CFG$raw_file)
  if (file.exists(dest) && file.size(dest) > 0) {
    log_step("raw file already present (%s)", basename(dest))
    return(dest)
  }
  zip <- tempfile(fileext = ".zip")
  log_step("downloading %s", CFG$data_url)
  utils::download.file(CFG$data_url, zip, mode = "wb")
  utils::unzip(zip, exdir = "data/raw")
  unlink(zip)
  log_step("extracted %s", basename(dest))
  dest
}

#' Load the raw sheet and normalise column names
load_raw <- function() {
  path <- fetch_data()
  log_step("reading sheet '%s'", CFG$sheet)

  raw <- readxl::read_excel(path, sheet = CFG$sheet)

  # The source workbook has human-readable headers with spaces; the canonical
  # schema is the one published with the dataset.
  names(raw) <- c("InvoiceNo", "StockCode", "Description", "Quantity",
                  "InvoiceDate", "UnitPrice", "CustomerID", "Country")

  raw
}

#' Clean the raw transaction log
#'
#' Rules, in order. Each is a *rule*, not a hand-picked list of rows:
#'
#'   1. Drop rows with no CustomerID. ~25% of lines; these are guest/website
#'      sessions and cannot be attributed to a customer.
#'   2. Drop rows with no Description. Product analysis needs a description.
#'   3. Drop cancellations: Quantity <= 0, UnitPrice <= 0, or an invoice
#'      number with the "C" cancellation prefix.
#'   4. Drop *entire* invoices containing a quantity outlier. Dropping only
#'      the negative leg would leave the original order in place, which is how
#'      the original notebook ended up needing two hardcoded invoice numbers.
#'
#' @param qty_fence_k Tukey-fence multiplier. See [qty_outlier_threshold].
#' @return list(data, audit) where audit records what was removed and why
#' Threshold above which a line-item quantity is treated as a data-entry error
#'
#' A quantile threshold looks reasonable but is fragile: if outliers make up
#' more than (1 - q) of all rows, the quantile lands *on* the outliers and the
#' rule silently flags nothing. On a small fixture with 2.4% extreme rows, a
#' 99.9th-percentile cut caught zero of them.
#'
#' A Tukey outer fence in log space is used instead. Log space makes the rule
#' scale-adaptive (a 500-unit order and a 50,000-unit order are both measured
#' against their own distribution rather than one global ceiling), and the
#' 3*IQR width tolerates a heavy tail without swallowing it. The fence adapts
#' to how heavy the tail actually is.
#'
#' @return numeric threshold on the original Quantity scale
qty_outlier_threshold <- function(quantity, fence_k = CFG$qty_fence_k) {
  quantity <- quantity[is.finite(quantity)]
  if (!length(quantity)) return(Inf)

  lq <- log1p(quantity)
  q3 <- stats::quantile(lq, 0.75, names = FALSE)
  iqr <- stats::IQR(lq)

  # Degenerate fallback. Whenever more than 75% of rows share one quantity --
  # which happens on small or highly-quantised data -- the IQR is exactly 0,
  # the fence collapses onto Q3, and the rule becomes either useless (it
  # flags everything) or inverted. In that regime the distribution carries no
  # usable scale information, so fall back to an order-of-magnitude rule:
  # flag quantities more than CFG$qty_degenerate_multiple x the median.
  if (!is.finite(iqr) || iqr <= 0) {
    return(stats::median(quantity) * CFG$qty_degenerate_multiple)
  }

  fence <- q3 + fence_k * iqr
  if (!is.finite(fence)) return(max(quantity))

  expm1(fence)
}

clean_transactions <- function(raw) {
  audit <- list()
  d <- raw

  audit$raw_rows <- nrow(d)
  audit$raw_missing_customerid <- sum(is.na(d$CustomerID))
  audit$raw_missing_description <- sum(is.na(d$Description))

  # 1 -- unattributable lines
  d <- d[!is.na(d$CustomerID), ]
  audit$after_drop_customerid <- nrow(d)

  # 2 -- missing description
  d <- d[!is.na(d$Description) & trimws(d$Description) != "", ]
  audit$after_drop_description <- nrow(d)

  # 3 -- cancellations and non-positive pricing
  is_cancel <- grepl("^C", d$InvoiceNo)
  bad_qty   <- d$Quantity <= 0
  bad_price <- d$UnitPrice <= 0
  audit$cancel_rows   <- sum(is_cancel)
  audit$nonpos_qty    <- sum(bad_qty)
  audit$nonpos_price  <- sum(bad_price)
  d <- d[!is_cancel & !bad_qty & !bad_price, ]
  audit$after_drop_cancellations <- nrow(d)

  # 4 -- quantity outliers, removing the whole invoice
  thresh <- qty_outlier_threshold(d$Quantity)
  suspect <- unique(d$InvoiceNo[d$Quantity > thresh])
  audit$qty_outlier_threshold <- thresh
  audit$qty_outlier_invoices  <- length(suspect)
  audit$qty_outlier_rows      <- sum(d$InvoiceNo %in% suspect)
  d <- d[!d$InvoiceNo %in% suspect, ]
  audit$clean_rows <- nrow(d)

  d <- d %>%
    mutate(CustomerID = as.integer(CustomerID),
           InvoiceNo  = as.character(InvoiceNo),
           StockCode  = as.character(StockCode),
           Country    = as.character(Country))

  audit$clean_customers   <- n_distinct(d$CustomerID)
  audit$clean_invoices    <- n_distinct(d$InvoiceNo)
  audit$clean_products    <- n_distinct(d$Description)
  audit$clean_countries   <- n_distinct(d$Country)
  audit$revenue           <- sum(d$Quantity * d$UnitPrice)

  list(data = d, audit = audit)
}

#' Descriptive summary used in the report
summarise_clean <- function(d) {
  tibble::tibble(
    metric = c("rows", "customers", "invoices", "products", "countries",
               "revenue", "date_min", "date_max"),
    value  = c(nrow(d), n_distinct(d$CustomerID), n_distinct(d$InvoiceNo),
               n_distinct(d$Description), n_distinct(d$Country),
               round(sum(d$Quantity * d$UnitPrice), 2),
               format(min(d$InvoiceDate), "%Y-%m-%d"),
               format(max(d$InvoiceDate), "%Y-%m-%d"))
  )
}
