# Central configuration -------------------------------------------------------
# Every tunable constant in the project lives here so that assumptions are
# visible in one place rather than scattered through the analysis.

CFG <- list(
  # ---- Data -------------------------------------------------------------
  data_url = "https://archive.ics.uci.edu/static/public/502/online+retail+ii.zip",
  raw_file = "online_retail_II.xlsx",
  sheet    = "Year 2010-2011",

  # ---- Cleaning ---------------------------------------------------------
  # Transactions above a Tukey outer fence (k x IQR in log space) are treated
  # as data-entry errors and the *entire* invoice is dropped, so both legs of a
  # matched cancellation go together. A rule rather than a hand-picked list of
  # invoice numbers, so it is reproducible and generalises.
  qty_fence_k = 3,

  # Fallback used only when >75% of rows share a single quantity, making the
  # IQR exactly zero and the fence above meaningless. Orders more than this
  # many times the median quantity are then flagged instead.
  qty_degenerate_multiple = 500,

  # ---- Modelling --------------------------------------------------------
  seed = 20240421,

  # BG/NBD + Pareto/NBD CLV horizon, in weeks. 52 weeks ~= 12 months.
  # ---- Censoring --------------------------------------------------------
  # Customers whose last purchase falls within this many days of the snapshot
  # are treated as still active (right-censored) rather than churned. The
  # purchase cycle in this dataset runs ~8-10 weeks, so 42 days is conservative.
  censor_window_days = 42,

  # Survival models must be checked against a non-parametric estimate. A mean
  # absolute gap above this between the Weibull and Kaplan-Meier curves means
  # the parametric fit does not describe the data and CLV should not be
  # published. See R/05_clv.R.
  max_survival_gap = 0.10,

  clv_horizon_weeks = 52,

  # ASSUMPTION, not a measurement. The UCI dataset carries no cost data, so
  # we assume a blended gross margin and apply it to convert revenue into
  # contribution profit. Override per deployment.
  gross_margin = 0.50,

  # Columns fed to k-means, after log transform and standardisation. Order is
  # not meaningful; membership is. Adding aov_log was tested and improves the
  # cluster balance (see reports/tables/diagnostics.md).
  cluster_features = c("recency_log", "frequency_log", "monetary_log", "aov_log"),

  # k-means
  k_max = 10,
  nstart = 25,
  n_boot = 200,

  # A partition whose bootstrap ARI falls below this is not reproducible
  # enough to act on, regardless of its silhouette. See choose_k().
  min_stability = 0.90,

  # ---- Output -----------------------------------------------------------
  currency = "GBP",
  currency_symbol = "\u00a3"
)

# Colour palette used across figures and the dashboard.
PALETTE <- c(
  primary   = "#7b2d8e",
  secondary = "#f73688",
  accent    = "#3d5a80",
  muted     = "#98c1d9",
  neutral   = "#4a4e69",
  warn      = "#ff5a36"
)
