# ──────────────────────────────────────────────────────────────
# R/functions/utils.R — 共用小工具
# ──────────────────────────────────────────────────────────────

# null/NA 安全的預設值運算子
`%||%` <- function(a, b) {
  if (is.null(a) || length(a) == 0) return(b)
  if (length(a) == 1 && is.na(a))   return(b)
  a
}

# 統一日誌格式
log_msg <- function(..., level = c("INFO", "WARN", "ERROR", "OK")) {
  level <- match.arg(level)
  ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  cat(sprintf("[%s] %-5s | %s\n", ts, level, paste0(...)))
  invisible(NULL)
}

# 載入 .env（若存在）
load_dotenv <- function(path) {
  if (file.exists(path)) {
    readRenviron(path)
    log_msg("已載入 .env：", path, level = "OK")
  } else {
    log_msg("未找到 .env（demo 模式可忽略）：", path, level = "WARN")
  }
  invisible(NULL)
}

# 確保資料夾存在
ensure_dir <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
  invisible(path)
}

# 標準化觀測資料的空殼（schema 單一來源）
empty_observations <- function() {
  tibble::tibble(
    source     = character(),
    station    = character(),
    county     = character(),
    lat        = double(),
    lon        = double(),
    datetime   = as.POSIXct(character()),
    variable   = character(),
    value      = double(),
    fetched_at = as.POSIXct(character())
  )
}

# 安全轉數值（"" / "-" / "ND" → NA）
as_num <- function(x) {
  x <- as.character(x)
  x[x %in% c("", "-", "ND", "NA", "x", "*")] <- NA
  suppressWarnings(as.numeric(x))
}
