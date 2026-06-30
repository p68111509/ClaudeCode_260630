# ──────────────────────────────────────────────────────────────
# R/bootstrap.R — 啟動載入邏輯集中地
# 載入順序：套件 → utils → .env → 設定 → API → functions → models → llm
# app/app.R 只需 source 此檔，保持啟動乾淨。
# ──────────────────────────────────────────────────────────────

suppressPackageStartupMessages({
  .pkgs <- c("shiny", "shinydashboard", "DT", "dplyr", "tidyr",
             "lubridate", "plotly", "glue", "httr2", "jsonlite",
             "config", "tibble", "mapgl", "sf", "bslib")
  .missing <- .pkgs[!vapply(.pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(.missing)) {
    stop("缺少套件，請先執行 source('install_deps.R')。缺少：",
         paste(.missing, collapse = ", "))
  }
  for (p in .pkgs) library(p, character.only = TRUE)
})

# 專案根目錄偵測（不依賴 here，避免其跨 session 的根目錄快取問題）：
#   1) 優先用環境變數 GEOAI_ROOT（由 run_dashboard.R / app.R 設定）
#   2) 否則從目前工作目錄往上層尋找含 DESCRIPTION 的資料夾
.find_root <- function() {
  env <- Sys.getenv("GEOAI_ROOT", unset = "")
  if (nzchar(env) && file.exists(file.path(env, "DESCRIPTION"))) {
    return(normalizePath(env, winslash = "/", mustWork = FALSE))
  }
  d <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  repeat {
    if (file.exists(file.path(d, "DESCRIPTION"))) return(d)
    parent <- dirname(d)
    if (parent == d) break
    d <- parent
  }
  stop("找不到專案根目錄（需含 DESCRIPTION）。請先 setwd() 到專案根目錄，",
       "或設定 Sys.setenv(GEOAI_ROOT='C:/Claude/GeoAI-project')。")
}
ROOT <- .find_root()
Sys.setenv(GEOAI_ROOT = ROOT)   # 供子流程沿用
.src <- function(...) source(file.path(ROOT, ...), encoding = "UTF-8")

# 1) 工具（其餘檔案會用到）
.src("R", "functions", "utils.R")

# 2) .env（金鑰）
load_dotenv(file.path(ROOT, ".env"))

# 3) 設定
CONFIG <- config::get(
  file   = file.path(ROOT, "config", "config.yml"),
  config = Sys.getenv("R_CONFIG_ACTIVE", "default")
)
Sys.setenv(TZ = CONFIG$app$timezone %||% "Asia/Taipei")

# 4) API（集中端點）
.src("R", "api", "endpoints.R")

# 5) functions（全部，定義順序不影響）
for (f in list.files(file.path(ROOT, "R", "functions"),
                     pattern = "[.]R$", full.names = TRUE)) {
  source(f, encoding = "UTF-8")
}

# 6) models
for (f in list.files(file.path(ROOT, "R", "models"),
                     pattern = "[.]R$", full.names = TRUE)) {
  source(f, encoding = "UTF-8")
}

# 7) llm 接點
.src("R", "llm", "explain.R")

# 8) 確保資料夾與歷史檔
ensure_dir(file.path(ROOT, CONFIG$paths$data_raw))
ensure_dir(file.path(ROOT, CONFIG$paths$data_processed))
ensure_dir(file.path(ROOT, dirname(CONFIG$paths$qc_history)))
if (isTRUE(CONFIG$app$demo_mode)) seed_demo_history()  # 預載示範趨勢

log_msg("Bootstrap 完成 | demo_mode = ", CONFIG$app$demo_mode,
        " | region = ", CONFIG$app$region, level = "OK")
