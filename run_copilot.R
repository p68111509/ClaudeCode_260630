# ──────────────────────────────────────────────────────────────
# run_copilot.R — 啟動 GeoAI Copilot 產品本體（主應用）
# 用法：
#   setwd("C:/Claude/GeoAI-project")
#   source("run_copilot.R")
# ──────────────────────────────────────────────────────────────

local({
  this_file <- tryCatch(normalizePath(sys.frame(1)$ofile, winslash = "/",
                                      mustWork = FALSE), error = function(e) NA)
  if (is.na(this_file) || !nzchar(this_file)) {
    args <- commandArgs(trailingOnly = FALSE)
    fa <- sub("^--file=", "", args[grepl("^--file=", args)])
    if (length(fa)) this_file <- normalizePath(fa[1], winslash = "/",
                                               mustWork = FALSE)
  }
  root <- if (!is.na(this_file) && nzchar(this_file)) dirname(this_file)
          else normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  if (!file.exists(file.path(root, "DESCRIPTION"))) {
    stop("無法定位專案根目錄。請先 setwd(\"C:/Claude/GeoAI-project\") 再 source。")
  }
  Sys.setenv(GEOAI_ROOT = root); setwd(root)
})

shiny::runApp(file.path(Sys.getenv("GEOAI_ROOT"), "app", "copilot"),
              launch.browser = TRUE)
