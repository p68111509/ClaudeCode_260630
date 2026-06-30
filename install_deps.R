# ──────────────────────────────────────────────────────────────
# install_deps.R — 安裝原型所需套件（首次執行一次）
# ──────────────────────────────────────────────────────────────

pkgs <- c("shiny", "shinydashboard", "DT", "dplyr", "tidyr",
          "lubridate", "plotly", "glue", "httr2", "jsonlite",
          "config", "tibble", "mapgl", "sf", "bslib", "testthat")

to_install <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]

if (length(to_install)) {
  message("安裝套件：", paste(to_install, collapse = ", "))
  install.packages(to_install, repos = "https://cloud.r-project.org")
} else {
  message("所有套件已安裝 ✅")
}
