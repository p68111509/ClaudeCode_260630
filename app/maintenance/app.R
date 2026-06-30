# ──────────────────────────────────────────────────────────────
# app/maintenance/app.R — 維運 Dashboard（附加）啟動點
# 與 copilot 相同的根目錄偵測邏輯。
# ──────────────────────────────────────────────────────────────

.root <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
while (!file.exists(file.path(.root, "DESCRIPTION")) &&
       dirname(.root) != .root) .root <- dirname(.root)
Sys.setenv(GEOAI_ROOT = .root)

source(file.path(.root, "R", "bootstrap.R"),                   encoding = "UTF-8")
source(file.path(.root, "app", "maintenance", "ui_def.R"),     encoding = "UTF-8")
source(file.path(.root, "app", "maintenance", "server_def.R"), encoding = "UTF-8")

shiny::shinyApp(ui = ui, server = server)
