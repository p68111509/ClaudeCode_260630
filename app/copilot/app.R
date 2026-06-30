# ──────────────────────────────────────────────────────────────
# app/copilot/app.R — GeoAI Copilot 產品本體 啟動點
# 乾淨啟動：偵測根目錄 → bootstrap → UI/Server → shinyApp。
# 根目錄偵測為「從目前工作目錄往上找 DESCRIPTION」，與資料夾深度無關。
# ──────────────────────────────────────────────────────────────

.root <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
while (!file.exists(file.path(.root, "DESCRIPTION")) &&
       dirname(.root) != .root) .root <- dirname(.root)
Sys.setenv(GEOAI_ROOT = .root)

source(file.path(.root, "R", "bootstrap.R"),               encoding = "UTF-8")
source(file.path(.root, "app", "copilot", "ui_def.R"),     encoding = "UTF-8")
source(file.path(.root, "app", "copilot", "server_def.R"), encoding = "UTF-8")

shiny::shinyApp(ui = ui, server = server)
