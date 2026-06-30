# ──────────────────────────────────────────────────────────────
# app/copilot/ui_def.R — GeoAI Copilot 產品本體 UI
# 滿版地圖 + 疊在圖上、可收合的「路徑比較」浮動面板（仿參考介面）。
# ──────────────────────────────────────────────────────────────

.pois <- demo_pois()$name

.copilot_css <- "
.map-card { position:relative; height:100%; padding:0 !important; overflow:hidden; }
.map-card .maplibregl-map { height:100% !important; }

.metrics-overlay { position:absolute; top:14px; left:14px; z-index:500;
  display:flex; gap:8px; flex-wrap:wrap; }
.metric-chip { background:rgba(255,255,255,.93); border-radius:10px;
  padding:6px 13px; box-shadow:0 2px 12px rgba(0,0,0,.18); font-size:12px;
  color:#5a5a5a; text-align:center; line-height:1.2; }
.metric-chip b { font-size:19px; display:block; margin-top:2px; }
.metric-chip.air b { color:#0e8fa3; } .metric-chip.green b { color:#3a9d4e; }
.metric-chip.time b { color:#666; }

.copilot-float { position:absolute; right:16px; top:16px; bottom:16px; z-index:600;
  width:340px; max-width:calc(100% - 32px);
  display:flex; flex-direction:column; background:#fff; border-radius:14px;
  box-shadow:0 12px 34px rgba(0,0,0,.30); overflow:hidden; }
.copilot-head { cursor:pointer; padding:11px 15px; background:#0e8fa3; color:#fff;
  font-weight:700; display:flex; justify-content:space-between; align-items:center;
  user-select:none; flex:0 0 auto; }
.copilot-head .chev { transition:transform .2s; font-size:13px; }
.copilot-float.collapsed { top:auto; bottom:16px; }
.copilot-float.collapsed .copilot-body { display:none; }
.copilot-float.collapsed .copilot-head .chev { transform:rotate(180deg); }
.copilot-body { padding:13px; overflow-y:auto; flex:1 1 auto; }
.copilot-input { display:flex; gap:6px; margin-top:8px; align-items:flex-start;
  flex:0 0 auto; padding:10px 13px; border-top:1px solid #eee; }
.copilot-input .shiny-input-container { margin-bottom:0; flex:1; width:auto !important; }

/* 路徑比較區塊 */
.pc-sec { border:1px solid #eee; border-radius:12px; padding:11px 13px; margin-bottom:10px; }
.pc-sec.hl { border-color:#3a9d4e; }
.pc-h { font-weight:700; font-size:13px; color:#555; margin-bottom:7px; }
.pc-row { display:flex; justify-content:space-between; align-items:baseline; }
.pc-big { font-size:24px; font-weight:800; }
.pc-big.air { color:#0e8fa3; } .pc-big.green { color:#3a9d4e; }
.pc-bar { height:8px; border-radius:6px; background:#eee; overflow:hidden; margin-top:7px; }
.pc-fill { height:100%; border-radius:6px; background:linear-gradient(90deg,#5ad,#0e8fa3); }
.pc-stats { display:flex; gap:10px; }
.pc-stat { text-align:center; flex:1; }
.pc-stat b { display:block; font-size:19px; color:#333; }
.pc-stat span { font-size:11px; color:#999; }
.pc-trees { font-size:15px; letter-spacing:3px; margin-top:3px; }
.pc-note { font-size:11px; color:#aaa; margin-top:2px; }
.pc-dl { margin-top:6px; }
.pc-dl .lab { font-size:12px; color:#666; display:flex; justify-content:space-between; }
.pc-dline { height:9px; border-radius:6px; margin:3px 0 7px; }
.pc-explain { font-size:13px; color:#333; line-height:1.6; }
"

ui <- bslib::page_sidebar(
  title = "GeoAI Copilot · 呼吸路徑",
  theme = bslib::bs_theme(
    version = 5, primary = "#0e8fa3",
    base_font = bslib::font_collection("Microsoft JhengHei", "PingFang TC",
                                       "system-ui", "sans-serif")
  ),
  fillable = TRUE,
  shiny::tags$head(shiny::tags$style(shiny::HTML(.copilot_css))),

  sidebar = bslib::sidebar(
    width = 300,
    shiny::p(class = "text-muted mb-1", "空污健康導航原型 · 台南示範區"),
    shiny::selectInput("group", "你的族群",
                       c("一般民眾", "氣喘患者", "孕婦", "年長者", "兒童")),
    shiny::selectInput("origin", "起點", choices = .pois, selected = .pois[1]),
    shiny::selectInput("dest",   "終點", choices = .pois, selected = .pois[2]),
    shiny::radioButtons("pollutant", "暴露依據污染物",
                        c("PM2.5" = "pm25", "AQI" = "aqi",
                          "NO₂" = "no2", "O₃" = "o3"),
                        selected = "pm25", inline = TRUE),
    shiny::sliderInput("height", "身高 (cm)", min = 140, max = 200,
                       value = 170, step = 1),
    shiny::div(class = "d-flex gap-2",
      shiny::actionButton("plan", "規劃", class = "btn-primary flex-fill",
                          icon = shiny::icon("route")),
      shiny::actionButton("reset", "重置", class = "btn-outline-secondary")),
    shiny::hr(),
    shiny::HTML(
      '<div style="font-size:13px;line-height:1.9">
         <div><span style="display:inline-block;width:26px;height:4px;
              background:#0e8fa3;vertical-align:middle"></span> 低暴露路徑（推薦）</div>
         <div><span style="display:inline-block;width:26px;height:4px;
              background:#5b54c9;vertical-align:middle"></span> 最短路線</div>
         <div><span style="display:inline-block;width:14px;height:14px;border-radius:50%;
              background:#7bd389;vertical-align:middle"></span> 綠地</div>
         <div style="margin-top:6px">色塊：污染暴露推估（綠→紅）</div>
       </div>')
  ),

  bslib::card(
    full_screen = TRUE,
    bslib::card_body(
      class = "map-card",
      mapgl::maplibreOutput("map", height = "100%"),
      shiny::div(class = "metrics-overlay", shiny::uiOutput("metrics")),

      shiny::div(
        class = "copilot-float", id = "copilotFloat",
        shiny::div(
          class = "copilot-head",
          onclick = "this.closest('.copilot-float').classList.toggle('collapsed')",
          shiny::span("📊 路徑比較"), shiny::span(class = "chev", "▾")
        ),
        shiny::div(class = "copilot-body",
                   shiny::uiOutput("panel"),
                   shiny::uiOutput("qa")),
        shiny::div(class = "copilot-input",
          shiny::textInput("q", NULL, width = "100%",
                           placeholder = "想追問什麼？例如：為什麼繞路？"),
          shiny::actionButton("ask", "問", class = "btn-secondary"))
      )
    )
  )
)
