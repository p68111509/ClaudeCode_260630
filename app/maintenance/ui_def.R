# ──────────────────────────────────────────────────────────────
# app/ui.R — 維運 Dashboard UI（BAD vs SAD 品管）
# ──────────────────────────────────────────────────────────────

ui <- shinydashboard::dashboardPage(
  skin = "black",

  shinydashboard::dashboardHeader(
    title = "GeoAI Copilot 維運", titleWidth = 240
  ),

  shinydashboard::dashboardSidebar(
    width = 240,
    shinydashboard::sidebarMenu(
      id = "tabs",
      shinydashboard::menuItem("總覽", tabName = "overview",
                               icon = shiny::icon("gauge-high")),
      shinydashboard::menuItem("BAD 明細（擋下）", tabName = "bad",
                               icon = shiny::icon("ban")),
      shinydashboard::menuItem("SAD 明細（觀察）", tabName = "sad",
                               icon = shiny::icon("magnifying-glass-chart")),
      shinydashboard::menuItem("趨勢", tabName = "trend",
                               icon = shiny::icon("chart-line")),
      shinydashboard::menuItem("API 金鑰狀態", tabName = "apis",
                               icon = shiny::icon("key")),
      shiny::br(),
      shiny::div(style = "padding:0 15px;",
        shiny::actionButton("refresh", "重新整理 / 重跑 QC",
                            icon = shiny::icon("rotate"),
                            width = "100%",
                            class = "btn-primary")),
      shiny::div(style = "padding:10px 15px; color:#9aa;",
                 shiny::textOutput("last_run"))
    )
  ),

  shinydashboard::dashboardBody(
    tags$head(tags$style(shiny::HTML("
      .small-box h3 { font-size: 30px; }
      .legend-pill { display:inline-block; padding:2px 10px; border-radius:999px;
                     color:#fff; font-size:12px; margin-right:6px; }
    "))),

    shinydashboard::tabItems(

      # ── 總覽 ──
      shinydashboard::tabItem(
        tabName = "overview",
        shiny::fluidRow(
          shinydashboard::valueBoxOutput("kpi_bad", width = 3),
          shinydashboard::valueBoxOutput("kpi_sad", width = 3),
          shinydashboard::valueBoxOutput("kpi_sources", width = 3),
          shinydashboard::valueBoxOutput("kpi_records", width = 3)
        ),
        shiny::fluidRow(
          shinydashboard::box(
            title = "來源 / 測站 品管狀態（紅=BAD擋下，黃=SAD觀察，綠=通過）",
            width = 7, status = "primary", solidHeader = TRUE,
            DT::DTOutput("status_tbl")
          ),
          shinydashboard::box(
            title = "BAD vs SAD 旗標分布", width = 5,
            status = "warning", solidHeader = TRUE,
            plotly::plotlyOutput("flag_bar", height = 320)
          )
        )
      ),

      # ── BAD ──
      shinydashboard::tabItem(
        tabName = "bad",
        shiny::fluidRow(shinydashboard::box(
          width = 12, status = "danger", solidHeader = TRUE,
          title = "🔴 BAD — 被擋下的紀錄（資料壞了，不可進模型）",
          DT::DTOutput("bad_tbl")
        ))
      ),

      # ── SAD ──
      shinydashboard::tabItem(
        tabName = "sad",
        shiny::fluidRow(shinydashboard::box(
          width = 12, status = "warning", solidHeader = TRUE,
          title = "🟡 SAD — 標記觀察的紀錄（合法但統計可疑）",
          DT::DTOutput("sad_tbl")
        )),
        shiny::fluidRow(shinydashboard::box(
          width = 12, status = "warning", solidHeader = TRUE,
          title = "測站時序（點選上表一列查看）",
          plotly::plotlyOutput("sad_series", height = 320)
        ))
      ),

      # ── 趨勢 ──
      shinydashboard::tabItem(
        tabName = "trend",
        shiny::fluidRow(shinydashboard::box(
          width = 12, status = "primary", solidHeader = TRUE,
          title = "BAD / SAD 計數隨時間（data/qc/qc_history.csv）",
          plotly::plotlyOutput("trend_plot", height = 380)
        ))
      ),

      # ── API ──
      shinydashboard::tabItem(
        tabName = "apis",
        shiny::fluidRow(shinydashboard::box(
          width = 12, status = "info", solidHeader = TRUE,
          title = "API 金鑰狀態（demo 模式不需金鑰）",
          DT::DTOutput("api_tbl")
        ))
      )
    )
  )
)
