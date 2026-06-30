# ──────────────────────────────────────────────────────────────
# app/server.R — 維運 Dashboard Server
# ──────────────────────────────────────────────────────────────

server <- function(input, output, session) {

  state <- shiny::reactiveVal(NULL)

  do_refresh <- function() {
    obs   <- standardize_observations(fetch_all())
    flags <- run_qc(obs, CONFIG)
    append_qc_history(flags, obs)
    state(list(
      obs   = obs,
      flags = flags,
      kpis  = qc_kpis(flags, obs),
      stat  = qc_source_status(flags, obs),
      ts    = Sys.time()
    ))
  }

  # 啟動時跑一次
  shiny::observeEvent(TRUE, do_refresh(), once = TRUE)
  shiny::observeEvent(input$refresh, do_refresh())

  output$last_run <- shiny::renderText({
    s <- state(); if (is.null(s)) return("尚未執行")
    paste0("最後更新：", format(s$ts, "%H:%M:%S"))
  })

  # ── KPI ──
  output$kpi_bad <- shinydashboard::renderValueBox({
    s <- state(); shiny::req(s)
    shinydashboard::valueBox(
      paste0(s$kpis$bad_rate, "%"),
      sprintf("BAD 率（擋下 %d 筆）", s$kpis$bad_n),
      icon = shiny::icon("ban"),
      color = if (s$kpis$bad_n > 0) "red" else "green")
  })
  output$kpi_sad <- shinydashboard::renderValueBox({
    s <- state(); shiny::req(s)
    shinydashboard::valueBox(
      paste0(s$kpis$sad_rate, "%"),
      sprintf("SAD 率（觀察 %d 筆）", s$kpis$sad_n),
      icon = shiny::icon("magnifying-glass-chart"),
      color = if (s$kpis$sad_n > 0) "yellow" else "green")
  })
  output$kpi_sources <- shinydashboard::renderValueBox({
    s <- state(); shiny::req(s)
    shinydashboard::valueBox(
      sprintf("%d / %d", s$kpis$sources_healthy, s$kpis$sources_total),
      "健康來源數", icon = shiny::icon("server"), color = "aqua")
  })
  output$kpi_records <- shinydashboard::renderValueBox({
    s <- state(); shiny::req(s)
    shinydashboard::valueBox(
      s$kpis$n_records, "本次檢查的測站×變數數",
      icon = shiny::icon("database"), color = "blue")
  })

  # ── 來源狀態表 ──
  output$status_tbl <- DT::renderDT({
    s <- state(); shiny::req(s)
    df <- s$stat |>
      dplyr::transmute(
        來源 = source,
        狀態 = dplyr::recode(status, red = "🔴 BAD", amber = "🟡 SAD",
                             green = "🟢 OK"),
        `BAD 數` = bad, `SAD 數` = sad)
    DT::datatable(df, rownames = FALSE,
                  options = list(dom = "t", pageLength = 20))
  })

  # ── 旗標長條圖 ──
  output$flag_bar <- plotly::renderPlotly({
    s <- state(); shiny::req(s)
    if (nrow(s$flags) == 0)
      return(plotly::plotly_empty(type = "scatter", mode = "markers") |>
               plotly::layout(title = "無旗標 🎉"))
    d <- s$flags |> dplyr::count(qc_type, check)
    plotly::plot_ly(d, x = ~check, y = ~n, color = ~qc_type, type = "bar",
                    colors = c(BAD = "#e2683f", SAD = "#edb24c")) |>
      plotly::layout(barmode = "stack", xaxis = list(title = ""),
                     yaxis = list(title = "筆數"), legend = list(title = list(text = "")))
  })

  # ── BAD / SAD 明細 ──
  flag_table <- function(type) {
    s <- state(); shiny::req(s)
    df <- dplyr::filter(s$flags, qc_type == type)
    if (nrow(df) == 0) df <- df[0, ]
    df |> dplyr::transmute(
      檢查 = check, 來源 = source, 測站 = station, 變數 = variable,
      數值 = value,
      時間 = format(datetime, "%m-%d %H:%M"),
      說明 = message)
  }
  output$bad_tbl <- DT::renderDT(
    DT::datatable(flag_table("BAD"), rownames = FALSE,
                  options = list(pageLength = 15)))
  output$sad_tbl <- DT::renderDT(
    DT::datatable(flag_table("SAD"), rownames = FALSE, selection = "single",
                  options = list(pageLength = 15)))

  # ── SAD：點選看時序 ──
  output$sad_series <- plotly::renderPlotly({
    s <- state(); shiny::req(s)
    sel <- input$sad_tbl_rows_selected
    sad <- flag_table("SAD")
    shiny::validate(shiny::need(length(sel) == 1 && nrow(sad) > 0,
                                "請於上表點選一列"))
    st <- sad$測站[sel]; va <- sad$變數[sel]
    ser <- s$obs |> dplyr::filter(station == st, variable == va) |>
      dplyr::arrange(datetime)
    plotly::plot_ly(ser, x = ~datetime, y = ~value, type = "scatter",
                    mode = "lines+markers", line = list(color = "#16a7bd")) |>
      plotly::layout(title = paste0(st, " · ", va),
                     xaxis = list(title = ""), yaxis = list(title = va))
  })

  # ── 趨勢 ──
  output$trend_plot <- plotly::renderPlotly({
    state()  # 依附 refresh
    h <- read_qc_history()
    shiny::validate(shiny::need(nrow(h) > 0, "尚無歷史資料"))
    agg <- h |> dplyr::group_by(run_at) |>
      dplyr::summarise(BAD = sum(bad_count), SAD = sum(sad_count),
                       .groups = "drop") |>
      tidyr::pivot_longer(c(BAD, SAD), names_to = "type", values_to = "n")
    plotly::plot_ly(agg, x = ~run_at, y = ~n, color = ~type,
                    type = "scatter", mode = "lines",
                    colors = c(BAD = "#e2683f", SAD = "#edb24c")) |>
      plotly::layout(xaxis = list(title = ""), yaxis = list(title = "筆數"),
                     legend = list(title = list(text = "")))
  })

  # ── API 金鑰狀態 ──
  output$api_tbl <- DT::renderDT({
    df <- api_key_status() |>
      dplyr::transmute(服務 = service, 名稱 = name,
                       金鑰 = ifelse(has_key, "✅ 已設定", "—"),
                       文件 = docs)
    DT::datatable(df, rownames = FALSE, escape = FALSE,
                  options = list(dom = "t", pageLength = 20))
  })
}
