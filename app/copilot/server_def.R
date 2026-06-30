# ──────────────────────────────────────────────────────────────
# app/copilot/server_def.R — GeoAI Copilot 產品本體 Server
# 流程：暴露網格(A,可選污染物) → 規劃路徑(B,ORS/幾何) → 綠覆(C,Sentinel) → 解釋(D,LLM)
# ──────────────────────────────────────────────────────────────

server <- function(input, output, session) {

  pois <- demo_pois()
  POLL_LAB <- list(pm25 = "PM2.5 (µg/m³)", aqi = "AQI",
                   no2 = "NO₂ (ppb)", o3 = "O₃ (ppb)")

  # ── 取資料 → 濾 BAD（pm25）→ 篩示範區 ──
  rng <- CONFIG$qc$bad$ranges$pm25
  obs <- standardize_observations(fetch_all()) |>
    dplyr::filter(!(variable == "pm25" &
                    (is.na(value) | value < rng[1] | value > rng[2])))
  bb <- CONFIG$app$region_bbox
  if (!is.null(bb)) {
    obs_r <- dplyr::filter(obs, !is.na(lat), !is.na(lon),
                           lat >= bb$lat[[1]], lat <= bb$lat[[2]],
                           lon >= bb$lon[[1]], lon <= bb$lon[[2]])
    if (dplyr::n_distinct(obs_r$station) >= 3) obs <- obs_r
  }

  latest_var <- function(v) {
    obs |>
      dplyr::filter(variable == v, !is.na(value), value >= 0) |>
      dplyr::group_by(station) |>
      dplyr::slice_max(datetime, n = 1, with_ties = FALSE) |>
      dplyr::ungroup()
  }
  pm_now  <- round(stats::median(latest_var("pm25")$value, na.rm = TRUE), 0)
  aqi_now <- round(pm_now * 2.1, 0)

  # ── 依污染物建暴露網格（隨 radio 切換）──
  grid_r <- shiny::reactive({
    v <- input$pollutant %||% "pm25"
    g <- predict_exposure(obs, v, grid_n = 22)
    if (nrow(g) < 9) { v <- "pm25"; g <- predict_exposure(obs, "pm25", grid_n = 22) }
    list(var = v, grid = g, stations = latest_var(v))
  })

  line_sf <- function(route, nm) sf::st_sf(
    name = nm,
    geometry = sf::st_sfc(
      sf::st_linestring(as.matrix(route[, c("lon", "lat")])), crs = 4326))

  # ── 地圖（隨 grid 與規劃結果重繪）──
  output$map <- mapgl::renderMaplibre({
    gr <- grid_r(); grid <- gr$grid; st <- gr$stations
    p  <- res()
    vrng  <- range(c(grid$value, st$value), na.rm = TRUE)
    vals4 <- seq(vrng[1], vrng[2], length.out = 4)
    cols4 <- c("#2ecc71", "#f1c40f", "#e67e22", "#e74c3c")

    cl <- sort(unique(grid$lon)); ct <- sort(unique(grid$lat))
    dlon <- if (length(cl) > 1) diff(cl)[1] / 2 else 0.004
    dlat <- if (length(ct) > 1) diff(ct)[1] / 2 else 0.004
    polys <- lapply(seq_len(nrow(grid)), function(i) sf::st_polygon(list(rbind(
      c(grid$lon[i]-dlon, grid$lat[i]-dlat), c(grid$lon[i]+dlon, grid$lat[i]-dlat),
      c(grid$lon[i]+dlon, grid$lat[i]+dlat), c(grid$lon[i]-dlon, grid$lat[i]+dlat),
      c(grid$lon[i]-dlon, grid$lat[i]-dlat)))))
    grid_sf  <- sf::st_sf(value = grid$value, geometry = sf::st_sfc(polys, crs = 4326))
    green_sf <- sf::st_as_sf(demo_green_centers(), coords = c("lon","lat"), crs = 4326)
    st_sf    <- sf::st_as_sf(st, coords = c("lon","lat"), crs = 4326)

    m <- mapgl::maplibre(style = mapgl::carto_style("positron"),
                         center = c(mean(range(grid$lon)), mean(range(grid$lat))),
                         zoom = 12.5) |>
      mapgl::add_source("grid", grid_sf) |>
      mapgl::add_fill_layer("grid_l", "grid",
        fill_color = mapgl::interpolate(column = "value", values = vals4, stops = cols4),
        fill_opacity = 0.35) |>
      mapgl::add_source("green", green_sf) |>
      mapgl::add_circle_layer("green_l", "green", circle_color = "#7bd389",
        circle_opacity = 0.22, circle_radius = 26, circle_blur = 0.6) |>
      mapgl::add_source("stations", st_sf) |>
      mapgl::add_circle_layer("st_l", "stations",
        circle_color = mapgl::interpolate(column = "value", values = vals4, stops = cols4),
        circle_radius = 6, circle_stroke_color = "#222", circle_stroke_width = 1,
        tooltip = "station") |>
      mapgl::add_continuous_legend(POLL_LAB[[gr$var]],
        values = as.character(round(vals4)), colors = cols4, position = "bottom-left")

    if (!is.null(p)) {
      brth <- line_sf(p$pr$breathing, "低暴露路徑")
      od <- sf::st_as_sf(data.frame(
        name = c(paste("起點：", p$o$name), paste("終點：", p$d$name)),
        lon = c(p$o$lon, p$d$lon), lat = c(p$o$lat, p$d$lat)),
        coords = c("lon","lat"), crs = 4326)
      m <- m |>
        mapgl::add_source("fast", line_sf(p$pr$fastest, "最短路線")) |>
        mapgl::add_line_layer("fast_l", "fast", line_color = "#5b54c9",
          line_width = 5, line_opacity = 0.9) |>
        mapgl::add_source("breath", brth) |>
        mapgl::add_line_layer("breath_l", "breath", line_color = "#0e8fa3",
          line_width = 6, line_opacity = 0.95) |>
        mapgl::add_markers(od, color = "#0e8fa3", popup = "name") |>
        mapgl::fit_bounds(brth, animate = TRUE)
    }
    m
  })

  # ── 規劃 ──
  res <- shiny::reactiveVal(NULL)
  shiny::observeEvent(input$plan, {
    o <- pois[pois$name == input$origin, ]; d <- pois[pois$name == input$dest, ]
    if (identical(o$name, d$name)) {
      shiny::showNotification("起點與終點不可相同", type = "warning"); return() }
    g   <- grid_r()$grid
    pr  <- plan_routes(list(lon = o$lon, lat = o$lat),
                       list(lon = d$lon, lat = d$lat), g)
    grn <- route_greenness(pr$breathing)
    h   <- health_stats(pr$len_breath_km, input$height %||% 170)
    carbon_g <- round((grn$green_pct %||% 0) / 100 * pr$len_breath_km * 250)
    ctx <- list(pm25 = pm_now, aqi = aqi_now,
                exposure_drop_pct = pr$exposure_drop_pct,
                green_pct = grn$green_pct, extra_min = pr$extra_min,
                group = input$group)
    res(list(pr = pr, g = grn, ctx = ctx, o = o, d = d, h = h,
             carbon_g = carbon_g, text = explain_route(ctx)))
  })
  shiny::observeEvent(input$reset, { res(NULL); qa(list()) })

  # ── 左上指標小晶片 ──
  output$metrics <- shiny::renderUI({
    p <- res(); if (is.null(p)) return(NULL)
    shiny::tagList(
      shiny::div(class = "metric-chip time", "多花時間",
                 shiny::tags$b(paste0(p$pr$extra_min, " 分"))),
      shiny::div(class = "metric-chip air", "暴露下降",
                 shiny::tags$b(paste0(p$pr$exposure_drop_pct, "%"))),
      shiny::div(class = "metric-chip green", "沿途綠覆",
                 shiny::tags$b(paste0(p$g$green_pct, "%")))
    )
  })

  # ── 右側「路徑比較」面板 ──
  output$panel <- shiny::renderUI({
    p <- res()
    if (is.null(p)) {
      return(shiny::div(class = "text-muted",
        "選擇族群、起訖點與污染物，按「規劃」即可得到路線比較與個人化建議。"))
    }
    maxlen <- max(p$pr$len_fast_km, p$pr$len_breath_km, 0.01)
    trees  <- round(p$carbon_g / 21800, 2)
    engine_tag <- if ((p$pr$engine %||% "geo") == "ors") "真實路網" else "幾何示意"

    shiny::tagList(
      # 改善效益
      shiny::div(class = "pc-sec hl",
        shiny::div(class = "pc-h", "改善效益"),
        shiny::div(class = "pc-row",
          shiny::span("暴露改善"),
          shiny::span(class = "pc-big air", paste0(p$pr$exposure_drop_pct, "%"))),
        shiny::div(class = "pc-bar",
          shiny::div(class = "pc-fill",
                     style = sprintf("width:%s%%", min(100, p$pr$exposure_drop_pct))))),
      # 負碳存摺
      shiny::div(class = "pc-sec",
        shiny::div(class = "pc-h", "🌱 負碳存摺"),
        shiny::div(class = "pc-big green", paste0(p$carbon_g, " g CO₂")),
        shiny::div(class = "pc-trees", paste(rep("🌳", 5), collapse = "")),
        shiny::div(class = "pc-note", paste0("沿途綠覆固碳量（示意）· 約 ", trees, " 顆樹/年"))),
      # 健康步帳
      shiny::div(class = "pc-sec",
        shiny::div(class = "pc-h", "👟 健康步帳"),
        shiny::div(class = "pc-stats",
          shiny::div(class = "pc-stat", shiny::tags$b(format(p$h$steps, big.mark = ",")),
                     shiny::span("步數")),
          shiny::div(class = "pc-stat", shiny::tags$b(p$h$kcal), shiny::span("kcal")),
          shiny::div(class = "pc-stat", shiny::tags$b(paste0(p$pr$extra_min, "′")),
                     shiny::span("多花時間")))),
      # 距離比較
      shiny::div(class = "pc-sec",
        shiny::div(class = "pc-h", "📏 距離比較"),
        shiny::div(class = "pc-dl",
          shiny::div(class = "lab", shiny::span("最短路線"),
                     shiny::span(paste0(p$pr$len_fast_km, " km"))),
          shiny::div(class = "pc-dline",
            style = sprintf("width:%s%%;background:#5b54c9",
                            round(100 * p$pr$len_fast_km / maxlen))),
          shiny::div(class = "lab", shiny::span("低暴露路徑"),
                     shiny::span(paste0(p$pr$len_breath_km, " km"))),
          shiny::div(class = "pc-dline",
            style = sprintf("width:%s%%;background:#0e8fa3",
                            round(100 * p$pr$len_breath_km / maxlen))))),
      # 健康解釋
      shiny::div(class = "pc-sec",
        shiny::div(class = "pc-h", "🤖 Copilot 健康解釋"),
        shiny::div(class = "pc-explain", p$text),
        shiny::div(class = "pc-note", paste0("路徑引擎：", engine_tag,
          "｜路徑平均 ", POLL_LAB[[grid_r()$var]], "：", p$pr$mean_pm25)))
    )
  })

  # ── 追問 Q&A ──
  qa <- shiny::reactiveVal(list())
  shiny::observeEvent(input$ask, {
    p <- res()
    if (is.null(p)) { shiny::showNotification("請先規劃路徑", type = "warning"); return() }
    if (!nzchar(trimws(input$q))) return()
    ans <- answer_question(p$ctx, input$q)
    qa(c(qa(), list(list(q = input$q, a = ans))))
    shiny::updateTextInput(session, "q", value = "")
  })
  output$qa <- shiny::renderUI({
    items <- qa(); if (length(items) == 0) return(NULL)
    shiny::div(class = "mt-1",
      lapply(items, function(it) shiny::div(class = "pc-sec",
        shiny::p(class = "mb-1", shiny::tags$b("Q："), it$q),
        shiny::p(class = "mb-0 text-primary", shiny::tags$b("A："), it$a))))
  })
}
