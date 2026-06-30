# ──────────────────────────────────────────────────────────────
# R/functions/demo_data.R
# 合成觀測資料 — 刻意注入 BAD / SAD 問題，方便無金鑰展示 Dashboard。
# 產出已是標準 schema（見 utils::empty_observations）。
# ──────────────────────────────────────────────────────────────

# 產生 n_stations 個測站、past hours 小時、每小時一筆的 PM2.5 / AQI 時序。
generate_demo_observations <- function(n_stations = 8, hours = 48, seed = 42) {
  set.seed(seed)
  now <- lubridate::floor_date(Sys.time(), "hour")
  times <- now - lubridate::hours(seq.int(hours - 1, 0))

  stations <- tibble::tibble(
    station = sprintf("示範站-%02d", seq_len(n_stations)),
    county  = "臺南市",
    lat     = round(runif(n_stations, 22.90, 23.10), 4),
    lon     = round(runif(n_stations, 120.10, 120.30), 4),
    base    = round(runif(n_stations, 15, 30), 1)   # 各站基準 PM2.5
  )

  # 正常時序：基準 + 日週期 + 噪音
  grid <- tidyr::expand_grid(station = stations$station, datetime = times) |>
    dplyr::left_join(stations, by = "station") |>
    dplyr::mutate(
      hr    = lubridate::hour(datetime),
      diurn = 6 * sin((hr - 6) / 24 * 2 * pi),
      pm25  = pmax(0, round(base + diurn + rnorm(dplyr::n(), 0, 2.5), 1))
    )

  st <- stations$station

  # ── 注入 SAD：離群尖峰（合法但統計可疑） ──
  grid <- grid |>
    dplyr::mutate(pm25 = ifelse(
      station == st[6] & datetime == max(times),
      round(base[1] + 55, 1), pm25))   # 單點暴衝

  # ── 注入 SAD：flatline（感測器卡住，最近 8 筆固定值） ──
  flat_rows <- grid$station == st[7] & grid$datetime >= (now - lubridate::hours(7))
  grid$pm25[flat_rows] <- 21.0

  # ── 注入 SAD：分布漂移（drift：後半時段整體上移） ──
  drift_rows <- grid$station == st[8] & grid$datetime >= (now - lubridate::hours(11))
  grid$pm25[drift_rows] <- grid$pm25[drift_rows] + 18

  # ── 注入 BAD：最新一筆缺值 ──
  grid$pm25[grid$station == st[1] & grid$datetime == max(times)] <- NA_real_

  # ── 注入 BAD：超出物理範圍（負值） ──
  grid$pm25[grid$station == st[2] & grid$datetime == max(times)] <- -4.0

  # ── 注入 BAD：stale（此站時序整體往前推 6 小時，最新資料過期） ──
  stale_idx <- grid$station == st[3]
  grid$datetime[stale_idx] <- grid$datetime[stale_idx] - lubridate::hours(6)

  # 轉 long：pm25 + 由 pm25 推 AQI（簡化線性對應，僅供 demo）
  long_pm <- grid |>
    dplyr::transmute(
      source = "moenv", station, county, lat, lon, datetime,
      variable = "pm25", value = pm25
    )
  long_aqi <- long_pm |>
    dplyr::mutate(variable = "aqi",
                  value = round(pmin(500, pmax(0, value * 2.1)), 0))

  dplyr::bind_rows(long_pm, long_aqi) |>
    dplyr::mutate(fetched_at = Sys.time()) |>
    dplyr::arrange(station, variable, datetime)
}

# 預載示範用的 QC 歷史趨勢（若歷史檔不存在）
seed_demo_history <- function() {
  path <- file.path(ROOT, CONFIG$paths$qc_history)
  if (file.exists(path)) return(invisible(NULL))
  set.seed(7)
  ts <- Sys.time() - lubridate::hours(seq.int(48, 1))
  hist <- tibble::tibble(
    run_at   = ts,
    source   = "moenv",
    n_total  = 16L,
    bad_count = pmax(0L, rpois(length(ts), 1.2)),
    sad_count = pmax(0L, rpois(length(ts), 2.0))
  )
  utils::write.csv(hist, path, row.names = FALSE, fileEncoding = "UTF-8")
  log_msg("已預載示範 QC 歷史：", path, level = "OK")
  invisible(NULL)
}

# 台南示範用地標（座標為近似值，供原型路徑規劃）
demo_pois <- function() {
  tibble::tibble(
    name = c("台南火車站", "國立成功大學", "台南公園", "孔廟",
             "林百貨", "花園夜市", "奇美博物館"),
    lon  = c(120.213, 120.218, 120.210, 120.204, 120.202, 120.205, 120.226),
    lat  = c(22.997,  23.000,  23.001,  22.990,  22.991,  23.011,  22.935)
  )
}
