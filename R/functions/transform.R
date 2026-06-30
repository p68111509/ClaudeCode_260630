# ──────────────────────────────────────────────────────────────
# R/functions/transform.R — 把各來源原始資料標準化成統一 long schema。
# 若輸入已是標準 schema（demo），直接通過。
# ──────────────────────────────────────────────────────────────

# 主入口：確保輸出符合標準 schema
standardize_observations <- function(raw) {
  if (is.null(raw) || nrow(raw) == 0) return(empty_observations())
  needed <- names(empty_observations())
  if (all(needed %in% names(raw))) {
    return(dplyr::select(tibble::as_tibble(raw), dplyr::all_of(needed)))
  }
  # 非標準：嘗試保留可對應欄位
  log_msg("輸入非標準 schema，嘗試對應", level = "WARN")
  tibble::as_tibble(raw)
}

# ── 環境部 records → long ──
transform_moenv <- function(records) {
  if (is.null(records) || length(records) == 0) return(empty_observations())
  df <- tibble::as_tibble(records)
  base <- tibble::tibble(
    source   = "moenv",
    station  = df$sitename %||% NA_character_,
    county   = df$county   %||% NA_character_,
    lat      = as_num(df$latitude),
    lon      = as_num(df$longitude),
    # MOENV v2 的時間欄位為 publishtime（格式 2026/06/28 00:00:00）；
    # 舊欄位 datacreationdate 作為後備
    datetime = lubridate::ymd_hms(df$publishtime %||% df$datacreationdate,
                                  tz = "Asia/Taipei", quiet = TRUE),
    fetched_at = Sys.time()
  )
  dplyr::bind_rows(
    dplyr::mutate(base, variable = "pm25", value = as_num(df$`pm2.5`)),
    dplyr::mutate(base, variable = "aqi",  value = as_num(df$aqi)),
    dplyr::mutate(base, variable = "no2",  value = as_num(df$no2)),
    dplyr::mutate(base, variable = "o3",   value = as_num(df$o3))
  )
}

# ── 氣象署 Station → long（溫度）──
# 每站 Coordinates 含 TWD67 + WGS84 兩列，取 WGS84；缺值哨兵(<= -90)轉 NA。
transform_cwa <- function(stations) {
  if (is.null(stations) || NROW(stations) == 0) return(empty_observations())
  df <- stations
  n  <- NROW(df)

  coords <- df$GeoInfo$Coordinates
  get_wgs <- function(field) vapply(coords, function(cd) {
    if (is.null(cd) || !"CoordinateName" %in% names(cd)) return(NA_real_)
    w <- which(cd$CoordinateName == "WGS84"); if (!length(w)) w <- 1
    suppressWarnings(as.numeric(cd[[field]][w[1]]))
  }, numeric(1))

  temp <- suppressWarnings(as.numeric(df$WeatherElement$AirTemperature))
  temp[!is.na(temp) & temp <= -90] <- NA

  tibble::tibble(
    source   = "cwa",
    station  = if (!is.null(df$StationName)) df$StationName else rep(NA_character_, n),
    county   = if (!is.null(df$GeoInfo$CountyName)) df$GeoInfo$CountyName else rep(NA_character_, n),
    lat      = get_wgs("StationLatitude"),
    lon      = get_wgs("StationLongitude"),
    datetime = lubridate::ymd_hms(df$ObsTime$DateTime, tz = "Asia/Taipei",
                                  quiet = TRUE),
    variable = "temp",
    value    = temp,
    fetched_at = Sys.time()
  )
}
