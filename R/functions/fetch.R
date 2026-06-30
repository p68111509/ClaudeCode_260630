# ──────────────────────────────────────────────────────────────
# R/functions/fetch.R — 對外抓取（使用 R/api/endpoints.R 的 API）
# 每個 fetch_* 回傳「原始」資料框；標準化交給 transform.R。
# 任何失敗都優雅降級為空殼，不讓 App 崩潰。
# ──────────────────────────────────────────────────────────────

# 統一入口：demo 模式回合成資料；否則打真實 API。
# 正式模式若因缺金鑰/失敗而無資料，會回退合成資料以維持 App 可用。
fetch_all <- function() {
  if (isTRUE(CONFIG$app$demo_mode)) {
    log_msg("Demo 模式：使用合成觀測資料")
    return(generate_demo_observations())
  }
  safe <- function(expr) tryCatch(expr, error = function(e) {
    log_msg("來源轉換失敗：", conditionMessage(e), level = "WARN"); empty_observations()
  })
  raw <- dplyr::bind_rows(
    safe(transform_moenv(fetch_moenv())),
    safe(transform_cwa(fetch_cwa()))
  )
  if (nrow(raw) == 0) {
    log_msg("正式模式無資料（缺金鑰或 API 失敗），回退合成資料", level = "WARN")
    return(generate_demo_observations())
  }
  log_msg(sprintf("正式模式：取得 %d 筆觀測", nrow(raw)), level = "OK")
  raw
}

# ── 環境部 AQI ──
fetch_moenv <- function(limit = 1000) {
  key <- api_get_key("moenv")
  if (is.na(key)) { log_msg("MOENV 無金鑰，跳過", level = "WARN"); return(NULL) }
  cfg <- API$moenv
  tryCatch({
    resp <- httr2::request(cfg$base_url) |>
      httr2::req_url_path_append(cfg$dataset) |>
      httr2::req_url_query(api_key = key, limit = limit) |>
      httr2::req_timeout(30) |>
      httr2::req_perform()
    parsed <- httr2::resp_body_json(resp, simplifyVector = TRUE)
    # v2 可能直接回傳陣列，或包在 $records 內 — 兩種都支援
    if (!is.null(parsed$records)) parsed$records else parsed
  }, error = function(e) {
    log_msg("MOENV 抓取失敗：", conditionMessage(e), level = "ERROR"); NULL
  })
}

# ── Sentinel Hub（Copernicus Data Space）OAuth：用 client_id/secret 換 access token ──
sentinel_token <- function() {
  cfg <- API$sentinel
  id  <- api_key(cfg$id_env); sec <- api_key(cfg$secret_env)
  if (is.na(id) || is.na(sec)) {
    log_msg("Sentinel 無 client 憑證，跳過", level = "WARN"); return(NA_character_)
  }
  tryCatch({
    resp <- httr2::request(cfg$token_url) |>
      httr2::req_body_form(grant_type = "client_credentials",
                           client_id = id, client_secret = sec) |>
      httr2::req_timeout(30) |>
      httr2::req_perform()
    httr2::resp_body_json(resp)$access_token
  }, error = function(e) {
    log_msg("Sentinel OAuth 失敗：", conditionMessage(e), level = "ERROR")
    NA_character_
  })
}

# ── Sentinel Hub Statistics API：取得 bbox 範圍近期的 NDVI 統計 ──
# 回傳 list(green_pct, mean_ndvi)；失敗回 NULL（呼叫端用合成場後備）。
# bbox = c(xmin_lon, ymin_lat, xmax_lon, ymax_lat)
sentinel_ndvi_stats <- function(bbox, days = 75) {
  tok <- sentinel_token()
  if (is.na(tok)) return(NULL)
  now <- Sys.time()
  from <- format(now - as.difftime(days, units = "days"), "%Y-%m-%dT00:00:00Z", tz = "UTC")
  to   <- format(now, "%Y-%m-%dT00:00:00Z", tz = "UTC")

  # veg 波段：NDVI>=0.2 記為 1，其平均即「植被像素比例」（逐像素綠覆%）
  evalscript <- paste(
    "//VERSION=3",
    "function setup(){return{input:[{bands:['B04','B08','dataMask']}],",
    "output:[{id:'ndvi',bands:1,sampleType:'FLOAT32'},",
    "{id:'veg',bands:1,sampleType:'FLOAT32'},{id:'dataMask',bands:1}]};}",
    "function evaluatePixel(s){let n=(s.B08-s.B04)/(s.B08+s.B04);",
    "return {ndvi:[n],veg:[n>=0.2?1:0],dataMask:[s.dataMask]};}", sep = "\n")

  body <- list(
    input = list(
      bounds = list(bbox = as.list(bbox),
                    properties = list(crs = "http://www.opengis.net/def/crs/EPSG/0/4326")),
      data = list(list(type = "sentinel-2-l2a",
                       dataFilter = list(maxCloudCoverage = 40)))),
    aggregation = list(
      timeRange = list(from = from, to = to),
      aggregationInterval = list(`of` = paste0("P", days, "D")),
      resx = 0.0009, resy = 0.0009,
      evalscript = evalscript)
  )

  tryCatch({
    resp <- httr2::request(paste0(API$sentinel$base_url, "/api/v1/statistics")) |>
      httr2::req_headers("Authorization" = paste("Bearer", tok),
                         "content-type" = "application/json") |>
      httr2::req_body_json(body) |>
      httr2::req_timeout(60) |>
      httr2::req_perform()
    js <- httr2::resp_body_json(resp)
    out <- js$data[[1]]$outputs
    mean_ndvi <- out$ndvi$bands$B0$stats$mean
    veg_frac  <- out$veg$bands$B0$stats$mean      # 植被像素比例 0~1
    if (is.null(mean_ndvi)) return(NULL)
    green_pct <- if (!is.null(veg_frac)) round(100 * veg_frac, 1)
                 else round(100 * max(0, min(1, mean_ndvi / 0.6)), 1)
    list(green_pct = green_pct,
         carbon_kg_per_km = round(green_pct / 100 * 18, 1),
         mean_ndvi = round(mean_ndvi, 3), source = "sentinel")
  }, error = function(e) {
    log_msg("Sentinel Statistics 失敗：", conditionMessage(e), level = "WARN"); NULL
  })
}

# ── 中央氣象署 自動氣象站 ──
fetch_cwa <- function() {
  key <- api_get_key("cwa")
  if (is.na(key)) { log_msg("CWA 無金鑰，跳過", level = "WARN"); return(NULL) }
  cfg <- API$cwa
  tryCatch({
    resp <- httr2::request(cfg$base_url) |>
      httr2::req_url_path_append(cfg$dataset) |>
      httr2::req_url_query(Authorization = key, format = "JSON") |>
      httr2::req_timeout(30) |>
      httr2::req_perform()
    httr2::resp_body_json(resp, simplifyVector = TRUE)$records$Station
  }, error = function(e) {
    log_msg("CWA 抓取失敗：", conditionMessage(e), level = "ERROR"); NULL
  })
}
