# ──────────────────────────────────────────────────────────────
# R/api/endpoints.R
# 【唯一】集中管理所有外部 API 端點與金鑰來源。
# 其他程式一律 source 此檔取用 `API`，不要在別處硬寫 URL。
# 新增 / 修改 API → 只動這個檔案。
# ──────────────────────────────────────────────────────────────

# 從環境變數取金鑰；自動去除前後空白（避免 .env 等號後多空格導致認證失敗）；
# 空字串視為未設定 (NA)
api_key <- function(env_name, default = NA_character_) {
  v <- trimws(Sys.getenv(env_name, unset = ""))
  if (!nzchar(v)) default else v
}

API <- list(

  moenv = list(
    name     = "環境部 環境資料開放平臺",
    base_url = "https://data.moenv.gov.tw/api/v2",
    dataset  = "aqx_p_432",                 # 空氣品質指標(AQI) 即時
    key_env  = "MOENV_API_KEY",
    docs     = "https://data.moenv.gov.tw/paradigm"
  ),

  cwa = list(
    name     = "中央氣象署 開放資料平臺",
    base_url = "https://opendata.cwa.gov.tw/api/v1/rest/datastore",
    dataset  = "O-A0003-001",               # 自動氣象站-氣象觀測資料
    key_env  = "CWA_AUTH_KEY",
    docs     = "https://opendata.cwa.gov.tw/dist/opendata-swagger.html"
  ),

  openaq = list(
    name     = "OpenAQ",
    base_url = "https://api.openaq.org/v3",
    key_env  = "OPENAQ_API_KEY",
    docs     = "https://docs.openaq.org/"
  ),

  waqi = list(
    name     = "World Air Quality Index (WAQI)",
    base_url = "https://api.waqi.info/feed",
    key_env  = "WAQI_TOKEN",
    docs     = "https://aqicn.org/api/"
  ),

  sentinel = list(
    name       = "Sentinel-2 (Copernicus Data Space / Sentinel Hub)",
    base_url   = "https://sh.dataspace.copernicus.eu",
    token_url  = "https://identity.dataspace.copernicus.eu/auth/realms/CDSE/protocol/openid-connect/token",
    id_env     = "SENTINEL_CLIENT_ID",
    secret_env = "SENTINEL_CLIENT_SECRET",
    key_env    = "SENTINEL_CLIENT_ID",   # 供 api_key_status 顯示用
    docs       = "https://documentation.dataspace.copernicus.eu/APIs/SentinelHub/Overview.html"
  ),

  google_routes = list(
    name     = "Google Routes API",
    base_url = "https://routes.googleapis.com/directions/v2:computeRoutes",
    key_env  = "GOOGLE_MAPS_KEY",
    docs     = "https://developers.google.com/maps/documentation/routes"
  ),

  ors = list(
    name     = "OpenRouteService（步行路網）",
    base_url = "https://api.openrouteservice.org/v2/directions/foot-walking/geojson",
    key_env  = "ORS_API_KEY",
    docs     = "https://openrouteservice.org/dev/#/api-docs"
  ),

  street_view = list(
    name     = "Google Street View Static API",
    base_url = "https://maps.googleapis.com/maps/api/streetview",
    key_env  = "GOOGLE_MAPS_KEY",
    docs     = "https://developers.google.com/maps/documentation/streetview"
  ),

  anthropic = list(
    name     = "Claude API (Anthropic)",
    base_url = "https://api.anthropic.com/v1/messages",
    model    = "claude-opus-4-8",
    version  = "2023-06-01",
    key_env  = "ANTHROPIC_API_KEY",
    docs     = "https://docs.anthropic.com/"
  ),

  gemini = list(
    name     = "Google Gemini (Generative Language API)",
    base_url = "https://generativelanguage.googleapis.com/v1beta/models",
    # gemini-3-flash-preview：實測你的金鑰可用的最新 flash 模型
    # （gemini-flash-latest 為自動更新別名，可作為較穩定的後備）
    model    = "gemini-3-flash-preview",
    key_env  = "GEMINI_API_KEY",
    docs     = "https://ai.google.dev/gemini-api/docs"
  )
)

# 取得某服務的金鑰（找不到服務即報錯）
api_get_key <- function(service) {
  cfg <- API[[service]]
  if (is.null(cfg)) stop(sprintf("未知的 API 服務：%s", service))
  api_key(cfg$key_env)
}

# 檢查哪些服務已備妥金鑰（Dashboard 與啟動訊息用）
api_key_status <- function() {
  data.frame(
    service = names(API),
    name    = vapply(API, function(x) x$name, character(1)),
    has_key = vapply(names(API), function(s) !is.na(api_get_key(s)), logical(1)),
    docs    = vapply(API, function(x) x$docs, character(1)),
    row.names = NULL, stringsAsFactors = FALSE
  )
}
