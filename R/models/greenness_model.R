# ──────────────────────────────────────────────────────────────
# R/models/greenness_model.R — 引擎 C：綠覆 / 碳
# 由 NDVI 估綠覆率與粗略固碳量。原型用合成 NDVI 場（高斯綠地）；
# 後續接 Sentinel-2 / 福衛八號 / 街景 CV，介面維持不變。
# ──────────────────────────────────────────────────────────────

# NDVI → 綠覆率(%) 與粗估固碳（kg CO2/km）
greenness_summary <- function(ndvi_values, veg_threshold = 0.2,
                              carbon_factor = 18) {
  ndvi_values <- ndvi_values[!is.na(ndvi_values)]
  if (length(ndvi_values) == 0) {
    return(list(green_pct = NA_real_, carbon_kg_per_km = NA_real_))
  }
  green_pct <- round(100 * mean(ndvi_values >= veg_threshold), 1)
  list(green_pct = green_pct,
       carbon_kg_per_km = round(green_pct / 100 * carbon_factor, 1))
}

# Demo 綠地中心（台南示範）：lon/lat 中心、amp 強度、rad 影響半徑（度）
.GREEN_CENTERS <- tibble::tibble(
  lon = c(120.210, 120.186, 120.231, 120.200, 120.219),
  lat = c(23.010,  22.986,  22.996,  23.030,  23.001),
  amp = c(0.60,    0.70,    0.50,    0.65,    0.55),
  rad = c(0.012,   0.010,   0.009,   0.011,   0.008)
)

demo_green_centers <- function() .GREEN_CENTERS

# 合成 NDVI 場：基底 + 各綠地高斯疊加（向量化、長度安全）
sample_ndvi <- function(lon, lat) {
  acc <- rep(0.08, length(lon))
  for (i in seq_len(nrow(.GREEN_CENTERS))) {
    c <- .GREEN_CENTERS[i, ]
    acc <- acc + c$amp *
      exp(-(((lon - c$lon)^2 + (lat - c$lat)^2) / (2 * c$rad^2)))
  }
  pmin(0.85, pmax(-0.1, acc))
}

# 一條路徑的綠覆摘要
# 優先用 Sentinel 真實 NDVI（依路徑外接 bbox），失敗則退回合成場。
route_greenness <- function(route) {
  pad  <- 0.003
  bbox <- c(min(route$lon) - pad, min(route$lat) - pad,
            max(route$lon) + pad, max(route$lat) + pad)
  real <- tryCatch(sentinel_ndvi_stats(bbox), error = function(e) NULL)
  if (!is.null(real) && !is.na(real$green_pct)) return(real)
  greenness_summary(sample_ndvi(route$lon, route$lat))   # 後備：合成場
}
