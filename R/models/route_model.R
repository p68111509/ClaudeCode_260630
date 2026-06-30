# ──────────────────────────────────────────────────────────────
# R/models/route_model.R — 引擎 B：呼吸路徑
# 給定起訖點與暴露網格，產生「最快(直線)」與「呼吸路徑(避開高暴露)」兩條候選，
# 並做距離加權的暴露評分。後續可接 OpenRouteService / Google Routes 取真實路網。
# ──────────────────────────────────────────────────────────────

# Haversine 距離（公里），可向量化
haversine <- function(lon1, lat1, lon2, lat2) {
  R <- 6371; rad <- pi / 180
  dlat <- (lat2 - lat1) * rad; dlon <- (lon2 - lon1) * rad
  a <- sin(dlat / 2)^2 + cos(lat1 * rad) * cos(lat2 * rad) * sin(dlon / 2)^2
  2 * R * asin(pmin(1, sqrt(a)))
}

route_length_km <- function(route) {
  n <- nrow(route); if (n < 2) return(0)
  sum(haversine(route$lon[-n], route$lat[-n], route$lon[-1], route$lat[-1]))
}

# 對路徑取樣點，從暴露網格取最近值
.sample_grid <- function(route, grid) {
  vapply(seq_len(nrow(route)), function(i) {
    d <- (grid$lon - route$lon[i])^2 + (grid$lat - route$lat[i])^2
    grid$value[which.min(d)]
  }, numeric(1))
}

# 距離加權暴露：sum(段長 × 段平均濃度)，單位 µg/m³·km（與行經時間×濃度成比例）
score_route <- function(route, grid) {
  if (nrow(grid) == 0 || nrow(route) < 2) {
    return(list(exposure = NA_real_, mean_pm25 = NA_real_))
  }
  vals <- .sample_grid(route, grid)
  n <- nrow(route)
  seg  <- haversine(route$lon[-n], route$lat[-n], route$lon[-1], route$lat[-1])
  segc <- (vals[-n] + vals[-1]) / 2
  list(exposure = round(sum(seg * segc), 1), mean_pm25 = round(mean(vals), 1))
}

# 由起點到終點的取樣路徑（二次貝茲）；bend 為中點垂直偏移量（度），0 = 直線
interpolate_route <- function(o, d, bend = 0, n = 40) {
  t <- seq(0, 1, length.out = n)
  dx <- d$lon - o$lon; dy <- d$lat - o$lat
  len <- sqrt(dx^2 + dy^2); if (len == 0) len <- 1e-6
  px <- -dy / len; py <- dx / len                      # 垂直單位向量
  cx <- (o$lon + d$lon) / 2 + bend * px
  cy <- (o$lat + d$lat) / 2 + bend * py
  tibble::tibble(
    lon = (1 - t)^2 * o$lon + 2 * (1 - t) * t * cx + t^2 * d$lon,
    lat = (1 - t)^2 * o$lat + 2 * (1 - t) * t * cy + t^2 * d$lat
  )
}

# ── OpenRouteService：取步行路網路線（含替代路線），回傳 list(每條: geom, dist_km） ──
ors_route <- function(o, d) {
  key <- api_key("ORS_API_KEY")
  if (is.na(key)) return(NULL)
  body <- list(
    coordinates = list(c(o$lon, o$lat), c(d$lon, d$lat)),
    alternative_routes = list(target_count = 3, share_factor = 0.6,
                              weight_factor = 1.6),
    instructions = FALSE
  )
  tryCatch({
    resp <- httr2::request(API$ors$base_url) |>
      httr2::req_headers(Authorization = key, "content-type" = "application/json") |>
      httr2::req_body_json(body) |>
      httr2::req_timeout(40) |>
      httr2::req_perform()
    feats <- httr2::resp_body_json(resp)$features
    lapply(feats, function(f) {
      m <- do.call(rbind, lapply(f$geometry$coordinates,
                                 function(p) c(p[[1]], p[[2]])))
      list(geom = tibble::tibble(lon = m[, 1], lat = m[, 2]),
           dist_km = f$properties$summary$distance / 1000)
    })
  }, error = function(e) {
    log_msg("ORS 路由失敗：", conditionMessage(e), level = "WARN"); NULL
  })
}

# 由身高估算步數 / 卡路里
health_stats <- function(dist_km, height_cm = 170) {
  stride_m <- height_cm * 0.415 / 100
  steps <- round(dist_km * 1000 / stride_m)
  list(steps = steps, kcal = round(steps * 0.04), stride_m = round(stride_m, 2))
}

# 主入口：有 ORS 金鑰走真實路網，否則退回幾何路線
plan_routes <- function(o, d, grid, walk_kmh = 4.8) {
  if (!is.na(api_key("ORS_API_KEY"))) {
    rr <- tryCatch(.plan_routes_ors(o, d, grid, walk_kmh), error = function(e) NULL)
    if (!is.null(rr)) return(rr)
  }
  .plan_routes_geometric(o, d, grid, walk_kmh)
}

# 真實路網：取 ORS 替代路線，依暴露計分挑「最短」與「低暴露」
.plan_routes_ors <- function(o, d, grid, walk_kmh) {
  routes <- ors_route(o, d)
  if (is.null(routes) || length(routes) == 0) return(NULL)
  sc <- lapply(routes, function(rt) {
    s <- score_route(rt$geom, grid)
    list(geom = rt$geom, len = rt$dist_km,
         exposure = s$exposure, mean = s$mean_pm25)
  })
  lens <- vapply(sc, function(x) x$len, numeric(1))
  exps <- vapply(sc, function(x) x$exposure %||% NA_real_, numeric(1))
  fast <- sc[[which.min(lens)]]
  low  <- sc[[ if (all(is.na(exps))) which.min(lens) else which.min(exps) ]]
  drop <- if (is.na(fast$exposure) || fast$exposure == 0) 0
          else round(100 * (fast$exposure - low$exposure) / fast$exposure, 0)
  list(fastest = fast$geom, breathing = low$geom,
       extra_min = max(0, round((low$len - fast$len) / walk_kmh * 60, 0)),
       exposure_drop_pct = max(0, drop), mean_pm25 = low$mean,
       len_fast_km = round(fast$len, 2), len_breath_km = round(low$len, 2),
       engine = "ors")
}

# 幾何後備：最快(直線) + 呼吸路徑(在多個彎曲候選中選暴露最低)
.plan_routes_geometric <- function(o, d, grid, walk_kmh = 4.8) {
  fastest <- interpolate_route(o, d, bend = 0)
  span <- sqrt((d$lon - o$lon)^2 + (d$lat - o$lat)^2)
  bends <- seq(-0.5, 0.5, length.out = 13) * span
  cands <- lapply(bends, function(b) interpolate_route(o, d, bend = b))
  expo  <- vapply(cands, function(r) score_route(r, grid)$exposure, numeric(1))
  breathing <- cands[[which.min(expo)]]

  ef <- score_route(fastest, grid); eb <- score_route(breathing, grid)
  lf <- route_length_km(fastest);   lb <- route_length_km(breathing)
  drop <- if (is.na(ef$exposure) || ef$exposure == 0) 0
          else round(100 * (ef$exposure - eb$exposure) / ef$exposure, 0)

  list(
    fastest = fastest, breathing = breathing,
    extra_min = max(0, round((lb - lf) / walk_kmh * 60, 0)),
    exposure_drop_pct = max(0, drop),
    mean_pm25 = eb$mean_pm25,
    len_fast_km = round(lf, 2), len_breath_km = round(lb, 2),
    engine = "geo"
  )
}
