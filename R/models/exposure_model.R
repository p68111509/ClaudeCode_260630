# ──────────────────────────────────────────────────────────────
# R/models/exposure_model.R — 引擎 A：暴露推估
# 【接點】此處為原型 stub，用 IDW（反距離權重）內插示意。
# 後續請替換成研究室既有的 ML/DL 模型，介面維持不變即可：
#   predict_exposure(obs, grid) -> data.frame(lon, lat, value)
# ──────────────────────────────────────────────────────────────

# 由觀測點以 IDW 推估網格 PM2.5（僅供原型示意）
predict_exposure <- function(obs, variable = "pm25",
                             grid_n = 30, power = 2) {
  pts <- obs |>
    dplyr::filter(variable == !!variable, !is.na(value),
                  !is.na(lat), !is.na(lon)) |>
    dplyr::group_by(station) |>
    dplyr::slice_max(datetime, n = 1, with_ties = FALSE) |>
    dplyr::ungroup()

  if (nrow(pts) < 3) {
    log_msg("暴露推估：有效觀測點不足，回傳空網格", level = "WARN")
    return(tibble::tibble(lon = double(), lat = double(), value = double()))
  }

  gx <- seq(min(pts$lon), max(pts$lon), length.out = grid_n)
  gy <- seq(min(pts$lat), max(pts$lat), length.out = grid_n)
  grid <- expand.grid(lon = gx, lat = gy)

  grid$value <- vapply(seq_len(nrow(grid)), function(i) {
    d <- sqrt((pts$lon - grid$lon[i])^2 + (pts$lat - grid$lat[i])^2)
    if (any(d == 0)) return(pts$value[which.min(d)])
    w <- 1 / d^power
    sum(w * pts$value) / sum(w)
  }, numeric(1))

  tibble::as_tibble(grid)
}
