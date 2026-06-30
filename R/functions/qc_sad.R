# ──────────────────────────────────────────────────────────────
# R/functions/qc_sad.R — SAD 檢查（統計 / 程度）
# SAD = Statistically Aberrant Data → 放行但標記，人工觀察（黃燈）
#
# 作用於完整時間序列；無歷史時自動降級（只跑能跑的）。
# ──────────────────────────────────────────────────────────────

# 穩健 z 分數（以 median / MAD）；回傳長度與輸入相同
robust_z <- function(x) {
  m  <- stats::median(x, na.rm = TRUE)
  ma <- stats::mad(x, na.rm = TRUE)
  if (is.na(ma) || ma == 0) return(rep(0, length(x)))
  0.6745 * (x - m) / ma
}

# PSI（Population Stability Index）：actual vs expected 分布。
# 注意：PSI 須用「歷史基準分布」當 expected 才穩定；用自身時序前後半互比，
# 在小樣本下極不穩定（會誤判）。本原型 drift 改用穩健 level-shift（見下），
# 此函式保留供未來接上歷史基準時使用。
compute_psi <- function(expected, actual, n_bins = 5) {
  expected <- expected[!is.na(expected)]; actual <- actual[!is.na(actual)]
  if (length(expected) < 4 * n_bins || length(actual) < 4 * n_bins) return(NA_real_)
  brk <- unique(stats::quantile(expected, probs = seq(0, 1, length.out = n_bins + 1),
                                na.rm = TRUE))
  if (length(brk) < 3) return(NA_real_)
  e <- table(cut(expected, brk, include.lowest = TRUE)) / length(expected)
  a <- table(cut(actual,   brk, include.lowest = TRUE)) / length(actual)
  e <- pmax(as.numeric(e), 1e-4); a <- pmax(as.numeric(a), 1e-4)
  sum((a - e) * log(a / e))
}

qc_sad <- function(full, cfg) {
  scfg <- cfg$qc$sad
  flags <- list()

  num <- dplyr::filter(full, !is.na(value), variable %in% c("pm25", "aqi", "temp"))
  if (nrow(num) == 0) return(dplyr::bind_rows(flags))

  # 1) 離群（穩健 z），每 (source, station, variable) 時序內
  out <- num |>
    dplyr::group_by(source, station, variable) |>
    dplyr::mutate(rz = robust_z(value)) |>
    dplyr::ungroup() |>
    dplyr::filter(abs(rz) > (scfg$mad_z_threshold %||% 3.5))
  if (nrow(out)) flags[["outlier"]] <- .mk_flag(
    out, "SAD", "sad_outlier", "warn",
    sprintf("穩健 z=%.1f 離群", out$rz))

  # 2) flatline：最近 N 筆標準差過小
  win <- scfg$flatline_window %||% 6
  flat <- num |>
    dplyr::group_by(source, station, variable) |>
    dplyr::arrange(datetime, .by_group = TRUE) |>
    dplyr::filter(dplyr::n() >= win) |>
    dplyr::summarise(
      sd_recent = stats::sd(utils::tail(value, win)),
      datetime  = max(datetime),
      lat = dplyr::first(lat), lon = dplyr::first(lon),
      county = dplyr::first(county), value = dplyr::last(value),
      .groups = "drop") |>
    dplyr::filter(sd_recent < (scfg$flatline_min_sd %||% 0.01))
  if (nrow(flat)) flags[["flatline"]] <- .mk_flag(
    flat, "SAD", "sad_flatline", "warn",
    sprintf("近 %d 筆近乎定值（疑卡死）", win))

  # 3) drift（穩健 level-shift）：直近窗中位數相對較早期的位移，以 MAD 標準化。
  #    對小樣本穩定；單點離群（→outlier）與定值（→flatline）不會誤判為漂移。
  win2 <- scfg$drift_window %||% 8
  thr  <- scfg$drift_shift_z %||% 4
  drift <- num |>
    dplyr::group_by(source, station, variable) |>
    dplyr::arrange(datetime, .by_group = TRUE) |>
    dplyr::filter(dplyr::n() >= 2 * win2) |>
    dplyr::summarise(
      recent_med  = stats::median(utils::tail(value, win2)),
      earlier_med = stats::median(utils::head(value, dplyr::n() - win2)),
      earlier_mad = stats::mad(utils::head(value, dplyr::n() - win2)),
      datetime = max(datetime),
      lat = dplyr::first(lat), lon = dplyr::first(lon),
      county = dplyr::first(county), value = dplyr::last(value),
      .groups = "drop") |>
    dplyr::mutate(zshift = ifelse(earlier_mad > 0,
                                  abs(recent_med - earlier_med) / earlier_mad, 0)) |>
    dplyr::filter(zshift > thr)
  if (nrow(drift)) {
    flags[["drift"]] <- .mk_flag(
      drift, "SAD", "sad_drift", "warn",
      sprintf("近期水準位移 z=%.1f（中位數 %.1f→%.1f）",
              drift$zshift, drift$earlier_med, drift$recent_med))
  }

  dplyr::bind_rows(flags)
}
