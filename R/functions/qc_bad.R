# ──────────────────────────────────────────────────────────────
# R/functions/qc_bad.R — BAD 檢查（硬規則 / 二元）
# BAD = Broken / Anomalous / Defective → 擋下，不可進模型（紅燈）
#
# 作用於「每測站最新一筆」快照（latest），stale 另用完整時序判定。
# 回傳統一的 flags tibble（見 qc_runner.R 的欄位約定）。
# ──────────────────────────────────────────────────────────────

qc_bad <- function(latest, full, cfg) {
  bcfg <- cfg$qc$bad
  flags <- list()

  # 1) 缺值
  miss <- dplyr::filter(latest, is.na(value))
  if (nrow(miss)) flags[["missing"]] <- .mk_flag(
    miss, "BAD", "bad_missing", "fail", "關鍵欄位缺值 (NA)")

  # 2) 超出物理範圍
  rng <- bcfg$ranges
  oor <- latest |>
    dplyr::filter(!is.na(value), variable %in% names(rng)) |>
    dplyr::rowwise() |>
    dplyr::filter(value < rng[[variable]][1] | value > rng[[variable]][2]) |>
    dplyr::ungroup()
  if (nrow(oor)) flags[["range"]] <- .mk_flag(
    oor, "BAD", "bad_range", "fail", "數值超出物理範圍")

  # 3) 重複 (source, station, datetime, variable)
  dup <- full |>
    dplyr::group_by(source, station, datetime, variable) |>
    dplyr::filter(dplyr::n() > 1) |>
    dplyr::slice(1) |>
    dplyr::ungroup()
  if (nrow(dup)) flags[["dup"]] <- .mk_flag(
    dup, "BAD", "bad_dup", "fail", "重複紀錄")

  # 4) stale：每「測站」最新資料時間超過門檻（單一故障站也要抓到）
  stale_min <- bcfg$stale_after_min %||% 120
  now <- Sys.time()
  stale <- full |>
    dplyr::group_by(source, station) |>
    dplyr::summarise(max_dt = max(datetime, na.rm = TRUE),
                     county = dplyr::first(county),
                     lat = dplyr::first(lat), lon = dplyr::first(lon),
                     .groups = "drop") |>
    dplyr::filter(as.numeric(difftime(now, max_dt, units = "mins")) > stale_min) |>
    dplyr::mutate(datetime = max_dt, variable = "*", value = NA_real_,
                  fetched_at = now)
  if (nrow(stale)) flags[["stale"]] <- .mk_flag(
    stale, "BAD", "bad_stale",
    "fail", sprintf("資料過期（>%d 分）", stale_min))

  dplyr::bind_rows(flags)
}
