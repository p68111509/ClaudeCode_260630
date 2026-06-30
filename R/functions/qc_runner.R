# ──────────────────────────────────────────────────────────────
# R/functions/qc_runner.R — QC 編排
# 串起 BAD + SAD，產生統一 flags，計算 KPI / 來源狀態，並寫入歷史。
#
# flags 統一欄位：
#   qc_type  : "BAD" | "SAD"
#   check    : 檢查代碼（bad_missing / sad_outlier ...）
#   severity : "fail"(紅) | "warn"(黃) | "pass"(綠)
#   message  : 人類可讀說明
#   source, station, county, lat, lon, datetime, variable, value
# ──────────────────────────────────────────────────────────────

# flags 建構器：把任一資料框包成統一 flags 列
.mk_flag <- function(df, qc_type, check, severity, message) {
  n <- nrow(df)
  tibble::tibble(
    qc_type  = qc_type,
    check    = check,
    severity = rep_len(severity, n),
    message  = rep_len(message, n),
    source   = df$source   %||% NA_character_,
    station  = df$station  %||% NA_character_,
    county   = df$county   %||% NA_character_,
    lat      = df$lat      %||% NA_real_,
    lon      = df$lon      %||% NA_real_,
    datetime = df$datetime %||% as.POSIXct(NA),
    variable = df$variable %||% NA_character_,
    value    = df$value    %||% NA_real_
  )
}

# 主流程：obs(標準 schema) → flags
run_qc <- function(obs, cfg = CONFIG) {
  if (nrow(obs) == 0) {
    log_msg("QC：無觀測資料", level = "WARN")
    return(.mk_flag(empty_observations(), "BAD", "bad_empty", "fail", "無資料"))
  }
  # 每測站最新一筆快照（供 BAD 的缺值/範圍判定）
  latest <- obs |>
    dplyr::group_by(source, station, variable) |>
    dplyr::slice_max(datetime, n = 1, with_ties = FALSE) |>
    dplyr::ungroup()

  flags <- dplyr::bind_rows(
    qc_bad(latest, obs, cfg),
    qc_sad(obs, cfg)
  )
  log_msg(sprintf("QC 完成：BAD=%d、SAD=%d",
                  sum(flags$qc_type == "BAD"), sum(flags$qc_type == "SAD")),
          level = "OK")
  flags
}

# KPI 摘要（給 Dashboard valueBox）
# 計數 (bad_n/sad_n) 為旗標筆數；比率 (rate) 以「受影響的 測站×變數 系列」為分母，
# 確保 ≤ 100%（一個系列即使被多次標記，只算一次）。
qc_kpis <- function(flags, obs) {
  n_records <- obs |>
    dplyr::distinct(source, station, variable) |> nrow()
  n_records <- max(n_records, 1)

  affected <- function(type) {
    flags |>
      dplyr::filter(qc_type == type, !is.na(station)) |>
      dplyr::distinct(source, station, variable) |> nrow()
  }
  list(
    n_records = n_records,
    bad_n     = sum(flags$qc_type == "BAD"),
    sad_n     = sum(flags$qc_type == "SAD"),
    bad_rate  = round(100 * affected("BAD") / n_records, 1),
    sad_rate  = round(100 * affected("SAD") / n_records, 1),
    sources_total   = dplyr::n_distinct(obs$source),
    sources_healthy = qc_source_status(flags, obs) |>
      dplyr::filter(status == "green") |> nrow()
  )
}

# 每來源狀態燈（red > amber > green）
qc_source_status <- function(flags, obs) {
  src <- tibble::tibble(source = unique(obs$source))
  agg <- flags |>
    dplyr::group_by(source) |>
    dplyr::summarise(
      bad = sum(qc_type == "BAD"),
      sad = sum(qc_type == "SAD"),
      .groups = "drop")
  src |>
    dplyr::left_join(agg, by = "source") |>
    dplyr::mutate(
      bad = tidyr::replace_na(bad, 0L),
      sad = tidyr::replace_na(sad, 0L),
      status = dplyr::case_when(bad > 0 ~ "red", sad > 0 ~ "amber", TRUE ~ "green")
    )
}

# 寫入 QC 歷史（每來源一列），供趨勢圖
append_qc_history <- function(flags, obs) {
  path <- file.path(ROOT, CONFIG$paths$qc_history)
  ensure_dir(dirname(path))
  ss <- qc_source_status(flags, obs)
  n_by_src <- obs |>
    dplyr::group_by(source, station, variable) |>
    dplyr::summarise(.groups = "drop") |>
    dplyr::count(source, name = "n_total")
  row <- ss |>
    dplyr::left_join(n_by_src, by = "source") |>
    dplyr::transmute(run_at = Sys.time(), source,
                     n_total = tidyr::replace_na(n_total, 0L),
                     bad_count = bad, sad_count = sad)
  old <- if (file.exists(path)) {
    utils::read.csv(path, stringsAsFactors = FALSE) |>
      dplyr::mutate(run_at = lubridate::ymd_hms(run_at, quiet = TRUE,
                                                tz = "Asia/Taipei"))
  } else NULL
  out <- dplyr::bind_rows(old, row)
  utils::write.csv(out, path, row.names = FALSE, fileEncoding = "UTF-8")
  invisible(out)
}

read_qc_history <- function() {
  path <- file.path(ROOT, CONFIG$paths$qc_history)
  if (!file.exists(path)) return(tibble::tibble())
  utils::read.csv(path, stringsAsFactors = FALSE) |>
    dplyr::mutate(run_at = lubridate::ymd_hms(run_at, quiet = TRUE,
                                              tz = "Asia/Taipei"))
}
