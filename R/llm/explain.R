# ──────────────────────────────────────────────────────────────
# R/llm/explain.R — 引擎 D：LLM 解釋層
# 供應商無關：優先 Gemini（GEMINI_API_KEY）→ 其次 Anthropic（ANTHROPIC_API_KEY）
# → 都沒有就用離線模板，讓原型可離線運作。
# ──────────────────────────────────────────────────────────────

# 統一補全介面；無可用金鑰或失敗時回 NA（呼叫端改用離線模板）
llm_complete <- function(system_text, user_text, max_tokens = 400) {
  gk <- api_get_key("gemini")
  if (!is.na(gk)) {
    out <- .llm_gemini(system_text, user_text, gk, max_tokens)
    if (!is.na(out)) return(out)
  }
  ak <- api_get_key("anthropic")
  if (!is.na(ak)) {
    out <- .llm_anthropic(system_text, user_text, ak, max_tokens)
    if (!is.na(out)) return(out)
  }
  NA_character_
}

.llm_gemini <- function(system_text, user_text, key, max_tokens) {
  cfg <- API$gemini
  url <- sprintf("%s/%s:generateContent", cfg$base_url, cfg$model)
  tryCatch({
    resp <- httr2::request(url) |>
      httr2::req_url_query(key = key) |>
      httr2::req_headers("content-type" = "application/json") |>
      httr2::req_body_json(list(
        system_instruction = list(parts = list(list(text = system_text))),
        contents = list(list(parts = list(list(text = user_text)))),
        generationConfig = list(maxOutputTokens = max_tokens))) |>
      httr2::req_timeout(40) |>
      httr2::req_perform()
    body <- httr2::resp_body_json(resp)
    body$candidates[[1]]$content$parts[[1]]$text
  }, error = function(e) {
    log_msg("Gemini 呼叫失敗：", conditionMessage(e), level = "WARN")
    NA_character_
  })
}

.llm_anthropic <- function(system_text, user_text, key, max_tokens) {
  cfg <- API$anthropic
  tryCatch({
    resp <- httr2::request(cfg$base_url) |>
      httr2::req_headers("x-api-key" = key,
                         "anthropic-version" = cfg$version,
                         "content-type" = "application/json") |>
      httr2::req_body_json(list(
        model = cfg$model, max_tokens = max_tokens, system = system_text,
        messages = list(list(role = "user", content = user_text)))) |>
      httr2::req_timeout(40) |>
      httr2::req_perform()
    httr2::resp_body_json(resp)$content[[1]]$text
  }, error = function(e) {
    log_msg("Anthropic 呼叫失敗：", conditionMessage(e), level = "WARN")
    NA_character_
  })
}

# ── 路徑健康解釋 ──
# context: list(pm25, aqi, exposure_drop_pct, green_pct, extra_min, group)
explain_route <- function(context) {
  sys <- "你是空污健康助手。請用繁體中文、3 句以內，說明推薦路線與原因，語氣親切務實。"
  usr <- glue::glue(
    "請根據以下數值，給 {context$group} 一段健康建議：\n",
    "- 目前 PM2.5：{context$pm25} µg/m³（AQI {context$aqi}）\n",
    "- 推薦『呼吸路徑』相對最快路線：暴露下降 {context$exposure_drop_pct}%、",
    "多花 {context$extra_min} 分鐘\n",
    "- 沿途綠覆率：{context$green_pct}%"
  )
  out <- llm_complete(sys, usr)
  if (is.na(out)) .explain_fallback(context) else out
}

# ── 追問問答 ──
answer_question <- function(context, question) {
  sys <- glue::glue(
    "你是空污健康助手，服務對象是{context$group}。當前數值：",
    "PM2.5 {context$pm25} µg/m³（AQI {context$aqi}）；",
    "推薦路線暴露下降 {context$exposure_drop_pct}%、多花 {context$extra_min} 分、",
    "綠覆 {context$green_pct}%。請用繁體中文、3 句以內回答使用者的問題。"
  )
  out <- llm_complete(sys, question)
  if (is.na(out)) .answer_fallback(context, question) else out
}

# ── 離線模板（無金鑰或失敗時） ──
.explain_fallback <- function(ctx) {
  glue::glue(
    "目前此區 PM2.5 約 {ctx$pm25} µg/m³（AQI {ctx$aqi}）。",
    "對{ctx$group}，建議走『呼吸路徑』：多花約 {ctx$extra_min} 分鐘，",
    "但累積暴露可降低 {ctx$exposure_drop_pct}%，沿途綠覆率 {ctx$green_pct}%。"
  )
}

.answer_fallback <- function(ctx, q) {
  glue::glue(
    "（離線示範回覆）關於「{q}」：目前 PM2.5 約 {ctx$pm25}（AQI {ctx$aqi}）。",
    "推薦的呼吸路徑暴露較低（↓{ctx$exposure_drop_pct}%）、綠覆 {ctx$green_pct}%，",
    "對{ctx$group}較友善；建議避開交通高峰並留意當下體感。"
  )
}
