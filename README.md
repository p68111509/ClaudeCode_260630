# GeoAI Copilot — R 原型

**主產品：GeoAI Copilot** — 空污健康導航(呼吸路徑)與綠覆引擎,並串 LLM 做 AI 解釋。
附加工具:**維運 Dashboard**(BAD vs SAD 品管)。
業主:國立成功大學測量系 · 吳治達老師研究室。

📄 文件都放在 [`docs/`](docs/):規格書 [`docs/SPEC.md`](docs/SPEC.md)、產品概念 [`docs/GeoAI-Copilot-concept.html`](docs/GeoAI-Copilot-concept.html)、工作坊講義 [`docs/workshop.html`](docs/workshop.html)、科技樹 [`docs/GeoAI-Copilot-Tech-Tree.html`](docs/GeoAI-Copilot-Tech-Tree.html)。

## 快速開始

```r
# 工作目錄設在專案根目錄
setwd("C:/Claude/GeoAI-project")
source("install_deps.R")    # 首次:安裝套件

source("run_copilot.R")     # ★ 啟動主產品 GeoAI Copilot
# 或
source("run_dashboard.R")   # 啟動附加的維運 Dashboard
```

預設 demo 模式,使用合成資料,**免金鑰**即可操作。

## 主產品:GeoAI Copilot（`app/copilot/`）

利用者向けの健康ナビ:
1. 選族群(一般/氣喘/孕婦/長者/兒童)與起訖點(台南地標)。
2. 地圖顯示 **PM2.5 暴露推估色塊 + 測站 + 綠地**。
3. 規劃並比較 **最快路線 vs 呼吸路徑**(避開高暴露、行經綠地)。
4. 指標卡:多花時間 / 暴露下降 / 綠覆率。
5. **Copilot 健康解釋**:個人化建議 + 自由追問(LLM;無金鑰時用離線模板)。

> 產品只使用「QC 通過」的資料:server 啟動時會濾除 BAD 的 PM2.5(NA/超範圍),再建暴露網格 — 與維運 Dashboard 的品管同源。

## 附加工具:維運 Dashboard（`app/maintenance/`）

監控資料品質:總覽 KPI、來源紅黃綠燈、BAD 明細(擋下)、SAD 明細(觀察)、趨勢、API 金鑰狀態。
品管二軸見 SPEC:**BAD**(壞掉→擋下)vs **SAD**(可疑→觀察)。

## 資料夾分層

```
geoai-copilot/
├── README.md               專案說明（本檔）
├── docs/                   📄 所有文件
│   ├── SPEC.md                 規格書
│   ├── workshop.html           工作坊講義
│   ├── GeoAI-Copilot-concept.html  產品概念頁
│   ├── *.docx                  原始需求/規劃文件
│   └── img/                    講義用截圖
├── run_copilot.R           ★ 啟動主產品
├── run_dashboard.R         啟動維運 Dashboard
├── install_deps.R          套件安裝
├── DESCRIPTION             相依套件 + 專案根錨點
├── config/
│   ├── config.yml          門檻、區域、demo 開關
│   └── .env.example        API 金鑰範本
├── R/
│   ├── bootstrap.R         載入順序(套件→設定→API→functions→models)
│   ├── api/endpoints.R     ★ 所有 API 集中於此,只 source 它
│   ├── functions/          純函式(utils/fetch/transform/qc_*/demo_data)
│   ├── models/             模型(與 function 分開)
│   │   ├── exposure_model.R   引擎 A:暴露推估(IDW;接點)
│   │   ├── route_model.R      引擎 B:呼吸路徑(距離加權暴露)
│   │   └── greenness_model.R  引擎 C:綠覆/碳(NDVI 場)
│   └── llm/explain.R       引擎 D:LLM 解釋 + 追問
└── app/
    ├── copilot/            ★ 主產品(app.R + ui_def.R + server_def.R)
    └── maintenance/        附加維運 Dashboard
```

## 維護要點

- **改 API** → 只動 `R/api/endpoints.R`。
- **改門檻** → 只動 `config/config.yml`。
- **改模型** → 只動 `R/models/`。
- **啟動邏輯** → 收斂在 `R/bootstrap.R`;各 app 的 `app.R` 只負責偵測根目錄 + source + 啟動。
- 注意:app 資料夾內 UI/Server 命名為 `ui_def.R` / `server_def.R`(避免與 Shiny 的 `ui.R`/`server.R` 自動載入機制衝突)。
