# 基本 QC 單元測試
# 執行：  source("R/bootstrap.R"); testthat::test_dir("tests/testthat")

testthat::test_that("BAD 抓得到缺值與超範圍", {
  obs <- generate_demo_observations()
  flags <- run_qc(obs, CONFIG)
  bad <- dplyr::filter(flags, qc_type == "BAD")
  testthat::expect_true(any(bad$check == "bad_missing"))
  testthat::expect_true(any(bad$check == "bad_range"))
})

testthat::test_that("SAD 抓得到 flatline 或 drift", {
  obs <- generate_demo_observations()
  flags <- run_qc(obs, CONFIG)
  sad <- dplyr::filter(flags, qc_type == "SAD")
  testthat::expect_true(any(sad$check %in% c("sad_flatline", "sad_drift",
                                             "sad_outlier")))
})

testthat::test_that("穩健 z 對定值回傳 0", {
  testthat::expect_equal(robust_z(rep(5, 10)), rep(0, 10))
})

testthat::test_that("PSI 對同分布接近 0", {
  set.seed(1); x <- rnorm(500)
  testthat::expect_lt(compute_psi(x, rnorm(500)), 0.1)
})
