test_that("valid_ranges cover every sidebar input and match the reference data", {
  expect_setequal(names(valid_ranges), names(input_vars))
  expect_setequal(names(input_labels), names(input_vars))
  expect_true(all(input_vars %in% names(data)))
  for (id in names(valid_ranges)) {
    rng <- valid_ranges[[id]]
    expect_lte(rng$min, rng$max)
  }
  # Immature cells are entered in %, so their ranges must be the % columns
  expect_equal(valid_ranges$myeloblasts$max, signif(max(data$myeloblastes_percent), 4))
})

test_that("the model returns a probability for the default UI values", {
  newdata <- tibble::tibble(
    BM_biopsy_age = 40, leucocytes = 5.84, hematies = 5.9, hemoglobine = 156,
    hematocrite = 49, vgm = 82.7, tcmh = 26.4, ccmh = 320, idr_cv = 15.4,
    plaquettes = 244, vpm = 10.6, poly_neutrophiles = 3.99, lymphocytes = 1.10,
    monocytes = 0.54, poly_eosinophiles = 0.16, poly_basophiles = 0.06,
    erythroblastes_percent = 0, myelemie_percent = 0.3, myeloblastes_percent = 0
  ) %>%
    dplyr::mutate(
      poly_neutrophiles_percent = poly_neutrophiles * 100 / leucocytes,
      poly_eosinophiles_percent = poly_eosinophiles * 100 / leucocytes,
      poly_basophiles_percent   = poly_basophiles * 100 / leucocytes,
      lymphocytes_percent       = lymphocytes * 100 / leucocytes,
      monocytes_percent         = monocytes * 100 / leucocytes,
      myelemie       = myelemie_percent * leucocytes / 100,
      erythroblastes = erythroblastes_percent * leucocytes / 100,
      myeloblastes   = myeloblastes_percent * leucocytes / 100
    )

  pred <- predict_fibrosis(newdata)
  expect_equal(pred[["Yes"]] + pred[["No"]], 1, tolerance = 1e-6)
  expect_equal(pred[["Yes"]], 0.1002, tolerance = 1e-3)
})

test_that("get_empirical_uncertainty finds a bin over the whole [0, 1] range", {
  for (p in c(0, 0.1166833, 0.5, 0.999, 1)) {
    res <- get_empirical_uncertainty(p, uncertainty_lookup)
    expect_false(is.null(res))
    expect_true(is.finite(res$pred_sd_bin))
  }
})

test_that("the uncertainty interval always contains the prediction", {
  grid <- seq(0, 1, by = 0.001)
  for (level in list(c("empirical_q025", "empirical_q975"), c("empirical_iqr_low", "empirical_iqr_high"))) {
    iv <- t(sapply(grid, get_prediction_interval, lookup_table = uncertainty_lookup,
                   lower = level[1], upper = level[2]))
    expect_true(all(iv[, "lower"] <= grid & grid <= iv[, "upper"]))
    expect_true(all(iv >= 0 & iv <= 1))
    # No jump in width at bin boundaries
    expect_lt(max(abs(diff(iv[, "upper"] - iv[, "lower"]))), 0.01)
  }
})

test_that("the JSON booster reproduces the original model predictions", {
  pred <- predict_fibrosis(data)
  expect_equal(pred$Yes, pred_all$Yes, tolerance = 1e-6)
})

test_that("missing values still give a probability", {
  nd <- data[1, model_features]
  nd[1, ] <- NA_real_
  p <- predict_fibrosis(nd)$Yes
  expect_true(p > 0 && p < 1)
})

test_that("a prediction with missing values is never confident", {
  expect_true(is_confident(0.05))
  expect_true(is_confident(0.95))
  expect_false(is_confident(0.5))
  expect_false(is_confident(0.05, n_missing = 1))
  expect_false(is_confident(0.95, n_missing = 3))
})

test_that("inputs excluded from the confidence rule do not change the prediction", {
  nd <- data[, model_features]
  p_full <- predict_fibrosis(nd)$Yes
  for (id in inputs_not_used_by_model) nd[[input_vars[[id]]]] <- NA_real_
  expect_equal(predict_fibrosis(nd)$Yes, p_full)
})
