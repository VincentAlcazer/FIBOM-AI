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

  pred <- predict(fibrosis_model, newdata = newdata, type = "prob", na.action = "na.pass")
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
