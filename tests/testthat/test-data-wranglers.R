# ==============================================================================
# 📦 Functions: wrangle_data() and ww_wrangle_data()
# ==============================================================================

# ------------------------------------------------------------------------------
# 🔒 Internal: wrangle_data()
# ------------------------------------------------------------------------------

## ---- Case: `.gam-based = "wfhz"` --------------------------------------------

testthat::test_that(
  "returns the expected outputs when `.gam_based` is set to 'flag_wfhz' ",
  {
    ## Sample data ----
    x <- anthro |>
      dplyr::rename(longitude = x, latitude = y) |>
      dplyr::filter(district == "Kotido") |>
      mwana::mw_wrangle_wfhz(
        sex = sex,
        .recode_sex = FALSE,
        weight = weight,
        height = height
      ) |>
      mwana::define_wasting(
        zscores = wfhz,
        .by = "zscores",
        edema = oedema
      )

    ## Observed results ----
    r <- do.call(what = wrangle_data, args = list(.data = x, .gam_based = "wfhz"))

    ## Test checks ----
    testthat::expect_type(object = r, type = "list")
    testthat::expect_true(length(r) == 3)
    testthat::expect_s3_class(object = r[[1]], class = "tbl_df")
    testthat::expect_s3_class(object = r[[1]], class = "tbl_df")
    testthat::expect_s3_class(object = r[[1]], class = "tbl_df")
    testthat::expect_true(length(r[3]) == length(unique(r[3])))
    testthat::expect_true(all(c("locationid", "cases") %in% names(r[[1]])))
    testthat::expect_true(all(c("locationid", "ctrls") %in% names(r[[2]])))
    testthat::expect_true(all(c("locationid", "longitude", "latitude") %in% names(r[[3]])))
    testthat::expect_true(sum(r[[1]][2]) == 26)
    testthat::expect_true(sum(r[[2]][2]) == 307)
  }
)

## ---- Case: `.gam-based = "muac"` --------------------------------------------

testthat::test_that(
  "returns the expected outputs when `.gam_based` is set to 'flag_mfaz' ",
  {
    ## Sample data ----
    x <- anthro |>
      dplyr::rename(longitude = x, latitude = y) |>
      dplyr::filter(district == "Kotido") |>
      mwana::mw_wrangle_age(age = age) |>
      mwana::mw_wrangle_muac(
        sex = sex,
        .recode_sex = FALSE,
        age = age,
        muac = muac,
        .recode_muac = TRUE,
        .to = "cm"
      ) |>
      dplyr::mutate(muac = mwana::recode_muac(muac, .to = "mm")) |>
      mwana::define_wasting(muac = muac, .by = "muac", edema = oedema)


    ## Observed results ----
    r <- do.call(
      what = wrangle_data,
      args = list(.data = x, .gam_based = "muac")
    )

    ## Test checks ----
    testthat::expect_type(object = r, type = "list")
    testthat::expect_true(length(r) == 3)
    testthat::expect_s3_class(object = r[[1]], class = "tbl_df")
    testthat::expect_s3_class(object = r[[1]], class = "tbl_df")
    testthat::expect_s3_class(object = r[[1]], class = "tbl_df")
    testthat::expect_true(length(r[3]) == length(unique(r[3])))
    testthat::expect_true(all(c("locationid", "cases") %in% names(r[[1]])))
    testthat::expect_true(all(c("locationid", "ctrls") %in% names(r[[2]])))
    testthat::expect_true(all(c("locationid", "longitude", "latitude") %in% names(r[[3]])))
    testthat::expect_true(sum(r[[1]][2]) == 70)
    testthat::expect_true(sum(r[[2]][2]) == 261)
  }
)

## ---- Case: `.gam-based = "combined"` ----------------------------------------

testthat::test_that(
  "returns the expected outputs when `.gam_based` is set to 'combined' ",
  {
    ### Observed data ----
    x <- anthro |>
      dplyr::rename(longitude = x, latitude = y) |>
      dplyr::filter(district == "Kotido") |>
      mwana::mw_wrangle_wfhz(
        sex = sex,
        .recode_sex = FALSE,
        weight = weight,
        height = height
      ) |>
      mwana::mw_wrangle_age(age = age) |>
      mwana::mw_wrangle_muac(
        sex = sex,
        .recode_sex = FALSE,
        muac = muac,
        .recode_muac = TRUE,
        .to = "cm",
        age = age
      ) |>
      dplyr::mutate(muac = mwana::recode_muac(muac, .to = "mm")) |>
      mwana::define_wasting(
        zscores = wfhz,
        muac = muac,
        .by = "combined",
        edema = oedema
      )

    ### Observed results ----
    r <- do.call(
      what = wrangle_data,
      args = list(.data = x, .gam_based = "combined")
    )

    ### Test checks ----
    testthat::expect_type(object = r, type = "list")
    testthat::expect_true(length(r) == 3)
    testthat::expect_s3_class(object = r[[1]], class = "tbl_df")
    testthat::expect_s3_class(object = r[[1]], class = "tbl_df")
    testthat::expect_s3_class(object = r[[1]], class = "tbl_df")
    testthat::expect_true(length(r[3]) == length(unique(r[3])))
    testthat::expect_true(all(c("locationid", "cases") %in% names(r[[1]])))
    testthat::expect_true(all(c("locationid", "ctrls") %in% names(r[[2]])))
    testthat::expect_true(all(c("locationid", "longitude", "latitude") %in% names(r[[3]])))
    testthat::expect_true(sum(r[[1]][2]) == 87)
    testthat::expect_true(sum(r[[2]][2]) == 259)
  }
)

# ------------------------------------------------------------------------------
# 🌐 External: ww_wrangle_data()
# ------------------------------------------------------------------------------

## ---- SaTScan input files get created and saved accordingly ------------------

testthat::test_that(
  "prepares and saves case, controls and geo files into a user-specified dir",
  {
    ### Sample data ----
    x <- anthro |>
      dplyr::rename(longitude = x, latitude = y) |>
      dplyr::filter(district == "Kotido") |>
      mwana::mw_wrangle_wfhz(
        sex = sex,
        .recode_sex = FALSE,
        weight = weight,
        height = height
      ) |>
      mwana::define_wasting(
        zscores = wfhz,
        .by = "zscores",
        edema = oedema
      )

    ### Create a temporary directory ----
    tmp <- withr::local_tempdir() # ensures cleanup after test
    out_dir <- file.path(tmp, "input-files") # this will be the dir

    ### Observed results ----
    do.call(
      what = ww_wrangle_data,
      args = list(
        .data = x,
        filename = "localityA",
        dir = out_dir,
        .gam_based = "wfhz"
      )
    )

    ## The tests ----
    testthat::expect_true(file.exists(file.path(out_dir, "localityA.cas")))
    testthat::expect_true(file.exists(file.path(out_dir, "localityA.ctl")))
    testthat::expect_true(file.exists(file.path(out_dir, "localityA.geo")))
    testthat::expect_message(
      object = do.call(
        what = ww_wrangle_data,
        args = list(
          .data = x,
          filename = "localityA",
          dir = out_dir,
          .gam_based = "wfhz"
        )
      ), regexp = paste0("`", basename(out_dir), "` already exists in project repo."), fixed = TRUE
    )
  }
)
