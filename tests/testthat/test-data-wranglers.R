# ---- Test check: data wranglers ----------------------------------------------

testthat::test_that(
  "wrangle_data() returns the expected outputs when `.for` is set to 'flag_wfhz' ",
  {
    ### Sample data ----
    x <- df |>
      mwana::mw_wrangle_wfhz(
        sex = sex,
        .recode_sex = TRUE,
        weight = weight,
        height = height
      ) |>
      mwana::define_wasting(
        zscores = wfhz,
        .by = "zscores",
        edema = edema
      )

    ### Observed results ----
    r <- do.call(what = wrangle_data, args = list(.data = x, .for = "wfhz"))

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
    testthat::expect_true(sum(r[[1]][2]) == 104)
    testthat::expect_true(sum(r[[2]][2]) == 428)
  }
)

testthat::test_that(
  "wrangle_data() returns the expected outputs when `.for` is set to 'flag_mfaz' ",
  {
    ### Sample data ----
    x <- df |>
      mwana::mw_wrangle_muac(
        sex = sex,
        .recode_sex = TRUE,
        age = age,
        muac = muac,
        .recode_muac = TRUE,
        .to = "cm"
      ) |>
      dplyr::mutate(muac = mwana::recode_muac(muac, .to = "mm")) |>
      mwana::define_wasting(muac = muac, .by = "muac", edema = edema)


    ### Observed results ----
    r <- do.call(
      what = wrangle_data,
      args = list(.data = x, .for = "muac")
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
    testthat::expect_true(sum(r[[1]][2]) == 115)
    testthat::expect_true(sum(r[[2]][2]) == 430)
  }
)

testthat::test_that(
  "wrangle_data() returns the expected outputs when `.for` is set to 'combined' ",
  {
    ### Observed data ----
    x <- df |>
      mwana::mw_wrangle_wfhz(
        sex = sex,
        .recode_sex = TRUE,
        weight = weight,
        height = height
      ) |>
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
        edema = edema
      )

    ### Observed results ----
    r <- do.call(
      what = wrangle_data,
      args = list(.data = x, .for = "combined")
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
    testthat::expect_true(sum(r[[1]][2]) == 174)
    testthat::expect_true(sum(r[[2]][2]) == 373)
  }
)
