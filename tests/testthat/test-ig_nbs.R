# Correctness checks -----------------------------------------------------------

nbs_roundto <- function(acronym) {
  switch(acronym, wfga = 2, wlrfga = 2, ffmfga = 0,  1)
}

nbs_pvals <- function(acronym) {
  if (acronym %in% c("wfga", "hcfga", "lfga", "wlrfga")) {
    c(0.03, 0.05, 0.10, 0.50, 0.90, 0.95, 0.97)
  } else if (acronym %in% c("ffmfga", "bfpfga", "fmfga")) {
    c(0.03,0.10, 0.50, 0.90, 0.97)
  } else if (!acronym %in% names(gigs::ig_nbs)) {
    stop("Bad acronym.")
  }
}

nbs_zvals <- function(acronym) {
  if (acronym %in% c("wfga", "hcfga", "lfga", "wlrfga")) {
    -3:3
  } else if (acronym %in% c("ffmfga", "bfpfga", "fmfga")) {
    qnorm(c(0.03,0.10, 0.50, 0.90, 0.97))
  } else if (!acronym %in% names(gigs::ig_nbs)) {
    stop("Bad acronym.")
  }
}

nbs_tolerance <- function(sex, acronym) {
  # 23/08/2023: With old linear equations for body composition:
  ## FMFGA: male = 26, female = 15
  ## BFPFGA: male = 0.5, female = 0.5
  ## FFMFGA: male = 7, female = 5
  switch(acronym,
         wfga = 0.01,
         lfga = 0.1,
         hcfga = 0.1,
         wlrfga = 0.01,
         ffmfga = switch(sex, male = 6, female = 1),
         bfpfga = switch(sex, male = 0.1, female = 0.1),
         fmfga = switch(sex, male = 1.9, female = 0.1)) + 1e-13
}

#' @srrstats {G5.4, G5.4c} Tests ensure that `gigs` functions can be used to
#'   replicate published growth charts, within a tolerance.
test_that(
  desc = "Conversion of z-scores/centiles to values works",
  code = {
    for (acronym in names(gigs::ig_nbs)[5]) {
      for (chr_sex in c("male", "female")) {
        for (chr_z_or_p in c("zscore", "centile")) {
          ref_tbl <- gigs::ig_nbs[[acronym]][[chr_sex]][[paste0(chr_z_or_p, "s")]]
          if (is.null(ref_tbl)) {
            ref_tbl <- gigs::ig_nbs[[acronym]][[chr_sex]][[1]]
          }
          dbl_z_or_p <- switch(chr_z_or_p,
                               centile = nbs_pvals(acronym),
                               zscore = nbs_zvals(acronym))
          xvar <- ref_tbl[[1]]
          sexvar <- if (chr_sex != "female") "M" else "F"
          conv_fn <- getFromNamespace(paste0(chr_z_or_p, "2value"), ns = "gigs")
          pkg_tbl <- lapply(X = dbl_z_or_p,
                            FUN = \(zp) {
                              conv_fn(zp, xvar, sexvar, "ig_nbs", acronym) |>
                                round(digits = nbs_roundto(acronym))
                            }) |>
            do.call(what = cbind) |>
            as.data.frame() |>
            setNames(names(ref_tbl)[-1])
          ref_tbl <- ref_tbl[-1]
          differences <- abs(pkg_tbl - ref_tbl)
          tolerance <- nbs_tolerance(chr_sex, acronym)
          maxdiff <- max(differences, na.rm = TRUE)
          expect_true(max(differences, na.rm = TRUE) <= tolerance)
        }
      }
    }
  }
)

#' @srrstats {G5.5, G5.6, G5.6a, G5.6b, G5.9b} Checks that conversion
#'   functionality works when converting values to z-scores/centiles AND vice
#'   versa. Uses multiple fixed seeds to generate random inputs, which do not
#'   affect the functions' results.
test_that(
  desc = "Conversion of values to z-scores works",
  code = {
    for (acronym in names(gigs::ig_nbs)) {
      for (chr_z_or_p in c("zscore", "centile")) {
        xvar <- gigs::ig_nbs[[acronym]][[1]][[1]][[1]]
        for (seed in seq(300, 400, 30)) {
          withr::with_seed(seed, {
            dbl_z_or_p <- rnorm(n = length(xvar))
            sexvar <- sample(c("M", "F"), size = length(xvar), replace = TRUE)
          })
          if (chr_z_or_p == "centile") dbl_z_or_p <- pnorm(dbl_z_or_p)

        fn_zp2val <- getFromNamespace(paste0(chr_z_or_p, "2value"), ns = "gigs")
        y_gigs <- fn_zp2val(dbl_z_or_p, xvar, sexvar, "ig_nbs", acronym)

        fn_val2zp <- getFromNamespace(paste0("value2", chr_z_or_p),
                                      ns = "gigs")
        gigs_z_or_p <- fn_val2zp(y_gigs, xvar, sexvar, "ig_nbs", acronym)

          expect_equal(gigs_z_or_p, expected = dbl_z_or_p, tolerance = 10e-10)
        }
      }
    }
  }
)

#' @srrstats {G5.9, G5.9a} Trivial noise does not meaningfully alter results.
test_that(
  desc = "Conversion of values to z-scores works with trivial noise",
  code = {
    for (acronym in names(gigs::ig_nbs)) {
      for (chr_z_or_p in c("zscore", "centile")) {
        xvar <- ig_nbs[[acronym]][[1]][[1]][[1]]
        xrange <- range(xvar)
        withr::with_seed(50, code = {
          xvar <- jitter(xvar, 1)
          xvar[xvar < xrange[1]] <- xrange[1]
          xvar[xvar > xrange[2]] <- xrange[2]
          dbl_z_or_p <- rnorm(n = length(xvar))
          sexvar <- sample(c("M", "F"), size = length(xvar), replace = TRUE)
        })
        if (chr_z_or_p == "centile") dbl_z_or_p <- pnorm(dbl_z_or_p)

        fn_zp2val <- getFromNamespace(paste0(chr_z_or_p, "2value"), ns = "gigs")
        y_gigs <- fn_zp2val(dbl_z_or_p, xvar, sexvar, "ig_nbs", acronym)

        fn_val2zp <- getFromNamespace(paste0("value2", chr_z_or_p),
                                      ns = "gigs")
        gigs_z_or_p <- fn_val2zp(y_gigs + .Machine$double.eps,
                                 xvar, sexvar, "ig_nbs", acronym)

        expect_equal(gigs_z_or_p, expected = dbl_z_or_p,
                     tolerance = sqrt(.Machine$double.eps))
      }
    }
  }
)
