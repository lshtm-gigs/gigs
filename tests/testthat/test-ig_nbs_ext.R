# Correctness checks -----------------------------------------------------------

nbs_ext_roundto <- function(acronym) {
  switch(acronym, wfga = 2,  1)
}

nbs_ext_pvals <- function() c(0.03, 0.05, 0.10, 0.50, 0.90, 0.95, 0.97)

nbs_ext_zvals <- function() seq(-3, 3, 1)

nbs_ext_tolerance <- function(acronym) {
  switch(acronym,
         wfga = 0.01,
         lfga = 0.1,
         hcfga = 0.1)
}

#' @srrstats {G5.4, G5.4c} Tests ensure that `gigs` functions can be used to
#'   replicate growth charts, within a tolerance.
test_that(
  desc = "Conversion of z-scores/centiles to values works",
  code = {
    for (acronym in names(gigs::ig_nbs_ext)) {
      for (chr_sex in c("male", "female")) {
        for (chr_z_or_p in c("zscore", "centile")) {
          ref_tbl <- gigs::ig_nbs_ext[[acronym]][[chr_sex]][[paste0(chr_z_or_p, "s")]]
          if (is.null(ref_tbl)) {
            ref_tbl <- gigs::ig_nbs_ext[[acronym]][[chr_sex]][[1]]
          }
          dbl_z_or_p <- switch(chr_z_or_p,
                               centile = nbs_ext_pvals(),
                               zscore = nbs_ext_zvals())
          xvar <- ref_tbl[[1]]
          sexvar <- if (chr_sex != "female") "M" else "F"
          conv_fn <- getFromNamespace(paste0(chr_z_or_p, "2value"), ns = "gigs")
          pkg_tbl <- lapply(X = dbl_z_or_p,
                            FUN = \(zp) {
                              conv_fn(zp, xvar, sexvar, "ig_nbs_ext", acronym) |>
                                round(digits = nbs_ext_roundto(acronym))
                            }) |>
            do.call(what = cbind) |>
            as.data.frame() |>
            setNames(names(ref_tbl)[-1])
          ref_tbl <- ref_tbl[-1]
          differences <- abs(pkg_tbl - ref_tbl)
          tolerance <- nbs_ext_tolerance(acronym)
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
    for (acronym in names(gigs::ig_nbs_ext)) {
      for (chr_z_or_p in c("zscore", "centile")) {
        xvar <- gigs::ig_nbs_ext[[acronym]][[1]][[1]][[1]]
        for (seed in seq(300, 400, 30)) {
          withr::with_seed(seed, {
            dbl_z_or_p <- rnorm(n = length(xvar))
            sexvar <- sample(c("M", "F"), size = length(xvar), replace = TRUE)
          })
          if (chr_z_or_p == "centile") dbl_z_or_p <- pnorm(dbl_z_or_p)

        fn_zp2val <- getFromNamespace(paste0(chr_z_or_p, "2value"), ns = "gigs")
        y_gigs <- fn_zp2val(dbl_z_or_p, xvar, sexvar, "ig_nbs_ext", acronym)

        fn_val2zp <- getFromNamespace(paste0("value2", chr_z_or_p),
                                      ns = "gigs")
        gigs_z_or_p <- fn_val2zp(y_gigs, xvar, sexvar, "ig_nbs_ext", acronym)

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
    for (acronym in names(gigs::ig_nbs_ext)) {
      for (chr_z_or_p in c("zscore", "centile")) {
        xvar <- ig_nbs_ext[[acronym]][[1]][[1]][[1]]
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
        y_gigs <- fn_zp2val(dbl_z_or_p, xvar, sexvar, "ig_nbs_ext", acronym)

        fn_val2zp <- getFromNamespace(paste0("value2", chr_z_or_p),
                                      ns = "gigs")
        gigs_z_or_p <- fn_val2zp(y_gigs + .Machine$double.eps,
                                 xvar, sexvar, "ig_nbs_ext", acronym)

        expect_equal(gigs_z_or_p, expected = dbl_z_or_p,
                     tolerance = sqrt(.Machine$double.eps))
      }
    }
  }
)
