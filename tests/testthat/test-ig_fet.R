# Correctness checks -----------------------------------------------------------

fet_zvals <- function(acronym) {
  switch(
    acronym,
    gafcrl = qnorm(c(0.03, 0.1, 0.5, 0.9, 0.97)),
    gwgfga = qnorm(c(0.03, 0.1, 0.25, 0.5, 0.75, 0.9, 0.97)),
    pifga = qnorm(c(0.03, 0.05, 0.1, 0.5, 0.9, 0.95, 0.97)),
    rifga = qnorm(c(0.03, 0.05, 0.1, 0.5, 0.9, 0.95, 0.97)),
    sdrfga = qnorm(c(0.03, 0.05, 0.1, 0.5, 0.9, 0.95, 0.97)),
    tcdfga = qnorm(c(0.03, 0.05, 0.1, 0.5, 0.9, 0.95, 0.97)),
    gaftcd = qnorm(c(0.03, 0.05, 0.1, 0.5, 0.9, 0.95, 0.97)),
    poffga = qnorm(c(0.03, 0.05, 0.5, 0.95, 0.97)),
    sffga = qnorm(c(0.03, 0.05, 0.5, 0.95, 0.97)),
    avfga = qnorm(c(0.03, 0.05, 0.5, 0.95, 0.97)),
    pvfga = qnorm(c(0.03, 0.05, 0.5, 0.95, 0.97)),
    cmfga = qnorm(c(0.03, 0.05, 0.5, 0.95, 0.97)),
    hefwfga = qnorm(c(0.03, 0.05, 0.1, 0.5, 0.9, 0.95, 0.97)),
    -3:3) # Where z-score tables are available
}

fet_pvals <- function(acronym) {
  switch(
    acronym,
    crlfga = c(0.03, 0.10, 0.25, 0.50, 0.75, 0.90, 0.97),
    gwgfga = c(0.03, 0.10, 0.25, 0.50, 0.75, 0.90, 0.97),
    gafcrl = c(0.03, 0.10, 0.50, 0.90, 0.97),
    poffga = c(0.03, 0.05, 0.5, 0.95, 0.97),
    sffga = c(0.03, 0.05, 0.5, 0.95, 0.97),
    avfga = c(0.03, 0.05, 0.5, 0.95, 0.97),
    pvfga = c(0.03, 0.05, 0.5, 0.95, 0.97),
    cmfga = c(0.03, 0.05, 0.5, 0.95, 0.97),
    c(0.03, 0.05, 0.1, 0.5, 0.9, 0.95, 0.97)
  )
}

fet_roundto <- function(acronym) {
  ifelse(test = acronym %in% c("pifga", "rifga", "sdrfga", "poffga", "sffga",
                               "avfga", "pvfga", "cmfga", "tcdfga", "gwgfga",
                               "gaftcd"),
         yes = 2,
         no = ifelse(acronym %in% c("efwfga", "gafcrl", "hefwfga"),
                     yes = 0,
                     no = 1))
}


#' @srrstats {G5.4, G5.4c} Tests ensure that `gigs` functions can be used to
#'   replicate published growth charts, within a tolerance.
test_that(desc = "Conversion of z-scores/centiles to values works", {
  for (acronym in names(gigs::ig_fet)) {
    for (chr_z_or_p in c("zscore", "centile")) {
      ref_tbl <- gigs::ig_fet[[acronym]][[paste0(chr_z_or_p, "s")]]
      if (is.null(ref_tbl)) {
        ref_tbl <- gigs::ig_fet[[acronym]][[1]]
      }
      dbl_z_or_p <- switch(chr_z_or_p,
                           centile = fet_pvals(acronym),
                           zscore = fet_zvals(acronym))
      xvar <- ref_tbl[[1]]
      conv_fn <- getFromNamespace(paste0(chr_z_or_p, "2value"), ns = "gigs")
      pkg_tbl <- lapply(X = dbl_z_or_p,
                        FUN = \(z_or_p) {
                          #' @srrstats {G5.5} Correctness test run with fixed
                          #'   seed
                          withr::with_seed(seed = 1000, {
                            round(conv_fn(z_or_p,
                                          x = xvar,
                                          family = "ig_fet",
                                          acronym = acronym),
                                  digits = fet_roundto(acronym))
                          })
                        }) |>
        do.call(what = cbind) |>
        as.data.frame() |>
        setNames(names(ref_tbl)[-1])
      col1_name <- names(ref_tbl)[1]
      pkg_tbl[[col1_name]] <- xvar
      pkg_tbl <- pkg_tbl[, c(ncol(pkg_tbl), 1:(ncol(pkg_tbl) - 1))]
      expect_equal(object = pkg_tbl, expected = ref_tbl, tolerance = 0.01,
                   ignore_attr = TRUE)
    }
  }
})

#' @srrstats {G5.5, G5.6, G5.6a, G5.6b, G5.9b} Checks that conversion
#'   functionality works when converting values to z-scores/centiles AND vice
#'   versa. Uses multiple fixed seeds to generate random inputs, which do not
#'   affect the functions' results.
test_that(
  desc = "Conversion of values to z-scores works",
  code = {
    for (acronym in names(gigs::ig_fet)) {
      for (chr_z_or_p in c("zscore", "centile")) {
        xvar <- ig_fet[[acronym]][[1]][[1]]
        for (seed in seq(300, 400, 30)) {
          withr::with_seed(seed, code = {
            dbl_z_or_p <- rnorm(n = length(xvar))
          })
          if (chr_z_or_p == "centile") {
            dbl_z_or_p <- pnorm(dbl_z_or_p)
          }

          fn_zp2val <- getFromNamespace(paste0(chr_z_or_p, "2value"),
                                        ns = "gigs")
          y_gigs <- fn_zp2val(dbl_z_or_p, x = xvar,
                              family = "ig_fet",
                              acronym = acronym)

          fn_val2zp <- getFromNamespace(paste0("value2", chr_z_or_p),
                                        ns = "gigs")
          gigs_z_or_p <- fn_val2zp(y_gigs, x = xvar,
                                   family = "ig_fet",
                                   acronym = acronym)

          expect_equal(gigs_z_or_p, expected = dbl_z_or_p,
                       tolerance = sqrt(.Machine$double.eps))
        }
      }
    }
  }
)

#' @srrstats {G5.9, G5.9a} Trivial noise does not meaningfully alter results.
test_that(
  desc = "Conversion of values to z-scores works with trivial noise",
  code = {
    for (acronym in names(gigs::ig_fet)) {
      for (chr_z_or_p in c("zscore", "centile")) {
        xvar <- ig_fet[[acronym]][[1]][[1]]
        xrange <- range(xvar)
        withr::with_seed(seed = 50, code = {
          xvar <- jitter(xvar, 1)
          xvar[xvar < xrange[1]] <- xrange[1]
          xvar[xvar > xrange[2]] <- xrange[2]
          dbl_z_or_p <- rnorm(n = length(xvar))
        })
        if (chr_z_or_p == "centile") dbl_z_or_p <- pnorm(dbl_z_or_p)

        fn_zp2val <- getFromNamespace(paste0(chr_z_or_p, "2value"),
                                      ns = "gigs")
        y_gigs <- fn_zp2val(dbl_z_or_p, x = xvar,
                            family = "ig_fet",
                            acronym = acronym)

        fn_val2zp <- getFromNamespace(paste0("value2", chr_z_or_p),
                                      ns = "gigs")
        gigs_z_or_p <- fn_val2zp(y_gigs + .Machine$double.eps,
                                 x = xvar,
                                 family = "ig_fet",
                                 acronym = acronym)

        expect_equal(gigs_z_or_p, expected = dbl_z_or_p,
                     tolerance = sqrt(.Machine$double.eps))
      }
    }
  }
)