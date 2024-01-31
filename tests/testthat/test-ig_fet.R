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
         no = ifelse(acronym %in% c("efwfga", "gafcrl"), yes = 0, no = 1))
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
      conv_fn <- get(paste0("ig_fet_", acronym, "_", chr_z_or_p, "2value"))
      pkg_tbl <- lapply(X = dbl_z_or_p,
                        FUN = \(zp) {
                          #' @srrstats {G5.5} Correctness test run with fixed
                          #'   random seed
                          set.seed(1000)
                          round(conv_fn(zp, xvar),
                                digits = fet_roundto(acronym))
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
        for (seed in seq(1, 100, 30)) {
          set.seed(seed = seed)
          dbl_z_or_p <- rnorm(n = length(xvar))
          if (chr_z_or_p == "centile") dbl_z_or_p <- pnorm(dbl_z_or_p)

          fn_stem <- paste0("ig_fet_", acronym)
          fn_zp2val <- get(paste0(fn_stem, "_", chr_z_or_p, "2value"))
          y_gigs <- fn_zp2val(dbl_z_or_p, xvar)

          fn_val2zp <- get(paste0(fn_stem, "_value2", chr_z_or_p))
          gigs_z_or_p <- fn_val2zp(y_gigs, xvar)

          expect_equal(gigs_z_or_p, expected = dbl_z_or_p,
                       tolerance = sqrt(.Machine$double.eps))
        }
      }
    }
  })

#' @srrstats {G5.9, G5.9a} Trivial noise does not meaningfully alter results.
test_that(
  desc = "Conversion of values to z-scores works with trivial noise",
  code = {
    for (acronym in names(gigs::ig_fet)) {
      for (chr_z_or_p in c("zscore", "centile")) {
        xvar <- ig_fet[[acronym]][[1]][[1]]
        set.seed(seed = 50)
        dbl_z_or_p <- rnorm(n = length(xvar))
        if (chr_z_or_p == "centile") dbl_z_or_p <- pnorm(dbl_z_or_p)

        fn_stem <- paste0("ig_fet_", acronym)
        fn_zp2val <- get(paste0(fn_stem, "_", chr_z_or_p, "2value"))
        y_gigs <- fn_zp2val(dbl_z_or_p, xvar)

        fn_val2zp <- get(paste0(fn_stem, "_value2", chr_z_or_p))
        gigs_z_or_p <- fn_val2zp(y_gigs + .Machine$double.eps, xvar)

        expect_equal(gigs_z_or_p, expected = dbl_z_or_p,
                     tolerance = sqrt(.Machine$double.eps))
      }
    }
  })

# Appropriate errors -----------------------------------------------------------

#' Regenerate messages from checkmate for testing
#' @param name Single-length character vector with name of input.
#' @param wanted Single-length character vector with name of expected data type.
#' @param got Single-length character vector with name of received data type.
#' @description Used to test `checkmate` error messages. These errors are given
#'   by `checkmate` assert_*-style functions in check_params.R.
error_type <- function(name, wanted, got) {
  paste0("Assertion on '", name, "' failed: Must be of type '", wanted,
         "', not '", got, "'.")
}

#' @param name Name of input vector.
error_length <- function(name) {
  paste0("Assertion on '", name,
         "' failed: Must have length >= 1, but has length 0.")
}


#' @srrstats {G5.2, G5.2a, G5.2b} Explicit tests of error and warning behaviour
#'   for bad data types.
test_that(
  desc = "Correct errors occur when .gigs_options are set to 'quiet':",
  code = {
    # GIGS will no longer give warnings for out-of-bound `x` elements
    for (option in names(.gigs_options)) {
      gigs_option_set(option, new_value = "quiet", silent = TRUE)
    }

    x <- seq(154, 280, by = 0.5)
    len_x <- length(x)
    z <- pnorm(rep_len(-3:3, len_x))
    acronym <- rep_len(names(gigs::ig_fet), len_x)

    # Test failures for each arg when converting centiles to values
    expect_error(
      ig_fet_zscore2value(as.character(z), x, acronym),
      regexp = error_type(name = "z", wanted = "numeric", got = "character")
    )
    expect_error(
      ig_fet_zscore2value(z, as.character(x), acronym),
      regexp = error_type(name = "x", wanted = "numeric", got = "character")
    )
    expect_error(
      ig_fet_zscore2value(z, x, 1),
      regexp = error_type(name = "acronym", wanted = "character", got = "double")
    )

    # And for conversion of values to centiles
    y <- ig_fet_zscore2value(z, x, acronym)
    expect_error(
      ig_fet_value2zscore(as.character(y), x, acronym),
      regexp = error_type(name = "y", wanted = "numeric", got = "character")
    )
    expect_error(
      ig_fet_value2zscore(y, as.character(x), acronym),
      regexp = error_type(name = "x", wanted = "numeric", got = "character")
    )
    expect_error(
      ig_fet_value2zscore(y, x, 1),
      regexp = error_type(name = "acronym", wanted = "character", got = "double")
    )

    # All bad acronyms cause function to error
    expect_error(
      object = ig_fet_zscore2value(z = 0.6, x = 50, acronym = "wrong_acronym"),
      regexp = paste0("Variable 'acronym': All elements were invalid.")
    )

    # Zero-length arguments cause function to error
    expect_error(
      object = ig_fet_zscore2value(z = double(), x = 50,
                                   acronym = "efwfga"),
      regexp = error_length("z")
    )
    expect_error(
      object = ig_fet_zscore2value(z = z, x = double(),
                                   acronym = "efwfga"),
      regexp = "Can't recycle `.*` \\(size [[:digit:]]*\\) to match `.*` \\(size 0\\)\\."
    )
    expect_error(
      object = ig_fet_zscore2value(z = z, x = 50,
                                   acronym = character()),
      regexp = "Can't recycle `.*` \\(size [[:digit:]]*\\) to match `.*` \\(size 0\\)\\."
    )
  }
)

# test_that(
#   desc = "Correct warnings occur when .gigs_options are set to 'warn':",
#   code = {
#     # GIGS will now give warnings for bad inputs which are not going to cause
#     # checkmate to throw an error
#     for (option in names(.gigs_options)) {
#       gigs_option_set(option, new_value = "warn", silent = TRUE)
#     }
#
#     x <- seq(154, 280, by = 0.5)
#     len_x <- length(x)
#     z <- pnorm(rep_len(-3:3, len_x))
#
#     set.seed(200)
#     acronym <- sample(names(gigs::ig_fet), size = len_x, replace = TRUE)
#
#     # Warn if centiles are out of bounds
#
#     # Warn if some data is missing
#
#     # Warn if some x variables are out of bounds
#
#     # Warn if
#   }
# )

# test_that(
#   desc = "Correct errors occur when .gigs_options are set to 'error':",
#   code = {
#     # GIGS will now give warnings for bad inputs which are not going to cause
#     # checkmate to throw an error
#     for (option in names(.gigs_options)) {
#       gigs_option_set(option, new_value = "error", silent = TRUE)
#     }
#
#     x <- seq(154, 280, by = 0.5)
#     len_x <- length(x)
#     z <- pnorm(rep_len(-3:3, len_x))
#
#     set.seed(200)
#     acronym <- sample(names(gigs::ig_fet), size = len_x, replace = TRUE)
#
#     # Warn if centiles are out of bounds
#
#     # Warn if some data is missing
#
#     # Warn if some x variables are out of bounds
#
#     # Warn if
#   }
# )

# Testing against other R implementations --------------------------------------

#' @srrstats {G5.4, G5.4a, G5.4b} Test correctness against other R
#'   implementations.
test_that(
  desc = "GIGS aligns with `growthstandards` package",
  code = {
    skip_if_not_installed(pkg = "growthstandards")
    y_gigs <- 45
    y_growthstandards <- 45 / 10
    gs_fns <- list(growthstandards::igfet_hccm2zscore,
                   growthstandards::igfet_bpdcm2zscore,
                   growthstandards::igfet_accm2zscore,
                   growthstandards::igfet_flcm2zscore,
                   growthstandards::igfet_ofdcm2zscore)
    gigs_fns <- list(ig_fet_hcfga_value2zscore,
                     ig_fet_bpdfga_value2zscore,
                     ig_fet_acfga_value2zscore,
                     ig_fet_flfga_value2zscore,
                     ig_fet_ofdfga_value2zscore)
    for (idx in seq_along(gs_fns)) {
      gigs <- gigs_fns[[idx]](y_gigs, 154)
      gs <- gs_fns[[idx]](154, y_growthstandards)
      # Set tolerance as 10e-5 to accomodate slight error in BPD-for-GA
      # equations in `growthstandards`
      expect_equal(object = gigs, expected = gs, tolerance = 10e-5)
    }
  }
)

# Functions still work with odd class structures -------------------------------

#' @srrstats {G2.6, EA2.6} INTERGROWTH-21st Fetal Growth functions still
#' operate even when univariate inputs have unusual class structures.
test_that(
  desc = "Test that univariate input works with alternate class structures",
  code = {
    skip_on_cran()

    # Conversion of values to z-scores works
    y <- units::set_units(x = 2500, g)
    ga <- units::set_units(x = 234, days)
    acronym <- "efwfga"
    expect_vector(ig_fet_value2centile(y, ga, acronym),
                  ptype = double(),
                  size = 1)

    # Estimation of fetal weight works
    headcirc <- units::set_units(x = 29L, cm)
    abdocirc <- units::set_units(x = 26L, cm)
    efw <- ig_fet_estimate_fetal_weight(headcirc_mm = headcirc,
                                        abdocirc_mm = abdocirc)
    expect_vector(efw, ptype = double(), size = 1)
  }
)