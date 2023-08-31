#' Retrieve coefficients for coefficient-based standards
#'
#' @description Retrieves growth standard coefficient values where needed for
#' the WHO Child Growth Standards and INTERGROWTH-21<sup>st</sup> Newborn Size
#' @param x X variable in whatever range and units are appropriate for the
#' standard(s) in use.
#' @param sex Sex(es), either `"M"` (male) or `"F"` (female).
#' @param acronym Acronym(s) denoting which LMS/GAMLSS-based growth standards
#' are in use.
#' @return A dataframe containing coefficients (where avilable) for each
#' provided combination of sex, gestational age, and acronym.
#' @rdname retrieve_coefficients
#' @keywords internal
#' @noRd
retrieve_coefficients <- function(x, sex, acronym, coeff_tbls, coeff_names) {
  key_x <- seq_along(x)
  if (!all(unique(acronym) %in% names(coeff_tbls))) {
    acronym[!acronym %in% names(coeff_tbls)] <- NA_character_
  }

  # Cache which are NA for x/sex/acronym - these cannot have coeffs
  is_na_x <- is.na(x)
  is_na_x_sex_acronym <- is_na_x | !sex %in% c("M", "F") | is.na(acronym)

  # Cache which aren't NA for x/sex/acronym - not NAs will be
  #   1. Retrieved if IN RANGE and %in%
  #   2. Interpolated if IN RANGE but not %in%
  #   3. Left as NA if not IN RANGE
  not_NA_key <- key_x[!is_na_x_sex_acronym]
  not_NA_x <- x[!is_na_x_sex_acronym]
  not_NA_sex <- sex[!is_na_x_sex_acronym]
  not_NA_acronym <- acronym[!is_na_x_sex_acronym]
  not_NA_sex_acronym <- paste0(not_NA_sex, "_", not_NA_acronym)

  # Initialise empty matrix to fill up
  out <- matrix(nrow = length(x), ncol = length(coeff_names),
                dimnames = list(as.character(key_x), coeff_names))

  # Vectors with values for retrieval steps - these are edited in the for loop
  # below and in the lapply() call for coeffs_interped.
  lgl_needs_retrieval <- logical(length = length(not_NA_x))
  chr_needs_retrieval <- character(length = length(not_NA_x))

  # This approach is long but minimises the length of vectors given to paste0(),
  # which reduces the function runtime
  not_NA_sex_u <- unique(not_NA_sex)
  not_NA_acr_u <- unique(not_NA_acronym)
  not_NA_sex_len <- length(not_NA_sex_u)
  not_NA_acr_len <- length(not_NA_acr_u)
  notNA_sex_acr_max_len <- not_NA_sex_len * not_NA_acr_len
  notNA_sex_r <- rep.int(not_NA_sex_u, times = not_NA_acr_len)
  notNA_acr_r <- rep_len(not_NA_acr_u, notNA_sex_acr_max_len)
  unique_sex_acronym <- character(length = notNA_sex_acr_max_len)
  for (i in 1:notNA_sex_acr_max_len) {
    sex_ <- notNA_sex_r[i]
    acr_ <- notNA_acr_r[i]
    sex_acr <- paste0(sex_, "_", acr_)
    needs_loading <- not_NA_sex == sex_ & not_NA_acronym == acr_
    needs_retrieval <- not_NA_sex == sex_ & not_NA_acronym == acr_
    x_ <- not_NA_x[needs_retrieval]
    if (any(needs_loading)) {
      unique_sex_acronym[i] <- sex_acr
    }
    if (any(needs_retrieval)) {
      sex_acr_x <- paste0(sex_acr, "_", x_)
      lgl_needs_retrieval[needs_retrieval] <- T
      chr_needs_retrieval[needs_retrieval] <- sex_acr_x
    }
  }
  unique_sex_acronym <- unique_sex_acronym[unique_sex_acronym != ""]

  # 2. Load coeff tables as needed
  coeffs_li <- lapply(
    X = unique_sex_acronym,
    FUN = \(x) load_coeff_matrices(sex_acronym = x, coeff_tbls = coeff_tbls)
  ) |>
    `names<-`(unique_sex_acronym)

  # 2a. Return NA coefficient values if no coeff tables were loaded
  no_coeff_tables <- all(vapply(coeffs_li,
                                FUN = is.null,
                                FUN.VALUE = logical(length = 1L)))
  if (no_coeff_tables) {
    return(matrix(nrow = length(x), ncol = length(coeff_names)))
  }

  # 3. For named unique sex/acronym combination in coeffs_li, get the
  # coefficients. Check whether there are x values which require either
  # retrieval or interpolation. Return a list of matrices with interpolation if
  # performed, then do.call("rbind") to get into one overall matrix of
  # interpolated coefficients.
  coeffs_interped <- lapply(
    X = unique_sex_acronym,
    FUN = \(sex_acro) {
      # Get coeff matrix for this sex/acronym
      coeff_mat <- .subset2(coeffs_li, sex_acro)
      # Store values of x with this sex and acronym
      is_curr_sex_acro <- not_NA_sex_acronym == sex_acro
      current_x <- not_NA_x[is_curr_sex_acro]
      # Cache x vars from current coeff matrix to prevent copying in `[` uses
      long_x <- coeff_mat[,1]
      x_in_range <- inrange(current_x, long_x)
      should_interp <- x_in_range & !current_x %in% long_x
      doesnt_need_retrieval <- !x_in_range | should_interp

      # Set lgl_needs_retrieval to FALSE where necessary
      lgl_needs_retrieval[is_curr_sex_acro][doesnt_need_retrieval] <<- FALSE

      interp_matrix <- function(matrix_rownames) {
        interpolated <- interpolate_coeffs(
          x_to_interp = current_x[should_interp],
          coeff_mat = coeff_mat,
          coeff_names = coeff_names,
          coeff_mat_x = long_x
        )
        rownames(interpolated) <- matrix_rownames
        interpolated
      }

      any_need_lerping <- any(should_interp, na.rm = TRUE)
      key_interp <- not_NA_key[is_curr_sex_acro][should_interp]
      if (any_need_lerping) {
        interp_matrix(key_interp)
      }
    }
  ) |>
    do.call(what = "rbind")

  # Retrieve coefficients with specially formatted values in chr_needs_retrieval
  coeffs_long_mat <- do.call(what = "rbind", coeffs_li)
  if (any(lgl_needs_retrieval)) {
    mat_keys <- chr_needs_retrieval[lgl_needs_retrieval]
    coeffs_retrieved <- coeffs_long_mat[mat_keys,
                                        2:(1 + length(coeff_names)),
                                        drop = FALSE] |>
      `rownames<-`(not_NA_key[lgl_needs_retrieval])
  }

  # See if interpolation/retrieval done --> if both, rbind before assigning to
  # out to prevent any weird subsetting/copying issues in memory
  isnull_interp <- is.null(coeffs_interped)
  exists_retrieved <- exists(x = "coeffs_retrieved")
  if (isnull_interp) {
    out[rownames(coeffs_retrieved), ] <- coeffs_retrieved
  } else if (!exists_retrieved) {
    out[rownames(coeffs_interped), ] <- coeffs_interped
  } else {
    coeffs_interped_retrieved <- rbind(coeffs_interped, coeffs_retrieved)
    out[rownames(coeffs_interped_retrieved), ] <- coeffs_interped_retrieved
  }
  out
}

#' Load coefficient tables for `retrieve_coefficients()`
#'
#' @param sex Sex, either `"M"` (male) or `"F"` (female).
#' @param acronym Acronym denoting which coefficient table is needed.
#' @param coeff_tbls A nested list of coefficient tables, either
#' `gigs::ig_nbs_coeffs` or `gigs::who_gs_coeffs` for the coefficient-based
#' INTERGROWTH-21<sup>st</sup> Newborn Size standards or WHO Child Growth
#' Standards, respectively.
#' @note Uses the internal `coeff_rownames` object, information on which can be
#' found in `data-raw/sysdata.R`.
#' @returns A matrix with coefficients from `coeff_tbls` specified by the
#' combination of `sex` and `acronym`.
#' @keywords internal
#' @noRd
load_coeff_matrices <- function(sex_acronym, coeff_tbls) {
  split <- unlist(strsplit(sex_acronym, split = "_"))
  sex <- split[1]
  acronym <- split[2]
  if (sex == "NA" || acronym == "NA") {
    return(NULL)
  }
  sex_long <- if (sex == "M") "male" else "female"
  coeff_mat <- as.matrix(coeff_tbls[[acronym]][[sex_long]], ncol = 4)
  rownames(coeff_mat) <- coeff_rownames[[sex_acronym]]
  coeff_mat
}

#' Linearly interpolate in LMS/MSNT coefficient tables
#' @param x_to_interp X values at which to get interpolated values of the
#' coefficients named in `coeff_names`.
#' @param coeff_mat A matrix of coefficients in which to interpolate, with the
#' names found in `coeff_names`.
#' @param coeff_names The names of the coefficients to be interpolated.
#' @param coeff_mat_x X values from the coefficient matrix. This is
#' `coeff_mat[, 1]` by default but can be overridden by the user.
#' @return Matrix containing interpolated coefficients, with the values of
#' `coeff_names` as column names.
#' @importFrom stats approx
#' @keywords internal
#' @noRd
interpolate_coeffs <- function(coeff_names,
                               coeff_mat,
                               x_to_interp,
                               coeff_mat_x = coeff_mat[, 1]) {
  interpolated <- vapply(
    X = coeff_names,
    FUN.VALUE = numeric(length = length(x_to_interp)),
    FUN = \(coeff_name) {
      stats::approx(x = coeff_mat_x,
                    y = coeff_mat[, coeff_name],
                    xout = x_to_interp)$y
    })
  if (!is.matrix(interpolated)) interpolated <- t(interpolated)
  interpolated
}