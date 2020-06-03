
#' @export
pretty_lme4_ranefs <- function(model) {
  vars <- dplyr::vars
  funs <- dplyr::funs

  table <- tidy_ranef_summary(model)

  ranef_names <- setdiff(names(table), c("var1", "grp", "vcov", "sdcor"))

  table <- table %>%
    # Format the numbers
    dplyr::mutate_at(c("vcov", "sdcor"), format_fixef_num) %>%
    dplyr::mutate_at(
      vars(dplyr::one_of(ranef_names)),
      format_ranef_cor
    ) %>%
    sort_ranef_grps() %>%
    # Format variable names and group names
    dplyr::mutate(
      var1 = fmt_replace_na(.data$var1, "&nbsp;"),
      grp = str_replace_same_as_previous(.data$grp, "&nbsp;")
    ) %>%
    dplyr::rename(
      Group = .data$grp,
      Parameter = .data$var1,
      Variance = .data$vcov,
      SD = .data$sdcor
    )

  # Rename columns 5:n to c("Correlations", "&nbsp;", ..., "&nbsp;")
  names_to_replace <- seq(from = 5, to = length(names(table)))
  new_names <- rep("&nbsp;", length(names_to_replace))
  new_names[1] <- "Correlations"
  names(table)[names_to_replace] <- new_names

  table
}

tidy_lme4_variances <- function(model) {
  lme4::VarCorr(model) %>%
    as.data.frame() %>%
    dplyr::filter(is.na(.data$var2)) %>%
    dplyr::select(-.data$var2)
}

tidy_lme4_covariances <- function(model) {
  lme4::VarCorr(model) %>%
    as.data.frame() %>%
    dplyr::filter(!is.na(.data$var2))
}

# Create a data-frame with random effect variances and correlations
tidy_ranef_summary <- function(model) {
  vars <- tidy_lme4_variances(model)
  cors <- tidy_lme4_covariances(model) %>%
    dplyr::select(-.data$vcov)

  # Create some 1s for the diagonal of the correlation matrix
  self_cor <- vars %>%
    dplyr::select(-.data$vcov) %>%
    dplyr::mutate(var2 = .data$var1, sdcor = 1.0) %>%
    stats::na.omit()

  # Spread out long-from correlations into a matrix
  cor_df <- dplyr::bind_rows(cors, self_cor) %>%
    dplyr::mutate(sdcor = fmt_fix_digits(.data$sdcor, 2))

  # Sort the var1, var2 columns by descending frequency of variable names
  sort_vars <- function(xs) {
    sorted1 <- rev(sort(table(xs$var1)))
    sorted2 <- rev(sort(table(xs$var2)))
    xs$var1 <- factor(xs$var1, names(sorted1))
    xs$var2 <- factor(xs$var2, names(sorted2))
    xs[1:4]
  }

  blank_param_col_names <- function(xs) {
    stats::setNames(xs, c("grp", "var2", rep("", length(xs) - 2)))
  }

  cor_matrix <- split(cor_df, cor_df$grp) %>%
    lapply(sort_vars) %>%
    lapply(tidyr::spread, "var1", "sdcor") %>%
    lapply(dplyr::arrange, dplyr::desc(.data$var2)) %>%
    lapply(blank_param_col_names) %>%
    lapply(tibble::repair_names) %>%
    lapply(dplyr::mutate, var2 = as.character(.data$var2)) %>%
    dplyr::bind_rows() %>%
    dplyr::rename(var1 = .data$var2)

  sorting_names <- utils::tail(names(cor_matrix), -2)
  sorters <- syms(c("grp", sorting_names))

  dplyr::left_join(vars, cor_matrix, by = c("grp", "var1"))
}

# Sort random effects groups, and make sure residual comes last
sort_ranef_grps <- function(df) {
  residual <- dplyr::filter(df, .data$grp == "Residual")
  df %>%
    dplyr::filter(.data$grp != "Residual") %>%
    dplyr::arrange(.data$grp) %>%
    dplyr::bind_rows(residual)
}

format_fixef_num <- function(xs) {
  xs %>%
    fmt_fix_digits(2) %>%
    fmt_minus_sign()
}

format_ranef_cor <- function(xs) {
  xs %>%
    fmt_leading_zero() %>%
    fmt_minus_sign() %>%
    fmt_replace_na(replacement = "&nbsp;")
}
