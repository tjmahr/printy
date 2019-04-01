
#' @export
pretty_lme4_ranefs <- function(model) {
  vars <- dplyr::vars
  funs <- dplyr::funs

  table <-  tidy_ranef_summary(model)

  ranef_names <- setdiff(names(table), c("var1", "grp", "vcov", "sdcor"))

  table <- table %>%
    # Format the numbers
    dplyr::mutate_at(c("vcov", "sdcor"), funs(format_fixef_num)) %>%
    dplyr::mutate_at(
      vars(dplyr::one_of(ranef_names)),
      funs(format_ranef_cor)
    ) %>%
    sort_ranef_grps() %>%
    # Format variable names and group names
    dplyr::mutate_(
      var1 = ~ fmt_replace_na(var1, "&nbsp;"),
      grp =  ~ str_replace_same_as_previous(grp, "&nbsp;")) %>%
    dplyr::rename_(
      Group = ~ grp,
      Parameter = ~ var1,
      Variance = ~ vcov,
      SD = ~ sdcor)

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
    dplyr::filter_(~ is.na(var2)) %>%
    dplyr::select_(~ -var2)
}

tidy_lme4_covariances <- function(model) {
  lme4::VarCorr(model) %>%
    as.data.frame() %>%
    dplyr::filter_(~ !is.na(var2))
}

# Create a data-frame with random effect variances and correlations
tidy_ranef_summary <- function(model) {
  vars <- tidy_lme4_variances(model)
  cors <- tidy_lme4_covariances(model) %>%
    dplyr::select_(~ -vcov)

  # Create some 1s for the diagonal of the correlation matrix
  self_cor <- vars %>%
    dplyr::select_( ~ -vcov) %>%
    dplyr::mutate_(var2 = ~ var1, sdcor = ~ 1.0) %>%
    stats::na.omit()

  # Spread out long-from correlations into a matrix
  cor_df <- dplyr::bind_rows(cors, self_cor) %>%
    dplyr::mutate_(sdcor = ~ fmt_fix_digits(sdcor, 2))

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
    lapply(tidyr::spread_, "var1", "sdcor") %>%
    lapply(dplyr::arrange_, ~ dplyr::desc(var2)) %>%
    lapply(blank_param_col_names) %>%
    lapply(tibble::repair_names) %>%
    lapply(dplyr::mutate_, .dots = list(var2 = ~ as.character(var2))) %>%
    # lapply(tibble::rownames_to_column, "..sort") %>%
    dplyr::bind_rows() %>%
    dplyr::rename_(var1 = ~ var2)

  sorting_names <- utils::tail(names(cor_matrix), -2)

  dplyr::left_join(vars, cor_matrix, by = c("grp", "var1")) %>%
    dplyr::arrange_(.dots = list(c("grp", sorting_names)))
}

# Sort random effects groups, and make sure residual comes last
sort_ranef_grps <- function(df) {
  residual <- dplyr::filter_(df, ~ grp == "Residual")
  df %>%
    dplyr::filter_(~ grp != "Residual") %>%
    dplyr::arrange_(~ grp) %>%
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
