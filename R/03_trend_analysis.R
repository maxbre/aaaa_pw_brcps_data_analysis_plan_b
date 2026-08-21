# R/03_trend_analysis.R - Test Mann-Kendall e Sen's Slope

compute_municipal_mk_sen_long <- function(df, value_var = "pol_value", group_var = "COMUNE", year_var = "year", p_alpha = 0.05) {
  message(">> Calcolo trend per: ", group_var)
  
  df |>
    dplyr::group_by(.data[[group_var]], .data[[year_var]]) |>
    dplyr::summarise(
      mean_val = mean(.data[[value_var]], na.rm = TRUE),
      n_sezioni = dplyr::n(),
      .groups = "drop_last"
    ) |>
    dplyr::arrange(.data[[year_var]]) |>
    dplyr::summarise(
      n_anni        = dplyr::n(),
      n_sezioni_avg = round(mean(n_sezioni)),
      mk_pvalue     = trend::mk.test(mean_val)$p.value,
      sen_slope     = as.numeric(trend::sens.slope(mean_val)$estimates),
      stat_z        = trend::mk.test(mean_val)$statistic,
      .groups       = "drop"
    ) |>
    dplyr::mutate(
      trend_sig = mk_pvalue < p_alpha,
      direzione = dplyr::case_when(
        trend_sig & sen_slope < 0 ~ "Decrescente significativo",
        trend_sig & sen_slope > 0 ~ "Crescente significativo",
        TRUE ~ "Non significativo"
      )
    )
}
