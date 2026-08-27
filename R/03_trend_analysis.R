# R/03_trend_analysis.R - Test Mann-Kendall e Sen's Slope

# compute_municipal_mk_sen_long <- function(df, value_var = "pol_value", group_var = "COMUNE", year_var = "year", p_alpha = 0.05) {
#   message(">> Calcolo trend per: ", group_var)
#   
#   df |>
#     dplyr::group_by(.data[[group_var]], .data[[year_var]]) |>
#     dplyr::summarise(
#       mean_val = mean(.data[[value_var]], na.rm = TRUE),
#       n_sezioni = dplyr::n(),
#       .groups = "drop_last"
#     ) |>
#     dplyr::arrange(.data[[year_var]]) |>
#     dplyr::summarise(
#       n_anni        = dplyr::n(),
#       n_sezioni_avg = round(mean(n_sezioni)),
#       mk_pvalue     = trend::mk.test(mean_val)$p.value,
#       sen_slope     = as.numeric(trend::sens.slope(mean_val)$estimates),
#       stat_z        = trend::mk.test(mean_val)$statistic,
#       .groups       = "drop"
#     ) |>
#     dplyr::mutate(
#       trend_sig = mk_pvalue < p_alpha,
#       direzione = dplyr::case_when(
#         trend_sig & sen_slope < 0 ~ "Decrescente significativo",
#         trend_sig & sen_slope > 0 ~ "Crescente significativo",
#         TRUE ~ "Non significativo"
#       )
#     )
# }

# include la correzione FDR (Benjamini-Hochberg) per i confronti multipli
compute_municipal_mk_sen_long <- function(df, 
                                          value_var = "pol_value", 
                                          group_var = "COMUNE", 
                                          year_var = "year", 
                                          p_alpha = 0.05, 
                                          adjust_method = "BH") {
  
  message(">> Calcolo trend per: ", group_var)
  
  df |>
    # 1. Aggregazione annuale per Comune
    dplyr::group_by(.data[[group_var]], .data[[year_var]]) |>
    dplyr::summarise(
      mean_val = mean(.data[[value_var]], na.rm = TRUE),
      n_sezioni = dplyr::n(),
      .groups = "drop"
    ) |>
    
    # 2. Calcolo MK e Sen per ciascun Comune
    dplyr::group_by(.data[[group_var]]) |>
    dplyr::arrange(.data[[year_var]]) |>
    dplyr::summarise(
      n_anni        = dplyr::n(),
      n_sezioni_avg = round(mean(n_sezioni, na.rm = TRUE)),
      
      # Calcola MK una sola volta per evitare ridondanze e controlla n_anni >= 3
      mk_pvalue = if (dplyr::n() >= 3) trend::mk.test(mean_val)$p.value else NA_real_,
      stat_z    = if (dplyr::n() >= 3) as.numeric(trend::mk.test(mean_val)$statistic) else NA_real_,
      sen_slope = if (dplyr::n() >= 3) as.numeric(trend::sens.slope(mean_val)$estimates) else NA_real_,
      .groups   = "drop"
    ) |>
    
    # 3. CORREZIONE PER CONFRONTI MULTIPLI
    dplyr::mutate(
      # Calcolo del p-value aggiustato su tutti i Comuni contemporaneamente
      p_adj = stats::p.adjust(mk_pvalue, method = adjust_method),
      
      # La significatività si basa ora su p_adj
      trend_sig = !is.na(p_adj) & p_adj < p_alpha,
      direzione = dplyr::case_when(
        trend_sig & sen_slope < 0 ~ "Decrescente significativo",
        trend_sig & sen_slope > 0 ~ "Crescente significativo",
        TRUE ~ "Non significativo"
      )
    )
}

# function to prepare dataset for culative plot
prep_exposure_data <- function(data, keys, ...) {
  # Capture grouping variables (e.g., PROVINCIA, COMUNE)
  group_vars <- enquos(...)
  
  data |> 
    # Filter using the new unique identifier
    filter(key %in% keys) |> 
    
    # Group by the key and any extra spatial variables
    # We include pol_name and year here so they remain in the final df
    group_by(key, pol_name, year, !!!group_vars, pol_value) |> 
    summarise(P1 = sum(P1, na.rm = TRUE), .groups = "drop_last") |> 
    
    # Sort by the pollution value for the cumulative sum
    arrange(pol_value, .by_group = TRUE) |> 
    
    # Calculate Cumulative Percentages
    mutate(
      cum_pop = cumsum(P1),
      #pct_cum_pop = cum_pop / sum(P1, na.rm = TRUE)
      pct_cum_pop = cum_pop / max(cum_pop, na.rm = TRUE) # Bulletproof denominator
    ) |> 
    ungroup() |> 
    # Ensure year is a factor for plotting aesthetics
    mutate(year = factor(year))
}

