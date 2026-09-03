# R/05_bootstrap_uncertainty.R - Bootstrap Spazio-Temporale e IID

# run_bootstrap_ac_spatiotemporal_opt <- function(data, col_years = paste0("no2_", 2019:2025), col_pop30p = "p30p", col_tasso = "tasso", col_end = "target_oms_no2", cluster_var = "cod_comune", B = 1000, seed = 1234) {
#   if (!is.null(seed)) set.seed(seed)
#   stopifnot(cluster_var %in% names(data), all(col_years %in% names(data)))
#   
#   n_rows     <- nrow(data)
#   num_years  <- length(col_years)
#   mean_log_RR <- log(data$rr[1])
#   sd_log_RR   <- (log(data$rr_uic[1]) - log(data$rr_lic[1])) / (2 * 1.96)
#   
#   attesi_vec <- data[[col_pop30p]] * (data[[col_tasso]] / 100)
#   exp_matrix <- as.matrix(data[, col_years])
#   target_val <- if (is.character(col_end) && col_end %in% names(data)) data[[col_end]] else as.numeric(col_end)
#   
#   cluster_indices <- split(seq_len(n_rows), data[[cluster_var]])
#   cluster_names   <- names(cluster_indices)
#   num_clusters    <- length(cluster_names)
#   
#   boot_casi_attr <- numeric(B)
#   boot_paf       <- numeric(B)
#   
#   pb <- txtProgressBar(min = 0, max = B, style = 3)
#   for (b in 1:B) {
#     sampled_year_idx <- sample.int(num_years, size = num_years, replace = TRUE)
#     sampled_clusters <- sample(cluster_names, size = num_clusters, replace = TRUE)
#     boot_row_idx     <- unlist(cluster_indices[sampled_clusters], use.names = FALSE)
#     
#     boot_exp_mat <- exp_matrix[boot_row_idx, sampled_year_idx, drop = FALSE]
#     boot_attesi  <- attesi_vec[boot_row_idx]
#     boot_target  <- if (length(target_val) > 1) target_val[boot_row_idx] else target_val
#     
#     no2_mean_boot <- rowMeans(boot_exp_mat, na.rm = TRUE)
#     delta_boot    <- no2_mean_boot - pmin(no2_mean_boot, boot_target)
#     
#     drawn_log_RR <- rnorm(1, mean = mean_log_RR, sd = sd_log_RR)
#     AF_i <- 1 - exp(- (drawn_log_RR / 10) * delta_boot)
#     AC_i <- boot_attesi * AF_i
#     
#     tot_attesi_b  <- sum(boot_attesi, na.rm = TRUE)
#     tot_casi_ac_b <- sum(AC_i, na.rm = TRUE)
#     
#     boot_casi_attr[b] <- tot_casi_ac_b
#     boot_paf[b]       <- tot_casi_ac_b / tot_attesi_b
#     setTxtProgressBar(pb, b)
#   }
#   close(pb)
#   
#   data.frame(casi_attribuibili = boot_casi_attr, paf = boot_paf)
# }
# 
# run_bootstrap_ac_simple <- function(data, col_exp = paste0("no2_", 2019:2025), col_pop30p = "p30p", col_tasso = "tasso", col_end = "target_oms_no2", B = 1000, seed = 1234) {
#   if (!is.null(seed)) set.seed(seed)
#   stopifnot(all(col_exp %in% names(data)))
#   
#   n_rows      <- nrow(data)
#   mean_log_RR <- log(data$rr[1])
#   sd_log_RR   <- (log(data$rr_uic[1]) - log(data$rr_lic[1])) / (2 * 1.96)
#   
#   no2_val <- if (length(col_exp) > 1) rowMeans(as.matrix(data[, col_exp]), na.rm = TRUE) else data[[col_exp]]
#   target_val <- if (is.character(col_end) && col_end %in% names(data)) data[[col_end]] else as.numeric(col_end)
#   
#   attesi_vec <- data[[col_pop30p]] * (data[[col_tasso]] / 100)
#   delta_vec  <- no2_val - pmin(no2_val, target_val)
#   
#   boot_casi_attr <- numeric(B)
#   boot_paf       <- numeric(B)
#   
#   pb <- txtProgressBar(min = 0, max = B, style = 3)
#   for (b in 1:B) {
#     idx <- sample.int(n_rows, size = n_rows, replace = TRUE)
#     sampled_attesi <- attesi_vec[idx]
#     sampled_delta  <- delta_vec[idx]
#     
#     drawn_log_RR <- rnorm(1, mean = mean_log_RR, sd = sd_log_RR)
#     AF_i <- 1 - exp(- (drawn_log_RR / 10) * sampled_delta)
#     AC_i <- sampled_attesi * AF_i
#     
#     boot_casi_attr[b] <- sum(AC_i, na.rm = TRUE)
#     boot_paf[b]       <- sum(AC_i, na.rm = TRUE) / sum(sampled_attesi, na.rm = TRUE)
#     setTxtProgressBar(pb, b)
#   }
#   close(pb)
#   
#   data.frame(casi_attribuibili = boot_casi_attr, paf = boot_paf)
# }

# compare_bootstrap_metrics <- function(..., var_name = "casi_attribuibili", methods = NULL, ref_method = 1) {
#   results_list <- list(...)
#   if (length(results_list) == 1 && is.list(results_list[[1]]) && !is.data.frame(results_list[[1]])) {
#     results_list <- results_list[[1]]
#   }
#   if (is.null(methods)) {
#     methods <- if (!is.null(names(results_list)) && all(names(results_list) != "")) names(results_list) else paste("Metodo", seq_along(results_list))
#   }
#   
#   numeric_list <- purrr::map2(results_list, methods, \(item, m_name) {
#     if (is.data.frame(item)) item[[var_name]] else item
#   })
#   
#   ref_idx <- if (is.character(ref_method)) match(ref_method, methods) else as.integer(ref_method)
#   
#   tibble::tibble(
#     Metodo   = methods,
#     Media    = purrr::map_dbl(numeric_list, \(x) mean(x, na.rm = TRUE)),
#     Mediana  = purrr::map_dbl(numeric_list, \(x) median(x, na.rm = TRUE)),
#     SD       = purrr::map_dbl(numeric_list, \(x) sd(x, na.rm = TRUE)),
#     Q2.5     = purrr::map_dbl(numeric_list, \(x) quantile(x, 0.025, na.rm = TRUE)),
#     Q97.5    = purrr::map_dbl(numeric_list, \(x) quantile(x, 0.975, na.rm = TRUE))
#   ) |> 
#     dplyr::mutate(
#       Ampiezza_IC95 = Q97.5 - Q2.5,
#       Ratio_SD      = SD / SD[ref_idx],
#       Ratio_IC95    = Ampiezza_IC95 / Ampiezza_IC95[ref_idx]
#     )
# }

# new version con calibrazione popolazione per calcolo AC coerente

# run_bootstrap_ac <- function(
#     data,
#     col_years = paste0("no2_", 2019:2025),
#     col_pop30p = "p30p",
#     col_tasso = "tasso",
#     col_end = "target_oms_no2",
#     cluster_var = "cod_comune",
#     B = 1000,
#     seed = 1234
# ) {
#   
#   # ------------------------------------------------------------
#   # 1. Controlli
#   # ------------------------------------------------------------
#   
#   if (!is.null(seed)) {
#     set.seed(seed)
#   }
#   
#   stopifnot(
#     cluster_var %in% names(data),
#     all(col_years %in% names(data)),
#     col_pop30p %in% names(data),
#     col_tasso %in% names(data),
#     all(c("rr", "rr_lic", "rr_uic") %in% names(data))
#   )
#   
#   # Controllo valori mancanti
#   if (anyNA(data[[col_pop30p]])) {
#     stop(
#       "Sono presenti NA in ", col_pop30p,
#       ". Gestire i valori mancanti prima del bootstrap."
#     )
#   }
#   
#   if (anyNA(data[[col_tasso]])) {
#     stop(
#       "Sono presenti NA in ", col_tasso,
#       ". Gestire i valori mancanti prima del bootstrap."
#     )
#   }
#   
#   if (anyNA(data[, col_years])) {
#     stop(
#       "Sono presenti NA nelle concentrazioni NO2. ",
#       "Gestire i valori mancanti prima del bootstrap."
#     )
#   }
#   
#   # ------------------------------------------------------------
#   # 2. Incertezza del RR
#   # ------------------------------------------------------------
#   
#   mean_log_RR <- log(data$rr[1])
#   
#   sd_log_RR <- (
#     log(data$rr_uic[1]) -
#       log(data$rr_lic[1])
#   ) / (2 * 1.96)
#   
#   # ------------------------------------------------------------
#   # 3. Popolazione regionale originale
#   # ------------------------------------------------------------
#   
#   P_regionale <- sum(
#     data[[col_pop30p]]
#   )
#   
#   # ------------------------------------------------------------
#   # 4. Matrice delle concentrazioni NO2
#   # ------------------------------------------------------------
#   
#   exp_matrix <- as.matrix(
#     data[, col_years, drop = FALSE]
#   )
#   
#   num_years <- length(col_years)
#   
#   # ------------------------------------------------------------
#   # 5. Target OMS
#   # ------------------------------------------------------------
#   
#   target_val <- if (
#     is.character(col_end) &&
#     length(col_end) == 1 &&
#     col_end %in% names(data)
#   ) {
#     data[[col_end]]
#   } else {
#     as.numeric(col_end)
#   }
#   
#   # ------------------------------------------------------------
#   # 6. Cluster comunali
#   # ------------------------------------------------------------
#   
#   cluster_indices <- split(
#     seq_len(nrow(data)),
#     data[[cluster_var]]
#   )
#   
#   cluster_names <- names(cluster_indices)
#   
#   num_clusters <- length(cluster_names)
#   
#   # ------------------------------------------------------------
#   # 7. Vettori per la distribuzione bootstrap
#   # ------------------------------------------------------------
#   
#   boot_AC  <- numeric(B)
#   boot_PAF <- numeric(B)
#   boot_RR  <- numeric(B)
#   
#   # ------------------------------------------------------------
#   # 8. Progress bar
#   # ------------------------------------------------------------
#   
#   pb <- txtProgressBar(
#     min = 0,
#     max = B,
#     style = 3
#   )
#   
#   # ------------------------------------------------------------
#   # 9. Bootstrap
#   # ------------------------------------------------------------
#   
#   for (b in seq_len(B)) {
#     
#     # ----------------------------------------------------------
#     # 9.1 Ricampionamento dei comuni
#     # ----------------------------------------------------------
#     
#     sampled_clusters <- sample(
#       cluster_names,
#       size = num_clusters,
#       replace = TRUE
#     )
#     
#     boot_row_idx <- unlist(
#       cluster_indices[sampled_clusters],
#       use.names = FALSE
#     )
#     
#     # ----------------------------------------------------------
#     # 9.2 Ricampionamento temporale
#     # ----------------------------------------------------------
#     
#     sampled_year_idx <- sample.int(
#       num_years,
#       size = num_years,
#       replace = TRUE
#     )
#     
#     # ----------------------------------------------------------
#     # 9.3 Estrazione dei dati bootstrap
#     # ----------------------------------------------------------
#     
#     boot_pop <- data[[col_pop30p]][boot_row_idx]
#     
#     boot_tasso <- data[[col_tasso]][boot_row_idx]
#     
#     boot_exp_mat <- exp_matrix[
#       boot_row_idx,
#       sampled_year_idx,
#       drop = FALSE
#     ]
#     
#     # ----------------------------------------------------------
#     # 9.4 Calibrazione della popolazione regionale
#     # ----------------------------------------------------------
#     
#     P_boot <- sum(boot_pop)
#     
#     weight_regionale <- P_regionale / P_boot
#     
#     boot_pop_cal <- boot_pop * weight_regionale
#     
#     # ----------------------------------------------------------
#     # 9.5 Numero atteso di casi
#     # ----------------------------------------------------------
#     
#     boot_attesi_cal <- (
#       boot_pop_cal *
#         boot_tasso /
#         100
#     )
#     
#     # ----------------------------------------------------------
#     # 9.6 Media temporale NO2
#     # ----------------------------------------------------------
#     
#     no2_mean_boot <- rowMeans(
#       boot_exp_mat
#     )
#     
#     # ----------------------------------------------------------
#     # 9.7 Delta rispetto al target OMS
#     # ----------------------------------------------------------
#     
#     boot_target <- if (length(target_val) > 1) {
#       
#       target_val[boot_row_idx]
#       
#     } else {
#       
#       target_val
#     }
#     
#     delta_boot <- pmax(
#       no2_mean_boot - boot_target,
#       0
#     )
#     
#     # ----------------------------------------------------------
#     # 9.8 Campionamento del RR
#     # ----------------------------------------------------------
#     
#     drawn_log_RR <- rnorm(
#       1,
#       mean = mean_log_RR,
#       sd = sd_log_RR
#     )
#     
#     drawn_RR <- exp(
#       drawn_log_RR
#     )
#     
#     # ----------------------------------------------------------
#     # 9.9 Attributable fraction
#     # ----------------------------------------------------------
#     
#     AF_i <- 1 -
#       exp(
#         -(drawn_log_RR / 10) *
#           delta_boot
#       )
#     
#     # ----------------------------------------------------------
#     # 9.10 Attributable cases
#     # ----------------------------------------------------------
#     
#     AC_i <- (
#       boot_attesi_cal *
#         AF_i
#     )
#     
#     # ----------------------------------------------------------
#     # 9.11 Totale regionale
#     # ----------------------------------------------------------
#     
#     tot_AC <- sum(
#       AC_i
#     )
#     
#     tot_attesi <- sum(
#       boot_attesi_cal
#     )
#     
#     # ----------------------------------------------------------
#     # 9.12 Salvataggio della replica
#     # ----------------------------------------------------------
#     
#     boot_AC[b] <- tot_AC
#     
#     boot_PAF[b] <- (
#       tot_AC /
#         tot_attesi
#     )
#     
#     boot_RR[b] <- drawn_RR
#     
#     setTxtProgressBar(pb, b)
#   }
#   
#   close(pb)
#   
#   # ------------------------------------------------------------
#   # 10. Output: distribuzione bootstrap
#   # ------------------------------------------------------------
#   
#   data.frame(
#     replicate = seq_len(B),
#     AC_boot = boot_AC,
#     PAF_boot = boot_PAF,
#     RR_boot = boot_RR
#   )
# }

# nuova versione con sezioni indipendenti, no clustering comunale

run_bootstrap_ac_ind <- function(
    data,
    col_years = paste0("no2_", 2019:2025),
    col_pop30p = "p30p",
    col_tasso = "tasso",
    col_end = "target_oms_no2",
    B = 1000,
    seed = 1234
) {
  
  # ------------------------------------------------------------
  # 1. Controlli
  # ------------------------------------------------------------
  
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  stopifnot(
    all(col_years %in% names(data)),
    col_pop30p %in% names(data),
    col_tasso %in% names(data),
    all(c("rr", "rr_lic", "rr_uic") %in% names(data))
  )
  
  # Controllo valori mancanti
  if (anyNA(data[[col_pop30p]])) {
    stop(
      "Sono presenti NA in ", col_pop30p,
      ". Gestire i valori mancanti prima del bootstrap."
    )
  }
  
  if (anyNA(data[[col_tasso]])) {
    stop(
      "Sono presenti NA in ", col_tasso,
      ". Gestire i valori mancanti prima del bootstrap."
    )
  }
  
  if (anyNA(data[, col_years])) {
    stop(
      "Sono presenti NA nelle concentrazioni NO2. ",
      "Gestire i valori mancanti prima del bootstrap."
    )
  }
  
  # ------------------------------------------------------------
  # 2. Incertezza del RR
  # ------------------------------------------------------------
  
  mean_log_RR <- log(data$rr[1])
  
  sd_log_RR <- (
    log(data$rr_uic[1]) -
      log(data$rr_lic[1])
  ) / (2 * 1.96)
  
  # ------------------------------------------------------------
  # 3. Popolazione regionale originale
  # ------------------------------------------------------------
  
  P_regionale <- sum(
    data[[col_pop30p]]
  )
  
  # ------------------------------------------------------------
  # 4. Matrice delle concentrazioni NO2
  # ------------------------------------------------------------
  
  exp_matrix <- as.matrix(
    data[, col_years, drop = FALSE]
  )
  
  num_years <- length(col_years)
  num_rows  <- nrow(data)
  
  # ------------------------------------------------------------
  # 5. Target OMS
  # ------------------------------------------------------------
  
  target_val <- if (
    is.character(col_end) &&
    length(col_end) == 1 &&
    col_end %in% names(data)
  ) {
    data[[col_end]]
  } else {
    as.numeric(col_end)
  }
  
  # ------------------------------------------------------------
  # 6. Vettori per la distribuzione bootstrap
  # ------------------------------------------------------------
  
  boot_AC  <- numeric(B)
  boot_PAF <- numeric(B)
  boot_RR  <- numeric(B)
  
  # ------------------------------------------------------------
  # 7. Progress bar
  # ------------------------------------------------------------
  
  pb <- txtProgressBar(
    min = 0,
    max = B,
    style = 3
  )
  
  # ------------------------------------------------------------
  # 8. Bootstrap
  # ------------------------------------------------------------
  
  for (b in seq_len(B)) {
    
    # ----------------------------------------------------------
    # 8.1 Ricampionamento indipendente delle sezioni
    # ----------------------------------------------------------
    
    boot_row_idx <- sample.int(
      num_rows,
      size = num_rows,
      replace = TRUE
    )
    
    # ----------------------------------------------------------
    # 8.2 Ricampionamento temporale
    # ----------------------------------------------------------
    
    sampled_year_idx <- sample.int(
      num_years,
      size = num_years,
      replace = TRUE
    )
    
    # ----------------------------------------------------------
    # 8.3 Estrazione dei dati bootstrap
    # ----------------------------------------------------------
    
    boot_pop <- data[[col_pop30p]][boot_row_idx]
    
    boot_tasso <- data[[col_tasso]][boot_row_idx]
    
    boot_exp_mat <- exp_matrix[
      boot_row_idx,
      sampled_year_idx,
      drop = FALSE
    ]
    
    # ----------------------------------------------------------
    # 8.4 Calibrazione della popolazione regionale
    # ----------------------------------------------------------
    
    P_boot <- sum(
      boot_pop
    )
    
    weight_regionale <- (
      P_regionale /
        P_boot
    )
    
    boot_pop_cal <- (
      boot_pop *
        weight_regionale
    )
    
    # ----------------------------------------------------------
    # 8.5 Numero atteso di casi
    # ----------------------------------------------------------
    
    boot_attesi_cal <- (
      boot_pop_cal *
        boot_tasso /
        100
    )
    
    # ----------------------------------------------------------
    # 8.6 Media temporale NO2
    # ----------------------------------------------------------
    
    no2_mean_boot <- rowMeans(
      boot_exp_mat
    )
    
    # ----------------------------------------------------------
    # 8.7 Delta rispetto al target OMS
    # ----------------------------------------------------------
    
    boot_target <- if (length(target_val) > 1) {
      
      target_val[boot_row_idx]
      
    } else {
      
      target_val
    }
    
    delta_boot <- pmax(
      no2_mean_boot - boot_target,
      0
    )
    
    # ----------------------------------------------------------
    # 8.8 Campionamento del RR
    # ----------------------------------------------------------
    
    drawn_log_RR <- rnorm(
      1,
      mean = mean_log_RR,
      sd = sd_log_RR
    )
    
    drawn_RR <- exp(
      drawn_log_RR
    )
    
    # ----------------------------------------------------------
    # 8.9 Attributable fraction
    # ----------------------------------------------------------
    
    AF_i <- 1 -
      exp(
        -(drawn_log_RR / 10) *
          delta_boot
      )
    
    # ----------------------------------------------------------
    # 8.10 Attributable cases
    # ----------------------------------------------------------
    
    AC_i <- (
      boot_attesi_cal *
        AF_i
    )
    
    # ----------------------------------------------------------
    # 8.11 Totale regionale
    # ----------------------------------------------------------
    
    tot_AC <- sum(
      AC_i
    )
    
    tot_attesi <- sum(
      boot_attesi_cal
    )
    
    # ----------------------------------------------------------
    # 8.12 Salvataggio della replica
    # ----------------------------------------------------------
    
    boot_AC[b] <- tot_AC
    
    boot_PAF[b] <- (
      tot_AC /
        tot_attesi
    )
    
    boot_RR[b] <- drawn_RR
    
    setTxtProgressBar(pb, b)
  }
  
  close(pb)
  
  # ------------------------------------------------------------
  # 9. Output: distribuzione bootstrap
  # ------------------------------------------------------------
  
  data.frame(
    replicate = seq_len(B),
    AC_boot = boot_AC,
    PAF_boot = boot_PAF,
    RR_boot = boot_RR
  )
}

#metrics of bootstrap

summarize_boot_metrics <- function(boot_res, ci_level = 0.95) {
  alpha <- (1 - ci_level) / 2
  
  boot_res %>%
    pivot_longer(
      cols = c(AC_boot, PAF_boot, RR_boot),
      names_to = "col",
      values_to = "val"
    ) %>%
    mutate(col = factor(col, levels = c("AC_boot", "PAF_boot", "RR_boot"))) %>%
    group_by(col) %>%
    summarise(
      media   = mean(val, na.rm = TRUE),
      mediana = median(val, na.rm = TRUE),
      sd      = sd(val, na.rm = TRUE),
      ci_025  = quantile(val, probs = alpha, na.rm = TRUE),
      ci_975  = quantile(val, probs = 1 - alpha, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      metrica = case_when(
        col == "AC_boot"  ~ "Casi Attribuibili (AC)",
        col == "PAF_boot" ~ "Frazione Attribuibili (PAF)",
        col == "RR_boot"  ~ "Rischio Relativo (RR)"
      ),
      stima_ic95 = case_when(
        col == "AC_boot"  ~ sprintf("%.0f (%.0f - %.0f)", mediana, ci_025, ci_975),
        col == "PAF_boot" ~ sprintf("%.2f%% (%.2f%% - %.2f%%)", mediana * 100, ci_025 * 100, ci_975 * 100),
        col == "RR_boot"  ~ sprintf("%.3f (%.3f - %.3f)", mediana, ci_025, ci_975)
      )
    ) %>%
    select(metrica, media, mediana, sd, ci_025, ci_975, stima_ic95)
}

