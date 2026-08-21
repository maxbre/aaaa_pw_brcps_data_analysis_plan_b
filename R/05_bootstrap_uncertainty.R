# R/05_bootstrap_uncertainty.R - Bootstrap Spazio-Temporale e IID

run_bootstrap_ac_spatiotemporal_opt <- function(data, col_years = paste0("no2_", 2019:2025), col_pop30p = "p30p", col_tasso = "tasso", col_end = "target_oms_no2", cluster_var = "cod_comune", B = 1000, seed = 1234) {
  if (!is.null(seed)) set.seed(seed)
  stopifnot(cluster_var %in% names(data), all(col_years %in% names(data)))
  
  n_rows     <- nrow(data)
  num_years  <- length(col_years)
  mean_log_RR <- log(data$rr[1])
  sd_log_RR   <- (log(data$rr_uic[1]) - log(data$rr_lic[1])) / (2 * 1.96)
  
  attesi_vec <- data[[col_pop30p]] * (data[[col_tasso]] / 100)
  exp_matrix <- as.matrix(data[, col_years])
  target_val <- if (is.character(col_end) && col_end %in% names(data)) data[[col_end]] else as.numeric(col_end)
  
  cluster_indices <- split(seq_len(n_rows), data[[cluster_var]])
  cluster_names   <- names(cluster_indices)
  num_clusters    <- length(cluster_names)
  
  boot_casi_attr <- numeric(B)
  boot_paf       <- numeric(B)
  
  pb <- txtProgressBar(min = 0, max = B, style = 3)
  for (b in 1:B) {
    sampled_year_idx <- sample.int(num_years, size = num_years, replace = TRUE)
    sampled_clusters <- sample(cluster_names, size = num_clusters, replace = TRUE)
    boot_row_idx     <- unlist(cluster_indices[sampled_clusters], use.names = FALSE)
    
    boot_exp_mat <- exp_matrix[boot_row_idx, sampled_year_idx, drop = FALSE]
    boot_attesi  <- attesi_vec[boot_row_idx]
    boot_target  <- if (length(target_val) > 1) target_val[boot_row_idx] else target_val
    
    no2_mean_boot <- rowMeans(boot_exp_mat, na.rm = TRUE)
    delta_boot    <- no2_mean_boot - pmin(no2_mean_boot, boot_target)
    
    drawn_log_RR <- rnorm(1, mean = mean_log_RR, sd = sd_log_RR)
    AF_i <- 1 - exp(- (drawn_log_RR / 10) * delta_boot)
    AC_i <- boot_attesi * AF_i
    
    tot_attesi_b  <- sum(boot_attesi, na.rm = TRUE)
    tot_casi_ac_b <- sum(AC_i, na.rm = TRUE)
    
    boot_casi_attr[b] <- tot_casi_ac_b
    boot_paf[b]       <- tot_casi_ac_b / tot_attesi_b
    setTxtProgressBar(pb, b)
  }
  close(pb)
  
  data.frame(casi_attribuibili = boot_casi_attr, paf = boot_paf)
}

run_bootstrap_ac_simple <- function(data, col_exp = paste0("no2_", 2019:2025), col_pop30p = "p30p", col_tasso = "tasso", col_end = "target_oms_no2", B = 1000, seed = 1234) {
  if (!is.null(seed)) set.seed(seed)
  stopifnot(all(col_exp %in% names(data)))
  
  n_rows      <- nrow(data)
  mean_log_RR <- log(data$rr[1])
  sd_log_RR   <- (log(data$rr_uic[1]) - log(data$rr_lic[1])) / (2 * 1.96)
  
  no2_val <- if (length(col_exp) > 1) rowMeans(as.matrix(data[, col_exp]), na.rm = TRUE) else data[[col_exp]]
  target_val <- if (is.character(col_end) && col_end %in% names(data)) data[[col_end]] else as.numeric(col_end)
  
  attesi_vec <- data[[col_pop30p]] * (data[[col_tasso]] / 100)
  delta_vec  <- no2_val - pmin(no2_val, target_val)
  
  boot_casi_attr <- numeric(B)
  boot_paf       <- numeric(B)
  
  pb <- txtProgressBar(min = 0, max = B, style = 3)
  for (b in 1:B) {
    idx <- sample.int(n_rows, size = n_rows, replace = TRUE)
    sampled_attesi <- attesi_vec[idx]
    sampled_delta  <- delta_vec[idx]
    
    drawn_log_RR <- rnorm(1, mean = mean_log_RR, sd = sd_log_RR)
    AF_i <- 1 - exp(- (drawn_log_RR / 10) * sampled_delta)
    AC_i <- sampled_attesi * AF_i
    
    boot_casi_attr[b] <- sum(AC_i, na.rm = TRUE)
    boot_paf[b]       <- sum(AC_i, na.rm = TRUE) / sum(sampled_attesi, na.rm = TRUE)
    setTxtProgressBar(pb, b)
  }
  close(pb)
  
  data.frame(casi_attribuibili = boot_casi_attr, paf = boot_paf)
}

compare_bootstrap_metrics <- function(..., var_name = "casi_attribuibili", methods = NULL, ref_method = 1) {
  results_list <- list(...)
  if (length(results_list) == 1 && is.list(results_list[[1]]) && !is.data.frame(results_list[[1]])) {
    results_list <- results_list[[1]]
  }
  if (is.null(methods)) {
    methods <- if (!is.null(names(results_list)) && all(names(results_list) != "")) names(results_list) else paste("Metodo", seq_along(results_list))
  }
  
  numeric_list <- purrr::map2(results_list, methods, \(item, m_name) {
    if (is.data.frame(item)) item[[var_name]] else item
  })
  
  ref_idx <- if (is.character(ref_method)) match(ref_method, methods) else as.integer(ref_method)
  
  tibble::tibble(
    Metodo   = methods,
    Media    = purrr::map_dbl(numeric_list, \(x) mean(x, na.rm = TRUE)),
    Mediana  = purrr::map_dbl(numeric_list, \(x) median(x, na.rm = TRUE)),
    SD       = purrr::map_dbl(numeric_list, \(x) sd(x, na.rm = TRUE)),
    Q2.5     = purrr::map_dbl(numeric_list, \(x) quantile(x, 0.025, na.rm = TRUE)),
    Q97.5    = purrr::map_dbl(numeric_list, \(x) quantile(x, 0.975, na.rm = TRUE))
  ) |> 
    dplyr::mutate(
      Ampiezza_IC95 = Q97.5 - Q2.5,
      Ratio_SD      = SD / SD[ref_idx],
      Ratio_IC95    = Ampiezza_IC95 / Ampiezza_IC95[ref_idx]
    )
}
