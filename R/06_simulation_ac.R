# -----------------------------------------------------------------------------
# Funzioni comuni
# -----------------------------------------------------------------------------

.prepare_ac_inputs <- function(
    data,
    exposure_cols,
    col_pop30p,
    col_tasso,
    col_end,
    rate_divisor,
    rr_increment
) {
  required_cols <- c(
    exposure_cols,
    col_pop30p,
    col_tasso,
    "rr",
    "rr_lic",
    "rr_uic"
  )

  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop(
      "Colonne mancanti: ",
      paste(missing_cols, collapse = ", "),
      "."
    )
  }

  if (!is.numeric(rate_divisor) || length(rate_divisor) != 1L ||
      !is.finite(rate_divisor) || rate_divisor <= 0) {
    stop("rate_divisor deve essere un numero finito maggiore di zero.")
  }

  if (!is.numeric(rr_increment) || length(rr_increment) != 1L ||
      !is.finite(rr_increment) || rr_increment <= 0) {
    stop("rr_increment deve essere un numero finito maggiore di zero.")
  }

  numeric_cols <- c(exposure_cols, col_pop30p, col_tasso)
  non_numeric <- numeric_cols[!vapply(data[numeric_cols], is.numeric, logical(1))]
  if (length(non_numeric) > 0) {
    stop(
      "Le seguenti colonne devono essere numeriche: ",
      paste(non_numeric, collapse = ", "),
      "."
    )
  }

  if (anyNA(data[numeric_cols])) {
    stop(
      "Sono presenti NA nelle colonne di esposizione, popolazione o tasso. ",
      "Gestire i valori mancanti prima della simulazione."
    )
  }

  finite_values <- vapply(
    data[numeric_cols],
    function(x) all(is.finite(x)),
    logical(1)
  )
  if (!all(finite_values)) {
    stop("Sono presenti valori non finiti negli input numerici.")
  }

  rr <- data$rr[1]
  rr_lic <- data$rr_lic[1]
  rr_uic <- data$rr_uic[1]

  if (anyNA(c(rr, rr_lic, rr_uic)) ||
      any(!is.finite(c(rr, rr_lic, rr_uic))) ||
      rr <= 0 || rr_lic <= 0 || rr_uic <= rr_lic) {
    stop("I parametri rr, rr_lic e rr_uic non sono validi.")
  }

  mean_log_RR <- log(rr)
  sd_log_RR <- (log(rr_uic) - log(rr_lic)) / (2 * 1.96)

  if (is.character(col_end) && length(col_end) == 1L) {
    if (!col_end %in% names(data)) {
      stop("La colonna target '", col_end, "' non è presente nei dati.")
    }
    target <- data[[col_end]]
  } else {
    target <- as.numeric(col_end)
  }

  if (!length(target) %in% c(1L, nrow(data))) {
    stop("Il target deve essere uno scalare o un vettore lungo nrow(data).")
  }
  if (anyNA(target) || any(!is.finite(target))) {
    stop("Il target contiene valori mancanti o non finiti.")
  }
  if (length(target) == 1L) {
    target <- rep(target, nrow(data))
  }

  expected_cases <-
    data[[col_pop30p]] * data[[col_tasso]] / rate_divisor

  if (any(expected_cases < 0)) {
    stop("Popolazione e tasso devono produrre casi attesi non negativi.")
  }

  list(
    target = target,
    expected_cases = expected_cases,
    total_expected = sum(expected_cases),
    mean_log_RR = mean_log_RR,
    sd_log_RR = sd_log_RR,
    rr_increment = rr_increment
  )
}


.start_progress_bar <- function(B, show_progress) {
  if (isTRUE(show_progress)) {
    utils::txtProgressBar(min = 0, max = B, style = 3)
  } else {
    NULL
  }
}


# -----------------------------------------------------------------------------
# 1. Simulazione 2025
#    - esposizione 2025, popolazione e tassi mantenuti fissi;
#    - propagazione della sola incertezza epidemiologica del RR.
# -----------------------------------------------------------------------------

run_simulation_ac_2025 <- function(
    data,
    col_exposure = "no2_2025",
    col_pop30p = "p30p",
    col_tasso = "tasso",
    col_end = "target_oms_no2",
    rate_divisor = 100,
    rr_increment = 10,
    B = 1000,
    seed = 1234,
    show_progress = interactive()
) {
  if (!is.null(seed)) {
    set.seed(seed)
  }

  if (length(B) != 1L || !is.finite(B) || B < 1 || B != as.integer(B)) {
    stop("B deve essere un intero positivo.")
  }
  B <- as.integer(B)

  inputs <- .prepare_ac_inputs(
    data = data,
    exposure_cols = col_exposure,
    col_pop30p = col_pop30p,
    col_tasso = col_tasso,
    col_end = col_end,
    rate_divisor = rate_divisor,
    rr_increment = rr_increment
  )

  delta_2025 <- pmax(
    data[[col_exposure]] - inputs$target,
    0
  )

  drawn_log_RR <- stats::rnorm(
    B,
    mean = inputs$mean_log_RR,
    sd = inputs$sd_log_RR
  )
  drawn_RR <- exp(drawn_log_RR)

  sim_AC <- numeric(B)
  sim_PAF <- numeric(B)

  pb <- .start_progress_bar(B, show_progress)
  if (!is.null(pb)) {
    on.exit(close(pb), add = TRUE)
  }

  for (b in seq_len(B)) {
    beta_b <- drawn_log_RR[b] / inputs$rr_increment

    # -expm1(-x) equivale a 1 - exp(-x), con maggiore stabilita numerica.
    AF_i <- -expm1(-beta_b * delta_2025)
    AC_i <- inputs$expected_cases * AF_i

    sim_AC[b] <- sum(AC_i)
    sim_PAF[b] <- sim_AC[b] / inputs$total_expected

    if (!is.null(pb)) {
      utils::setTxtProgressBar(pb, b)
    }
  }

  data.frame(
    replicate = seq_len(B),
    simulation = "2025",
    exposure_year = 2025L,
    AC_sim = sim_AC,
    PAF_sim = sim_PAF,
    RR_sim = drawn_RR
  )
}


# -----------------------------------------------------------------------------
# 2. Simulazione storica 2019-2025
#    - a ogni replica viene estratto UN SOLO anno;
#    - lo stesso scenario annuale è applicato a tutte le sezioni;
#    - popolazione e tassi restano fissi, per rendere confrontabili gli anni;
#    - viene propagata anche l'incertezza epidemiologica del RR.
# -----------------------------------------------------------------------------

run_simulation_ac_historical <- function(
    data,
    col_years = paste0("no2_", 2019:2025),
    col_pop30p = "p30p",
    col_tasso = "tasso",
    col_end = "target_oms_no2",
    rate_divisor = 100,
    rr_increment = 10,
    B = 1000,
    seed = 1234,
    show_progress = interactive()
) {
  if (!is.null(seed)) {
    set.seed(seed)
  }

  if (length(B) != 1L || !is.finite(B) || B < 1 || B != as.integer(B)) {
    stop("B deve essere un intero positivo.")
  }
  B <- as.integer(B)

  inputs <- .prepare_ac_inputs(
    data = data,
    exposure_cols = col_years,
    col_pop30p = col_pop30p,
    col_tasso = col_tasso,
    col_end = col_end,
    rate_divisor = rate_divisor,
    rr_increment = rr_increment
  )

  exposure_matrix <- as.matrix(data[, col_years, drop = FALSE])

  # Il target viene sottratto riga per riga a tutti gli scenari annuali.
  delta_matrix <- sweep(
    exposure_matrix,
    MARGIN = 1,
    STATS = inputs$target,
    FUN = "-"
  )
  delta_matrix <- pmax(delta_matrix, 0)

  num_years <- length(col_years)

  # Una sola colonna/anno per replica: si conserva l'intero campo spaziale.
  sampled_year_idx <- sample.int(
    num_years,
    size = B,
    replace = TRUE
  )

  drawn_log_RR <- stats::rnorm(
    B,
    mean = inputs$mean_log_RR,
    sd = inputs$sd_log_RR
  )
  drawn_RR <- exp(drawn_log_RR)

  sim_AC <- numeric(B)
  sim_PAF <- numeric(B)

  pb <- .start_progress_bar(B, show_progress)
  if (!is.null(pb)) {
    on.exit(close(pb), add = TRUE)
  }

  for (b in seq_len(B)) {
    delta_b <- delta_matrix[, sampled_year_idx[b]]
    beta_b <- drawn_log_RR[b] / inputs$rr_increment

    AF_i <- -expm1(-beta_b * delta_b)
    AC_i <- inputs$expected_cases * AF_i

    sim_AC[b] <- sum(AC_i)
    sim_PAF[b] <- sim_AC[b] / inputs$total_expected

    if (!is.null(pb)) {
      utils::setTxtProgressBar(pb, b)
    }
  }

  year_labels <- suppressWarnings(
    as.integer(sub(".*([0-9]{4})$", "\\1", col_years))
  )
  if (anyNA(year_labels)) {
    year_labels <- col_years
  }

  data.frame(
    replicate = seq_len(B),
    simulation = "historical",
    exposure_year = year_labels[sampled_year_idx],
    AC_sim = sim_AC,
    PAF_sim = sim_PAF,
    RR_sim = drawn_RR
  )
}


# -----------------------------------------------------------------------------
# Riepilogo comune
# I percentili sono indicati come intervallo di simulazione, non come IC.
# -----------------------------------------------------------------------------

summarize_simulation_metrics <- function(sim_res, interval_level = 0.95) {
  required_cols <- c("AC_sim", "PAF_sim", "RR_sim")
  missing_cols <- setdiff(required_cols, names(sim_res))

  if (length(missing_cols) > 0) {
    stop(
      "Colonne mancanti nei risultati: ",
      paste(missing_cols, collapse = ", "),
      "."
    )
  }

  if (length(interval_level) != 1L || !is.finite(interval_level) ||
      interval_level <= 0 || interval_level >= 1) {
    stop("interval_level deve essere compreso tra 0 e 1.")
  }

  alpha <- (1 - interval_level) / 2

  sim_res |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(required_cols),
      names_to = "col",
      values_to = "val"
    ) |>
    dplyr::mutate(
      col = factor(col, levels = c("AC_sim", "PAF_sim", "RR_sim"))
    ) |>
    dplyr::group_by(col) |>
    dplyr::summarise(
      media = mean(val, na.rm = TRUE),
      mediana = stats::median(val, na.rm = TRUE),
      sd = stats::sd(val, na.rm = TRUE),
      int_inf = stats::quantile(val, probs = alpha, na.rm = TRUE),
      int_sup = stats::quantile(val, probs = 1 - alpha, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      metrica = dplyr::case_when(
        col == "AC_sim" ~ "Casi attribuibili (AC)",
        col == "PAF_sim" ~ "Frazione attribuibile (PAF)",
        col == "RR_sim" ~ "Rischio relativo (RR)"
      ),
      stima_intervallo = dplyr::case_when(
        col == "AC_sim" ~ sprintf(
          "%.0f (%.0f - %.0f)", mediana, int_inf, int_sup
        ),
        col == "PAF_sim" ~ sprintf(
          "%.2f%% (%.2f%% - %.2f%%)",
          mediana * 100,
          int_inf * 100,
          int_sup * 100
        ),
        col == "RR_sim" ~ sprintf(
          "%.3f (%.3f - %.3f)", mediana, int_inf, int_sup
        )
      )
    ) |>
    dplyr::select(
      metrica,
      media,
      mediana,
      sd,
      int_inf,
      int_sup,
      stima_intervallo
    )
}


# -----------------------------------------------------------------------------
# Grafico della distribuzione simulata
# Supporta sia l'output 2025 sia quello storico.
# -----------------------------------------------------------------------------

plot_simulation_density <- function(
    sim_res,
    var_name = "AC_sim",
    simulation_type = NULL,
    causa_label = "Mortalità per cause respiratorie",
    stima_centrale = NULL,
    central_label = NULL,
    interval_level = 0.95,
    output_path = NULL
) {
  if (!var_name %in% names(sim_res)) {
    stop(
      "La colonna '", var_name,
      "' non esiste. Colonne disponibili: ",
      paste(names(sim_res), collapse = ", "),
      "."
    )
  }

  if (length(interval_level) != 1L || !is.finite(interval_level) ||
      interval_level <= 0 || interval_level >= 1) {
    stop("interval_level deve essere compreso tra 0 e 1.")
  }

  df_plot <- sim_res

  # Se sono presenti entrambe le simulazioni, simulation_type permette
  # di selezionare quella da rappresentare.
  if (!is.null(simulation_type)) {
    if (!"simulation" %in% names(df_plot)) {
      stop(
        "simulation_type e stato specificato, ma manca la colonna 'simulation'."
      )
    }

    df_plot <- df_plot[df_plot$simulation == simulation_type, , drop = FALSE]

    if (nrow(df_plot) == 0L) {
      stop("Nessuna replica trovata per simulation_type = '", simulation_type, "'.")
    }
  } else if ("simulation" %in% names(df_plot)) {
    available_types <- unique(stats::na.omit(df_plot$simulation))

    if (length(available_types) > 1L) {
      stop(
        "Sono presenti piu tipi di simulazione: ",
        paste(available_types, collapse = ", "),
        ". Specificare simulation_type."
      )
    }

    if (length(available_types) == 1L) {
      simulation_type <- available_types
    }
  }

  vals <- df_plot[[var_name]]

  if (!is.numeric(vals)) {
    stop("La variabile da rappresentare deve essere numerica.")
  }

  vals <- vals[is.finite(vals)]

  if (length(vals) < 2L || length(unique(vals)) < 2L) {
    stop("Servono almeno due valori finiti e distinti per stimare la densita.")
  }

  is_paf <- identical(var_name, "PAF_sim") ||
    grepl("paf|fraction", var_name, ignore.case = TRUE)

  is_rr <- identical(var_name, "RR_sim") ||
    grepl("(^|_)rr($|_)", var_name, ignore.case = TRUE)

  if (is.null(stima_centrale)) {
    stima_centrale <- stats::median(vals)

    if (is.null(central_label)) {
      central_label <- "Mediana simulata"
    }
  } else {
    if (length(stima_centrale) != 1L || !is.finite(stima_centrale)) {
      stop("stima_centrale deve essere un singolo valore numerico finito.")
    }

    if (is.null(central_label)) {
      central_label <- "Stima di riferimento"
    }
  }

  alpha <- (1 - interval_level) / 2
  interval <- stats::quantile(
    vals,
    probs = c(alpha, 1 - alpha),
    na.rm = TRUE,
    names = FALSE
  )

  dens <- stats::density(vals, na.rm = TRUE)

  df_dens <- data.frame(
    x = dens$x,
    y = dens$y
  )
  df_dens$interval_zone <-
    df_dens$x >= interval[1] & df_dens$x <= interval[2]

  max_y <- max(df_dens$y)
  interval_pct <- round(interval_level * 100)

  fmt <- function(val) {
    if (is_paf) {
      if (max(abs(vals), na.rm = TRUE) > 1) {
        scales::number(
          val,
          accuracy = 0.01,
          suffix = "%",
          big.mark = " ",
          decimal.mark = "."
        )
      } else {
        scales::percent(
          val,
          accuracy = 0.01,
          big.mark = " ",
          decimal.mark = "."
        )
      }
    } else if (is_rr) {
      scales::number(
        val,
        accuracy = 0.001,
        decimal.mark = "."
      )
    } else {
      scales::number(
        val,
        accuracy = 0.1,
        big.mark = " ",
        decimal.mark = "."
      )
    }
  }

  if (is_paf) {
    x_lab <- "Frazione attribuibile di popolazione (PAF)"
    metric_title <- "PAF"
  } else if (is_rr) {
    x_lab <- "Rischio relativo per 10 microgrammi/m3 (RR)"
    metric_title <- "RR"
  } else {
    x_lab <- "Casi attribuibili potenzialmente evitabili (AC)"
    metric_title <- "AC"
  }

  simulation_key <- tolower(as.character(simulation_type %||% ""))

  if (simulation_key == "2025") {
    subtitle_text <- paste0(
      "Simulazione 2025: incertezza epidemiologica RR"
    )
    caption_text <- paste0(
      "Area ombreggiata: intervallo di simulazione al ",
      interval_pct,
      "% (metodo dei percentili). Esposizione 2025 mantenuta fissa."
    )
  } else if (simulation_key %in% c("historical", "storica", "storico")) {
    subtitle_text <- paste0(
      "Simulazione storica 2019-2025: variabilita dell'esposizione e ",
      "incertezza RR"
    )
    caption_text <- paste0(
      "Area ombreggiata: intervallo di simulazione al ",
      interval_pct,
      "% (metodo dei percentili). Unico scenario annuale e estratto ",
      "per ciascuna replica."
    )
  } else {
    subtitle_text <- "Distribuzione della simulazione probabilistica"
    caption_text <- paste0(
      "Area ombreggiata: intervallo di simulazione al ",
      interval_pct,
      "% (metodo dei percentili)."
    )
  }

  x_labels <- if (is_paf) {
    if (max(abs(vals), na.rm = TRUE) > 1) {
      scales::number_format(
        accuracy = 0.1,
        suffix = "%",
        big.mark = " ",
        decimal.mark = "."
      )
    } else {
      scales::percent_format(
        accuracy = 0.1,
        big.mark = " ",
        decimal.mark = "."
      )
    }
  } else if (is_rr) {
    scales::number_format(accuracy = 0.001, decimal.mark = ".")
  } else {
    scales::number_format(big.mark = " ", decimal.mark = ".")
  }

  p <- ggplot2::ggplot() +
    ggplot2::geom_area(
      data = df_dens[df_dens$interval_zone, , drop = FALSE],
      ggplot2::aes(x = x, y = y),
      fill = "aquamarine3",
      alpha = 0.25
    ) +
    ggplot2::geom_line(
      data = df_dens,
      ggplot2::aes(x = x, y = y),
      color = "grey30",
      linewidth = 0.6
    ) +
    ggplot2::geom_vline(
      xintercept = stima_centrale,
      color = "firebrick3",
      linetype = "dashed",
      linewidth = 0.7
    ) +
    ggplot2::geom_vline(
      xintercept = interval,
      color = "aquamarine4",
      linetype = "dashed",
      linewidth = 0.6
    ) +
    ggplot2::annotate(
      "text",
      x = stima_centrale,
      y = max_y * 1.05,
      label = paste0(central_label, ": ", fmt(stima_centrale)),
      color = "firebrick3",
      size = 3.5,
      fontface = "bold",
      hjust = -0.05
    ) +
    ggplot2::annotate(
      "text",
      x = interval[1],
      y = max_y * 0.5,
      label = paste0("Intervallo ", interval_pct, "% inf.: ", fmt(interval[1])),
      color = "aquamarine4",
      size = 3.3,
      hjust = 1.1
    ) +
    ggplot2::annotate(
      "text",
      x = interval[2],
      y = max_y * 0.5,
      label = paste0("Intervallo ", interval_pct, "% sup.: ", fmt(interval[2])),
      color = "aquamarine4",
      size = 3.3,
      hjust = -0.1
    ) +
    ggplot2::scale_x_continuous(
      labels = x_labels,
      expand = ggplot2::expansion(mult = c(0.22, 0.15))
    ) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::labs(
      title = bquote("Incertezza" ~ .(metric_title) ~ NO[2] ~ "-" ~ .(causa_label)),
      subtitle = subtitle_text,
      x = x_lab,
      y = "Densita",
      caption = caption_text
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        hjust = 0.5,
        face = "bold",
        size = 12,
        margin = ggplot2::margin(b = 6)
      ),
      plot.subtitle = ggplot2::element_text(
        hjust = 0.5,
        size = 10,
        margin = ggplot2::margin(b = 15)
      ),
      plot.caption = ggplot2::element_text(hjust = 0),
      plot.margin = ggplot2::margin(t = 15, r = 25, b = 10, l = 25)
    )

  if (!is.null(output_path)) {
    ggplot2::ggsave(
      filename = output_path,
      plot = p,
      width = 8,
      height = 5,
      dpi = 300,
      bg = "white"
    )
  }

  p
}


# Operatore interno usato per impostare una stringa di default.
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L || is.na(x[1])) y else x
}


# -----------------------------------------------------------------------------
# Esempio d'uso
# -----------------------------------------------------------------------------

# sim_2025 <- run_simulation_ac_2025(
#   data = df,
#   B = 10000,
#   seed = 1234
# )
#
# sim_historical <- run_simulation_ac_historical(
#   data = df,
#   B = 10000,
#   seed = 1234
# )
#
# summary_2025 <- summarize_simulation_metrics(sim_2025)
# summary_historical <- summarize_simulation_metrics(sim_historical)
#
# plot_2025 <- plot_simulation_density(
#   sim_res = sim_2025,
#   var_name = "AC_sim",
#   causa_label = "Mortalità per cause respiratorie"
# )
#
# plot_historical <- plot_simulation_density(
#   sim_res = sim_historical,
#   var_name = "AC_sim",
#   causa_label = "Mortalità per cause respiratorie"
# )
#
# Per controllare la distribuzione degli anni estratti:
# table(sim_historical$exposure_year)
