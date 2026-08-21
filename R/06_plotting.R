# R/06_plotting.R - Generazione mappe sf, terra e grafici ggplot2

plot_sezione_map <- function(sez_analyzed, fill_var, palette = "magma", title = "", legend_title = "", use_log = FALSE) {
  if (!fill_var %in% names(sez_analyzed)) stop(paste0("Colonna '", fill_var, "' assente."))
  
  scale_trans  <- if (use_log) scales::transform_pseudo_log() else "identity"
  scale_breaks <- if (use_log) {
    function(limits) { limits[1] <- max(1, limits[1], na.rm = TRUE); scales::breaks_log(n = 5)(limits) }
  } else scales::breaks_extended(n = 5)
  
  ggplot2::ggplot(sez_analyzed) +
    ggplot2::geom_sf(ggplot2::aes(fill = .data[[fill_var]]), color = NA) +
    ggplot2::scale_fill_viridis_c(
      option = palette, transform = scale_trans, breaks = scale_breaks,
      name = legend_title, na.value = "transparent",
      labels = scales::label_number(big.mark = " ", accuracy = 1),
      guide = ggplot2::guide_colorbar(barheight = ggplot2::unit(5.5, "cm"), barwidth = ggplot2::unit(0.4, "cm"))
    ) +
    ggplot2::theme_void() +
    ggplot2::labs(title = title) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 13))
}

plot_and_save_layers <- function(raster_stack, output_dir = NULL, title_prefix = "", custom_titles = NULL, color_palette = "viridis", fill_name = expression("["*mu*g~m^-3*"]")) {
  range_vals <- terra::minmax(raster_stack)
  global_min <- min(range_vals[1, ], na.rm = TRUE)
  global_max <- max(range_vals[2, ], na.rm = TRUE)
  
  if (!is.null(output_dir) && !dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  plot_list <- list()
  for (i in seq_len(terra::nlyr(raster_stack))) {
    single_layer <- raster_stack[[i]]
    plot_title   <- if (!is.null(custom_titles) && length(custom_titles) >= i) custom_titles[i] else paste0(title_prefix, names(raster_stack)[i])
    
    p <- ggplot2::ggplot() +
      tidyterra::geom_spatraster(data = single_layer) +
      ggplot2::scale_fill_viridis_c(name = fill_name, option = color_palette, limits = c(global_min, global_max), na.value = "transparent") +
      #ggspatial::annotation_scale(location = "bl", style = "ticks") +
      #ggspatial::annotation_north_arrow(location = "tl", style = ggspatial::north_arrow_minimal()) +
      ggplot2::labs(title = plot_title) +
      ggplot2::theme_void()+
      ggplot2::theme(plot.title = element_text(hjust = 0.5))
    
  if (!is.null(output_dir)) {
    ggplot2::ggsave(file.path(output_dir, paste0(names(raster_stack)[i], ".png")), p, bg = "white")
  } else {
    print(p)  # Forces plot output directly to the RStudio Plots pane
  }
  
  plot_list[[names(raster_stack)[i]]] <- p
}
invisible(plot_list)
}

plot_annual_boxplots_df <- function(df, value_var = "pol_value", year_var = "year", fill_color = "#2b83ba", y_title = expression(paste("NO"[2], " [", mu*g~m^-3, "]")), title = "Distribuzione annuale", output_path = NULL) {
  p <- ggplot2::ggplot(df, ggplot2::aes(x = factor(.data[[year_var]]), y = .data[[value_var]])) +
    ggplot2::geom_boxplot(fill = fill_color, color = "gray20", alpha = 0.7, outlier.size = 0.5) +
    ggplot2::labs(title = title, x = "Anno", y = y_title) +
    ggplot2::theme_minimal()
  
  if (!is.null(output_path)) ggplot2::ggsave(output_path, p, bg = "white")
  return(p)
}

plot_mk_sen_map <- function(shp, df_mk, join_by = "COMUNE", shp_region = NULL, title = "Mappa Trend", subtitle = "") {
  shp_mk <- dplyr::left_join(shp, df_mk, by = join_by)
  p <- ggplot2::ggplot(shp_mk) +
    ggplot2::geom_sf(ggplot2::aes(fill = sen_slope), color = "white", linewidth = 0.08) +
    ggplot2::geom_sf(data = dplyr::filter(shp_mk, trend_sig == TRUE), fill = NA, color = "gray30", linewidth = 0.25)
  
  if (!is.null(shp_region)) p <- p + ggplot2::geom_sf(data = shp_region, fill = NA, color = "black", linewidth = 0.5)
  
  p + ggplot2::scale_fill_gradient2(low = "#2b83ba", mid = "#e0f3f8", high = "#ffffbf", midpoint = -0.5, name = "Sen's slope") +
    ggplot2::labs(title = title, subtitle = subtitle) +
    ggplot2::theme_void()+
    theme(legend.position = "bottom")
}

plot_bootstrap_comparison <- function(..., var_name = "casi_attribuibili", methods = NULL, title = "Confronto Bootstrap", output_path = NULL) {
  results_list <- list(...)
  if (length(results_list) == 1 && is.list(results_list[[1]]) && !is.data.frame(results_list[[1]])) results_list <- results_list[[1]]
  if (is.null(methods)) methods <- names(results_list)
  
  df_plot <- purrr::map2(results_list, methods, \(item, m_name) {
    vals <- if (is.data.frame(item)) item[[var_name]] else item
    tibble::tibble(valore = vals, Metodo = m_name)
  }) |> dplyr::bind_rows()
  
  p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = valore, fill = Metodo, color = Metodo)) +
    ggplot2::geom_density(alpha = 0.30) +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = title, x = var_name, y = "Densità")
  
  if (!is.null(output_path)) ggplot2::ggsave(output_path, p, bg = "white")
  return(p)
}

#' Plot della distribuzione di densità bootstrap (AC e PAF)
plot_bootstrap_density <- function(df_boot, 
                                   var_name = "casi_attribuibili", 
                                   causa_label = "Mortalità per causa...)",
                                   stima_centrale = NULL,
                                   output_path = NULL) {
  
  # 1. Controllo esistenza colonna
  if (!var_name %in% names(df_boot)) {
    stop(paste0("La colonna '", var_name, "' non esiste nel dataframe. Colonne disponibili: ", 
                paste(names(df_boot), collapse = ", ")))
  }
  
  # 2. Estrazione del vettore numerico
  vals <- df_boot[[var_name]]
  
  # Riconoscimento PAF (sia 'paf' che 'paf_percentuale')
  is_paf <- grepl("paf", var_name, ignore.case = TRUE)
  
  # 3. Stima centrale (se non specificata, usa la mediana bootstrap)
  if (is.null(stima_centrale)) {
    stima_centrale <- median(vals, na.rm = TRUE)
  }
  
  # 4. Calcolo quantili IC 95% e densità
  ci <- quantile(vals, probs = c(0.025, 0.975), na.rm = TRUE)
  dens <- density(vals, na.rm = TRUE)
  
  df_dens <- data.frame(x = dens$x, y = dens$y) |> 
    mutate(ci_zone = if_else(x >= ci[1] & x <= ci[2], "Inside", "Outside"))
  
  max_y <- max(df_dens$y)
  
  # 5. Formattatore dinamico con gestione automatica frazione (0-1) vs percentuale (0-100)
  fmt <- function(val) {
    if (is_paf) {
      if (max(vals, na.rm = TRUE) > 1) {
        scales::number(val, accuracy = 0.01, suffix = "%", big.mark = " ", decimal.mark = ".")
      } else {
        scales::percent(val, accuracy = 0.01, big.mark = " ", decimal.mark = ".")
      }
    } else {
      scales::number(val, accuracy = 1, big.mark = " ", decimal.mark = ".")
    }
  }
  
  x_lab <- if (is_paf) "Frazione Attribuibile Popolazione (PAF)" else "Casi Evitabili (AC)"
  metric_title <- if (is_paf) "PAF" else "AC"
  
  # 6. Costruzione del grafico
  p <- ggplot() +
    # Area IC 95%
    geom_area(data = subset(df_dens, ci_zone == "Inside"), aes(x = x, y = y), fill = "aquamarine3", alpha = 0.25) +
    geom_line(data = df_dens, aes(x = x, y = y), color = "grey30", linewidth = 0.6) +
    
    # Linee verticali
    geom_vline(xintercept = stima_centrale, color = "firebrick3", linetype = "dashed", linewidth = 0.7) +
    geom_vline(xintercept = ci, color = "aquamarine4", linetype = "dashed", linewidth = 0.6) +
    
    # Annotazioni di testo
    annotate("text", 
             x = stima_centrale, y = max_y * 1.05, 
             label = paste0("Stima centrale: ", fmt(stima_centrale)), 
             color = "firebrick3", size = 3.5, fontface = "bold", hjust = -0.05) +
    
    annotate("text", 
             x = ci[1], y = max_y * 0.5, 
             label = paste0("IC 95% inf.: ", fmt(ci[1])), 
             color = "aquamarine4", size = 3.3, hjust = 1.1) +
    
    annotate("text", 
             x = ci[2], y = max_y * 0.5, 
             label = paste0("IC 95% sup.: ", fmt(ci[2])), 
             color = "aquamarine4", size = 3.3, hjust = -0.1) +
    
    # Asse X scalato correttamente
    scale_x_continuous(
      labels = if (is_paf) {
        if (max(vals, na.rm = TRUE) > 1) {
          scales::number_format(accuracy = 0.1, suffix = "%", big.mark = " ", decimal.mark = ".")
        } else {
          scales::percent_format(accuracy = 0.1, big.mark = " ", decimal.mark = ".")
        }
      } else {
        scales::number_format(big.mark = " ", decimal.mark = ".")
      },
      expand = expansion(mult = c(0.22, 0.15))
    ) +
    coord_cartesian(clip = "off") +
    
    labs(
      title = bquote("Incertezza" ~ .(metric_title) ~ NO[2] ~ "-" ~ .(causa_label)),
      subtitle = "Distribuzione impatto sanitario evitato (Bootstrap Spazio-Temporale)",
      x = x_lab, 
      y = "Densità",
      caption = "Area ombreggiata: IC 95% (Metodo Percentili). Propagazione incertezza."
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 12, margin = margin(b = 6)),
      plot.subtitle = element_text(hjust = 0.5, size = 10, margin = margin(b = 15)),
      plot.margin = margin(t = 15, r = 25, b = 10, l = 25)
    )
  
  if (!is.null(output_path)) {
    ggsave(output_path, plot = p, width = 8, height = 5, dpi = 300, bg = "white")
  }
  
  return(p)
}

