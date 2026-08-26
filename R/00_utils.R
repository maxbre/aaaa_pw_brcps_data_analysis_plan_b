# R/00_utils.R - Gestione etichette, dizionari e funzioni generiche

var_labels <- c(
  "pol_bin_ue"      = "no2_bin",
  "pop_dens_z_wz"   = "pop_dens",
  "prop_dis_z"      = "p_disoc",
  "prop_eedu9_z"    = "p_elistr",
  "prop_over65_z"   = "p_over65",
  "id_pro_fct_23"   = "pro_vr",
  "id_pro_fct_24"   = "pro_vi",
  "id_pro_fct_25"   = "pro_bl",
  "id_pro_fct_26"   = "pro_tv",
  "id_pro_fct_27"   = "pro_ve",
  "id_pro_fct_28"   = "pro_pd",
  "id_pro_fct_29"   = "pro_ro"
)

clean_label <- function(vars, dict = var_labels) {
  translated <- dict[vars]
  dplyr::coalesce(unname(translated), vars)
}

get_province_labels <- function(link_table_path, short = FALSE) {
  tbl_link_prov <- readr::read_csv(link_table_path, show_col_types = FALSE)
  if (short) {
    tbl_link_prov |> 
      dplyr::select(CODPRO, prov_abb) |> 
      dplyr::mutate(CODPRO = as.character(CODPRO), prov_abb = toupper(prov_abb)) |> 
      tibble::deframe()
  } else {
    tbl_link_prov |> 
      dplyr::select(CODPRO, PROVINCIA) |> 
      dplyr::mutate(CODPRO = as.character(CODPRO)) |> 
      tibble::deframe()
  }
}

fpath_province <- './data_input/tbl_link_prov.csv'
prov_labels <- get_province_labels(fpath_province, short = FALSE)

#-------------------------------------------------------------------------------

ggsave_report <- function(filename, plot = ggplot2::last_plot(), width = 15, ratio = 1.618, height = width / ratio, units = "cm", dpi = 300, ...) {
  ggplot2::ggsave(filename = filename, plot = plot, width = width, height = height, units = units, dpi = dpi, ...)
}

pollutant_label <- function(pollutant) {
  switch(
    pollutant,
    "NO2"   = expression(NO[2] ~ "[ " * mu * "g m"^-3 * " ]"),
    "PM2.5" = expression(PM[2.5] ~ "[ " * mu * "g m"^-3 * " ]"),
    "PM10"  = expression(PM[10] ~ "[ " * mu * "g m"^-3 * " ]")
  )
}
