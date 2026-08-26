# R/01_demographics.R - Elaborazione e sintesi demografica censuaria

# process_census_demographics <- function(shp_sez, id_cols = c("SEZ21_ID", "PROCOM")) {
#   new_vars <- c("area_km2", "area_ha", "pop_tot", "pop_30p", "pop_65p", 
#                 "dens_km2", "dens_ha", "pct_30p_tot", "pct_65p_tot", "pct_65p_30p")
#   
#   shp_sez |> 
#     dplyr::mutate(
#       area_m2     = sf::st_area(geom),
#       area_km2    = units::drop_units(units::set_units(area_m2, km^2)),
#       area_ha     = units::drop_units(units::set_units(area_m2, ha)),
#       pop_tot     = P1,
#       pop_30p     = rowSums(dplyr::across(P20:P29), na.rm = TRUE),
#       pop_65p     = rowSums(dplyr::across(dplyr::c_across(c("P27", "P28", "P29"))), na.rm = TRUE),
#       dens_km2    = dplyr::if_else(area_km2 > 0, pop_tot / area_km2, 0),
#       dens_ha     = dplyr::if_else(area_ha > 0, pop_tot / area_ha, 0), 
#       pct_30p_tot = dplyr::if_else(pop_tot > 0, (pop_30p / pop_tot) * 100, NA_real_),
#       pct_65p_tot = dplyr::if_else(pop_tot > 0, (pop_65p / pop_tot) * 100, NA_real_),
#       pct_65p_30p = dplyr::if_else(pop_30p > 0, (pop_65p / pop_30p) * 100, NA_real_)
#     ) |> 
#     dplyr::select(dplyr::all_of(id_cols), dplyr::all_of(new_vars))
# }

# R/01_demographics.R - Elaborazione e sintesi demografica censuaria

process_census_demographics <- function(shp_sez, id_cols = c("SEZ21_ID", "PROCOM")) {
  new_vars <- c("area_km2", "area_ha", "pop_tot", "pop_30p", "pop_65p", 
                "dens_km2", "dens_ha", "pct_30p_tot", "pct_65p_tot", "pct_65p_30p")
  
  # Intersezione sicura con le colonne realmente presenti nel dataset
  cols_30p <- intersect(paste0("P", 20:29), names(shp_sez))
  cols_65p <- intersect(paste0("P", 27:29), names(shp_sez))
  
  shp_sez |> 
    dplyr::mutate(
      area_m2     = sf::st_area(geom),
      area_km2    = units::drop_units(units::set_units(area_m2, km^2)),
      area_ha     = units::drop_units(units::set_units(area_m2, ha)),
      pop_tot     = P1,
      pop_30p     = rowSums(dplyr::across(dplyr::any_of(cols_30p)), na.rm = TRUE),
      pop_65p     = rowSums(dplyr::across(dplyr::any_of(cols_65p)), na.rm = TRUE),
      dens_km2    = dplyr::if_else(area_km2 > 0, pop_tot / area_km2, 0),
      dens_ha     = dplyr::if_else(area_ha > 0, pop_tot / area_ha, 0), 
      pct_30p_tot = dplyr::if_else(pop_tot > 0, (pop_30p / pop_tot) * 100, NA_real_),
      pct_65p_tot = dplyr::if_else(pop_tot > 0, (pop_65p / pop_tot) * 100, NA_real_),
      pct_65p_30p = dplyr::if_else(pop_30p > 0, (pop_65p / pop_30p) * 100, NA_real_)
    ) |> 
    dplyr::select(dplyr::all_of(intersect(id_cols, names(shp_sez))), dplyr::all_of(new_vars))
}

summarise_regional_demographics <- function(sez_analyzed) {
  sez_analyzed |> 
    sf::st_drop_geometry() |> 
    dplyr::summarise(
      tot_tracts           = dplyr::n(),
      inhabited_tracts     = sum(pop_tot > 0, na.rm = TRUE),
      tot_pop              = sum(pop_tot, na.rm = TRUE),
      tot_pop_30p          = sum(pop_30p, na.rm = TRUE),
      tot_pop_65p          = sum(pop_65p, na.rm = TRUE),
      pct_30p_tot          = (tot_pop_30p / tot_pop) * 100,
      pct_65p_tot          = (tot_pop_65p / tot_pop) * 100,
      pct_65p_30p          = (tot_pop_65p / tot_pop_30p) * 100,
      tot_area_km2         = sum(area_km2, na.rm = TRUE),
      tot_area_ha          = sum(area_ha, na.rm = TRUE),
      avg_tract_area_ha    = mean(area_ha, na.rm = TRUE),
      median_tract_area_ha = median(area_ha, na.rm = TRUE),
      overall_dens_km2     = tot_pop / tot_area_km2, 
      overall_dens_ha      = tot_pop / tot_area_ha,
      mean_tract_dens_km2  = mean(dens_km2, na.rm = TRUE),
      mean_tract_dens_ha   = mean(dens_ha, na.rm = TRUE),
      median_tract_dens_km2= median(dens_km2, na.rm = TRUE),
      median_tract_dens_ha = median(dens_ha, na.rm = TRUE)
    ) |> 
    tidyr::pivot_longer(cols = dplyr::everything(), names_to = "metric", values_to = "value")
}
