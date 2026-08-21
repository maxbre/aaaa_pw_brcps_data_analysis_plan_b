# R/04_hia_calculations.R - Unione tassi e calcolo casi attribuibili (AC)

join_pwe_by_causa <- function(df_sez, df_tassi, join_by = "cod_comune") {
  df_sez_clean  <- df_sez %>% dplyr::mutate(dplyr::across(dplyr::all_of(join_by), as.character))
  df_tassi_clean <- df_tassi %>% dplyr::mutate(dplyr::across(dplyr::all_of(join_by), as.character))
  
  df_tassi_clean %>%
    split(.$causa) %>%
    purrr::map(~ dplyr::left_join(df_sez_clean, .x, by = join_by))
}

AC_string_pmin <- function(data, col_pop30p, col_tasso, col_start, col_end) {
  data |> 
    dplyr::mutate(
      attesi    = .data[[col_pop30p]] * (.data[[col_tasso]] / 100),
      delta_PWE = .data[[col_start]] - pmin(.data[[col_start]], .data[[col_end]]),
      AF        = 1 - exp(-log(rr) / 10 * delta_PWE),
      AC        = attesi * AF,
      AF_low    = 1 - exp(-log(rr_lic) / 10 * delta_PWE),
      AC_low    = attesi * AF_low,
      AF_upp    = 1 - exp(-log(rr_uic) / 10 * delta_PWE),
      AC_upp    = attesi * AF_upp
    )
}

#' Aggrega casi attribuibili per comune e standardizza per 100.000 abitanti
#'
#' @param sez_sf  oggetto sf a livello di sezione censuaria, con colonne:
#'                ID_SEZ (id sezione), cod_comune (id comune),
#'                AC (casi attribuibili), pop_30p (popolazione 30+)
#'
#' @return oggetto sf a livello di comune con geometria dissolta,
#'         casi attribuibili totali, popolazione totale e tasso per 100k
#
# this is a slower version, see next one
#
# aggrega_per_comune <- function(sez_sf) {
#   
#   stopifnot(all(c("SEZ21_ID", "cod_comune", "AC", "pop_30p") %in% names(sez_sf)))
#   
#   comune_sf <- sez_sf %>%
#     group_by(cod_comune) %>%
#     summarise(
#       n_sezioni     = n(),
#       AC_tot        = sum(AC, na.rm = TRUE),
#       pop_30p_tot   = sum(pop_30p, na.rm = TRUE),
#       tasso_100k    = AC_tot / pop_30p_tot * 100000,
#       .groups = "drop"
#     ) %>%
#     st_make_valid()  # evita geometrie invalide dopo il dissolve
#   
#   return(comune_sf)
# }

aggrega_per_comune <- function(sez_sf) {
  
  stopifnot(all(c("SEZ21_ID", "cod_comune", "AC", "pop_30p") %in% names(sez_sf)))
  
  # 1. Aggregazione statistica: veloce, nessuna geometria coinvolta
  stats_comune <- sez_sf %>%
    st_drop_geometry() %>%
    group_by(cod_comune) %>%
    summarise(
      n_sezioni   = n(),
      AC_tot      = sum(AC, na.rm = TRUE),
      pop_30p_tot = sum(pop_30p, na.rm = TRUE),
      tasso_100k  = AC_tot / pop_30p_tot * 100000,
      .groups = "drop"
    )
  
  # 2. Dissolve geometrico: fatto una sola volta, sulla sola colonna di raggruppamento
  sf_use_s2(FALSE)  # geometrie planari, spesso più veloce/robusto per dissolve su piccola scala
  
  geom_comune <- sez_sf %>%
    st_make_valid() %>%          # pulizia PRIMA del dissolve, non dopo
    group_by(cod_comune) %>%
    summarise(.groups = "drop")  # nessuna colonna numerica -> dissolve puro, più leggero
  
  sf_use_s2(TRUE)  # ripristina il default
  
  # 3. Join tra geometria dissolta e statistiche
  comune_sf <- geom_comune %>%
    left_join(stats_comune, by = "cod_comune")
  
  return(comune_sf)
}
