# main.R - Pipeline di Esecuzione Principale

# 1. Caricamento Librerie ------------------------------------------------------

suppressPackageStartupMessages({
  library(tidyverse)
  library(sf)
  library(terra)
  library(tidyterra)
  library(ggspatial)
  library(trend)
  library(scales)
  library(patchwork)
})

# 2. Source di tutti i moduli della cartella /R --------------------------------

r_files <- list.files("R", pattern = "\\.R$", full.names = TRUE)
sapply(r_files, source)

# 3. Caricamento Dataset -------------------------------------------------------

# metadata
meta <- read_rds('./data_input/meta_21.rds')

# sezioni e popolazione censuaria 2021
sez_pop_21 <- read_sf("./data_input/shp_sez_pop_21.gpkg")

# shp comuni e percorsi
fpath_shp_com <- "./data_input/shp_comuni_2021.shp"
shp_comuni    <- read_sf(fpath_shp_com)

# shp provincia
shp_prov <- shp_comuni |> 
  mutate(cod_prov = substr(sprintf("%05.0f", PRO_COM), 1, 2)) |> 
  group_by(cod_prov) |> 
  summarise(.groups = "drop")

#shp regione
shp_rv <- shp_prov |> 
  summarise()

# geometrie sezioni censuarie
fpath_geom_sez <- "./data_input/shp_SEZ21_ID_geom.gpkg"
geom_sez       <- read_sf(fpath_geom_sez)

# raster CAMx
r_camx <- read_camx_tifs("./data_input/camx_tif")

# dati NO2 2019-2025
sez_no2 <- read_rds('./data_input/dat_no2_2019_2025.rds')

# tassi di incidenza e RR 2024
tassi_2024 <- read_csv('./data_input/inq_pop_tasso_rr.csv') |> 
  filter(inquinante == "no2", anno_tasso == 2024) |> 
  rename(cod_comune = cod_istat) |> 
  mutate(cod_comune = as.character(cod_comune))

# 4. Pipeline Demografica ------------------------------------------------------

demo_sez_21 <- process_census_demographics(sez_pop_21)
sum_demo    <- summarise_regional_demographics(demo_sez_21)
sum_demo

# sezioni con pop30p uguale zero
sez_pop_21 |> 
  st_drop_geometry() |> 
  mutate(p30p=rowSums(across(P20:P29), na.rm = TRUE)) |> 
  filter(p30p==0) |> 
  select(P1:P29, p30p) |> 
  summarise(
    total_rows = n(),
    across(everything(), \(x) sum(x, na.rm = TRUE))) |> 
  write_csv('./output/tbl_sez_classi_eta_pop30p_zero.csv')

# map pop30p

p_map_pop <- plot_sezione_map(
  sez_analyzed = demo_sez_21,
  fill_var     = "pop_30p",
  title        = NULL, #"Popolazione 30+ per sezione censuaria ISTAT",
  legend_title = "Abitanti\n(scala log)",
  use_log      = TRUE)+
  geom_sf(data=shp_rv,  fill=NA, colour ="grey50")

# note the log scale
p_map_pop
ggsave_report("./output/map_sez_pop30p.png", plot = p_map_pop)

# 5. Pipeline Raster CAMx ------------------------------------------------------

r_camx_crop <- crop_camx_stack(r_camx, vector_filename = "shp_comuni_2021.shp")

plot_and_save_layers(r_camx_crop, 
                     output_dir = "./output/raster_maps", 
                     custom_titles = paste0(2019:2025))

# Store the returned list of plots
camx_plots <- plot_and_save_layers(r_camx_crop,
                                   output_dir = NULL, 
                                   custom_titles = paste0(2019:2025))

# Display the first plot (e.g., 2019)
camx_plots[[1]]

# modify the north arrow and scale then plot
# display all plots together in a grid
no2_map_ts_rv <- patchwork::wrap_plots(camx_plots, ncol=3, guides = "collect")+
  plot_spacer() +
  guide_area() +
  plot_layout(ncol = 3, guides = "collect")

ggsave("./output/map_no2_ts_rv.png", plot=no2_map_ts_rv)
#ggsave_report("./output/map_no2_ts_rv.png", width = 6, height = 5, plot = no2_map_ts_rv)

# 6. Pipeline Trend Analysis (Mann-Kendall) ------------------------------------

# cfr function in 03_trend_analysis.R
# with BH correction
df_mk <- compute_municipal_mk_sen_long(sez_no2)
p_mk  <- plot_mk_sen_map(shp = shp_comuni, df_mk = df_mk, title = NULL)
p_mk
ggsave_report("./output/map_mk_sen_no2.png", plot = p_mk)

# boxplot trend
# plot_annual_boxplots_df(
#   df          = sez_no2,
#   value_var   = "pol_value",
#   year_var    = "year",
#   title       = "Distribuzione annuale NO2",
#   output_path = NULL #"./output/boxplot_annuale_no2.png"
#   )

# boxplot trend improved version
sez_no2 |> 
  mutate(year_fct = factor(year)) |> 
  ggplot() +
  geom_boxplot(aes(x = year_fct, y = pol_value)) +
  geom_hline(aes(yintercept = 10, linetype = "target OMS"), 
             color = "red", linewidth = 0.5) +
  scale_linetype_manual(name = NULL, values = c("target OMS" = "dashed")) +
  facet_wrap(vars(PROVINCIA), ncol = 2, strip.position = "top") +
  labs(x = NULL,
       y = expression(NO[2]~"["*mu*gm^-3*"]"))+
  theme_bw() +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.75, 0.1),   # x,y in [0,1] relative to full plot area
    legend.background = element_rect(fill = "white", color = "grey80", linewidth = 0.5),
    legend.key.width = unit(1.5, "cm")
  )

ggsave("./output/boxplot_no2_provincia.png")
#ggsave_report("./output/boxplot_no2_provincia.png")

# cumulative plot

# define mapping (Raw key -> Clean label)
scenario_no2 <- c(
  "camx_2019_no2" = "2019",
  "camx_2025_no2"  = "2025"
)

# prepare dataset by region
df_plot_no2 <- sez_no2 |> 
  prep_exposure_data(keys = names(scenario_no2)) |> # <--- look at this
  mutate(
    key = factor(
      key, 
      levels = names(scenario_no2), # Keeps the order
      labels = unname(scenario_no2) # Applies clean display labels
    )
  )

ggplot(df_plot_no2, aes(x = pol_value, y = pct_cum_pop, colour = key)) +
  geom_step(linewidth = 0.5) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal(base_size = 12) +
  labs(
    x = expression(paste("NO"[2], " [", mu, "g/m"^3, "]")),
    y = "popolazione cumulativa esposta",
    colour = "anno" )+
  geom_vline(xintercept = 10, colour = "grey50", linetype = "dashed")+
  annotate("text", x=8.5, y = 0.93, label = "OMS", colour="grey50", size=4)

ggsave("./output/no2_exp_pop_2019_2025.png", bg="white")

# prepare dataset by provincia
df_plot_no2_prov <- sez_no2 |> 
  prep_exposure_data(keys = names(scenario_no2), PROVINCIA) |> # <--- look at this
  mutate(
    key = factor(
      key, 
      levels = names(scenario_no2), # Keeps the order
      labels = unname(scenario_no2) # Applies clean display labels
    )
  )

ggplot(df_plot_no2_prov, aes(x = pol_value, y = pct_cum_pop, colour = key)) +
  geom_step(linewidth = 0.5) +
  facet_wrap(vars(PROVINCIA), ncol=2) +
  #scale_y_continuous(labels = scales::percent) +
  theme_minimal(base_size = 12) +
  labs(
    x = expression(paste("NO"[2], " [", mu, "g/m"^3, "]")),
    y = "popolazione cumulativa esposta",
    colour = "scenario" 
  ) +
  scale_y_continuous(breaks = breaks_extended(5), labels = scales::percent) +
  scale_x_continuous(breaks= breaks_extended(5))+
  geom_vline(xintercept = 10, colour = "grey50", linetype = "dashed")+
  annotate("text", x=8.5, y = 0.75, label = "OMS", colour="grey50", size=3, angle = 90)+
  theme_minimal(base_size = 12)+
  theme(legend.position = "inside",
        legend.position.inside = c(0.6,0),
        legend.justification = c(0, 0))

ggsave("./output/no2_exp_pop_2019_2025_by_province.png", bg="white")

# # =========================================================

# 7. Pipeline HIA & Join Cause -------------------------------------------------

pwe_sez_wide <- sez_no2 |> 
  select(COD_ISTAT, SEZ21, SEZ21_ID, year, pol_name, pol_value, P1, p30p) |> 
  mutate(target_oms_no2 = 10, cod_comune = str_sub(COD_ISTAT, 3, -1)) |> 
  pivot_wider(names_from = c(pol_name, year), values_from = pol_value, names_sep = "_")

list_cause <- join_pwe_by_causa(df_sez = pwe_sez_wide, df_tassi = tassi_2024)

ac_sez_no2 <- list_cause |> 
  map(\(df) AC_string_pmin(df, col_pop30p = "p30p",
                           col_tasso = "tasso", 
                           col_start = "no2_2025", 
                           col_end = "target_oms_no2"))

write_rds(ac_sez_no2, './output/ac_sez_no2_2019_2025_wide_all_causes.rds')

#ac_sez_no2<- read_rds('./output/ac_sez_no2_2019_2025_wide_all_causes.rds')

# delta PWE --------------------------------------------------------------------

# ac_sez_no2$RES |> 
#   summarise(mean_dealta = mean(delta_PWE),
#             median_delta = median((delta_PWE)),
#             min_delta = min(delta_PWE),
#             max_delta = max(delta_PWE))

ac_sez_no2$RES |> 
  summarise(
    across(
      delta_PWE, 
      list(
        mean   = \(x) mean(x, na.rm = TRUE),
        median = \(x) median(x, na.rm = TRUE),
        min    = \(x) min(x, na.rm = TRUE),
        max    = \(x) max(x, na.rm = TRUE),
        iqr    = \(x) IQR(x, na.rm = TRUE)
      ),
      .names = "{.fn}_delta")
  ) |> 
  write_csv('./output/tbl_sezioni_delta_controfattuale.csv')

# stratificazione by provincia

ac_sez_no2$RES |> 
  group_by(provincia) |> 
  summarise(
    across(
      delta_PWE, 
      list(
        mean   = \(x) mean(x, na.rm = TRUE),
        median = \(x) median(x, na.rm = TRUE),
        min    = \(x) min(x, na.rm = TRUE),
        max    = \(x) max(x, na.rm = TRUE),
        iqr    = \(x) IQR(x, na.rm = TRUE)
      ),
      .names = "{.fn}_delta"),
    .groups= "drop"
  ) |> 
  write_csv('./output/tbl_sezioni_delta_controfattuale_by_provincia.csv')

# ------------------------------------------------------------------------------
# stat causa RESP, stima puntuale

ac_sez_no2$RES |> 
  mutate(cod_prov = substr(sprintf("%05.0f", COD_ISTAT), 3, 4)) |> 
  group_by(provincia=prov_labels[cod_prov]) |> 
  summarise(attesi = sum(attesi),
            AC = sum(AC),
            AF = AC/attesi,
            AC_low = sum(AC_low),
            AC_upp = sum(AC_upp),
            .groups = "drop") |> 
  write_csv('./output/tbl_ac_af_provincia_stima_puntuale.csv')

ac_sez_no2$RES |> 
  summarise(attesi = sum(attesi),
            AC = sum(AC),
            AF = AC/attesi,
            AC_low = sum(AC_low),
            AC_upp = sum(AC_upp))|> 
  write_csv('./output/tbl_ac_af_regione_stima_puntuale.csv')

#-------------------------------------------------------------------------------
# mapping no2 and ac

# reading geometry sezione, comune, provincia, regione

# fpath_shp_com   <- "./data_input/shp_comuni_2021.shp"
# shp_comuni <- read_sf(fpath_shp_com)
# 
# shp_prov <- shp_comuni |> 
#   mutate(cod_prov = substr(sprintf("%05.0f", PRO_COM), 1, 2)) |> 
#   group_by(cod_prov) |> 
#   summarise(.groups = "drop")
# 
# shp_rv <- shp_prov |> 
#   summarise()

# join geom to dataframe and cause field to be filtered out
# pay attention to this!
ac_sez_no2_sf <- ac_sez_no2 |> 
  map(\(df) left_join(geom_sez, df, by = "SEZ21_ID")) |> 
  list_rbind(names_to = "causa") |> 
  st_as_sf()

# filtering per causa
# pay attention to this!
ac_sez_no2_resp_sf <- ac_sez_no2_sf |> filter(causa=="RES")

# plot no2 2025 per sezione
plot_sezione_map(
  sez_analyzed = ac_sez_no2_resp_sf,
  fill_var     = "no2_2025",                 # Nome colonna con valore da mappare
  palette      = "viridis",                   
  title        = NULL, #"Concentrazione media annuale NO2 per sezione (2025)",
  legend_title = pollutant_label("NO2")
)+
  geom_sf(data=shp_rv,  fill=NA, colour ="grey50")

ggsave_report("./output/map_sezioni_no2_2025.png")


# delta target oms

ggplot(ac_sez_no2_resp_sf) +
  geom_sf(aes(fill = delta_PWE), color = NA) +
  scale_fill_viridis_c(
    option = "magma",
    name = pollutant_label("NO2"), 
    na.value = "transparent") +
  theme_void()+
  geom_sf(data=shp_rv,  fill=NA, colour ="grey50")

ggsave_report("./output/map_sezioni_delta_no2_2025_target_oms.png")

# map attesi

ggplot(ac_sez_no2_resp_sf) +
  geom_sf(aes(fill = attesi), color = NA) +
  scale_fill_viridis_c(
    option = "plasma",
    begin = 0.1,
    end = 0.9,
    name = "attesi", 
    na.value = "transparent") +
  theme_void()+
  geom_sf(data=shp_rv,  fill=NA, colour ="grey50")

ggsave_report("./output/map_sezioni_attesi.png")

# map AC
ggplot(ac_sez_no2_resp_sf) +
  geom_sf(aes(fill = AC), color = NA) +
  scale_fill_viridis_c(
    option = "magma",
    begin = 0.2,
    end = 0.8,
    name = "AC", 
    na.value = "transparent") +
  theme_void()+
  geom_sf(data=shp_rv,  fill=NA, colour ="grey50")

ggsave_report("./output/map_sezioni_AC.png")

# calcolo dell'esposizione media pesata 2025
# NOTA da impiegare solo ed esclusivamente a livello descrittivo medio 
# dettaglio per cause respiratorie ma di fatto è indifferente, solo perchè è una lista

# per tutta la regione
ac_sez_no2_resp_sf |>
  st_drop_geometry() |> 
  summarise(pwe_avg_rv = sum(no2_2025 * p30p, na.rm = TRUE) / sum(p30p, na.rm = TRUE)
  ) |> 
  write_csv('./output/tbl_pwe_regione_no2_2025.csv')

# per le province della Regione Veneto
ac_sez_no2_resp_sf |>
  st_drop_geometry() |>
  group_by(provincia) |>
  summarise(pwe_avg_rv = sum(no2_2025 * p30p, na.rm = TRUE) / sum(p30p, na.rm = TRUE) )|> 
  ungroup()|> 
  write_csv('./output/tbl_pwe_provincia_no2_2025.csv')

# La popolazione over 30 del Veneto è esposta a una concentrazione media pesata (PWE) di $NO_2$ pari a $X\ \mu g/m^3$
# Il livello medio provinciale di PWE varia  nell'intervallo da circa   (Belluno) circa (Padova, Venezia).

# map AC not significant, not to be shown!?

# map AF -------------------------------------------------------------

ggplot(ac_sez_no2_resp_sf) +
  geom_sf(aes(fill = AF), color = NA) +
  scale_fill_viridis_c(
    option = "magma",
    name = "AF", 
    na.value = "transparent") +
  theme_void()+
  geom_sf(data=shp_rv,  fill=NA, colour ="grey50")

ggsave_report("./output/map_AF_sezioni_no2_2025.png")

# aggregazione somme per comune
# it takes a long time

comune_sf <- aggrega_per_comune(ac_sez_no2_resp_sf)

# controllo rapido
comune_sf %>%
  st_drop_geometry() %>%
  arrange(desc(AC_tot)) %>%
  head(10)

# fare grafico su questo oggetto
comune_ac_sf <- shp_comuni |>
  mutate(cod_comune = as.character(PRO_COM)) |> 
  left_join(comune_sf |> st_drop_geometry(), by=join_by(cod_comune)) 

# AC per comune
comune_ac_sf |> 
  ggplot()+
  geom_sf(aes(fill = AC_tot))+
  scale_fill_viridis_c()+
  labs(fill="AC")+
  theme_void()

ggsave_report("./output/map_AC_comune_no2_2025.png")

# tasso norm 100k
# comune_ac_sf |> 
#   ggplot()+
#   geom_sf(aes(fill = tasso_100k))+
#   scale_fill_viridis_c()+
#   theme_void()

# mappa geometrie sezioni

# Semplificazione topologica (mantiene il 5% dei punti)
geom_sez_simp <- rmapshaper::ms_simplify(geom_sez, keep = 0.05, keep_shapes = TRUE)

# In alternativa con sf (più rapido, ma può creare micro-spazi tra poligoni):
#geom_sez_simp <- sf::st_simplify(geom_sez, dTolerance = 20, preserveTopology = TRUE)

# Rendering con ggplot2
ggplot(data = geom_sez_simp) +
  geom_sf(fill = "transparent", color = "grey40", linewidth = 0.05) +
  theme_void()+
  geom_sf(data=shp_rv,  fill=NA, colour ="grey40")

ggsave_report("./output/map_sezioni_censuarie_rv.png")

# 8. Pipeline Bootstrap & Incertezza -------------------------------------------

# check the function for the default
# questo run considera tutti gli anni disponibili

# Definizione delle colonne degli anni di esposizione dal 2019 al 2025
anni_no2 <- paste0("no2_", 2019:2025)

# Esecuzione del bootstrap spazio-temporale su tutte le cause (CVD, NAT, RES, TUM)
# boot_spat <- map(
#   ac_sez_no2,
#   \(x) run_bootstrap_ac_spatiotemporal_opt(
#     data        = x,
#     col_years   = anni_no2,
#     col_pop30p  = "p30p",
#     col_tasso   = "tasso",
#     col_end     = "target_oms_no2",
#     cluster_var = "cod_comune",
#     B           = 1000,
#     seed        = 1234
#   )
# )

# new version
boot_spat_ac <- map(
  ac_sez_no2,
  \(x) run_bootstrap_ac_ind(
    data        = x,
    col_years   = anni_no2,
    col_pop30p  = "p30p",
    col_tasso   = "tasso",
    col_end     = "target_oms_no2",
    B           = 1000,
    seed        = 1234
  )
)

write_rds(boot_spat_ac, './output/list_boostrap_spatio_temp_all_causes.rds')

#boot_spat <- read_rds('./output/list_boostrap_spatio_temp_all_causes.rds')

# Estrazione dei percentili 95% CI (2.5%, 50%, 97.5%) per ogni causa
# sintesi_spatiotemporal <- boot_spat |>
#   map(\(df_boot) {
#     df_boot |>
#       summarise(
#         
#         AC_median = median(casi_attribuibili),
#         AC_p2.5   = quantile(casi_attribuibili, 0.025),
#         AC_p97.5  = quantile(casi_attribuibili, 0.975),
#         
#         PAF_median = median(paf),
#         PAF_p2.5   = quantile(paf, 0.025),
#         PAF_p97.5  = quantile(paf, 0.975)
#       )
#   }) |>
#   list_rbind(names_to = "causa")


sintesi_spatiotemporal <- boot_spat_ac |>
  map(\(df_boot) {
    df_boot |>
      summarise(
        
        AC_median = median(AC_boot),
        AC_p2.5   = quantile(AC_boot, 0.025),
        AC_p97.5  = quantile(AC_boot, 0.975),
        
        PAF_median = median(PAF_boot),
        PAF_p2.5   = quantile(PAF_boot, 0.025),
        PAF_p97.5  = quantile(PAF_boot, 0.975)
      )
  }) |>
  list_rbind(names_to = "causa")

# check resuts
sintesi_spatiotemporal |> 
  filter(causa =="RES") |> 
  write_csv('./output/tab_stat_bootstrap_spatio_temp_cause_resp.csv')

# plot bootstrap density
plot_bootstrap_density(
  df_boot     = boot_spat_ac$RES,
  var_name    = "AC_boot",
  causa_label = "Mortalità per cause respiratorie",
  output_path = "./output/ac_resp_bootstrap_density_spat_temp.png"
)

plot_bootstrap_density(
  df_boot     = boot_spat_ac$RES,
  var_name    = "AC_boot",
  causa_label = NULL,
  output_path = NULL)+
  labs(title=NULL, 
       subtitle = NULL,
       x = "casi attribuibili",
       caption = NULL)

ggsave_report("./output/ac_resp_bootstrap_density_spat_temp_no_labs.png")

plot_bootstrap_density(
  df_boot     = boot_spat_ac$RES,
  var_name    = "AC_boot",
  causa_label = NULL,
  output_path = NULL)+
  labs(title=NULL, 
       subtitle = NULL,
       x = "casi attribuibili",
       caption = NULL)+
  geom_vline(xintercept = 109, colour = "grey50", linetype = "dotted")

ggsave_report("./output/ac_resp_bootstrap_density_spat_temp_no_labs_e_stima_puntuale.png")


summary_table <- summarize_boot_metrics(boot_spat_ac$RES)

summary_table |> 
  write_csv('./output/tbl_bootstrap_summary.csv')

#-------------------------------------------------------------------------------
# stop from here
# simple bootstrap -------------------------------------------------------------

# # qui nota ancora anni di esposizione dal 2019 al 2025, campionati casualmente
# boot_simp <- map(ac_sez_no2,
#                  \(x) run_bootstrap_ac_simple(x, col_exp = paste0("no2_", 2019:2025), B = 1000))
# 
# 
# 
# comp_metrics <- compare_bootstrap_metrics(
#   Semplice = boot_simp$RES,
#   SpazioTemporale = boot_spat$RES,
#   var_name = "casi_attribuibili"
# )
# 
# comp_metrics
# 
# plot_bootstrap_comparison(
#   Semplice = boot_simp$RES,
#   SpazioTemporale = boot_spat$RES,
#   var_name = "casi_attribuibili"
# )
# 
# ggsave_report("./output/bootstrap_comparison_res.png")
#-------------------------------------------------------------------------------

# simple bootstrap
# questa solo considerando anno 2025  cause respiratorie

# boot_simp_2025 <- map(ac_sez_no2, ~ run_bootstrap_ac_simple(.x,
#                                                             col_exp = "no2_2025",
#                                                             B = 1000))
# 
# write_rds(boot_simp_2025, './output/list_boostrap_simple_2025_all_causes.rds')
# 
# #boot_simp_2025 <- read_rds('./output/list_boostrap_simple_2025_all_causes.rds')
# 
# # confronto più significativo
# comp_metrics_2025 <- compare_bootstrap_metrics(
#   Semplice2025 = boot_simp_2025$RES,
#   SpazioTemporale = boot_spat$RES,
#   var_name = "casi_attribuibili"
# )
# 
# comp_metrics_2025 |> 
#   write_csv('./output/tbl_bootstrap_compare_metrics_simple_vs_bootstrap.csv')
# 
# plot_bootstrap_comparison(
#   Semplice = boot_simp_2025$RES,
#   SpazioTemporale = boot_spat$RES,
#   var_name = "casi_attribuibili")+
#   labs(title = NULL,
#        x = "casi attribuibili")
# 
# ggsave_report("./output/bootstrap_comparison_res_simple_spattemp.png")


# bootstrap ac indipendent sections

# solo anno 2025

# boot_spat_ac_indip_2025 <- map(
#   ac_sez_no2,
#   \(x) run_bootstrap_ac_indip(
#     data        = x,
#     col_years   = "no2_2025",
#     col_pop30p  = "p30p",
#     col_tasso   = "tasso",
#     col_end     = "target_oms_no2",
#     B           = 1000,
#     seed        = 1234)
#   )
# 
# 
# 
# plot_bootstrap_density(
#   df_boot     = boot_spat_ac_indip_2025$RES,
#   var_name    = "AC_boot",
#   causa_label = NULL,
#   output_path = NULL)+
#   labs(title=NULL, 
#        subtitle = NULL,
#        x = "casi attribuibili",
#        caption = NULL)+
#   geom_vline(xintercept = 109, colour = "grey50", linetype = "dotted")
# 
# 
# plot_bootstrap_comparison(
#    Semplice2025 = boot_spat_ac_indip_2025$RES,
#    SpazioTemporale = boot_spat_ac$RES,
#    var_name = "AC_boot")+
#    labs(title = NULL,
#         x = "casi attribuibili")
#    
# # spazio temporale anno 2025
# 
# boot_spat_ac_2025 <- map(
#   ac_sez_no2,
#   \(x) run_bootstrap_ac(
#     data        = x,
#     col_years   = "no2_2025",
#     col_pop30p  = "p30p",
#     col_tasso   = "tasso",
#     col_end     = "target_oms_no2",
#     cluster_var = "cod_comune",
#     B           = 1000,
#     seed        = 1234
#   )
# )
# 
# plot_bootstrap_comparison(
#   Semplice2025 = boot_spat_ac_indip_2025$RES,
#   Spatial2025 = boot_spat_ac_2025$RES,
#   var_name = "AC_boot")+
#   labs(title = NULL,
#        x = "casi attribuibili")
# 
# 
# plot_bootstrap_comparison(
#   SpatialTemp = boot_spat_ac$RES,
#   Spatial2025 = boot_spat_ac_2025$RES,
#   var_name = "AC_boot")+
#   labs(title = NULL,
#        x = "casi attribuibili")
# 
# ### isolare effetto clustering -------------------------------------------------
# 
# 
# library(dplyr)
# library(ggplot2)
# library(tidyr)
# 
# # 1. Esecuzione delle due simulazioni bootstrap
# set.seed(1234)
# 
# res_cluster <- run_bootstrap_ac(
#   data = ac_sez_no2$RES, 
#   B = 1000, 
#   seed = 1234
# )  |> mutate(Metodo = "Spazio-Temporale (Cluster)")
# 
# res_indip <- run_bootstrap_ac_indip(
#   data = ac_sez_no2$RES, 
#   B = 1000, 
#   seed = 1234
# )  |>  mutate(Metodo = "Semplice (Indipendente)")
# 
# # 2. Unione dei risultati
# df_compare <- bind_rows(res_cluster, res_indip)
# 
# # 3. Calcolo delle metriche sintetiche e dell'effetto di inflazione
# summary_metrics <- df_compare %>%
#   group_by(Metodo) %>%
#   summarise(
#     Media         = mean(AC_boot),
#     Mediana       = median(AC_boot),
#     SD            = sd(AC_boot),
#     Q2.5          = quantile(AC_boot, 0.025),
#     Q97.5         = quantile(AC_boot, 0.975),
#     Ampiezza_IC95 = Q97.5 - Q2.5,
#     .groups       = "drop"
#   )  |> 
#   mutate(
#     # Isolamento dell'impatto del clustering (Ratio rispetto al metodo indipendente)
#     Ratio_SD   = SD / SD[Metodo == "Semplice (Indipendente)"],
#     Ratio_IC95 = Ampiezza_IC95 / Ampiezza_IC95[Metodo == "Semplice (Indipendente)"]
#   )
# 
# summary_metrics
# 
# 
# ggplot(df_compare, aes(x = AC_boot, fill = Metodo, color = Metodo)) +
#   geom_density(alpha = 0.35, linewidth = 0.8) +
#   scale_fill_manual(values = c("steelblue", "firebrick")) +
#   scale_color_manual(values = c("steelblue", "firebrick")) +
#   labs(
#     title = "Impatto del Clustering Spaziale sulla Distribuzione dei Casi Attribuibili",
#     subtitle = "Confronto tra ricampionamento indipendente delle sezioni e ricampionamento a cluster comunali",
#     x = "Casi Attribuibili (AC)",
#     y = "Densità di Probabilità",
#     fill = "Schema Bootstrap",
#     color = "Schema Bootstrap"
#   ) +
#   theme_minimal(base_size = 12) +
#   theme(
#     legend.position = "bottom",
#     plot.title = element_text(face = "bold")
#   )
