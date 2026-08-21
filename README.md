
<!-- README.md is generated from README.Rmd. Please edit that file -->

# aaaa_pw_brcps_data_analysis_plan_b

My data analysis for the master course “BRCPS26” @unipd.

This is actually the Plan B of the project work.

For reference see the approach in my GitHub repos:

- 20261019_pw_data_analysis_pipeline

- 20261019_report_pw_ubep

which are hopefully left for a next and much more interesting
finalisation.

So for now, let’s go straight and hit the long and winding road in a
future time!

…but dangerous paths are much more interesting! ;-)

------------------------------------------------------------------------

    progetto/
    ├── progetto.Rproj
    ├── main.R
    ├── R/
    │   ├── 00_utils.R
    │   ├── 01_demographics.R
    │   ├── 02_raster_processing.R
    │   ├── 03_trend_analysis.R
    │   ├── 04_hia_calculations.R
    │   ├── 05_bootstrap_uncertainty.R
    │   └── 06_plotting.R
    ├── data_input/
    └── output/

- main.R: Script principale di orchestrazione dell’intera pipeline.

- R/00_utils.R: Helper generici, dizionari di etichette e funzioni
  IO/export.

- R/01_demographics.R: Calcolo metriche demografiche di sezione e
  sintesi regionale.

- R/02_raster_processing.R: Lettura stack raster CAMx, ritaglio e
  mascheramento su vettori ISTAT.

- R/03_trend_analysis.R: Boxplot concentrazioni ambientali, test
  Mann-Kendall e calcolo dello stimatore Sen’s slope per comune.

- R/04_hia_calculations.R: Join con i tassi di mortalità per causa e
  calcolo dei casi attribuibili (AC).

- R/05_bootstrap_uncertainty.R: Funzioni per il Bootstrap
  Spazio-Temporale e IID e tabella di confronto metriche di incertezza.

- R/06_plotting.R: Funzioni di visualizzazione cartografica (sf e terra)
  e grafici ggplot2.

- data_input/ e output/: Cartelle predisposte per l’inserimento dei dati
  di input e per il salvataggio dei risultati generati.
