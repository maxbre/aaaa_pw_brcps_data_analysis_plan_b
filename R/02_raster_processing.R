# R/02_raster_processing.R - Manipolazione dati raster CAMx

read_camx_tifs <- function(dir_path = "data_input/camx_tif") {
  tif_files <- list.files(path = dir_path, pattern = "\\.tif$", full.names = TRUE)
  if (length(tif_files) == 0) stop(paste("Nessun file .tif trovato in:", dir_path))
  terra::rast(tif_files)
}

crop_camx_stack <- function(raster_stack, vector_dir = "./data_input", vector_filename = NULL, mask = TRUE) {
  if (is.null(vector_filename)) {
    vec_files <- list.files(path = vector_dir, pattern = "\\.(shp|gpkg|geojson)$", full.names = TRUE)
    if (length(vec_files) == 0) stop(paste("Nessun vettore trovato in:", vector_dir))
    vector_path <- vec_files[1]
  } else {
    vector_path <- file.path(vector_dir, vector_filename)
  }
  
  vec <- terra::vect(vector_path)
  if (terra::crs(raster_stack) != terra::crs(vec)) {
    vec <- terra::project(vec, terra::crs(raster_stack))
  }
  
  cropped_stack <- terra::crop(raster_stack, vec)
  if (mask) cropped_stack <- terra::mask(cropped_stack, vec)
  
  return(cropped_stack)
}
