rm(list=ls())
library(ggrepel)
library(ggspatial)
library(mapSpain)
library(sf)
library(dplyr)
library(ggplot2)
library(future.apply)
setwd("C://Users//samue//Desktop//ProyectoTFG/DatosENP/")

#---------------------------------------------1 Recortar Comunidades autonomas--------------------------------------------

##---------------------------------------1.1 Procesar capas-----------------------------------

#El zip descargado de: https://www.miteco.gob.es/es/biodiversidad/servicios/banco-datos-naturaleza/informacion-disponible/enp_descargas.html
#Contiene tanto peninsula, baleares y canarias en una sola capa


####---------------------1.1.1 Leer capa--------------------------

enp <- st_read("Datos/Datosdescargados/enp2024_p.shp") 
parqnacionales <- enp %>%
  filter(ODESIGNATE == "Parque Nacional")

####--------------------1.1.2 Corregir geometrias----------------
parqnacionales <- st_cast(parqnacionales, "MULTIPOLYGON")
parqnacionales <- st_make_valid(parqnacionales)


####---------------1.1.3 Reproyectar geometrías--------------

parqnacionales <-  st_transform(parqnacionales, 3035)


####------------------1.1.4 Guardar capa final-------------------

st_write(parqnacionales, 
         dsn = "Datos/Datoscorregidos/ENPcorregido/parquesnacionalescorregido.gpkg", 
         driver = "GPKG", 
         delete_layer = TRUE)


##---------------------------------------1.2 Bucle para las comunidades autonomas------------------------------------

parqnacionalesfinal <- st_read("Datos/Datoscorregidos/ENPcorregido/parquesnacionalescorregido.gpkg")

####-------------1.2.1 Obtener comunidades autónomas---------
ccaa_sf <- esp_get_ccaa(moveCAN = FALSE, epsg = 3035)
ccaa_sf <- st_make_valid(ccaa_sf)

for (i in 1:nrow(ccaa_sf)) {
  nombre_ccaa <- ccaa_sf$ccaa.shortname.es[i]
  codigo_ccaa <- ccaa_sf$codauto[i]
  
  cat("Procesando:", nombre_ccaa, "\n")
  
  ccaa_geom <- ccaa_sf[i, ]
  
  ####----------1.2.2 Calcular y expandir bbox---------------
  bbox <- st_bbox(ccaa_geom)
  
  x_diff <- bbox$xmax - bbox$xmin
  y_diff <- bbox$ymax - bbox$ymin
  max_diff <- max(x_diff, y_diff)
  
  x_buffer <- max_diff * 0.15
  y_buffer <- max_diff * 0.15
  
  bbox_expandida <- structure(
    c(
      xmin = bbox$xmin - x_buffer,
      ymin = bbox$ymin - y_buffer,
      xmax = bbox$xmax + x_buffer,
      ymax = bbox$ymax + y_buffer
    ),
    class = "bbox",
    crs = st_crs(ccaa_geom)
  )
  
  ####----------1.2.3 Convertir bbox a polígono-------------
  bbox_poly <- st_as_sfc(st_bbox(bbox_expandida, crs = st_crs(ccaa_geom)))
  
  ####----------1.2.4 Intersección con polígono--------------
  parqnacionales_crop <- st_intersection(parqnacionalesfinal, bbox_poly)
  
  ####------------1.2.5 Guardar como GeoPackage---------------
  st_write(parqnacionales_crop, paste0("Datos/DatosporComunidad/Parqnacionales/", nombre_ccaa, ".gpkg"), delete_dsn = TRUE)
}



#---------------------------------------2 Recortar municipios-----------------------------------
##-----------------------------2.1 Configurar paralelización--------------------
###-------------------2.1.1 Maximo de memoria----------------
options(future.globals.maxSize = 2 * 1024^3)  

###-------------------2.1.2 Numero de nucleos-------------------
parallel::detectCores() #Detectamos cuantos nucleos tenemos
future::availableCores() #Cuantos nucleos estan disponibles para ser usaros

plan(multisession, workers = 12)

##-----------------------------2.2 Obtener municipios-----------------------
municipios <- esp_get_munic(moveCAN = FALSE, epsg = 3035)
CCAA_sf <- esp_get_ccaa(moveCAN = FALSE, epsg = 3035)

##----------------------------2.3 Comunidades a paralelizar------------------
comunidades <- unique(CCAA_sf$ccaa.shortname.es)

##-----------------------------2.4 Función a paralelizar--------------------------

procesar_comunidad <- function(comunidad_objetivo) {
  cat("▶ Procesando comunidad:", comunidad_objetivo, "\n")
  
  ###------------------------2.4.1 Codigo de comunidad------------------------
  cod_comunidad <- CCAA_sf %>%
    filter(ccaa.shortname.es == comunidad_objetivo) %>%
    pull(codauto)
  
  ###------------------------2.4.2 Muncipios de comunidad---------------------
  municipios_comunidad <- municipios %>%
    filter(codauto == cod_comunidad) %>%
    st_make_valid()
  
  ###-----------------------2.4.3 Ruta de parques nacionales por comunidad---------------
  
  parqnacionales_path <- paste0("Datos/DatosporComunidad/Parqnacionales/", comunidad_objetivo, ".gpkg")
  dir_out <- paste0("Capasfinales/Parqnacionales/", comunidad_objetivo)
  
  if (!file.exists(parqnacionales_path)) {
    return(paste0("⚠ Archivo suelos no encontrado para ", comunidad_objetivo))
  }
  
  dir.create(dir_out, showWarnings = FALSE, recursive = TRUE)
  
  ###-----------------------2.4.4 Leer parques nacionales---------------------------------
  parqnacionales_ccaa <- st_read(parqnacionales_path, quiet = TRUE)
  
  resultados <- vector("list", nrow(municipios_comunidad))
  ###-------------------------2.4.5 Bucle--------------------------------------
  for (i in seq_len(nrow(municipios_comunidad))) {
    muni <- municipios_comunidad[i, ]
    nombre_muni <- gsub(" ", "_", gsub("/", "o", muni$name))
    output_path <- file.path(dir_out, paste0(nombre_muni, ".geojson"))
    
    ####--------------------2.4.5.1 Mantener municipios ya obtenidos------------    
    if (file.exists(output_path)) {
      resultados[[i]] <- paste0("✔ Ya existe: ", nombre_muni)
      next
    }
    
    tryCatch({
      
      ###------------------2.4.5.2 Calcular y expandir bbox-----------------
      bbox <- st_bbox(muni)
      x_diff <- bbox$xmax - bbox$xmin
      y_diff <- bbox$ymax - bbox$ymin
      max_diff <- max(x_diff, y_diff)
      
      x_buffer <- max_diff * 0.15
      y_buffer <- max_diff * 0.15
      
      bbox_expandido <- structure(
        c(
          xmin = bbox$xmin - x_buffer,
          ymin = bbox$ymin - y_buffer,
          xmax = bbox$xmax + x_buffer,
          ymax = bbox$ymax + y_buffer
        ),
        class = "bbox",
        crs = st_crs(muni)
      )
      
      ###------------------2.4.5.3 Convertir bbox a polígono--------------
      bbox_poly <- st_as_sfc(st_bbox(bbox_expandido, crs = st_crs(muni)))
      
      ###------------------2.4.5.4 Intersección con poligono--------------
      recorte <- st_intersection(parqnacionales_ccaa, bbox_poly)
      
      ###------------------2.4.5.5 Detector de error---------------------
      
      if (nrow(recorte) > 0) {
        st_write(recorte, output_path, delete_layer = TRUE, quiet = TRUE)
        resultados[[i]] <- paste0("💾 Guardado: ", nombre_muni)
      } else {
        st_write(muni[0, ], output_path, delete_layer = TRUE, quiet = TRUE)  # Archivo vacío
        resultados[[i]] <- paste0("⚠ Vacío (guardado): ", nombre_muni)
      }
    }, error = function(e) {
      resultados[[i]] <- paste0("❌ Error en ", nombre_muni, ": ", e$message)
    })
  }
  
  return(resultados)
}

##----------------------------2.5 Ejecutar en paralelo-------------------
resultados_totales <- future_lapply(comunidades, procesar_comunidad)
##----------------------------2.6 Resumen final--------------------------
cat("\n✅ Procesamiento completado.\n")
