# ============================================================
#  PANEL MENSUAL iNaturalist (versión API)
#  Igual que la versión con CSV, pero los datos se descargan de la
#  API de iNaturalist y se guardan en una caché local.
#  - 1ª ejecución: descarga todo el proyecto.
#  - Siguientes: solo pide lo nuevo/modificado (updated_since).
# ============================================================

Sys.setlocale("LC_TIME", "es_ES.UTF-8")

# ── 0. PARÁMETROS DEL USUARIO ────────────────────────────────
PROYECTO_ID   <- "maestro-ecologist-extendido"          # id numérico o slug (el final de la URL del proyecto)
ARCHIVO_CACHE <- "data/cache_inat.rds"  # caché local de observaciones
FORZAR_DESCARGA_COMPLETA <- FALSE       # TRUE = ignora la caché y vuelve a bajar todo
                                        # (úsalo de vez en cuando: updated_since no detecta
                                        #  observaciones borradas o sacadas del proyecto)
ULTIMOS_N_MESES <- NULL                 # NULL = genera todos los meses; 2 = solo los 2 últimos


# ── 1. PAQUETES ──────────────────────────────────────────────
# install.packages(c("tidyverse", "lubridate", "patchwork", "sf",
#                    "rnaturalearth", "rnaturalearthdata", "httr2"))

library(tidyverse)
library(lubridate)
library(patchwork)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(httr2)


# ── 2. DESCARGA DESDE LA API ─────────────────────────────────
# Doc: https://api.inaturalist.org/v1/docs/
# Máx. 200 resultados por página. Se pagina con id_above (en vez de
# page) porque page se limita a 10.000 resultados en total.

descargar_inat <- function(project_id, updated_since = NULL) {

  filas   <- list()
  id_above <- 0
  total   <- NA

  repeat {
    resp <- request("https://api.inaturalist.org/v1/observations") |>
      req_url_query(
        project_id    = project_id,
        per_page      = 200,
        order_by      = "id",
        order         = "asc",
        id_above      = id_above,
        updated_since = updated_since   # si es NULL, se omite
      ) |>
      req_user_agent("panel-mensual-inat (R/httr2)") |>
      req_retry(max_tries = 5) |>
      req_perform() |>
      resp_body_json()

    if (is.na(total)) {
      total <- resp$total_results
      message("  · Observaciones a descargar: ", total)
    }

    res <- resp$results
    if (length(res) == 0) break

    filas <- c(filas, map(res, function(o) {
      tibble(
        id                = o$id,
        observed_on       = o$observed_on %||% NA_character_,
        rank              = o$taxon$rank %||% NA_character_,
        taxon_name        = o$taxon$name %||% NA_character_,
        iconic_taxon_name = o$taxon$iconic_taxon_name %||% NA_character_,
        location          = o$location %||% NA_character_
      )
    }))

    id_above <- tail(map_dbl(res, "id"), 1)
    message("  · Descargadas ", length(filas), " / ", total)

    if (length(res) < 200) break
    Sys.sleep(1)   # la API pide no pasar de ~60 peticiones/minuto
  }

  if (length(filas) == 0) return(NULL)
  bind_rows(filas)
}

# Convierte la respuesta de la API al formato que usa el resto del script
# (mismas columnas que el CSV: observed_on, taxon_species_name, latitude...)
normalizar_inat <- function(df) {
  df |>
    mutate(
      observed_on = ymd(observed_on),
      # nombre de especie: si la ID es especie o inferior, nos quedamos con el binomio
      taxon_species_name = if_else(
        rank %in% c("species", "subspecies", "variety", "form", "hybrid"),
        word(taxon_name, 1, 2),
        NA_character_
      ),
      latitude  = as.numeric(str_split_fixed(location, ",", 2)[, 1]),
      longitude = as.numeric(str_split_fixed(location, ",", 2)[, 2])
    ) |>
    select(id, observed_on, taxon_species_name, iconic_taxon_name,
           latitude, longitude)
}

if (!dir.exists(dirname(ARCHIVO_CACHE))) dir.create(dirname(ARCHIVO_CACHE), recursive = TRUE)

if (file.exists(ARCHIVO_CACHE) && !FORZAR_DESCARGA_COMPLETA) {
  cache <- readRDS(ARCHIVO_CACHE)
  message("Caché cargada: ", nrow(cache$datos), " observaciones (última descarga: ",
          format(cache$ultima_descarga, "%Y-%m-%d %H:%M"), " UTC)")

  # Margen de 1 día por si hay desfases de reloj
  desde <- format(cache$ultima_descarga - days(1), "%Y-%m-%dT%H:%M:%SZ")
  nuevos <- descargar_inat(PROYECTO_ID, updated_since = desde)

  datos <- cache$datos
  if (!is.null(nuevos)) {
    nuevos <- normalizar_inat(nuevos)
    # las observaciones modificadas sustituyen a las antiguas (mismo id)
    datos <- bind_rows(anti_join(datos, nuevos, by = "id"), nuevos)
    message("Actualizadas/añadidas: ", nrow(nuevos))
  } else {
    message("No hay observaciones nuevas.")
  }
} else {
  message("Descarga completa del proyecto '", PROYECTO_ID, "'...")
  crudo <- descargar_inat(PROYECTO_ID)
  if (is.null(crudo)) stop("La API no devolvió observaciones para ese proyecto.")
  datos <- normalizar_inat(crudo)
}

saveRDS(list(datos = datos, ultima_descarga = Sys.time() |> with_tz("UTC")),
        ARCHIVO_CACHE)

inat <- datos |>
  mutate(
    mes_obs  = month(observed_on),
    anio_obs = year(observed_on)
  )

if (nrow(inat) == 0 || all(is.na(inat$observed_on))) {
  stop("No hay observaciones con fecha válida (observed_on).")
}


# ── 2.1 RANGO DE MESES A GENERAR ──────────────────────────────
fecha_min <- min(inat$observed_on, na.rm = TRUE)
fecha_max <- max(inat$observed_on, na.rm = TRUE)

inicio_rango <- floor_date(fecha_min, "month")
fin_rango    <- floor_date(fecha_max, "month")

secuencia_meses <- seq(inicio_rango, fin_rango, by = "1 month")

lista_mes_anio <- tibble(
  fecha = secuencia_meses,
  mes   = month(secuencia_meses),
  anio  = year(secuencia_meses)
)

if (!is.null(ULTIMOS_N_MESES)) {
  lista_mes_anio <- tail(lista_mes_anio, ULTIMOS_N_MESES)
}

message("Se generarán ", nrow(lista_mes_anio), " paneles: de ",
        format(min(lista_mes_anio$fecha), "%B %Y"), " a ",
        format(max(lista_mes_anio$fecha), "%B %Y"))


# ── 3. PALETA DE COLORES iNATURALIST ─────────────────────────
inat_colores <- c(
  "Aves"           = "#1E90FF",
  "Insecta"        = "#FF8C00",
  "Plantae"        = "#73AC13",
  "Mammalia"       = "#5D4037",
  "Reptilia"       = "#9E9D24",
  "Fungi"          = "#BF360C",
  "Amphibia"       = "#00897B",
  "Arachnida"      = "#DC143C",
  "Actinopterygii" = "#4682B4",
  "Mollusca"       = "#6A1B9A",
  "Chromista"      = "#F9A825",
  "Protozoa"       = "#E91E63",
  "Desconocido"    = "#AAAAAA"
)

limpiar_taxon <- function(x) replace_na(x, "Desconocido")

mundo <- ne_countries(scale = "medium", returnclass = "sf")


# ── 4. FIGURA VACÍA (para meses sin observaciones) ────────────
generar_panel_vacio <- function(nombre_mes) {
  aviso <- ggplot() +
    theme_void() +
    annotate("text", x = 0.5, y = 0.55,
             label    = "Sin observaciones",
             size     = 9,
             fontface = "bold",
             color    = "#AAAAAA",
             hjust    = 0.5) +
    annotate("text", x = 0.5, y = 0.42,
             label = "no hay registros para este mes",
             size  = 5,
             color = "#BBBBBB",
             hjust = 0.5) +
    xlim(0, 1) + ylim(0, 1) +
    theme(plot.background = element_rect(fill = "white", color = NA))

  aviso +
    plot_annotation(
      title   = paste("Resumen mensual de Maestro Eco Extendido ·", nombre_mes),
      caption = "Datos: q haces leyendo esto?",
      theme = theme(
        plot.title      = element_text(face = "bold", size = 18,
                                       hjust = 0.5, color = "#222222",
                                       margin = margin(b = 20)),
        plot.caption    = element_text(size = 7, color = "#999999",
                                       hjust = 1),
        plot.background = element_rect(fill = "white", color = NA),
        plot.margin     = margin(12, 12, 8, 12)
      )
    )
}


# ── 5. FUNCIÓN PRINCIPAL: genera el panel completo de un mes ──
generar_panel_mes <- function(mes, anio, inat) {

  inat_mes       <- inat |> filter(mes_obs == mes, anio_obs == anio)
  inat_historico <- inat |> filter(!(mes_obs == mes & anio_obs == anio))
  inat_mes_anio_anterior <- inat |> filter(mes_obs == mes, anio_obs == anio - 1)

  nombre_mes <- format(as.Date(paste(anio, mes, "01", sep = "-")), "%B %Y")

  if (nrow(inat_mes) == 0) {
    message("  · ", nombre_mes, ": sin observaciones — generando figura vacía")
    return(generar_panel_vacio(nombre_mes))
  }


  # ── GRÁFICO 1: DONUT de grupos taxonómicos ──────────────────
  taxa_counts <- inat_mes |>
    mutate(iconic_taxon_name = limpiar_taxon(iconic_taxon_name)) |>
    count(iconic_taxon_name, name = "n") |>
    arrange(desc(n)) |>
    mutate(
      pct       = n / sum(n),
      ymax      = cumsum(pct),
      ymin      = lag(ymax, default = 0),
      pos_label = (ymax + ymin) / 2
    )

  total_obs <- sum(taxa_counts$n)

  p1 <- ggplot(taxa_counts,
               aes(ymax = ymax, ymin = ymin,
                   xmax = 4,   xmin = 2.5,
                   fill = iconic_taxon_name)) +
    geom_rect(color = "white", linewidth = 0.4) +
    annotate("text", x = 0, y = 0,
             label    = paste0(total_obs, "\nobs."),
             size     = 5.5,
             fontface = "bold",
             color    = "#333333",
             hjust    = 0.5, vjust = 0.5) +
    coord_polar(theta = "y") +
    xlim(c(0, 4)) +
    scale_fill_manual(values = inat_colores, name = NULL) +
    theme_void() +
    theme(
      legend.position  = "right",
      legend.key.size  = unit(0.45, "cm"),
      legend.text      = element_text(size = 9, color = "#333333"),
      plot.title       = element_text(face = "bold", size = 14,
                                      hjust = 0.5, color = "#333333",
                                      margin = margin(b = 6)),
      plot.background  = element_rect(fill = "white", color = NA)
    ) +
    labs(title = "Grupos taxonómicos")


  # ── GRÁFICO 2: CONTADOR de especies ─────────────────────────
  n_especies <- inat_mes |>
    filter(!is.na(taxon_species_name)) |>
    distinct(taxon_species_name) |>
    nrow()

  ids_historicas <- inat_historico |>
    filter(!is.na(taxon_species_name)) |>
    distinct(taxon_species_name) |>
    pull(taxon_species_name)

  ids_mes <- inat_mes |>
    filter(!is.na(taxon_species_name)) |>
    distinct(taxon_species_name) |>
    pull(taxon_species_name)

  n_nuevas <- sum(!ids_mes %in% ids_historicas)

  texto_nuevas <- if (length(ids_historicas) > 0 && n_nuevas > 0) {
    paste0("✦ ", n_nuevas, " nuevas para el proyecto")
  } else if (length(ids_historicas) == 0) {
    "(sin histórico disponible para comparar)"
  } else {
    "sin especies nuevas este mes"
  }

  n_especies_anio_anterior <- inat_mes_anio_anterior |>
    filter(!is.na(taxon_species_name)) |>
    distinct(taxon_species_name) |>
    nrow()

  nombre_mes_anio_anterior <- format(
    as.Date(paste(anio - 1, mes, "01", sep = "-")),
    "%B %Y"
  )

  hay_datos_anio_anterior <- nrow(inat_mes_anio_anterior) > 0

  texto_comparativa <- if (!hay_datos_anio_anterior) {
    paste0("(sin datos de ", nombre_mes_anio_anterior, " para comparar)")
  } else {
    diferencia <- n_especies - n_especies_anio_anterior
    simbolo    <- if (diferencia > 0) "▲" else if (diferencia < 0) "▼" else "■"
    if (diferencia == 0) {
      paste0(simbolo, " mismas especies que en ", nombre_mes_anio_anterior,
             " (", n_especies_anio_anterior, ")")
    } else {
      verbo <- if (diferencia > 0) "más" else "menos"
      paste0(simbolo, " ", abs(diferencia), " ", verbo, " que en ",
             nombre_mes_anio_anterior, " (", n_especies_anio_anterior, ")")
    }
  }

  color_comparativa <- if (!hay_datos_anio_anterior) {
    "#999999"
  } else if (n_especies > n_especies_anio_anterior) {
    "#73AC13"
  } else if (n_especies < n_especies_anio_anterior) {
    "#DC143C"
  } else {
    "#777777"
  }

  p2 <- ggplot() +
    theme_void() +
    annotate("text", x = 0.5, y = 0.70,
             label    = n_especies,
             size     = 22,
             fontface = "bold",
             color    = "#73AC13",
             hjust    = 0.5) +
    annotate("text", x = 0.5, y = 0.51,
             label = "especies\nobservadas",
             size  = 4.5,
             color = "#555555",
             hjust = 0.5) +
    annotate("segment", x = 0.2, xend = 0.8, y = 0.38, yend = 0.38,
             color = "#DDDDDD", linewidth = 0.8) +
    annotate("text", x = 0.5, y = 0.29,
             label    = texto_nuevas,
             size     = 5,
             fontface = "italic",
             color    = "#1E90FF",
             hjust    = 0.5) +
    annotate("text", x = 0.5, y = 0.15,
             label    = texto_comparativa,
             size     = 4.3,
             fontface = "italic",
             color    = color_comparativa,
             hjust    = 0.5) +
    xlim(0, 1) + ylim(0, 1) +
    theme(
      plot.title      = element_text(face = "bold", size = 14,
                                     hjust = 0.5, color = "#333333",
                                     margin = margin(b = 6)),
      plot.background = element_rect(fill = "white", color = NA)
    ) +
    labs(title = "Diversidad del mes")


  # ── GRÁFICO 3: TOP 5 especies más observadas ────────────────
  top3 <- inat_mes |>
    filter(!is.na(taxon_species_name)) |>
    mutate(iconic_taxon_name = limpiar_taxon(iconic_taxon_name)) |>
    group_by(taxon_species_name, iconic_taxon_name) |>
    summarise(n = n(), .groups = "drop") |>
    arrange(desc(n)) |>
    slice_head(n = 5) |>
    mutate(
      etiqueta = taxon_species_name,
      etiqueta = factor(etiqueta, levels = rev(etiqueta))
    )

  p3 <- ggplot(
    top3,
    aes(x = n, y = etiqueta, fill = iconic_taxon_name)
  ) +
    geom_col(width = 0.65, alpha = 1) +
    geom_text(
      aes(label = etiqueta, x = n * 0.03),
      hjust      = 0,
      size       = 5.2,
      color      = "white",
      fontface   = "italic",
      lineheight = 1
    ) +
    geom_text(
      aes(label = n),
      hjust     = -0.25,
      fontface  = "bold",
      size      = 4.5,
      color     = "#333333"
    ) +
    scale_fill_manual(values = inat_colores) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.20))) +
    theme_minimal() +
    theme(
      legend.position  = "none",
      axis.title       = element_blank(),
      axis.text.y      = element_blank(),
      axis.text.x      = element_blank(),
      axis.ticks       = element_blank(),
      panel.grid       = element_blank(),
      plot.title = element_text(
        face = "bold", size = 15, hjust = 0.5,
        color = "#333333", margin = margin(b = 6)
      ),
      plot.background = element_rect(fill = "white", color = NA)
    ) +
    labs(title = "Especies más observadas")


  # ── GRÁFICO 4: MAPA de distribución geográfica ──────────────
  obs_geo <- inat_mes |>
    filter(!is.na(latitude), !is.na(longitude)) |>
    mutate(iconic_taxon_name = limpiar_taxon(iconic_taxon_name))

  if (nrow(obs_geo) > 0) {
    cx <- mean(c(min(obs_geo$longitude), max(obs_geo$longitude)))
    cy <- mean(c(min(obs_geo$latitude),  max(obs_geo$latitude)))

    rango_x <- (max(obs_geo$longitude) - min(obs_geo$longitude)) * 0.6 + 0.5
    rango_y <- (max(obs_geo$latitude)  - min(obs_geo$latitude))  * 0.6 + 0.5

    radio <- max(rango_x, rango_y)

    xlim_mapa <- c(cx - radio, cx + radio)
    ylim_mapa <- c(cy - radio, cy + radio)

    p4 <- ggplot() +
      geom_sf(data = mundo,
              fill      = "#EFEFEF",
              color     = "#C0C0C0",
              linewidth = 0.25) +
      geom_point(data = obs_geo,
                 aes(x = longitude, y = latitude,
                     color = iconic_taxon_name),
                 alpha = 0.75,
                 size  = 1.8) +
      scale_color_manual(values = inat_colores, name = NULL) +
      coord_sf(xlim = xlim_mapa, ylim = ylim_mapa, expand = FALSE) +
      theme_void() +
      theme(
        legend.position  = "none",
        plot.title       = element_text(face = "bold", size = 15,
                                        hjust = 0.5, color = "#333333",
                                        margin = margin(b = 6)),
        plot.background  = element_rect(fill = "white",   color = NA),
        panel.background = element_rect(fill = "#C8E6F5", color = NA),
        panel.border     = element_rect(fill = NA, color = "#BBBBBB",
                                        linewidth = 0.4),
        aspect.ratio     = 1
      ) +
      labs(title = "Distribución geográfica")
  } else {
    p4 <- ggplot() +
      theme_void() +
      annotate("text", x = 0.5, y = 0.5,
               label = "sin coordenadas\nválidas este mes",
               size = 5, color = "#AAAAAA", hjust = 0.5) +
      xlim(0, 1) + ylim(0, 1) +
      theme(
        plot.title      = element_text(face = "bold", size = 15,
                                       hjust = 0.5, color = "#333333",
                                       margin = margin(b = 6)),
        plot.background = element_rect(fill = "white", color = NA),
        aspect.ratio    = 1
      ) +
      labs(title = "Distribución geográfica")
  }


  # ── COMPOSICIÓN DEL PANEL 2×2 ────────────────────────────────
  (p1 | p2) / (p3 | p4) +
    plot_layout(widths = c(1, 1), heights = c(1, 1)) +
    plot_annotation(
      title    = paste("Resumen mensual de Maestro Eco Extendido ·", nombre_mes),
      caption  = "Datos: q haces leyendo esto?",
      theme = theme(
        plot.title      = element_text(face = "bold", size = 18,
                                       hjust = 0.5, color = "#222222",
                                       margin = margin(b = 20)),
        plot.caption    = element_text(size = 7, color = "#999999",
                                       hjust = 1),
        plot.background = element_rect(fill = "white", color = NA),
        plot.margin     = margin(12, 12, 8, 12)
      )
    )
}


# ── 6. BUCLE: generar y exportar un panel por cada mes ───────
if (!dir.exists("figures")) dir.create("figures", recursive = TRUE)

meses_vacios <- character(0)

for (i in seq_len(nrow(lista_mes_anio))) {

  mes_i  <- lista_mes_anio$mes[i]
  anio_i <- lista_mes_anio$anio[i]

  panel_i <- generar_panel_mes(mes_i, anio_i, inat)

  nombre_archivo <- paste0("figures/panel_inat_",
                           sprintf("%04d", anio_i), "_",
                           sprintf("%02d", mes_i), ".png")

  ggsave(nombre_archivo, panel_i,
         width  = 10,
         height = 10,
         dpi    = 150,
         bg     = "white")

  message("✔ Panel guardado como '", nombre_archivo, "'")

  if (nrow(filter(inat, mes_obs == mes_i, anio_obs == anio_i)) == 0) {
    meses_vacios <- c(meses_vacios, format(lista_mes_anio$fecha[i], "%B %Y"))
  }
}

message("\n========================================")
message("Listo: ", nrow(lista_mes_anio), " paneles generados en 'figures/'.")
if (length(meses_vacios) > 0) {
  message("Meses sin observaciones (figura vacía generada): ",
          paste(meses_vacios, collapse = ", "))
}
message("========================================")

# Copia del panel más reciente con nombre fijo (para mostrarlo en el README)
ultimo <- tail(sort(list.files("figures",
                               pattern = "^panel_inat_\\d{4}_\\d{2}\\.png$",
                               full.names = TRUE)), 1)
file.copy(ultimo, "figures/ultimo_panel.png", overwrite = TRUE)
