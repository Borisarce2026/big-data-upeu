
############################################################
# EJERCICIO 1. ANÁLISIS EXPLORATORIO DE DATOS (EDA)
# Proyecto: Simulación de tráfico y movilidad urbana
# Entorno: R 4.5.2
############################################################

# ==========================================================
# 0. PREPARACIÓN Y VALIDACIÓN DEL CONJUNTO DE DATOS
# ==========================================================

library(dplyr)
library(ggplot2)

if (!exists("datos") || !is.data.frame(datos)) {
  stop("No existe el objeto 'datos' o no es un data frame.")
}

variables_requeridas <- c(
  "tiempo", "punto", "intervalo", "hora",
  "intensidad", "vehiculos", "velocidad_kmh", "congestion"
)

variables_faltantes <- setdiff(variables_requeridas, names(datos))

if (length(variables_faltantes) > 0) {
  stop(
    "Faltan variables requeridas: ",
    paste(variables_faltantes, collapse = ", ")
  )
}

# Directorio para conservar los resultados del EDA
directorio_resultados <- "resultados_eda"

if (!dir.exists(directorio_resultados)) {
  dir.create(directorio_resultados, recursive = TRUE)
}

# ==========================================================
# 1. CARACTERIZACIÓN GENERAL DE LOS DATOS
# ==========================================================

cat("\n============================================\n")
cat("1. CARACTERIZACIÓN DEL CONJUNTO DE DATOS\n")
cat("============================================\n")

cat("\nNúmero de observaciones:", nrow(datos), "\n")
cat("Número de variables:", ncol(datos), "\n")

cat("\nNombres de las variables:\n")
print(names(datos))

cat("\nEstructura y tipos de datos:\n")
str(datos)

cat("\nResumen estadístico general:\n")
print(summary(datos))

cat("\nPrimeras seis observaciones:\n")
print(head(datos))

cat("\nNúmero de valores distintos por variable:\n")
print(sapply(datos, function(x) length(unique(x))))


# ==========================================================
# 2. EVALUACIÓN DE LA CALIDAD DE LOS DATOS
# ==========================================================

cat("\n============================================\n")
cat("2. CALIDAD DE LOS DATOS\n")
cat("============================================\n")

# Valores ausentes por variable
faltantes <- data.frame(
  variable = names(datos),
  cantidad = colSums(is.na(datos)),
  porcentaje = round(
    colMeans(is.na(datos)) * 100, 2
  )
)

cat("\nValores ausentes por variable:\n")
print(faltantes)

# Registros completamente duplicados
cat("\nRegistros completamente duplicados:\n")
cat(sum(duplicated(datos)), "\n")

# Combinaciones repetidas de tiempo y punto de medición
if (all(c("tiempo", "punto") %in% names(datos))) {
  cat("\nCombinaciones repetidas de tiempo y punto:\n")
  cat(
    sum(duplicated(datos[c("tiempo", "punto")])),
    "\n"
  )
}

# Verificación de valores no finitos en variables numéricas
variables_numericas <- names(
  datos[vapply(datos, is.numeric, logical(1))]
)

if (length(variables_numericas) > 0) {
  valores_no_finitos <- sapply(
    datos[variables_numericas],
    function(x) sum(!is.finite(x) & !is.na(x))
  )
  
  cat("\nValores no finitos en variables numéricas:\n")
  print(valores_no_finitos)
}

# Comprobación de rangos y valores negativos
if ("vehiculos" %in% names(datos)) {
  cat("\nRegistros con número negativo de vehículos:\n")
  cat(sum(datos$vehiculos < 0, na.rm = TRUE), "\n")
}

if ("velocidad_kmh" %in% names(datos)) {
  cat("\nRegistros con velocidad negativa:\n")
  cat(sum(datos$velocidad_kmh < 0, na.rm = TRUE), "\n")
}


# ==========================================================
# 3. ANÁLISIS DESCRIPTIVO DE VARIABLES NUMÉRICAS
# ==========================================================

cat("\n============================================\n")
cat("3. ESTADÍSTICAS DESCRIPTIVAS\n")
cat("============================================\n")

variables_interes <- c(
  "intervalo", "hora", "intensidad",
  "vehiculos", "velocidad_kmh"
)

variables_interes <- intersect(
  variables_interes, variables_numericas
)

if (length(variables_interes) > 0) {
  
  resumen_numerico <- do.call(
    rbind,
    lapply(variables_interes, function(variable) {
      
      x <- datos[[variable]]
      x <- x[is.finite(x)]
      
      if (length(x) == 0) {
        return(data.frame(
          variable = variable,
          n = 0,
          media = NA_real_,
          desviacion = NA_real_,
          minimo = NA_real_,
          mediana = NA_real_,
          maximo = NA_real_
        ))
      }
      
      data.frame(
        variable = variable,
        n = length(x),
        media = mean(x),
        desviacion = if (length(x) > 1) sd(x) else NA_real_,
        minimo = min(x),
        mediana = median(x),
        maximo = max(x)
      )
    })
  )
  
  resumen_numerico[-1] <- lapply(
    resumen_numerico[-1], round, digits = 2
  )
  
  print(resumen_numerico)
}


# ==========================================================
# 4. DISTRIBUCIONES Y FRECUENCIAS
# ==========================================================

cat("\n============================================\n")
cat("4. DISTRIBUCIONES Y FRECUENCIAS\n")
cat("============================================\n")

cat("\nFrecuencia por punto de medición:\n")
print(table(datos$punto, useNA = "ifany"))

cat("\nFrecuencia por nivel de congestión:\n")
print(table(datos$congestion, useNA = "ifany"))

cat("\nEstadísticas de vehículos:\n")
print(summary(datos$vehiculos))

cat("\nEstadísticas de velocidad:\n")
print(summary(datos$velocidad_kmh))


# ==========================================================
# 5. VISUALIZACIÓN DE RESULTADOS
# ==========================================================

tema_eda <- theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(
      face = "bold", size = 14
    ),
    plot.subtitle = element_text(color = "grey35"),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

# 5.1 Distribución del número de vehículos

grafico_vehiculos <- ggplot(
  datos,
  aes(x = vehiculos)
) +
  geom_histogram(
    bins = 30,
    fill = "#2878B5",
    color = "white",
    na.rm = TRUE
  ) +
  labs(
    title = "Distribución del número de vehículos",
    subtitle = "Observaciones registradas en los puntos de medición",
    x = "Número de vehículos",
    y = "Frecuencia"
  ) +
  tema_eda

print(grafico_vehiculos)

ggsave(
  file.path(directorio_resultados, "01_distribucion_vehiculos.png"),
  grafico_vehiculos, width = 9, height = 6, dpi = 300
)


# 5.2 Distribución de la velocidad

grafico_velocidad <- ggplot(
  datos,
  aes(x = velocidad_kmh)
) +
  geom_histogram(
    bins = 30,
    fill = "#35A779",
    color = "white",
    na.rm = TRUE
  ) +
  labs(
    title = "Distribución de la velocidad",
    subtitle = "Velocidad registrada en kilómetros por hora",
    x = "Velocidad (km/h)",
    y = "Frecuencia"
  ) +
  tema_eda

print(grafico_velocidad)

ggsave(
  file.path(directorio_resultados, "02_distribucion_velocidad.png"),
  grafico_velocidad, width = 9, height = 6, dpi = 300
)

# 5.3 Distribución de vehículos por punto de medición
# Gráfico de violín con mediana y cuartiles

grafico_puntos <- ggplot(
  datos,
  aes(x = punto, y = vehiculos, fill = punto)
) +
  geom_violin(
    trim = FALSE,
    alpha = 0.75,
    color = "#34495E",
    na.rm = TRUE
  ) +
  geom_boxplot(
    width = 0.12,
    fill = "white",
    color = "#34495E",
    outlier.shape = NA,
    na.rm = TRUE
  ) +
  labs(
    title = "Distribución del número de vehículos por punto",
    subtitle = "Densidad, mediana y rango intercuartílico",
    x = "Punto de medición",
    y = "Número de vehículos",
    fill = "Punto"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(
      angle = 0,
      hjust = 0.5
    )
  )

print(grafico_puntos)

ggsave(
  file.path(
    directorio_resultados,
    "03_vehiculos_por_punto.png"
  ),
  plot = grafico_puntos,
  width = 10,
  height = 6,
  dpi = 300
)


# 5.4 Frecuencia por nivel de congestión

frecuencias_congestion <- as.data.frame(
  table(datos$congestion, useNA = "ifany")
)

names(frecuencias_congestion) <- c(
  "nivel_congestion", "frecuencia"
)

grafico_congestion <- ggplot(
  frecuencias_congestion,
  aes(x = nivel_congestion, y = frecuencia)
) +
  geom_col(
    fill = "#D97732",
    width = 0.7
  ) +
  geom_text(
    aes(label = frecuencia),
    vjust = -0.4,
    size = 3.8
  ) +
  labs(
    title = "Distribución de los niveles de congestión",
    x = "Nivel de congestión",
    y = "Número de observaciones"
  ) +
  expand_limits(
    y = max(frecuencias_congestion$frecuencia, 1) * 1.12
  ) +
  tema_eda

print(grafico_congestion)

ggsave(
  file.path(directorio_resultados, "04_niveles_congestion.png"),
  grafico_congestion, width = 9, height = 6, dpi = 300
)


# 5.5 Evolución temporal de la velocidad media

if (inherits(datos$tiempo, c("POSIXct", "POSIXlt", "Date"))) {
  
  velocidad_temporal <- datos %>%
    filter(
      !is.na(tiempo),
      is.finite(velocidad_kmh)
    ) %>%
    group_by(tiempo) %>%
    summarise(
      velocidad_media = mean(velocidad_kmh),
      .groups = "drop"
    ) %>%
    arrange(tiempo)
  
  if (nrow(velocidad_temporal) > 0) {
    
    grafico_temporal <- ggplot(
      velocidad_temporal,
      aes(x = tiempo, y = velocidad_media)
    ) +
      geom_line(
        color = "#2878B5",
        linewidth = 0.7
      ) +
      labs(
        title = "Evolución temporal de la velocidad media",
        x = "Fecha y hora",
        y = "Velocidad media (km/h)"
      ) +
      tema_eda
    
    print(grafico_temporal)
    
    ggsave(
      file.path(directorio_resultados, "05_evolucion_velocidad.png"),
      grafico_temporal, width = 10, height = 6, dpi = 300
    )
  }
}

# ==========================================================
# 6. RELACIONES ENTRE VARIABLES NUMÉRICAS
# ==========================================================

if (all(c("vehiculos", "velocidad_kmh") %in% names(datos))) {
  
  datos_relacion <- datos %>%
    filter(
      is.finite(vehiculos),
      is.finite(velocidad_kmh)
    )
  
  if (nrow(datos_relacion) > 1) {
    
    correlacion <- cor(
      datos_relacion$vehiculos,
      datos_relacion$velocidad_kmh
    )
    
    cat("\nCorrelación entre vehículos y velocidad:\n")
    print(round(correlacion, 4))
    
    grafico_relacion <- ggplot(
      datos_relacion,
      aes(x = vehiculos, y = velocidad_kmh)
    ) +
      geom_point(
        alpha = 0.25,
        color = "#2878B5"
      ) +
      geom_smooth(
        method = "lm",
        formula = y ~ x,
        color = "#D97732",
        se = TRUE
      ) +
      labs(
        title = "Relación entre vehículos y velocidad",
        subtitle = paste(
          "Correlación de Pearson:",
          round(correlacion, 3)
        ),
        x = "Número de vehículos",
        y = "Velocidad (km/h)"
      ) +
      tema_eda
    
    print(grafico_relacion)
    
    ggsave(
      file.path(directorio_resultados, "06_vehiculos_velocidad.png"),
      grafico_relacion, width = 9, height = 6, dpi = 300
    )
  }
}


# ==========================================================
# 7. CIERRE DEL ANÁLISIS
# ==========================================================

cat("\n============================================\n")
cat("EDA FINALIZADO\n")
cat("============================================\n")

cat("Observaciones analizadas:", nrow(datos), "\n")
cat("Variables analizadas:", ncol(datos), "\n")
cat(
  "Resultados gráficos guardados en:",
  normalizePath(directorio_resultados, winslash = "/", mustWork = FALSE),
  "\n"
)
