# Dr. Byron González
# http://byrong.cc

if(!require(readxl)){install.packages("readxl")}
if(!require(stats)){install.packages("stats")}
if(!require(ggstatsplot)){install.packages("ggstatsplot")}
if(!require(PMCMRplus)){install.packages("PMCMRplus")}
if(!require(FSA)){install.packages("FSA")}
if(!require(rcompanion)){install.packages("rcompanion")}

# Importar la tabla "barrenador" ------
barr<-read_excel("barrenador.xlsx")
head(barr)

# Calcular el estadístico de prueba

resultado <- kruskal.test(capturados ~ distancia, data = barr); resultado

# Prueba de Dunn para comparaciones múltiples
dunn_resultado <- dunnTest(capturados ~ distancia,
                           data = barr,
                           method = "bh")

# Extraer la tabla de comparaciones en pares
tabla_comparaciones <- dunn_resultado$res

# Generar las letras 
cartas_significancia <- cldList(P.adj ~ Comparison,
                                data = tabla_comparaciones, 
                                threshold = 0.05,
                                remove.zero = FALSE) # <--- ESTO EVITA EL ERROR

# Formatear la salida estilo "Prueba de Tukey"
# Calcular las medianas de capturas para cada distancia
medianas <- aggregate(capturados ~ distancia, data = barr, FUN = median)
colnames(medianas) <- c("Group", "Mediana")

# Fusionar las tablas por la columna común "Group"
tabla_estilo_tukey <- merge(medianas, cartas_significancia, by = "Group")

# Ordenar de mayor a menor según la Mediana y seleccionar columnas limpias
tabla_estilo_tukey <- tabla_estilo_tukey[order(-tabla_estilo_tukey$Mediana), c("Group", "Mediana", "Letter")]

# Imprimir el resultado final ordenado
print(tabla_estilo_tukey)

## Resolución mediante ggstatsplot ------
windows(11,11)
ggbetweenstats(
  data = barr,
  x = distancia,
  y = capturados,
  type = "nonparametric", # ANOVA para Kruskall-Wallis
  plot.type = "box",
  pairwise.comparisons = T,
  pairwise.display = "significant",
  centrality.plotting = T,
  bf.message = FALSE
)
