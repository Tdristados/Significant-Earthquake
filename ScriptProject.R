# librerias
library(tidyverse)
library(GGally)
library(gridExtra)
library(plotly)
library(dplyr)
library(ggplot2)
library(maps)
library(MVN)
library(readr)

#---------------------Importación de la base de datos------------------
terremoto <- read_csv("Dataset_clean.csv", 
                      col_types = cols(Year = col_integer()))
cuantis = c('Year','Depth','Latitude','Longitude','Mag', 'NST')

View(terremoto)
summary(terremoto)


#--------------Graficos de las variables -------------------------# 
# histogramas
#------------------------------------------------------------------------------------------
h1=ggplot(terremoto,aes(x=Year))+geom_histogram(color="red");h1
h2=ggplot(terremoto,aes(x=Longitude))+geom_histogram(color="blue");h2
h3=ggplot(terremoto,aes(x=Latitude))+geom_histogram(color="green");h3
h4=ggplot(terremoto,aes(x=Depth))+geom_histogram(color="pink");h4
h5=ggplot(terremoto,aes(x=Mag))+geom_histogram(color="orange");h5
h6=ggplot(terremoto,aes(x=NST))+geom_histogram(color="yellow");h6
grid.arrange(h1,h2,h3,h4,h5,h6,ncol=2,nrow=4)


g1=ggplot(terremoto)+geom_point(aes(Mag,Year))+theme_bw()
g1

#------------------------------------------------------------------------------------------
# Diagramas BOXPLOTS 

# para la estatura separado por Contiente y medido en Year
b1=ggplot(terremoto,aes(x=Continent,y=Year,fill=Continent))+
  stat_boxplot(geom="errorbar",width=0.25)+
  geom_boxplot()+
  theme(legend.position = "none")
b1

# para la estatura separado por continente y medido en Mag
b2=ggplot(terremoto,aes(x=Continent,y=Mag,fill=Continent))+
  stat_boxplot(geom="errorbar",width=0.25)+
  geom_boxplot()+
  theme(legend.position = "none")
b2

# para la estatura separado por Contiente y medido por NST
b3=ggplot(terremoto,aes(x=Continent,y=NST,fill=Continent))+
  stat_boxplot(geom="errorbar",width=0.25)+
  geom_boxplot()+
  theme(legend.position = "none")
b3

#------------------------------------------------------------------------------------------

# Diagrama de barras para la variable categórica 'Region'
barplot(table(terremoto$Region), main="Cantidad de terremotos por MagSource")

#------------------------------------------------------------------------------------------
# Diagrama circular (de torta) de la cantidad de terremotos medidos a escala Magtype
# Agrupar y contar los tipos de magnitud (MagType)
magtype_counts <- terremoto %>%
  group_by(MagType) %>%
  summarise(count = n()) %>%
  mutate(percentage = round(count / sum(count) * 100, 1),  # Calcular porcentaje
         number = row_number())  # Añadir un número para cada categoría

# Crear las etiquetas que se mostrarán en la leyenda
magtype_counts$label <- paste0(magtype_counts$number, ". ", magtype_counts$percentage, 
                               "% - Escala ", magtype_counts$MagType, 
                               " (", magtype_counts$count, " terremotos)")

# Crear el gráfico de torta con el perímetro negro y letras más grandes en la leyenda
ggplot(magtype_counts, aes(x = "", y = count, fill = factor(number))) +
  geom_bar(stat = "identity", width = 2, color = "black") +  # Añadir perímetro y líneas negras
  coord_polar("y") +  # Para convertirlo en gráfico circular
  geom_text(aes(label = number), position = position_stack(vjust = 0.5), size = 8) +  # Solo números en la torta
  theme_void() +  # Eliminar los ejes
  scale_fill_manual(values = c("#D32F2F", "#1976D2", "#388E3C", "#FBC02D", "#7B1FA2", 
                               "#F57C00", "#0288D1", "#C2185B", "#689F38", "#E64A19", 
                               "#303F9F", "#FFEB3B", "#8E24AA", "#009688", "#D81B60", 
                               "#4CAF50", "#FF5722"), 
                    labels = magtype_counts$label) +  # Etiquetas personalizadas en la leyenda
  guides(fill = guide_legend(title = "Leyenda", ncol = 1)) +  # Leyenda a la derecha
  theme(legend.text = element_text(size = 18),  # Cambiar el tamaño del texto de la leyenda
        legend.title = element_text(size = 20),
        plot.title = element_text(hjust = 0.5)) +  # Cambiar el tamaño del título de la leyenda
  ggtitle("Proporción de Terremotos por MagType")

#------------------------------------------------------------------------------------------

# Agrupar y contar los tipos de magSource
magSource_counts <- terremoto %>%
  group_by(magSource) %>%
  summarise(count = n()) %>%
  mutate(percentage = round(count / sum(count) * 100, 1),  # Calcular porcentaje
         number = row_number())  # Añadir un número para cada categoría

# Crear el gráfico de barras horizontales sin la leyenda y con los números al final de cada barra
ggplot(magSource_counts, aes(x = reorder(magSource, -count), y = count, fill = factor(number))) +
  geom_bar(stat = "identity", color = "black") +  # Barras con borde negro
  geom_text(aes(label = count), hjust = -0.1, size = 4) +  # Añadir número de terremotos al final de cada barra
  coord_flip() +  # Hacer que el gráfico sea horizontal
  theme_minimal() +  # Estilo de gráfico más limpio
  scale_y_continuous(limits = c(0, 15000), expand = c(0, 0)) +  # Ajustar la escala del eje Y para hacerlo más compacto
  scale_fill_manual(values = c("#FF0000", "#00FF00", "#0000FF", "#FFFF00", "#FF00FF", 
                               "#00FFFF", "#800000", "#808000", "#008000", "#800080", 
                               "#008080", "#000080", "#FF4500", "#2E8B57", "#FFD700", 
                               "#DA70D6", "#8B0000", "#00CED1", "#FF1493", "#ADFF2F", 
                               "#4B0082", "#FF6347", "#7CFC00", "#4682B4", "#FF69B4", 
                               "#A52A2A", "#DEB887", "#5F9EA0", "#B8860B", "#D2691E", 
                               "#6495ED", "#DC143C", "#008B8B", "#BDB76B", "#9932CC", 
                               "#8B008B", "#556B2F", "#FF8C00", "#8FBC8F", "#483D8B", 
                               "#9400D3", "#00BFFF", "#FFDAB9", "#CD5C5C", "#4B0082", 
                               "#2F4F4F", "#20B2AA", "#B22222")) +  # 47 colores fuertes
  labs(title = "Cantidad de Terremotos por Fuente (magSource)", x = "Fuente (magSource)", y = "Cantidad de Terremotos") +  # Etiquetas
  theme(legend.position = "none",  # Eliminar la leyenda
        plot.title = element_text(hjust = 0.5, size = 16))  # Centrar y agrandar el título

#------------------------------------------------------------------------------------------
# Cargar librerías necesarias
library(ggplot2)

# Contar la frecuencia de cada tipo de magnitud (MagType)
magtype_counts <- terremoto %>%
  group_by(MagType) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# Crear gráfico de barras horizontales
ggplot(magtype_counts, aes(x = reorder(MagType, count), y = count, fill = MagType)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # Para hacer las barras horizontales
  scale_fill_manual(values = rainbow(n = nrow(magtype_counts))) +  # Colores diferentes para cada barra
  labs(title = "Cantidad de Terremotos por Tipo de Magnitud (MagType)", x = "Tipo de Magnitud (MagType)", y = "Cantidad de Terremotos") +
  theme_minimal()
#--------------------------------------------------------------------------------
# Crear un mapa mundial con los terremotos
world_map <- map_data("world")
ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "lightgray") +
  geom_point(data = terremoto, aes(x = Longitude, y = Latitude, color = Mag), size = 2) +
  ggtitle("Distribución de Terremotos en el Mundo") +
  labs(x = "Longitud", y = "Latitud") +
  theme_minimal()
#------------------------------------------------------------------------------------------
#Pruebas de hipótesis 

n = nrow(Variables_cuantitativas);n
vm = colMeans(Variables_cuantitativas);vm
s = cov(Variables_cuantitativas);s
alpha = 0.05
mu_0 = c(5.5,66.35)
t1 = (vm[5]-mu_0[1])/(sqrt(s[5,5])/sqrt(n));t1
t2 = (vm[2]-mu_0[2])/(sqrt(s[2,2])/sqrt(n));t2

vc_t1 = qt(1-alpha/2, df = n-1, lower.tail = TRUE);vc_t1
vc_t2 = qt(1-alpha, df = n-1, lower.tail = TRUE);vc_t2

## t1 = 75.01 >> vct1 = 1.9
## t2 = 0.0032 < vct = 1.65
#------------------------------------------------------------------------------------------
# Intervalos de confianza

asia_data <- subset(terremoto, Continent == "Asia")
europe_data <- subset(terremoto, Continent == "Europe")

# Calcular medias y desviaciones estándar
mean_asia <- mean(asia_data$Depth, na.rm = TRUE)
mean_europe <- mean(europe_data$Depth, na.rm = TRUE)
sd_asia <- sd(asia_data$Depth, na.rm = TRUE)
sd_europe <- sd(europe_data$Depth, na.rm = TRUE)

# Tamaños de las muestras
n_asia <- nrow(asia_data)
n_europe <- nrow(europe_data)

# Nivel de confianza (95%)
alpha <- 0.05
z <- qnorm(1 - alpha/2)

# Calcular el error estándar
se_diff <- sqrt((sd_asia^2 / n_asia) + (sd_europe^2 / n_europe))

# Intervalo de confianza
lower_bound <- (mean_asia - mean_europe) - z * se_diff
upper_bound <- (mean_asia - mean_europe) + z * se_diff

# Mostrar resultados
cat("Intervalo de confianza para la diferencia de profundidades (Asia - Europa): [", lower_bound, ",", upper_bound, "]\n")

#------------------------------------------------------------------------------------------
#Pruebas de hipótesis 

#---------------------------------------------------#
# PRUEBA DE HIPÓTESIS DE NORMALIDAD CASO UNIVARIADO #
#---------------------------------------------------#

Variables_cuantitativas <- terremoto[,cuantis]

# Gráficas de normalidad
qq_plot <- mvn(data = Variables_cuantitativas,
               mvnTest = "mardia",
               univariatePlot = 'qqplot')  # Desactiva los gráficos Q-Q
#Limpiar caché :v
gc()

#-----------Pruebas Univariadas-----------
# Anderson
pn_AD=mvn(data=Variables_cuantitativas,
          univariateTest= "AD",
          desc = TRUE);pn_AD

# Kolmogorov smirnov
pn_KS=mvn(data=Variables_cuantitativas,
          univariateTest= "Lillie",
          desc = TRUE);pn_KS


#-----------Pruebas Multivariadas-----------
pnm = mvn(data=Variables_cuantitativas,
          mvnTest='mardia')
pnm$multivariateNormality
#-------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------

sd = sd(terremoto$Longitude, na.rm = TRUE);sd

summary(terremoto)
# Cargar librerías necesarias
library(ggplot2)
library(vcd)  # Para gráficos de mosaico

# Crear tabla de contingencia entre dos variables cualitativas
tabla_mosaico <- table(terremoto$Continent, terremoto$MagType)

# Crear diagrama de mosaico
mosaic(~ Continent + MagType, data = terremoto,
       main = "Diagrama de Mosaico entre Continente y Tipo de Magnitud",
       shade = TRUE, legend = TRUE)

#----------------------------------------------------------------------

#########################################################
##                    PCA MODEL                        ##
#########################################################

library(readr)
terremoto <- read_csv("Dataset_clean.csv", 
                      col_types = cols(Year = col_integer()))


numeric_data <- terremoto[, c("Longitude", "Latitude", "Depth", "Mag", "NST")]
cor_numeric <- cor(numeric_data)
cor_matrix <- melt(cor_numeric)


# Métricas de calidad
KMO(cor_numeric)  # 0.42
cortest.bartlett(cor_numeric, n=3152) # p-value = 0


#---------------------------------------------------------------------
# Heap map de correlaciones
# Crear el heatmap con números
ggplot(cor_matrix, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +  # Dibujar los cuadros del heatmap
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1, 1), space = "Lab",
                       name = "Correlación") +
  geom_text(aes(label = round(value, 2)), color = "black", size = 4) +  # Agregar números
  theme_minimal() +  # Tema limpio
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +  # Rotar etiquetas
  labs(title = "Mapa de Calor de Correlaciones", x = "", y = "") +
  coord_fixed()  # Asegurar proporciones iguales

#---------------------------------------------------------------------
# PCA #

terremoto.pca <- PCA(numeric_data, scale.unit = TRUE, graph = FALSE)

# Cargas de las componentes principales
terremoto.pca$var$coord


var_exp <- terremoto.pca$eig[,2]/100
# Crear un data frame para el gráfico
df_var_exp <- data.frame(
  Componente = 1:length(var_exp),
  Varianza_Explicada = var_exp
)

# Generar gráfico de la varianza en un heapMap
ggplot(df_var_exp, aes(x = Componente, y = Varianza_Explicada)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  geom_text(aes(label = round(Varianza_Explicada, 2)), vjust = -0.5) +
  labs(
    title = "Varianza Explicada por Componente Principal",
    x = "Componente Principal",
    y = "Varianza Explicada"
  ) +
  theme_minimal()

#-----------------------------------------------
# Realizar análisis PCA
data.pca <- PCA(numeric_data, scale.unit = TRUE, graph = FALSE)

# Crear biplot (variables y observaciones)
fviz_pca_var(data.pca, 
             col.var = "contrib", # Color según la contribución de las variables
             gradient.cols = c("blue", "purple", "red"),
             repel = TRUE) +      # Evitar superposición de etiquetas
  ggtitle("Scatter plot Componentes principales") +
  theme_minimal()


#----------------------------------------------------------------
# Gráfico 3D para tomar 3 Dimensiones
var_coords <- as.data.frame(data.pca$var$coord)

fig <- plot_ly(
  x = var_coords$Dim.1, 
  y = var_coords$Dim.2, 
  z = var_coords$Dim.3,
  text = rownames(var_coords), # Nombres de las variables
  type = "scatter3d", 
  mode = "markers+text",
  marker = list(size = 5, color = "red"),
  textposition = "top center"
) %>%
  layout(
    scene = list(
      xaxis = list(title = "PC1"),
      yaxis = list(title = "PC2"),
      zaxis = list(title = "PC3")
    ),
    title = "PCA 3D - Variables"
  )
fig # Mostrar el gráfico


#----------------------------------------------------------------
# Realizar PCA

pca <- prcomp(numeric_data, center = TRUE, scale. = TRUE)
pca_data <- as.data.frame(pca$x)  # Componentes principales
pca$rotation

# Agregar la columna de Continentes al resultado del PCA
pca_data$Continent <- terremoto$Continent

# Crear el gráfico
ggplot(pca_data, aes(x = PC1, y = PC2, color = Continent)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
  labs(
    title = "Scatter Plot por Continente",
    x = "PC1",
    y = "PC2",
    color = "Continente"
  ) +
  theme_minimal() +
  scale_color_brewer(palette = "Set1")  # Paleta similar a Set1 de seaborn
#---------------------------------------------------------------------------
