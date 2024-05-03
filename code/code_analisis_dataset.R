setwd("/Users/benjamin/Documents")
data <- read.csv("hotel_bookings.csv", na.strings = "" )

#LIBRERIAS 

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}

if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}

library(dplyr)
library(ggplot2)

#----------------------------------LIMPIEZA DE DATOS--------------------------------------#
#se selecciona solo las columnas que usaremos para resolver las preguntas
data <- data[, c('arrival_date_year', 'arrival_date_month', 'arrival_date_day_of_month', 
                 'hotel', 'is_canceled', 'children', 'babies', 
                 'required_car_parking_spaces', 'reserved_room_type')]
print(data)

# se convierte todos los valores numericos a usar en enteros 
data$arrival_date_year <- as.integer(data$arrival_date_year)
data$arrival_date_day_of_month <- as.integer(data$arrival_date_day_of_month)
data$children <- as.integer(data$children)
data$babies <- as.integer(data$babies)
data$required_car_parking_spaces <- as.integer(data$required_car_parking_spaces)


#se limpian los valores nulos en nuestro dataset
data <- na.omit(data)

#se verifica que no quede ningun valor nulo en las columnas
nulos_por_columna <- colSums(is.na(data))
print(nulos_por_columna)

#se obtiene los valores atipicos con la desviacion estandar en la columna children
mean_children <- mean(data$children)
sd_children <- sd(data$children)
print(sd_children)
# Definir un umbral (por ejemplo, 10 veces la desviación estándar)
threshold <- 10

# Encontrar valores atípicos
outliers <- data$children[abs(data$children - mean_children) > threshold * sd_children]

# Mostrar los valores atípicos
print(outliers)
#se borran los valores atipicos
data <- data[!data$children %in% outliers, ]


#se procede a hacer lo mismo con la columnas babies

mean_babies <- mean(data$babies)
sd_babies <- sd(data$babies)
print(sd_babies)

# Definir un umbral (por ejemplo, 10 veces la desviación estándar)
threshold <- 20

# Encontrar valores atípicos
outliers_babies <- data$babies[abs(data$babies - mean_babies) > threshold * sd_babies]

# Mostrar los valores atípicos
print(outliers_babies)

# Se eliminan los valores atípicos
data <- data[!data$babies %in% outliers_babies, ]


#------------------------------------GRAFICACION DE DATOS-----------------------------------------#

#--------------------------------------------------------Pregunta 1:

#Encontrar el hotel con mayor cantidad de reservas tomando en cuenta las cancelaciones: 

reservas_totales <- data %>%
  group_by(hotel) %>%
  summarise(total_reservas = n())

# Calcular el número de reservas canceladas por tipo de hotel
reservas_canceladas <- data %>%
  filter(is_canceled == 1) %>%
  group_by(hotel) %>%
  summarise(total_canceladas = n())

# Combinar los datos de reservas totales y canceladas
reservas_diferencia <- left_join(reservas_totales, reservas_canceladas, by = "hotel")

# Calcular la diferencia entre reservas totales y canceladas
reservas_diferencia <- reservas_diferencia %>%
  mutate(diferencia = total_reservas - total_canceladas)

# Crear el gráfico de barras
ggplot(reservas_diferencia, aes(x = hotel, y = diferencia, fill = hotel)) +
  geom_bar(stat = "identity") +
  labs(title = "Diferencia entre Reservas vigentes por Tipo de Hotel",
       x = "Tipo de Hotel",
       y = "Diferencia") +
  theme_minimal()


#--------------------------------------------------------Pregunta 2:


ggplot(reservas_por_año, aes(x = arrival_date_year, y = total_reservas, color = hotel)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Tendencia de la Demanda a lo largo de los Años",
       x = "Año",
       y = "Número Total de Reservas",
       color = "Tipo de Hotel") +
  theme_minimal()


#--------------------------------------------------------Pregunta 3:
estaciones <- data.frame(
  mes = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"),
  temporada = c(rep("Invierno", 3), rep("Primavera", 3), rep("Verano", 3), rep("Otoño", 3))
)

# Unir datos de estaciones con los datos originales
data <- left_join(data, estaciones, by = c("arrival_date_month" = "mes"))


# Calcular el número total de reservas por temporada
reservas_temporada <- data %>%
  group_by(temporada) %>%
  summarise(total_reservas = n())


# Calcular el promedio de reservas por temporada
promedio_reservas_temporada <- reservas_temporada %>%
  mutate(promedio_reservas = total_reservas / n_distinct(data$arrival_date_year))

# Crear el gráfico de barras
ggplot(reservas_temporada, aes(x = temporada, y = total_reservas, fill = temporada)) +
  geom_bar(stat = "identity") +
  labs(title = "Cantidad de Reservas por Estación",
       x = "Estación",
       y = "Cantidad de Reservas") +
  theme_minimal()


#--------------------------------------------------------Pregunta 4:
# Calcular el número total de reservas por mes
reservas_por_mes <- data %>%
  group_by(arrival_date_month) %>%
  summarise(total_reservas = n())

# Ordenar los meses según el número de reservas de menor a mayor
meses_ordenados <- reservas_por_mes %>%
  arrange(total_reservas) %>%
  pull(arrival_date_month)

# Convertir la columna arrival_date_month a factor con el orden especificado
data$arrival_date_month <- factor(data$arrival_date_month, levels = meses_ordenados)

# Recalcular el número total de reservas por mes con los meses ordenados
reservas_por_mes <- data %>%
  group_by(arrival_date_month) %>%
  summarise(total_reservas = n())

# Crear el gráfico de barras
ggplot(reservas_por_mes, aes(x = arrival_date_month, y = total_reservas, fill = arrival_date_month)) +
  geom_bar(stat = "identity") +
  labs(title = "Cantidad de Reservas por Mes (Ordenados por Número de Reservas)",
       x = "Mes",
       y = "Número Total de Reservas") +
  theme_minimal()


#--------------------------------------------------------Pregunta 5:

reservas_niños <- sum(data$children > 0)
reservas_bebes <- sum(data$babies > 0)
reservas_ambos <- sum(data$children > 0 & data$babies > 0)
reservas_ninguno <- sum(data$children == 0 & data$babies == 0)

# Crear un data frame con los conteos
conteos <- data.frame(
  Categoria = c("Niños", "Bebés", "Niños y Bebés", "Ninguno"),
  Reservas = c(reservas_niños, reservas_bebes, reservas_ambos, reservas_ninguno)
)

pie_chart <- ggplot(conteos, aes(x = "", y = Reservas, fill = Categoria)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Proporción de Reservas que Incluyen Niños y/o Bebés",
       fill = "Categoría") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Imprimir el gráfico de pastel y las cantidades
print(pie_chart)
print(conteos)


#--------------------------------------------------------Pregunta 6:

reservas_con_parking <- sum(data$required_car_parking_spaces > 0)
reservas_sin_parking <- sum(data$required_car_parking_spaces == 0)

# Crear un data frame con los conteos
conteo_parking <- data.frame(
  Uso_Parking = c("Sí", "No"),
  Reservas = c(reservas_con_parking, reservas_sin_parking)
)

# Crear el gráfico de barras
ggplot(conteo_parking, aes(x = Uso_Parking, y = Reservas, fill = Uso_Parking)) +
  geom_bar(stat = "identity") +
  labs(title = "Importancia de Contar con Espacios de Estacionamiento",
       x = "Uso de Estacionamiento",
       y = "Cantidad de Reservas") +
  theme_minimal()


#--------------------------------------------------------Pregunta 7:

# Calcular el número de cancelaciones de reservas por mes
cancelaciones_por_mes <- data %>%
  filter(is_canceled == 1) %>%
  group_by(arrival_date_month) %>%
  summarise(total_cancelaciones = n())

# Ordenar los meses de menor a mayor por el número total de cancelaciones
cancelaciones_por_mes <- cancelaciones_por_mes[order(cancelaciones_por_mes$total_cancelaciones), ]

# Convertir los nombres de los meses en un factor y ordenarlos
meses_ordenados <- factor(cancelaciones_por_mes$arrival_date_month, levels = unique(cancelaciones_por_mes$arrival_date_month))

# Crear el gráfico de barras
ggplot(cancelaciones_por_mes, aes(x = meses_ordenados, y = total_cancelaciones, fill = meses_ordenados)) +
  geom_bar(stat = "identity") +
  labs(title = "Cantidad de Cancelaciones de Reservas por Mes",
       x = "Mes",
       y = "Cantidad de Cancelaciones") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  



#--------------------------------------------------------Pregunta 8:
head(data)
reservas_por_tipo_habitacion <- data %>%
  group_by(arrival_date_month, reserved_room_type) %>%
  summarise(total_reservas = n())

# Ordenar los meses cronológicamente
meses_ordenados <- factor(reservas_por_tipo_habitacion$arrival_date_month, levels = month.name)

# Crear el gráfico de barras
ggplot(reservas_por_tipo_habitacion, aes(x = meses_ordenados, y = total_reservas, fill = reserved_room_type)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Cantidad de Reservas por Tipo de Habitación y Mes",
       x = "Mes",
       y = "Cantidad de Reservas",
       fill = "Tipo de Habitación") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
