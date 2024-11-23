setwd("D:/andres/Macc/AED/Proyecto")

# Importamos la base de datos

library(readr)
datos<- read_csv("Significant Earthquake Dataset 1900-2023.csv")

###############################################################################
# Normalizamos y estandarizamos (Incluimos y sacamos variables a conveniencia)#
###############################################################################

# Extraemos el año y lo incluimos en los datos
datos$year <- substr(datos$Time, 1, 4)

# Lo mismo para las regiones
#creamos una columna llamada Region para simplificar la información de Place
datos$Region <- sub(".*,(.*)", "\\1", datos$Place)
datos$Region <- trimws(datos$Region)
datos$Region[datos$Region== ""] <- datos$Place[datos$Region == ""]

# Crear función para modificar la columna "Region" quitando lo que esté antes del último guion
remove_before_last_dash <- function(data) {
  data$Region <- gsub(".*-", "", data$Region)
  return(data)
}

datos <- remove_before_last_dash(datos)

# Eliminar tuplas que contienen las cadenas '1', '1960 Great Chilean Earthquake (Valdivia Earthquake)' y 'AK' en la columna "Region"
datos <- datos[!(datos$Region %in% c('1', '1960 Great Chilean Earthquake (Valdivia Earthquake)', 'AK','Andaman Islands Earthquake', '1906 San Francisco Earthquake')),]

# Se eliminan las palabras mencionadas de la columna "Region"
datos$Region <- gsub("(north|south|of|west|east|border|southern|ridge|the|central|off|coast|northern|southeast|northwest|northeastern|Earthquake|northeast|near|eastern|northwestern|southwest|southwestern|western|region|U.S.)\\s",
                     "", datos$Region,
                     ignore.case = TRUE)

datos$Region <- ifelse(grepl("region$", datos$Region),
                       gsub(" region$", "", datos$Region),
                       datos$Region)

library(dplyr)

datos <- datos %>%
  mutate(Continent = case_when(
    Region %in% c("Chile", "Argentina", "Peru", "Ecuador", "Easter Island") ~ "South America",
    Region %in% c("Mexico", "Panama") ~ "Central America",
    Region %in% c("Alaska") ~ "North America",
    Region %in% c("India", "Indian Ridge", "Indonesia", "China", "Japan", "Philippines", "Taiwan", "Afghanistan", "Iran", "Timor Leste") ~ "Asia",
    Region %in% c("Vanuatu", "Tonga", "Solomon Islands", "Antarctic Ridge", "Fiji", "Fiji Islands", "Kermadec Islands", "Kuril Islands", "Macquarie Island", "Mariana Islands", "New Caledonia", "New Zealand", "Pacific Rise", "Papua New Guinea") ~ "Pacific Ocean",
    Region %in% c("Atlantic Ridge", "Sandwich Islands") ~ "Atlantic Ocean",
    Region %in% c("Fiji") ~ "Africa",
    Region %in% c("Greece", "Russia", "Turkey") ~ "Europe",
    TRUE ~ NA_character_
  ))

View(datos)
terremotos_cleaned <- datos[, c("ID", "year", "Region", "Continent", "Longitude", 
                              "Latitude", "Depth", "Mag", "MagType", "magSource", "nst")]

# Renombrar la columna 'nst' a 'NST'
names(terremotos_cleaned)[names(terremotos_cleaned) == "nst"] <- "NST"
names(terremotos_cleaned)[names(terremotos_cleaned) == "year"] <- "Year"
# Reordenar las columnas en el orden que solicitaste
terremotos_cleaned <- terremotos_cleaned[, c("ID", "Year", "Region", "Continent", 
                                                       "Longitude", "Latitude", "Depth", "Mag", 
                                                       "MagType", "magSource", "NST")]

View(terremotos_cleaned)



# Guardar el nuevo archivo CSV
write.csv(terremotos_cleaned, "terremotos_cleaned.csv", row.names = FALSE)
###########################################################################

# Cargar librerías necesarias
library(dplyr)

# Cargar la base de datos de terremotos
terremotos_cleaned <- read_csv("terremotos_cleaned.csv")


# Mapeo de continentes para las regiones conocidas
region_to_continent <- c(
  'Romania' = 'Europe', 'Indonesia' = 'Asia', 'Philippines' = 'Asia', 'New Zealand' = 'Oceania',
  'Vanuatu' = 'Oceania', 'Turkey-Syria' = 'Asia', 'Nepal' = 'Asia', 'Molucca Sea' = 'Asia', 
  'Reykjanes Ridge' = 'Atlantic Ocean', 'Guadeloupe' = 'North America', 'Turkey-Iran' = 'Asia',
  'Mid-Atlantic Ridge' = 'Atlantic Ocean', 'Balleny Islands' = 'Antarctica', 'Loyalty Islands' = 'Oceania', 
  'Ethiopia' = 'Africa', 'Pacific-Antarctic Ridge' = 'Pacific Ocean', 'CA' = NA, 'El Salvador' = 'North America', 
  'Samoa Islands' = 'Oceania', 'Santa Cruz Islands' = 'Oceania', 'Gabon' = 'Africa', 'Bouvet Island' = 'Atlantic Ocean', 
  'Portugal' = 'Europe', 'Guatemala' = 'North America', 'Italy' = 'Europe', 'Pacific Ocean' = 'Pacific Ocean', 
  'Saint Helena' = 'Africa', 'America' = 'America', 'Mid-Indian Ridge' = 'Indian Ocean', 'Myanmar' = 'Asia', 
  'American Samoa' = 'Oceania', 'Guam' = 'Oceania', 'Banda Sea' = 'Asia', 'Pakistan' = 'Asia', 
  'Svalbard and Jan Mayen' = 'Europe', 'Samoa' = 'Oceania', 'Oregon' = 'North America', 'Brazil' = 'South America', 
  'Svalbard' = 'Europe', 'Strait Gibraltar' = 'Europe', 'Virgin Islands' = 'North America', 
  'Bosnia and Herzegovina' = 'Europe', 'Nicaragua' = 'North America', 'Scotia Sea' = 'Atlantic Ocean', 
  'Wallis and Futuna' = 'Oceania', 'Ascension Island' = 'Atlantic Ocean', 'Colombia' = 'South America', 
  'Cyprus' = 'Asia', 'Laos' = 'Asia', 'Prince Edward Islands' = 'Indian Ocean', 'Flores Sea' = 'Asia', 
  'Indian Ocean' = 'Indian Ocean', 'Chile-Argentina' = 'South America', 'Indian-Antarctic Ridge' = 'Indian Ocean', 
  'Hawaii' = 'Oceania', 'Sea Okhotsk' = 'Asia', 'Australia' = 'Oceania', 'Chile Rise' = 'South America', 
  'Haiti' = 'North America', 'Greenland Sea' = 'Europe', 'Tajikistan' = 'Asia', 'Owen Fracture Zone' = 'Indian Ocean', 
  'Drake Passage' = 'South America', 'Somalia' = 'Africa', 'Costa Rica' = 'North America', 'Crozet Islands' = 'Indian Ocean', 
  'Mauritius - Reunion' = 'Indian Ocean', 'Atlantic Ocean' = 'Atlantic Ocean', 'Mongolia' = 'Asia', 
  'Galapagos Triple Junction' = 'Pacific Ocean', 'Shetland Islands' = 'Antarctica', 'Xizang' = 'Asia', 
  'Algeria' = 'Africa', 'Iceland' = 'Europe', 'Guyana' = 'South America', 'Croatia' = 'Europe', 
  'Palau' = 'Oceania', 'Norwegian Sea' = 'Europe', 'Chagos Archipelago' = 'Indian Ocean', 'Tanzania' = 'Africa', 
  'D\'Entrecasteaux Islands' = 'Oceania', 'Java Sea' = 'Asia', 'Micronesia' = 'Oceania', 
  'Indian Ocean Triple Junction' = 'Indian Ocean', 'Egypt' = 'Africa', 'NV Earthquake' = NA, 
  'Idaho' = 'North America', 'Utah' = 'North America', 'Cayman Islands' = 'North America', 
  'Jamaica' = 'North America', 'Puerto Rico' = 'North America', 'Canada' = 'North America', 
  'Sao Tome and Principe' = 'Africa', 'Albania' = 'Europe', 'Thailand' = 'Asia', 'Mauritius' = 'Indian Ocean', 
  'Bolivia' = 'South America', 'Zimbabwe' = 'Africa', 'Venezuela' = 'South America', 
  'Yemen' = 'Asia', 'Mayotte' = 'Indian Ocean', 'Malawi' = 'Africa', 'California' = 'North America', 
  'Iraq' = 'Asia', 'Korea' = 'Asia', 'Grenada' = 'North America', 'Montana' = 'North America', 
  'Mozambique' = 'Africa', 'Botswana' = 'Africa', 'Zambia' = 'Africa', 'Madagascar' = 'Africa', 
  'Bangladesh' = 'Asia', 'Democratic Republic Congo' = 'Africa', 'Galapagos Islands' = 'Pacific Ocean', 
  'Chile-Bolivia' = 'South America', 'Turkey-Iraq' = 'Asia', 'Washington' = 'North America', 
  'Gulf Alaska' = 'North America', 'Ukraine' = 'Europe', 'New Zealand' = 'Oceania', 'Alaska' = 'North America'
  # Continúa agregando más regiones si es necesario
)

# Asignar continente según el nombre de la región
terremotos_cleaned <- terremotos_cleaned %>%
  mutate(Continent = ifelse(is.na(Continent) & Region %in% names(region_to_continent), 
                            region_to_continent[Region], Continent))

# Filtrar filas donde la región no tiene sentido (por ejemplo, 'CA') y eliminarlas
terremotos_cleaned <- terremotos_cleaned %>%
  filter(!is.na(Continent) & nchar(Region) > 2) # Ajuste según lo que consideres un nombre inválido

# Guardar la nueva base de datos sin regiones con continentes en NA
write.csv(terremotos_cleaned, "eartquake_cleaned.csv", row.names = FALSE)

library(readr)
terremotos_cleaned <- read_csv("terremotos_cleaned.csv")
View(terremotos_cleaned)


terremotos_cleaned <-terremotos_cleaned %>%
  mutate(Continent = case_when(
    Continent %in% c('Indian Ocean', 'Pacific Ocean', 'Atlantic Ocean') ~ 'Region Ocean',
    Continent %in% c('North America', 'South America', 'Central America') ~ 'America',
    TRUE ~ Continent  # Mantiene el valor original si no coincide con los anteriores
  ))


View(terremotos_cleaned)
# Guardar la nueva base de datos con los cambios
write.csv(l, "base_completa.csv", row.names = FALSE)


library(readr)
base_limpia <- read_csv("base_limpia.csv")
View(base_limpia)

l <- base_limpia %>%
  filter(!is.na(Depth) & !is.na(NST)) # Ajuste según lo que consideres un nombre inválido
summary(l)
