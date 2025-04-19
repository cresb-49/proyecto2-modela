library(dplyr)
library(ggplot2)
data(starwars)
glimpse(starwars)

summary(starwars)
str(starwars)
anyNA(starwars)
table(starwars$species)

# Limpieza de datos NA debido a que no son necesarios de graficar
starwars_clean <- starwars %>%
  filter(!is.na(height) & !is.na(mass) & !is.na(species) & !is.na(sex))

# Dispersión entre altura y peso
ggplot(starwars_clean, aes(x = height, y = mass)) +
  geom_point(color = "steelblue", size = 3, alpha = 0.7) +
  labs(
    title = "Altura vs Peso de los personajes de Star Wars",
    x = "Altura (cm)",
    y = "Peso (kg)"
  ) +
  theme_minimal()

# Cantidad de personajes por sexo
ggplot(starwars, aes(x = sex)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Distribución de personajes por sexo", x = "Sexo", y = "Cantidad") +
  theme_minimal()

# Distribucion de personajes por especie
ggplot(starwars, aes(x = species)) +
  geom_bar(fill = "darkgreen") +
  labs(title = "Cantidad de personajes por especie", x = "Especie", y = "Cantidad") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Distribucion de personajes por planetas de origen
ggplot(starwars, aes(x = homeworld)) +
  geom_bar(fill = "tomato") +
  labs(title = "Personajes por planeta de origen", x = "Planeta", y = "Cantidad") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Especie VS Genero
ggplot(sw_clean, aes(x = species, fill = gender)) +
  geom_bar(position = "dodge") +
  labs(title = "Especie por identidad de género", x = "Especie", y = "Cantidad") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Altura promedio por especie
ggplot(starwars %>% filter(!is.na(height)), aes(x = sex, y = height)) +
  geom_boxplot(fill = "orchid") +
  labs(title = "Altura por sexo", x = "Sexo", y = "Altura (cm)") +
  theme_minimal()

#Numero de peliculas por personaje
starwars %>%
  mutate(num_films = lengths(films)) %>%
  ggplot(aes(x = reorder(name, -num_films), y = num_films)) +
  geom_bar(stat = "identity", fill = "darkred") +
  labs(title = "Películas por personaje", x = "Personaje", y = "Cantidad de películas") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Numero de vehiculos por personaje
starwars %>%
  mutate(num_vehicles = lengths(vehicles)) %>%
  ggplot(aes(x = reorder(name, -num_vehicles), y = num_vehicles)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(title = "Vehículos por personaje", x = "Personaje", y = "Cantidad") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Personajes por año de nacimiento faltnte
birth_na <- starwars %>%
  mutate(has_birthyear = ifelse(is.na(birth_year), "Desconocido", "Registrado"))

ggplot(birth_na, aes(x = has_birthyear, fill = has_birthyear)) +
  geom_bar(width = 0.6, color = "white", alpha = 0.9) +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, size = 5, fontface = "bold") +
  scale_fill_manual(values = c("Desconocido" = "#D95F02", "Registrado" = "#1B9E77")) +
  labs(
    title = "Disponibilidad del año de nacimiento",
    subtitle = "En el universo de personajes de Star Wars",
    x = NULL,
    y = "Cantidad"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 13, hjust = 0.5),
    axis.text = element_text(size = 12),
    axis.title.y = element_text(margin = margin(r = 10))
  ) +
  guides(fill = "none")

# Peliculas por personaje
starwars %>%
  mutate(
    num_films = lengths(films),
    has_birthyear = !is.na(birth_year)
  ) %>%
  group_by(has_birthyear) %>%
  summarise(
    Promedio_peliculas = mean(num_films),
    Max_peliculas = max(num_films),
    Min_peliculas = min(num_films)
  )

starwars %>%
  mutate(
    num_films = lengths(films),
    has_birthyear = ifelse(is.na(birth_year), "Desconocido", "Registrado")
  ) %>%
  ggplot(aes(x = has_birthyear, y = num_films, fill = has_birthyear)) +
  geom_boxplot() +
  labs(title = "Películas por personaje según disponibilidad de año de nacimiento",
       x = "Año de nacimiento",
       y = "Cantidad de películas") +
  theme_minimal() +
  scale_fill_manual(values = c("Desconocido" = "#D95F02", "Registrado" = "#1B9E77")) +
  guides(fill = FALSE)

#Altura Vs Peso por especie
ggplot(
  data = starwars %>% filter(!is.na(height), !is.na(mass), !is.na(species)),
  aes(x = height, y = mass, color = species)
) +
  geom_point(size = 3, alpha = 0.8) +
  labs(
    title = "Altura vs Peso según especie en personajes de Star Wars",
    subtitle = "Relación entre características físicas y clasificación por especie",
    x = "Altura (cm)",
    y = "Peso (kg)",
    color = "Especie"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text = element_text(size = 12),
    legend.position = "bottom",
    legend.text = element_text(size = 10)
  ) +
  guides(color = guide_legend(ncol = 3))
