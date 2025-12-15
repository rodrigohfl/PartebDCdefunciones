library(tidyverse)
ruta <- "D:/OneDrive/Data analyst jobs/Data civica/Parte B/PartebDCdefunciones/datos/conjunto_de_datos_edr2024_csv/conjunto_de_datos/conjunto_de_datos_defunciones_registradas24_csv.csv"

df <- read_csv(ruta, show_col_types = FALSE)
df <- df %>%
  mutate(es_homicidio = grepl("^(X85|X86|X87|X88|X89|X9\\d|Y0\\d)", causa_def))
df_hom_2024 <- df %>%
  filter(es_homicidio, anio_ocur == 2024)
df_hom_2024 <- df_hom_2024 %>%
  mutate(sexo_txt = case_when(
    sexo == 1 ~ "Hombres",
    sexo == 2 ~ "Mujeres",
    TRUE ~ "No especificado"
  ))
df_hom_2024 <- df_hom_2024 %>%
  mutate(
    edad_anios = anio_ocur - anio_nacim,
    grupo_edad = case_when(
      edad_anios >= 0  & edad_anios <= 9  ~ "Infancias (0-9)",
      edad_anios >= 10 & edad_anios <= 19 ~ "Adolescentes (10-19)",
      edad_anios >= 20 & edad_anios <= 35 ~ "Jóvenes (20-35)",
      edad_anios >= 36 & edad_anios <= 59 ~ "Adultos (36-59)",
      edad_anios >= 60                    ~ "Adultos mayores (60+)",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(grupo_edad))



# Crear carpeta outputs si no existe
dir.create("outputs", showWarnings = FALSE)

# -------------------------
# Hallazgo 1: Homicidios por sexo
# -------------------------
graf_sexo <- df_hom_2024 %>%
  count(sexo_txt, name = "total")

p1 <- ggplot(graf_sexo, aes(x = sexo_txt, y = total)) +
  geom_col() +
  labs(
    title = "Homicidios en México (ocurridos en 2024), por sexo",
    x = "Sexo",
    y = "Número de homicidios"
  ) +
  theme_minimal()

ggsave("outputs/hallazgo1_homicidios_por_sexo.png", p1, width = 8, height = 5, dpi = 150)

# -------------------------
# Hallazgo 2: Homicidios por grupo de edad y sexo
# -------------------------
graf_edad_sexo <- df_hom_2024 %>%
  count(grupo_edad, sexo_txt, name = "total")

p2 <- ggplot(graf_edad_sexo, aes(x = grupo_edad, y = total, fill = sexo_txt)) +
  geom_col(position = "dodge") +
  labs(
    title = "Homicidios en México (2024) por grupo de edad y sexo",
    x = "Grupo de edad",
    y = "Número de homicidios",
    fill = "Sexo"
  ) +
  theme_minimal()

ggsave("outputs/hallazgo2_homicidios_por_edad_y_sexo.png", p2, width = 10, height = 5, dpi = 150)

# -------------------------
# Hallazgo 3: Top 15 estados por homicidios
# (usa ent_ocurr como clave; si luego quieres nombres, se une con el catálogo)
# -------------------------
top_estados <- df_hom_2024 %>%
  count(ent_ocurr, name = "homicidios") %>%
  arrange(desc(homicidios)) %>%
  slice_head(n = 15)

p3 <- ggplot(top_estados, aes(x = reorder(ent_ocurr, homicidios), y = homicidios)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Estados con mayor número de homicidios en México (2024)",
    x = "Entidad (clave)",
    y = "Número de homicidios"
  ) +
  theme_minimal()

ggsave("outputs/hallazgo3_top15_estados.png", p3, width = 9, height = 6, dpi = 150)

