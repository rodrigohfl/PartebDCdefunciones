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
