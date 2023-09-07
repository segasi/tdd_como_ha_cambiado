### Cargar paquetes, definir setup y tema de gráficas -----
source("02_codigo/00_paquetes_setup_tema.R")

# Aumentar número de renglones que se imprimen de un data.frame
options(max.print = 9999)

### Extraer datos de la tabla en la primera página de la publicación -----

# Fuente: https://www.gob.mx/conasami/documentos/evolucion-del-salario-minimo?idiom=es

## Extraer texto de tabla ----
texto <- 
  pdf_text(pdf = "01_bd/01_salarios_minimos/_ndice_de_Salario_M_nimo_Real_Septiembre_2023.pdf")[[1]]

## Extraer información de la cadena de texto y formatearla para generar un tibble ----
bd_sm <- 
  texto %>% 
  # Convertir en un tibble
  as_tibble() %>% 
  # Separar renglones usando los linebreaks (\n)
  separate_rows(value, sep = "\\n") %>% 
  # Eliminar renglones de metadatos y nota a pie
  slice(6:29) %>% 
  # Eliminar espacios vacíos al comienzo y final de la cadena de texto de cada renglón
  mutate(value = str_trim(string = value)) %>% 
  # Separar pedazos de información en columnas a partir de espacios en blanco entre cadenas de texto
  separate(col = value, into = c("año", "1", "2", "3",
                                 "4", "5", "6",
                                 "7", "8", "9",
                                 "10", "11", "12", "promedio"), 
           sep = " +") %>% 
  # Eliminar columna promedio
  select(-promedio) %>% 
  # Transformas estructura de la base de datos
  pivot_longer(-año, names_to = "mes", values_to = "variable_interes") %>% 
  # Generar diversas variables
  mutate(# Convertir tipo de año y mes a double
    año = as.double(año),
    mes = as.double(mes),
    fecha = make_date(year = año, month = mes, day = 1),
    
    # Convertir tipo de variable_interes a double
    variable_interes = as.double(variable_interes),
    # Eliminar valor de agosto de 2023. Esto es necesario porque el código de arriba está asignando el valor del promedio parcial de 2023 a agosto, pero el último mes que registra un valor en el PDF original es julio de ese año
    variable_interes = if_else(año == 2023 & mes == 9, NA, variable_interes),
    # Crear variable presidente
    presidente = case_when(fecha > "2006-11-01" & fecha < "2012-12-01" ~ "Calderón",
                           fecha > "2012-11-01" & fecha < "2018-12-01" ~ "Peña Nieto",
                           fecha > "2018-11-01" ~ "López Obrador"),
    # Reordenar valores de variable presidente
    presidente = fct_relevel(presidente, "Calderón", "Peña Nieto", "López Obrador"), 
    # Crear variable color_lineas
    colores_lineas = case_when(presidente == "Calderón" ~ "#00308E", 
                               presidente == "Peña Nieto" ~ "#004e22", 
                               presidente == "López Obrador" ~ "#a50f15")) %>% 
  # En su caso, eliminar observaciones correspondientes previas a Calderón
  filter(fecha > "2006-11-01") %>% 
  # Calcular meses transcurridos desde el comienzo de la administración
  mutate(meses_transcurridos = row_number() - 1,
         .by = presidente) 


# Calcular cambio para el mismo mes transcurrido ----
bd_sm %>%
  mutate(cambio = variable_interes - lag(variable_interes),
         cambio_por = cambio/lag(variable_interes)*100,
         .by = meses_transcurridos) %>% 
  arrange(meses_transcurridos) %>% 
  as.data.frame()  
# tail(n = 100)


# Calcular cambio entre el valor máximo de cada administración vs. el último disponible de la actual ----
meses_max <- 
  bd_sm %>%
  # Agrupar tibble por presidente
  group_by(presidente) %>% 
  # Mantener las observaciones con los valores máximos de variable_interes
  slice_max(order_by = variable_interes) %>% 
  # Desagrupar
  ungroup() %>% 
  # Excluir renglón de López Obrador; para esta administración utilizaré el último valor disponible
  filter(presidente != "López Obrador") %>% 
  # Seleccionar variable fecha
  select(4) %>% 
  # Generar vector con valores de fechas
  pull()
  

# Hacer cálculo
bd_sm %>%
  # Eliminar observaciones con NA en variable de interés
  filter(!is.na(variable_interes)) %>% 
  # Mantener observaciones con valores máximos en administraciones previas y la última disponible de la actual
  filter(fecha %in% meses_max | fecha == max(x = fecha)) %>% 
  # Seleccionar columnas de interés
  select(4, 5, 3) %>% 
  # Calcular cambios
  mutate(cambio_vs_fch = variable_interes - lag(variable_interes, n = 2),
         cambio_por_vs_fch = cambio_vs_fch/lag(variable_interes, n = 2)*100,
         cambio_vs_epn = variable_interes - lag(variable_interes, n = 1),
         cambio_por_vs_epn = cambio_vs_epn/lag(variable_interes, n = 1)*100, 
         cambio_vs_epn = ifelse(presidente == "López Obrador", cambio_vs_epn, NA),
         cambio_por_vs_epn = ifelse(presidente == "López Obrador", cambio_por_vs_epn, NA))  %>% 
  as.data.frame()
  

# Primera gráfica ----
bd_sm %>% 
  mutate(punto_ultimo_valor = if_else(fecha == "2023-08-01", variable_interes, NA),
         etiqueta_ultimo_valor = if_else(fecha == "2023-08-01", variable_interes, NA),
         etiqueta_ultimo_valor = dollar(x = round(x = etiqueta_ultimo_valor, 2))) %>% 
  ggplot(aes(x = fecha,
             y = variable_interes,
             color = colores_lineas)) +
  # Recuadro gris
  annotate(geom = "rect", 
           xmin = as.Date("2012-12-09"), xmax = as.Date("2018-11-25"), 
           ymin = -Inf, ymax = Inf, 
           color = "grey90", alpha = 0.2) +
  # Etiqueta eje y
  annotate(geom = "text", label = "Pesos constantes (2018)", angle = 90, x = as.Date("2006-10-01"), y = 135, hjust = 0, size = 5, fontface = "bold", family = "Noto Sans JP",  color = "grey30") +
  # Etiqueta Calderón
  annotate(geom = "text", label = "Calderón", x = as.Date("2008-10-15"), y = 95, hjust = 0, size = 9, fontface = "bold", family = "Noto Sans JP", color = "grey50") +
  # Etiqueta Peña Nieto
  annotate(geom = "text", label = "Peña Nieto", x = as.Date("2014-10-15"), y = 95, hjust = 0, size = 9, fontface = "bold", family = "Noto Sans JP", color = "white") +
  # Etiqueta López Obrador
  annotate(geom = "text", label = "López Obrador", x = as.Date("2020-03-15"), y = 95, hjust = 0, size = 9, fontface = "bold", family = "Noto Sans JP", color = "grey50") +
  geom_line(linewidth = 1.5) +
  geom_point(aes(x = fecha,
                 y = punto_ultimo_valor),
             color = "#a50f15", 
             size = 4) +
  geom_text(aes(x = fecha,
                 y = punto_ultimo_valor,
                 label = etiqueta_ultimo_valor), 
            fontface = "bold",
             family = "Noto Sans JP",
             color = "#a50f15", 
            vjust = 1.9,
             size = 5) +
  scale_x_date(breaks = c(seq.Date(from = as.Date("2006-12-01"),
                                   to = as.Date("2022-12-01"),
                                   by = "2 years"),
                          as.Date("2023-08-01")),
               date_labels = "%Y\n%b",
               limits = c(as.Date("2006-08-01"), as.Date("2023-12-31")),
               expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(from = 0, to = 200, by = 10),
                     labels = dollar_format(accuracy = 1, prefix = "$")) +
  scale_color_identity() +
  labs(title = "En 2023 el <span style='color:#a50f15'>salario mínimo</span> llegó a su punto más alto de los últimos tres sexenios...",
       subtitle = "En agosto de 2023 es <span style='color:#00308E'>114.2%</span> y <span style='color:#004e22'>85.4%</span> mayor que los máximos alcanzados en los sexenios de <span style='color:#00308E'>Calderón</span> y <span style='color:#004e22'>Peña Nieto</span>", 
       x = "",
       y = NULL,
       caption = "Elaborado por Sebastián Garrido de Sierra / @segasi\nFuente: Evolución del Salario Mínimo Real, CONASAMI-STyPS, corte a agosto de 2023, tinyurl.com/salario-minimo-mx\nNota: Las cifras corresponden al \"salario mínimo general ponderado por la población subordinada y remunerada en las distintas áreas geográficas del país en el periodo previo a su fijación\" y están\nexpresadas en pesos de la segunda quincena de julio de 2018") +
  tema +
  theme(plot.title = element_markdown(size = 31),
        plot.subtitle = element_markdown(size = 20.5),
        axis.text = element_text(size = 16),
        axis.title.y = element_text(vjust = -16, size = 16),
        axis.title.x = element_text(size = 1),
        legend.position = "none")

ggsave("03_vis/01_salario_minimo/evolucion_evolucion_salario_minimo_12_2006_08_2023_1.png", dpi = 300, width = 16, height = 10)


ggsave("03_vis/01_salario_minimo/evolucion_evolucion_salario_minimo_12_2006_08_2023_1_ig.png", dpi = 300, width = 14, height = 14)


# Calcular cambio entre primer y último mes disponible de cada administración ----
bd_sm %>%
  filter(fecha %in% c("2006-12-01", "2012-11-01",
                      "2012-12-01", "2018-11-01",
                      "2018-12-01", "2023-08-01")) %>% 
  # Agrupar tibble por presidente
  group_by(presidente) %>% 
  # Calcular cambios
  mutate(cambio = variable_interes - lag(variable_interes),
         cambio_por = cambio/lag(variable_interes)*100)  %>% 
  as.data.frame()


# Segunda gráfica ----
bd_sm %>% 
  mutate(punto_ultimo_valor = if_else(fecha == "2023-08-01", variable_interes, NA),
         etiqueta_ultimo_valor = if_else(fecha == "2023-08-01", variable_interes, NA),
         etiqueta_ultimo_valor = dollar(x = round(x = etiqueta_ultimo_valor, 2))) %>% 
  ggplot(aes(x = meses_transcurridos,
             y = variable_interes,
             color = colores_lineas)) +
  geomtextpath::geom_textline(aes(label = presidente), linewidth = 1.5, family = "Noto Sans JP", size = 7, hjust = 0.79, fontface = "bold") +
  geom_point(aes(x = meses_transcurridos,
                 y = punto_ultimo_valor),
             color = "#a50f15", 
             size = 4) +
  geom_text(aes(x = meses_transcurridos,
                y = punto_ultimo_valor,
                label = etiqueta_ultimo_valor), 
            fontface = "bold",
            family = "Noto Sans JP",
            color = "#a50f15", 
            vjust = 1.9,
            size = 5) +
  # Etiqueta eje y
  annotate(geom = "text", label = "Pesos constantes (2018)", angle = 90, x = -1.5, y = 135, hjust = 0, size = 5, fontface = "bold", family = "Noto Sans JP",  color = "grey30") +
  scale_x_continuous(breaks = c(seq(from = 0, to = 60, by = 10), 71),
                     limits = c(-3, 72),
                     expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(from = 0, to = 200, by = 10),
                     labels = dollar_format(accuracy = 1, prefix = "$")) +
  scale_color_identity() +
  labs(title = "El <span style='color:#a50f15'>salario mínimo</span> ha crecido <span style='color:#a50f15'>94.3%</span> en la actual administración...",
       subtitle = "A lo largo de los sexenios de <span style='color:#00308E'>Calderón</span> y <span style='color:#004e22'>Peña Nieto</span> aumentó <span style='color:#00308E'>0.28%</span> y <span style='color:#004e22'>14.6%</span>, respectivamente",
       x = "Meses transcurridos\nde la administración",
       y = NULL,
       caption = "Elaborado por Sebastián Garrido de Sierra / @segasi\nFuente: Evolución del Salario Mínimo Real, CONASAMI-STyPS, corte a agosto de 2023, tinyurl.com/salario-minimo-mx\nNota: Las cifras corresponden al \"salario mínimo general ponderado por la población subordinada y remunerada en las distintas áreas geográficas del país en el periodo previo a su fijación\" y están\nexpresadas en pesos de la segunda quincena de julio de 2018") +
  tema +
  theme(plot.title = element_markdown(size = 39),
        plot.subtitle = element_markdown(),
        legend.position = "none")
  
ggsave("03_vis/01_salario_minimo/evolucion_evolucion_salario_minimo_12_2006_08_2023_2.png", dpi = 300, width = 16, height = 10)









