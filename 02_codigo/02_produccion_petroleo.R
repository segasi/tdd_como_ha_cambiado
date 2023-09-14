### Cargar paquetes, definir setup y tema de gráficas -----
source("02_codigo/00_paquetes_setup_tema.R")

# Aumentar número de renglones que se imprimen de un data.frame
options(max.print = 9999)

### Importar base de datos -----

# Fuente: https://sie.energia.gob.mx/bdiController.do?action=cuadro&cvecua=PMXB1C02

bd_prod_petroleo <- 
  read_excel("01_bd/02_produccion_petroleo/csrliekmvy_PMXB1C02_03092023_22_22.xls",
           range = "a7:gs9")

### Preparar tibble ----
bd_prod_petroleo <- 
  bd_prod_petroleo %>% 
  # Eliminar renglón con NAs
  filter(!is.na(...1)) %>% 
  # Transformas estructura de la base de datos
  pivot_longer(-...1, names_to = "mes_año", values_to = "variable_interes") %>% 
  # Eliminar primera columna
  select(-1) %>%
  # Separar valor de mes y año a partir de diagonal
  separate(col = mes_año, into =  c("mes_txt", "año")) %>% 
  # Generar/transformar diversas variables
  mutate(mes = case_when(mes_txt == "Ene" ~ 1, # Construir variable mes en formato numérico
                         mes_txt == "Feb" ~ 2, 
                         mes_txt == "Mar" ~ 3,
                         mes_txt == "Abr" ~ 4,
                         mes_txt == "May" ~ 5,
                         mes_txt == "Jun" ~ 6,
                         mes_txt == "Jul" ~ 7,
                         mes_txt == "Ago" ~ 8,
                         mes_txt == "Sep" ~ 9,
                         mes_txt == "Oct" ~ 10,
                         mes_txt == "Nov" ~ 11,
                         mes_txt == "Dic" ~ 12),
         # Transformar tipo de la variable año
         año = as.double(año),
         # Construir variable fecha
         fecha = make_date(year = año, month = mes, day = 1),
         # Construir presidente
         presidente = case_when(fecha > "2006-11-01" & fecha < "2012-12-01" ~ "Calderón",
                                fecha > "2012-11-01" & fecha < "2018-12-01" ~ "Peña Nieto",
                                fecha > "2018-11-01" ~ "López Obrador"),
         # Reordenar niveles de la variable presidente
         presidente = fct_relevel(presidente, "Calderón", "Peña Nieto", "López Obrador"), 
         # Definir tono de color que debe tener la línea correspondiente al período de cada administración
         colores_lineas = case_when(presidente == "Calderón" ~ "#00308E", 
                                    presidente == "Peña Nieto" ~ "#004e22", 
                                    presidente == "López Obrador" ~ "#a50f15")) %>% 
  # Reubicar columnas
  select(año, mes_txt, mes, fecha, variable_interes, everything()) %>% 
  # En su caso, eliminar observaciones correspondientes previas a Calderón
  filter(fecha > "2006-11-01") %>% 
  # Calcular meses transcurridos desde el comienzo de la administración
  mutate(meses_transcurridos = row_number() - 1,
         .by = presidente) 

# Imprimir primeras y últimas 10 observaciones de la base de datos

# Primeras
bd_prod_petroleo %>%
  as.data.frame() %>% 
  head(n = 10)

# Últimas
bd_prod_petroleo %>%
  as.data.frame() %>% 
  tail(n = 10)
  


# Primera gráfica ----

# Calcular resumen de seis números de variable de interés
summary(object = bd_prod_petroleo$variable_interes)

bd_prod_petroleo %>% 
  mutate(punto_ultimo_valor = if_else(fecha == "2023-07-01", variable_interes, NA),
         etiqueta_ultimo_valor = if_else(fecha == "2023-07-01", variable_interes, NA),
         etiqueta_ultimo_valor = str_c(comma(x = round(x = etiqueta_ultimo_valor, 2)), "\nmiles de\nbarriles\ndiarios")) %>% 
  ggplot(aes(x = fecha,
             y = variable_interes,
             color = colores_lineas)) +
  # Recuadro gris
  annotate(geom = "rect", 
           xmin = as.Date("2012-12-09"), xmax = as.Date("2018-11-25"), 
           ymin = -Inf, ymax = Inf, 
           color = "grey90", alpha = 0.2) +
  # Etiqueta eje y
  annotate(geom = "text", label = "Miles de barriles diarios", angle = 90, x = as.Date("2006-08-01"), y = 1450, hjust = 0, size = 5, fontface = "bold", family = "Noto Sans JP", color = "grey30") +
  # Etiqueta Calderón
  annotate(geom = "text", label = "Calderón", x = as.Date("2008-10-15"), y = 3050, hjust = 0, size = 9, fontface = "bold", family = "Noto Sans JP", color = "grey50") +
  # Etiqueta Peña Nieto
  annotate(geom = "text", label = "Peña Nieto", x = as.Date("2014-10-15"), y = 3050, hjust = 0, size = 9, fontface = "bold", family = "Noto Sans JP", color = "white") +
  # Etiqueta López Obrador
  annotate(geom = "text", label = "López Obrador", x = as.Date("2020-03-15"), y = 3050, hjust = 0, size = 9, fontface = "bold", family = "Noto Sans JP", color = "grey50") +
  geom_line(linewidth = 1.5) +
  geom_point(aes(x = fecha, y = punto_ultimo_valor),
             color = "#a50f15", size = 4) +
  geom_text(aes(x = fecha, y = punto_ultimo_valor, label = etiqueta_ultimo_valor), 
            fontface = "bold", family = "Noto Sans JP",
            color = "#a50f15", lineheight = 0.9,
            vjust = 1.2, size = 4.5) +
  scale_x_date(breaks = c(seq.Date(from = as.Date("2006-12-01"),
                                   to = as.Date("2022-12-01"),
                                   by = "2 years"),
                          as.Date("2023-08-01")),
               date_labels = "%Y\n%b",
               limits = c(as.Date("2006-06-01"), as.Date("2023-12-31")),
               expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(from = 0, to = 3500, by = 250),
                     limits = c(1450, 3300),
                     labels = comma) +
  scale_color_identity() +
  labs(title = "En este sexenio se revirtió la tendencia a la baja de la <span style='color:#a50f15'>producción de petróleo</span>...",
       subtitle = "Misma que significó una caída de <span style='color:#a50f15'>42.4%</span> durante las administraciones de <span style='color:#00308E'>Calderón</span> y <span style='color:#004e22'>Peña Nieto</span>", 
       x = "",
       y = NULL,
       caption = "Elaborado por Sebastián Garrido de Sierra / @segasi\nFuente: Producción de petróleo crudo por entidad federativa, SIE-SENER-PEMEX, tinyurl.com/produccion-petroleo-mx\nNota: Las cifras corresponden a la producción medida a 20ºC de temperatura y 1 atmósfera.") +
  tema +
  theme(plot.title = element_markdown(size = 32),
        plot.subtitle = element_markdown(),
        axis.text = element_text(size = 16),
        axis.title.y = element_text(vjust = -16, size = 16),
        axis.title.x = element_text(size = 1),
        legend.position = "none")

ggsave("03_vis/02_produccion_petroleo/produccion_petroleo_crudo_12_2006_07_2023_1.png", dpi = 300, width = 16, height = 10)


# Calcular cambio entre primer y último mes disponible de cada administración ----
bd_prod_petroleo %>%
  filter(fecha %in% c("2006-12-01", "2012-11-01",
                      "2012-12-01", "2018-11-01",
                      "2018-12-01", "2023-07-01")) %>% 
  # Agrupar tibble por presidente
  group_by(presidente) %>% 
  # Calcular cambios
  mutate(cambio = variable_interes - lag(variable_interes),
         cambio_por = cambio/lag(variable_interes)*100)  %>% 
  as.data.frame()

# Calcular cambio entre primer mes disponible de cada administración y el mes transcurrido 55----
bd_prod_petroleo %>%
  filter(meses_transcurridos %in% c(0, 55)) %>% 
  # Agrupar tibble por presidente
  group_by(presidente) %>% 
  # Calcular cambios
  mutate(cambio = variable_interes - lag(variable_interes),
         cambio_por = cambio/lag(variable_interes)*100)  %>% 
  as.data.frame()


# Calcular cambio entre el último mes disponible y el mes correspondiente en administraciones previas cada administración ----
bd_prod_petroleo %>%
  filter(meses_transcurridos == 55) %>% 
  # Calcular cambios
  mutate(cambio_vs_fch = variable_interes - lag(variable_interes, n = 2),
         cambio_por_vs_fch = cambio_vs_fch/lag(variable_interes, n = 2)*100,
         cambio_vs_epn = variable_interes - lag(variable_interes, n = 1),
         cambio_por_vs_epn = cambio_vs_epn/lag(variable_interes, n = 1)*100, 
         cambio_vs_epn = ifelse(presidente == "López Obrador", cambio_vs_epn, NA),
         cambio_por_vs_epn = ifelse(presidente == "López Obrador", cambio_por_vs_epn, NA))  %>% 
  as.data.frame()


# Segunda gráfica ----
bd_prod_petroleo %>%
  mutate(punto_ultimo_valor = if_else(fecha == "2023-07-01", variable_interes, NA),
         etiqueta_ultimo_valor = if_else(fecha == "2023-07-01", variable_interes, NA),
         etiqueta_ultimo_valor = str_c(comma(x = round(x = etiqueta_ultimo_valor, 2)), "\nmiles de\nbarriles\ndiarios")) %>% 
  ggplot(aes(x = meses_transcurridos,
             y = variable_interes,
             color = colores_lineas)) +
  geomtextpath::geom_textline(aes(label = presidente), linewidth = 1.5, family = "Noto Sans JP", size = 7, hjust = 0.48, fontface = "bold") +
  geom_point(aes(x = meses_transcurridos, y = punto_ultimo_valor),
             color = "#a50f15", size = 4) +
  geom_text(aes(x = meses_transcurridos, y = punto_ultimo_valor, label = etiqueta_ultimo_valor), 
            fontface = "bold", family = "Noto Sans JP",
            color = "#a50f15", lineheight = 0.9,
            vjust = 1.2, hjust = 0.7, size = 4.5) +
  scale_x_continuous(breaks = c(seq(from = 0, to = 60, by = 10), 71),
                     limits = c(-1, 72),
                     expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(from = 0, to = 3500, by = 250),
                     limits = c(1450, 3300),
                     labels = comma) +
  scale_color_identity() +
  labs(title = "La <span style='color:#a50f15'>producción de petróleo</span> ha crecido <span style='color:#a50f15'>7.3%</span> durante la actual administración...",
       subtitle = "Pero en el mes 55 es <span style='color:#00308E'>26.8%</span> y <span style='color:#004e22'>6.6%</span> menor que en el mismo período de los sexenios de <span style='color:#00308E'>Calderón</span> y <span style='color:#004e22'>Peña Nieto</span>", 
       x = "Meses transcurridos",
       y = "Miles de barriles diarios",
       caption = "Elaborado por Sebastián Garrido de Sierra / @segasi\nFuente: Producción de petróleo crudo por entidad federativa, SIE-SENER-PEMEX, tinyurl.com/produccion-petroleo-mx\nNota: Las cifras corresponden a la producción medida a 20ºC de temperatura y 1 atmósfera.") +
  tema +
  theme(plot.title = element_markdown(size = 32.5),
        plot.subtitle = element_markdown(size = 21.5),
        legend.position = "none")

ggsave("03_vis/02_produccion_petroleo/produccion_petroleo_crudo_12_2006_07_2023_2.png", dpi = 300, width = 16, height = 10)



