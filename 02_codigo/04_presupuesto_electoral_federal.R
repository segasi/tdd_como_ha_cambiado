### Cargar paquetes, definir setup y tema de gráficas -----
source("02_codigo/00_paquetes_setup_tema.R")

### Importar bases de datos ----

## Proyecto de presupuesto y presupuesto aprobado del IFE e INE entre 2007 y 2023 ----

# Construí esta base de datos a partir de la información consultada de dos fuentes. Por un lado, las cifras registradas en las columnas proyecto_presupuesto_institucion, presupuesto_institucion_aprobado y presupuesto_federacion_total las obtuve de los presupuestos de egresos de la federación de 2007 a 2023. En todos los casos consideré las cifras publicadas en el anexo titulado "Adecuaciones aprobadas por la Cámara de Diputados" de cada presupuesto.

# Por otro lado, as cifras registradas en la columna presupuesto_partidos_politicos proviene de diferentes documentos del INE obtenidos en dos sitios: https://tinyurl.com/presupuesto-pp y https://tinyurl.com/presupuesto-ife-ine.

# En ambos casos, los documentos que sirvieron como fuentes primarias pueden ser encontrados en las carpetas c_de_d e ine, en el directorio 01_bd/04_presupuesto_electoral_federal de este proyecto.

# Dos notas más antes de importar las bases de datos. Primero, en todos los años, el presupuesto aprobado por la Cámara de Diputados al IFE e INE incluyó el financiamiento de los partidos políticos por actividades ordinarias, especiales, franquicias postales y telegráficas. En años electorales (2009, 2012, 2015, 2018 y 2021) además incluyó financiamiento para gastos de campaña.

# Segundo, el 10 de febrero de 2014 entró en vigor la reforma electoral que transformó el IFE en el INE. Además del cambio de nombre, la reforma amplió las facultades y obligaciones del INE. En esta liga https://www.te.gob.mx/consultareforma2014/node/2898 se puede consultar un resumen de la misma.

bd_pef <- 
  read_excel("01_bd/04_presupuesto_electoral_federal/presupuesto_electoral_federal_2007_2023.xlsx",
           range = "a1:f18") 


## Índice de precios implícitos ----

# Fuente: INEGI, https://www.inegi.org.mx/app/indicadores/?tm=0#D735143

bd_indice <-
  read_excel("01_bd/inegi/Indicadores20231004124404.xls",
             range = "a5:b75") %>% 
  clean_names() %>% 
  # Eliminar información de 2006
  filter(!str_detect(string = periodos, pattern = "2006"))


### Realizar diversas transformaciones a bd_pef ----
bd_pef <- 
  bd_pef %>% 
  # Eliminar segunda columna
  select(-2) %>% 
  # Renombrar variables
  rename(presupuesto_proyecto = 2,
         presupuesto_aprobado = 3)


### Realizar diversas transformaciones a bd_indice ----
bd_indice <- 
  bd_indice %>% 
  # Eliminar la información correspondiente a periodos diferentes al último trimestre de cada año
  filter(str_detect(string = periodos, pattern = "/04")) %>% 
  # Renombrar variables
  rename(año = 1,
         ipi_b2018 = 2) %>% 
  # Eliminar trimestre de la columna año y convertir en variable tipo double
  mutate(año = str_remove(string = año, pattern = "/04"),
         año = as.double(x = año)) %>% 
  # Incluir valor del índice de precios implícitos para 2023. Para ello, multiplico el valor del índice de precios implícitos de 2022 (123.442607785598) por 1 + el deflactor del PIB estimado por la SHCP para 2023 (fuente: https://tinyurl.com/shcp-cgpe-2023, página 134), que en este caso es 0.05 (o 5%).
  add_row(año = 2023, ipi_b2018 = 123.442607785598 * 1.05) %>% 
  # Crear versión del índice de precios implícitos para que tenga como año base el 2023
  mutate(valor_año_base = 123.442607785598 * 1.05,
         ipi_b2023 = ipi_b2018 * 100/valor_año_base)
  

### Unir variable ipi_b2023 del tibble bd_indice al tibble bd_pef ----
bd_pef <- 
  bd_pef %>%
  full_join(y = bd_indice %>% 
              select(-3),
            by = "año")

### Construir diversas variables ----
bd_pef <- 
  bd_pef %>% 
  # Deflactar el presupuesto aprobado del INE, basado en los pasos descritos en esta página: https://www.dallasfed.org/research/basics/nominal 
  mutate(presupuesto_aprobado_def = presupuesto_aprobado/(ipi_b2023/100),
         # Calcular qué % del presupuesto solicitado fue aprobado
         prop_aprobado_vs_proyecto = presupuesto_aprobado/presupuesto_proyecto,
         # Calcular qué % del presupuesto aprobado al IFE/INE corresponde a financiamiento de los partidos políticos
         prop_pp = presupuesto_partidos_politicos/presupuesto_aprobado,
         # Calcular qué % del presupuesto total aprobado representa el presupuesto aprobado del INE
         prop_aprobado_vs_total = presupuesto_aprobado/presupuesto_federacion_total,
         # Crear variable presidente
         presidente = case_when(año >= 2006 & año <= 2012 ~ "Calderón",
                                año >= 2013 & año <= 2018 ~ "Peña Nieto",
                                año >= 2019 ~ "López Obrador"),
         # Reordenar valores de variable presidente
         presidente = fct_relevel(presidente, "Calderón", "Peña Nieto", "López Obrador"), 
         # Crear variable color_lineas
         colores_barras = case_when(presidente == "Calderón" ~ "#00308E", 
                                    presidente == "Peña Nieto" ~ "#004e22", 
                                    presidente == "López Obrador" ~ "#a50f15")) %>% 
  # Calcular meses transcurridos desde el comienzo de la administración
  mutate(años_transcurridos = row_number(),
         .by = presidente) 

bd_pef %>%
  ggplot(aes(x = ipi_b2018, y = ipi_b2023)) +
  geom_point()

### Gráfica 1 ----
bd_pef %>%
  ggplot(aes(x = año,
             y = presupuesto_aprobado_def/1e6,
             fill = colores_barras)) +
  # Recuadro gris
  annotate(geom = "rect",
           xmin = 2012.5, xmax = 2018.5,
           ymin = -Inf, ymax = Inf,
           color = "grey90", alpha = 0.2) +
  geom_col() +
  geom_text(aes(label = str_c(dollar(x = round(presupuesto_aprobado_def/1e6)), "\nmillones")), 
            fontface = "bold",
            family = "Noto Sans JP",
            color = "white", 
            vjust = 1.3,
            size = 4, 
            lineheight = 0.95) +
  scale_fill_identity() +
  # Etiqueta eje y
  annotate(geom = "text", label = "Millones de pesos constantes (año base 2023)", angle = 90, x = 2006.2, y = 10, hjust = 0, size = 6, fontface = "bold", family = "Noto Sans JP",  color = "grey30", lineheight = 0.9) +
  # Etiqueta Calderón
  annotate(geom = "text", label = "Calderón", x = 2009.55, y = 32500, hjust = 0.5, size = 9, fontface = "bold", family = "Noto Sans JP", color = "grey50") +
  # Etiqueta Peña Nieto
  annotate(geom = "text", label = "Peña Nieto", x = 2015.55, y = 32500, hjust = 0.5, size = 9, fontface = "bold", family = "Noto Sans JP", color = "white") +
  # Etiqueta López Obrador
  annotate(geom = "text", label = "López Obrador", x = 2021.3, y = 32500, hjust = 0.5, size = 9, fontface = "bold", family = "Noto Sans JP", color = "grey50") +
  scale_x_continuous(breaks = 2007:2023,
                     limits = c(2006, 2023.5),
                     expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 34000)) +
  labs(title = "El <span style='color:#c31c78'>presupuesto electoral federal</span> mantiene un comportamiento cíclico...",
       subtitle = "<span style='color:#a50f15'>Aumenta</span> sustantivamente en años electorales y tiende a <span style='color:#a50f15'>disminuir</span> en el resto", 
       x = "",
       y = NULL,
       caption = str_c("Elaborado por Sebastián Garrido de Sierra / @segasi\nFuentes: Presupuesto de egresos de la federación 2007-2023, Cámara de Diputados. En todos los casos obtuve las cifras del anexo titulado \"Adecuaciones aprobadas por la H. Cámara de Diputados\";\nÍndice de precios implícitos del PIB (Índice base 2018 = 100), Banco de Información Económica (BIE), INEGI, url: tinyurl.com/indice-precios.\n", 
                       str_wrap("Notas: Mientras que las cifras de 2007 a 2014 corresponden al presupuesto anual aprobado al Instituto Federal Electoral (IFE), a partir de 2015 reflejan el presupuesto anual asignado al Instituto Nacional Electoral (INE). Para deflactar el presupuesto primero cambié el año base del índice de precios implícitos del PIB para que fuera 2023 y después deflacté. Dado que esta gráfica forma parte de una serie dedicada a explorar \"cómo han evolucionado diversos fenómenos económicos, políticos y sociales en México a partir del 1 de diciembre de 2006\" (tinyurl.com/como-ha-cambiado-01), incluyo los apellidos de los últimos tres presidentes para mantener la consistencia con visualizaciones de entregas anteriores, así como para ofrecer cierto contexto a la audiencia. Su propósito no es sugerir relación causal alguna.", width = 193))) +
  tema +
  theme(plot.title = element_markdown(size = 35.5),
        plot.subtitle = element_markdown(size = 25),
        panel.grid = element_blank(),
        axis.text = element_text(size = 16),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 1),
        legend.position = "none")

ggsave("03_vis/04_presupuesto_electoral_federal/g1_evolucion_presupuesto_ine_2007_2023.png", dpi = 300, width = 16, height = 12)



### Gráfica 2 ----
bd_pef %>% 
  mutate(punto_ultimo_valor = if_else(año == 2023, presupuesto_aprobado_def/1e6, NA),
         etiqueta_ultimo_valor = if_else(año == 2023, str_c(dollar(x = round(presupuesto_aprobado_def/1e6)), "\nmillones"), NA)) %>% 
  ggplot(aes(x = años_transcurridos,
             y = presupuesto_aprobado_def/1e6,
             color = colores_barras)) +
  geomtextpath::geom_textline(aes(label = presidente), linewidth = 1.5, family = "Noto Sans JP", size = 7, hjust = 0.88, fontface = "bold") +
  geom_point(aes(x = años_transcurridos,
                 y = punto_ultimo_valor),
             color = "#a50f15", 
             size = 5) +
  geom_text(aes(x = años_transcurridos,
                y = punto_ultimo_valor,
                label = etiqueta_ultimo_valor), 
            fontface = "bold",
            family = "Noto Sans JP",
            color = "#a50f15", 
            hjust = 0.7, 
            vjust = 1.5,
            lineheight = 0.9,
            size = 5) +
  # Etiqueta eje y
  annotate(geom = "text", label = "Millones de pesos constantes (año base 2023)", angle = 90, x = 0.88, y = 14700, hjust = 0, size = 6, fontface = "bold", family = "Noto Sans JP",  color = "grey30", lineheight = 0.9) +
  scale_x_continuous(breaks = seq(from = 0, to = 5, by = 1),
                     limits = c(0.8, 5.1),
                     expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(from = 0, to = 30000, by = 2500),
                     expand = c(0, 0),
                     limits = c(14700, 30800),
                     labels = dollar_format(accuracy = 1, prefix = "$")) +
  scale_color_identity() +
  labs(title = "En el <span style='color:#a50f15'>tercer</span> y <span style='color:#a50f15'>cuarto</span> años de la actual administración el <span style='color:#c31c78'>presupuesto</span><br><span style='color:#c31c78'>electoral federal</span> fue menor que seis años atrás, respectivamente...",
       subtitle = "Es la <span style='color:#a50f15'>primera</span> vez que esto ocurre durante el período analizado",
       x = "Años transcurridos\nde la administración",
       y = NULL,
       caption = str_c("Elaborado por Sebastián Garrido de Sierra / @segasi\nFuentes: Presupuesto de egresos de la federación 2007-2023, Cámara de Diputados. En todos los casos obtuve las cifras del anexo titulado \"Adecuaciones aprobadas por la H. Cámara de Diputados\";\nÍndice de precios implícitos del PIB (Índice base 2018 = 100), Banco de Información Económica (BIE), INEGI, url: tinyurl.com/indice-precios.\n", 
                       str_wrap("Notas: Mientras que las cifras de 2007 a 2014 corresponden al presupuesto anual aprobado al Instituto Federal Electoral (IFE), a partir de 2015 reflejan el presupuesto anual asignado al Instituto Nacional Electoral (INE). Para deflactar el presupuesto primero cambié el año base del índice de precios implícitos del PIB para que fuera 2023 y después deflacté. Dado que esta gráfica forma parte de una serie dedicada a explorar \"cómo han evolucionado diversos fenómenos económicos, políticos y sociales en México a partir del 1 de diciembre de 2006\" (tinyurl.com/como-ha-cambiado-01), incluyo los apellidos de los últimos tres presidentes para mantener la consistencia con visualizaciones de entregas anteriores, así como para ofrecer cierto contexto a la audiencia. Su propósito no es sugerir relación causal alguna.", width = 193))) +
  tema +
  theme(plot.title = element_markdown(size = 37, lineheight = 1.1),
        plot.subtitle = element_markdown(size = 25),
        legend.position = "none")

ggsave("03_vis/04_presupuesto_electoral_federal/g2_evolucion_anual_por_admins_presupuesto_ine_2007_2023.png", dpi = 300, width = 16, height = 12)



### Gráfica 3 ----
bd_pef %>%
  ggplot(aes(x = año,
             y = prop_aprobado_vs_proyecto,
             fill = colores_barras)) +
  # Recuadro gris
  annotate(geom = "rect",
           xmin = 2012.5, xmax = 2018.5,
           ymin = -Inf, ymax = Inf,
           color = "grey90", alpha = 0.2) +
  geom_col() +
  geom_text(aes(label = str_c(round(x = prop_aprobado_vs_proyecto * 100, digits = 1), "%")), 
            fontface = "bold",
            family = "Noto Sans JP",
            color = "white", 
            vjust = 1.5,
            size = 5, 
            lineheight = 0.95) +
  scale_fill_identity() +
  # Etiqueta eje y
  annotate(geom = "text", label = "Porcentaje del presupuesto\ solicitado que fue aprobado", angle = 90, x = 2006.2, y = 0, hjust = 0, size = 6, fontface = "bold", family = "Noto Sans JP",  color = "grey30", lineheight = 0.9) +
  # Etiqueta Calderón
  annotate(geom = "text", label = "Calderón", x = 2009.55, y = 1.1, hjust = 0.5, size = 9, fontface = "bold", family = "Noto Sans JP", color = "grey50") +
  # Etiqueta Peña Nieto
  annotate(geom = "text", label = "Peña Nieto", x = 2015.55, y = 1.1, hjust = 0.5, size = 9, fontface = "bold", family = "Noto Sans JP", color = "white") +
  # Etiqueta López Obrador
  annotate(geom = "text", label = "López Obrador", x = 2021.3, y = 1.1, hjust = 0.5, size = 9, fontface = "bold", family = "Noto Sans JP", color = "grey50") +
  scale_x_continuous(breaks = 2007:2023,
                     limits = c(2006, 2023.5),
                     expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 1.15)) +
  labs(title = "En <span style='color:#a50f15'>2022</span> y <span style='color:#a50f15'>2023</span> el <span style='color:#c31c78'>presupuesto electoral federal</span> aprobado representó<br>poco más del <span style='color:#a50f15'>80%</span> solicitado...",
       subtitle = "Durante los 15 años previos el monto aprobado <span style='color:#a50f15'>nunca</span> fue menor al <span style='color:#a50f15'>91%</span> solicitado", 
       x = "",
       y = NULL,
       caption = str_c("Elaborado por Sebastián Garrido de Sierra / @segasi\nFuentes: Presupuesto de egresos de la federación 2007-2023, Cámara de Diputados. En todos los casos obtuve las cifras del anexo titulado \"Adecuaciones aprobadas por la H. Cámara de Diputados\";\nÍndice de precios implícitos del PIB (Índice base 2018 = 100), Banco de Información Económica (BIE), INEGI, url: tinyurl.com/indice-precios.\n", 
                       str_wrap("Notas: Mientras que las cifras de 2007 a 2014 corresponden al presupuesto anual aprobado al Instituto Federal Electoral (IFE), a partir de 2015 reflejan el presupuesto anual asignado al Instituto Nacional Electoral (INE). Dado que esta gráfica forma parte de una serie dedicada a explorar \"cómo han evolucionado diversos fenómenos económicos, políticos y sociales en México a partir del 1 de diciembre de 2006\" (tinyurl.com/como-ha-cambiado-01), incluyo los apellidos de los últimos tres presidentes para mantener la consistencia con visualizaciones de entregas anteriores, así como para ofrecer cierto contexto a la audiencia. Su propósito no es sugerir relación causal alguna.", width = 195))) +
  tema +
  theme(plot.title = element_markdown(size = 36.5, lineheight = 1.1),
        plot.subtitle = element_markdown(size = 25),
        panel.grid = element_blank(),
        axis.text = element_text(size = 16),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 1),
        legend.position = "none")

ggsave("03_vis/04_presupuesto_electoral_federal/g3_evolucion_por_aprobado_vs_proyecto_ine_2007_2023.png", dpi = 300, width = 16, height = 12)





  

### Gráfica 4 ----
bd_pef %>%
  select(año, prop_aprobado_vs_total) %>% 
  mutate(por_aprobado_vs_total = prop_aprobado_vs_total * 100) %>% 
  arrange(-por_aprobado_vs_total)

bd_pef %>%
  select(año, prop_aprobado_vs_total) %>% 
  mutate(por_aprobado_vs_total = prop_aprobado_vs_total * 100)

bd_pef %>%
  select(año, "Presupuesto electoral federal" = prop_aprobado_vs_total) %>% 
  mutate("Resto del presupuesto" = 1 - `Presupuesto electoral federal`) %>% 
  pivot_longer(-año,
               names_to = "tipo",
               values_to = "proporcion") %>% 
  ggplot(aes(x = año,
             y = proporcion,
             fill = tipo)) +
  # Recuadro gris
  annotate(geom = "rect",
           xmin = 2012.5, xmax = 2018.5,
           ymin = -Inf, ymax = Inf,
           color = "grey50", 
           fill = "transparent",
           alpha = 0.2) +
  # Etiqueta Calderón
  annotate(geom = "text", label = "Calderón", x = 2009.55, y = 1.05, hjust = 0.5, size = 9, fontface = "bold", family = "Noto Sans JP", color = "#00308E") +
  # Etiqueta Peña Nieto
  annotate(geom = "text", label = "Peña Nieto", x = 2015.55, y = 1.05, hjust = 0.5, size = 9, fontface = "bold", family = "Noto Sans JP", color = "#004e22") +
  # Etiqueta López Obrador
  annotate(geom = "text", label = "López Obrador", x = 2021.3, y = 1.05, hjust = 0.5, size = 9, fontface = "bold", family = "Noto Sans JP", color = "#a50f15") +
  geom_col() +
  geom_hline(yintercept = seq(from = 0.1, to = 0.9, by = 0.1),
             color = "white",
             linewidth = 0.3,
             linetype = 3) +
  scale_x_continuous(breaks = 2007:2023,
                     limits = c(2006.5, 2023.5),
                     expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 1.1),
                     breaks = seq(from = 0, to = 1, by = 0.1),
                     labels = percent,
                     sec.axis = dup_axis()) +
  scale_fill_manual(values = c("#c31c78", "grey50")) +
  labs(title = "En los últimos <span style='color:#a50f15'>17 años</span> el <span style='color:#c31c78'>presupuesto electoral federal</span> siempre ha<br>representado menos del <span style='color:#a50f15'>0.46%</span> del presupuesto federal total...",
       subtitle = "Y en <span style='color:#a50f15'>cuatro</span> de los últimos <span style='color:#a50f15'>cinco</span> años ha sido inferior al <span style='color:#a50f15'>0.28%</span>", 
       x = "",
       y = NULL,
       caption = str_c("Elaborado por Sebastián Garrido de Sierra / @segasi\nFuentes: Presupuesto de egresos de la federación 2007-2023, Cámara de Diputados. En todos los casos obtuve las cifras del anexo titulado \"Adecuaciones aprobadas por la H. Cámara de Diputados\".\n", 
                       str_wrap("Notas: Mientras que las cifras de 2007 a 2014 corresponden al presupuesto anual aprobado al Instituto Federal Electoral (IFE), a partir de 2015 reflejan el presupuesto anual asignado al Instituto Nacional Electoral (INE). Dado que esta gráfica forma parte de una serie dedicada a explorar \"cómo han evolucionado diversos fenómenos económicos, políticos y sociales en México a partir del 1 de diciembre de 2006\" (tinyurl.com/como-ha-cambiado-01), incluyo los apellidos de los últimos tres presidentes para mantener la consistencia con visualizaciones de entregas anteriores, así como para ofrecer cierto contexto a la audiencia. Su propósito no es sugerir relación causal alguna.", width = 195)),
       fill = NULL) +
  tema +
  theme(plot.title = element_markdown(size = 38, lineheight = 1.1),
        plot.subtitle = element_markdown(size = 25),
        panel.grid = element_blank(),
        axis.text = element_text(size = 16),
        axis.title.x = element_text(size = 1),
        legend.position = c(0.8, -0.08),
        legend.direction = "horizontal")


ggsave("03_vis/04_presupuesto_electoral_federal/g4_proporcion_presupuesto_federal_electoral_vs_total_2007_2023.png", dpi = 300, width = 16, height = 12)



### Gráfica 5 ----
bd_pef %>%
  select(año, prop_pp) %>% 
  mutate(por_pp = prop_pp * 100) %>% 
  arrange(-por_pp)

bd_pef %>%
  select(año, prop_pp) %>% 
  mutate(por_pp = prop_pp * 100)

bd_pef %>%
  select(año, "Financiamiento de partidos políticos" = prop_pp) %>% 
  mutate("Gasto de operación de la autoridad electoral" = 1 - `Financiamiento de partidos políticos`) %>% 
  pivot_longer(-año,
               names_to = "tipo",
               values_to = "proporcion") %>% 
  mutate(tipo = fct_relevel(tipo, "Gasto de operación de la autoridad electoral", "Financiamiento de partidos políticos")) %>% 
  ggplot(aes(x = año,
             y = proporcion,
             fill = tipo)) +
  # Recuadro gris
  annotate(geom = "rect",
           xmin = 2012.5, xmax = 2018.5,
           ymin = -Inf, ymax = Inf,
           color = "grey50", 
           fill = "transparent",
           alpha = 0.2) +
  # Etiqueta Calderón
  annotate(geom = "text", label = "Calderón", x = 2009.55, y = 1.05, hjust = 0.5, size = 9, fontface = "bold", family = "Noto Sans JP", color = "#00308E") +
  # Etiqueta Peña Nieto
  annotate(geom = "text", label = "Peña Nieto", x = 2015.55, y = 1.05, hjust = 0.5, size = 9, fontface = "bold", family = "Noto Sans JP", color = "#004e22") +
  # Etiqueta López Obrador
  annotate(geom = "text", label = "López Obrador", x = 2021.3, y = 1.05, hjust = 0.5, size = 9, fontface = "bold", family = "Noto Sans JP", color = "#a50f15") +
  geom_col() +
  geom_hline(yintercept = seq(from = 0.1, to = 0.9, by = 0.1),
             color = "white",
             linewidth = 0.3,
             linetype = 3) +
  scale_x_continuous(breaks = 2007:2023,
                     limits = c(2006.5, 2023.5),
                     expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 1.1),
                     breaks = seq(from = 0, to = 1, by = 0.1),
                     labels = percent,
                     sec.axis = dup_axis()) +
  scale_fill_manual(values = c("#c31c78", "grey50")) +
  labs(title = "Al menos el <span style='color:#a50f15'>26%</span> del <span style='color:#c31c78'>presupuesto electoral federal</span> aprobado en cada uno<br>de los ultimos <span style='color:#a50f15'>17 años</span>, se ha destinado al financiamiento de los partidos...",
       subtitle = "El resto fue asignado al gasto de operación de la <span style='color:#c31c78'>autoridad electoral federal</span>", 
       x = "",
       y = NULL,
       caption = str_c("Elaborado por Sebastián Garrido de Sierra / @segasi\nFuentes: Presupuesto de egresos de la federación 2007-2023, Cámara de Diputados. En todos los casos obtuve las cifras del anexo titulado \"Adecuaciones aprobadas por la H. Cámara de Diputados\";\nAcuerdos del Consejo General del IFE e INE aprobados entre 2006 y 2022.\n", 
                       str_wrap("Notas: Mientras que las cifras de 2007 a 2014 corresponden al presupuesto anual aprobado al Instituto Federal Electoral (IFE), a partir de 2015 reflejan el presupuesto anual asignado al Instituto Nacional Electoral (INE). Dado que esta gráfica forma parte de una serie dedicada a explorar \"cómo han evolucionado diversos fenómenos económicos, políticos y sociales en México a partir del 1 de diciembre de 2006\" (tinyurl.com/como-ha-cambiado-01), incluyo los apellidos de los últimos tres presidentes para mantener la consistencia con visualizaciones de entregas anteriores, así como para ofrecer cierto contexto a la audiencia. Su propósito no es sugerir relación causal alguna.", width = 195)),
       fill = NULL) +
  tema +
  theme(plot.title = element_markdown(size = 34, lineheight = 1.1),
        plot.subtitle = element_markdown(size = 25),
        panel.grid = element_blank(),
        axis.text = element_text(size = 16),
        axis.title.x = element_text(size = 1),
        legend.position = c(0.715, -0.08),
        legend.direction = "horizontal")

ggsave("03_vis/04_presupuesto_electoral_federal/g5_proporcion_presupuesto_federal_electoral_partidos_vs_autoridad_electoral_2007_2023.png", dpi = 300, width = 16, height = 12)







