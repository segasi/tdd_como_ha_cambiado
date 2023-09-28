### Cargar paquetes, definir setup y tema de gráficas -----
source("02_codigo/00_paquetes_setup_tema.R")

### Importar datos ----

## Base de datos de matrimonios ----

# Fuente: INEGI, https://www.inegi.org.mx/sistemas/olap/registros/vitales/nupcialidad/tabulados/Mat02.asp?t=02&c=11840
bd_matrimonios <- 
  read_csv("01_bd/03_matrimonios/INEGI_exporta_27_9_2023_7_57_47.csv", 
         locale = locale(encoding = "WINDOWS-1252"),
         skip = 8) %>% 
  clean_names()

## Base de datos de conciliación demográfica y proyecciones poblacionales ----

# Fuente: CONAPO, https://www.gob.mx/conapo/acciones-y-programas/conciliacion-demografica-de-1950-a-2019-y-proyecciones-de-la-poblacion-de-mexico-y-de-las-entidades-federativas-2020-a-2070

bd_poblacion <- 
  read_excel("01_bd/conapo/ConDem50a19_ProyPob20a70/0_Pob_Mitad_1950_2070.xlsx") %>% 
  clean_names()


### Realizar diversas transformaciones a bd_matrimonios y generar objeto bd_matrimonios_totales ----
bd_matrimonios_totales <- 
  bd_matrimonios %>% 
  # Mantener columnas de interés 
  select(1:31) %>% 
  # Sólo mantener renglón que corresponde al Total
  filter(str_detect(string = x1, "Total")) %>% 
  # Transformar estructura de base de datos
  pivot_longer(-x1,
               names_to = "año",
               values_to = "num_matrimonios") %>% 
  # Eliminar primera columna
  select(-1) %>%   
  # "Limpiar" valores de la columna año
  mutate(año = str_remove(string = año, pattern = "x"),
         año = str_sub(string = año, start = 1, end = 4),
         año = as.double(x = año),
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
  # Sólo mantener observaciones de 2007 en adelante
  filter(año > 2006)


### Realizar diversas transformaciones a bd_matrimonios y generar objeto bd_matrimonios_x_rangos ----
bd_matrimonios_x_rangos <- 
  bd_matrimonios %>% 
  # Mantener columnas de interés
  select(1:31) %>% 
  # Renombrar variable
  rename(rango_edad = x1) %>% 
  # Sólo mantener renglones que corresponden a un rango de edad
  filter(str_detect(string = rango_edad, "De |Menores")) %>% 
  # Transformar estructura de base de datos
  pivot_longer(-rango_edad,
               names_to = "año",
               values_to = "num_matrimonios") %>% 
  # "Limpiar" valores de la columna año
  mutate(año = str_remove(string = año, pattern = "x"),
         año = str_sub(string = año, start = 1, end = 4),
         año = as.double(x = año),
         # Remplazar NAs por 0s en num_matrimonios
         num_matrimonios = if_else(condition = is.na(num_matrimonios),
                                   true = 0,
                                   false = num_matrimonios),
         # Reordenar niveles de rango_edad
         rango_edad = fct_relevel(rango_edad, "Menores de 15 años", "De 15 a 19 años", "De 20 a 24 años", "De 25 a 29 años", "De 30 a 34 años", "De 35 a 39 años", "De 40 a 44 años", "De 45 a 49 años", "De 50 años y más")) %>% 
  mutate(prop_matri_x_rango_edad = num_matrimonios/sum(x = num_matrimonios),
         .by = año) %>% 
  # Sólo mantener observaciones de 2007 en adelante
  filter(año > 2006)

### Realizar diversas transformaciones a base de datos de proyecciones poblacionales y generar objeto bd_poblacion_15a_y_mas ---- 
bd_poblacion_15a_y_mas <- 
  bd_poblacion %>% 
  # Renombrar variable
  rename(año = ano) %>% 
  # Filtrar observaciones
  filter(cve_geo == 0,
         año > 2006 & año < 2023,
         edad > 14) %>% 
  # Calcular población total por año
  summarise(poblacion_15a_mas = sum(x = poblacion),
            .by = año)

### Unir datos de población anual a bd_matrimonios_totales ----
bd_matrimonios_totales <- 
  bd_matrimonios_totales %>% 
  left_join(bd_poblacion_15a_y_mas,
            by = "año")

### Calcular tasa de matrimonios por cada 100k habitantes ----
bd_matrimonios_totales <- 
  bd_matrimonios_totales %>% 
  mutate(tasa_x_c_100k = round(num_matrimonios/poblacion_15a_mas * 100000, 1))  

### Primera gráfica ----
bd_matrimonios_totales %>%
  ggplot(aes(x = año,
             y = tasa_x_c_100k,
             fill = colores_barras)) +
  # Recuadro gris
  annotate(geom = "rect",
           xmin = 2012.5, xmax = 2018.5,
           ymin = -Inf, ymax = Inf,
           color = "grey90", alpha = 0.2) +
  geom_col() +
  geom_text(aes(label = tasa_x_c_100k), 
            fontface = "bold",
            family = "Noto Sans JP",
            color = "white", 
            vjust = 1.5,
            size = 5) +
  scale_fill_identity() +
  # Etiqueta eje y
  annotate(geom = "text", label = "Tasa de matrimonios por cada 100 mil\nhabitantes con 15 o más años", angle = 90, x = 2006.2, y = 10, hjust = 0, size = 5, fontface = "bold", family = "Noto Sans JP",  color = "grey30", lineheight = 0.9) +
  # Etiqueta Calderón
  annotate(geom = "text", label = "Calderón", x = 2009.55, y = 830, hjust = 0.5, size = 9, fontface = "bold", family = "Noto Sans JP", color = "grey50") +
  # Etiqueta Peña Nieto
  annotate(geom = "text", label = "Peña Nieto", x = 2015.55, y = 830, hjust = 0.5, size = 9, fontface = "bold", family = "Noto Sans JP", color = "white") +
# Etiqueta López Obrador
annotate(geom = "text", label = "López Obrador", x = 2020.6, y = 830, hjust = 0.5, size = 9, fontface = "bold", family = "Noto Sans JP", color = "grey50") +
  scale_x_continuous(breaks = 2007:2022,
                     limits = c(2006, 2022.5),
                     expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 900)) +
  labs(title = "Los <span style='color:#a50f15'>matrimonios</span> en México aumentaron después de la pandemia...",
       subtitle = "Pero su tendencia general sigue a la baja. En 2022 la <span style='color:#a50f15'>tasa de matrimonios</span> fue <span style='color:#a50f15'>34.5%</span> menor que en 2007.", 
       x = "",
       y = NULL,
       caption = "Elaborado por Sebastián Garrido de Sierra / @segasi\nFuentes: Matrimonios por grupos quinquenales de edad de los contrayentes, INEGI, tinyurl.com/matrimonios-mx; Conciliación Demográfica de 1950 a 2019 y Proyecciones de la población de México\ny de las entidades federativas 2020 a 2070, CONAPO, tinyurl.com/poblacion-mx\nNotas: Calculé las tasas de matrimonios considerando las proyecciones poblacionales de CONAPO a mitad de año, para personas con 15 años o más. Dado que esta gráfica forma parte de\nuna serie dedicada a explorar \"cómo han evolucionado diversos fenómenos económicos, políticos y sociales en México a partir del 1 de diciembre de 2006\" (tinyurl.com/como-ha-cambiado-01),\nincluyo los apellidos de los últimos tres presidentes para mantener la consistencia con visualizaciones de entregas anteriores, así como para ofrecer cierto contexto a la audiencia. Su propósito no es\nsugerir relación causal alguna.") +
  tema +
  theme(plot.title = element_markdown(size = 38),
        plot.subtitle = element_markdown(size = 22),
        panel.grid = element_blank(),
        axis.text = element_text(size = 16),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 1),
        legend.position = "none")

ggsave("03_vis/03_matrimonios/evolucion_tasa_matrimonios_2007_2022.png", dpi = 300, width = 16, height = 12)


### Segunda gráfica ----
bd_matrimonios_x_rangos %>% 
  ggplot(aes(x = año,
             y = prop_matri_x_rango_edad,
             fill = rango_edad)) +
  geom_area(color = "white", linewidth = 0.1) +
  # Etiquetas para cada rango de edad
  annotate(geom = "text", label = "De 15 a 19 años", x = 2007.1, y = 0.95, hjust = 0, size = 5, fontface = "bold", family = "Noto Sans JP", color = "white") +
  annotate(geom = "text", label = "De 20 a 24 años", x = 2007.1, y = 0.74, hjust = 0, size = 5, fontface = "bold", family = "Noto Sans JP", color = "white") +
  annotate(geom = "text", label = "De 25 a 29 años", x = 2007.1, y = 0.44, hjust = 0, size = 5, fontface = "bold", family = "Noto Sans JP", color = "white") +
  annotate(geom = "text", label = "De 30 a 34 años", x = 2007.1, y = 0.245, hjust = 0, size = 5, fontface = "bold", family = "Noto Sans JP", color = "white") +
  annotate(geom = "text", label = "De 35 a 39 años", x = 2021.9, y = 0.28, hjust = 1, size = 5, fontface = "bold", family = "Noto Sans JP", color = "white") +
  annotate(geom = "text", label = "De 40 a 44 años", x = 2021.9, y = 0.195, hjust = 1, size = 5, fontface = "bold", family = "Noto Sans JP", color = "white") +
  annotate(geom = "text", label = "De 45 a 49 años", x = 2021.9, y = 0.143, hjust = 1, size = 5, fontface = "bold", family = "Noto Sans JP", color = "white") +
  annotate(geom = "text", label = "De 50 años y más", x = 2021.9, y = 0.06, hjust = 1, size = 5, fontface = "bold", family = "Noto Sans JP", color = "white") +
  scale_x_continuous(breaks = 2007:2022,
                     limits = c(2006.9, 2022.2),
                     expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(-0.02, 1.005),
                     breaks = seq(from = 0, to = 1, by = 0.1),
                     labels = percent,
                     sec.axis = dup_axis()) +
  scale_fill_manual(values = rev(monochromeR::generate_palette("#a50f15",
                                                           modification = "go_lighter",
                                                           n_colours = 9))) +
  labs(title = "Quienes se casan en México, lo hacen cada vez <span style='color:#a50f15'>más tarde</span> en la vida...",
       subtitle = "En 2022 el <span style='color:#a50f15'>54.4%</span> de las personas que contrajeron matrimonio tenían <span style='color:#a50f15'>30 o más años</span>. En 2007 representaban el <span style='color:#a50f15'>30.3%</span>.", 
       x = "",
       y = NULL,
       caption = "Elaborado por Sebastián Garrido de Sierra / @segasi\nFuentes: Matrimonios por grupos quinquenales de edad de los contrayentes, INEGI, tinyurl.com/matrimonios-mx; Conciliación Demográfica de 1950 a 2019 y Proyecciones de la población de\nMéxico y de las entidades federativas 2020 a 2070, CONAPO, tinyurl.com/poblacion-mx\nNotas: La gráfica no incluye la información de las personas que contrajeron matrimonio pero su edad no fue especificada. Aunque la gráfica sí incluye la información de la población menor a 15 años\nque contrajo matrimonio, este conjunto de la población es tan pequeño que es imperceptible en la misma.") +
  tema +
  theme(plot.title = element_markdown(size = 36),
        plot.subtitle = element_markdown(size = 19.8),
        panel.grid = element_blank(),
        axis.text = element_text(size = 16),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 1),
        legend.position = "none")

ggsave("03_vis/03_matrimonios/evolucion_porcentaje_matrimonios_por_rngo_edad_2007_2022.png", dpi = 300, width = 16, height = 12)


# Calculo del porcentaje de personas que se casó cada año y tenía entre 15 a 24 años
bd_matrimonios_x_rangos %>% 
  filter(str_detect(string = rango_edad, pattern = "15 a |20 a")) %>% 
  arrange(año, rango_edad) %>% 
  summarise(prop_subtotal = sum(x = prop_matri_x_rango_edad),
            .by = año)

# Calculo del porcentaje de personas que se casó cada año y tenía 30 o más años
bd_matrimonios_x_rangos %>% 
  filter(str_detect(string = rango_edad, pattern = "De 3|De 4|De 5")) %>% 
  arrange(año, rango_edad) %>% 
  summarise(prop_subtotal = sum(x = prop_matri_x_rango_edad),
            .by = año)

# Calculo del porcentaje de personas que se casó cada año y tenía entre 25 y 29 años
bd_matrimonios_x_rangos %>% 
  filter(str_detect(string = rango_edad, pattern = "De 25"))  
  

    
