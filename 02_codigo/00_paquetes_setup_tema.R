### Paquetes ----
pacman::p_load(cowplot, geomtextpath, gganimate, GGally, ggforce, ggmap, ggrepel, ggridges, gifski, geomtextpath, ggtext, httr, janitor, jsonlite, leaflet, leaflet.extras, pdftools, RColorBrewer, rcartocolor, readxl, scales, sf, shadowtext, tidyverse,  treemapify, wesanderson, zoo)

### Setup general ----
Sys.setlocale("LC_ALL", "es_ES.UTF-8") 
options(scipen = 9999)
theme_set(theme_gray())

### Definir tema de gráficas ----
tema <-  
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white", color = "white"),
        text = element_text(family = "Roboto", color = "grey45"),
        plot.title = element_text(size = 30, face = "bold", margin = margin(t = 10, r = 0, b = 20, l = 0), family = "Roboto Black", color = "grey15"),
        plot.title.position = "plot",
        plot.subtitle = element_text(size = 22, face = "bold", color = "grey30", margin = margin(0, 0, 20, 0), family = "Noto Sans JP"),
        plot.caption = element_text(hjust = 0, size = 13),
        plot.caption.position = "plot",
        panel.grid = element_line(linetype = 3, color = "grey90"), 
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.title = element_text(size = 16, face = "bold", family = "Roboto Black"),
        legend.text = element_text(size = 14, family = "Noto Sans JP"),
        legend.title.align = 0.5,
        axis.title = element_text(size = 22, hjust = 1, face = "bold", family = "Noto Sans JP", color = "grey30"),
        axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 15, l = 0)), 
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 10)), 
        axis.text = element_text(size = 18, family = "Noto Sans JP"),
        strip.background = element_rect(fill = "grey40", color  = "grey40"),
        strip.text = element_text(color = "white", size = 18))


