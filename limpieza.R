library(tidyverse)
library(ggplot2)
library(ggradar)
suppressPackageStartupMessages(library(dplyr))
library(scales)

csv <-read_delim('data/ng-clients.csv',
            delim =',',
            col_types = cols(
                edad = col_integer(),
                id = col_character()
            ))
#Cargamos los datos
datos <- csv %>%  mutate(
                dependientes = ifelse(is.na(dependientes), 0, dependientes),
                edo_civil = ifelse(is.na(edo_civil), 'SOLTERO', edo_civil),
                edad = ifelse(is.na(edad), 0, edad),
                edad = ifelse(edad == 0, 44, edad),
                edad = ifelse(edad > 100, 44,edad),
                genero = ifelse(is.na(genero), 'Masculino', genero),
                fecha_conversion = substr(fecha_conversion,5,15),
            )

# Calculamos la media para sustituir las edades inválidas
print('EdadMedia')
print(median(data.frame(datos)$edad))

# Definimos el formato de la fecha de
print('Fecha formato')
formated <- substr(datos$fecha_conversion,5,15)

# Creamos un nuevo csv
write.csv(datos,'data/clients.csv', row.names = FALSE)

# Visualizamos la información
View(datos)


# Exploración de datos
segment_graph <- datos %>%
  group_by(segmento) %>%
  summarize(segmento_mean = mean(segmento),
            segmento_max = max(segmento),
            segmento_min = min(segmento),
            segmento_sd = sd(segmento))

datos %>%
  ggplot(aes(x = segmento)) +
  geom_bar()
ggsave("displ_box_plot_segment.png")


datos %>%
  ggplot(aes(x = segmento, y = edad)) +
  geom_boxplot()
ggsave("displ_box_plot_segment_edad.png")


datos %>%
  ggplot(aes(x = edad)) +
  geom_histogram()
ggsave("displ_box_plot_edad.png")


datos %>%
     add_rownames( var = "segmento" ) %>%
     mutate_each(funs(rescale), -segmento) %>%
     tail(4) %>% select(1:10) -> segmento_radar
ggradar(segmento_radar) 
ggsave("displ_spider_segment.png")
