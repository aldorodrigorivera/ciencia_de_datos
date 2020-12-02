library(tidyverse)

#Cargamos los datos
datos <- read_delim('data/ng-clients.csv',
            delim =',',
            col_types = cols(
                edad = col_integer(),
                id = col_character()
            ))%>%
            mutate(
                dependientes = ifelse(is.na(dependientes), 0, dependientes),
                edo_civil = ifelse(is.na(edo_civil), 'SOLTERO', edo_civil),
                edad = ifelse(is.na(edad), 0, edad),
                edad = ifelse(edad == 0, median(data.frame(datos)$edad), edad),
                edad = ifelse(edad > 100, median(data.frame(datos)$edad), edad),
                genero = ifelse(is.na(genero), 'Masculino', genero),
                fecha_conversion = substr(fecha_conversion,5,15),
            )

# Calculamos la media para sustituir las edades inválidas
print('EdadMedia')
print(median(data.frame(datos)$edad))

# Definimos el formato de la fecha de
print('Fecha formato')
formated <- substr(datos$fecha_conversion,5,15)

#Creamos un nuevo csv
write.csv(datos,'data/clients.csv', row.names = FALSE)

View(datos)