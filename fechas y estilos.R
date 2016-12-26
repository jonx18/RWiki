library("jsonlite")#Cargo libreria de JSON
jsonfile <- list(julio="",papa="",johnny="",obama="")
jsonfile$julio <- readLines("C:\\Users\\Jonx\\Downloads\\WikiAnalisis\\page334671.txt")#Leo Archivo de Julio Cortazar
jsonfile$papa <- readLines("C:\\Users\\Jonx\\Downloads\\WikiAnalisis\\page23056.txt")#Leo Archivo del Papa
jsonfile$johnny <- readLines("C:\\Users\\Jonx\\Downloads\\WikiAnalisis\\page71870.txt")#Leo Archivo de Johnny Depp
jsonfile$obama <- readLines("C:\\Users\\Jonx\\Downloads\\WikiAnalisis\\page534366.txt")#Leo Archivo del Barack Obama
#Por cada archivo tengo
#[1] Listado de fechas de revisiones
#[2] Cambios de estilo en toda la historia del articulo
#[3] Toral de revisiones
#[4] Distribucion de aporte por autor
#[5] Cantidad de revisiones dia a dia
#[6] Contenido revision a revision
#[7] Listado de nombres de categoria revision a revision
#json <- fromJSON(jsonfile[2])#Obtengo datos de estilos Ejemplo
json <- list(julio=fromJSON(jsonfile$julio[2]),papa=fromJSON(jsonfile$papa[2]),
             johnny=fromJSON(jsonfile$johnny[2]),obama=fromJSON(jsonfile$obama[2]))
#df <- data.frame(json)#Creo data frame con datos de estilos Ejemplo, no olvidarme de agregar nombes
df.articulos <- list(julio=data.frame(json$julio),papa=data.frame(json$papa),
             johnny=data.frame(json$johnny),obama=data.frame(json$obama))
#json <- fromJSON(jsonfile[6])#Obtengo datos de fecha hora y peso EJEMPLO
json <- list(julio=fromJSON(jsonfile$julio[6]),papa=fromJSON(jsonfile$papa[6]),
             johnny=fromJSON(jsonfile$johnny[6]),obama=fromJSON(jsonfile$obama[6]))
#strptime(names(json)[1],format="%Y-%m-%d %H:%M:%S.0")#ejemplo de como formartear una fecha y hora
#df$content_size <- json#Cargo en el data frame pesos Ejeemplo
#df$date <- names(json)#Cargo en el data frame fechas
#df.articulos$johnny<-df.articulos$johnny[1:(nrow(df.articulos$johnny)-1),] # en caso de problemas de tamaño
df.articulos$julio$content_size <- json$julio
df.articulos$papa$content_size <- json$papa
df.articulos$johnny$content_size <- json$johnny
df.articulos$obama$content_size <- json$obama

df.articulos$julio$date <- names(json$julio)
df.articulos$papa$date <- names(json$papa)
df.articulos$johnny$date <- names(json$johnny)
df.articulos$obama$date <- names(json$obama)
#lapply(names(json$julio),function(x) which(df.articulos$julio$date == x))#obtengo todos los indices
#which(x == a) devuelve un vector de los ´indices de x si la operaci´on es (TRUE) (en este ejemplo, los valores
#de i para los cuales x[i] == a). El argumento de esta funci´on debe ser una variable de
#tipo l´ogico