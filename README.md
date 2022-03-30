
# Diagrama de pares con `ggpairs` - modificaciones
Villa-Guerrero, Pablo 

Fecha: 28/3/2022

# Diagrama de pares con `ggpairs`.

`GGally` está diseñado para ayudar a ggplot2 . El paquete R `ggplot2` es un sistema de trazado basado en la gramática de gráficos. `GGally` amplía `ggplot2` añadiendo varias funciones para reducir la complejidad de combinar objetos geométricos con datos transformados. 

Para instalar este paquete desde Github o CRAN , puede ejecutar desde la consola R:

```{r eval=FALSE}
# Github
library(devtools)
install_github("ggobi/ggally")

# CRAN
install.packages("GGally")
```

El paquete `GGally` contiene la función `ggpairs` que es el equivalente en `ggplot2` a la función pairs de R base. Puedes pasar un data frame que contenga tanto variables continuas como categóricas.

```{r  warning=FALSE, message=FALSE}
library(ggplot2)
library(GGally)
```

## Tema en ggplots

Personalizamos la apariencia de los gráficos hechos con `ggplot2` usando un tema denominado `Mytheme`.

```{r warning=FALSE, message=FALSE}
Mytheme <- theme(
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.background = element_rect(fill = "#EAEDED"),
  panel.border = element_rect(colour = NA, fill = NA, size = 1),
  
  axis.title.x = element_text(colour = "black", size = 11, face = 'bold'),
  axis.text.x = element_text(colour = 'black', size = 10),
  axis.title.y = element_text(colour = "black", size = 11, angle = 90, face = 'bold'),
  axis.text.y = element_text(colour = 'black', size = 10), 
  plot.title = element_text(colour = 'black', size = 14, face = 'bold'),
  plot.subtitle = element_text(colour = '#5B3888', size = 12),
  plot.caption = element_text(colour = '#424949', size = 10),
  
  strip.background = element_rect(fill = "#5B3888"),
  strip.text.x = element_text(size = 12, angle = 0, face = "bold", colour = "white"),
  strip.text.y = element_text(size = 12, angle = -90, face = "bold", colour = "white"),
  
  legend.title = element_text(colour = "#5B3888", face = "bold", size = 9),
  legend.text = element_text(colour = "#5B3888", size = 8))
```

## Base de datos

Utilizaremos una base de datos que consta de cinco variables cuantitativas edáficas de un suelo cultivado con banano, del cual se analiza la densidad aparente (da), la densidad real (dr), el porcentaje de porosidad total (ppt), el porcentaje de humedad del suelo (phs) y la resistencia a la compactación del suelo (rps).

```{r warning=FALSE, message=FALSE}
dirSuelo <- "https://raw.githubusercontent.com/pVillaGuerrero/-Correlation-Matrix/main/suelo.csv"
suelo <-read.csv(dirSuelo, header = T, sep = ",")
suelo <- na.omit(suelo)
```

## Modificación de la correlación

Se crea la función `myStyleCORstar`, la cual genera las estrellas de significancia para la correlación.

```{r warning=FALSE, message=FALSE}
myStyleCORstar <- function(data, mapping, color = I("black"), sizeRange = c(1, 5), ...) {
# obtener los datos x e y para usar el otro código
  x <-  GGally::eval_data_col(data, mapping$x)
  y <- GGally::eval_data_col(data, mapping$y)
  
  ct <- cor.test(x,y)
  sig <- symnum(
    ct$p.value, corr = FALSE, na = FALSE,
    cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
    symbols = c("***", "**", "*", ".", " ")
  )
  r <- unname(ct$estimate)
  rt <- format(r, digits=2)[1]
# Dado que no podemos imprimirlo para obtener el tamaño de la estrella, solo use el rango de tamaño máximo
  cex <- max(sizeRange)
# Función auxiliar para calcular un tamaño utilizable
  percent_of_range <- function(percent, range) {
    percent * diff(range) + min(range, na.rm = TRUE)
  }
# Graficar el valor cor
  ggally_text(
    label = as.character(rt), 
    mapping = aes(),
    xP = 0.5, yP = 0.5, 
    size = I(percent_of_range(cex * abs(r), sizeRange)),
    color = color,
    ...
  ) + 
# Agregar las estrellas de significancia
    geom_text(
      aes_string(
        x = 0.8,
        y = 0.8
      ),
      label = sig, 
      size = I(cex),
      color = color,
      ...
    )
}
```

Generamos la matriz con las variables de la base de datos `suelo` con la función `GGally::ggpairs()`.

```{r warning=FALSE, message=FALSE}
ggpairs(suelo, 
        lower = list(continuous = wrap("points", 
                                       shape = 1,
                                       col="#5B3888")),
        diag = list(continuous = wrap("barDiag", 
                                      col="white",
                                      fill="#5B3888")),
        upper = list(continuous = myStyleCORstar))+
  Mytheme
```
![Image text](https://raw.githubusercontent.com/pVillaGuerrero/GGally---ggpairs/main/000001.png)

## Modificación del gráfico de dispersión

```{r warning=FALSE, message=FALSE}
mySMOOTH <- function(data, mapping, ...) {
  ggplot(data = data, mapping = mapping) +
    geom_point(color = I("#5B3888"), shape=1) + 
    geom_smooth(method = "lm", color = I("red"), se=FALSE, linetype = "dashed", ...)
}
```

Generamos la matriz con las variables de la base de datos `suelo` con la función `GGally::ggpairs()`.

```{r warning=FALSE, message=FALSE}
ggpairs(suelo, 
        lower = list(continuous = mySMOOTH),
                                       #fill="white")), #5B3888
        diag = list(continuous = wrap("barDiag", 
                                      col="white",
                                      #alpha = 0.5,
                                      fill="#5B3888"
                                      )),
        upper = list(continuous = myStyleCORstar))+
  Mytheme
```
![Image text](https://raw.githubusercontent.com/pVillaGuerrero/GGally---ggpairs/main/000002.png)

## Modificación de la correlación por colores

Se selecciona los colores para representar la correlación.

```{r warning=FALSE, message=FALSE}
# Seleccionar los 5 colores intermedios de los 7 colores para que el color extremo no sea tan intenso.
corColors <- RColorBrewer::brewer.pal(n = 7, name = "RdYlGn")[2:6]
corColors
```
Se crea la función para que coloque la correlación por colores.

```{r warning=FALSE, message=FALSE}
myCORcolor <- function(data, mapping, color = I("black"), sizeRange = c(1, 5), ...) {

# obtener los datos x e y para usar el otro código
  x <- GGally::eval_data_col(data, mapping$x)
  y <- GGally::eval_data_col(data, mapping$y)

  ct <- cor.test(x,y)

  r <- unname(ct$estimate)
  rt <- format(r, digits=2)[1]
  tt <- as.character(rt)

# Graficar el valor cor
  p <- ggally_text(
   label = tt, 
   mapping = aes(),
   xP = 0.5, yP = 0.5, 
   size = 6,
   color=color,
   ... ) +
  Mytheme

  corColors <- RColorBrewer::brewer.pal(n = 7, name = "RdYlGn")[2:6]

  if (r <= -0.8) {
    corCol <- corColors[1]
  } else if (r <= -0.6) {
    corCol <- corColors[2]
  } else if (r < 0.6) {
    corCol <- corColors[3]
  } else if (r < 0.8) {
    corCol <- corColors[4]
  } else {
    corCol <- corColors[5]
  }
  p <- p + theme(
    panel.background = element_rect(fill= corCol)
  )

  p
}

```

Generamos la matriz con las variables de la base de datos `suelo` con la función `GGally::ggpairs()`.

```{r warning=FALSE, message=FALSE}
# no es necesario usar la funcion wrap si no hay parámetros.
# lower$combo se lo eliminó.
ggpairs(
  suelo,
  lower = list(continuous = mySMOOTH),
  upper = list(continuous = myCORcolor),
  diag = list(continuous = wrap("barDiag", col="white", fill="#5B3888")), 
  axisLabels = "none") 
```

![Image text](https://raw.githubusercontent.com/pVillaGuerrero/GGally---ggpairs/main/000003.png)
