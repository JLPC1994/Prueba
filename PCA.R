# Principal component analysis (PCA) --------------------------------------
#Datos de coberturas vegetales (Mosaico Cultivos, Pastos, Bosques, Ganaderia)
# Ingresar datos ----------------------------------------------------------

## Datos
  
setwd() #Ingresar el directorio de trabajo
#getwd() 
dir() #Elementos del Directorio

#Importar base de datos en csv
Database <- read.table("PCA_matriz.csv", header=TRUE, sep=";")
Database

## Escoger las variables contuinuas para el PCA
Variables <- Database[, 4:10]
Variables

## Determinar la varianza de las variables
library(dplyr) #cargar paquete
Variables %>% dplyr::summarise(across(where(is.numeric),~var(.x, na.rm = TRUE)))

#Clase de las variables
sapply(Variables, class) #Convertir a numeric aquellas en character

# Estandarizar variables --------------------------------------------------

# Estandarizar variables. Revisar si este paso es necesario.
library(vegan) #paquete
Variables_pad <- decostand(x = Variables, method = "standardize") #standardize: scale x to zero mean and unit variance 
Variables_pad

#Verificar variaza. Debe ser 1 y media 0
Variables_pad %>% dplyr::summarise(across(where(is.numeric),~var(.x, na.rm = TRUE)))


# PCA ---------------------------------------------------------------------

## PCA
library(factoextra)
library(FactoMineR)
#Results for the Principal Component Analysis (PCA)
PCA.p <- PCA(X = Variables, scale.unit = TRUE, graph = FALSE)
PCA.p

## Autovalores: porcentage de explicación
PCA.p$eig

## Visualizar porcentage de explicacion por eje o componente
fviz_screeplot(PCA.p, addlabels = TRUE, ylim = c(0, 100), main = "",xlab = "Dimensiones",ylab = "Porcentage de variancia explicada")

## Variables (Principal Component Analysis Results for variables)
var <- get_pca_var(PCA.p)
var

## Posicion de variables
var$coord

## Contribuicion (%) de variables en cada eje
var$contrib

## Loadings - correlacion entre variables y ejes
var$cor

#Link between the variable and the continuous variables (R-square)
## Variable importantes para el eje 1
dimdesc(PCA.p)$Dim.1

## Variable importantes para el eje 2
dimdesc(PCA.p)$Dim.2

## Variable importantes para el eje 3
dimdesc(PCA.p)$Dim.3


# Graficar componentes ----------------------------------------------------

library(RColorBrewer)

#PC1 y PC2
fviz_pca_biplot(X = pca.p, geom.ind = "point",
                axes = c(1, 2),
                fill.ind = Database$Coberturas,
                col.ind = "black",
                alpha.ind = 0.7,
                addEllipses = FALSE,
                pointshape = 21,
                pointsize = 3,
                palette = c("#1f78b4", "#33a02c", "#e31a1c", "#ff7f00"),
                col.var = "black",
                invisible = "quali",
                title = "PC1-PC2") + labs(x = "PC1 (23.6%)", y = "PC2 (18.4%)")


#PC1 y PC3

fviz_pca_biplot(X = pca.p, geom.ind = "point",
                fill.ind = Database$CoberturasCO,
                axes = c(1, 3),
                col.ind = "black",
                alpha.ind = 0.7,
                addEllipses = FALSE,
                pointshape = 21,
                pointsize = 3,
                palette = c("#1f78b4", "#33a02c", "#e31a1c", "#ff7f00"),
                col.var = "black",
                invisible = "quali",
                title = "PC1-PC3") + labs(x = "PC1 (23.6%)", y = "PC3 (15.9%)")

#PC2 y PC3

fviz_pca_biplot(X = pca.p, geom.ind = "point",
                fill.ind = Database$Coberturas,
                axes = c(2, 3),
                col.ind = "black",
                alpha.ind = 0.7,
                addEllipses = FALSE,
                pointshape = 21,
                pointsize = 3,
                palette = c("#1f78b4", "#33a02c", "#e31a1c", "#ff7f00"),
                col.var = "black",
                invisible = "quali",
                title = "PC2-PC3") + labs(x = "PC2 (18.4%)", y = "PC3 (15.9%)")