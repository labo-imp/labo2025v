# Arbol elemental con libreria  rpart
# Debe tener instaladas las librerias  data.table ,  rpart  y  rpart.plot
# Correr en Google Cloud con RStudio

# cargo las librerias que necesito
require("data.table")
require("rpart")
require("rpart.plot")

# Aqui se debe poner la carpeta de la materia de SU computadora local
setwd("~/buckets/b1") # Establezco el Working Directory

# cargo el dataset pequeno  del disco local
dataset <- fread("~/datasets/dataset_pequeno.csv")

dtrain <- dataset[foto_mes == 202107] # defino donde voy a entrenar

# genero el modelo,  aqui se construye el arbol
# quiero predecir clase_ternaria a partir de el resto de las variables
modelo_final <- rpart(
    formula = "clase_ternaria ~ .",
    data = dtrain, # los datos donde voy a entrenar
    xval = 0,
    cp = -0.3, # esto significa no limitar la complejidad de los splits
    minsplit = 0, # minima cantidad de registros para que se haga el split
    minbucket = 1, # tamaño minimo de una hoja
    maxdepth = 3  # profundidad maxima del arbol
)


# grafico el arbol
prp(modelo_final,
    extra = 101, digits = -5,
    branch = 1, type = 4, varlen = 0, faclen = 0
)


dapply <- dataset[foto_mes == 202109] # defino donde voy a aplicar el modelo

# aplico el modelo a los datos nuevos
prediccion <- predict(
    object = modelo_final,
    newdata = dapply,
    type = "prob"
)

# prediccion es una matriz con TRES columnas,
# llamadas "BAJA+1", "BAJA+2"  y "CONTINUA"
# cada columna es el vector de probabilidades

# agrego a dapply una columna nueva que es la probabilidad de BAJA+2
tb_prediccion <- as.data.table(list( 
    "numero_de_cliente" = dapply$numero_de_cliente,
    "prob"=prediccion[, "BAJA+2"] 
  ))

# solo le envio estimulo a los registros
#  con probabilidad de BAJA+2 mayor  a  1/40
tb_prediccion[, Predicted := as.numeric(prob > 1 / 40)]


# genero el archivo para Kaggle
# primero creo la carpeta donde va el experimento
dir.create("./exp/")
dir.create("./exp/KA2001")

# solo los campos para Kaggle
fwrite(tb_prediccion[, list(numero_de_cliente, Predicted)],
        file = "./exp/KA2001/K101_001.csv",
        sep = ","
)
