*************************************************************************************
Master Universitario en Ciencia de datos

Asignatura: Tipología y ciclo de vida de los datos aula 1

Práctica 2: Limpieza y validación de los datos

Estudiante: José Ahias Lopez Portillo
*************************************************************************************

# Red Wine Quality (UCI Machine Learning)


## Descripción

### Información del conjunto de datos

•	DataSet: Red Wine Quality (UCI Machine Learning)
•	Url DataSet: https://www.kaggle.com/uciml/red-wine-quality-cortez-et-al-2009
•	Descripción: Los dos conjuntos de datos están relacionados con variantes rojas y blancas del vino portugués "Vinho Verde".

Debido a problemas de privacidad y logística, solo variables fisicoquímicas (entradas) y sensoriales (el resultado) están disponibles.

### Metadata

Columnas
fixed acidity
volatile acidity
citric acid
residual sugar
chlorides
free sulfur dioxide
total sulfur dioxide
density
pH
sulphates
alcohol
quality (score between 0 and 10)

### Objetivo del análisis científico

El objetivo principal de este análisis científico es poder crear un modelo predictivo de clasificación que permitan identificar si un vino es bueno o malo, para ello tomaremos 11 columnas de entrada predictiva y adicionaremos una columna de predicción binaria en base a la condición “quality>5”, que al crear, entrenar y validar el modelo por medio de un algoritmo de “Random Forests” podremos predecir fácilmente si los vinos son buenos o malos.




