
#*******************************************************************************
#*******************************************************************************
#--------------------------------------------------------------------------------
#Dependiendo de la configuración de nuestro entorno puede 
#ser necesario instalar las siguientes librerías
#install.packages("ggplot2")
#install.packages("ggthemes")
#install.packages("corrplot")
#install.packages("reshape2")
#install.packages("dplyr")
#install.packages("randomForest")
#install.packages("ggplot")
#install.packages("Rcpp")
#install.packages("psych")
#install.packages("nortest")
#install.packages("rattle")
#install.packages("ROCR")
#install.packages("caret")
#install.packages("e1071")
options(warn=1)

#---------------------------------------------------------------------------------
#Llamar librerías para nuestro desarrollo de ciencia de datos.
library(nortest)
library(Rcpp)
library(rlang)
library(ggplot2)
library(ggthemes)
library(corrplot)
library(reshape2)
library(dplyr)
library(randomForest)
library(psych)
library(rpart)
library(rpart.plot)
library(caret)
library(ROCR)
library(e1071)
library(rattle)



#*******************************************************************************
#*******************************************************************************
#Inicio de preprocesamiento (limpieza, análisis)

#---------------------------------------------------------------------------------
#---------------------------------------------------------------------------------
##Para poder hacer la lectura es necesario configurar 
#una carpeta de DataSet en el directorio “C:” y copiar el archivo 
#que vamos utilizar para generar nuestro modelo de clasificación.
df_wine<-read.csv("C:/dataset/winequality-red.csv")



#ver un resumen de metadatos
str(df_wine)


# analizar si existen valores no validos
sapply(df_wine, function(x) sum(x=="NULL" || is.na(x) || length((as.character(x)))==0))


##Adicionar una nueva columna que clasifique el vino en bueno o malo
df_wine$IsGood <- ifelse(df_wine$quality>5,1,0)



#Funcion de histograma global
GetHist <-function(df,x){
  if(is.numeric(df[, x])){
    
    ggplot(df, aes_string(x)) +
      geom_density(alpha=0.05)+
      scale_x_continuous(breaks = seq(0,20,0.1))+
      geom_vline(aes(xintercept = mean(df[, x])),col='red',size=2)+
      ggtitle(paste('Distribution of ', x))
    
  }
}

#Funcion para crear histogramas en base IsGood SI/NO
GetHist2Class <-function(df,x){
  if(is.numeric(df[, x])){
    
    ggplot(df, aes_string(x)) +
      geom_density(alpha=0.05)+
      facet_grid(.~ ifelse(IsGood==1,"SI","NO") ) +
      scale_x_continuous(breaks = seq(0,20,0.1))+
      geom_vline(aes(xintercept = mean(df[, x])),col='red',size=2)+
      ggtitle(paste('Distribution of ', x))
    
  }
}

#fixed.acidity
GetHist(df_wine,"fixed.acidity")
GetHist2Class(df_wine,"fixed.acidity")


#volatile.acidity
GetHist(df_wine,"volatile.acidity")
GetHist2Class(df_wine,"volatile.acidity")

#citric.acid
GetHist(df_wine,"citric.acid")
GetHist2Class(df_wine,"citric.acid")


#residual.sugar
GetHist(df_wine,"residual.sugar")
GetHist2Class(df_wine,"residual.sugar")


#chlorides
GetHist(df_wine,"chlorides")
GetHist2Class(df_wine,"chlorides")


#free.sulfur.dioxide
GetHist(df_wine,"free.sulfur.dioxide")
GetHist2Class(df_wine,"free.sulfur.dioxide")


#total.sulfur.dioxide
GetHist(df_wine,"total.sulfur.dioxide")
GetHist2Class(df_wine,"total.sulfur.dioxide")


#density
GetHist(df_wine,"density")
GetHist2Class(df_wine,"density")


#sulphates
GetHist(df_wine,"sulphates")
GetHist2Class(df_wine,"sulphates")

#alcohol
GetHist(df_wine,"alcohol")
GetHist2Class(df_wine,"alcohol")


#quality
#IsGood
GetHist(df_wine,"quality")
GetHist(df_wine,"IsGood")

#Luego de analizar visualmente las distribuciones vamos a eliminar
#los valores atípicos de las siguientes variables:
  #alcohol
  #fixed.acidity
  #residual.sugar
  #chlorides
  #total.sulfur.dioxide
  #sulphates


remove_outliers <- function(x, limit = 3) {
  mn <- mean(x, na.rm = T)
  out <- limit * sd(x, na.rm = T)
  x < (mn - out) | x > (mn + out)
}

df_wine<-df_wine[remove_outliers(df_wine$alcohol,3)==FALSE,]
df_wine<-df_wine[remove_outliers(df_wine$fixed.acidity,3)==FALSE,]
df_wine<-df_wine[remove_outliers(df_wine$residual.sugar,3)==FALSE,]
df_wine<-df_wine[remove_outliers(df_wine$chlorides,3)==FALSE,]
df_wine<-df_wine[remove_outliers(df_wine$total.sulfur.dioxide,3)==FALSE,]
df_wine<-df_wine[remove_outliers(df_wine$sulphates,3)==FALSE,]

count(df_wine)



#En este momento hemos eliminado los valores atípicos que pueden 
#afectar a nuestro modelos, vamos hacer un análisis 
#de los estadísticos mas importante:
describe(df_wine)



#Aunque los histogramas y los estadísticos nos indican que nuestras variables
#no siguen una distribución normal, vamos a realizar una prueba de validación de normalidad
#utilizando Anderson-Darling, en la cual vamos a validar el p_valor vrs α=0.05, 
#si es superior al nivel de significancia determinamos 
#que la distribución es normal de lo contrario es una distribución libre.
alpha = 0.05
col.names = colnames(df_wine)
for (i in 1:ncol(df_wine)) {
  if (i == 1) cat("Variables que no siguen una distribución normal:\n")
  if (is.integer(df_wine[,i]) | is.numeric(df_wine[,i])) {
    p_val = ad.test(df_wine[,i])$p.value
    if (p_val < alpha) {
      cat(col.names[i])
      # Format output
      if (i < ncol(df_wine) - 1) cat(", ")
      if (i %% 3 == 0) cat("\n")
    }
  }
}


fligner.test(IsGood ~ fixed.acidity, data = df_wine)
fligner.test(IsGood ~ volatile.acidity, data = df_wine)
fligner.test(IsGood ~ citric.acid, data = df_wine)
fligner.test(IsGood ~ residual.sugar, data = df_wine)
fligner.test(IsGood ~ chlorides, data = df_wine)
fligner.test(IsGood ~ free.sulfur.dioxide, data = df_wine)
fligner.test(IsGood ~ total.sulfur.dioxide, data = df_wine)
fligner.test(IsGood ~ density, data = df_wine)
fligner.test(IsGood ~ pH, data = df_wine)
fligner.test(IsGood ~ sulphates, data = df_wine)
fligner.test(IsGood ~ alcohol, data = df_wine)


##-----------------------------------------------------
##analisis de correlacion
#Matrix de correlacion (Heatmap)
corrplot(cor(df_wine,method = "spearman"))




#calcular las correlaciones cuantitativamente
corr_matrix <- matrix(nc = 3, nr = 0)
colnames(corr_matrix) <- c("variable","estimate", "p-value")
for (i in 1:(ncol(df_wine) - 2)) {
  if (is.integer(df_wine[,i]) | is.numeric(df_wine[,i])) {
    spearman_test = cor.test(df_wine[,i],
                             df_wine[,length(df_wine)],
                             method = "spearman")
    corr_coef = spearman_test$estimate
    p_val = spearman_test$p.value
    xcol=col.names[i]
    # Add row to matrix
    newrow = matrix(ncol = 3, nrow = 1)
    newrow[1][1] = xcol
    newrow[2][1] = corr_coef
    newrow[3][1] = p_val
    corr_matrix <- rbind(corr_matrix, newrow)
  }
}

corr_matrix


##-----------------------------------------------------
#Generar el modelo predictivo y test
#eliminar la variable Quality porque de ahi derivamos IsGood
df_wine <-df_wine[,-12]
write.csv(df_wine, "C:/dataset/Clearwinequality-red.csv")


#generar el modelo de entrenamiento y test
samp <- sample(nrow(df_wine), 0.7 * nrow(df_wine))
train <- df_wine[samp, ]
test <- df_wine[-samp, ]

#Generando el modelo de Random Forest
model <- randomForest(as.factor(IsGood) ~ ., data = train,  do.trace=T, importance=T)
model

# Validar que variables son mas relevante en la prediccion
varImpPlot(model)
 

#Consultar informacion del modelo con la validacion contra test
pred<-predict(model, newdata=test, type="class")
test$IsGood <- factor(test$IsGood)
confusionMatrix(pred, test$IsGood)


























