
install.packages("mlbench")
install.packages("leaps")
install.packages("e1071")
install.packages("caTools")
install.packages("rapportools") # para diagramas de caja

library(tidyverse)
library(ggplot2)
library(caret)
library(mlbench)
library(janitor)
library(leaps)
library(rpart)
library(rpart.plot)

### como preparar mi data
#https://www.youtube.com/watch?v=45DxMVZBkB4

#https://www.youtube.com/watch?v=zwc7doFXpFs



library(readr)
file.choose()
apar_csv<- "C:\\Users\\Andres\\Desktop\\estadistica\\modelos predictivos\\apart\\train.csv"

# aplicacion del metodo backward ################ empezamos con todasa las variables
apartame_train<-read_csv(apar_csv)
full.model <- lm(precio ~ ., data=apartame_train)
summary(full.model)
### ahora emplear el criterio de akaike
library(MASS)  # Para poder usar la funcion stepAIC
modback <- stepAIC(full.model, trace=TRUE, direction="backward")

### funcion leaps:

model_subset <- regsubsets(precio ~ alcobas + banos + mt2 + administracion, 
                           data=apartame_train, nbest=2, nvmax=13)
names(summary(model_subset))
summary(model_subset)$which
plot(model_subset, labels=obj$xnames, main=NULL, 
     scale=c("bic", "Cp", "adjr2", "r2"),
     col=gray(seq(0, 0.9, length = 10)))

par(mfrow=c(1, 2))
plot(model_subset, scale="adjr2", main=expression(R[Adj]^2))
plot(model_subset, scale="bic", main="BIC")


#######3


modback$anova
summary(modback)

head(apartame_train)
View(apartame_train)
names(apartame_train)
plot(apartame_train)
cor.test(apartame_train$precio,apartame_train$administracion) # valor de correlacion
cor.test(apartame_train$precio,apartame_train$mt2)
modelo_1<- lm(precio ~ administracion + mt2, data=apartame_train) #modelo_1
summary(modelo_1)
modelo_2<- lm(precio ~.,data=apartame_train) #modelo_2
summary(modelo_2)
# 0.7426
modelo_2$coefficients
prediccion_2 <- predict(modelo_2, newdata=apartame_test) ## se va a predecir con los datos de test
prediccion_2
summary(prediccion_2)

modelo_3<-lm(precio~administracion+mt2+banos+alcobas, data=apartame_train)
summary(modelo_3)
View(apartame_train)

# Adjusted R-squared:  0.7426 
setwd("C:/Users/Andres/Desktop/estadistica/modelos predictivos/bicicletas")
n <- 294
solucion <- data.frame(Id=1:n, precio=prediccion_2)
write.csv(solucion, file='2sampleSubmission.csv', row.names=FALSE)## guardar en csv

##############
aparte_csv<- "C:\\Users\\Andres\\Desktop\\estadistica\\modelos predictivos\\apart\\test.csv"
apartame_test<-read_csv(aparte_csv)
head(apartame_test)
View(apartame_test)

modelo <- lm(precio ~ mt2 + banos, data=apartame_train) #modelo
summary(modelo)
prediccion <- predict(modelo, newdata=apartame_test)
prediccion


####### https://www.youtube.com/watch?v=hY53T7MCXdY


setwd("C:/Users/Andres/Desktop/estadistica/modelos predictivos/bicicletas")
n <- 294
solucion <- data.frame(Id=1:n, precio=prediccion)
write.csv(solucion, file='1sampleSubmission.csv', row.names=FALSE)




### cargaos data de entrenamiento y test 

### datos de entrenamiento         

cicla.csv<- "C:\\Users\\Andres\\Desktop\\estadistica\\modelos predictivos\\bicicletas\\byke_train.csv"
byke_train <- read.csv(cicla.csv,header = TRUE)
byke_train<-byke_train[,-9]
byke_train$Hora<-as.factor(byke_train$Hora)
byke_train$Mes<-as.factor(byke_train$Mes)
str(byke_train)
View(byke_train)
names(byke_train)


full.model <- lm(Total ~ ., data=byke_train) ## Metodo backward
summary(full.model)
#######                                             leaps

mode_subset <- regsubsets(Total ~.,
                           data=byke_train, nbest=2, nvmax=13)
names(summary(mode_subset))
summary(mode_subset)$which
plot(mode_subset, labels=obj$xnames, main=NULL, 
     scale=c("bic", "Cp", "adjr2", "r2"),
     col=gray(seq(0, 0.9, length = 10)))

par(mfrow=c(1, 2))
plot(mode_subset, scale="adjr2", main=expression(R[Adj]^2))
plot(mode_subset, scale="bic", main="BIC")

#########

View(byke_train)

### datos de test

file.choose()
ciclates.csv<- "C:\\Users\\Andres\\Desktop\\estadistica\\modelos predictivos\\bicicletas\\byke_test.csv"
byke_test<- read.csv(ciclates.csv)
byke_test
View(byke_test)
pairs(byke_train)

str(byke_test)
plot(byke_train)

summary(byke_train)



# Usar 10-fold cross-validation para todos los metodos que siguen.......



ctrl = trainControl(method="cv",number=10) ## metodo, numero de k folds remuestreo !!!!!

#### dueños de nuestros propios hiperparametros para busvar la mejor
# combinacion que optimice la metrica 
# exponente de  distancia de minkoski #2 euclidida #1 manhatan


my_grid<-expand.grid(kmax=c(5),distance=c(1,2),
         
                           kernel=c("gaussian","triangular"))
my_grid
set.seed(123) # para replicar resultados

# Ahora estimemos nuetsro primer modelo:
modelo1 <- train(Total~.,data=byke_train,method="kknn",
                 metric="RMSE",
                 trControl=ctrl,
                 tuneGrid=my_grid)
modelo1


set.seed(123)

# To train the model # aca vamos a dejar que ellos mismos elijan parametros

fit2 <- train(Price ~., 
              data = byke_train, 
              method = "kknn", 
              metric = "RMSE",
              trControl = fitControl,
              tuneLength = 4)

# To show the results
fit2



#Using random grid -------------------------------------------------------
  # Aqui vamos a dejar que train elija los valores de los 
  # hiper-parametros (los que pueda elegir). Solo vamos a pedir
  # que considere 4 valores
  

# To train the model

fit2 <- train(Price ~ Weight + MPG.city, 
              data = train_data, 
              method = "kknn", 
              metric = "RMSE",
              trControl = fitControl,
              tuneLength = 4)




#RMSE      Rsquared   MAE    
#140.5326  0.4010316  104.264


## sin procesar ahora KNN

### 
modelo2 <- train(Total~.,data=byke_train,method="knn",trControl=ctrl)
modelo2
summary(modelo2)

colnames(byke_train)
# todas las variables
#k  RMSE      Rsquared   MAE     
#5  55.49368  0.9074020  35.07201
#7  55.95480  0.9065248  35.36168
#9  56.60151  0.9049771  35.86160






prediccion_sinpro <- predict(modelo2, newdata=byke_test)
View(byke_test)
prediccion_sinpro
summary(prediccion_sinpro)
setwd("C:/Users/Andres/Desktop/estadistica/modelos predictivos/bicicletas")
dim(byke_train)
n <- 3474
solucion <- data.frame(Id=1:n, Total=prediccion_sinpro)
write.csv(solucion, file='sinproampleSubmission.csv', row.names=FALSE)




# KNN con preprocesamiento

modelo2b <- train(Total~.,data=byke_train,method="knn",preProcess=c("center","scale"),trControl=ctrl)
modelo2b

# KNN con preprocesamiento y grid search para k #### distintos valores de k, definir los hiperparametros

knnGrid <- expand.grid(k=c(1,5,10)) ### se puede cambiar a 2,3,5, se puede agregar distancia y kernel


modelo2c <- train(Total~.,data=byke_train,method="knn",preProcess=c("center","scale"),tuneGrid=knnGrid,trControl=ctrl)
modelo2c


# CART Parámetro de control = maxdepth sin profundidad
colnames(byke_train)
modelo3a <- train(Total~.,data=byke_train,method="rpart2",trControl=ctrl)
modelo3a
#maxdepth  RMSE      Rsquared   MAE      
#1         150.9880  0.3052757  107.07048
#2         139.6453  0.4058827   98.52216
#3         130.0238  0.4851182   92.63431

###3CART Búsqueda de mejor Maxdepth

cartGrid <- expand.grid(maxdepth=c(10,20,30))
modelo3b <- train(Total~.,data=byke_train,method="rpart2",tuneGrid=cartGrid,trControl=ctrl)
modelo3b
colnames(byke_train)

####maxdepth  RMSE       Rsquared   MAE      
#1        150.99011  0.3052959  107.07333
#5        123.53103  0.5352491   88.81767
#10        109.84216  0.6328152   78.40909
#20         97.12834  0.7127572   69.76893


### vamos a predecir con CART

prediccion_tree <- predict(modelo3b, newdata=byke_test)
View(byke_test)
prediccion_tree
summary(prediccion_tree)
setwd("C:/Users/Andres/Desktop/estadistica/modelos predictivos/bicicletas")
dim(byke_train)
n <- 3474
solucion <- data.frame(Id=1:n, Total=prediccion_tree)
write.csv(solucion, file='treesampleSubmission.csv', row.names=FALSE)

modelo3b <- train(Total~.,data=byke_train,method="rpart2",tuneGrid=cartGrid,trControl=ctrl)
modelo3b



#### vamos a predecir con multivariate adaptive regression splines MARS......................................

MarsGrid<- expand.grid( degree = c(1,2,3),nprune=c(2,5,10,15))
modelomate<- train(Total~ Hora+Mes,data=byke_train,method="earth",tunegrid=MarsGrid,trcontrol=ctrl)
modelomate
colnames(byke_train)
warnings()
str(byke_train)


#### predecir con random forest
x_bike=byke_train[,-12]
y_bike=byke_train$Total

modelo_rain<-train(x= x_bike,
                   y= y_bike,
                   method = "rf"
                   
                   )
modelo5<-train(Total~ Año+DíaFestivo+DíaNoLaborable+Hora+Estación+Clima+Temperatura+DíaSemana,data=byke_train,method="rf",tunelength=6,trcontrol=ctrl)
modelo5
#sin estacion ni clima
#mtry  RMSE       Rsquared   MAE     
#2     111.75094  0.6497768  78.91963
#3      94.53932  0.7304330  64.32883
#4      92.67332  0.7383887  62.33489

# Año+DíaFestivo+DíaNoLaborable+Hora+Estación+Clima
#mtry  RMSE      Rsquared   MAE     
#2     93.84429  0.7693058  64.83851
#4     64.74626  0.8729344  40.54162
#6     66.43460  0.8666202  40.92891

#   RMSE      Rsquared   MAE     
#2     80.01386  0.8377938  54.79079
#5     48.52979  0.9282575  29.63383
#8     51.31757  0.9198392  30.73028

prediccion_tree5 <- predict(modelo5, newdata=byke_test)
View(byke_test)
prediccion_tree5
summary(prediccion_tree5)
setwd("C:/Users/Andres/Desktop/estadistica/modelos predictivos/bicicletas")
dim(byke_train)
n <- 3474
solucion <- data.frame(Id=1:n, Total=prediccion_tree5)
write.csv(solucion, file='randomsampleSubmission.csv', row.names=FALSE)



data_y<-byke_train$Total
modelo6<-train(x=byke_train, y = data_y ,method="rf")
modelo6


#### boosting  ###########################...............................................................

modelo6<-train(Total~Humedad+Temperatura,data=byke_train, method="xgbTree",trcontrol=ctrl)
modelo6
### bosting con grid
boostinggrid<- expand.grid(eta=c(0.01,0.05,0.1),max_depth=c(1,3,5,10),nrounds=c(20,100,500,1000),subsample=c(0.5,0.75))
modelo6a<-train(Total~Humedad+Temperatura,data=byke_train, method="xgbTree",tunegrid=boostinggrid,trcontrol=ctrl)
modelo6

# metodos de ensamble

ctrl = trainControl(method="cv",number=10)

set.seed(2020)
cartGrid <- expand.grid(maxdepth=c(10,20,30))
modelo3b <- train(Total~.,data=byke_train,method="rpart2",tuneGrid=cartGrid,trControl=ctrl)
modelo3b

modelo2 <- train(Total~.,data=byke_train,method="knn",trControl=ctrl)
modelo2
summary(modelo2)

modelo5<-train(Total~.,data=byke_train,method="rf",tunelength=6,trcontrol=ctrl)
modelo5

resamps<-resamples(list(rpar2=modelo3b,
                        knn=modelo2))
resamps
summary(resamps)

### gradiente boosting machine...........................................................

modelo9<-train(Total~ Hora+Mes,data=byke_train,method="gbm",distribution="gaussian",trcontrol=ctrl,verbose=FALSE)
modelo9
warnings()

str(byke_train)

knnn
# https://www.youtube.com/watch?v=9C6HI_CyRG4 video

##### ahora vamos a predecir con knn

modelo2c <- train(Total~.,data=byke_train,method="knn",preProcess=c("center","scale"),tuneGrid=knnGrid,trControl=ctrl)
modelo2c
prediccion_k <- predict(modelo2c, newdata=byke_test)
View(byke_test)
prediccion_k
summary(prediccion_k)
setwd("C:/Users/Andres/Desktop/estadistica/modelos predictivos/bicicletas")
dim(byke_train)
n <- 3474
solucion <- data.frame(Id=1:n, Total=prediccion_k)
write.csv(solucion, file='BICIsampleSubmission.csv', row.names=FALSE)












##### redes neuronales


LIBRERIAS Y DATOS
# -----------------------------------------------------
library(MASS); library(neuralnet); library(ggplot2)
set.seed(65)
datos    <- byke_train
n        <- nrow(datos)
muestra  <- sample(n, n * .70)
train    <- datos[muestra, ]
test     <- datos[-muestra, ]


# NORMALIZACION DE VARIABLES
# -----------------------------------------------------
maxs      <- apply(byke_train, 2, max)
mins      <- apply(byke_train, 2, min)
datos_nrm <- as.data.frame(scale(datos, center = mins, scale = maxs - mins))
train_nrm <- datos_nrm[muestra, ]
test_nrm  <- datos_nrm[-muestra, ]


# FORMULA
# -----------------------------------------------------
nms  <- names(train_nrm)
frml <- as.formula(paste("Total ~", paste(nms[!nms %in% "Total"], collapse = " + ")))
colnames(byke_train)

# MODELO
# -----------------------------------------------------
modelo.nn <- neuralnet(frml,
                       data          = byke_train,
                       hidden        = c(7,5), # ver Notas para detalle 
                       threshold     = 0.05,   # ver Notas para detalle
                       algorithm     = "rprop+" 
)

?neuralnet

# PREDICCION
# -----------------------------------------------------
pr.nn   <- compute(modelo.nn,within(test_nrm,rm(medv)))

# se transoforma el valor escalar al valor nominal original
medv.predict <- pr.nn$net.result*(max(datos$medv)-min(datos$medv))+min(datos$medv)
medv.real    <- (test_nrm$medv)*(max(datos$medv)-min(datos$medv))+min(datos$medv)



# SUMA DE ERROR CUADRATICO
# -----------------------------------------------------
(se.nn <- sum((medv.real - medv.predict)^2)/nrow(test_nrm))


#GRAFICOS
# -----------------------------------------------------
# Errores
qplot(x=medv.real, y=medv.predict, geom=c("point","smooth"), method="lm", 
      main=paste("Real Vs Prediccion. Summa de Error Cuadratico=", round(se.nn,2)))
# Red
plot(modelo.nn)



###........ MODELOS DE CLASIFICACION

library(caret)
library(readr)
train_digit <- read_csv("C:/Users/Andres/Desktop/estadistica/modelos predictivos/digit/train.csv")

str(train_digit)

library(readr)
test_digit <- read_csv("C:/Users/Andres/Desktop/estadistica/modelos predictivos/digit/test.csv")
View(test_digit)

ctrla = trainControl(method="cv",number=10)
svmGrid<-expand.grid(C=2^seq(-5,15,2),sigma=2^seq(-15,3,2))
#rsquared 0.60 con glm

str(train_digit)
modelo_a<-train(label~.,data=train_digit,method="svmRadial",preProcess=c("center","scale"),tuneGrid=svmGrid,
                                                                         trControl=ctrla)
modelo_a

prediccion_digit <- predict(modelo_a, newdata=test_digit)
prediccion_digit
summary(prediccion_digit)

setwd("C:/Users/Andres/Desktop/estadistica/modelos predictivos/digit")
n <- 28000
solucione <- data.frame(Id=1:n, Total=prediccion_digit)
write.csv(solucione, file='glmsample_Submission.csv', row.names=FALSE)



#############################%%%%%%%%%%%%%%%%%%&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

install.packages("naivebayes")
library(tidyverse)
library(tidytext)
library(naivebayes)
library(tm)
library(caret)


# modelo naive

datos_traine
# datos de test
datos_teste

train_digit$label<-as.factor(train_digit$label)


str(train_digit)
mod <- naive_bayes( label~., data = train_digit)
mod

pred <- predict(mod,test_digit)
pred
summary(pred)
tab <- table(test_digit, pred, dnn = c("Actual", "Predicha"))
confusionMatrix(tab)

setwd("C:/Users/Andres/Desktop/estadistica/modelos predictivos/digit")
n <- 28000
solucione <- data.frame(Id=1:n, Total=pred)
write.csv(solucione, file='naisample_Submission.csv', row.names=FALSE)


################################&&&&&&&&&&&&&&&&&&&&////////////////////////(((((((((())))))))))



setwd("C:/Users/Andres/Desktop/estadistica/modelos predictivos/bicicletas")
n <- 294
solucion <- data.frame(Id=1:n, precio=prediccion)
write.csv(solucion, file='1sampleSubmission.csv', row.names=FALSE)



##################################&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&//////////

# knn



set.seed(30)

SP_ctrl <- trainControl(method="cv", number = 10) 


set.seed(30)


#preprocesando datos
set.seed(30)

SP_knnEntrenado <- train(label ~., 
                         data = train_digit, 
                         method = "knn",  
                         tuneLength = 20,
                         trControl = SP_ctrl,
                         preProcess = c("center","scale")
)

set.seed(30)
SP_knnPrediccionn <- predict(SP_knnEntrenado, newdata = train_digit)
confusionMatrix(SP_knnPrediccionn, train_digit$label)


# error de prueba
set.seed(30)
SP_knnPrediccion <- predict(SP_knnEntrenado, newdata = test_digit )
set.seed(30)
confusionMatrix(SP_knnPrediccion, heart_test$evento_muerte)

#####%%%%%&///))()()()()((())//&&&&&&&&%$$$#####)//////////%%%%%%%%%%%

library(caret)
library(e1071)

# Índices observaciones de entrenamiento
set.seed(12)
train <- createDataPartition(y = train_digit$label , p = 0.7, list = FALSE, times = 1)

# Datos entrenamiento
datos_train <- heart[train, ]
# Datos test
datos_test <- heart[-train, ]


fitControl <- trainControl(method = "cv", 
                           number = 10, 
                           classProbs = TRUE, 
                           search = "grid")

grid_C <- data.frame(C = c(0.001, 0.01, 0.1, 1, 5, 10, 15, 20))
set.seed(325)
modelo_svc <- train(label ~., data = train_digit, 
                    method = "svmLinear", 
                    trControl = fitControl, 
                    preProc = c("center", "scale"), #estandarizacion de los datos
                    tuneGrid = grid_C)

predo <- predict(modelo_svc,test_digit)
predo
summary(predo)

confusionMatrix(predict(modelo_svc, datos_train), datos_train$evento_muerte)


SP_knnPrediccionn <- predict(SP_knnEntrenado, newdata = train_digit)

#################

CON R PART

library(tidyverse)
library(rpart)
library(rpart.plot)
library(caret)


set.seed(1649)


train_digit$label<-as.factor(train_digit$label)
arbol_1 <- rpart(formula = label ~ ., data = train_digit)
prediccion_1 <- predict(arbol_1, newdata = test_digit, type = "class")
summary(prediccion_1)


setwd("C:/Users/Andres/Desktop/estadistica/modelos predictivos/digit")
n <- 28000
solucione <- data.frame(Id=1:n, Total=prediccion_1)
write.csv(solucione, file='treesample_Submission.csv', row.names=FALSE)


######### KNNN SOLO


set.seed(28)

SP_knnEntrenado <- train(label ~ ., 
                         data = train_digit, 
                         method = "knn",  
                         tuneLength = 20
)

train_digit$label<-as.factor(train_digit$label)



###### arboles de decision

require(rpart)
mod_rpart1 <- rpart(label~., data=train_digit,
                    method="class")

plot(mod_rpart1$cptable)
predicci.pruned <- predict(mod.pruned, newdata = test_digit,type="class")

setwd("C:/Users/Andres/Desktop/estadistica/modelos predictivos/digit")
n <- 28000
solucione <- data.frame(Id=1:n, Total=predicci.pruned)
write.csv(solucione, file='diezample_Submission.csv', row.names=FALSE)

mod.pruned = prune(mod_rpart1,mod_rpart1$cptable[10,"CP"])

# KNN RAPIDO
install.packages("doMC")
library(doMC)
registerDoMC(cores = 3)
registerDoMC(cores = 3)

ctrl <- trainControl(method="repeatedcv",repeats = 1, number = 4, verboseIter = T, allowParallel = T)
knnFit <- train(label ~ ., data = train_digit, method = "knn", trControl = ctrl)



library(doMC)
registerDoMC(cores = 3)
tc <- trainControl(method = "cv", number = 10, verboseIter = F, allowParallel = T)
modSVMR1 <- train(label ~. , data= train_digit, method = "svmRadial", trControl = tc)
SVMRadial_predict1 <- as.numeric(predict(modSVMR1,newdata = validating))-1


