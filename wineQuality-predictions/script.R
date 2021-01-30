#install.packages("ggplot2")
#install.packages("ggcorrplot")
#install.packages("caret")
#install.packages("e1071")
#install.packages("factoextra")
#install.packages("randomForest")
#install.packages("rpart")

library(e1071)
compute.svm = function(trainset, testset, kernel, cost=1, gamma=1){
  svm.model = svm(quality_label ~ .,
                  data = trainset,
                  type = "C-classification",
                  kernel = kernel,
                  gamma = gamma,
                  cost = cost)
  
  #testing del mdodello
  prediction.svm = predict(svm.model, testset)
  svm.table = table(testset$quality_label, prediction.svm)
  confusionMatrix(svm.table)
}

library(randomForest)
compute.randomForest = function(trainset, testset){

  forest.model = randomForest(quality_label ~ ., data=trainset)
  prediction.forest = predict(forest.model, newdata = testset)
  forest.table = table(prediction.forest, testset$quality_label)
  confusionMatrix(forest.table)
}

wine = read.csv("winequality-white.csv", header = TRUE, sep = ";")

str(wine)
wine.data = wine
wine.data$quality_label = "Media"
wine.data$quality_label[wine.data$quality < 6] = "Bassa"
wine.data$quality_label[wine.data$quality > 7] = "Alta"
wine.data$quality_label = factor(wine.data$quality_label)

# seleziono tutte le variabili tranne "quality"
wine.active = wine.data[,-c(12)]
sum(is.na(wine.active)) # non ci sono valori nulli

#DISTRIBUZIONE CLASSI
table.distribuzione = table(wine.data$quality)
barplot(table.distribuzione)

table.distribuzione = table(wine.active$quality)
barplot(table.distribuzione)

# ANALISI UNIVARIATA
# è importante dividere il dataset in attributi input e target
x = wine.active[, 1:11]
y = wine.active[, 12]

# It demonstrate that all variables, except alcohol contains outliers.
# stampo boxplot per ogni attributo
oldpar = par(mfrow = c(2, 6))
for (i in 1:11) {
  boxplot(x[, i], main = names(wine.active)[i])
}
par(oldpar)

library(caret)
# . We can have a more clear idea by plotting the distribution of
#each feature of the input space
featurePlot(
  x,
  y,
  plot = "density",
  scales = list(
    x = list(relation = "free"),
    y = list(relation = "free")
  ),
  auto.key = list(columns = 3)
)


# ANALISI MULTIVARIATA
# We now use a scatterplot matrice to roughly determine if there is a linear correlation between our variables:
pairs(wine.active[, c(7, 8, 10, 11)],
      col = wine.active$quality_label,
      oma = c(3, 3, 3, 15))
par(xpd = TRUE)
legend(
  "bottomright",
  fill = unique(wine.active$quality_label),
  legend = c(levels(wine.active$quality_label))
)


ggplot(wine.active, aes(
  x = density ,
  y = alcohol,
  colour = factor(quality_label)
))  +
  geom_point() +
  labs(x = "density",
       y = "alchool",
       title = "Relazione tra densità e alcohol e la loro classificazione") +
  theme_minimal()

# stampare relazione tra altri attributi e analisi e bivariata

# matrice di correlazione
library(ggcorrplot)
ggcorrplot(
  cor(wine),
  hc.order = TRUE,
  type = "lower",
  lab = TRUE,
  insig = "blank"
)


### PRIMO MODELLO SVM
#creo training e test set
ind = sample(2,
             nrow(wine.active),
             replace = TRUE,
             prob = c(0.7, 0.3))
testset = wine.active[ind == 2, ]
trainset = wine.active[ind == 1, ]

#train del modello senza parametri costo e gamma
compute.svm(trainset, testset, "radial")

#tune model
tune.out = tune(
  svm,
  quality_label ~ .,
  data = trainset,
  kernel = "radial",
  ranges = list(
    cost = c(0.1 , 1 , 10 , 100 , 1000),
    gamma = c(0.5, 1, 2, 3, 4)
  )
)

summary(tune.out) # best parameters: cost=1, gamma=0.5


#train del modello con parametri cost=10, gamma=0.5
compute.svm(trainset, testset, "radial", cost=1, gamma=0.5)


#################################################################

# divido il dataset in due classi (alta qualità = 2, bassa = 1)
wine_ridotto = wine

wine_ridotto$quality_label = "Alta"
wine_ridotto$quality_label[wine_ridotto$quality < 7] = "Bassa"
wine_ridotto$quality_label = factor(wine_ridotto$quality_label)
wine_ridotto.active = wine_ridotto[, -c(12)]

table.distribuzione = table(wine_ridotto.active$quality_label)
barplot(table.distribuzione)

#creo training e test set
ind = sample(2,
             nrow(wine_ridotto.active),
             replace = TRUE,
             prob = c(0.7, 0.3))
testset.wine_ridotto = wine_ridotto.active[ind == 2, ]
trainset.wine_ridotto = wine_ridotto.active[ind == 1, ]


compute.svm(trainset.wine_ridotto, testset.wine_ridotto, "radial")

#tune model
tune.out = tune(
  svm,
  quality_label ~ .,
  data = trainset.wine_ridotto,
  kernel = "radial",
  ranges = list(
    cost = c(0.1 , 1 , 10 , 100 , 1000),
    gamma = c(0.5, 1, 2, 3, 4)
  )
)

summary(tune.out) #best costo: 10, gamma: 1

compute.svm(trainset.wine_ridotto, testset.wine_ridotto, "radial", cost=10, gamma=1)

###############################################
## SECONDO MODELLO: RANDOM FOREST
compute.randomForest(trainset, testset)

#creo training e test set senza densità
ind = sample(2,
             nrow(wine.senzaDensita),
             replace = TRUE,
             prob = c(0.7, 0.3))
testsetSD = wine.senzaDensita[ind == 2, ]
trainsetSD = wine.senzaDensita[ind == 1, ]

compute.randomForest(trainsetSD, testsetSD)

#### testo su dataset con classe ALTA e BASSA
compute.randomForest(trainset.wine_ridotto, testset.wine_ridotto)

#########################################
#precision, recall, fmeasure, ROC, 10crossfoldvalidation
