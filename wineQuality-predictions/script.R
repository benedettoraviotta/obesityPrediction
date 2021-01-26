#install.packages("ggplot2")
#install.packages("ggcorrplot")

wine.data = read.csv("winequality-white.csv", header = TRUE, sep=";")

str(wine.data)

wine.data$quality_label = 1
wine.data$quality_label[wine.data$quality < 6] = 0
wine.data$quality_label = factor(wine.data$quality_label)

# seleziono tutte le variabili tranne "quality"
wine.active = wine.data[, -c(12)]

sum(is.na(wine.active)) # non ci sono valori nulli

# ANALISI UNIVARIATA 
# è importante dividere il dataset in attributi input e target
x = wine.active[, 1:11]
y = wine.active[, 12]

# It demonstrate that all variables, except alcohol contains outliers.
# stampo boxplot per ogni attributo
oldpar = par(mfrow = c(2,6))
for ( i in 1:11 ) {
  boxplot(x[, i], main=names(wine.active)[i])
}
par(oldpar)

library(caret)
# . We can have a more clear idea by plotting the distribution of
#each feature of the input space
featurePlot(x, y, plot="density", scales=list(x=list(relation="free"),
                                              y=list(relation="free")), 
            auto.key=list(columns=2))


# ANALISI MULTIVARIATA
# visualization of instances' distributions
ggplot(wine.active, aes(x=quality_label, y=alcohol, colours = quality_label)) + 
  geom_jitter() + ggtitle("Relation between quality_label, alcohol and their classification")

plot(
  x = wine.active$alcohol,
  y = wine.active$volatile.acidity,
  col = wine.active$quality_label
)

ggplot(wine.active, aes(x=volatile.acidity , y=alcohol,colour='red',size=3 ))  +
  geom_point() + 
  labs(x="volatile.acidity", 
       y="alchool", 
       title = "Wine data set",
       subtitle = "Scatter plot"
  )
