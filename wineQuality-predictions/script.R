#install.packages("ggplot2")
#install.packages("ggcorrplot")
#install.packages("caret")

wine.data = read.csv("winequality-white.csv", header = TRUE, sep=";")

str(wine.data)

wine.data$quality_label = "Alta"
wine.data$quality_label[wine.data$quality < 6] = "Bassa"
wine.data$quality_label = factor(wine.data$quality_label)

# seleziono tutte le variabili tranne "quality"
wine.active = wine.data[, -c(12)]

sum(is.na(wine.active)) # non ci sono valori nulli

# ANALISI UNIVARIATA 
# Ã¨ importante dividere il dataset in attributi input e target
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
# We now use a scatterplot matrice to roughly determine if there is a linear correlation between our variables:
pairs(wine.active[,c(7, 8, 10, 11)], col = wine.active$quality_label, oma=c(3,3,3,15))
par(xpd = TRUE)
legend("bottomright", fill = unique(wine.active$quality_label), legend = c( levels(wine.active$quality_label)))

# visualization of instances' distributions
plot(
  x = wine.active$alcohol,
  y = wine.active$density,
  col = wine.active$quality_label
) #scatter plot. Colore definito sulla base di quality labels

plot(
  x = wine.active$residual.sugar,
  y = wine.active$density,
  col = wine.active$quality_label
) #scatter plot. Colore definito sulla base di quality labels

ggplot(wine.active, aes(x=density , y=alcohol, colour=factor(quality_label)))  +
  geom_point() + 
  labs(x="density", 
       y="alchool", 
       title = "Relazione tra densità e alcohol e la loro classificazione"
  ) + 
  theme_minimal()
