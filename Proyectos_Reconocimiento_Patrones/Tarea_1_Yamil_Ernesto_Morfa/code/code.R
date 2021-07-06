library('MVA')
library(ggplot2)
library(GGally)
library(ellipse)


data = deport[-9]
pairs(data)
ggpairs(data)
plot(deport[,6:7], xlim=c(12,18))
text(deport[,6], deport[,7], rownames(deport), cex = 0.6, pos = 4, col = "red" )
color=rep("black", 55)
color[12] = "red"
color[55] = "red"
pairs(data, col = color, pch = 16)
boxplot(data)
plotcorr(cor(data))
p <- princomp(data)
loadings(p)
summary(p)

data = oef2.data

p <- princomp(data)
ld <- loadings(p)
p1 = c()
p2 = c()
for (i in (1:35)){
  p1[i] = data[i,]*ld[,1]
  p2[i] = data[i,]*ld[,2]  
}
plot(1:35, p1)
lines(1:35, p1)
plot(1:35, p2)
lines(1:35, p2)

temp <- matrix(data, 35, 12, byrow=T)
nombresestaciones <- c("St. John_s", "Charlottetown", "Halifax", "Sydney",
    "Yarmouth", "Fredericton", "Arvida", "Montreal", "Quebec City", "Schefferville", 
    "Sherbrooke", "Kapuskasing", "London", "Ottawa", "Thunder Bay", "Toronto", "Churchill",
    "The Pas", "Winnipeg", "Prince Albert", "Regina", "Beaverlodge", "Calgary", "Edmonton",
    "Kamloops", "Prince George", "Prince Rupert", "Vancouver", "Victoria", "Dawson",
    "Whitehorse", "Frobisher Bay", "Inuvik", "Resolute", "Yellowknife")
rownames(temp)<-nombresestaciones

biplot(p, xlabs =rownames(temp))
