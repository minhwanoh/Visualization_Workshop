packages=c("clue", "DAAG", "lsmeans", "RColorBrewer")
install.packages(packages)

library(RColorBrewer)
# load the data
data(VADeaths)

# make the plot grid for plotting multiple figures
par(mfrow=c(2,3))
# plot the histograms 
hist(VADeaths,breaks=10, col=brewer.pal(3,"Set3"),main="Set3 3 colors")
hist(VADeaths,breaks=3 ,col=brewer.pal(3,"Set2"),main="Set2 3 colors")
hist(VADeaths,breaks=7, col=brewer.pal(3,"Set1"),main="Set1 3 colors")
hist(VADeaths,,breaks= 2, col=brewer.pal(8,"Set3"),main="Set3 8 colors")
hist(VADeaths,col=brewer.pal(8,"Greys"),main="Greys 8 colors")
hist(VADeaths,col=brewer.pal(8,"Greens"),main="Greens 8 colors")

# clear the grid
par(mfrow=c(1,1))

barplot(table(iris$Species,iris$Sepal.Length),
        col  = brewer.pal(3,"Set1")) #Stacked Plot


library(clue) # for k-medoids
library(DAAG)


# It illustrates the application of K-medoid clustering algorithm for 
# qualitative data with definedned distances between data points.
par(mfrow=c(1,1))
countries<-read.table("countries_data.txt")
#countries<-countries[2:12,]
rownames(countries)<-c("BEL","BRA","CHI","CUB","EGY","FRA","IND","ISR","USA","USS","YUG","ZAI")
colnames(countries)<-c("BEL","BRA","CHI","CUB","EGY","FRA","IND","ISR","USA","USS","YUG","ZAI")
# dissimilarity between countries clustering

dist_mat<-countries[1:12,1:12]
cl<-kmedoids(dist_mat,2)
fit <- cmdscale(dist_mat,eig=TRUE, k=2) # k is the number of dim
# plot solution 
x <- fit$points[,1]
y <- fit$points[,2]
plot(x, y, xlab="First MDS Coordinate", ylab="Second MDS Coordinate", 
     main="Metric  MDS",   type="n")
text(x, y, labels = row.names(dist_mat), cex=.7, col=cl$cluster)



#Heat maps enable you to do exploratory data analysis with two
#dimensions as the axis and the third dimension shown by intensity
#of color. However you need to convert the dataset to a
#matrix format. Here’s the code:


### NBA Box Score heatmap#################
nba <- read.csv("ppg2008.csv", sep=",")

# order the rows by number of points
nba <- nba[order(nba$PTS),]
# add row names
row.names(nba) <- nba$Name

# choose columns to plot
nba <- nba[,2:20]

# convert the data to matrix format
nba_matrix <- data.matrix(nba)

# Final heatmap
nba_heatmap <- heatmap(nba_matrix, Rowv=NA, Colv=NA,
                       col = brewer.pal(9, "Blues"), scale="column", 
                       margins=c(5,10))


### Spider Web ##############
source("webplot.R")

# By default, 
webplot(mtcars)

# plot for NBA boxscore comparison

par(mar = c(1, 1, 2, 1))
chosen_players = c("Nate Robinson ", "Rashard Lewis ", "LeBron James ","Dwyane Wade ")
chosen_cols = c("PTS", "FGP","AST", "STL","BLK", "TRB")
colours = brewer.pal(length(chosen_players), "Set2")
for (i in 1:length(chosen_players)){
  if (i==1){
    webplot(nba, chosen_players[i], y.cols = chosen_cols, col = colours[i], main = "Compare Boxscore")
  } else{
    webplot(nba, chosen_players[i], y.cols = chosen_cols, add = T, col = colours[i])
  }
}
par(new = T)
par(mar = c(0, 0, 0, 0))
plot(0, type = "n", axes = F)
legend("bottomright", lwd = 2, col = colours, chosen_players, bty = "n")



# Interactive Shot Charts using Shiny
## source code: https://github.com/toddwschneider/ballr
packages <- c("shiny", "ggplot2", "hexbin", "dplyr", "httr", "jsonlite")
install.packages(packages)
library(shiny)
runGitHub("ballr", "toddwschneider")
