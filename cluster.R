library(quantmod)
library(data.table)
library(zoo)
library(dplyr)
library(factoextra)
library(NbClust)
library(rowr)
library(fpc)
library(ggplot2)

## get list of all sti stock names
stock_names = read.csv('SGX_StockName_Symbol_Industry.csv')

# # use only when finding new data #  
# data <- new.env()
# lapply(as.character(stock_names$Symbol), function(x){
#   try(getSymbols(x,env=data, from = '2017-10-02', to = '2017-12-30'),silent=TRUE)
# })

data = readRDS("data.rds")

## get close price for each symbol
vec_of_stocks = c(numeric)
for(i in 1:length(ls(data))) {
  stock = as.data.frame(data[[ls(data)[i]]][,4])
  vec_of_stocks[i] = stock
}

## remove those with missing dates
# run this and count manually
sapply(vec_of_stocks, function(x) length(x) >= 63)
errors_index = c(22, 24, 28, 34, 50, 52, 216, 400, 401)
errors_name = c()
for(index in errors_index) {
  errors_name = c(errors_name, ls(data)[index])
}
col_names = ls(data)
col_names = col_names[!col_names %in% errors_name]
column_names = as.character(stock_names[which(stock_names$Symbol %in% col_names),]$Name)

## make a DF with row as date and col as stock name
vec_of_stocks = vec_of_stocks[sapply(vec_of_stocks, function(x) length(x) >= 63)]
df_of_stocks = as.data.frame(vec_of_stocks)
names(df_of_stocks) = column_names
## replace na with previous known and remove those with constant values throughout
df_of_stocks = df_of_stocks %>% do(na.locf(.))
df_of_stocks = df_of_stocks[vapply(df_of_stocks, function(x) length(unique(x)) > 1, logical(1L))]

## find % change from first to last day in quarter
percent_change = numeric()
for(i in 1:ncol(df_of_stocks)) {
  percent_change[i] = (df_of_stocks[63,i] - df_of_stocks[1,i])/df_of_stocks[1,i] *100
}

## find optimum number of clusters on corr matrix
cor_matrix = cor(df_of_stocks)
fviz_nbclust(cor_matrix, kmeans, method = "wss") #suggests 3
fviz_nbclust(cor_matrix, kmeans, nstart = 25,  method = "gap_stat", nboot = 10) #suggests 3

## plot kmeans
clus <- kmeans(cor_matrix, centers=9)
par(cex=0.9, family="sans")
plotcluster(cor_matrix, clus$cluster)

## put clusters into indiv tables
clustersdf = as.data.frame(clus$cluster)
clustersdf = cbind(rownames(clustersdf), clustersdf, percent_change)

result = data.frame()
for(i in 1:9) {
  mycluster = clustersdf[which(clustersdf$`clus$cluster` == i),]
  names(mycluster) = c("Name", "Cluster", "Percent_Change")
  mycluster = merge(mycluster, stock_names, by="Name")
  #add average price for each stock over 3 months
  result = rbind(result, mycluster)
}

write.csv(result, "SGX_Stock_By_Cluster_stacked.csv")


## plot hclust
library(ape)
clusters <- hclust(dist(cor_matrix), method = 'single')
pdf(height=100, width=15)
plot(as.phylo(clusters), cex=1, label.offset = 1)
dev.off()

