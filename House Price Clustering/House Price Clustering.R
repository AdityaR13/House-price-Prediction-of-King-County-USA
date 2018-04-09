#### Alternatively Clustering can be done according geo-mapping ( Only Visualization for now )
# Clustering
# Visualizing zipcode and lat and long
ggplot(data = hprice, mapping = aes(x = zipcode, y = price)) + geom_boxplot()

ggplot(hprice, mapping = aes(x=lat, y=long)) + geom_point(colour= "green") + geom_smooth(method = 'lm')


hcluster <- kmeans(scale(hprice[,c(18,19,24)]),25,100)  #( creating 25 clusters,100 random start)

# factoring clusters
hprice$cluster<-factor(hcluster$cluster)

# plotting clusters
ggplot(data= hprice, aes(x = long, y = lat)) + geom_point(aes(color=cluster))
ggplot(hprice, aes(rate, fill = cluster)) + geom_density(position = "stack")