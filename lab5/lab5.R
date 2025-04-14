library(parameters)
library (factoextra)
library (cluster)
library(scatterplot3d)
library(lattice)

file_path <- 'D:/Универ/3 курс/6 семестр/Бигдата/DataSets_Lab_5/09_темпы роста_стран/GrowthSW.csv'

# Читаем файл как текст
file_lines <- readLines(file_path, encoding = "UTF-8")

# Разделяем строки на столбцы
data_list <- lapply(file_lines, function(line) {
  strsplit(line, ",")[[1]] # Разделяем строку по запятой
})

# Получаем имена столбцов из первой строки
column_names <- data_list[[1]]

# Извлекаем данные, начиная со второй строки
data_rows <- data_list[-1]

# Создаем матрицу данных
data_matrix <- do.call(rbind, data_rows)

dataset <- as.data.frame(data_matrix, stringsAsFactors = FALSE)

# Назначаем имена столбцов
colnames(dataset) <- column_names

#Преобразуем необходимые столбцы в числовой формат (кроме первого со странами)
for (col in 2:ncol(dataset)){
  dataset[[col]] <- as.numeric(dataset[[col]])
}

dataset = dataset[2:7]

# 1. Выполнить дескриптивный анализ данных 

mean(as.matrix(dataset), na.rm = TRUE)
median(as.matrix(dataset), na.rm = TRUE)

moda <- function(x) {
  tab <- tabulate(x)
  is_max <- tab == max(tab)
  mean(which(is_max == TRUE))
}

moda(as.matrix(dataset))

results <- data.frame(
  Минимум = apply(dataset, 2, min, na.rm = TRUE),
  Q1 = apply(dataset, 2, quantile, probs = 0.25, na.rm = TRUE),
  Медиана = apply(dataset, 2, median, na.rm = TRUE),
  Q3 = apply(dataset, 2, quantile, probs = 0.75, na.rm = TRUE),
  Максимум = apply(dataset, 2, max, na.rm = TRUE),
  IQR = apply(dataset, 2, function(x) IQR(x, na.rm = TRUE)),
  Дисперсия=apply(dataset, 2, var, na.rm = TRUE),
  Среднекв.отклонение=apply(dataset, 2, sd, na.rm = TRUE)
)

for (col in 1:ncol(dataset)) {
  if(any(is.na(dataset[,col]))){ # Проверяем, есть ли NA в столбце
    median_val <- median(dataset[, col], na.rm = TRUE)
    dataset[is.na(dataset[, col]), col] <- median_val
    }
}

# 2.	Оценить оптимальное число кластеров, для этого построить диаграмму 
# "Метод силуэта", “Метод локтя”,  "Статистику разрыва" и Алгоритм консенсуса

dataset_scaled <- data.frame(scale(dataset))
dist.dataset_scaled <- dist(dataset_scaled) # матрица попарных расстояний
clust.dataset_scaled <- hclust(dist.dataset_scaled, "ward.D")

# “Метод локтя”
fviz_nbclust(dataset_scaled, kmeans, method = "wss")

# "Метод силуэта"
fviz_nbclust(dataset_scaled, kmeans, method = "silhouette") +
  labs(subtitle = "Silhouette method")

# "Статистика разрыва"
gap_stat <- clusGap(dataset_scaled, FUN = kmeans, nstart = 5,K.max =5, B = 5)
fviz_gap_stat(gap_stat)

# Алгоритм консенсуса
n_clust <- n_clusters(dataset_scaled, package = c("easystats", "NbClust", "mclust"), standardize = FALSE)
n_clust
plot(n_clust)

# 3. Выполнить иерархическую кластеризацию вашего набора данных, построив дендрограмму

clusters = 5
plot(clust.dataset_scaled, cex=0.5)
rect.hclust(clust.dataset_scaled, k=clusters, border="red")

# 4. Построить диаграмму со столбчатыми диаграммами и боксплотами групп

groups <- cutree(clust.dataset_scaled, k=5) 

g1<-colMeans(dataset_scaled[groups==1,])

g2<-colMeans(dataset_scaled[groups==2,])

g3<-colMeans(dataset_scaled[groups==3,])

g4<-colMeans(dataset_scaled[groups==4,])

g5<-colMeans(dataset_scaled[groups==5,])

df<-data.frame(g1,g2,g3,g4,g5)
df1<-t(df)
df<-t(df1)

barplot(df, ylim=c(0,12), axes = FALSE, 
        col=c("red","green","blue","yellow","pink"), beside=TRUE)
axis(2, at = 0:5, labels = 0:5)
legend("top", legend = rownames(df), col=c("red","green","blue","yellow","pink"), lwd=10, bty = "n")

# 5. Выполнить кластеризацию своего датасета по k-means.

km.res <- kmeans(as.matrix(dataset_scaled), 5, nstart = 10)
fviz_cluster(km.res, dataset_scaled[, -5], ellipse.type = "norm")

fviz_cluster(km.res, dataset_scaled[, -5],
             palette = "Set2", ggtheme = theme_minimal())

# 6.	Выполнить построение scatterplot с помощью функций plot или pairs

pairs(dataset_scaled)
colors <- c("red","green","blue","yellow","purple")
pairs(dataset_scaled,pch = 19, cex = 0.8,
      col = colors[groups],
      lower.panel=NULL)

# 7.	Построить трехмерную кластеризацию по scatterplot3d

groups <- cutree(clust.dataset_scaled, k=3) 
colors <- c("yellow", "green", "orange")
colors_groups <- colors[groups]
s3d <- scatterplot3d(dataset_scaled[2:4], pch=16, color=colors_groups)