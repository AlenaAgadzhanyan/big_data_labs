library(rvest)
library(dplyr)

data <- read_table2("D:/Универ/3 курс/6 семестр/Бигдата/assess.dat")

# Функция для подготовки данных и построения дендрограммы
prepare_dendrogram_data <- function(data) {
  clustering_data <- data[, 3:ncol(data)]
  
  
  for (i in 1:ncol(clustering_data)) {
    mean_val <- mean(clustering_data[[i]], na.rm = TRUE)
    clustering_data[[i]][is.na(clustering_data[[i]])] <- mean_val
  }
  distance_matrix <- dist(clustering_data, method = "euclidean")
  
  hc <- hclust(distance_matrix, method = "ward.D2")
  
  return(hc)
}

# Функция для проведения анализа кластеров и вывода информации
perform_cluster_analysis <- function(data, hc, k) {
  clusters <- cutree(hc, k = k)
  
  cat("\nАнализ с", k, "кластерами:\n")
  for (i in 1:k) {
    cluster_members <- data[clusters == i, ]
    cat("\nКластер", i, ":\n")
    print(cluster_members$NAME)
    
    cluster_mean <- colMeans(cluster_members[,3:ncol(cluster_members)])
    cat("\nСредние значения оценок задач для кластера", i, ":\n")
    print(cluster_mean)
    
    cat("\nРекомендации по выбору первого участника из каждого кластера:\n")
    
    cluster_members <- data[clusters == i, ] 
    cluster_members$sum_tasks <- rowSums(cluster_members[,3:ncol(cluster_members)])
    cluster_members_sorted <- cluster_members[order(-cluster_members$sum_tasks),]
    
    cat("\nКластер", i, ":\n")
    cat("Рекомендуем выбрать", cluster_members_sorted$NAME[1], "первым, основываясь на сумме баллов по задачам.\n")
    cat("Сумма баллов:", cluster_members_sorted$sum_tasks[1], "\n")
    cat("Особенно хорошо он/она справляется со следующими задачами:\n")
    excellent_tasks <- names(cluster_members_sorted[,3:(ncol(cluster_members_sorted)-1)])[cluster_members_sorted[1,3:(ncol(cluster_members_sorted)-1)] >= 9]
    print(excellent_tasks)
  }
  
  rect.hclust(hc, k = 3, border = "red")  # Обводим кластеры красным цветом
  rect.hclust(hc, k = 4, border = "blue")
}


hc <- prepare_dendrogram_data(data)

plot(hc,
     labels = data$NAME,
     main = "Дендрограмма иерархической кластеризации",
     xlab = "",
     ylab = "Высота",
     sub = "",
     cex = 0.8)

perform_cluster_analysis(data, hc, 3)

perform_cluster_analysis(data, hc, 4)