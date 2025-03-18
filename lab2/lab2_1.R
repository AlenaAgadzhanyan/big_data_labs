answers = read.csv("C:/Users/Honor/Desktop/Универ/3 курс/6 семестр/Бигдата/films.csv")
answers_without_surname <- subset(answers, select = -c(1,2))
cat('max по столбцам:\n')
max_value = sapply(answers_without_surname, max)
print(max_value)
cat('min по столбцам:\n')
min_value = sapply(answers_without_surname, min)
print(min_value)
cat('mean по столбцам:\n')
mean_value = colMeans(answers_without_surname)
print(mean_value)

f <- function(x) {
    y = answers_without_surname[,x]
    cat('Название фильма:', x, '\n')
    y1 <- y[y > 7]
    y2 <- y[y < 3]
    names1 <- answers[y > 7, 'Фамилия']
    names2 <- answers[y < 3, 'Фамилия']
    cat('Количество людей, оценивших выше 7 баллов:', length(y1), '\n')
    cat('Вектор респондентов:\n')
    print(names1)
    cat('Количество людей, оценивших ниже 3 баллов:', length(y2), '\n')
    cat('Вектор респондентов:\n')
    print(names2)
    cat('\n')
}
people_preference <- sapply(colnames(answers_without_surname), f)

cat('Фильмы, отсортированные по убыванию рейтинга:\n')
print(answers_without_surname[order(-mean_value)])

cat('\n Показан график средних значений.')
par(mar=c(15,3,1,1)) 
barplot(mean_value, las=2) # параметр las=2 поворачивает надписи

films_name <- colnames(answers_without_surname)
indeces <- 1:length(films_name) # Создаем индексы для оси X
plot(indeces, mean_value,
     type = "n",       
     xlim = c(0.5, length(films_name) + 0.5),  # Задаем пределы для оси X, чтобы столбцы не выходили за края
     ylim = c(0, max(mean_value) + 1),  # Задаем пределы для оси Y
     xlab = "",  
     main = "Оценки фильмов", 
     axes = FALSE        # Отключаем автоматическую отрисовку осей
)

# Рисуем оси вручную
axis(1, at = indeces, labels = films_name, las = 2)
axis(2, at = seq(0, max(mean_value), by = 1), las = 2)

# Рисуем столбцы (прямоугольники)
for (i in 1:length(films_name)) {
  rect(i - 0.4, 0, i + 0.4, mean_value[i],  # Координаты прямоугольников
       col = "skyblue", border = "black") 
}
grid(nx = NA, ny = NULL, lty = 2, col = "lightgray")# Добавляем сетку

mean_v = colMeans(answers_without_surname)

count_07 <- integer(10)
count_03 <- integer(10)

print("Количество людей с предпочтениями >7:")
count_03 <-colSums(answers_without_surname > 7);count_03
print("Количество людей с предпочтениями <3:")
count_07 <-colSums(answers_without_surname < 3);count_07

rating <- integer(10)
rating<-sort(sapply(answers[2:11], mean, na.rm = TRUE),decreasing = TRUE);rating

print(rating)

cat('\n Показан график средних значений.\n')
par(mar=c(15,3,1,1)) 
barplot(rating, las=2) 
