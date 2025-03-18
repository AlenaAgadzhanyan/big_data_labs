answers = read.csv("D:/Универ/3 курс/6 семестр/Бигдата/films.csv")

scores <- answers[-(1:2)] 


# Дескриптивный анализ

# Добавим функцию моды 
moda <- function(x) {
  tab <- tabulate(x)
  is_max <- tab == max(tab)
  mean(which(is_max == TRUE))
}

# Общее среднее, медиана и мода по всей таблице
cat("Mean = ", mean(as.matrix(scores)), "\n")
cat("Median =", median(as.matrix(scores)), "\n")
cat("Moda = ",moda(as.matrix(scores)), "\n")

# Минимум, максимум, 1 и 3 квартили, медиана и среднее для каждого столбца
cat("", summary(scores), "\n")
sum = summary(scores)


# По отдельности (+ мода)

minValues <- data.frame(Мин.=apply(scores, 2, min)) 
maxValues <- data.frame(Макс.=apply(scores, 2, max))
rangeValues <- maxValues - minValues; colnames(rangeValues) <- 'Размах'
medianValues <- data.frame(Медиана=apply(scores, 2, median))
modeValues <- data.frame(Мода=apply(scores, 2, moda))
meanValues <- data.frame(Среднее=apply(scores, 2, mean))
cbind(minValues, maxValues, rangeValues, medianValues, modeValues, meanValues)

IQR(scores$Дюна) # межквартильный размах (3квартиль - 1квартиль)

sdValues <- data.frame(Дисперсия=apply(scores, 2, var))
varValues <- data.frame(Среднекв.отклонение=apply(scores, 2, sd))
cbind(sdValues, varValues)

hist(as.matrix(scores), main="Гистограмма частот оценок по всему датасету")
plot(density(as.matrix(scores)))
hist(scores$Дюна, main="Гистограмма частот оценок Дюна")
plot(density(scores$Дюна))

m = apply(scores, 2, moda)
par(mar=c(5,11,1,1)) 
barplot(m,horiz = TRUE,las=1)

mark <- tabulate(as.matrix(scores))
barplot(mark,horiz = TRUE,las=1)

boxplot(scores, main = "Боксплот ", las=2, xlab="", ylab="Оценки")

t(scores[order(-colMeans(scores))])


