dataset <- read.csv('D:/Универ/3 курс/6 семестр/Бигдата/Athlet_Events/athlete_events.csv')
hockey_players <- dataset[dataset$Sport == "Ice Hockey" & !is.na(dataset$Weight), c("Name", "Sex", "Weight", "Sport")]
hockey_players <- unique(hockey_players)

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

print("\n2. Статистика веса:")
mean_weight <- mean(hockey_players$Weight)
median_weight <- median(hockey_players$Weight)
mode_weight <- getmode(hockey_players$Weight)
variance_weight <- var(hockey_players$Weight)
sd_weight <- sd(hockey_players$Weight)  # Стандартное отклонение
iqr_weight <- IQR(hockey_players$Weight) # Интерквартильный размах

print(paste("Среднее:", mean_weight))
print(paste("Медиана:", median_weight))
print(paste("Мода:", mode_weight))
print(paste("Дисперсия:", variance_weight))
print(paste("Среднеквадратичное отклонение:", sd_weight))
print(paste("Межквартильный размах:", iqr_weight))

# Распределение веса по полу
print("\n3. Распределение веса по полу:")
aggregate(Weight ~ Sex, data = hockey_players, FUN = function(x) c(Mean = mean(x), Median = median(x), Mode = getmode(x), Variance = var(x), SD = sd(x)))
aggregate(Weight ~ Sex, data = hockey_players, FUN = length)   # Количество игроков по полу

# Boxplot для сравнения распределения веса по полу
boxplot(Weight ~ Sex, data = hockey_players,
        main = "Распределение веса хоккеистов по полу",
        xlab = "Пол", ylab = "Вес (кг)")

# Гистограмма для общего распределения веса
hist(hockey_players$Weight,
     main = "Распределение веса хоккеистов",
     xlab = "Вес (кг)", ylab = "Частота",
     col = "lightblue")

# Гистограммы для распределения веса по полу (опционально, если нужно более детально)
par(mfrow=c(1,2)) # Разделяем окно графика на 2 части
hist(hockey_players$Weight[hockey_players$Sex == "M"],
     main = "Вес хоккеистов (мужчины)",
     xlab = "Вес (кг)", ylab = "Частота",
     col = "lightblue")
hist(hockey_players$Weight[hockey_players$Sex == "F"],
     main = "Вес хоккеисток (женщины)",
     xlab = "Вес (кг)", ylab = "Частота",
     col = "pink")
par(mfrow=c(1,1)) 

boxplot(hockey_players$Weight, main = "Выбросы в весе хоккеистов", ylab = "Вес (кг)")

# Тест Шапиро-Уилка (Shapiro-Wilk test) и Q-Q plot.
shapiro_test_result <- shapiro.test(hockey_players$Weight)
print(shapiro_test_result)
hist(hockey_players$Weight)
qqnorm(hockey_players$Weight)  
qqline(hockey_players$Weight)

# Вывод о нормальности
alpha <- 0.05 # Уровень значимости
if (shapiro_test_result$p.value > alpha) {
  print("Вывод: Распределение веса, вероятно, нормально (тест Шапиро-Уилка не отвергает гипотезу о нормальности).")
} else {
  print("Вывод: Распределение веса, вероятно, не нормально (тест Шапиро-Уилка отвергает гипотезу о нормальности).")
}

# Тест Уискоксона
wilcox.test(hockey_players$Weight, mu=81, conf.int=TRUE)

# Тест Стьюдента.
t.test(hockey_players$Weight, mu=55, conf.int=TRUE)

#Задание 4 (сравнение двух независимых выборок - двухвыборочные тесты).

hockey.swimming <- dataset[dataset$Sport %in% c("Ice Hockey", "Swimming") & dataset$Sex == "F" & !is.na(dataset$Weight), c("Name", "Sex", "Weight", "Sport")]
hockey.swimming <- unique(hockey.swimming)

weights <- hockey.swimming[, "Weight"]

# Проверка на нормальность распределения
# Тест Шапиро-Уилкса (Shapiro-Wilk test)
shapiro.test(weights)
# Квантильно-квантильный график
qqnorm(weights)
qqline(weights)

# Проведём тест на равенство дисперсий для женщин
# Основная гипотеза - дисперсии равны.
# Тест Бартлетта - параметрический
bartlett.test(Weight~Sport, data=hockey.swimming)
# Тест Флингера-Киллина - непераметрический
fligner.test(Weight~Sport, hockey.swimming)

weights_only_hockey <- hockey.swimming[hockey.swimming$Sport == "Ice Hockey" , "Weight"]
weights_only_swimming <- hockey.swimming[hockey.swimming$Sport == "Swimming" , "Weight"]

# Проверка гипотезы о равенстве среднего веса женщин в двух разных видах спорта 
wilcox.test(weights_only_hockey, weights_only_swimming)

# Стьюдент
t.test(hockey.swimming$Weight~hockey.swimming$Sport)

