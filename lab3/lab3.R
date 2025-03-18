setwd('D:/Универ/3 курс/6 семестр/Бигдата/lab3')

library(readxl)

# 3.2

list_men <- data.frame(read_excel('статистика.xlsx', sheet='Мужчины'))
list_women <- data.frame(read_excel('статистика.xlsx', sheet='Женщины'))
rownames(list_men) <- list_men[,1] #для даты в круговой диаграмме
rownames(list_women) <- list_women[,1]  #для даты в круговой диаграмме
list_men <- list_men[,-1]
list_women <- list_women[,-1]

sum_column_men = sapply(list_men, sum)
sum_column_women = sapply(list_women, sum, na.rm=TRUE)

barplot(sum_column_men, names=c(1:8), xlab="Место", ylab="Количество", main="Мужчины")
barplot(sum_column_women, names=c(1:8), xlab="Место", ylab="Количество", main="Женщины")

gold_men = list_men[list_men[,1] > 0, 1]
gold_women = list_women[list_women[,1] > 0 & !is.na(list_women[,1]), 1]

names(gold_men) <- rownames(list_men[list_men[,1] > 0,])
names(gold_women) <- rownames(list_women[list_women[,1] > 0 & !is.na(list_women[,1]),])

legend_colors <- c("#F0F8FF", "#E6E6FA", "#FAF0E6", "#FFF0F5", "#FFFFF0", "#FFFAFA", "#F8F8FF", "#F5F5DC", "#FFF5EE")
pie(gold_men, labels=gold_men, main = "Кол-во золотых медалей по годам (мужчины)", col = legend_colors)
legend("topleft", legend=names(gold_men), cex=0.5, fill = legend_colors)
pie(gold_women, labels=gold_women, main = "Кол-во золотых медалей по годам (женщины)", col = legend_colors)
legend("topleft", legend=names(gold_women), cex=0.5, fill = legend_colors)


prize_places_trend_men <- data.frame("Количество призовых мест у мужчин"=rowSums(list_men[rownames(list_men) >= 1994, 1:3]))
prize_places_trend_women <- data.frame("Количество призовых мест у женщин"=rowSums(list_women[rownames(list_men) >= 1994, 1:3]))

prize_places_trend_women[is.na(prize_places_trend_women)] <- 0
prize_places_trend <- cbind(prize_places_trend_men, prize_places_trend_women)

color_men <- "lightblue"
color_women <- "pink"

matplot(rownames(prize_places_trend_men), prize_places_trend_men, xaxt="n", type="b", pch=16, lwd=4,
        main='Призовые места Канады по хоккею за последние 30 лет\n(мужчины)',
        xlab='Год', ylab='Количество призовых мест', col=color_men)
axis(side=1, at=rownames(prize_places_trend_men))
matplot(rownames(prize_places_trend_women), prize_places_trend_women, xaxt="n", type="b", pch=16, lwd=4,
        main='Призовые места Канады по хоккею за последние 30 лет\n(женщины)',
        xlab='Год', ylab='Количество призовых мест', col=color_women, ylim=c(0, 3))
axis(side=1, at=rownames(prize_places_trend_women))

# 3.3

colors2 <- c("#1c76b3", "#ff7d0c", "#2a9f2a", "#d62526", "#9366bc", "#8d564c", "#ffb900")
par(mfrow=c(1,1))

gold_medals <- data.frame(read_excel('статистика.xlsx', sheet='золото'))
rownames(gold_medals) <- gold_medals[,1]
gold_medals <- gold_medals[,-1]
gold_medals_summer <- gold_medals[c(1,3,5),]
gold_medals_winter <- gold_medals[c(2,4,6),]

matplot(
  rownames(gold_medals),
  gold_medals,
  xaxt = "n",
  type = "b",
  pch = 16,
  lty = 1,
  lwd = 4,
  xlim = c(2014, 2029),
  ylim = c(0, 60),
  main = 'Спортивные достижения по золотым медалям за последние 6 олимпиад',
  xlab = 'Год',
  ylab = 'Количество золотых медалей',
  col = colors2
)
axis(side=1, at=rownames(gold_medals))
legend('topright', colnames(gold_medals), lwd=4, col=colors2)

prize_places <- data.frame(read_excel('статистика.xlsx', sheet='призовые'))
rownames(prize_places) <- prize_places[,1]
prize_places <- prize_places[,-1]
prize_places_summer <- prize_places[c(1,3,5),]
prize_places_winter <- prize_places[c(2,4,6),]
matplot(rownames(prize_places), prize_places, xaxt="n", type="b", pch=16, lty=1, lwd=4,
        xlim=c(2014, 2030), ylim=c(0, 150),
        main='Спортивные достижения по призовым местам за последние 6 олимпиад',
        xlab='Год', ylab='Количество призовых мест', col=colors2)
axis(side=1, at=rownames(gold_medals))
legend('topright', colnames(gold_medals), lwd=4, col=colors2)

# 3.4

prize_places_last_6 <- head(prize_places_trend, 6)
par(mfrow=c(1,3))
matplot(rownames(prize_places_last_6), prize_places_last_6, xaxt="n", type="b", pch=16, lty=c(2,3), lwd=4,
        ylim=c(0, 3.5), xlab='Год', ylab='Количество призовых мест', col=c(color_men, color_women))
axis(side=1, at=rownames(prize_places_last_6))
legend('topleft', c('Мужчины', 'Женщины'), lty=c(2,3), lwd=4, col=c(color_men, color_women))
barplot(t(as.matrix(prize_places_last_6)), beside=TRUE, xlab="Год", ylab="Количество призовых мест",
        ylim=c(0, 4), col=c(color_men, color_women),
        main="Призовые места Канады по хоккею на льду за последние 6 игр")
legend('topleft', c('Мужчины', 'Женщины'), fill=c(color_men, color_women))
prize_places_last_6_sum <- sapply(prize_places_last_6, sum)
pie(prize_places_last_6_sum, labels=c(prize_places_last_6_sum[1], prize_places_last_6_sum[2]),
    col=c(color_men, color_women), sub="Сумма за 6 игр (мужчины и женщины)")
legend('topleft', c('Мужчины', 'Женщины'), fill=c(color_men, color_women))