install.packages("rvest")
options(timeout = 300)
install.packages("stringi")
library(stringi)
library(rvest)

years <- 2014:2021

countries <- c("Switzerland", "Germany", "Australia", "Latvia", "Israel")
colors <- c("blue", "green", "red", "purple", "orange")

getData <- function(year) {
  cat(sprintf("Получение данных за %d год...", year))
  url <- sprintf("https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=%d", year)
  page <- read_html(url)
  table_node <- html_nodes(page, "table#t2")[[1]]
  table <- as.data.frame(html_table(table_node, na.strings="-"))
  table <- table[-1] # убираем RANK
  cat(" Готово\n")
  return(table)
}

getCountryRow <- function(allData, year, country) {
  table <- allData[as.character(year)][[1]]
  table <- subset(table, table$Country == country)
  table <- table[-1]
  return(table)
}

allData <- lapply(years, getData) #возвращает список той же длины, что и years, в котором к каждому элементу years была применена функция 
names(allData) <- years

makeCountriesComparisonDF <- function(allData, countries, dataColumnIndex) {
  years <- names(allData)
  countriesDataList <- lapply(countries, function(country) {
    listDataByYears <- lapply(years, function(year) {  
      country_row <- getCountryRow(allData, year, country)
      if (is.null(country_row) || nrow(country_row) == 0) {
        index_value <- NA
      } else {
        index_value <- country_row[1, dataColumnIndex]
      }
      
      df <- data.frame(value = index_value)
      rownames(df) <- year 
      colnames(df) <- country
      return(df)
    })
    do.call("rbind", listDataByYears)
  })
  do.call("cbind", countriesDataList)
}

QOLIData <- makeCountriesComparisonDF(allData, countries, 1)

# Вычисляем min и max с обработкой NA
min_qoli <- min(as.matrix(QOLIData), na.rm = TRUE)
max_qoli <- max(as.matrix(QOLIData), na.rm = TRUE)

# Проверяем, являются ли min и max конечными, и устанавливаем значения по умолчанию, если нет
if (!is.finite(min_qoli)) {
  min_qoli <- 0  # Значение по умолчанию
}
if (!is.finite(max_qoli)) {
  max_qoli <- 100  # Значение по умолчанию
}

matplot(
  years,
  QOLIData,
  type="b",
  pch=16,
  lty=1,
  lwd=1.8,
  cex=0.8,
  col=colors,
  ylim=c(min_qoli - 5, max_qoli + 100),
  main='Индекс качества жизни (чем выше, тем лучше)',
  xlab='Год',
  ylab='Индекс качества жизни'
)
legend('topleft', countries, ncol=3, lty=1, lwd=2, col=colors)

# Второй график (Индекс покупательной способности)
PPIData <- makeCountriesComparisonDF(allData, countries, 2)

# Вычисляем диапазон для PPIData
ppi_range <- range(as.matrix(PPIData), na.rm = TRUE)

# Проверяем, являются ли значения конечными, и устанавливаем значения по умолчанию, если нет
if (!all(is.finite(ppi_range))) {
  min_ppi <- 0  # Значение по умолчанию
  max_ppi <- 200  # Значение по умолчанию (увеличено)
  ppi_range <- c(min_ppi, max_ppi)  # Устанавливаем диапазон вручную
}

# Расширяем диапазон на 10% (можно изменить этот процент)
range_extension <- 0.1 * diff(ppi_range)
min_ppi <- ppi_range[1] - range_extension
max_ppi <- ppi_range[2] + range_extension

matplot(
  years,
  PPIData,
  type="b",
  pch=16,
  lty=1,
  lwd=1.8,
  cex=0.8,
  col=colors,
  ylim=c(min_ppi, max_ppi),  # Используем вычисленные min_ppi и max_ppi
  main='Индекс покупательной способности (чем выше, тем лучше)',
  xlab='Год',
  ylab='Индекс покупательной способности'
)
legend('topleft', countries, ncol=3, cex=0.5, lty=1, lwd=2, col=colors)

SIData <- makeCountriesComparisonDF(allData, countries, 3)
matplot(
  years,
  SIData,
  type="b",
  pch=16,
  lty=1,
  lwd=1.8,
  cex=0.8,
  col=colors,
  ylim=c(min_ppi, max_ppi),  # Используем вычисленные min_ppi и max_ppi
  main='Индекс безопасности (чем выше, тем лучше)',
  xlab='Год', 
  ylab='Индекс безопасности'
)
legend('topleft', countries, ncol=3, lty=1, lwd=2, col=colors)

HCIData <- makeCountriesComparisonDF(allData, countries, 4)
matplot(
  years,
  HCIData,
  type="b",
  pch=16,
  lty=1,
  lwd=1.8,
  cex=0.8,
  col=colors,
  main='Индекс медицинского обслуживания (чем выше, тем лучше)',
  xlab='Год', 
  ylab='Индекс медицинского обслуживания'
)
legend('topleft', countries, ncol=3, lty=1, cex = 0.45, lwd=2, col=colors)


COLIData <- makeCountriesComparisonDF(allData, countries, 5)
matplot(
  years,
  COLIData,
  type="b",
  pch=16,
  lty=1,
  lwd=1.8,
  cex=0.8,
  col=colors,
  ylim=c(min_ppi, max_ppi),
  main='Индекс прожиточного минимума (чем ниже, тем лучше)',
  xlab='Год', 
  ylab='Индекс прожиточного минимума'
)
legend('topleft', countries, ncol=3, lty=1, cex = 0.6, lwd=2, col=colors)

PPTIRData <- makeCountriesComparisonDF(allData, countries, 6)
matplot(
  years,
  PPTIRData,
  type="b",
  pch=16,
  lty=1,
  lwd=1.8,
  cex=0.8,
  col=colors,
  main='Отношение цены на жильё к доходу (чем ниже, тем лучше)',
  xlab='Год', 
  ylab='Отношение цены на жильё к доходу'
  
)
legend('topleft', countries, ncol=3, lty=1, cex = 0.5, lwd=2, col=colors)

TCTIData <- makeCountriesComparisonDF(allData, countries, 7)
matplot(
  years,
  TCTIData,
  type="b",
  pch=16,
  lty=1,
  lwd=1.8,
  cex=0.8,
  col=colors,
  main='Индекс времени движения на дороге (чем ниже, тем лучше)',
  xlab='Год', 
  ylab='Индекс времени движения на дороге'
)
legend('topright', countries, ncol=3, lty=1, lwd=2, col=colors)

PIData <- makeCountriesComparisonDF(allData, countries, 8)
matplot(
  years,
  PIData,
  type="b",
  pch=16,
  lty=1,
  lwd=1.8,
  cex=0.8,
  col=colors,
  main='Индекс загрязнения (чем ниже, тем лучше)',
  xlab='Год', 
  ylab='Индекс загрязнения'
)
legend('topright', countries, ncol=3, cex=0.5, lty=1, lwd=2, col=colors)

CIData <- makeCountriesComparisonDF(allData, countries, 9)
matplot(
  years,
  CIData,
  type="b",
  pch=16,
  lty=1,
  lwd=1.8,
  cex=0.8,
  col=colors,
  main='Климатический индекс (чем выше, тем лучше)',
  xlab='Год', 
  ylab='Климатический индекс'
)
legend('topleft', countries, ncol=3, cex=0.5, lty=1, lwd=2, col=colors)