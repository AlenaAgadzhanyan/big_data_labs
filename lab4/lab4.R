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

# Функция для извлечения данных страны за конкретный год
extractCountryYearData <- function(allData, year, country, dataColumnIndex) {
  country_row <- getCountryRow(allData, year, country) # Get data row
  
  if (is.null(country_row) || nrow(country_row) == 0) {
    index_value <- NA  
  } else {
    index_value <- country_row[1, dataColumnIndex] 
  }
  
  df <- data.frame(value = index_value) 
  rownames(df) <- year 
  colnames(df) <- country 
  
  return(df)
}

# Функция для создания таблицы данных для одной страны за все годы
createCountryDataFrame <- function(allData, country, years, dataColumnIndex) {
  listDataByYears <- lapply(years, function(year) {
    extractCountryYearData(allData, year, country, dataColumnIndex)  # Apply extraction function
  })
  return(do.call("rbind", listDataByYears)) # Combine data frames by rows
}

# Главная функция для создания таблицы сравнения стран
makeCountriesComparisonDF <- function(allData, countries, dataColumnIndex) {
  years <- names(allData) # Get years from data names
  
  countriesDataList <- lapply(countries, function(country) {
    createCountryDataFrame(allData, country, years, dataColumnIndex) # Apply data frame creation function
  })
  return(do.call("cbind", countriesDataList))  # Combine data frames by columns
}


plot_data <- list(
  list(index = 1,  title = 'Индекс качества жизни (чем выше, тем лучше)', ylab = 'Индекс качества жизни', legend_pos = 'topleft', ylim = "auto", cex = 0.5),
  list(index = 2,  title = 'Индекс покупательной способности (чем выше, тем лучше)', ylab = 'Индекс покупательной способности', legend_pos = 'topleft', ylim = "auto", cex = 0.5),
  list(index = 3,  title = 'Индекс безопасности (чем выше, тем лучше)', ylab = 'Индекс безопасности', legend_pos = 'topleft', ylim = "ppi", cex = 0.5),
  list(index = 4,  title = 'Индекс медицинского обслуживания (чем выше, тем лучше)', ylab = 'Индекс медицинского обслуживания', legend_pos = 'topleft', ylim = "auto", cex = 0.45),
  list(index = 5,  title = 'Индекс прожиточного минимума (чем ниже, тем лучше)', ylab = 'Индекс прожиточного минимума', legend_pos = 'topleft', ylim = "ppi", cex = 0.6),
  list(index = 6,  title = 'Отношение цены на жильё к доходу (чем ниже, тем лучше)', ylab = 'Отношение цены на жильё к доходу', legend_pos = 'topleft', ylim = "auto", cex = 0.5),
  list(index = 7,  title = 'Индекс времени движения на дороге (чем ниже, тем лучше)', ylab = 'Индекс времени движения на дороге', legend_pos = 'topright', ylim = "auto", cex = 0.5),
  list(index = 8,  title = 'Индекс загрязнения (чем ниже, тем лучше)', ylab = 'Индекс загрязнения', legend_pos = 'topright', ylim = "auto", cex = 0.5),
  list(index = 9,  title = 'Климатический индекс (чем выше, тем лучше)', ylab = 'Климатический индекс', legend_pos = 'topleft', ylim = "auto", cex = 0.5)
)


# Функция для вычисления пределов ylim, обрабатывающая NA и бесконечные значения
calculate_ylim <- function(data, type = "auto", default_min = 0, default_max = 100) {
  data_matrix <- as.matrix(data)
  data_range <- range(data_matrix, na.rm = TRUE)
  min_val <- data_range[1]
  max_val <- data_range[2]
  
  if (!all(is.finite(data_range))) {
    min_val <- default_min
    max_val <- default_max
  }
  
  if (type == "auto") {
    # Разумные границы для обычных случаев
    min_val <- min_val - 0.05 * abs(min_val)  # 5% ниже минимального значения
    max_val <- max_val + 0.1 * abs(max_val)   # 10% выше максимального значения
  }
  
  return(c(min_val, max_val))
}

for (i in seq_along(plot_data)) {
  data_index <- plot_data[[i]]$index
  plot_title <- plot_data[[i]]$title
  plot_ylab <- plot_data[[i]]$ylab
  legend_position <- plot_data[[i]]$legend_pos
  ylim_type <- plot_data[[i]]$ylim
  legend_cex <- plot_data[[i]]$cex
  
  data <- makeCountriesComparisonDF(allData, countries, data_index)
  
  # Вычисляем ylim
  if (ylim_type == "auto") {
    ylim <- calculate_ylim(data, type = "auto")
  } else if (ylim_type == "ppi") {
    
    if (!exists("min_ppi") || !exists("max_ppi")) {
      PPIData <- makeCountriesComparisonDF(allData, countries, 2)  
      ppi_range <- range(as.matrix(PPIData), na.rm = TRUE)
      
      if (!all(is.finite(ppi_range))) {
        min_ppi <- 0  
        max_ppi <- 200  
        ppi_range <- c(min_ppi, max_ppi)  
      }
      range_extension <- 0.1 * diff(ppi_range)
      min_ppi <- ppi_range[1] - range_extension
      max_ppi <- ppi_range[2] + range_extension
    }
    ylim <- c(min_ppi, max_ppi)
  } else {
   
    ylim <- calculate_ylim(data, type="auto") 
  }
  
  
  matplot(
    years,
    data,
    type = "b",
    pch = 16,
    lty = 1,
    lwd = 1.8,
    cex = 0.8,
    col = colors,
    ylim = ylim,
    main = plot_title,
    xlab = 'Год',
    ylab = plot_ylab
  )
  
  legend(legend_position, countries, ncol = 3, cex = legend_cex, lty = 1, lwd = 2, col = colors)
}