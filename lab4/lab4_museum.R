library(rvest)

# URL страницы Википедии
url <- "https://ru.wikipedia.org/wiki/Список_музеев_Ростовской_области"

# Чтение HTML-кода страницы
page <- read_html(url)

# Определение CSS-селектора для таблицы (ищем таблицы с классом 'wikitable')
table_nodes <- html_nodes(page, "table.wikitable")

museum_table <- table_nodes[[1]]

table_rows <- html_nodes(museum_table, "tr")

if (length(table_rows) > 1) {
  table_rows <- table_rows[-1] # Убираем первую строку
}

second_column_cells <- html_nodes(table_rows, "td:nth-child(2)")

links <- html_nodes(second_column_cells, "a:not(a[href^=\"#cite_note\"])")

museum_links <- html_attr(links, "href")

base_url <- "https://ru.wikipedia.org"

full_museum_links <- paste0(base_url, museum_links)
full_museum_links <- unique(full_museum_links)

full_museum_links <- full_museum_links[full_museum_links != ""]


museum_links_df <- data.frame(
  Ссылка_на_музей = full_museum_links,
  stringsAsFactors = FALSE
)

museum <- html_table(table_nodes[[1]], fill = TRUE)
museum <- museum[, -ncol(museum)]
museum <- museum[, -ncol(museum)]

merged_df <- cbind(museum, museum_links_df)

