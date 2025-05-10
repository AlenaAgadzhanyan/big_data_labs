library(igraph)

N <- 1

# 1. Создайте кольцевой граф g со случайным числом вершин G_size (от N+10 до (N/10+5)^2+5N). 
# Выведите число ребер и вершин этого графа. Постройте граф, выведите его матрицу смежности.

G_size <- sample((N+10):((N%/%10+5)**2+5*N), 1)
g <- graph.ring(n=G_size)
ecount(g)
vcount(g)
plot(g, edge.arrow.size=.5, vertex.size=20)
g[]

# 2. Создайте граф g1 из пустого графа с числом вершин G_size желтого цвета. Добавьте ему 8N 
# случайных ребер, сформированных из вектора вершин, окрасьте ребра красным цветом, 
# нарисуйте граф и выведите его матрицу смежности. Добавьте графу g1 еще 10N случайных ребер, 
# сформированных из вектора вершин, окрасьте ребра синим цветом, нарисуйте граф и выведите 
# его матрицу смежности.

g1 <- graph.empty()+vertices(1:G_size, color='yellow')
plot(g1, edge.arrow.size=.5, vertex.size=20)
g1 <- g1+edges(sample(V(g1), 2*8*N, replace=TRUE), color='red')
plot(g1, edge.arrow.size=.5, vertex.size=20)
g1[]
g1 <- g1+edges(sample(V(g1), 2*10*N, replace=TRUE), color='blue')
plot(g1, edge.arrow.size=.5, vertex.size=20)
g1[]

# 3. Добавьте ребра между вершиной 2N+23 и 2N+20, 2N+12 и N+15, 2N-1 и N+8, 2N и 2N+1, N+7 и N+13, 
# окрасьте их в черный цвет (предварительно проверьте существуют ли такие вершины – функцией
# %in% либо match, для несуществующих вершин ребра не добавляйте). Нарисуйте граф. 
# Выведите соседей N-й вершины, ребра, инцидентные этой вершине. 
# Соединены ли вершины N+10 и N+12? Выведите матрицу смежности.

v <- c(2*N+23, 2*N+20, 2*N+12, N+15, 2*N-1, N+8, 2*N, 2*N+1, N+7, N+13)
for (i in seq(1, length(v), 2)) {
  if (v[i] %in% V(g1) && v[i+1] %in% V(g1)) {
    g1 <- add.edges(g1, c(v[i],v[i+1]), color='black')
  }
}
plot(g1, edge.arrow.size=.5, vertex.size=20)
neighbors(g1, V(g1)[N], mode='out')
incident(g1, V(g1)[N], mode='all')
if ((N+10) %in% V(g1) && (N+12) %in% V(g1)) {
  are.connected(g1, V(g1)[N+10], V(g1)[N+12])
}
g1[]

# 4. Добавьте еще одну вершину и подключите ее к той, которая имеет наибольшее количество 
# связанных с ней узлов. Присвойте имена всем вершинам (например, буквы в алфавитном порядке – 
# используйте заглавные и, если не хватит, строчные буквы). Выведите матрицу смежности. 
# Выберите вершины, для которых значение связности меньше 5 и больше 2.

x <- length(V(g1))+1
g1 <- g1+vertices(x, color='green')
deg <- degree(g1, mode='all')
for (i in which(deg==max(deg))) {
  g1 <- g1+edges(c(x,i, i,x), color='green')
}
v <- c(toupper(letters[1:26]), tolower(letters[1:26]))
mn <- min(length(V(g1)), length(v))
g1 <- set_vertex_attr(g1, 'name', 1:mn, v[1:mn])
plot(g1, edge.arrow.size=.5, vertex.size1=20)
g1[]
deg <- degree(g1, mode='all')
v[which(deg<5&deg>2)]

# 5. Испробуйте алгоритмы размещения Вашего графа (in_circle, as_tree, lattice)

coords <- layout_(g1, in_circle())
plot(g1, layout=coords, edge.arrow.size=.2)
coords <- layout_(g1, as_tree())
plot(g1, layout=coords, edge.arrow.size=.2)
g1<-graph.lattice(length=47,dim=1,nei=5, circular=FALSE)
plot(g1,vertex.size=2,vertex.label=NA,layout=layout.kamada.kawai,edge.arrow.size=.1)

# 6. Выполните измерение диаметра графа g1, выведите список самых коротких путей для каждой 
# вершины и откалибруйте величины вершин согласно их степеней.

diameter(g1)
all_shortest_paths(g1, 1, to=V(g1), mode='all', weights=NULL)
deg <- degree(g1, mode='all')
plot(g1, edge.arrow.size=.2, vertex.size=deg)



# задание 2 вариант 1

# Функция для поиска оптимального места встречи
find_meeting_point <- function(N, K, roads) {
  
  graph <- graph.empty(n = N, directed = FALSE)
  
  # Добавляем ребра (дороги) в граф
  for (i in 1:K) {
    from <- roads[i, 1]
    to <- roads[i, 2]
    weight <- roads[i, 3]
    graph <- add.edges(graph, c(from, to), attr = list(weight = weight))
  }
  
  # Проверяем, является ли граф связным
  if (!is.connected(graph)) {
    stop("Граф не связный. Невозможно найти точку встречи.")
  }
  
  # Вычисляем матрицу кратчайших расстояний между всеми парами домов
  distances <- distances(graph, mode = "all", algorithm = "dijkstra")
  
  # Находим дом, от которого суммарное расстояние до всех остальных минимально
  min_sum_distance <- Inf
  optimal_meeting_point <- -1
  
  for (house in 1:N) {
    sum_distance <- sum(distances[house, ])
    if (sum_distance < min_sum_distance) {
      min_sum_distance <- sum_distance
      optimal_meeting_point <- house
    }
  }
  
  return(optimal_meeting_point)
}

N <- 5
K <- 6

roads <- matrix(c(
  1, 2, 10,  
  1, 3, 15,  
  2, 4, 12, 
  3, 5, 8,  
  4, 5, 5,   
  2, 3, 2   
), ncol = 3, byrow = TRUE)

optimal_house <- find_meeting_point(N, K, roads)

cat("Оптимальное место встречи: Дом", optimal_house, "\n")