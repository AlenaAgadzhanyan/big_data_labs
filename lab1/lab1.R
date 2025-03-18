# Task 1
p <- c(7:4)
q <- c(0:3)
print(p)
print(q)
print(p + q)
print(p - q)
print(p * q)
print(p/q)
print(p^q)

# Task 2
print(c(rbind(0, 1:10*2)))
print(2^(1:20))
print(10^(0:4))

# Task 3
print(sum(1/((1:50)*(2:51))))
print(sum(1/(2^(0:20))))
vector <- (1 + 3*(0:9))/3^(0:9)
print(sum(vector))
print(length(vector[vector > 0.5]))

# Task 4
vec3 <- seq(3, 27, 3)
print(vec3)
print(vec3[2])
print(vec3[5])
print(vec3[7])
print(vec3[length(vec3) - 1])
print(vec3[-(length(vec3) - 1)])
print(vec3[-6])
print(vec3[100])
print(vec3[-c(1, length(vec3))])
print(vec3[vec3 > 4 & vec3 < 10])
print(vec3[vec3 < 4 | vec3 > 10])

# Task 5.1
g <- c(1, 0, 2, 3, 6, 7, 12, 15, 0, NA, NA, 9, 4, 16, 2, 0)
print(g[1])
print(g[length(g)])
print(g[3:5])
print(g[g == 2])
print(g[g > 4])
print(g[g %% 3 == 0])
print(g[g %% 3 == 0 & g > 4])
print(g[g < 1 | g > 5])
print(which(g == 0))
print(which(g >= 2 & g <= 8))

# Task 5.16

m <- as.integer(readline(prompt = "Введите количество строк (m): "))
n <- as.integer(readline(prompt = "Введите количество столбцов (n): "))
set.seed(100)  
matr <- matrix(sample(1:100, m * n, replace = TRUE), nrow = m, ncol = n)
rownames(matr) <- paste("Row", 1:m)
colnames(matr) <- paste("Col", 1:n)
cat("Исходная матрица:\n")
print(matr)
cat("Субматрица из четных строк:\n")
print(matr[seq(2, m, by = 2), ])
cat("Вектор максимальных элементов столбцов:\n")
print(apply(matr, 2, max))