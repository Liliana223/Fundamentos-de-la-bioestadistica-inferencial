# Fundamentos de la bioestadistica inferencial
# Prof. Astrid Liliana Vargas Sanchez
# Introducción a R para bioestadistica
# Modulo 1

# -------------------------------
# Asignación de variables
# -------------------------------
valor_a = 112.3
valor_b <- 25L

valor_c<-12L # Numeros enteros. Se puede crear variables sin dejar espacios sin embargo se recomienda dejar espacios
print(valor_c) # por legibilidad y estilo profesional

texto <- "Hola mundo" # Se sugiere usar nombres que indiquen claramente el contenido de la variable
print(texto)

logico <- TRUE
print(logico)

30 -> numero_dos

# -------------------------------
# Verificar el tipo de variable
# -------------------------------
print(class(valor_a))   # "numeric"
print(class(valor_b))   # "integer"
print(class(valor_c))  # "integer"
print(class(texto))   # "character"
print(class(logico))  # "logical"

# -------------------------------
# Operadores aritméticos
# -------------------------------
suma <- valor_a + valor_b
resta <- valor_a - valor_c
producto <- valor_b * valor_b
division <- valor_b / valor_b
potencia <- valor_a^valor_b
modulo <- valor_b %% valor_c     # Residuo

# Mostrar resultados
print(paste("Suma:", suma)) # paste: unir (concatenar) varios elementos en una sola cadena de texto.
print(paste("Producto:", producto))

# -------------------------------
# Operadores comparativos
# -------------------------------
print(valor_a > valor_b)     # TRUE
print(valor_a == valor_b)    # FALSE

# -------------------------------
# Operadores lógicos
# -------------------------------
valor_x <- TRUE

valor_y <- FALSE

print(valor_x & valor_y)     # FALSE 
print(valor_x | valor_y)     # TRUE 

# 1. VECTORES (con c())
vector_numerico <- c(3, 2, 3, 4, 8)

print("Vector numérico:")
print(vector_numerico)

vector_caracter <- c("rojo", "azul", "verde")

print("Vector de caracteres:")
print(vector_caracter)

# 2. MATRICES (con matrix())
matriz <- matrix(1:9, nrow = 3, ncol = 3)
print("Matriz 3x3:")
print(matriz)

# 3. DATA FRAMES (tablas de datos)
df <- data.frame(
  nombre = c("Carlos", "Luisa", "Pedro"),
  edad = c(25, 30, 22),
  aprobado = c(TRUE, TRUE, FALSE)
)
print("Data frame:")
print(df)

# -------------------------------
# Conjuntos de datos
# -------------------------------

# Usamos el dataset iris que ya se encuentra en R
data(iris) 

# -------------------------------
# Probabilidad
# -------------------------------

# Supongamos que lanzamos un dado de 6 caras, y queremos estimar la probabilidad de que salga un número 
# mayor que 4 (es decir, 5 o 6).

# Probabilidad teórica
eventos_favorables <- 2  # números 5 y 6
espacio_muestral <- 6    # total de caras del dado

probabilidad <- eventos_favorables / espacio_muestral
probabilidad #  Hay una probabilidad del 33,3% de que el evento ocurra.


set.seed(123)  # Esto fija una "semilla" para el generador de números aleatorios.
# Sirve para que la simulación sea reproducible: cada vez que ejecutes el código, obtendrás los mismos resultados.
# Si no usas set.seed(), cada vez obtendrás resultados ligeramente distintos.

lanzamientos <- sample(1:6, size = 10000, replace = TRUE) #  Simulando 10.000 lanzamientos de un dado de 6 caras
lanzamientos

# La función sample() en R se utiliza para seleccionar elementos aleatoriamente de un conjunto. Es muy útil para 
# hacer simulaciones, muestreos y juegos de azar (dados, monedas, cartas, etc.).
# replace = TRUE (cada número del 1 al 6 puede repetirse en los 10 000 valores generados)


# Contar cuántos valores son mayores que 4
favorables <- sum(lanzamientos > 4)

# Probabilidad empírica o experimental
probabilidad_empirica <- favorables / length(lanzamientos) # length() cuenta cuantos elementos hay en el vector 
print(probabilidad_empirica) # Hay una probabilidad del 33,5% de que el evento ocurra.

# --------------------------------------------
# Medidas de tendencia central en R
# --------------------------------------------

# 1. Crear un vector de datos numéricos
datos <- c(10, 20, 30, 30, 40, 50, 50, 50, 60)

# Ver los datos
print("Datos:")
print(datos)

# --------------------------------------------
# 2. Calcular la media (promedio)
# --------------------------------------------
media <- mean(datos)
cat("Media:", media, "\n")

# --------------------------------------------
# 3. Calcular la mediana (valor central)
# --------------------------------------------
mediana <- median(datos)
cat("Mediana:", mediana, "\n")

# --------------------------------------------
# 4. Calcular la moda (valor que más se repite)
# --------------------------------------------

# install.packages("DescTools")   # Solo la primera vez
library(DescTools)

moda <- Mode(datos)

cat("Moda:", moda, "\n")

# --------------------------------------------
# Medidas de dispersión en R
# --------------------------------------------

# 1. Crear un vector de datos numéricos
datos <- c(10, 20, 30, 30, 40, 50, 50, 50, 60)

# Ver los datos
print("Datos:")
print(datos)

# --------------------------------------------
# 2. Varianza
# --------------------------------------------
varianza <- var(datos)
cat("Varianza:", varianza, "\n")

# --------------------------------------------
# 3. Desviación estándar
# --------------------------------------------
desviacion <- sd(datos)
cat("Desviacion estandar:", desviacion, "\n")


# -------------------------------
# Medidas de forma
# -------------------------------

# install.packages("moments")  # Solo una vez

library(moments)             # Cargarlo cada vez

# Crear un vector de datos numéricos
datos <- c(10, 20, 30, 30, 40, 50, 50, 50, 60, 80, 100)

# Mostrar datos
print("Datos:")
print(datos)

# --------------------------------------------
# Asimetría (Skewness): Grado de distribución de una distribución de datos.
# --------------------------------------------
asimetria <- skewness(datos)
cat("Coeficiente de asimetria:", asimetria, "\n")

# --------------------------------------------
# Interpretación básica
# --------------------------------------------
if (asimetria > 0) {
  cat("La distribucion esta sesgada a la derecha (asimetria positiva).\n")
} else if (asimetria < 0) {
  cat("La distribucion esta sesgada a la izquierda (asimetria negativa).\n")
} else {
  cat("La distribucion es simetrica.\n")
}

# --------------------------------------------
# Curtosis: Permite evaluar el grado de concentración de una distribución de datos en torno a su media. 
# Determina si una distribución es más puntiaguda o aplanada. 
# --------------------------------------------
curtosis <- kurtosis(datos)
cat("Curtosis:", curtosis, "\n")

if (curtosis > 3) {
  cat("Distribucion leptocurtica (mas apuntada que la normal).\n")
} else if (curtosis < 3) {
  cat("Distribucion platicurtica (mas plana que la normal).\n")
} else {
  cat("Distribucion mesocurtica (similar a la normal).\n")
}

# Creación de grafica para visualizar los datos

# install.packages("ggplot2")  # Para gráficos
library(ggplot2)

# --------------------------------------------
# Crear un data frame para ggplot
# --------------------------------------------
df <- data.frame(valores = datos)

# --------------------------------------------
# Graficar histograma con curva de densidad
# --------------------------------------------
ggplot(df, aes(x = valores)) +
  geom_histogram(aes(y = after_stat(density)), # En lugar de graficar el número de casos en cada barra, use la densidad estimada
                 bins = 10, # 10 barras
                 fill = "skyblue", 
                 color = "black", 
                 alpha = 0.7) +
  geom_density(color = "red", linewidth = 1.2) + # Para superponer una curva de densidad
  geom_vline(aes(xintercept = mean(valores)),
             color = "blue", linetype = "dashed", size = 1) +
  labs(
    title = "Distribución con histograma y curva de densidad",
    subtitle = paste0("Asimetría = ", round(asimetria, 2), 
                      " | Curtosis = ", round(curtosis, 2)),
    x = "Valores",
    y = "Densidad"
  ) +
  theme_minimal()

# La distribucion esta sesgada a la derecha (asimetria positiva).
# Distribucion platicurtica (mas plana que la normal).

# Densidad: Se usa para comparar cómo se distribuyen los datos en lugar de cuántos hay exactamente en cada intervalo.

# -------------------------------
# Pruebas de normalidad
# -------------------------------


# Crear un conjunto de datos (simulación normal)
set.seed(123)
datos <- rnorm(100, mean = 50, sd = 10) # rnorm() se usa para generar datos simulados que siguen una distribución 
#                                         normal.

# -------------------------------
# Shapiro-Wilk Test:  Muestras pequeñas y medianas (n < 5000).
# -------------------------------

shapiro.test(datos) # Los datos podrían ser normales.

# Interpretación:

# Si el p-value > 0.05 los datos podrían ser normales.
# Si el p-value ≤ 0.05 los datos no son normales.

# -------------------------------
# Kolmogorov-Smirnov Test (K-S): compara la distribución empírica de los datos con una distribución teórica.
# -------------------------------

# Estandarizar los datos
datos.z <- scale(datos)

# Estandarizar un dato significa convertirlo a una escala común
# Primero necesitas estandarizar los datos y luego aplicar el test con la distribución normal como referencia

# Kolmogorov-Smirnov con distribución normal
ks.test(datos.z, "pnorm") # Los datos podrían ser normales.

# Interpretación:

# Si el p-value > 0.05 los datos podrían ser normales.
# Si el p-value ≤ 0.05 los datos no son normales.


# -------------------------------
# Anderson-Darling Test
# -------------------------------

# install.packages("nortest")   # Solo la primera vez
library(nortest)

# Anderson-Darling test
ad.test(datos) # Los datos podrían ser normales.

# Interpretación:

# Si el p-value > 0.05 los datos podrían ser normales.
# Si el p-value ≤ 0.05 los datos no son normales.


# -------------------------------
# Grafico visual
# -------------------------------

# Gráfico Q-Q

qqnorm(datos)
qqline(datos, col = "red", lwd = 2)

# Si los puntos siguen la línea roja, los datos son aproximadamente normales.


# Histograma

hist(datos,
     main = "Histograma de los datos simulados",
     xlab = "Valores",
     ylab = "Frecuencia",
     col = "lightblue",
     border = "white")

# Histograma con ggplot2

# install.packages("ggplot2")  # Para gráficos
library(ggplot2)

# Convertir en dataframe
df <- data.frame(valor = datos)

# Se construye un gráfico de frecuencias absolutas

ggplot(df, aes(x = valor)) +
  geom_histogram(aes(y = ..density..), bins = 20, fill = "lightblue", color = "white") +
  stat_function(fun = dnorm,
                args = list(mean = mean(df$valor), sd = sd(df$valor)), # agrega la curva normal teórica, usando 
                                                                       # la media y desviación estándar de tus datos.
                color = "red", size = 1.2) +
  labs(title = "Histograma con Curva Normal",
       x = "Valores", y = "Densidad") +
  theme_minimal()

# -------------------------------
# Homogeneidad de varianzas
# -------------------------------

set.seed(123)

# Crear un data frame con 3 grupos
grupo <- rep(c("A", "B", "C"), each = 30)

# Generar datos con diferente varianza
valor <- c(rnorm(30, mean = 50, sd = 5),    # Grupo A.30 Genera 30 números aleatorios que siguen una distribución normal
           rnorm(30, mean = 55, sd = 10),   # Grupo B. Media de 55 y desviación estandar de 10
           rnorm(30, mean = 60, sd = 5))    # Grupo C

datos <- data.frame(grupo = factor(grupo), valor = valor)

# -------------------------------
# Prueba de Levene (más robusta ante no normalidad)
# -------------------------------

# install.packages("car")  # Solo la primera vez
library(car)

leveneTest(valor ~ grupo, data = datos)

# Interpretación:

# Si el p-value > 0.05 las varianzas podrían ser similares.
# Si el p-value ≤ 0.05 llas varianzas podrían ser diferentes.

# Valor de p: Pr(>F) = 0.003395. Las varianzas son diferentes (heterocedasticidad)

# -------------------------------
# Prueba de Bartlett (sensible a desviaciones de la normalidad)
# -------------------------------

bartlett.test(valor ~ grupo, data = datos) # Las varianzas son diferentes (heterocedasticidad)

# Interpretación:

# Si el p-value > 0.05 las varianzas podrían ser similares.
# Si el p-value ≤ 0.05 llas varianzas podrían ser diferentes.

# -------------------------------
# Prueba de Fligner-Killeen (no paramétrica, muy robusta)
# -------------------------------

fligner.test(valor ~ grupo, data = datos) # Las varianzas son diferentes (heterocedasticidad)

# Interpretación:

# Si el p-value > 0.05 las varianzas podrían ser similares.
# Si el p-value ≤ 0.05 llas varianzas podrían ser diferentes.

# -------------------------------
# Transformación de los datos
# -------------------------------

set.seed(123)
datos <- rexp(100, rate = 0.2)  # Datos con distribución exponencial (asimétricos)

shapiro.test(datos)  # Probablemente p < 0.05 → no normal

# -------------------------------
# a) Transformación logarítmica
# -------------------------------

datos_log <- log(datos)
shapiro.test(datos_log) # p < 0.05 → no normal

# Gráfico Q-Q

qqnorm(datos_log)
qqline(datos_log, col = "red", lwd = 2)

# Los puntos no siguen la línea roja.

# -------------------------------
# b) Transformación de raíz cuadrada
# -------------------------------

datos_sqrt <- sqrt(datos)
shapiro.test(datos_sqrt) # p < 0.05 → no normal

hist(datos_sqrt,
     main = "Histograma de los datos simulados",
     xlab = "Valores",
     ylab = "Frecuencia",
     col = "lightblue",
     border = "white")

# -------------------------------
# c) Transformación inversa (1/x)
# -------------------------------

datos_inv <- 1 / datos
shapiro.test(datos_inv) # p < 0.05 → no normal

# Histograma con ggplot2

# install.packages("ggplot2")  # Para gráficos
library(ggplot2)

# Convertir en dataframe
df <- data.frame(valor = datos_inv)

# Se construye un gráfico de frecuencias absolutas

ggplot(df, aes(x = valor)) +
  geom_histogram(aes(y = ..density..), bins = 20, fill = "lightblue", color = "white") +
  stat_function(fun = dnorm,
                args = list(mean = mean(df$valor), sd = sd(df$valor)), # agrega la curva normal teórica, usando 
                # la media y desviación estándar de tus datos.
                color = "red", size = 1.2) +
  labs(title = "Histograma con Curva Anormal",
       x = "Valores", y = "Densidad") +
  theme_minimal()

# -------------------------------
# Calculo de Intervalos de confianza (IC)
# -------------------------------

# Datos de muestra
datos <- c(23, 18, 21, 16, 25, 20, 22, 19, 24, 17)

# Cálculo del intervalo de confianza para la media

resultado <- t.test(datos, conf.level = 0.95)
# Calcula la media muestral, usa la distribución t de Student porque no se conoce la desviación estándar poblacional.
# Calcula el intervalo de confianza al 95%.

intervalo_confianza <- resultado$conf.int
# Extrae solo el intervalo de confianza del resultado completo del t.test.

# Imprimir el intervalo de confianza
print(intervalo_confianza)
# La media poblacional, con un 95% de confianza, está entre 18.33 y 22.66.

