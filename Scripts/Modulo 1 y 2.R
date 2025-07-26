# Fundamentos de la bioestadistica inferencial
# Prof. Astrid Liliana Vargas Sanchez
# Introducción a R para bioestadistica

# -------------------------------
# Asignación de variables
# -------------------------------
valor_a <- 101

texto <- "Hola mundo" # Se sugiere usar nombres que indiquen claramente el contenido de la variable
print(texto)

logico <- TRUE
print(logico)

# -------------------------------
# Verificar el tipo de variable
# -------------------------------
print(class(valor_a))       # "numeric"
print(class(valor_b))       # "numeric"
print(class(valor_c))       # "integer"
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

# La función sample() en R se utiliza para seleccionar elementos aleatoriamente de un conjunto. Es muy útil para 
# hacer simulaciones, muestreos y juegos de azar (dados, monedas, cartas, etc.).


# Contar cuántos valores son mayores que 4
favorables <- sum(lanzamientos > 4)

# Probabilidad empírica o experimental
probabilidad_empirica <- favorables / length(lanzamientos) # length() cuenta cuantos elementos hay en el vector 
probabilidad_empirica # Hay una probabilidad del 33,5% de que el evento ocurra.

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

# -------------------------------
# Kolmogorov-Smirnov Test (K-S): compara la distribución empírica de los datos con una distribución teórica.
# -------------------------------

# Estandarizar los datos
datos.z <- scale(datos)

# Estandarizar un dato significa convertirlo a una escala común
# Primero necesitas estandarizar los datos y luego aplicar el test con la distribución normal como referencia

# Kolmogorov-Smirnov con distribución normal
ks.test(datos.z, "pnorm") # Los datos podrían ser normales.


# -------------------------------
# Anderson-Darling Test
# -------------------------------

# install.packages("nortest")   # Solo la primera vez
library(nortest)

# Anderson-Darling test
ad.test(datos) # Los datos podrían ser normales.

# Interpretación:

# Hipótesis nula (H0): los datos siguen una distribución normal.

# Si el p-value > 0.05, no se rechaza H0 → los datos podrían ser normales.
# Si el p-value ≤ 0.05, se rechaza H0 → los datos no son normales.

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

# Valor de p: Pr(>F) = 0.003395. Las varianzas son diferentes (heterocedasticidad)

# -------------------------------
# Prueba de Bartlett (sensible a desviaciones de la normalidad)
# -------------------------------

bartlett.test(valor ~ grupo, data = datos) # Las varianzas son diferentes (heterocedasticidad)

# -------------------------------
# Prueba de Fligner-Killeen (no paramétrica, muy robusta)
# -------------------------------

fligner.test(valor ~ grupo, data = datos) # Las varianzas son diferentes (heterocedasticidad)

# Interpretación 

# Hipótesis:
# H₀ (nula): Las varianzas de los grupos son iguales (homocedasticidad).

# H₁ (alternativa): Al menos un grupo tiene varianza diferente (heterocedasticidad).

# p > 0.05	No se rechaza la hipótesis nula → las varianzas son iguales (homocedasticidad)
# p ≤ 0.05	Se rechaza la hipótesis nula → las varianzas son diferentes (heterocedasticidad)


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

# -------------------------------
# Pruebas estadisticas para calcular el valor p
# -------------------------------

# -------------------------------
# Continuos + categoricos. K = 2. Independientes. n ≥ 30
# -------------------------------

# -------------------------------
# T student
# -------------------------------

set.seed(123)

# Crear dos grupos

# Grupo 1: 40 observaciones
grupo1 <- rnorm(40, mean = 50, sd = 10)

# Grupo 2: 40 observaciones
grupo2 <- rnorm(40, mean = 55, sd = 10)

# Verificar la normalidad

shapiro.test(grupo1)  # p > 0.05 → normal
shapiro.test(grupo2)  # p > 0.05 → normal

# Verificar la Homogeneidad de varianzas (Prueba de Levene)

# Instalar si es necesario
# install.packages("car")  # Solo la primera vez
library(car)

# Crear marco de datos y factor de grupo
datos <- c(grupo1, grupo2)
grupo <- factor(c(rep("Grupo1", 40), rep("Grupo2", 40))) # Aquí creamos un vector de etiquetas que indique a qué grupo
                                                         # pertenece cada valor del vector datos.
print (grupo)

leveneTest(datos ~ grupo)  # Esta línea aplica la prueba de Levene para verificar si las varianzas de los grupos 
                           # son iguales (homocedasticidad). p > 0.05 → varianzas homogéneas

# Prueba T de Student

t.test(grupo1, grupo2, var.equal = TRUE) 

# Interpretación del resultado:
# Hipótesis nula (H₀): No hay diferencia entre las medias.
# Hipótesis alternativa (H₁): Las medias son diferentes.

# Si el p-valor < 0.05, se rechaza H₀ y se concluye que hay diferencias significativas entre los grupos.

# -------------------------------
# Test de Welch
# -------------------------------

set.seed(123)  # Para reproducibilidad

# Crear dos grupos

# Grupo 1: media 50, desviación estándar 5
grupo1 <- rnorm(40, mean = 50, sd = 5)

# Grupo 2: media 55, desviación estándar 15 (mayor varianza)
grupo2 <- rnorm(40, mean = 55, sd = 15)

# Verificar la normalidad

shapiro.test(grupo1)  # p > 0.05 → normal
shapiro.test(grupo2)  # p > 0.05 → normal

# Verificar la Homogeneidad de varianzas (Prueba de Levene)

# Instalar si es necesario
# install.packages("car")  # Solo la primera vez
library(car)

# Unir los datos
datos <- c(grupo1, grupo2)
grupo <- factor(c(rep("Grupo1", 40), rep("Grupo2", 40))) 

# Prueba de Levene
leveneTest(datos ~ grupo) # p < 0.05 → NO hay homogeneidad de varianzas → usar Test de Welch.

# Test de Welch

t.test(grupo1, grupo2, var.equal = FALSE)

# Interpretación del resultado:
# Hipótesis nula (H₀): No hay diferencia entre las medias.
# Hipótesis alternativa (H₁): Las medias son diferentes.

# Si el p-valor < 0.05, se rechaza H₀ y se concluye que hay diferencias significativas entre los grupos.

# Visualizar con un boxplot

boxplot(grupo1, grupo2,
        names = c("Grupo 1", "Grupo 2"),
        col = c("lightblue", "lightgreen"),
        main = "Comparación de grupos",
        ylab = "Valores")

# -------------------------------
# Continuos + categoricos. K = 2. Independientes. n < 30
# -------------------------------

# -------------------------------
# U de Mann-Whitney
# -------------------------------

set.seed(123)

# Crear dos grupos

# Grupo 1: 20 datos con distribución exponencial
grupo1 <- rexp(20, rate = 0.2)

# Grupo 2: 20 datos con otra distribución (por ejemplo, exponencial con diferente tasa)
grupo2 <- rexp(20, rate = 0.1)

# Verificar la normalidad

# Prueba de Shapiro-Wilk
shapiro.test(grupo1) # p < 0.05, los datos no son normales.
shapiro.test(grupo2)

# Cuando uno de los dos grupos es normal pero el otro no lo es, o los dos tienen distribuciones distintas, se 
# recomienda:
# Seguir usando la prueba no paramétrica (U de Mann-Whitney)
# ¿Por qué? Porque la prueba T asume que ambos grupos son normales, y si uno no lo es, esa suposición se rompe.

# Transformacion de datos

# Intentamos log-transformar
grupo1_log <- log(grupo1)
grupo2_log <- log(grupo2)

# Verificamos normalidad otra vez
shapiro.test(grupo1_log)
shapiro.test(grupo2_log) # p < 0.05, los datos no son normales.

# Por qué no debes transformar solo un grupo?
# Cuando haces una transformación (log, raíz, etc.), estás cambiando la escala de los datos. Si transformas solo 
# uno de los dos grupos:
  
# Estás rompiendo la comparabilidad directa entre los grupos.

# Las pruebas estadísticas (como la prueba T o U de Mann-Whitney) suponen que los datos están en la misma escala o 
# unidad.

# El objetivo de la transformación es aplicar la misma corrección a todos los datos para que las comparaciones sean 
# válidas.

# U de Mann-Whitney (Wilcoxon rank-sum test)

wilcox.test(grupo1, grupo2, alternative = "two.sided")

# Interpretación del resultado:
# Hipótesis nula (H₀): No hay diferencia entre las medias.
# Hipótesis alternativa (H₁): Las medias son diferentes.

# Si el p-valor < 0.05, se rechaza H₀ y se concluye que hay diferencias significativas entre los grupos.

boxplot(grupo1, grupo2,
        names = c("Grupo 1", "Grupo 2"),
        col = c("tomato", "lightblue"),
        main = "Comparación con Mann-Whitney",
        ylab = "Valores")


# -------------------------------
# Continuos + categoricos. K = 2. Emparejados. n ≥ 30
# -------------------------------
# -------------------------------
# T student
# -------------------------------

# Simulamos datos: medición antes y después de un tratamiento
# Comparar presión arterial antes y después de un tratamiento en los mismos pacientes.

set.seed(123)
n <- 35  # mayor a 30
antes <- rnorm(n, mean = 100, sd = 10)
despues <- antes + rnorm(n, mean = -5, sd = 8)  # se espera una disminución

# Visualizamos resumen
summary(antes)
summary(despues)

# Verificamos normalidad de las diferencias
diferencias <- antes - despues
shapiro.test(diferencias)  # p > 0.05 → normalidad aceptable

# Aplicamos T de Student pareada
t.test(antes, despues, paired = TRUE) # menor a 0.05, Si hay diferencia significativa.


# También podemos visualizar
boxplot(antes, despues, names = c("Antes", "Después"),
        main = "Comparación de medidas pareadas",
        col = c("skyblue", "lightgreen"))

# Interpretacion:

# H₀ (nula): La mediana de las diferencias entre los pares es cero.(Es decir, no hay cambio o efecto).
# H₁ (alternativa): La mediana de las diferencias es distinta de cero.

# p > 0.05 → No se rechaza H₀ → No hay diferencia significativa.
# p ≤ 0.05 → Se rechaza H₀ → Sí hay diferencia significativa entre los grupos pareados.

# -------------------------------
# Continuos + categoricos. K = 2. Emparejados. n < 30
# -------------------------------

# -------------------------------
# Prueba de Wilcoxon
# -------------------------------

# Simulamos datos: medición antes y después de un tratamiento
# Comparar presión arterial antes y después de un tratamiento en los mismos pacientes.

# Cargar librería para pruebas de normalidad
library(car)

# Simulamos datos con distribución sesgada y ruidosa
set.seed(123)
antes <- rexp(20, rate = 0.2)      # Distribución exponencial (muy asimétrica)
despues <- antes + rnorm(20, mean = 2, sd = 3)  # Añadimos ruido

# Calculamos las diferencias
diferencias <- antes - despues

# Verificamos normalidad de las diferencias
shapiro.test(diferencias)  # p < 0.05 → No normalidad

# Transformación de datos
# Logarítmica (agregamos una constante porque puede haber valores negativos)

log_dif <- log(diferencias - min(diferencias) + 1)
# En este ejemplo usamos - min(diferencias) + 1 para "levantar" los datos y evitar logaritmos de 0 o números negativos.

shapiro.test(log_dif) # p < 0.05 → No normalidad

# Prueba de Wilcoxon
wilcox.test(antes, despues, paired = TRUE) # mayor a 0.05, no hay diferencia significativa.


# Visualización
boxplot(antes, despues, names = c("Antes", "Después"),
        col = c("skyblue", "lightgreen"),
        main = "Comparación de medidas pareadas - Wilcoxon")

# Interpretacion:

# H₀ (nula): La mediana de las diferencias entre los pares es cero.(Es decir, no hay cambio o efecto).
# H₁ (alternativa): La mediana de las diferencias es distinta de cero.

# p > 0.05 → No se rechaza H₀ → No hay diferencia significativa.
# p ≤ 0.05 → Se rechaza H₀ → Sí hay diferencia significativa entre los grupos pareados.

# -------------------------------
# Continuos + categoricos. K > 2. 1 variable de agrupación. Independientes. Parametricos
# -------------------------------

# -------------------------------
# ANOVA de un factor
# -------------------------------

# Instala paquetes si no los tienes
# install.packages("car")
# install.packages("ggplot2")

library(car)
library(ggplot2)

# Simular datos para 3 grupos con características diferentes
set.seed(123)
grupo1 <- rnorm(30, mean = 50, sd = 5)
grupo2 <- rnorm(30, mean = 55, sd = 5)
grupo3 <- rnorm(30, mean = 60, sd = 5)

# Unir datos
datos <- c(grupo1, grupo2, grupo3)
grupo <- factor(rep(c("Grupo1", "Grupo2", "Grupo3"), each = 30))

# Crear data frame
df <- data.frame(valor = datos, grupo = grupo)

# Verificar normalidad por grupo
by(df$valor, df$grupo, shapiro.test) # Normalidad

# Verificar homogeneidad de varianzas
leveneTest(valor ~ grupo, data = df) # p>0.05 → Homogeneidad

# ANOVA de un factor
modelo_anova <- aov(valor ~ grupo, data = df)
summary(modelo_anova)

# Si el p-value del resumen ANOVA es < 0.05, hay diferencias significativas entre grupos.

# Si ANOVA es significativa, hacer prueba post-hoc
TukeyHSD(modelo_anova)

# Si p < 0.05, hay diferencia significativa entre esos grupos.

# Visualizar los resultados
ggplot(df, aes(x = grupo, y = valor, fill = grupo)) +
  geom_boxplot() +
  labs(title = "Comparación de grupos - ANOVA", y = "Valor") +
  theme_minimal()

# Otro ejemplo

# Supongamos que medimos el nivel de glucosa en 3 grupos con diferentes tipos de dieta. Con datos normales.

set.seed(123)

# Simulación de datos para tres dietas
dietaA <- rnorm(30, mean = 100, sd = 10)
dietaB <- rnorm(30, mean = 110, sd = 10)
dietaC <- rnorm(30, mean = 120, sd = 10)

# Unimos todos los datos
glucosa <- c(dietaA, dietaB, dietaC)
dieta <- factor(rep(c("DietaA", "DietaB", "DietaC"), each = 30))

# Creamos el data frame
datos <- data.frame(dieta, glucosa)

boxplot(glucosa ~ dieta,
        data = datos,
        col = c("lightblue", "lightgreen", "lightpink"),
        main = "Boxplot de niveles de glucosa por tipo de dieta",
        xlab = "Tipo de dieta",
        ylab = "Nivel de glucosa (mg/dL)")

# -------------------------------
# Continuos + categoricos. K > 2. 1 variable de agrupación. Independientes. No parametricos
# -------------------------------

# -------------------------------
#  Prueba de Kruskal-Wallis
# -------------------------------

# Supongamos que medimos el nivel de glucosa en 3 grupos con diferentes tipos de dieta. Pero esta vez los datos 
# no son normales.

set.seed(123)

# Creamos 3 grupos con distribución no normal (usamos la distribución gamma)
grupo1 <- rgamma(20, shape = 2, scale = 10)
grupo2 <- rgamma(20, shape = 3, scale = 10)
grupo3 <- rgamma(20, shape = 5, scale = 10)

# Unimos los datos
glucosa <- c(grupo1, grupo2, grupo3)
dieta <- factor(rep(c("Dieta1", "Dieta2", "Dieta3"), each = 20))

# Data frame
datos <- data.frame(dieta, glucosa)

# Normalidad

shapiro.test(grupo1) 
shapiro.test(grupo2) 
shapiro.test(grupo3) # p < 0.05, los datos no siguen distribución normal

# Prueba de Kruskal-Wallis

kruskal.test(glucosa ~ dieta, data = datos)

# Interpretación del resultado

# Si el p-value < 0.05, hay diferencias estadísticamente significativas en las medianas de al menos un grupo.
# Esta prueba no te dice entre qué grupos están las diferencias.

# Comparaciones post-hoc si p < 0.05 (Opcional)

# Comparación múltiple post-hoc de Dunn con corrección de Bonferroni

# install.packages("FSA")  # Solo si no lo tienes
library(FSA)

dunnTest(glucosa ~ dieta, data = datos, method = "bonferroni")

# Dieta1 vs Dieta2 → P.adj = 0.577 No hay diferencia significativa.

# Dieta1 vs Dieta3 → P.adj = 0.000004 Sí hay diferencia estadísticamente significativa.

# Dieta2 vs Dieta3 → P.adj = 0.00124 Sí hay diferencia estadísticamente significativa.

# Visualización con boxplot

boxplot(glucosa ~ dieta,
        data = datos,
        col = c("lightblue", "lightgreen", "lightpink"),
        main = "Niveles de glucosa por tipo de dieta",
        xlab = "Tipo de dieta",
        ylab = "Glucosa (mg/dL)")

# -------------------------------
# Continuos + categoricos. K > 2. 1 variable de agrupación. Emparejados. Parametricos
# -------------------------------
# -------------------------------
#  Prueba de ANOVA de medidas repetidas
# -------------------------------

# Simulamos datos: medición antes, durante y después de un tratamiento
# Comparar presión arterial antes, durante y después de un tratamiento en los mismos pacientes.

# Paquetes necesarios
# install.packages("tidyverse")
# install.packages("ez")
library(tidyverse)
library(ez)

# Simulación de datos
set.seed(123)
id <- rep(1:30, each = 3)  # 30 sujetos, 3 mediciones cada uno
momento <- rep(c("Antes", "Durante", "Despues"), times = 30)
valor <- c(rnorm(30, mean = 50, sd = 5),
           rnorm(30, mean = 55, sd = 5),
           rnorm(30, mean = 60, sd = 5))

datos <- data.frame(id = factor(id),
                    momento = factor(momento, levels = c("Antes", "Durante", "Despues")),
                    valor = valor)
head(datos)

# ANOVA de medidas repetidas
anova_result <- ezANOVA(data = datos,
                        dv = valor,
                        wid = id,
                        within = momento,
                        detailed = TRUE)
anova_result

# p = 0.3455291. Este es el valor crudo, sin aplicar correcciones por violación de la esfericidad.
# p = 0.1160841. Como p > 0.05, no se viola el supuesto de esfericidad, por tanto podemos usar el valor p 
# original del ANOVA sin correcciones.

# La esfericidad implica que las varianzas de las diferencias entre todos los pares de condiciones (por ejemplo, antes
# vs durante, antes vs después, durante vs después) sean iguales.
# Es una extensión del supuesto de homogeneidad de varianzas, pero para datos emparejados/repetidos.


# Si el p-value < 0.05, hay diferencias significativas entre al menos dos momentos.

# Visualización

library(ggplot2)

ggplot(datos, aes(x = momento, y = valor, group = id)) +
  geom_line(aes(color = id), alpha = 0.4) +
  geom_point() +
  stat_summary(fun = mean, geom = "line", group = 1, color = "black", size = 1.2) +
  stat_summary(fun = mean, geom = "point", size = 3, color = "red") +
  theme_minimal() +
  labs(title = "Medidas repetidas por sujeto",
       y = "Valor", x = "Momento")

# El ANOVA mide diferencias en promedios, no casos individuales.
# Puede pasar que: Algunas personas tengan grandes cambios, pero en otras, los valores no cambian o incluso cambian 
# en dirección contraria, y en conjunto, la media del grupo no cambia de forma significativa.

# -------------------------------
# Continuos + categoricos. K > 2. 1 variable de agrupación. Emparejados. No parametricos
# -------------------------------
# -------------------------------
#  Test de Friedman
# -------------------------------

# Simulamos datos: medición antes, durante y después de un tratamiento
# Comparar presión arterial antes, durante y después de un tratamiento en los mismos pacientes.
# No se cumple el supuesto de normalidad (no paramétrico).

# Simulación de datos
set.seed(123)  # Reproducibilidad
paciente <- factor(1:10) # 10 categorias

antes <- c(130, 125, 140, 135, 132, 138, 129, 137, 134, 136)
durante <- c(128, 124, 138, 133, 130, 135, 128, 135, 133, 134)
despues <- c(127, 123, 137, 132, 129, 134, 127, 133, 132, 133)

# Creamos un dataframe en formato "wide"
datos <- data.frame(paciente, antes, durante, despues)

# Convertimos a formato "long" para usar con friedman.test
library(tidyr)
datos_long <- pivot_longer(datos, cols = -paciente,
                           names_to = "momento", values_to = "valor")

# Convertimos a factores
datos_long$momento <- factor(datos_long$momento, levels = c("antes", "durante", "despues"))

# Prueba de Friedman
friedman.test(valor ~ momento | paciente, data = datos_long)

# Si el p-value < 0.05, hay diferencias significativas en las mediciones entre los distintos momentos.

# Boxplot por momento
ggplot(datos_long, aes(x = momento, y = valor, fill = momento)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Presión arterial en diferentes momentos",
       x = "Momento",
       y = "Presión arterial (mmHg)") +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "Pastel1")

# -------------------------------
# Continuos + categoricos. K > 2. Mas de 1 variable de agrupación. Independientes. Parametricos o No parametricos
# -------------------------------

# -------------------------------
# ANOVA factorial 
# -------------------------------

# Ejemplo: Efecto de la dieta y el sexo sobre el peso corporal
# Variables de agrupación (factores):
# dieta (3 niveles: A, B, C)
# sexo (2 niveles: masculino, femenino)
# Variable independiente (dependiente en el análisis): peso (peso corporal en kg)

set.seed(123)

# Crear factores
dieta <- factor(rep(c("A", "B", "C"), each = 20))
sexo <- factor(rep(c("Masculino", "Femenino"), times = 30))

# Simular pesos con ligeras diferencias por grupo
peso <- rnorm(60, mean = 70, sd = 5) +
  ifelse(dieta == "B", 5, 0) +
  ifelse(dieta == "C", -3, 0) +
  ifelse(sexo == "Femenino", -2, 0)

# Crear el dataframe
datos <- data.frame(dieta, sexo, peso)

# Anova factorial

modelo <- aov(peso ~ dieta * sexo, data = datos)
summary(modelo)

# Interpretar cada fila:
  
# dieta: si p < 0.05 → hay diferencia según dieta.
# sexo: si p < 0.05 → hay diferencia de medias según sexo.
# dieta:sexo: si p < 0.05 → hay interacción entre sexo y dieta (el efecto de la dieta depende del sexo).


# Prueba post-hoc (opcional si algún factor es significativo)

TukeyHSD(modelo)

# Visualización con ggplot2

library(ggplot2)

ggplot(datos, aes(x = dieta, y = peso, fill = sexo)) +
  geom_boxplot() +
  labs(title = "ANOVA factorial: efecto de dieta y sexo en el peso",
       x = "Tipo de dieta", y = "Peso (kg)") +
  theme_minimal()

# -------------------------------
# Categoricos + categoricos. 
# -------------------------------

# -------------------------------
# Prueba de Chi-cuadrado (χ²) 
# -------------------------------

# Supón que tienes tres variables: Dieta: "A", "B", "C"
# Resultado: "Mejoró" o "No mejoró"

# Creamos una tabla de contingencia
tabla <- matrix(c(30, 10, 
                  20, 20,
                  10, 30),
                nrow = 3, byrow = TRUE)

colnames(tabla) <- c("Mejoró", "NoMejoró")
rownames(tabla) <- c("DietaA", "DietaB", "DietaC")

tabla <- as.table(tabla)
tabla

# Ejemplo de interpretacion de la tabla:
# 30: Número de personas que siguieron la Dieta A y mejoraron.
# 10: personas que siguieron la Dieta A y no mejoraron.

# Prueba de Chi-cuadrado
chisq.test(tabla)

# Interpretación
# p < 0.05 → Hay asociación significativa entre tipo de dieta y mejoría.

# p ≥ 0.05 → No hay evidencia de asociación (variables independientes).

# -------------------------------
# Prueba de Prueba exacta de Fisher 
# -------------------------------

# Supón que tienes dos variables: Tratamiento: "A" o "B"
# Resultado: "Mejoró" o "No mejoró"

# Crear la tabla de contingencia
tabla <- matrix(c(8, 2,    # Tratamiento A: mejoró, no mejoró
                  1, 9),   # Tratamiento B: mejoró, no mejoró
                nrow = 2, byrow = TRUE)

# Asignar nombres a filas y columnas
dimnames(tabla) <- list(
  Tratamiento = c("A", "B"),
  Resultado = c("Mejoró", "No mejoró")
)

# Ver la tabla
tabla

# Aplicar prueba exacta de Fisher
fisher.test(tabla)

# Interpretación:

# Si p < 0.05: hay evidencia de asociación significativa entre tratamiento y resultado.
# Si p ≥ 0.05: no se puede rechazar la hipótesis nula de independencia.

# Visualización

# Paquete necesario
library(ggplot2)

# Crear los datos como data.frame para graficar
datos <- data.frame(
  Tratamiento = rep(c("A", "B"), each = 2),
  Resultado = rep(c("Mejoró", "No mejoró"), 2),
  Frecuencia = c(8, 2, 1, 9)
)

# Verificar estructura
print(datos)

# Gráfico de barras apilado
ggplot(datos, aes(x = Tratamiento, y = Frecuencia, fill = Resultado)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Resultado por Tratamiento",
       x = "Tratamiento",
       y = "Frecuencia") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")

# Las personas que tomaron el tratamiento A presentaron una mejoria

# -------------------------------
# Prueba de McNemar 
# -------------------------------

# Ejemplo: Tenemos los resultados de 70 pacientes antes y después del tratamiento, con una prueba que indica si 
# la infección está presente (Positivo) o ausente (Negativo).

# Matriz de datos emparejados
#        Después
#        Pos  Neg
# Antes
# Pos     30   10
# Neg      5   25

matriz <- matrix(c(30, 10,
                   5, 25),
                 nrow = 2,
                 byrow = TRUE,
                 dimnames = list("Antes" = c("Positivo", "Negativo"),
                                 "Después" = c("Positivo", "Negativo")))

print(matriz)

# Interpretación:

# Antes Positivo → Después Negativo (10 personas):
# Estas personas mejoraron con el tratamiento (la infección desapareció).
# Interpretamos esto como que el tratamiento funcionó.

# Antes Negativo → Después Positivo (5 personas):
# Estas personas estaban bien, pero desarrollaron la infección después.
# Esto sugiere que el tratamiento no fue efectivo o hubo reinfección.

# Antes Positivo → Después Positivo (30 personas):
# El tratamiento no funcionó para estas personas.

# Antes Negativo → Después Negativo (25 personas):
# Estas personas no estaban enfermas antes ni después.

# Prueba de McNemar
resultado <- mcnemar.test(matriz)
print(resultado)

# ¿Cómo se interpreta?
# Hipótesis nula (H₀): No hay diferencia entre las proporciones de cambio (los discordantes).

# p > 0.05: No hay evidencia significativa de cambio.
# p < 0.05: Hay una diferencia significativa en los emparejamientos (por ejemplo, el tratamiento tuvo un efecto).

# Gráfico de barras

# Instalar si no tienes ggplot2
# install.packages("ggplot2")

library(ggplot2)

# Datos reorganizados para el gráfico
datos <- data.frame(
  Antes = rep(c("Positivo", "Negativo"), each = 2),
  Después = rep(c("Positivo", "Negativo"), times = 2),
  Frecuencia = c(30, 10, 5, 25)
)

# Gráfico de barras apilado
ggplot(datos, aes(x = Antes, y = Frecuencia, fill = Después)) +
  geom_bar(stat = "identity") +
  labs(title = "Cambios antes y después del tratamiento",
       x = "Estado Antes",
       y = "Frecuencia",
       fill = "Estado Después") +
  theme_minimal()
