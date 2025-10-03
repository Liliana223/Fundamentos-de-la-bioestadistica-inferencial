# Fundamentos de la bioestadistica inferencial
# Prof. Astrid Liliana Vargas Sanchez
# Introducción a R para bioestadistica
# Modulo 2

# -------------------------------
# Pruebas estadisticas para calcular el valor p
# -------------------------------

# -------------------------------
# Continuos + Continuos
# -------------------------------

# -------------------------------
# Correlación de Pearson
# -------------------------------

# Datos simulados
altura <- c(160, 165, 170, 175, 180, 185, 190)
peso <- c(55, 60, 65, 70, 75, 78, 82)

# Correlación de Pearson
cor.test(altura, peso, method = "pearson")

# Si p < 0.05, se rechaza H₀: hay una correlación significativa.

# -------------------------------
# Correlación de Spearman
# -------------------------------

# Correlación de Spearman
cor.test(altura, peso, method = "spearman")

# Si p < 0.05, se rechaza H₀: hay una correlación significativa.

# -------------------------------
# Continuos + categoricos. K = 2. Independientes. n ≥ 30
# Comparamos dos grupos
# -------------------------------

# -------------------------------
# T student
# -------------------------------

set.seed(123)

# Cargar paquete
library(car)  # Para la prueba de Levene

set.seed(123)  # Reproducibilidad

# Simulación de datos
colesterol_dieta_grasas <- rnorm(40, mean = 220, sd = 25)   # Grupo 1
colesterol_dieta_balanceada <- rnorm(40, mean = 200, sd = 25) # Grupo 2

# Unimos en un dataframe
colesterol <- c(colesterol_dieta_grasas, colesterol_dieta_balanceada)
dieta <- factor(c(rep("AltaGrasas", 40), rep("Balanceada", 40))) # Crear la variable categórica “dieta”
datos <- data.frame(dieta, colesterol)

# --- Paso 1: Verificar homogeneidad de varianzas
leveneTest(colesterol ~ dieta, data = datos)  # p > 0.05 → varianzas homogéneas

# --- Paso 2: T de Student (independientes, varianzas homogéneas)
t.test(colesterol ~ dieta, data = datos, var.equal = TRUE)

#Interpretación esperada:

# Prueba de Levene

# Si p > 0.05 → se cumple la homogeneidad de varianzas (supuesto de la t de Student clásica).

#S i p < 0.05 → habría que usar Welch en lugar de la t de Student estándar.

# t.test()

# Hipótesis nula (H₀): No hay diferencia en los niveles de colesterol entre dietas.

# Hipótesis alternativa (H₁): Sí hay diferencia.

# Si p < 0.05 → concluimos que la dieta influye en los niveles de colesterol.

# -------------------------------
# Test de Welch
# -------------------------------

# Simulamos los datos
set.seed(123)

# Variable de agrupación
dieta <- factor(c(rep("AltaGrasas", 40), rep("Balanceada", 40)))

# Variable dependiente (colesterol)
colesterol <- c(
  rnorm(40, mean = 220, sd = 30),   # Grupo AltaGrasas
  rnorm(40, mean = 200, sd = 15)    # Grupo Balanceada (menor varianza)
)

# Creamos el data frame
datos <- data.frame(dieta, colesterol)

# Ver primeras filas
head(datos)

# Test de Welch (sin asumir igualdad de varianzas)
t.test(colesterol ~ dieta, data = datos, var.equal = FALSE)

# Interpretación del Test de Welch

# Hipótesis nula (H₀): el colesterol promedio es igual en ambos grupos.

# Hipótesis alternativa (H₁): el colesterol promedio es diferente entre los grupos.

# El resultado de t.test() te dará:

# Si p < 0.05, se rechaza H₀ → hay evidencia de diferencia significativa en colesterol entre las dietas, aun cuando 
# las varianzas son distintas.


# Visualizar con un boxplot

# Boxplot comparando colesterol según dieta
boxplot(colesterol ~ dieta, data = datos,
        main = "Niveles de colesterol según tipo de dieta",
        xlab = "Tipo de dieta",
        ylab = "Colesterol (mg/dL)",
        col = c("tomato", "skyblue"),
        border = "black")

# Añadir línea horizontal con el promedio general
abline(h = mean(datos$colesterol), col = "darkgreen", lty = 2)

# Interpretación del gráfico

# Cada caja representa la distribución del colesterol en el grupo correspondiente.

# La línea negra dentro de cada caja es la mediana.

# Los bigotes muestran la variabilidad (rango intercuartílico y posibles valores extremos).

# Puedes ver si un grupo tiene valores más altos o más dispersos.

# En este caso, esperarías que el grupo con dieta Alta en Grasas tenga en promedio un colesterol mayor y con más variabilidad que el grupo Balanceada.

# -------------------------------
# Continuos + categoricos. K = 2. Independientes. n < 30
# Comparamos dos grupos
# -------------------------------

# -------------------------------
# U de Mann-Whitney
# -------------------------------

set.seed(123)

# Crear dos grupos

# Grupo 1: 20 datos con distribución exponencial
grupo1 <- rexp(20, rate = 0.2) # Dieta alta en azúcares

# Grupo 2: 20 datos con otra distribución (por ejemplo, exponencial con diferente tasa)
grupo2 <- rexp(20, rate = 0.1) # Dieta balanceada, más dispersa

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

# Creamos data frame
datos <- data.frame(
  glucosa = c(grupo1, grupo2),
  dieta = factor(c(rep("AltaAzucares", 20), rep("Balanceada", 20)))
)


# U de Mann-Whitney (Wilcoxon rank-sum test)

wilcox.test(glucosa ~ dieta, data = datos, exact = FALSE)

# Interpretación del resultado:
# Hipótesis nula (H₀):La distribución (mediana) de los niveles de glucosa es la misma en ambos grupos.
# Hipótesis alternativa (H₁):La distribución (mediana) de los niveles de glucosa es diferente en al menos uno de 
# los grupos.

# Si el p-valor < 0.05, se rechaza H₀ y se concluye que hay diferencias significativas entre los grupos.
# La dieta sí afecta los niveles de glucosa.

# Gráfico boxplot
ggplot(datos, aes(x = dieta, y = glucosa, fill = dieta)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Glucosa en sangre según tipo de dieta",
       x = "Tipo de dieta", y = "Glucosa (mg/dL)") +
  scale_fill_manual(values = c("tomato", "skyblue")) +
  theme_minimal()


# -------------------------------
# Continuos + categoricos. K = 2. Emparejados. n ≥ 30
# Comparamos dos grupos
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
# Comparamos dos grupos
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
# Comparamos mas de dos grupos
# -------------------------------

# -------------------------------
# ANOVA de un factor
# -------------------------------

# Instala paquetes si no los tienes
# install.packages("car")
# install.packages("ggplot2")

library(car)
library(ggplot2)

# Supongamos que medimos el nivel de colesterol en 3 grupos con diferentes tipos de dieta. Con datos normales.

set.seed(123)

# Variable de agrupación (Independiente): tipo de dieta. (Alta en grasas, Balanceada, Vegetariana).
dieta <- factor(rep(c("AltaGrasas", "Balanceada", "Vegetariana"), each = 20))

# Variable dependiente: niveles de colesterol (mg/dL)
colesterol <- c(rnorm(20, mean = 220, sd = 15),   # Alta en grasas
                rnorm(20, mean = 200, sd = 15),   # Balanceada
                rnorm(20, mean = 180, sd = 15))   # Vegetariana

# Crear dataframe
datos <- data.frame(dieta, colesterol)

# 1. Verificar normalidad dentro de cada grupo
by(datos$colesterol, datos$dieta, shapiro.test)

# 2. Verificar homogeneidad de varianzas
bartlett.test(colesterol ~ dieta, data = datos)

# 3. ANOVA de un factor
anova_model <- aov(colesterol ~ dieta, data = datos)
summary(anova_model)

# 4. Prueba post-hoc de Tukey
TukeyHSD(anova_model)

# 5. Visualización con Boxplot
boxplot(colesterol ~ dieta, data = datos,
        main = "Colesterol según tipo de dieta",
        xlab = "Tipo de dieta",
        ylab = "Colesterol (mg/dL)",
        col = c("tomato", "skyblue", "lightgreen"))

# Interpretación: Si el ANOVA resulta significativo (p < 0.05), se interpreta que al menos un grupo difiere.

# El TukeyHSD luego indica qué pares de grupos son diferentes.


# -------------------------------
# Continuos + categoricos. K > 2. 1 variable de agrupación. Independientes. No parametricos
# Comparamos mas de dos grupos
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
# Variables de agrupación (factores) (Independientes):
# dieta (3 niveles: A, B, C)
# sexo (2 niveles: masculino, femenino)
# Variable dependiente en el análisis: peso (peso corporal en kg)

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
