library(tidyverse)
library(randomForest)

lung_cancer <- read_csv("lung_cancer.csv")

head(lung_cancer)
str(lung_cancer)

#### Individuos con y sin cáncer de pulmón

p_cancer <- ggplot(lung_cancer, aes(x = LUNG_CANCER)) +
  geom_bar() +
  labs(
    title = "Individuos con y sin cáncer de pulmón",
    x = "Cáncer de pulmón",
    y = "Número de individuos"
  ) +
  theme_minimal()

#### edad segun cancer de pulmón 
p_edad <- ggplot(lung_cancer, aes(x = LUNG_CANCER, y = AGE)) +
  geom_boxplot() +
  labs(
    title = "Edad según cáncer de pulmón",
    x = "Cáncer de pulmón",
    y = "Edad"
  ) +
  theme_minimal()


##### Variables binarias según cáncer de pulmón

lung_long <- lung_cancer %>%
  pivot_longer(
    cols = -c(GENDER, AGE, LUNG_CANCER),
    names_to = "Variable",
    values_to = "Respuesta"
  )

p_variables <- ggplot(lung_long, aes(x = factor(Respuesta), fill = LUNG_CANCER)) +
  geom_bar(position = "fill") +
  facet_wrap(~Variable, ncol = 3) +
  labs(
    title = "Características de los individuos según cáncer de pulmón",
    x = "Respuesta (1 = No, 2 = Sí)",
    y = "Proporción",
    fill = "Cáncer de pulmón"
  ) +
  theme_minimal()


###### Comparación estadística
#Edad
wilcox.test(AGE ~ LUNG_CANCER, data = lung_cancer)
#Estadísticos descriptivos
lung_cancer %>%
  group_by(LUNG_CANCER) %>%
  summarise(
    n = n(),
    media = mean(AGE),
    mediana = median(AGE),
    desviacion = sd(AGE)
  )
#Variables Categóricas
variables <- setdiff(
  names(lung_cancer),
  c("GENDER", "AGE", "LUNG_CANCER")
)

resultados <- data.frame(
  Variable = variables,
  p_value = NA
)

for (i in 1:length(variables)) {
  
  tabla <- table(
    lung_cancer[[variables[i]]],
    lung_cancer$LUNG_CANCER
  )
  
  resultados$p_value[i] <- chisq.test(tabla)$p.value
}

resultados

## Signifcativas
resultados %>%
  mutate(
    Significativo = ifelse(p_value < 0.05, "Sí", "No")
  )

####### Modelo predictivo Random Forest
#Separar entrenamiento y test
set.seed(123)

indice <- sample(
  1:nrow(lung_cancer),
  size = 0.7 * nrow(lung_cancer)
)

train <- lung_cancer[indice, ]
test <- lung_cancer[-indice, ]

# Variable respuesta
y_train <- factor(train$LUNG_CANCER)

# Variables predictoras
x_train <- train[, setdiff(names(train), "LUNG_CANCER")]

# Crear Random Forest
modelo <- randomForest(
  x = x_train,
  y = y_train,
  ntree = 500,
  importance = TRUE
)
modelo

#Predicciones
predicciones <- predict(
  modelo,
  newdata = test[, setdiff(names(test), "LUNG_CANCER")]
)


#Matriz de confusión
matriz_confusion <- table(
  Real = test$LUNG_CANCER,
  Predicho = predicciones
)

#Accuracy
accuracy <- mean(predicciones == test$LUNG_CANCER)
accuracy

#Importancia de las variables

importancia <- as.data.frame(importance(modelo))
importancia$Variable <- rownames(importancia)
rownames(importancia) <- NULL


##### Conclusión #####


# El análisis mostró una asociación estadísticamente significativa
# entre el cáncer de pulmón y las variables YELLOW_FINGERS, ANXIETY,
# PEER_PRESSURE, FATIGUE, ALLERGY, WHEEZING, ALCOHOL CONSUMING,
# COUGHING, SWALLOWING DIFFICULTY y CHEST PAIN. La edad no mostró
# diferencias significativas entre los grupos (p = 0.1823).

# El modelo Random Forest obtuvo una accuracy del 90.32% en el
# conjunto de test. Las variables más importantes fueron
# YELLOW_FINGERS y ALCOHOL CONSUMING según MeanDecreaseAccuracy,
# mientras que AGE fue la más importante según MeanDecreaseGini.






















