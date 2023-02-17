library(readr)
library(DescTools)
library(pROC)
#dados de treinamento
treino = read.csv("~/Marcus/Lighthouse project/desafio_manutencao_preditiva_treino.csv")

#dados de teste
teste = read.csv("~/Marcus/Lighthouse project/desafio_manutencao_preditiva_teste.csv")

#resumo estatístico e gráficos das variáveis
Desc(treino, plotit = T)

#modificações aplicadas
treino$failure = ifelse(treino$failure_type == "No Failure", 0 , 1)
treino$heat = treino$process_temperature_k - treino$air_temperature_k
treino$power = round((treino$torque_nm * (treino$rotational_speed_rpm * 0.1047198)), 1)
treino$overstrain = round((treino$tool_wear_min * treino$torque_nm), 1)
treino$random = 0.001

#ajuste do modelo de regressão logística
modelo_logistico = glm(failure ~ tool_wear_min + torque_nm + rotational_speed_rpm + overstrain + heat + power + random, data = treino, family = binomial)

#sumário do modelo
summary(modelo_logistico)

#melhorias do modelo com base no primeiro teste, remoção da variável 'random'
modelo_logistico = glm(failure ~ tool_wear_min + torque_nm + rotational_speed_rpm + overstrain + heat + power, data = treino, family = binomial)

#sumário do modelo
summary(modelo_logistico)

#fazendo a predição de treino
p1 = predict(modelo_logistico, treino, type = "response")

pred1 = round(ifelse(p1 > 0.03153471, 1, 0), 0)
treino$pred_failure = pred1

#calculando o erro
treino$erro = ifelse(treino$pred_failure - treino$failure == 0, 0, 1)
treino$acerto = ifelse(treino$pred_failure - treino$failure == 0, 1, 0)
treino$positivo = ifelse(treino$pred_failure & treino$failure == 1, 1, 0)
treino$negativo = ifelse(treino$pred_failure & treino$failure == 0, 1, 0)

treino_roc = roc(treino$failure ~ p1, plot = T, print.auc = T)
coords(treino_roc,"best", ret = "threshold", transpose = F)

#fazendo a predição de teste
teste$heat = teste$process_temperature_k - teste$air_temperature_k
teste$power = round((teste$torque_nm * (teste$rotational_speed_rpm * 0.1047198)), 1)
teste$overstrain = round((teste$tool_wear_min * teste$torque_nm), 1)
p2 = predict(modelo_logistico, teste, type = "response")

pred2 = round(ifelse(p2 > 0.03153471, 1, 0))
teste$pred_failure = pred2
teste$pred_failure = ifelse(teste$pred_failure == 1, "Failure", "No Failure")

#gerando o CSV para entrega
results = data.frame(rowNumber = teste$udi, predictedValues = teste$pred_failure)
write.csv2(results, "Resultados.csv")
