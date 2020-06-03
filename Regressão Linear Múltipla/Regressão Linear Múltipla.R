# --------------------------------------------
# Regressão Linear Simples
# Elaborado por: Pablo Chang (05/05/2020)
# https://github.com/PabloChang/R
# --------------------------------------------
# O arquivo de dados e script devem estar numa mesma pasta; 
# Os dados devem ser salvos em ".csv (separado por vírgulas)";
# Não pode haver espaço e acentuação nos títulos, única guia;
# Os tratametos/fatores devem ser numéricos;
# Para rodar os comandos, use Ctrl + Enter em cada linha;
# Ativar/desativar comentários: Ctrl + Shift + C.

# --------------------------------------------
# 1) LEITURA E PREPARAÇÃO DOS DADOS
# --------------------------------------------
# Comando para definir a localização da pasta.
# Caso não tenho instalado o pacote é só rodar: 
# install.packages("rstudioapi")
  library(rstudioapi)
  current_path = 
    rstudioapi::getActiveDocumentContext()$path 
  setwd(dirname(current_path ))
  print( getwd() )

# Troque o nome do arquivo de dados (entre " .csv"):
  dados <- read.csv2(
    "DadosExemploRLM.csv", 
  header = T)

# Troca os pontos dos gráficos por vírgula:
  options(OutDec=",")

# Mostra as 6 primeiras linhas para visualização.
# As colunas devem seguir a ordem: tratamento // resposta;
  head(dados)
  
# Filtrar dados por categorias, quando existir:
# Exemplo: mostrar somente dados da Profundidade 2.
# (Para usar, exclua o "#" abaixo e modifique)
# dados <- filter(dados, Profundidade==2)

# Troque os nomes das colunas (entre "c(  )"):
# X1 = c(  ): para a primeira variável independente;
# X2 = c(  ): para a segunda variável independente;
# X3 ...
# Y = c(  ): para variável resposta Y a ser analisada.
  attach(dados) 
  dados <- data.frame(X1 = c(cyl),
                      X2 = c(disp),
                      X3 = c(hp),
                      X4 = c(wt),
                      Y = c(mpg)
  )

# Mostra as 6 primeiras linhas para ver como ficou.
head(dados)

# Deletar valores condicionais
  # RESP < x: deletar valores abaixo de x.
  # RESP > x: deletar valores acima de x.
  # RESP == x: deletar valores iguais a x.
  # Para usar, exclua o "#" abaixo e modifique:
# r <- with(dados, which(RESP<0, arr.ind=TRUE))
#  dados <- dados[-r, ]
# r <- with(dados, which(RESP==0, arr.ind=TRUE))
#  dados <- dados[-r, ]

# Anexar os dados na memória do R:
attach(dados) 


# --------------------------------------------
# 2) RESUMO DESCRITIVO 
# --------------------------------------------
# Pacote "agricolae".
# Caso não tenho instalado é só rodar: 
# install.packages("agricolae")
library(agricolae)

# Cálculo dos Quartis
quartil1 <- quantile(Y, 0.25)
quartil2 <- quantile(Y, 0.5)
quartil3 <- quantile(Y, 0.75)

# Resumo descritivo
resumo <- data.frame(Estatística = c(
  "Mínimo", 
  "1º Quartil",
  "Mediana",
  "Média",
  "3º Quartil",
  "Máximo",
  "Variância",
  "Desvio Padrão",
  "Coeficiente de Variação (%)",
  "Assimetria",
  "Curtose",
  "Tamanho da Amostra" ),
  Resultado = c(
    min(Y), 
    quartil1[[1]], 
    quartil2[[1]],
    mean(Y),
    quartil3[[1]],
    max(Y),
    var(Y),
    sd(Y),
    100*sd(Y)/mean(Y),
    skewness(Y),
    kurtosis(Y),
    length(Y)
  ),
  stringsAsFactors = FALSE)
resumo

# Exportar o Resumo Descritivo para Excel:
write.csv2(resumo, 
           file = "RLM - Resumo Descritivo.csv") 


# --------------------------------------------
# 3) BOXPLOT 
# --------------------------------------------
# Boxplot geral, com a cruz representando a média.
# Altere as cores e legendas entre aspas " ".
require(graphics)
boxplot(Y,
        col="yellow",
        outcol="blue",
        ylab="Y")
points(mean(Y),col="red",pch=3)


# --------------------------------------------
# 4) Relação entre as variáveis
# --------------------------------------------
# Matriz de relação entre as variáveis com pontos:
pairs(dados, col = 2, pch = 19)

# Com cores e dendograma: 
# Mostra a relação de proximidade:
heatmap(abs(cor(dados)))

# Mostra correlação linear de Pearson:
# Quanto mais próximo de +1, correlação positiva;
# Quanto mais próximo de -1, correlação negativa.
require(GGally)
ggcorr(dados, label=T)
# Config: https://rpubs.com/melinatarituba/353262


# --------------------------------------------
# 5) TESTE DE NORMALIDADE
# --------------------------------------------
# Pacote com alguns testes:
library(nortest)
library(ExpDes)

# Cálculo dos resíduos:
mod = lm(Y ~ X1+X2+X3+X4)

# Gráficos de resíduos:
# Para ser normal, o histograma deve ter formato de sino no centro.
# Se o gráfico Normal Q-Q se assemelhar a uma reta crescente,
# então existe normalidade.
plotres(mod)

# Testes:
# Se p-value > 0,05, os resíduos possuem distribuição normal.
shapiro.test(mod$res) # Shapiro-Wilk
ad.test(mod$res) # Anderson-Darling

# Os dados são normais?
# Se SIM, rode o comando abaixo e pule para a etapa 6).
  Y.TR <- Y
# Se NÃO, faça a transformação em 5.1).

# A falta de normalidade não introduz problema 
# na estimação dos parâmetros, mas sim,
# nos desvios padrões, afetando a validade dos
# intervalos de confiança e dos testes de 
# hipótese.


# --------------------------------------------
# 5.1) TRANSFORMAÇÃO DE DADOS NÃO-NORMAIS
# --------------------------------------------
# Faça os testes, até atingir a normalidade!

# 1. Raiz quadrada:
  TR1 <- (Y)^2
  modTR1 = lm(TR1 ~ X1+X2+X3+X4)
  # Gráfico dos resíduos
  plotres(modTR1)
  # Testes
  shapiro.test(modTR1$res) # Shapiro-Wilk
  ad.test(modTR1$res) # Anderson-Darling
  
# 2. Logarítmica:
  # Obs: precisa excluir valores = 0.
  TR2 <- log(Y)
  modTR2 = lm(TR2 ~ X1+X2+X3+X4)
  # Gráfico dos resíduos
  plotres(modTR2)
  # Testes
  shapiro.test(modTR2$res) # Shapiro-Wilk
  ad.test(modTR2$res) # Anderson-Darling
  
# 3. Hiperbólica
  TR3 <- 1/Y
  modTR3 = lm(TR3 ~ X1+X2+X3+X4)
  # Gráfico dos resíduos
  plotres(modTR3)
  # Testes
  shapiro.test(modTR3$res) # Shapiro-Wilk
  ad.test(modTR3$res) # Anderson-Darling
  
# 4. Box-Cox
  require(MASS)
  # Cálculo
  par(mfrow=c(1, 1))
  bc=boxcox(Y ~ X1+X2+X3+X4, data=dados, plotit=T)
  lambda.max <- bc$x[which.max(bc$y)]
  lambda.max # Se for próximo de zero, usar logarítmico (TR2).
  TR4 <- (Y^(lambda.max)-1)/lambda.max
  modTR4 = lm(TR4 ~ X1+X2+X3+X4)
  # Gráfico dos resíduos
  plotres(modTR4)
  # Testes
  shapiro.test(modTR4$res) # Shapiro-Wilk
  ad.test(modTR4$res) # Anderson-Darling
  
# Digite o nº do TR escolhido dentro de ( ):
  Y.TR <- 
    (Y) #troque aqui, por exemplo: (TR2).
  # Com isso, as próximas análises irão usar os
  # dados transformados!

  
  
  
# --------------------------------------------
# 6) TESTE DE HOMOCEDASTICIDADE DAS VARIÂNCIAS
# --------------------------------------------
  # Isso implica que cada tratamento que está sendo 
  # comparado pelo teste F, deve ter aproximadamente 
  # a mesma variância para que a ANOVA tenha validade.
  
  # Redefinição de dados:
  dados.TR <- data.frame(X1,
                         X2,
                         X3,
                         X4,
                         Y.TR)
  attach(dados.TR)
  mod.TR <- lm(Y.TR ~ X1+X2+X3+X4)
  
  # Teste de Bartlett:
  # Se p-value > 0,05, há homogeneidade das variâncias.
  bartlett.test(mod.TR$res ~ X1)
  bartlett.test(mod.TR$res ~ X2)
  bartlett.test(mod.TR$res ~ X3)
  bartlett.test(mod.TR$res ~ X4)
  
  # Boxplot de Xamentos vs resíduos:
  # Se os boxplots forem semelhantes, há homocedasticidade.
  par(mfrow=c(1, 1))
  boxplot(mod.TR$res ~ X1)
  boxplot(mod.TR$res ~ X2)
  boxplot(mod.TR$res ~ X3)
  boxplot(mod.TR$res ~ X4)


# --------------------------------------------
# 7) TESTE DE INDEPENDÊNCIA
# --------------------------------------------
# Os dados são aleatórios e independentes.
# Ou seja, uma observação não influencia na outra
# e não existe influência do tempo ou local da coleta.
  
  # Teste de Durbin-Watson:
  # Se p-value > 0,05, então há independência.
  require(lmtest)
  dwtest(mod.TR) 
  
  # Gráfico de resíduos padronizados vs valores ajustados
  # (Standardized Residuals vs Fitted Values):
  # Se os pontos forem aleatórios e espalhados,  
  # então os dados são aleatórios e independentes;
  # Se apresentar uma tendência, então há dependência.
  plotres(mod.TR)

  
# --------------------------------------------
# 8) ANOVA (análise de variância) da regressão
# --------------------------------------------

# Tabela ANOVA
# Se Pr(>F) < 0,05, então existe efeito da 
# variável explicativa (X) sobre a variável resposta (Y).
anova(mod.TR)  


# df=graus de liberdade. Pr=probabilidade 
# de ser maior que F tabelado.

# Exportar tabela ANOVA para Excel:
write.csv2(
  as.data.frame(anova(mod.TR)), 
  file = 
    "RLM - ANOVA da Regressão.csv") 



# --------------------------------------------
# 9) Coeficientes do modelo
# --------------------------------------------
# A equação de um modelo linear é:
# y = a*x+b

# Cálculo dos coeficientes
# Os coeficientes estão na coluna Estimate.
# a = X
# b = (Intercept)
# Se Pr(>|t|) < 0,05, então o coeficiente foi
# significativo a 5%.
  coefs <- as.data.frame(summary(mod.TR)[[4]])
  coefs.o <- as.data.frame(summary(mod)[[4]])
  coefs$Estimate <- coefs.o$Estimate
  coefs$`Std. Error` <- coefs.o$`Std. Error`
  colnames(coefs) <- c(
    "Estimativa", 
    "Erro padrão",
    "Valor t",
    "Pr(>|t|)"
  ) 
  coefs

# Exportar tabela para Excel:
  write.csv2(coefs,file = 
    "RLM - Coeficientes do modelo.csv") 

# --------------------------------------------
# 10) Intervalos de confiança
# --------------------------------------------
# Cálculo dos intervalos de confiança:
  ic <- as.data.frame(confint(mod.TR))

# Se os dados não foram transformados em 4.1), 
# rode o seguinte comando e pule para 10)
  ic

# a) Se os dados foram transformados em 4.1),
# Rode apenas a opção que foi usada para fazer
# a transformação inversa:
  # 1. Raiz quadrada:  
  ic <- (ic)^2
  ic  
  
  # 2. Logarítmica:  
  ic <- exp(ic)
  ic 

  # 3. Hiperbólica:  
  ic <- (ic)^(-1)
  ic 

  # 4. Box-Cox:  
  ic <- ((ic*lambda.max)+1)^(1/lambda.max)
  ic 


# --------------------------------------------
# 11) Coeficientes de determinação (R²)
# --------------------------------------------
# Quanto mais próximo de 1, então os dados 
# observados são mais próximos de ajustados. 
  
  # R²
  erro_puro <- lm(Y.TR ~ 
                    factor(X1) +
                    factor(X2) +
                    factor(X3) +
                    factor(X4))
  summary(mod.TR)$r.squared/summary(erro_puro)$r.squared

  # R² ajustado
  summary(mod.TR)$adj.r.squared/summary(erro_puro)$adj.r.squared
  

# --------------------------------------------
# 12) Teste da Falta de Ajuste
# --------------------------------------------
# Usar apenas quando tiver repetições.
# Se Pr(>F) > 0,05, então o modelo se ajusta bem aos dados 
# a 5% de significância.
anova(mod.TR, erro_puro)                        

# Exportar para Excel:
write.csv2(
  as.data.frame(anova(mod.TR, erro_puro)), 
  file = 
    "RLM - Teste da Falta de Ajuste.csv") 


# --------------------------------------------
# 13) Simplificação do modelo
# --------------------------------------------
# O método indica quais variáveis poderão 
# ser mantidas no modelo sem perda de qualidade.

# Método stepwise:
step(mod.TR, direction = "both")

# Método backward:
step(mod.TR, direction = "backward")

# Método forward:
step(mod.TR, direction = "forward")


# --------------------------------------------
# 13.1) ANOVA do novo modelo
# --------------------------------------------
# Modelo com as variáveis retiradas:
novo_modelo <- lm(Y.TR ~ X1+X3+X4) # modificar

# Tabela ANOVA
# Se Pr(>F) < 0,05, então existe efeito da 
# variável explicativa (X) sobre a variável resposta (Y).
anova(novo_modelo)  


# df=graus de liberdade. Pr=probabilidade 
# de ser maior que F tabelado.

# Exportar tabela ANOVA para Excel:
write.csv2(
  as.data.frame(anova(novo_modelo)), 
  file = 
    "RLM - ANOVA do novo modelo.csv") 


# --------------------------------------------
# 13.2) Coeficientes do modelo
# --------------------------------------------
# A equação de um modelo linear múltiplo é:
# y = a*x1+b*x2+b*x3+b*x4 + (Intercept)

# Cálculo dos coeficientes
# Se Pr(>|t|) < 0,05, então o coeficiente foi
# significativo a 5%.
coefs <- as.data.frame(summary(novo_modelo)[[4]])
ajuste.o = lm(Y ~ X1+X3+X4, data=dados) # modificar
coefs.o <- as.data.frame(summary(ajuste.o)[[4]])
coefs$Estimate <- coefs.o$Estimate
coefs$`Std. Error` <- coefs.o$`Std. Error`
colnames(coefs) <- c(
                  "Estimativa", 
                  "Erro padrão",
                  "Valor t",
                  "Pr(>|t|)"
                  ) 
coefs

# Exportar tabela para Excel:
write.csv2(coefs,file = 
             "RLM - Coeficientes do novo modelo.csv") 

# --------------------------------------------
# 13.3) Intervalos de confiança
# --------------------------------------------
# Cálculo dos intervalos de confiança:
ic <- as.data.frame(confint(novo_modelo))

# Se os dados não foram transformados em 4.1), 
# rode o seguinte comando e pule para 10)
ic

# a) Se os dados foram transformados em 4.1),
# Rode apenas a opção que foi usada para fazer
# a transformação inversa:
# 1. Raiz quadrada:  
ic <- (ic)^2
ic  

# 2. Logarítmica:  
ic <- exp(ic)
ic 

# 3. Hiperbólica:  
ic <- (ic)^(-1)
ic 

# 4. Box-Cox:  
ic <- ((ic*lambda.max)+1)^(1/lambda.max)
ic 


# --------------------------------------------
# 13.4) Coeficientes de determinação (R²)
# --------------------------------------------
# Quanto mais próximo de 1, então os dados 
# observados são mais próximos de ajustados. 

# R²
erro_puro <- lm(Y.TR ~ 
                  factor(X1) +
                  factor(X3) +
                  factor(X4)) # modificar 
summary(novo_modelo)$r.squared/summary(erro_puro)$r.squared

# R² ajustado
summary(novo_modelo)$adj.r.squared/summary(erro_puro)$adj.r.squared


# --------------------------------------------
# 13.5) Teste da Falta de Ajuste
# --------------------------------------------
# Usar apenas quando tiver repetições.
# Se Pr(>F) > 0,05, então o modelo se ajusta bem aos dados 
# a 5% de significância.
anova(novo_modelo, erro_puro)                        

# Exportar para Excel:
write.csv2(
  as.data.frame(anova(novo_modelo, erro_puro)), 
  file = 
    "RLM - Teste da Falta de Ajuste do novo modelo.csv") 


