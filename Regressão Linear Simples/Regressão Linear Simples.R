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
  "DadosExemploRLS.csv", 
  header = T)

# Troca os pontos dos gráficos por vírgula:
  options(OutDec=",")

# Mostra as 6 primeiras linhas para visualização.
# As colunas devem seguir a ordem: tratamento // resposta;
  head(dados)

# Troque os nomes das colunas (entre "c(  )"):
# TRAT = c(  ): para variável independente X;
# RESP = c(  ): para variável resposta Y a ser analisada.
  attach(dados) 
  dados <- data.frame(X = c(Densidade), 
                      Y = c(MassaFrescaDaRaiz)
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
           file = "RLS - Resumo Descritivo.csv") 


# --------------------------------------------
# 3) BOXPLOT 
# --------------------------------------------
# Boxplot geral, com a cruz representando a média.
# Altere as cores e legendas entre aspas " ".
require(graphics)
boxplot(Y,
        col="yellow",
        outcol="blue",
        ylab="Massa fresca da raiz (g)")
points(mean(Y),col="red",pch=3)

# Boxplot para cada tratamento
boxplot(Y ~ X,
        col="skyblue1",
        outcol="blue",
        xlab="Densidade (g/cm³)",
        ylab="Massa fresca da raiz (g)")
points(tapply(Y,X,mean),col="red",pch=3)


# --------------------------------------------
# 4) TESTE DE NORMALIDADE
# --------------------------------------------
# Pacote com alguns testes:
library(nortest)
library(ExpDes)

# Cálculo dos resíduos:
mod = with(dados, lm(Y ~ X))

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
# Se SIM, rode o comando abaixo e pule para a etapa 5).
  Y.TR <- Y
# Se NÃO, faça a transformação em 4.1).

# A falta de normalidade não introduz problema 
# na estimação dos parâmetros, mas sim,
# nos desvios padrões, afetando a validade dos
# intervalos de confiança e dos testes de 
# hipótese.


# --------------------------------------------
# 4.1) TRANSFORMAÇÃO DE DADOS NÃO-NORMAIS
# --------------------------------------------
# Faça os testes, até atingir a normalidade!

# 1. Raiz quadrada:
  TR1 <- (Y)^2
  modTR1 = with(dados, lm(TR1 ~ X))
  # Gráfico dos resíduos
  plotres(modTR1)
  # Testes
  shapiro.test(modTR1$res) # Shapiro-Wilk
  ad.test(modTR1$res) # Anderson-Darling
  
# 2. Logarítmica:
  # Obs: precisa excluir valores = 0.
  TR2 <- log(Y)
  modTR2 = with(dados, lm(TR2 ~ X))
  # Gráfico dos resíduos
  plotres(modTR2)
  # Testes
  shapiro.test(modTR2$res) # Shapiro-Wilk
  ad.test(modTR2$res) # Anderson-Darling
  
# 3. Hiperbólica
  TR3 <- 1/Y
  modTR3 = with(dados, lm(TR3 ~ X))
  # Gráfico dos resíduos
  plotres(modTR3)
  # Testes
  shapiro.test(modTR3$res) # Shapiro-Wilk
  ad.test(modTR3$res) # Anderson-Darling
  
# 4. Box-Cox
  require(MASS)
  # Cálculo
  par(mfrow=c(1, 1))
  bc=boxcox(Y ~ X, data=dados, plotit=T)
  lambda.max <- bc$x[which.max(bc$y)]
  lambda.max # Se for próximo de zero, usar logarítmico (TR2).
  TR4 <- (Y^(lambda.max)-1)/lambda.max
  modTR4 = with(dados, lm(TR4 ~ X))
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
# 5) TESTE DE HOMOCEDASTICIDADE DAS VARIÂNCIAS
# --------------------------------------------
  # Isso implica que cada tratamento que está sendo 
  # comparado pelo teste F, deve ter aproximadamente 
  # a mesma variância para que a ANOVA tenha validade.
  
  # Redefinição de dados:
  dados.TR <- data.frame(X, Y.TR)
  attach(dados.TR)
  ajuste <- lm(Y.TR ~ X)
  mod.TR = with(dados.TR, ajuste)
  
  # Teste de Bartlett:
  # Se p-value > 0,05, há homogeneidade das variâncias.
  bartlett.test(mod.TR$res ~ X) 
  
  # Boxplot de Xamentos vs resíduos:
  # Se os boxplots forem semelhantes, há homocedasticidade.
  par(mfrow=c(1, 1))
  boxplot(mod.TR$res ~ X)


# --------------------------------------------
# 6) TESTE DE INDEPENDÊNCIA
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
# 7) ANOVA (análise de variância) da regressão
# --------------------------------------------

# Tabela ANOVA
# Se Pr(>F) < 0,05, então existe efeito da 
# variável explicativa (X) sobre a variável resposta (Y).
anova(mod.TR)  


# df=graus de liberdade. Pr=probabilidade 
# de ser maior que F tabelado.

# Exportar tabela ANOVA para Excel:
write.csv2(
  as.data.frame(anova(ajuste)), 
  file = 
    "RLS - ANOVA da Regressão.csv") 


# --------------------------------------------
# 8) Coeficientes do modelo
# --------------------------------------------
# A equação de um modelo linear é:
# y = a*x+b

# Cálculo dos coeficientes
# Os coeficientes estão na coluna Estimate.
# a = X
# b = (Intercept)
# Se Pr(>|t|) < 0,05, então o coeficiente foi
# significativo a 5%.
  coefs <- as.data.frame(summary(ajuste)[[4]])
  ajuste.o = lm(Y ~ X, data=dados) 
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
    "RLS - Coeficientes do modelo.csv") 

# --------------------------------------------
# 9) Intervalos de confiança
# --------------------------------------------
# Cálculo dos intervalos de confiança:
  ic <- as.data.frame(confint(ajuste))

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
# 10) Coeficientes de determinação (R²)
# --------------------------------------------
  
  # R²
  erro_puro <- lm(Y.TR ~ factor(X))
  summary(ajuste)$r.squared/summary(erro_puro)$r.squared

  # R² ajustado
  summary(ajuste)$adj.r.squared/summary(erro_puro)$adj.r.squared
  

# --------------------------------------------
# 11) Gráfico da regressão
# --------------------------------------------
# Gráfico de dispersão (cinza):
  par(mfrow=c(1, 1))
  plot (Y ~ X,
      xlab="Densidade (g/cm³)", 
      ylab="Massa fresca da raiz (g)",
      pch=19, col="gray", data = dados)

# Linha de tendência (vermelho):
abline(ajuste.o, col="red", lwd=2) 

# Médias (azul):
  u <- as.data.frame(tapply(Y, X, mean))
  u[[1]]
  options(OutDec=".")
  v <- factor(X)
  v <- levels(v)
  v <- as.numeric(v)
  options(OutDec=",")
  points(u[[1]] ~ v,col="blue",pch=19)


  
# --------------------------------------------
# 12) Teste da Falta de Ajuste
# --------------------------------------------
# Usar apenas quando Y tiver repetições.
# Se Pr(>F) > 0,05, então o modelo se ajusta bem aos dados 
# a 5% de significância.
anova(ajuste, erro_puro)                        

# Exportar para Excel:
write.csv2(
  as.data.frame(anova(ajuste, erro_puro)), 
  file = 
    "RLS - Teste da Falta de Ajuste.csv") 





