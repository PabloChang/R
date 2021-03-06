# --------------------------------------------
# Regress�o Linear Simples
# Elaborado por: Pablo Chang (16/07/2020)
# https://github.com/PabloChang/R
# --------------------------------------------
# O arquivo de dados e script devem estar numa mesma pasta; 
# Os dados devem ser salvos em ".csv (separado por v�rgulas)";
# N�o pode haver espa�o e acentua��o nos t�tulos, �nica guia;
# Para rodar os comandos, use Ctrl + Enter em cada linha;
# Ativar/desativar coment�rios: Ctrl + Shift + C.

# --------------------------------------------
# 1) LEITURA E PREPARA��O DOS DADOS
# --------------------------------------------
# Comando para definir a localiza��o da pasta:
library(rstudioapi) # precisa ter instalado o pacote "rstudioapi"
  current_path = 
    rstudioapi::getActiveDocumentContext()$path 
  setwd(dirname(current_path ))
  print( getwd() )

# Troque o nome do arquivo de dados (entre " .csv"):
  dados <- read.csv2(
  "DadosExemploRLS.csv", 
  header = T)

# Troca os pontos dos gr�ficos por v�rgula:
  options(OutDec=",")

# Mostra as 6 primeiras linhas para visualiza��o.
# As colunas devem seguir a ordem: tratamento // resposta;
  head(dados)
  
# Filtrar dados por categorias, quando existir:
# Exemplo: mostrar somente dados da Profundidade 2.
# (Para usar, exclua o "#" abaixo e modifique)
# require(dplyr)
# dados <- filter(dados, Profundidade==2)

# Troque os nomes das colunas (entre "c(  )"):
# TRAT = c(  ): para vari�vel independente X;
# RESP = c(  ): para vari�vel resposta Y a ser analisada.
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

# Anexar os dados na mem�ria do R:
attach(dados) 


# --------------------------------------------
# 2) RESUMO DESCRITIVO 
# --------------------------------------------
# Pacote "agricolae".
# Caso n�o tenho instalado � s� rodar: 
# install.packages("agricolae")
library(agricolae)

# C�lculo dos Quartis
quartil1 <- quantile(Y, 0.25)
quartil2 <- quantile(Y, 0.5)
quartil3 <- quantile(Y, 0.75)

# Resumo descritivo
resumo <- data.frame(Estat�stica = c(
  "M�nimo", 
  "1� Quartil",
  "Mediana",
  "M�dia",
  "3� Quartil",
  "M�ximo",
  "Vari�ncia",
  "Desvio Padr�o",
  "Coeficiente de Varia��o (%)",
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
# Boxplot geral, com a cruz representando a m�dia.
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
        xlab="Densidade (g/cm�)",
        ylab="Massa fresca da raiz (g)")
points(tapply(Y,X,mean),col="red",pch=3)


# --------------------------------------------
# 4) TESTE DE NORMALIDADE
# --------------------------------------------
# Pacote com alguns testes:
library(nortest)
library(ExpDes)

# C�lculo dos res�duos:
mod = lm(Y ~ X)

# Gr�ficos de res�duos:
# Para ser normal, o histograma deve ter formato de sino no centro.
# Se o gr�fico Normal Q-Q se assemelhar a uma reta crescente,
# ent�o existe normalidade.
plotres(mod)

# Testes:
# Se p-value > 0,05, os res�duos possuem distribui��o normal.
shapiro.test(mod$res) # Shapiro-Wilk
ad.test(mod$res) # Anderson-Darling

# Os dados s�o normais?
# Se SIM, rode o comando abaixo e pule para a etapa 5).
  Y.TR <- Y
# Se N�O, fa�a a transforma��o em 4.1).

# A falta de normalidade n�o introduz problema 
# na estima��o dos par�metros, mas sim,
# nos desvios padr�es, afetando a validade dos
# intervalos de confian�a e dos testes de 
# hip�tese.


# --------------------------------------------
# 4.1) TRANSFORMA��O DE DADOS N�O-NORMAIS
# --------------------------------------------
# Fa�a os testes, at� atingir a normalidade!

# TR1. Raiz quadrada:
  TR1 <- (Y)^2
  modTR1 = lm(TR1 ~ X)
  # Gr�fico dos res�duos
  plotres(modTR1)
  # Testes
  shapiro.test(modTR1$res) # Shapiro-Wilk
  ad.test(modTR1$res) # Anderson-Darling
  
# TR2. Logar�tmica:
  # Obs: precisa excluir valores = 0.
  TR2 <- log(Y)
  modTR2 = lm(TR2 ~ X)
  # Gr�fico dos res�duos
  plotres(modTR2)
  # Testes
  shapiro.test(modTR2$res) # Shapiro-Wilk
  ad.test(modTR2$res) # Anderson-Darling
  
# TR3. Hiperb�lica
  TR3 <- 1/Y
  modTR3 = lm(TR3 ~ X)
  # Gr�fico dos res�duos
  plotres(modTR3)
  # Testes
  shapiro.test(modTR3$res) # Shapiro-Wilk
  ad.test(modTR3$res) # Anderson-Darling
  
# TR4. Box-Cox
  require(MASS)
  # C�lculo
  par(mfrow=c(1, 1))
  bc=boxcox(Y ~ X, data=dados, plotit=T)
  lambda.max <- bc$x[which.max(bc$y)]
  lambda.max # Se for pr�ximo de zero, usar logar�tmico (TR2).
  TR4 <- (Y^(lambda.max)-1)/lambda.max
  modTR4 = lm(TR4 ~ X)
  # Gr�fico dos res�duos
  plotres(modTR4)
  # Testes
  shapiro.test(modTR4$res) # Shapiro-Wilk
  ad.test(modTR4$res) # Anderson-Darling
  
# Digite o TR escolhido dentro de ( ):
  Y.TR <- 
    (Y) #troque aqui, por exemplo: (TR2).
  # Com isso, as pr�ximas an�lises ir�o usar os
  # dados transformados!

  
# --------------------------------------------
# 5) TESTE DE HOMOCEDASTICIDADE DAS VARI�NCIAS
# --------------------------------------------
  # Isso implica que cada tratamento que est� sendo 
  # comparado pelo teste F, deve ter aproximadamente 
  # a mesma vari�ncia para que a ANOVA tenha validade.
  
  # Redefini��o de dados:
  dados.TR <- data.frame(X, Y.TR)
  attach(dados.TR)
  mod.TR <- lm(Y.TR ~ X)

  # Teste de Bartlett:
  # Se p-value > 0,05, h� homogeneidade das vari�ncias.
  bartlett.test(mod.TR$res ~ X) 
  
  # Boxplot de Xamentos vs res�duos:
  # Se os boxplots forem semelhantes, h� homocedasticidade.
  par(mfrow=c(1, 1))
  boxplot(mod.TR$res ~ X)


# --------------------------------------------
# 6) TESTE DE INDEPEND�NCIA
# --------------------------------------------
# Os dados s�o aleat�rios e independentes.
# Ou seja, uma observa��o n�o influencia na outra
# e n�o existe influ�ncia do tempo ou local da coleta.
  
  # Teste de Durbin-Watson:
  # Se p-value > 0,05, ent�o h� independ�ncia.
  require(lmtest)
  dwtest(mod.TR) 
  
  # Gr�fico de res�duos padronizados vs valores ajustados
  # (Standardized Residuals vs Fitted Values):
  # Se os pontos forem aleat�rios e espalhados,  
  # ent�o os dados s�o aleat�rios e independentes;
  # Se apresentar uma tend�ncia, ent�o h� depend�ncia.
  plotres(mod.TR)


# --------------------------------------------
# 7) ANOVA (an�lise de vari�ncia) da regress�o
# --------------------------------------------

# Tabela ANOVA
# Se Pr(>F) < 0,05, ent�o existe efeito da 
# vari�vel explicativa (X) sobre a vari�vel resposta (Y).
anova(mod.TR)  


# df=graus de liberdade. Pr=probabilidade 
# de ser maior que F tabelado.

# Exportar tabela ANOVA para Excel:
write.csv2(
  as.data.frame(anova(mod.TR)), 
  file = 
    "RLS - ANOVA da Regress�o.csv") 


# --------------------------------------------
# 8) Coeficientes do modelo
# --------------------------------------------
# A equa��o de um modelo linear �:
# y = a*x+b

# C�lculo dos coeficientes
# Os coeficientes est�o na coluna Estimativa.
# a = X
# b = (Intercept)
# Se Pr(>|t|) < 0,05, ent�o o coeficiente foi
# significativo a 5%.
  coefs <- as.data.frame(summary(mod.TR)[[4]])
  coefs.o <- as.data.frame(summary(mod)[[4]])
  coefs$Estimate <- coefs.o$Estimate
  coefs$`Std. Error` <- coefs.o$`Std. Error`
  colnames(coefs) <- c(
    "Estimativa", 
    "Erro padr�o",
    "Valor t",
    "Pr(>|t|)"
  ) 
  coefs

# Exportar tabela para Excel:
  write.csv2(coefs,file = 
    "RLS - Coeficientes do modelo.csv") 

# --------------------------------------------
# 9) Intervalos de confian�a
# --------------------------------------------
# C�lculo dos intervalos de confian�a:
  ic <- as.data.frame(confint(mod.TR))

# Se os dados n�o foram transformados em 4.1), 
# rode o seguinte comando e pule para 10)
  ic

# a) Se os dados foram transformados em 4.1),
# Rode apenas a op��o que foi usada para fazer
# a transforma��o inversa:
  # 1. Raiz quadrada:  
  ic <- (ic)^2
  ic  
  
  # 2. Logar�tmica:  
  ic <- exp(ic)
  ic 

  # 3. Hiperb�lica:  
  ic <- (ic)^(-1)
  ic 

  # 4. Box-Cox:  
  ic <- ((ic*lambda.max)+1)^(1/lambda.max)
  ic 


# --------------------------------------------
# 10) Coeficientes de determina��o (R�)
# --------------------------------------------
  
  # R�
  erro_puro <- lm(Y.TR ~ factor(X))
  summary(mod.TR)$r.squared/summary(erro_puro)$r.squared

  # R� ajustado
  summary(mod.TR)$adj.r.squared/summary(erro_puro)$adj.r.squared
  

# --------------------------------------------
# 11) Gr�fico da regress�o
# --------------------------------------------
# Gr�fico de dispers�o (cinza):
  par(mfrow=c(1, 1))
  plot (Y ~ X,
      xlab="Densidade (g/cm�)", 
      ylab="Massa fresca da raiz (g)",
      pch=19, col="gray", data = dados)

# Linha de tend�ncia (vermelho):
abline(mod, col="red", lwd=2) 

# M�dias (azul):
  u <- as.data.frame(tapply(Y, X, mean))
  options(OutDec=".")
  v <- factor(X)
  v <- levels(v)
  v <- as.numeric(v)
  options(OutDec=",")
  points(u[[1]] ~ v,col="blue",pch=19)

  
# --------------------------------------------
# 12) Teste da Falta de Ajuste
# --------------------------------------------
# Usar apenas quando Y tiver repeti��es.
# Se Pr(>F) > 0,05, ent�o o modelo se ajusta bem aos dados 
# a 5% de signific�ncia.
anova(mod.TR, erro_puro)                        

# Exportar para Excel:
write.csv2(
  as.data.frame(anova(mod.TR, erro_puro)), 
  file = 
    "RLS - Teste da Falta de Ajuste.csv") 



