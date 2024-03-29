# ____________________________________________
# Regress�o Linear M�ltipla
# Elaborado por: Pablo Chang (25/07/21)
# https://github.com/PabloChang/R
# ____________________________________________
# O arquivo de dados e script devem estar numa mesma pasta; 
# Os dados devem ser salvos em ".csv (separado por v�rgulas)";
# N�o pode haver espa�o e acentua��o nos t�tulos, �nica guia;
# Para rodar os comandos, use Ctrl + Enter em cada linha;
# Ativar/desativar coment�rios: Ctrl + Shift + C.

# ____________________________________________
# 1) LEITURA E PREPARA��O DOS DADOS ----
# ____________________________________________
# Comando para definir a localiza��o da pasta:
# Precisa ter instalado o pacote "agricolae".
{
  require(agricolae)
  current_path=rstudioapi::getActiveDocumentContext()$path 
  setwd(dirname(current_path))
  print(getwd())
}

# Troque o nome do arquivo de dados (entre " .csv"):
dados <- read.csv2(
  "DadosExemploRLM.csv",
  header = T)

# Troca os pontos dos gr�ficos por v�rgula:
options(OutDec=",")

# Mostra as 6 primeiras linhas para visualiza��o.
head(dados)
  
# Filtrar dados por categorias, quando existir:
# Exemplo: mostrar somente dados da Profundidade 2.
# (Para usar, exclua o "#" abaixo e modifique)
# require(dplyr)
# dados <- filter(dados, Profundidade==2)

# Troque os nomes das colunas (entre "c(  )"):
# X1 = c(  ): para a primeira vari�vel independente;
# X2 = c(  ): para a segunda vari�vel independente;
# X3 ...
# Y = c(  ): para vari�vel resposta Y a ser analisada.
{attach(dados) 
dados <- data.frame(X1 = c(cyl),
                    X2 = c(disp),
                    X3 = c(hp),
                    X4 = c(wt),
                    Y = c(mpg)
)

# Mostra as 6 primeiras linhas para ver como ficou.
head(dados)}

# Deletar valores condicionais
  # Y < x: deletar valores abaixo de x.
  # Y > x: deletar valores acima de x.
  # Y == x: deletar valores iguais a x.
  # Para usar, exclua o "#" abaixo e modifique:
# r <- with(dados, which(Y<0, arr.ind=TRUE))
#  dados <- dados[-r, ]
# r <- with(dados, which(Y==0, arr.ind=TRUE))
#  dados <- dados[-r, ]

# Anexar os dados na mem�ria do R:
attach(dados) 


# ____________________________________________
# 2) RESUMO DESCRITIVO ----
# ____________________________________________
{
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
}

# Exportar o Resumo Descritivo para Excel:
write.csv2(resumo, 
           file = "RLM - Resumo Descritivo.csv") 


# ____________________________________________
# 3) BOXPLOT ----
# ____________________________________________
# Boxplot de Y. A cruz � a m�dia.
{
require(graphics)
boxplot(Y,
        col="yellow",  # cor do boxplot
        outcol="blue", # cor de outlier
        ylab="Y")
points(mean(Y),col="red",pch=3)
}


# ____________________________________________
# 4) RELA��O ENTRE AS VARI�VEIS ----
# ____________________________________________
# Trocar nome das vari�veis:
{
renomeado <- dados
colnames(renomeado) <- 
  c("cyl",  # X1
    "disp", # X2
    "hp",   # X3
    "wt",   # X4
    "mpg"   # Y
    )
}

# Correla��o linear de Pearson:
# Quanto mais pr�ximo de +1, correla��o positiva;
# Quanto mais pr�ximo de -1, correla��o negativa.
{require(PerformanceAnalytics)
chart.Correlation(renomeado, histogram=T)}

# Com cores e dendograma: 
# Rela��o de proximidade entre vari�veis:
heatmap(abs(cor(renomeado)))

# Com escala de cores e n�meros:
# Config: https://rpubs.com/melinatarituba/353262
{
require(ggplot2)
require(GGally)
require(plyr)
require(reshape)
ggcorr(renomeado, 
       label=T,
       label_round=3) # Casas decimais
}


# --------------------------------------------
# 5) TESTE DE NORMALIDADE
# --------------------------------------------
{
# Pacote com alguns testes:
library(nortest)
library(ExpDes)

# C�lculo dos res�duos:
mod = lm(Y ~ X1+X2+X3+X4)

# Gr�ficos de res�duos:
# Para ser normal, o histograma deve ter formato de sino no centro.
# Se o gr�fico Normal Q-Q se assemelhar a uma reta crescente,
# ent�o existe normalidade.
plotres(mod)
}

# Testes:
# Se p-value > 0,05, os res�duos possuem distribui��o normal.
shapiro.test(mod$res) # Shapiro-Wilk
ad.test(mod$res) # Anderson-Darling

# Os dados s�o normais?
# Se SIM, rode o comando abaixo e pule para a etapa 6).
  {Y.TR <- Y
# Se N�O, fa�a a transforma��o em 5.1).

# A falta de normalidade n�o introduz problema 
# na estima��o dos par�metros, mas sim,
# nos desvios padr�es, afetando a validade dos
# intervalos de confian�a e dos testes de 
# hip�tese.


# --------------------------------------------
# 5.1) TRANSFORMA��O DE DADOS N�O-NORMAIS
# --------------------------------------------
# Fa�a os testes, at� atingir a normalidade!

# TR1. Raiz quadrada:
  {TR1 <- sqrt(Y)
  modTR1 = lm(TR1 ~ X1+X2+X3+X4)
  # Gr�fico dos res�duos
  plotres(modTR1)}
  # Testes
  shapiro.test(modTR1$res) # Shapiro-Wilk
  ad.test(modTR1$res) # Anderson-Darling
  
# TR2. Logar�tmica:
  # Obs: precisa excluir valores = 0.
  {TR2 <- log(Y)
  modTR2 = lm(TR2 ~ X1+X2+X3+X4)
  # Gr�fico dos res�duos
  plotres(modTR2)}
  # Testes
  shapiro.test(modTR2$res) # Shapiro-Wilk
  ad.test(modTR2$res) # Anderson-Darling
  
# TR3. Hiperb�lica
  {TR3 <- 1/Y
  modTR3 = lm(TR3 ~ X1+X2+X3+X4)
  # Gr�fico dos res�duos
  plotres(modTR3)}
  # Testes
  shapiro.test(modTR3$res) # Shapiro-Wilk
  ad.test(modTR3$res) # Anderson-Darling
  
# TR4. Box-Cox
  {require(MASS)
  # C�lculo
  par(mfrow=c(1, 1))
  bc=boxcox(Y ~ X1+X2+X3+X4, data=dados, plotit=T)
  lambda.max <- bc$x[which.max(bc$y)]
  lambda.max # Se for pr�ximo de zero, usar logar�tmico (TR2).
  TR4 <- (Y^(lambda.max)-1)/lambda.max
  modTR4 = lm(TR4 ~ X1+X2+X3+X4)
  # Gr�fico dos res�duos
  plotres(modTR4)}
  # Testes
  shapiro.test(modTR4$res) # Shapiro-Wilk
  ad.test(modTR4$res) # Anderson-Darling
  
# Digite o TR escolhido dentro de ( ):
  Y.TR <- 
    (Y) # troque aqui, por exemplo: (TR2).
  # Com isso, as pr�ximas an�lises ir�o usar os
  # dados transformados!
}
  

# --------------------------------------------
# 6) TESTE DE HOMOCEDASTICIDADE DAS VARI�NCIAS
# --------------------------------------------
# Isso implica que cada tratamento que est� sendo 
# comparado pelo teste F, deve ter aproximadamente 
# a mesma vari�ncia para que a ANOVA tenha validade.
{
# Redefini��o de dados:
dados.TR <- data.frame(X1,
                       X2,
                       X3,
                       X4,
                       Y.TR)
attach(dados.TR)
mod.TR <- lm(Y.TR ~ X1+X2+X3+X4)
}
  
# Teste de Bartlett:
# Se p-value > 0,05, h� homogeneidade das vari�ncias.
bartlett.test(mod.TR$res ~ X1)
bartlett.test(mod.TR$res ~ X2)
bartlett.test(mod.TR$res ~ X3)
bartlett.test(mod.TR$res ~ X4)

# Boxplot de tramentos vs res�duos:
# Se os boxplots forem semelhantes, h� homocedasticidade.
par(mfrow=c(1, 1))
boxplot(mod.TR$res ~ X1)
boxplot(mod.TR$res ~ X2)
boxplot(mod.TR$res ~ X3)
boxplot(mod.TR$res ~ X4)


# --------------------------------------------
# 7) TESTE DE INDEPEND�NCIA
# --------------------------------------------
# Os dados s�o aleat�rios e independentes.
# Ou seja, uma observa��o n�o influencia na outra
# e n�o existe influ�ncia do tempo ou local da coleta.
  
# Teste de Durbin-Watson:
# Se p-value > 0,05, ent�o h� independ�ncia.
{require(lmtest)
dwtest(mod.TR)}

# Gr�fico de res�duos padronizados vs valores ajustados
# (Standardized Residuals vs Fitted Values):
# Se os pontos forem aleat�rios e espalhados,  
# ent�o os dados s�o aleat�rios e independentes;
# Se apresentar uma tend�ncia, ent�o h� depend�ncia.
plotres(mod.TR)

  
# --------------------------------------------
# 8) ANOVA (an�lise de vari�ncia) da regress�o
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
    "RLM - ANOVA da Regress�o.csv") 


# --------------------------------------------
# 9) Coeficientes do modelo
# --------------------------------------------
# A equa��o de um modelo linear �:
# y = b + a*X1 + b*X2 + c*X3 + d*X4

# C�lculo dos coeficientes
# Os coeficientes est�o na coluna Estimate.
# a = X
# b = (Intercept)
# Se Pr(>|t|) < 0,05, ent�o o coeficiente foi
# significativo a 5%.
{
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
}

# Exportar tabela para Excel:
  write.csv2(coefs,file = 
    "RLM - Coeficientes do modelo.csv") 

# Equa��o em texto:
cat("mpg=",coefs$Estimativa[1],
               "+(",coefs$Estimativa[2],")*cyl",
               "+(",coefs$Estimativa[3],")*disp",
               "+(",coefs$Estimativa[4],")*hp",
               "+(",coefs$Estimativa[5],")*wt",
    sep="")


# --------------------------------------------
# 10) Intervalos de confian�a
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
# 11) Coeficientes de determina��o (R�)
# --------------------------------------------
# Quanto mais pr�ximo de 1, ent�o os dados 
# observados s�o mais pr�ximos de ajustados. 
  
  # R�
  erro_puro <- lm(Y.TR ~ 
                    factor(X1) +
                    factor(X2) +
                    factor(X3) +
                    factor(X4))
  summary(mod.TR)$r.squared/summary(erro_puro)$r.squared

  # R� ajustado
  summary(mod.TR)$adj.r.squared/summary(erro_puro)$adj.r.squared
  

# --------------------------------------------
# 12) Teste da Falta de Ajuste
# --------------------------------------------
# Usar apenas quando tiver repeti��es.
# Se Pr(>F) > 0,05, ent�o o modelo se ajusta bem aos dados 
# a 5% de signific�ncia.
anova(mod.TR, erro_puro)                        

# Exportar para Excel:
write.csv2(
  as.data.frame(anova(mod.TR, erro_puro)), 
  file = 
    "RLM - Teste da Falta de Ajuste.csv") 


# --------------------------------------------
# 13) Simplifica��o do modelo
# --------------------------------------------
# O m�todo indica quais vari�veis poder�o 
# ser mantidas no modelo sem perda de qualidade.

# M�todo stepwise:
step(mod.TR, direction = "both")

# M�todo backward:
step(mod.TR, direction = "backward")

# M�todo forward:
step(mod.TR, direction = "forward")


# --------------------------------------------
# 13.1) ANOVA do novo modelo
# --------------------------------------------
# Modelo com as vari�veis retiradas:
novo_modelo <- lm(Y.TR ~ X1+X3+X4) # modificar

# Tabela ANOVA
# Se Pr(>F) < 0,05, ent�o existe efeito da 
# vari�vel explicativa (X) sobre a vari�vel resposta (Y).
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
# A equa��o de um modelo linear m�ltiplo �:
# y = a*x1+b*x2+b*x3+b*x4 + (Intercept)

# C�lculo dos coeficientes
# Se Pr(>|t|) < 0,05, ent�o o coeficiente foi
# significativo a 5%.
coefs <- as.data.frame(summary(novo_modelo)[[4]])
ajuste.o = lm(Y ~ X1+X3+X4, data=dados) # modificar
coefs.o <- as.data.frame(summary(ajuste.o)[[4]])
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
             "RLM - Coeficientes do novo modelo.csv") 

# --------------------------------------------
# 13.3) Intervalos de confian�a
# --------------------------------------------
# C�lculo dos intervalos de confian�a:
ic <- as.data.frame(confint(novo_modelo))

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
# 13.4) Coeficientes de determina��o (R�)
# --------------------------------------------
# Quanto mais pr�ximo de 1, ent�o os dados 
# observados s�o mais pr�ximos de ajustados. 

# R�
erro_puro <- lm(Y.TR ~ 
                  factor(X1) +
                  factor(X3) +
                  factor(X4)) # modificar 
summary(novo_modelo)$r.squared/summary(erro_puro)$r.squared

# R� ajustado
summary(novo_modelo)$adj.r.squared/summary(erro_puro)$adj.r.squared


# --------------------------------------------
# 13.5) Teste da Falta de Ajuste
# --------------------------------------------
# Usar apenas quando tiver repeti��es.
# Se Pr(>F) > 0,05, ent�o o modelo se ajusta bem aos dados 
# a 5% de signific�ncia.
anova(novo_modelo, erro_puro)                        

# Exportar para Excel:
write.csv2(
  as.data.frame(anova(novo_modelo, erro_puro)), 
  file = 
    "RLM - Teste da Falta de Ajuste do novo modelo.csv") 


