# --------------------------------------------
# Fatorial duplo em Delineamento Inteiramente ao Acaso
# Elaborado por: Pablo Chang (21/04/2020)
# https://github.com/PabloChang/R
# --------------------------------------------
# O arquivo de dados e script devem estar numa mesma pasta; 
# Os dados devem ser salvos em ".csv (separado por v�rgulas)";
# N�o pode haver espa�o e acentua��o nos t�tulos, �nica guia;
# Os tratametos/fatores devem ser num�ricos;
# Para rodar os comandos, use Ctrl + Enter em cada linha;
# Ativar/desativar coment�rios: Ctrl + Shift + C.

# --------------------------------------------
# 1) LEITURA E PREPARA��O DOS DADOS
# --------------------------------------------
# Comando para definir a localiza��o da pasta:
# Caso n�o tenho instalado o pacote � s� rodar (sem #):
# install.packages("rstudioapi")
library(rstudioapi)
  current_path =
    rstudioapi::getActiveDocumentContext()$path
  setwd(dirname(current_path))
  print(getwd())

# Troque o nome do arquivo de dados (entre " .csv"):
dados <- read.csv2(
  "ExemploDadosFatorial.csv", 
  header = T)

# Comando para trocar os pontos dos gr�ficos por v�rgula.
# Obs: para digitar no R, o padr�o ainda � "."
options(OutDec=",")

# Mostra as 6 primeiras linhas para visualiza��o.
head(dados)

# Troque os nomes das colunas (entre "c(  )"):
# FATOR1 = c(  ): para TRATamento;
# RESP = c(  ): para vari�vel resposta a ser analisada.
attach(dados) 
dados <- data.frame(FATOR1 = c(Recipiente),
                    FATOR2 = c(Especie),
                    RESP = c(Altura)
                    )

# Comando para deletar observa��es com dados vazios:
dados <- na.omit(dados)

# Mostra as 6 primeiras linhas para ver como ficou.
head(dados)

# Deletar valores condicionais
  # RESP < x: deletar valores abaixo de x.
  # RESP > x: deletar valores acima de x.
  # RESP == x: deletar valores iguais a x.
  # Para usar, exclua o "#" abaixo e modifique:
# r <- with(dados, which(RESP==0, arr.ind=TRUE))
#  dados <- dados[-r, ]
# r <- with(dados, which(RESP<0, arr.ind=TRUE))
#  dados <- dados[-r, ]

 
# Anexa os dados na mem�ria do R:
attach(dados) 


# --------------------------------------------
# 2) RESUMO DESCRITIVO 
# --------------------------------------------
# Pacote "agricolae".
# Caso n�o tenho instalado � s� rodar: 
# install.packages("agricolae")
library(agricolae)

# C�lculo dos Quartis
quartil1 <- quantile(RESP, 0.25)
quartil2 <- quantile(RESP, 0.5)
quartil3 <- quantile(RESP, 0.75)

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
                        min(RESP), 
                        quartil1[[1]], 
                        quartil2[[1]],
                        mean(RESP),
                        quartil3[[1]],
                        max(RESP),
                        var(RESP),
                        sd(RESP),
                        100*sd(RESP)/mean(RESP),
                        skewness(RESP),
                        kurtosis(RESP),
                        length(RESP)
                      ),
                      stringsAsFactors = FALSE)
resumo

# Exportar o Resumo Descritivo para Excel:
# Obs: antes de rodar novamente, feche o arquivo.
write.csv2(resumo, 
           file = "Fatorial - Resumo Descritivo.csv") 


# --------------------------------------------
# 3) BOXPLOT 
# --------------------------------------------
# Boxplot geral, com a cruz representando a m�dia:
# Altere as cores e legendas entre aspas " ".
  require(graphics)
  boxplot(RESP,
            col="yellow",
            outcol="blue",
            ylab="Altura da planta (cm)")
  points(mean(RESP),col="red",pch=3)

# Boxplot para cada FATOR1:
  boxplot(RESP ~ FATOR1,
      col="skyblue1",
      outcol="blue",
      xlab="Recipiente",
      ylab="Altura da planta (cm)")
  points(tapply(RESP,TRAT,mean),col="red",pch=3)
  
# Boxplot para cada n�vel do FATOR2:
  boxplot(RESP ~ FATOR2,
          col="green",
          outcol="blue",
          xlab="Especie",
          ylab="Altura da planta (cm)")
  points(tapply(RESP,FATOR1,mean),col="red",pch=3)

    
# --------------------------------------------
# 4) TESTE DE NORMALIDADE
# --------------------------------------------
# Pacote com alguns testes:
library(nortest)
require(ExpDes)

# Defini��o de fatores:
FATOR1 <- as.factor(FATOR1)
FATOR2 <- as.factor(FATOR2)
RESP <- as.numeric(RESP)

# C�lculo dos res�duos:
mod = with(dados, aov(RESP ~ FATOR1*FATOR2))

# Gr�ficos de res�duos:
# Para ser normal, o histograma deve ter formato de sino no centro.
# Se o gr�fico Normal Q-Q se assemelhar a uma reta crescente,
# ent�o existe normalidade.
plotres(mod)

# Testes:
# Se p-value > 0,05, os res�duos possuem distribui��o normal.
  shapiro.test(mod$res) # Shapiro-Wilk
  ad.test(mod$res) # Anderson-Darling

# Os res�duos s�o normais?
  # Se SIM, rode o comando abaixo e pule para a etapa 5).
    RESP.TR <- RESP
  # Se N�O, fa�a a transforma��o em 4.1).


# --------------------------------------------
# 4.1) TRANSFORMA��O DE DADOS N�O-NORMAIS
# --------------------------------------------
# Fa�a os testes, at� atingir a normalidade!
  
# 1. Raiz quadrada:
  TR1 <- (RESP)^2
  modTR1 = with(dados, aov(TR1 ~ FATOR1*FATOR2))
  # Gr�fico dos res�duos
  plotres(modTR1)
  # Testes
  shapiro.test(modTR1$res) # Shapiro-Wilk
  ad.test(modTR1$res) # Anderson-Darling
  
# 2. Logar�tmica:
# Obs: precisa excluir valores = 0.
  TR2 <- log(RESP)
  modTR2 = with(dados, aov(TR2 ~ FATOR1*FATOR2))
  # Gr�fico dos res�duos
  plotres(modTR2)
  # Testes
  shapiro.test(modTR2$res) # Shapiro-Wilk
  ad.test(modTR2$res) # Anderson-Darling
  
# 3. Hiperb�lica
  TR3 <- 1/RESP
  modTR3 = with(dados, aov(TR3 ~ FATOR1*FATOR2))
  # Gr�fico dos res�duos
  plotres(modTR3)
  # Testes
  shapiro.test(modTR3$res) # Shapiro-Wilk
  ad.test(modTR3$res) # Anderson-Darling
  
# 4. Box-Cox
  require(MASS)
  # C�lculo
  par(mfrow=c(1, 1))
  bc=boxcox(RESP ~ FATOR1*FATOR2, data=dados, plotit=T)
  lambda.max <- bc$x[which.max(bc$y)]
  lambda.max # Se for pr�ximo de zero, usar logar�tmico (TR2).
  TR4 <- (RESP^(lambda.max)-1)/lambda.max
  modTR4 = with(dados, aov(TR4 ~ FATOR1*FATOR2))
  # Gr�fico dos res�duos
  plotres(modTR4)
  # Testes
  shapiro.test(modTR4$res) # Shapiro-Wilk
  ad.test(modTR4$res) # Anderson-Darling
  
# Digite o n� do TR escolhido dentro de ( ):
  RESP.TR <- 
    (RESP) #troque aqui, por exemplo: (TR2).
  # Com isso, as pr�ximas an�lises ir�o usar os
  # dados transformados!


  
# --------------------------------------------
# 5) TESTE DE HOMOCEDASTICIDADE DAS VARI�NCIAS
# --------------------------------------------
# Isso implica que cada tratamento que est� sendo 
# comparado pelo teste F, deve ter aproximadamente 
# a mesma vari�ncia para que a ANOVA tenha validade.
  
  # Redefini��o de dados:
  dados.TR <- data.frame(FATOR1, FATOR2, RESP.TR)
  attach(dados.TR)
  FATOR1 <- as.factor(FATOR1)
  FATOR2 <- as.factor(FATOR2)
  RESP.TR <- as.numeric(RESP.TR)
  anv <- aov(RESP.TR ~ FATOR1*FATOR2)
  mod.TR = with(dados.TR, anv)
  
  # Teste de Bartlett por fator:
  # Se p-value > 0,05, h� homogeneidade das vari�ncias.
  bartlett.test(mod.TR$res ~ FATOR1) 
  bartlett.test(mod.TR$res ~ FATOR2)
  

  # Boxplot de tratamentos vs res�duos:
  # Se os boxplots forem semelhantes, ent�o h� homocedasticidade.
  par(mfrow=c(1, 1))
  boxplot(mod.TR$res ~ FATOR1)
  boxplot(mod.TR$res ~ FATOR2)
  
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
# 7) ANOVA - an�lise de vari�ncia
# --------------------------------------------

# Tabela ANOVA
# Se Pr(>F) < 0,05, ent�o existe diferen�a
# significativa a 5%.
summary(anv)
                      
# df=graus de liberdade. Pr=probabilidade 
# de ser maior que F tabelado.

# Exportar tabela ANOVA para Excel:
write.csv2(
  as.data.frame(summary(anv)[[1]]), 
  file = 
    "Fatorial - Tabela ANOVA.csv") 


# --------------------------------------------
# 8) TESTE DE COMPARA��O DE M�DIAS
# --------------------------------------------
# Ativar Pacote:
library(ExpDes.pt)

# Ensina como usar o comando:
help(fat2.dic)

#Mostrar a ordem dos fatores
levels(FATOR1)
levels(FATOR2)

# Relat�rio completo do teste espec�fico (defina em mcomp=" ").
# ATEN��O: para dados transformados, n�o use estes valores m�dios!
# Use apenas os resultados do teste (as letras).
# Obs: se n�o tiver letras em Grupos, significa que n�o houve diferen�a.
# Cuidado: nas colunas "Tratamentos", est� por ordem dos fatores.
fat2.dic(FATOR1, 
         FATOR2, 
         RESP.TR, 
         quali=c(TRUE,TRUE), 
         mcomp="sk",
         fac.names=c("Recipiente","Esp�cie"), 
         sigT = 0.05, 
         sigF = 0.05)

# Desativar pacote ExpDes.pt por incompatibilidade:
detach(package:ExpDes.pt, unload = TRUE)




