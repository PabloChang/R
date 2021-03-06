# --------------------------------------------
# Fatorial duplo em Delineamento Inteiramente ao Acaso
# Elaborado por: Pablo Chang (03/09/2020)
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

# Filtrar dados por categorias, quando existir:
  # Exemplo: mostrar somente dados da Profundidade 2.
  # (Para usar, exclua o "#" abaixo e modifique)
# require(dplyr)
# dados <- filter(dados, Profundidade==2)

# Troque os nomes das colunas (entre "c(  )"):
  # FATOR1 = c(  ): para primeiro fator;
  # FATOR1 = c(  ): para segundo fator;
  # RESP = c(  ): para vari�vel resposta a ser analisada.
attach(dados) 
dados <- data.frame(FATOR1 = as.character(Recipiente),
                    FATOR2 = as.character(Especie),
                    RESP = c(Altura)
                    )

# Comando para deletar observa��es com dados vazios:
dados <- na.omit(dados)

# Deletar valores negativos ou iguais a zero:
if(min(dados$RESP)<= 0){
  r <- with(dados, which(RESP<=0, arr.ind=TRUE))
  dados <- dados[-r, ]
  print("Deletado valores nulos ou negativos.")
}else{
  print("N�o possui valores nulos ou negativos.")
}

# Mostra as 6 primeiras linhas para ver como ficou.
head(dados)

# Deletar valores condicionais
  # RESP < x: deletar valores abaixo de x.
  # RESP > x: deletar valores acima de x.
  # RESP == x: deletar valores iguais a x.
  # Para usar, exclua o "#" nas duas linhas e modifique:
# r <- with(dados, which(RESP==0, arr.ind=TRUE))
# dados <- dados[-r, ]

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
  points(tapply(RESP,FATOR1,mean),col="red",pch=3)
  
# Boxplot para cada n�vel do FATOR2:
  boxplot(RESP ~ FATOR2,
          col="green",
          outcol="blue",
          xlab="Especie",
          ylab="Altura da planta (cm)")
  points(tapply(RESP,FATOR2,mean),col="red",pch=3)

    
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
mod = aov(RESP ~ FATOR1*FATOR2)

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
  
# TR1. Raiz quadrada:
  TR1 <- sqrt(RESP)
  modTR1 = aov(TR1 ~ FATOR1*FATOR2)
  # Gr�fico dos res�duos
  plotres(modTR1)
  # Testes
  shapiro.test(modTR1$res) # Shapiro-Wilk
  ad.test(modTR1$res) # Anderson-Darling
  
# TR2. Logar�tmica:
# Obs: precisa excluir valores = 0.
  TR2 <- log(RESP)
  modTR2 = aov(TR2 ~ FATOR1*FATOR2)
  # Gr�fico dos res�duos
  plotres(modTR2)
  # Testes
  shapiro.test(modTR2$res) # Shapiro-Wilk
  ad.test(modTR2$res) # Anderson-Darling
  
# TR3. Hiperb�lica
  TR3 <- 1/RESP
  modTR3 = aov(TR3 ~ FATOR1*FATOR2)
  # Gr�fico dos res�duos
  plotres(modTR3)
  # Testes
  shapiro.test(modTR3$res) # Shapiro-Wilk
  ad.test(modTR3$res) # Anderson-Darling
  
# TR4. Box-Cox
  require(MASS)
  # C�lculo
  par(mfrow=c(1, 1))
  bc=boxcox(RESP ~ FATOR1*FATOR2, data=dados, plotit=T)
  lambda.max <- bc$x[which.max(bc$y)]
  lambda.max # Se for pr�ximo de zero, usar logar�tmico (TR2).
  TR4 <- (RESP^(lambda.max)-1)/lambda.max
  modTR4 = aov(TR4 ~ FATOR1*FATOR2)
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
  mod.TR <- aov(RESP.TR ~ FATOR1*FATOR2)

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
summary(mod.TR)
                      
# df=graus de liberdade. Pr=probabilidade 
# de ser maior que F tabelado.

# Exportar tabela ANOVA para Excel:
write.csv2(
  as.data.frame(summary(mod.TR)[[1]]), 
  file = 
    "Fatorial - Tabela ANOVA.csv") 


# --------------------------------------------
# 8) TESTE DE COMPARA��O DE M�DIAS
# --------------------------------------------
# Ativar Pacote:
library(ExpDes.pt)

# Ensina como usar o comando:
help(fat2.dic)

# Mostrar a ordem* dos n�veis de fatores:
as.data.frame(levels(FATOR1))
as.data.frame(levels(FATOR2))

# Relat�rio completo do teste espec�fico (defina em mcomp=" ").
# ATEN��O: 
# - Para dados transformados, n�o use estes valores m�dios!
# - Use apenas os resultados do teste (as letras);
# - Se for TR3, as letras s�o de MENOR para MAIOR;
# - As colunas "Tratamentos" est� por ordem* num�rica;
# - Pode copiar este relat�rio para Word, depois copiar
#   apenas os resultados para Excel, e classificar em ordem.
fat2.dic(FATOR1, 
         FATOR2, 
         RESP.TR, 
         quali=c(TRUE,TRUE), 
         mcomp="tukey",
         fac.names=c("Recipiente","Esp�cie"), 
         sigT = 0.05, 
         sigF = 0.05)

# Desativar pacote ExpDes.pt por incompatibilidade:
detach(package:ExpDes.pt, unload = TRUE)


# --------------------------------------------
# 9) M�DIAS POR N�VEL DE FATOR (EM ESCALA ORIGINAL)
# --------------------------------------------
# Mostra as m�dias espec�ficas de acordo com 
# cada n�vel de fator, em escala original.

# FATOR2 - m�dias de cada n�vel:
  # Digite, entre (), o n�vel desejado para FATOR2:
  nivel <- ("e1")
  require(dplyr)  
  fac <- filter(dados, FATOR2==nivel) 
  medias <- as.data.frame(
    tapply(
      fac$RESP, 
      fac$FATOR1, mean)) 
  medias
  # Exportar a tabela para Excel:
  write.csv2(medias, file = 
               "Fatorial - M�dias por n�vel de FATOR2.csv") 

# FATOR2 - m�dias gerais:
  mediageralF2 <- as.data.frame(
    tapply(
      dados$RESP, 
      dados$FATOR2, 
      mean)
  )
  mediageralF2
  # Exportar para Excel:
  write.csv2(mediageralF2, file = 
               "Fatorial - M�dias gerais por FATOR2.csv") 

# FATOR1 - m�dias gerais:
  mediageralF1 <- as.data.frame(
    tapply(
      dados$RESP, 
      dados$FATOR1, 
      mean)
  )
  mediageralF1
  # Exportar para Excel:
  write.csv2(mediageralF1, file = 
               "Fatorial - M�dias gerais por FATOR1.csv") 


# --------------------------------------------
# 10) COEFICIENTE DE VARIA��O (CV) POR FATOR
# --------------------------------------------
# Calcula os valores de CV (%) para cada n�vel
# de fator correspondente.
  
# FATOR2:
  sdF2 <-as.data.frame(tapply(
    dados$RESP, dados$FATOR2, sd))
  desviopadraoF2 <- sdF2/mediageralF2*100
  desviopadraoF2
  # Exportar a tabela para Excel:
  write.csv2(desviopadraoF2, file = 
               "Fatorial - CV de FATOR2.csv")
  
# FATOR1:
  sdF1 <-as.data.frame(tapply(
    dados$RESP, dados$FATOR1, sd))
  desviopadraoF1 <- sdF1/mediageralF1*100
  desviopadraoF1
  # Exportar a tabela para Excel:
  write.csv2(desviopadraoF1, file = 
               "Fatorial - CV de FATOR1.csv")
  
  
# --------------------------------------------
# 11) DMS - Diferen�a M�nima Significativa do teste Tukey
# --------------------------------------------
# Valor que retrata a diferen�a m�nima para que duas
# m�dias tenham diferen�a significativa a 5%.
# C�lculo do DMS:
t.HSD <- TukeyHSD(mod.TR, ordered=TRUE)

# Deve-se modificar manualmente os comandos abaixo
# sobre compara��o entre duas m�dias que tem o formato:
# "FATOR1:FATOR2-FATOR1:FATOR2".
# � fixado um dos n�veis e comparado as 2 primeiras m�dias.
# Ex�: "r1:e1-r2:e1" que compara duas primeiras m�dias dentro de e1 (fixo);
# Ex�: "r3:e1-r3:e2" que compara duas primeiras m�dias dentro de r3 (fixo);
# Quando rodar os comandos, � normal que uma das duplas
# apare�a um erro. Apenas ignore.

#FATOR2: Esp�cie
  # e1
  dF2_1 <- unname(0.5*diff(t.HSD$'FATOR1:FATOR2'["r1:e1-r2:e1", 2:3]))
  dF2_1 <- unname(0.5*diff(t.HSD$'FATOR1:FATOR2'["r2:e1-r1:e1", 2:3]))
  # e2
  dF2_2 <- unname(0.5*diff(t.HSD$'FATOR1:FATOR2'["r1:e2-r2:e2", 2:3]))
  dF2_2 <- unname(0.5*diff(t.HSD$'FATOR1:FATOR2'["r2:e2-r1:e2", 2:3]))

#FATOR1: Recipiente
  # r1
  dF1_1 <- unname(0.5*diff(t.HSD$'FATOR1:FATOR2'["r1:e1-r1:e2", 2:3]))
  dF1_1 <- unname(0.5*diff(t.HSD$'FATOR1:FATOR2'["r1:e2-r1:e1", 2:3]))
  # r2
  dF1_2 <- unname(0.5*diff(t.HSD$'FATOR1:FATOR2'["r2:e1-r2:e2", 2:3]))
  dF1_2 <- unname(0.5*diff(t.HSD$'FATOR1:FATOR2'["r2:e2-r2:e1", 2:3]))
  # r3
  dF1_3 <- unname(0.5*diff(t.HSD$'FATOR1:FATOR2'["r3:e1-r3:e2", 2:3]))
  dF1_3 <- unname(0.5*diff(t.HSD$'FATOR1:FATOR2'["r3:e2-r3:e1", 2:3]))

# Gerar tabela de dms:
dms <- data.frame(
        Tratamentos = c(
                      "Esp�cie", 
                      "e1",
                      "e2",
                      "Recipiente",
                      "r1",
                      "r2",
                      "r3"),
                DMS = c(
                      NA,  # FATOR2
                      dF1_1, 
                      dF1_2, 
                      NA,  # FATOR1
                      dF1_1,
                      dF1_2,
                      dF1_3),
                stringsAsFactors = FALSE)

# Os dados foram transformados em 4.1)?
# Se N�O, apenas rode o comando abaixo:
  dms
  # Exportar a tabela para Excel:
  write.csv2(dms, file = "Fatorial - DMS.csv")

# Se SIM, rode o comando abaixo:
  LimSup <- mean(RESP.TR)
  LimInf <- LimSup-dms$DMS
  # E rode apenas a op��o que foi usada para realizar
  # a transforma��o inversa:

# TR1. Raiz quadrada:
  dms.sqrt <- (LimSup)^2-(LimInf)^2
  dms$DMS <- dms.sqrt
  dms
  # Exportar a tabela para Excel:
  write.csv2(dms, file = "Fatorial - DMS.csv")

# TR2. Logar�tmica:
  dms.log <- exp(LimSup)-exp(LimInf)
  dms$DMS <- dms.log
  dms
  # Exportar a tabela para Excel:
  write.csv2(dms, file = "Fatorial - DMS.csv")

# TR3. Rec�proca:
  dms.hip <- (LimSup)^(-1)-(LimInf)^(-1)
  dms$DMS <- abs(dms.hip) 
  dms
  # Exportar a tabela para Excel:
  write.csv2(dms, file = "Fatorial - DMS.csv")

# TR4. Box-Cox:
  dms.bc <- ((LimSup*lambda.max)+1)^(1/lambda.max) -
    ((LimInf*lambda.max)+1)^(1/lambda.max)
  dms$DMS <- dms.bc 
  dms
  # Exportar a tabela para Excel:
  write.csv2(dms, file = "Fatorial - DMS.csv")

  