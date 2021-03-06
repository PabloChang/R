# --------------------------------------------
# Delineamento em Blocos ao Acaso
# Elaborado por: Pablo Chang (28/11/2020)
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
  "DadosExemploDBC.csv", 
  header = T)

# Comando para trocar os pontos dos gr�ficos por v�rgula.
# Obs: para digitar comando no R, o padr�o ainda � "."
options(OutDec=",")

# Mostra as 6 primeiras linhas para visualiza��o.
head(dados)

# Filtrar dados por categorias, quando existir:
# Exemplo: mostrar somente dados da Profundidade 2.
# (Para usar, exclua o "#" abaixo e modifique)
# require(dplyr)
# dados <- filter(dados, Profundidade==2)

# Troque os nomes das colunas (entre "c(  )"):
# TRAT = c(  ): para tratamento;
# BLOCO = c(  ): para bloco;
# RESP = c(  ): para vari�vel resposta a ser analisada.
attach(dados) 
dados <- data.frame(TRAT = as.character(Densidade),
                    BLOCO = as.character(Genotipo),
                    RESP = c(MassaFrescaDaRaiz)
                    )

# Mostra as 6 primeiras linhas para ver como ficou.
head(dados)

# Deletar observa��es com dados vazios:
dados <- na.omit(dados)

# Deletar valores negativos ou iguais a zero:
if(min(dados$RESP)<= 0){
  r <- with(dados, which(RESP<=0, arr.ind=TRUE))
  dados <- dados[-r, ]
  print("Deletado valores nulos ou negativos.")
}else{
  print("N�o possui valores nulos ou negativos.")
}

# Deletar valores condicionais:
  # RESP < x: deletar valores abaixo de x.
  # RESP > x: deletar valores acima de x.
  # RESP == x: deletar valores iguais a x.
  # Para usar, exclua o "#" nas duas linhas e modifique:
# r <- with(dados, which(RESP<0, arr.ind=TRUE))
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
           file = "DBC - Resumo Descritivo.csv") 


# --------------------------------------------
# 3) BOXPLOT 
# --------------------------------------------
# Boxplot geral, com a cruz representando a m�dia.
# Altere as cores e legendas entre aspas " ".
  require(graphics)
  boxplot(RESP,
          col="yellow",
          outcol="blue",
          ylab="Massa fresca da raiz (g)")
  points(mean(RESP),col="red",pch=3)
  
  # Boxplot para cada tratamento
  boxplot(RESP ~ TRAT,
          col="skyblue1",
          outcol="blue",
          xlab="Densidade (g/cm�)",
          ylab="Massa fresca da raiz (g)")
  points(tapply(RESP,TRAT,mean),col="red",pch=3)
  
  # Boxplot para cada bloco
  boxplot(RESP ~ BLOCO,
          col="green",
          outcol="blue",
          xlab="Gen�tipo", 
          ylab="Massa fresca da raiz (g)")
  points(tapply(RESP,BLOCO,mean),col="red",pch=3)

    
# --------------------------------------------
# 4) TESTE DE NORMALIDADE
# --------------------------------------------
  # Pacote com alguns testes:
  library(nortest)
  library(ExpDes)
  
  # Defini��o de fatores:
  TRAT <- as.factor(TRAT)
  BLOCO <- as.factor(BLOCO)
  RESP <- as.numeric(RESP)
  
  # C�lculo dos res�duos:
  mod = aov(RESP ~ TRAT + BLOCO)

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
    RESP.TR <- RESP
  # Se N�O, fa�a a transforma��o em 4.1).


# --------------------------------------------
# 4.1) TRANSFORMA��O DE DADOS N�O-NORMAIS
# --------------------------------------------
# Fa�a os testes, at� atingir a normalidade!
    
# TR1. Raiz quadrada:
    TR1 <- sqrt(RESP)
    modTR1 = aov(TR1 ~ TRAT + BLOCO)
    # Gr�fico dos res�duos
    plotres(modTR1)
    # Testes
    shapiro.test(modTR1$res) # Shapiro-Wilk
    ad.test(modTR1$res) # Anderson-Darling
    
# TR2. Logar�tmica:
# Obs: precisa excluir valores = 0.
    TR2 <- log(RESP)
    modTR2 = aov(TR2 ~ TRAT + BLOCO)
    # Gr�fico dos res�duos
    plotres(modTR2)
    # Testes
    shapiro.test(modTR2$res) # Shapiro-Wilk
    ad.test(modTR2$res) # Anderson-Darling
    
# TR3. Hiperb�lica
    TR3 <- 1/RESP
    modTR3 = aov(TR3 ~ TRAT + BLOCO)
    # Gr�fico dos res�duos
    plotres(modTR3)
    # Testes
    shapiro.test(modTR3$res) # Shapiro-Wilk
    ad.test(modTR3$res) # Anderson-Darling
    
# TR4. Box-Cox
    require(MASS)
    # C�lculo
    par(mfrow=c(1, 1))
    bc=boxcox(RESP ~ TRAT, data=dados, plotit=T)
    lambda.max <- bc$x[which.max(bc$y)]
    lambda.max # Se for pr�ximo de zero, usar logar�tmico (TR2).
    TR4 <- (RESP^(lambda.max)-1)/lambda.max
    modTR4 = aov(TR4 ~ TRAT + BLOCO)
    # Gr�fico dos res�duos
    plotres(modTR4)
    # Testes
    shapiro.test(modTR4$res) # Shapiro-Wilk
    ad.test(modTR4$res) # Anderson-Darling
    
# Digite o TR escolhido dentro de ( ):
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
    dados.TR <- data.frame(TRAT, BLOCO, RESP.TR)
    attach(dados.TR)
    TRAT <- as.factor(TRAT)
    BLOCO <- as.factor(BLOCO)
    RESP.TR <- as.numeric(RESP.TR)
    mod.TR <- aov(RESP.TR ~ TRAT + BLOCO)
   
    # Teste de Bartlett:
    # Se p-value > 0,05, h� homogeneidade das vari�ncias.
    bartlett.test(mod.TR$res ~ TRAT)
    bartlett.test(mod.TR$res ~ BLOCO)
    
    # Boxplot de tratamentos vs res�duos:
    # Se os boxplots forem semelhantes, h� homocedasticidade.
    par(mfrow=c(1, 1))
    boxplot(mod.TR$res ~ TRAT)  
    boxplot(mod.TR$res ~ BLOCO)  

  
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
    "DBC - Tabela ANOVA.csv") 


# --------------------------------------------
# 8) TESTE DE COMPARA��O DE M�DIAS
# --------------------------------------------
# Teste Tukey, SNL (Student-Newman-Keuls), Duncan, t e 
# Scott-Knott a 5% de signific�ncia.
require(easyanova)
require(dplyr)
# TabelaS usando dados transformados (quando for o caso),
# mas mostrando m�dias originais:

# Por tratamento:
  par(mfrow=c(1, 1))
  ttuk <- easyanova::ea1(dados.TR, design=2)
  teste <- arrange(ttuk$`Adjusted means`, ttuk$`Adjusted means`$treatment)
  ttuk.o <- easyanova::ea1(dados, design=2)
  teste.o <- arrange(ttuk.o$`Adjusted means`, ttuk.o$`Adjusted means`$treatment)
  teste$adjusted.mean <- teste.o$adjusted.mean
  colnames(teste) <- c(
    "Tratamentos", 
    "M�dias",
    "Erro padr�o",
    "Tukey",
    "SNK",
    "Duncan",
    "t",
    "Scott-Knott"
  ) 
  teste

# Exportar para Excel:
write.csv2(teste, file = 
             "DBC - Testes de Compara��o de M�dias.csv")


# --------------------------------------------
# 9) DMS - Diferen�a M�nima Significativa do teste Tukey
# --------------------------------------------
# Valor que retrata a diferen�a m�nima para que duas
# m�dias tenham diferen�a significativa a 5%.

  # C�lculo do DMS:
  t.HSD <- TukeyHSD(mod.TR, ordered=TRUE)
  dms <- unname(0.5*diff(t.HSD$TRAT[1, 2:3]))
  LimSup <- mean(RESP.TR) 
  LimInf <- LimSup-dms  
  
  # Os dados foram transformados em 4.1)?
  # Se N�O, apenas rode o comando abaixo:
    dms
  
  # Se SIM, escolha o TR usado para
  # realizar a transforma��o inversa:
    # TR1. Raiz quadrada:
    dms.sqrt <- (LimSup)^2-(LimInf)^2 
    dms.sqrt
    
    # TR2. Logar�tmica:
    dms.log <- exp(LimSup)-exp(LimInf) 
    dms.log
    
    # TR3. Hiperb�lica:
    dms.hip <- (LimSup)^(-1)-(LimInf)^(-1)
    dms.hip
    
    # TR4. Box-Cox:
    dms.bc <- ((LimSup*lambda.max)+1)^(1/lambda.max) -
              ((LimInf*lambda.max)+1)^(1/lambda.max)
    dms.bc

    
# --------------------------------------------
# 10) Gr�fico de barras de Tukey
# --------------------------------------------
my_bar <- barplot(teste$M�dias,
                  ylim=c(0, 1.3*max(teste$M�dias)),
                  beside=T,  
                  col="seagreen3",
                  names.arg = teste$Tratamentos,
                  xlab="Densidade (g/cm�)",
                  ylab="Massa fresca da raiz (g)")

# Letras do Tukey acima das barras:
text(my_bar, 
     teste$M�dia+0.1*max(teste$M�dias),
     teste$Tukey, cex=1) 


