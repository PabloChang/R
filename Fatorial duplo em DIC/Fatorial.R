# --------------------------------------------
# Fatorial duplo em Delineamento Inteiramente ao Acaso
# Elaborado por: Pablo Chang (03/09/2020)
# https://github.com/PabloChang/R
# --------------------------------------------
# O arquivo de dados e script devem estar numa mesma pasta; 
# Os dados devem ser salvos em ".csv (separado por vírgulas)";
# Não pode haver espaço e acentuação nos títulos, única guia;
# Para rodar os comandos, use Ctrl + Enter em cada linha;
# Ativar/desativar comentários: Ctrl + Shift + C.

# --------------------------------------------
# 1) LEITURA E PREPARAÇÃO DOS DADOS
# --------------------------------------------
# Comando para definir a localização da pasta:
library(rstudioapi) # precisa ter instalado o pacote "rstudioapi"
  current_path =
    rstudioapi::getActiveDocumentContext()$path
  setwd(dirname(current_path))
  print(getwd())

# Troque o nome do arquivo de dados (entre " .csv"):
dados <- read.csv2(
  "ExemploDadosFatorial.csv", 
  header = T)

# Comando para trocar os pontos dos gráficos por vírgula.
# Obs: para digitar no R, o padrão ainda é "."
options(OutDec=",")

# Mostra as 6 primeiras linhas para visualização.
head(dados)

# Filtrar dados por categorias, quando existir:
  # Exemplo: mostrar somente dados da Profundidade 2.
  # (Para usar, exclua o "#" abaixo e modifique)
# require(dplyr)
# dados <- filter(dados, Profundidade==2)

# Troque os nomes das colunas (entre "c(  )"):
  # FATOR1 = c(  ): para primeiro fator;
  # FATOR1 = c(  ): para segundo fator;
  # RESP = c(  ): para variável resposta a ser analisada.
attach(dados) 
dados <- data.frame(FATOR1 = as.character(Recipiente),
                    FATOR2 = as.character(Especie),
                    RESP = c(Altura)
                    )

# Comando para deletar observações com dados vazios:
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

# Anexa os dados na memória do R:
attach(dados) 


# --------------------------------------------
# 2) RESUMO DESCRITIVO 
# --------------------------------------------
# Pacote "agricolae".
# Caso não tenho instalado é só rodar: 
# install.packages("agricolae")
library(agricolae)

# Cálculo dos Quartis
quartil1 <- quantile(RESP, 0.25)
quartil2 <- quantile(RESP, 0.5)
quartil3 <- quantile(RESP, 0.75)

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
# Boxplot geral, com a cruz representando a média:
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
  
# Boxplot para cada nível do FATOR2:
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

# Definição de fatores:
FATOR1 <- as.factor(FATOR1)
FATOR2 <- as.factor(FATOR2)
RESP <- as.numeric(RESP)

# Cálculo dos resíduos:
mod = aov(RESP ~ FATOR1*FATOR2)

# Gráficos de resíduos:
# Para ser normal, o histograma deve ter formato de sino no centro.
# Se o gráfico Normal Q-Q se assemelhar a uma reta crescente,
# então existe normalidade.
plotres(mod)

# Testes:
# Se p-value > 0,05, os resíduos possuem distribuição normal.
  shapiro.test(mod$res) # Shapiro-Wilk
  ad.test(mod$res) # Anderson-Darling

# Os resíduos são normais?
  # Se SIM, rode o comando abaixo e pule para a etapa 5).
    RESP.TR <- RESP
  # Se NÃO, faça a transformação em 4.1).


# --------------------------------------------
# 4.1) TRANSFORMAÇÃO DE DADOS NÃO-NORMAIS
# --------------------------------------------
# Faça os testes, até atingir a normalidade!
  
# TR1. Raiz quadrada:
  TR1 <- sqrt(RESP)
  modTR1 = aov(TR1 ~ FATOR1*FATOR2)
  # Gráfico dos resíduos
  plotres(modTR1)
  # Testes
  shapiro.test(modTR1$res) # Shapiro-Wilk
  ad.test(modTR1$res) # Anderson-Darling
  
# TR2. Logarítmica:
# Obs: precisa excluir valores = 0.
  TR2 <- log(RESP)
  modTR2 = aov(TR2 ~ FATOR1*FATOR2)
  # Gráfico dos resíduos
  plotres(modTR2)
  # Testes
  shapiro.test(modTR2$res) # Shapiro-Wilk
  ad.test(modTR2$res) # Anderson-Darling
  
# TR3. Hiperbólica
  TR3 <- 1/RESP
  modTR3 = aov(TR3 ~ FATOR1*FATOR2)
  # Gráfico dos resíduos
  plotres(modTR3)
  # Testes
  shapiro.test(modTR3$res) # Shapiro-Wilk
  ad.test(modTR3$res) # Anderson-Darling
  
# TR4. Box-Cox
  require(MASS)
  # Cálculo
  par(mfrow=c(1, 1))
  bc=boxcox(RESP ~ FATOR1*FATOR2, data=dados, plotit=T)
  lambda.max <- bc$x[which.max(bc$y)]
  lambda.max # Se for próximo de zero, usar logarítmico (TR2).
  TR4 <- (RESP^(lambda.max)-1)/lambda.max
  modTR4 = aov(TR4 ~ FATOR1*FATOR2)
  # Gráfico dos resíduos
  plotres(modTR4)
  # Testes
  shapiro.test(modTR4$res) # Shapiro-Wilk
  ad.test(modTR4$res) # Anderson-Darling
  
# Digite o nº do TR escolhido dentro de ( ):
  RESP.TR <- 
    (RESP) #troque aqui, por exemplo: (TR2).
  # Com isso, as próximas análises irão usar os
  # dados transformados!

  
# --------------------------------------------
# 5) TESTE DE HOMOCEDASTICIDADE DAS VARIÂNCIAS
# --------------------------------------------
# Isso implica que cada tratamento que está sendo 
# comparado pelo teste F, deve ter aproximadamente 
# a mesma variância para que a ANOVA tenha validade.
  
  # Redefinição de dados:
  dados.TR <- data.frame(FATOR1, FATOR2, RESP.TR)
  attach(dados.TR)
  FATOR1 <- as.factor(FATOR1)
  FATOR2 <- as.factor(FATOR2)
  RESP.TR <- as.numeric(RESP.TR)
  mod.TR <- aov(RESP.TR ~ FATOR1*FATOR2)

  # Teste de Bartlett por fator:
  # Se p-value > 0,05, há homogeneidade das variâncias.
  bartlett.test(mod.TR$res ~ FATOR1) 
  bartlett.test(mod.TR$res ~ FATOR2)
  

  # Boxplot de tratamentos vs resíduos:
  # Se os boxplots forem semelhantes, então há homocedasticidade.
  par(mfrow=c(1, 1))
  boxplot(mod.TR$res ~ FATOR1)
  boxplot(mod.TR$res ~ FATOR2)
  
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
# 7) ANOVA - análise de variância
# --------------------------------------------

# Tabela ANOVA
# Se Pr(>F) < 0,05, então existe diferença
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
# 8) TESTE DE COMPARAÇÃO DE MÉDIAS
# --------------------------------------------
# Ativar Pacote:
library(ExpDes.pt)

# Ensina como usar o comando:
help(fat2.dic)

# Mostrar a ordem* dos níveis de fatores:
as.data.frame(levels(FATOR1))
as.data.frame(levels(FATOR2))

# Relatório completo do teste específico (defina em mcomp=" ").
# ATENÇÃO: 
# - Para dados transformados, não use estes valores médios!
# - Use apenas os resultados do teste (as letras);
# - Se for TR3, as letras são de MENOR para MAIOR;
# - As colunas "Tratamentos" está por ordem* numérica;
# - Pode copiar este relatório para Word, depois copiar
#   apenas os resultados para Excel, e classificar em ordem.
fat2.dic(FATOR1, 
         FATOR2, 
         RESP.TR, 
         quali=c(TRUE,TRUE), 
         mcomp="tukey",
         fac.names=c("Recipiente","Espécie"), 
         sigT = 0.05, 
         sigF = 0.05)

# Desativar pacote ExpDes.pt por incompatibilidade:
detach(package:ExpDes.pt, unload = TRUE)


# --------------------------------------------
# 9) MÉDIAS POR NÍVEL DE FATOR (EM ESCALA ORIGINAL)
# --------------------------------------------
# Mostra as médias específicas de acordo com 
# cada nível de fator, em escala original.

# FATOR2 - médias de cada nível:
  # Digite, entre (), o nível desejado para FATOR2:
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
               "Fatorial - Médias por nível de FATOR2.csv") 

# FATOR2 - médias gerais:
  mediageralF2 <- as.data.frame(
    tapply(
      dados$RESP, 
      dados$FATOR2, 
      mean)
  )
  mediageralF2
  # Exportar para Excel:
  write.csv2(mediageralF2, file = 
               "Fatorial - Médias gerais por FATOR2.csv") 

# FATOR1 - médias gerais:
  mediageralF1 <- as.data.frame(
    tapply(
      dados$RESP, 
      dados$FATOR1, 
      mean)
  )
  mediageralF1
  # Exportar para Excel:
  write.csv2(mediageralF1, file = 
               "Fatorial - Médias gerais por FATOR1.csv") 


# --------------------------------------------
# 10) COEFICIENTE DE VARIAÇÃO (CV) POR FATOR
# --------------------------------------------
# Calcula os valores de CV (%) para cada nível
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
# 11) DMS - Diferença Mínima Significativa do teste Tukey
# --------------------------------------------
# Valor que retrata a diferença mínima para que duas
# médias tenham diferença significativa a 5%.
# Cálculo do DMS:
t.HSD <- TukeyHSD(mod.TR, ordered=TRUE)

# Deve-se modificar manualmente os comandos abaixo
# sobre comparação entre duas médias que tem o formato:
# "FATOR1:FATOR2-FATOR1:FATOR2".
# É fixado um dos níveis e comparado as 2 primeiras médias.
# Ex¹: "r1:e1-r2:e1" que compara duas médias dentro de e1 (fixo);
# Ex²: "r3:e1-r3:e2" que compara duas médias dentro de r3 (fixo);
# Quando rodar os comandos, é normal que uma das duplas
# apareça um erro. Apenas ignore.

#FATOR2: Espécie
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
                      "Espécie", 
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
# Se NÃO, apenas rode o comando abaixo:
  dms
  # Exportar a tabela para Excel:
  write.csv2(dms, file = "Fatorial - DMS.csv")

# Se SIM, rode o comando abaixo:
  LimSup <- mean(RESP.TR)
  LimInf <- LimSup-dms$DMS
  # E rode apenas a opção que foi usada para realizar
  # a transformação inversa:

# TR1. Raiz quadrada:
  dms.sqrt <- (LimSup)^2-(LimInf)^2
  dms$DMS <- dms.sqrt
  dms
  # Exportar a tabela para Excel:
  write.csv2(dms, file = "Fatorial - DMS.csv")

# TR2. Logarítmica:
  dms.log <- exp(LimSup)-exp(LimInf)
  dms$DMS <- dms.log
  dms
  # Exportar a tabela para Excel:
  write.csv2(dms, file = "Fatorial - DMS.csv")

# TR3. Recíproca:
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

  