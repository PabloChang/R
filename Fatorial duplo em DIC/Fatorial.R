# --------------------------------------------
# Fatorial duplo em Delineamento Inteiramente ao Acaso
# Elaborado por: Pablo Chang (21/04/2020)
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
# Comando para definir a localização da pasta:
# Caso não tenho instalado o pacote é só rodar (sem #):
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

# Comando para trocar os pontos dos gráficos por vírgula.
# Obs: para digitar no R, o padrão ainda é "."
options(OutDec=",")

# Mostra as 6 primeiras linhas para visualização.
head(dados)

# Troque os nomes das colunas (entre "c(  )"):
# FATOR1 = c(  ): para TRATamento;
# RESP = c(  ): para variável resposta a ser analisada.
attach(dados) 
dados <- data.frame(FATOR1 = c(Recipiente),
                    FATOR2 = c(Especie),
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
  points(tapply(RESP,TRAT,mean),col="red",pch=3)
  
# Boxplot para cada nível do FATOR2:
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

# Definição de fatores:
FATOR1 <- as.factor(FATOR1)
FATOR2 <- as.factor(FATOR2)
RESP <- as.numeric(RESP)

# Cálculo dos resíduos:
mod = with(dados, aov(RESP ~ FATOR1*FATOR2))

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
  
# 1. Raiz quadrada:
  TR1 <- (RESP)^2
  modTR1 = with(dados, aov(TR1 ~ FATOR1*FATOR2))
  # Gráfico dos resíduos
  plotres(modTR1)
  # Testes
  shapiro.test(modTR1$res) # Shapiro-Wilk
  ad.test(modTR1$res) # Anderson-Darling
  
# 2. Logarítmica:
# Obs: precisa excluir valores = 0.
  TR2 <- log(RESP)
  modTR2 = with(dados, aov(TR2 ~ FATOR1*FATOR2))
  # Gráfico dos resíduos
  plotres(modTR2)
  # Testes
  shapiro.test(modTR2$res) # Shapiro-Wilk
  ad.test(modTR2$res) # Anderson-Darling
  
# 3. Hiperbólica
  TR3 <- 1/RESP
  modTR3 = with(dados, aov(TR3 ~ FATOR1*FATOR2))
  # Gráfico dos resíduos
  plotres(modTR3)
  # Testes
  shapiro.test(modTR3$res) # Shapiro-Wilk
  ad.test(modTR3$res) # Anderson-Darling
  
# 4. Box-Cox
  require(MASS)
  # Cálculo
  par(mfrow=c(1, 1))
  bc=boxcox(RESP ~ FATOR1*FATOR2, data=dados, plotit=T)
  lambda.max <- bc$x[which.max(bc$y)]
  lambda.max # Se for próximo de zero, usar logarítmico (TR2).
  TR4 <- (RESP^(lambda.max)-1)/lambda.max
  modTR4 = with(dados, aov(TR4 ~ FATOR1*FATOR2))
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
  anv <- aov(RESP.TR ~ FATOR1*FATOR2)
  mod.TR = with(dados.TR, anv)
  
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
summary(anv)
                      
# df=graus de liberdade. Pr=probabilidade 
# de ser maior que F tabelado.

# Exportar tabela ANOVA para Excel:
write.csv2(
  as.data.frame(summary(anv)[[1]]), 
  file = 
    "Fatorial - Tabela ANOVA.csv") 


# --------------------------------------------
# 8) TESTE DE COMPARAÇÃO DE MÉDIAS
# --------------------------------------------
# Ativar Pacote:
library(ExpDes.pt)

# Ensina como usar o comando:
help(fat2.dic)

#Mostrar a ordem dos fatores
levels(FATOR1)
levels(FATOR2)

# Relatório completo do teste específico (defina em mcomp=" ").
# ATENÇÃO: para dados transformados, não use estes valores médios!
# Use apenas os resultados do teste (as letras).
# Obs: se não tiver letras em Grupos, significa que não houve diferença.
# Cuidado: nas colunas "Tratamentos", está por ordem dos fatores.
fat2.dic(FATOR1, 
         FATOR2, 
         RESP.TR, 
         quali=c(TRUE,TRUE), 
         mcomp="sk",
         fac.names=c("Recipiente","Espécie"), 
         sigT = 0.05, 
         sigF = 0.05)

# Desativar pacote ExpDes.pt por incompatibilidade:
detach(package:ExpDes.pt, unload = TRUE)





