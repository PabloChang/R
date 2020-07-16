# --------------------------------------------
# Delineamento Inteiramente ao Acaso
# Elaborado por: Pablo Chang (15/07/2020)
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
# Caso não tenho instalado o pacote é só rodar (sem #):
# install.packages("rstudioapi")
library(rstudioapi)
  current_path =
    rstudioapi::getActiveDocumentContext()$path
  setwd(dirname(current_path))
  print(getwd())

# Troque o nome do arquivo de dados (entre " .csv"):
dados <- read.csv2(
  "DadosExemploDIC.csv", 
  header = T)

# Comando para trocar os pontos dos gráficos por vírgula.
# Obs: para digitar comando no R, o padrão ainda é "."
options(OutDec=",")

# Mostra as 6 primeiras linhas para visualização.
head(dados)

# Filtrar dados por categorias, quando existir:
# Exemplo: mostrar somente dados da Profundidade 2.
# (Para usar, exclua o "#" abaixo e modifique)
# require(dplyr)
# dados <- filter(dados, Profundidade==2)

# Troque os nomes das colunas (entre "c(  )"):
# TRAT = c(  ): para tratamento;
# RESP = c(  ): para variável resposta a ser analisada.
attach(dados) 
dados <- data.frame(TRAT = as.character(Densidade), 
                    RESP = c(MassaFrescaDaRaiz)
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
# r <- with(dados, which(RESP<0, arr.ind=TRUE))
#  dados <- dados[-r, ]
# r <- with(dados, which(RESP==0, arr.ind=TRUE))
#  dados <- dados[-r, ]

# Anexar os dados na memória do R:
attach(dados) 
dados
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
           file = "DIC - Resumo Descritivo.csv") 


# --------------------------------------------
# 3) BOXPLOT 
# --------------------------------------------
# Boxplot geral, com a cruz representando a média.
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
      xlab="Densidade (g/cm³)",
      ylab="Massa fresca da raiz (g)")
  points(tapply(RESP,TRAT,mean),col="red",pch=3)

    
# --------------------------------------------
# 4) TESTE DE NORMALIDADE
# --------------------------------------------
# Pacote com alguns testes:
library(nortest)
library(ExpDes)
    
# Definição de fatores:
TRAT <- as.factor(TRAT)
RESP <- as.numeric(RESP)
  
# Cálculo dos resíduos:
mod = aov(RESP ~ TRAT)

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
  modTR1 = aov(TR1 ~ TRAT)
  # Gráfico dos resíduos
  plotres(modTR1)
  # Testes
  shapiro.test(modTR1$res) # Shapiro-Wilk
  ad.test(modTR1$res) # Anderson-Darling
  
# TR2. Logarítmica:
# Obs: precisa excluir valores = 0.
  TR2 <- log(RESP)
  modTR2 = aov(TR2 ~ TRAT)
  # Gráfico dos resíduos
  plotres(modTR2)
  # Testes
  shapiro.test(modTR2$res) # Shapiro-Wilk
  ad.test(modTR2$res) # Anderson-Darling
  
# TR3. Hiperbólica
  TR3 <- 1/RESP
  modTR3 = aov(TR3 ~ TRAT)
  # Gráfico dos resíduos
  plotres(modTR3)
  # Testes
  shapiro.test(modTR3$res) # Shapiro-Wilk
  ad.test(modTR3$res) # Anderson-Darling
  
# TR4. Box-Cox
  require(MASS)
  # Cálculo
  par(mfrow=c(1, 1))
  bc=boxcox(RESP ~ TRAT, data=dados, plotit=T)
  lambda.max <- bc$x[which.max(bc$y)]
  lambda.max # Se for próximo de zero, usar logarítmico (TR2).
  TR4 <- (RESP^(lambda.max)-1)/lambda.max
  modTR4 = aov(TR4 ~ TRAT)
  # Gráfico dos resíduos
  plotres(modTR4)
  # Testes
  shapiro.test(modTR4$res) # Shapiro-Wilk
  ad.test(modTR4$res) # Anderson-Darling
  
# Digite o TR escolhido dentro de ( ):
  RESP.TR <- 
    (TR2) #troque aqui, por exemplo: (TR2).
  # Com isso, as próximas análises irão usar os
  # dados transformados!
  

# --------------------------------------------
# 5) TESTE DE HOMOCEDASTICIDADE DAS VARIÂNCIAS
# --------------------------------------------
# Isso implica que cada tratamento que está sendo 
# comparado pelo teste F, deve ter aproximadamente 
# a mesma variância para que a ANOVA tenha validade.
  
  # Redefinição de dados:
  dados.TR <- data.frame(TRAT, RESP.TR)
  attach(dados.TR)
  TRAT <- as.factor(TRAT)
  RESP.TR <- as.numeric(RESP.TR)
  mod.TR <- aov(RESP.TR ~ TRAT)
   
  # Teste de Bartlett:
  # Se p-value > 0,05, há homogeneidade das variâncias.
  bartlett.test(mod.TR$res ~ TRAT) 

  # Boxplot de tratamentos vs resíduos:
  # Se os boxplots forem semelhantes, há homocedasticidade.
  par(mfrow=c(1, 1))
  boxplot(mod.TR$res ~ TRAT)
  
  
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
    "DIC - Tabela ANOVA.csv") 


# --------------------------------------------
# 8) TESTE DE COMPARAÇÃO DE MÉDIAS
# --------------------------------------------
# Teste Tukey, SNL (Student-Newman-Keuls), Duncan, t e 
# Scott-Knott a 5% de significância.
  require(easyanova)
  require(dplyr)

# Tabela usando dados transformados (quando for o caso),
# mas mostrando médias originais:
  par(mfrow=c(1, 1))
  ttuk <- easyanova::ea1(dados.TR, design=1, plot=2)
  teste <- arrange(ttuk$Means, ttuk$Means$treatment)
  ttuk.o <- easyanova::ea1(dados, design=1, plot=2)
  teste.o <- arrange(ttuk.o$Means, ttuk.o$Means$treatment)
  teste$mean <- teste.o$mean
  colnames(teste) <- c(
    "Tratamentos", 
    "Médias",
    "Erro padrão",
    "Tukey",
    "SNK",
    "Duncan",
    "t",
    "Scott-Knott"
  ) 
teste

# Exportar para Excel:
write.csv2(teste, file = 
             "DIC - Testes de Comparação de Médias.csv") 


# --------------------------------------------
# 9) DMS - Diferença Mínima Significativa do teste Tukey
# --------------------------------------------
# Valor que retrata a diferença mínima para que duas
# médias tenham diferença significativa a 5%.

# Cálculo do DMS:
  t.HSD <- TukeyHSD(mod.TR, ordered=TRUE)
  dms <- unname(0.5*diff(t.HSD$TRAT[1, 2:3]))
  LimSup <- mean(RESP.TR) 
  LimInf <- LimSup-dms  

# Os dados foram transformados em 4.1)?
  # Se NÃO, apenas rode o comando abaixo:
    dms
  
  # Se SIM, escolha o TR usado para
  # realizar a transformação inversa:
    # TR1. Raiz quadrada:
    dms.sqrt <- (LimSup)^2-(LimInf)^2 
    dms.sqrt
  
    # TR2. Logarítmica:
    dms.log <- exp(LimSup)-exp(LimInf) 
    dms.log
  
    # TR3. Hiperbólica:
    dms.hip <- (LimSup)^(-1)-(LimInf)^(-1)
    dms.hip
    
    # TR4. Box-Cox:
    dms.bc <- ((LimSup*lambda.max)+1)^(1/lambda.max) -
              ((LimInf*lambda.max)+1)^(1/lambda.max)
    dms.bc


