# --------------------------------------------
# Delineamento em Blocos ao Acaso
# Elaborado por: Pablo Chang (21/04/2020)
# https://github.com/PabloChang/R
# --------------------------------------------
# Os dados devem ser salvos em ".csv (separado por vírgulas)";
# Não pode haver espaço e acentuação nos títulos, única guia;
# O arquivo de dados e script devem estar numa mesma pasta; 
# Para rodar os comandos, use Ctrl + Enter em cada linha!


# --------------------------------------------
# 1) LEITURA E PREPARAÇÃO DOS DADOS
# --------------------------------------------
# Comando para definir a localização da pasta
# Caso não tenho instalado o pacote é só rodar: 
# install.packages("rstudioapi")
library(rstudioapi)
  current_path = 
    rstudioapi::getActiveDocumentContext()$path 
  setwd(dirname(current_path ))
  print( getwd() )

# Troque o nome do arquivo de dados (entre " .csv"):
dados <- read.csv2(
  "DadosExemploDBC.csv", 
  header = T)

# Troca os pontos dos gráficos por vírgula:
options(OutDec=",")

# Mostra as 6 primeiras linhas para visualização.
# As colunas devem seguir a ordem: 
# bloco // tratamentos // resposta;
head(dados)

# Reescrita dos títulos para efeitos de script.
# Sendo "TRAT" comoo tratamento e "RESP" como var. resposta.
colnames(dados) <- c(
                     "BLOCO",
                     "TRAT",
                     "RESP"
                     ) 

# Mostra as 6 primeiras linhas para ver como ficou.
head(dados)

# Salva os títulos na memória do R, sem precisar ficar chamando.
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
write.csv2(resumo, 
           file = "DBC - Resumo Descritivo.csv") 


# --------------------------------------------
# 3) BOXPLOT 
# --------------------------------------------
# Boxplot geral, com a cruz representando a média.
# Altere as cores e legendas entre aspas " ".
require(graphics)  
boxplot(RESP, 
          col="white", 
          ylab="Massa fresca da raiz (g)")
  points(mean(RESP),col="red",pch=3)
  
  # Boxplot para cada tratamento
  boxplot(RESP ~ TRAT,
      col="white", 
      xlab="Densidade (g/cm³)", 
      ylab="Massa fresca da raiz (g)")
  points(tapply(RESP,TRAT,mean),col="red",pch=3)
  
  # Boxplot para cada bloco
  boxplot(RESP ~ BLOCO,
          col="white", 
          xlab="Genótipo", 
          ylab="Massa fresca da raiz (g)")
  points(tapply(RESP,TRAT,mean),col="red",pch=3)

    
# --------------------------------------------
# 4) TESTE DE NORMALIDADE
# --------------------------------------------
# Pacote com alguns testes:
library(nortest)

# Para ser normal, o histograma tem que ter formato de sino.
  hist(RESP,
       main="Histograma",
       xlab="Massa fresca da raiz (g)",
       ylab="Frequência") 

# Se o gráfico se assemelhar a uma reta crescente,
#então existe normalidade.
  qqnorm(RESP,
         xlab="Quantis teóricos",
         ylab="Resíduos", 
         main="Gráfico Normal de 
           Probabilidade dos Resíduos")
  qqline(RESP,
         col="red")

# Testes:
# Se p-value > 0,05, os dados possuem distribuição normal.
  shapiro.test(RESP) # Shapiro-Wilk
  ad.test(RESP) # Anderson-Darling

# Os dados são normais?
  # Se SIM, rode o comando abaixo e pule para a etapa 5).
    RESP.TR <- RESP
  # Se NÃO, faça a transformação em 4.1).


# --------------------------------------------
# 4.1) TRANSFORMAÇÃO DE DADOS NÃO-NORMAIS
# --------------------------------------------
# Faça os testes, até atingir a normalidade!
  
  # 1. Raiz quadrada:
    TR1 <- sqrt(RESP)
    # Histograma
    hist(TR1,
         main="Transformação pela raiz quadrada",
         xlab="Massa fresca da raiz (g)",
         ylab="Frequência")
    # Gráfico normal
    qqnorm(TR1,
           xlab="Quantis teóricos",
           ylab="Resíduos", 
           main="Gráfico Normal de 
           Probabilidade dos Resíduos")
    qqline(TR1,
           col="red")
    # Testes
    shapiro.test(TR1) # Shapiro-Wilk
    ad.test(TR1) # Anderson-Darling
  
  # 2. Logarítmica:
    TR2 <- log(RESP)
    # Histograma
    hist(TR2,
         main="Transformação logarítmica",
         xlab="Massa fresca da raiz (g)",
         ylab="Frequência") 
    # Gráfico normal
    qqnorm(TR2,
           xlab="Quantis teóricos",
           ylab="Resíduos", 
           main="Gráfico Normal de 
           Probabilidade dos Resíduos")  
    qqline(TR2,
           col="red")
    # Testes
    shapiro.test(TR2) # Shapiro-Wilk
    ad.test(TR2) # Anderson-Darling
  
  # 3. Hiperbólica
    TR3 <- 1/RESP
    # Histograma
    hist(TR3,
         main="Transformação inversa",
         xlab="Massa fresca da raiz (g)",
         ylab="Frequência") 
    # Gráfico normal
    qqnorm(TR3,
           xlab="Quantis teóricos",
           ylab="Resíduos", 
           main="Gráfico Normal de 
           Probabilidade dos Resíduos")
    qqline(TR3,
           col="red")
    # Testes
    shapiro.test(TR3) # Shapiro-Wilk
    ad.test(TR3) # Anderson-Darling
  
  # 4. Box-Cox
    require(MASS)
      boxcox(RESP ~ TRAT, data=dados, plotit=T)
      boxcox(RESP ~ TRAT, data=dados, lam=seq(-0.5, 0.5, 1/10))
    lambda.max <- (0.06) # Digite aqui o valor da lambda 
    # em que atingiu o ponto máximo.
    TR4 <- (RESP^(lambda.max)-1)/lambda.max
    hist(TR4,
         main="Transformação por Box-Cox",
         xlab="Massa fresca da raiz (g)",
         ylab="Frequência")
    # Gráfico normal
    qqnorm(TR4, 
           xlab="Quantis teóricos",
           ylab="Resíduos", 
           main="Gráfico Normal de 
           Probabilidade dos Resíduos")
    qqline(TR4,
           col="red")
    # Testes
    shapiro.test(TR4) # Shapiro-Wilk
    ad.test(TR4) # Anderson-Darling
  
# Digite na 2ª linha o nº do TR escolhido dentro de ( ):
  RESP.TR <- 
    (RESP) #troque aqui, por exemplo: (TR2).
  # Com isso, as próximas análises irão usar os
  # dados transformados!


# --------------------------------------------
# 5) TESTE DE HOMOCEDASTICIDADE DAS VARIÂNCIAS
# --------------------------------------------
# Se p-value > 0,05, há homogeneidade das variâncias.
# Teste de Bartlett por fator:
  bartlett.test(RESP.TR, TRAT) 

  
# --------------------------------------------
# 6) TESTE DE INDEPENDÊNCIA
# --------------------------------------------

# Teste de Durbin-Watson
  # Se p-value > 0,05, então
  # os dados são aleatórios e independentes.
  # Ou seja, uma observação não influencia na outra.
  require(lmtest)
  ajuste = lm(RESP.TR ~ TRAT, data=dados) 
  dwtest(ajuste) 

# Gráfico de resíduos x valores ajustados
  # Se os pontos forem aleatórios e espalhados,  
  # então os dados são aleatórios e independentes;
  # Se apresentar uma tendência, então há dependência.
  plot(ajuste$residuals, pch=20,
       xlab="Ordem de coleta", ylab="Resíduos", 
       main="Resíduos vs. ordem de coleta", cex.main=0.95,
       xaxt='n', yaxt='n')
  abline(0, 0, col="red") 
    
  
# --------------------------------------------
# 7) ANOVA - análise de variância
# --------------------------------------------

# Cálculo da análise de variância
TRAT <- as.factor(TRAT)
BLOCO <- as.factor(BLOCO)
anv <- aov(RESP.TR ~ TRAT + BLOCO)

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
    "DBC - Tabela ANOVA.csv") 


# --------------------------------------------
# 8) TESTE DE COMPARAÇÃO DE MÉDIAS
# --------------------------------------------

# Teste Tukey a 5% de significância

  # Por tratamento:
    ttuk <- HSD.test(anv, 'TRAT')
    teste <- as.data.frame(ttuk$groups)
    anv.o <- aov(RESP ~ TRAT + BLOCO)
    ttuk.o <- HSD.test(anv.o, 'TRAT', )
    teste.o <-as.data.frame(ttuk.o$groups)
    teste$RESP.TR <- teste.o$RESP
    colnames(teste) <- c(
      "Médias", 
      "Grupos"
    ) 
  teste
  # Exportar para Excel:
  write.csv2(teste, file = 
               "DBC - Teste Tukey por tratamento.csv") 
  
  # Por bloco:
    ttuk_B <- HSD.test(anv, 'BLOCO')
    teste_B <- as.data.frame(ttuk_B$groups)
    ttuk.o_B <- HSD.test(anv.o, 'BLOCO', )
    teste.o_B <-as.data.frame(ttuk.o_B$groups)
    teste_B$RESP.TR <- teste.o_B$RESP
    colnames(teste_B) <- c(
      "Médias", 
      "Grupos"
    ) 
  teste_B
  # Exportar para Excel:
  write.csv2(teste_B, file = 
               "DBC - Teste Tukey por bloco.csv") 

# Gráfico de barras 
# Altere a escala em ylim=c(  ).  
  # Por tratamento:
  bar.group(ttuk.o$groups,
          ylim=c(0,0.2),
          xlab="Densidade do solo (g/cm³)",
          ylab="Escala transformada",
          density=4,
          border="blue")

  # Por bloco:
  bar.group(ttuk.o_B$groups,
            ylim=c(0,0.2),
            xlab="Densidade do solo (g/cm³)",
            ylab="Escala transformada",
            density=4,
            border="blue")


