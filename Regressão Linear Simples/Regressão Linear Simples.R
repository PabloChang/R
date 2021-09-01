# --------------------------------------------
# Regressão Linear Simples
# Elaborado por: Pablo Chang (16/07/2020)
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
{
  library(rstudioapi) # precisa ter instalado o pacote "rstudioapi"
  current_path = 
    rstudioapi::getActiveDocumentContext()$path 
  setwd(dirname(current_path ))
  print( getwd() )
}

# Troque o nome do arquivo de dados (entre " .csv"):
  dados <- read.csv2(
  "DadosExemploRLS.csv", 
  header = T)

# Troca os pontos dos gráficos por vírgula:
  options(OutDec=",")

# Mostra as 6 primeiras linhas para visualização.
# As colunas devem seguir a ordem: tratamento // resposta;
  head(dados)
  
# Filtrar dados por categorias, quando existir:
# Exemplo: mostrar somente dados da Profundidade 2.
# (Para usar, exclua o "#" abaixo e modifique)
# require(dplyr)
# dados <- filter(dados, Profundidade==2)

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
{
  require(graphics)
  boxplot(Y,
          col="yellow",
          outcol="blue",
          ylab="Massa fresca da raiz (g)")
  points(mean(Y),col="red",pch=3)
}

# Boxplot para cada tratamento
{
  boxplot(Y ~ X,
          col="skyblue1",
          outcol="blue",
          xlab="Densidade (g/cm³)",
          ylab="Massa fresca da raiz (g)")
  points(tapply(Y,X,mean),col="red",pch=3)
}

# --------------------------------------------
# 4) TESTE DE NORMALIDADE
# --------------------------------------------
{
# Pacote com alguns testes:
library(nortest)
library(ExpDes)

# Cálculo dos resíduos:
mod = lm(Y ~ X)

# Gráficos de resíduos:
# Para ser normal, o histograma deve ter formato de sino no centro.
# Se o gráfico Normal Q-Q se assemelhar a uma reta crescente,
# então existe normalidade.
plotres(mod)
}

# Testes:
# Se p-value > 0,05, os resíduos possuem distribuição normal.
shapiro.test(mod$res) # Shapiro-Wilk
ad.test(mod$res) # Anderson-Darling

# A falta de normalidade não introduz problema 
# na estimação dos parâmetros, mas sim,
# nos desvios padrões, afetando a validade dos
# intervalos de confiança e dos testes de 
# hipótese.

# --------------------------------------------
# 5) TESTE DE HOMOCEDASTICIDADE DAS VARIÂNCIAS
# --------------------------------------------
  # Isso implica que cada tratamento que está sendo 
  # comparado pelo teste F, deve ter aproximadamente 
  # a mesma variância para que a ANOVA tenha validade.
  
  # Redefinição de dados:
  dados.TR <- data.frame(X, Y)
  attach(dados.TR)
  mod.TR <- lm(Y ~ X)

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
  as.data.frame(anova(mod.TR)), 
  file = 
    "RLS - ANOVA da Regressão.csv") 


# --------------------------------------------
# 8) Coeficientes do modelo
# --------------------------------------------
# A equação de um modelo linear é:
# y = a*x+b

# Cálculo dos coeficientes
# Os coeficientes estão na coluna Estimativa.
# a = X
# b = (Intercept)
# Se Pr(>|t|) < 0,05, então o coeficiente foi
# significativo a 5%.
  coefs <- as.data.frame(summary(mod)[[4]])
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
  ic <- as.data.frame(confint(mod.TR))

# Se os dados não foram transformados em 4.1), 
# rode o seguinte comando e pule para 10)
  ic


# --------------------------------------------
# 10) Coeficientes de determinação (R²)
# --------------------------------------------
{
erro_puro <- lm(Y ~ factor(X))
R2 <- summary(mod.TR)$r.squared/summary(erro_puro)$r.squared
R2 # R²
}

{
R2_aj <- summary(mod.TR)$adj.r.squared/summary(erro_puro)$adj.r.squared
R2_aj # R² ajustado
}
  
# --------------------------------------------
# 11) Teste da Falta de Ajuste
# --------------------------------------------
# Usar apenas quando Y tiver repetições.
# Se Pr(>F) > 0,05, então o modelo se ajusta bem aos dados 
# a 5% de significância.
anova(mod.TR, erro_puro)                        

# Exportar para Excel:
write.csv2(
  as.data.frame(anova(mod.TR, erro_puro)), 
  file = 
    "RLS - Teste da Falta de Ajuste.csv") 


# --------------------------------------------
# 12) Gráfico da regressão
# --------------------------------------------
# Equação
{
texto <- sprintf('y=(%.2f)+(%.2f)*x,  R² = %.2f',
                 mod$coefficients[1],
                 mod$coefficients[2],
                 summary(mod)$r.squared/summary(erro_puro)$r.squared)
texto

# Barras de erro padrão médio
tgc <- summarySE(dados.TR, measurevar="Y", groupvars=c("X"))
}


# Gráfico 
# Para salvar, use "Save as Image..." -> Image format: SVG.
{
  library(ggplot2)
  (
    grafico <- ggplot(data = tgc, aes(x = X, y = Y)) +
    labs(x = "Densidade (g/cm³)",
         y = "Massa fresca da raiz (g)") +
    geom_errorbar(aes(ymin=Y-se,
                      ymax=Y+se),
                  width=.05,
                  col = "red") +
    geom_smooth(method = "lm") +
        stat_summary(
        geom = "point",
        fun = "mean",
        col = "black",
        size = 2,
        shape = 21,
        fill = "red"
      ) +
    theme_bw() +
    geom_text(aes(x=,min(X), y=min(Y), label=texto), hjust=0, vjust=1)
  )
}

