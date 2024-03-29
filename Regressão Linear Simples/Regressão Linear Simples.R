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
          xlab="Densidade (g/cm�)",
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

# C�lculo dos res�duos:
mod = lm(Y ~ X)

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

# A falta de normalidade n�o introduz problema 
# na estima��o dos par�metros, mas sim,
# nos desvios padr�es, afetando a validade dos
# intervalos de confian�a e dos testes de 
# hip�tese.

# --------------------------------------------
# 5) TESTE DE HOMOCEDASTICIDADE DAS VARI�NCIAS
# --------------------------------------------
  # Isso implica que cada tratamento que est� sendo 
  # comparado pelo teste F, deve ter aproximadamente 
  # a mesma vari�ncia para que a ANOVA tenha validade.
  
  # Redefini��o de dados:
  dados.TR <- data.frame(X, Y)
  attach(dados.TR)
  mod.TR <- lm(Y ~ X)

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
  coefs <- as.data.frame(summary(mod)[[4]])
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


# --------------------------------------------
# 10) Coeficientes de determina��o (R�)
# --------------------------------------------
{
erro_puro <- lm(Y ~ factor(X))
R2 <- summary(mod.TR)$r.squared/summary(erro_puro)$r.squared
R2 # R�
}

{
R2_aj <- summary(mod.TR)$adj.r.squared/summary(erro_puro)$adj.r.squared
R2_aj # R� ajustado
}
  
# --------------------------------------------
# 11) Teste da Falta de Ajuste
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


# --------------------------------------------
# 12) Gr�fico da regress�o
# --------------------------------------------
# Equa��o
{
texto <- sprintf('y=(%.2f)+(%.2f)*x,  R� = %.2f',
                 mod$coefficients[1],
                 mod$coefficients[2],
                 summary(mod)$r.squared/summary(erro_puro)$r.squared)
texto

# Barras de erro padr�o m�dio
tgc <- summarySE(dados.TR, measurevar="Y", groupvars=c("X"))
}


# Gr�fico 
# Para salvar, use "Save as Image..." -> Image format: SVG.
{
  library(ggplot2)
  (
    grafico <- ggplot(data = tgc, aes(x = X, y = Y)) +
    labs(x = "Densidade (g/cm�)",
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

