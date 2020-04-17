# --------------------------------------------
# Delineamento Inteiramente Casualizado (DIC)
# Elaborado por: Pablo Chang
#
# Para rodar os comandos, use Ctrl + Enter em
# cada linha!
# --------------------------------------------

# * O diretório dos dados deve ser o mesmo do script;
# * Os dados devem ser salvos em ".csv (separado por vírgulas)";
# Para mudar o diretório no RStudio, aperte Alt -> S -> W -> Enter.

# --------------------------------------------
# 1) LEITURA E PREPARAÇÃO DOS DADOS
# --------------------------------------------
# Digite o nome do arquivo abaixo entre " .csv":
dados <- read.csv2("Teste.csv", header = T)

# Mostra os 6 primeiros dados para visualização.
head(dados)

# Reescrever o nome das colunas de acordo com cada fator.
# Sendo "RESP" a variável resposta.
# Obs: aqui no exemplo tem 3 fatores, mude de acordo com
# sua necessidade. Mude o restante do script também.
colnames(dados) <- c("FATOR1", 
                     "FATOR2", 
                     "FATOR3", 
                     "RESP") 

# Mostra os 6 primeiros dados para ver como ficou.
head(dados)

# Salva os dados na memória do R, sem precisar ficar chamando.
attach(dados) 


# --------------------------------------------
# 2) RESUMO DESCRITIVO 
# --------------------------------------------
# Pacote "agricolae". Deve instalar o pacote com
# Alt -> T -> Enter.
library(agricolae)

# Cálculo dos Quartis
quartil1 <- quantile(RESP, 0.25)
quartil2 <- quantile(RESP, 0.5)
quartil3 <- quantile(RESP, 0.75)

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
                      Resposta = c(
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

# Exportando Resumo Estatístico para Excel:
write.csv2(resumo, 
           file = "DIC_1. Resumo Estatístico.csv") 


# --------------------------------------------
# 3) BOXPLOT 
# --------------------------------------------
# Boxplot geral, com o ponto representando a média.
boxplot(RESP, col="white")
points(mean(RESP),col="red",pch=3)

# Boxplot para cada fator
boxplot(RESP ~ FATOR1)  
boxplot(RESP ~ FATOR2)

