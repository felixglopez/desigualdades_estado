##CODIGOS PARA EXPLORAR OS DADOS APLICADOS AO TEXTO
## DESIGUALDADES NOS ESTADOS
## FELIX GARCIA LOPEZ E MARCSON ARAÚJO

library(readxl)
library(tidyverse)

##CÓDIGOS GERAL
# Caminho do arquivo Excel
arquivo <- "C:/Users/r1705296/Documents/Git/desigualdades_estado/data/planilha_dados_graficos.xlsx"

# Listar todas as abas do arquivo Excel
abas <- excel_sheets("C:/Users/r1705296/Documents/Git/desigualdades_estado/data/planilha_dados_graficos.xlsx")

print(abas)



# Nome da aba ou índice da aba
total_uf_cor <- "total_uf_cor"  # Ou use um índice, por exemplo, 1

# Ler a aba específica
dados_cor <- read_excel(arquivo, sheet = total_uf_cor)

###########################################################
##EXPLORANDO VARIAÇÕES E DESIGUALDADES POR CATEGORIAS DE COR
############################################################

# Nome da aba ou índice da aba
total_uf_cor <- "total_uf_cor"  # Ou use um índice, por exemplo, 1

# Ler a aba específica
dados_cor <- read_excel(arquivo, sheet = total_uf_cor)

# Visualizar os dados

head(dados_cor)
dados_cor$vinculos_executivo_estadual <- as.numeric(dados_cor$vinculos_executivo_estadual)


# Filtrar apenas os anos 2004 e 2021
dados_filtrados <- subset(dados_cor, ano %in% c(2004, 2021))
head(dados_filtrados)



# Calcular os totais por UF, cor e ano
totais_por_ano <- dados_filtrados %>%
        filter(cor_descricao %in% c("Branca","Preta", "Parda")) %>% 
        group_by(sigla_uf, codigo_uf, cor_descricao, ano) %>%
        summarize(total = sum(vinculos_executivo_estadual, na.rm = TRUE)) %>% 
        ungroup()

# Calcular a variação percentual entre 2004 e 2021
variacao_percentual <- totais_por_ano %>%
        pivot_wider(names_from = ano, values_from = total, names_prefix = "ano_") %>%
        mutate(variacao = ((ano_2021 - ano_2004) / ano_2004) * 100) %>%
        select(codigo_uf, sigla_uf, cor_descricao, variacao)

# Visualizar o resultado
print(variacao_percentual, n = 50)

#ver o gráfico com a variação por UF de apenas uma cor.
variacao_percentual %>% 
        filter(cor_descricao == 'Parda', sigla_uf != "RR") %>% 
        ggplot(aes(x = sigla_uf, y = variacao))+
        geom_col()


##gráfico de faceta com variação em cada uma das três cores
variacao_percentual %>% 
        filter(sigla_uf != "RR") %>% 
        ggplot(aes(x = sigla_uf, y = variacao))+
        geom_col()+
        facet_wrap(~cor_descricao, ncol = 1)



View(totais_por_ano)
