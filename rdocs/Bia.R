source("rdocs/source/packages.R")

# ---------------------------------------------------------------------------- #

#   ______   _____  ________      ________ 
#  |  ____| / ____| |__   __|  /\  |__   __|
#  | |__    | (___     | |    /  \    | |   
#  |  __|    \___ \    | |   / /\ \   | |   
#  | |____   ____) |   | |  / ____ \  | |   
#  |______  |_____/    |_| /_/    \_\ |_|   
#  
#         Consultoria estatística 
#

# ---------------------------------------------------------------------------- #
# ############################## README ###################################### #
# Consultor, favor utilizar este arquivo .R para realizar TODAS as análises
# alocadas a você neste projeto pelo gerente responsável, salvo instrução 
# explícita do gerente para mudança.
#
# Escreva seu código da forma mais clara e legível possível, eliminando códigos
# de teste depreciados, ou ao menos deixando como comentário. Dê preferência
# as funções dos pacotes contidos no Tidyverse para realizar suas análises.
# ---------------------------------------------------------------------------- #




#mudando nome dos bancos
geral <- Tabela_geral_de_resultados_corrigida_1_
doença <- Analise_doenças
medic <- Medicamentos
view(geral)
view(doença)
view(medic)


#manipulando o banco geral
geral2 <- geral[, c("...1", "H", "Índ AGC")]
geral2$pacientes <- geral2$...1
geral2$pacientes <- as.numeric(gsub("[^0-9]", "", geral2$pacientes))
geral2 <- geral2[, c("pacientes", "H", "Índ AGC")]
geral2$pacientes <- paste0("P", geral2$pacientes)

View(geral2)




###############################
########## análise 8 ##########
###############################


#separando os pacientes
valores <- strsplit(as.character(doença$`PACIENTES:`), ",")
doença2 <- data.frame(
  pacientes = unlist(valores)
)
view(doença2)


#limpando a coluna
doença2$pacientes <- as.character(trimws(doença2$pacientes)) #retirando espaços
doença2$pacientes <- na.omit(doença2$pacientes) #retirando NA's
doença2$pacientes <- sub("\\.0$", "", doença2$pacientes)


#quantas doenças cada paciente tem 

###arrumando a frequência de cada um
doença2$pacientes <- paste0("P", doença2$pacientes)
banco1 <- doença2 %>%
  filter(!is.na(pacientes)) %>%
  count(pacientes) %>%
  mutate(
    freq = n %>% percent(),
  ) %>%
  mutate(
    freq = gsub("\\.", ",", freq) %>% paste("%", sep = ""),
    label = str_c(n, " (", freq, ")") %>% str_squish()
  )
banco1 <- banco1[c("pacientes", "n")]
view(banco1)

###substituindo 
banco1$ndoença <- ifelse(banco1$n <= 1, "uma doença", "mais de uma doença")


#juntando os bancos (geral e doença)
df1 <- geral2[c("pacientes","H")]
final8 <- merge(df1, banco1, by = "pacientes", all = TRUE)
view(final8)


#teste quiquadrado 
final8 <- na.omit(final8)
final8$H <- as.factor(final8$H)
final8$ndoença <- as.factor(final8$ndoença)
tabela_contingencia <- table(final8$ndoença, final8$H)
chi_squared <- chisq.test(tabela_contingencia) #p-value = 0.2486






###############################
########## análise 9 ##########
###############################


#separando os pacientes
valores <- strsplit(as.character(medic$`PACIENTES`), ",")
medic2 <- data.frame(
  pacientes = unlist(valores)
)
view(medic2)


#limpando a coluna
medic2$pacientes <- as.character(trimws(medic2$pacientes)) #retirando espaços
medic2$pacientes <- sub("\\.0$", "", medic2$pacientes)
medic2 <- medic2[!is.na(medic2$pacientes), , drop = FALSE] #retirando NA's


#quantas medics cada paciente tem 

###arrumando a frequência de cada um

medic2$pacientes <- paste0("P", medic2$pacientes)
banco2 <- medic2 %>%
  filter(!is.na(pacientes)) %>%
  count(pacientes) %>%
  mutate(
    freq = n %>% percent(),
  ) %>%
  mutate(
    freq = gsub("\\.", ",", freq) %>% paste("%", sep = ""),
    label = str_c(n, " (", freq, ")") %>% str_squish()
  )
banco2 <- banco2[c("pacientes", "n")]
view(banco2)

###substituindo 
banco2$nmedic <- ifelse(banco2$n <= 4, "4 medicamentos ou menos", "mais que 4 medicamentos")


#juntando os bancos (geral e medic)
df2 <- geral2[c("pacientes","Índ AGC")]
final9 <- merge(df2, banco2, by = "pacientes", all = TRUE)
view(final9)
view(df2)

final9$`Índ AGC` <- as.numeric(final9$`Índ AGC`)
final9$nmedic <- as.factor(final9$nmedic)
final9 <- na.omit(final9)
resultado_teste <- t.test(final9$`Índ AGC`,final9$nmedic) #p-value = 0.01818


# gráfico
ggplot(final9) +
  aes(
    x = nmedic,
    y = `Índ AGC`
  ) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Uso de polifarmácia por idosos", y = "Índice AGC") +
  theme_estat()
ggsave("box_9.pdf", width = 158, height = 93, units = "mm")

#quadro medida resumo 
quadro_resumo <- final9 %>% 
  group_by(nmedic) %>% # caso mais de uma categoria
  summarize(Média = round(mean(`Índ AGC`),2),
            `Desvio Padrão` = round(sd(`Índ AGC`),2),
            `Variância` = round(var(`Índ AGC`),2),
            `Mínimo` = round(min(`Índ AGC`),2),
            `1º Quartil` = round(quantile(`Índ AGC`, probs = .25),2),
            Mediana = round(quantile(`Índ AGC`, probs = .5),2),
            `3º Quartil` = round(quantile(`Índ AGC`, probs = .75),2),
            `Máximo` = round(max(`Índ AGC`),2)) %>% t() %>% as.data.frame() %>% 
  mutate(V1 = str_replace(V1,"\\.",",")) # adicionar mais mutate(...) se tiver mais categorias

xtable::xtable(quadro_resumo)








