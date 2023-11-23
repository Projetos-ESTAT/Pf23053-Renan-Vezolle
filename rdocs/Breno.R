source("rdocs/source/packages.R")

# ---------------------------------------------------------------------------- #

#        ______   _____  ________      ________ 
#      |  ____| / ____| |__   __| /\  |__   __|
#     | |__    | (___     | |   /  \    | |   
#    |  __|    \___ \    | |  / /\ \   | |   
#   | |____   ____) |   | |  /____ \  | |   
#  |______   |_____/   |_| /_/    \_\|_|   
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


# Pacotes
library(dplyr)
library(forcats)
library(readxl)
library(car)

# Ajustando o banco
df_renan <- read_excel("banco/Tabela geral de resultados - corrigida (1).xlsx")
df_renan <- df_renan[,c(1:5, 18, 20:22)]
df_renan <- df_renan[-c(65, 98),]

df_renan$IDADE = as.integer(df_renan$IDADE)
df_renan$IDADE
summary(df_renan$IDADE)

df_renan$`Índ AGC`
summary(df_renan$`Índ AGC`)

# Criando grupo etário
vec_ret = c()
for (i in 1:length(df_renan$...1)){
  if (df_renan$IDADE[i] < 80){
    vec_ret = append(vec_ret, '60 - 79 anos')
  }else vec_ret = append(vec_ret, '80 anos ou mais')
}
vec_ret

df_renan$grupo_etario = vec_ret


# Criando var cat para renda familiar
vec_rf = c()
for (i in 1:length(df_renan$...1)){
  if (is.na(df_renan$REN.FAM[i])){
    vec_rf = append(vec_rf, NA)
  }else if (df_renan$REN.FAM[i] <= 1320){
    vec_rf = append(vec_rf, 'Até 1')
  }else if (df_renan$REN.FAM[i] <= 3960){
    vec_rf = append(vec_rf, '(1 - 3]')
  }else if (df_renan$REN.FAM[i] <= 6600){
    vec_rf = append(vec_rf, '(3 - 5]')
  }else vec_rf = append(vec_rf, 'Maior que 5')
}
vec_rf

df_renan$rf = fct_relevel(vec_rf, c('Até 1', '(1 - 3]', '(3 - 5]', 'Maior que 5'))


# Substituindo valores escritos inadequadamente
df_renan[df_renan == "F"] <- "Feminino"
df_renan[df_renan == "M"] <- "Masculino"

df_renan[df_renan == "Branco"] <- "Branca"
df_renan[df_renan == "Preto"] <- "Preta"
df_renan[df_renan == "Pardo"] <- "Parda"

## ANÁLISE 10 ##

# Testes
# NOrmalidade
shapiro.test(df_renan$`Índ AGC`)

ge_60 = filter(df_renan, `grupo_etario` == "60 - 79 anos")
ge_60
summary(ge_60$`Índ AGC`)

ge_80 = filter(df_renan, `grupo_etario` == "80 anos ou mais")
ge_80
summary(ge_80$`Índ AGC`)

# Mann-Whitney
wilcox.test(ge_60$`Índ AGC`, ge_80$`Índ AGC`)

# Correlação de Pearson
cor(df_renan$IDADE, df_renan$`Índ AGC`)

# Associação - qui-quadrado e exato de fisher
chisq.test(df_renan$`Índ AGC`, df_renan$grupo_etario)
fisher.test(df_renan$`Índ AGC`, df_renan$grupo_etario)

# histograma grupo 60-79
ggplot(ge_60) +
  #aes(x = `Índ AGC`, fill=grupo_etario) +
  aes(x = `Índ AGC`) +
  geom_histogram(
    aes(y = 100 * after_stat(count) / sum(after_stat(count))),
    colour = "white",
    fill = "#A11D21",
    binwidth = .15
  ) +
  labs(x = "AGC", y = "Porcentagem") +
  theme_estat()
ggsave("graph_breno_1.pdf", width = 158, height = 93, units = "mm")


# histograma grupo 80+
ggplot(ge_80) +
  #aes(x = `Índ AGC`, fill=grupo_etario) +
  aes(x = `Índ AGC`) +
  geom_histogram(
    aes(y = 100 * after_stat(count) / sum(after_stat(count))),
    colour = "white",
    fill = "#A11D21",
    binwidth = .15
  ) +
  labs(x = "AGC", y = "Porcentagem") +
  theme_estat()
ggsave("graph_breno_2.pdf", width = 158, height = 93, units = "mm")


# Gráfico de densidade  
ggplot(df_renan, aes(x = `Índ AGC`, fill = grupo_etario)) +
  geom_density(alpha = .75) +
  labs(x = "AGC", y = "Densidade", fill = "Grupo etário:") +
  theme_estat()
ggsave("graph_breno_8.pdf", width = 158, height = 93, units = "mm")




## ANÁLISE 11 ##  

# ANOVA, levene e KW
aov_res = aov(df_renan$`Índ AGC` ~ as.factor(df_renan$SEXO))
summary(aov_res)
leveneTest(df_renan$`Índ AGC` ~ as.factor(df_renan$SEXO))
kruskal.test(df_renan$`Índ AGC` ~ as.factor(df_renan$SEXO))

aov_res = aov(df_renan$`Índ AGC` ~ as.factor(df_renan$ETNIA))
summary(aov_res)
leveneTest(df_renan$`Índ AGC` ~ as.factor(df_renan$ETNIA))
kruskal.test(df_renan$`Índ AGC` ~ as.factor(df_renan$ETNIA))

aov_res = aov(df_renan$`Índ AGC` ~ as.factor(df_renan$rf))
summary(aov_res)
leveneTest(df_renan$`Índ AGC` ~ as.factor(df_renan$rf))
kruskal.test(df_renan$`Índ AGC` ~ as.factor(df_renan$rf))

aov_res = aov(df_renan$`Índ AGC` ~ as.factor(df_renan$ISAR))
summary(aov_res)
leveneTest(df_renan$`Índ AGC` ~ as.factor(df_renan$ISAR))
kruskal.test(df_renan$`Índ AGC` ~ as.factor(df_renan$ISAR))

aov_res = aov(df_renan$`Índ AGC` ~ as.factor(df_renan$TRST))
summary(aov_res)
leveneTest(df_renan$`Índ AGC` ~ as.factor(df_renan$TRST))
kruskal.test(df_renan$`Índ AGC` ~ as.factor(df_renan$TRST))

aov_res = aov(df_renan$`Índ AGC` ~ as.factor(df_renan$ICCI))
summary(aov_res)
leveneTest(df_renan$`Índ AGC` ~ as.factor(df_renan$ICCI))
kruskal.test(df_renan$`Índ AGC` ~ as.factor(df_renan$ICCI))


# tabelando valores para facilitar a transcrição para o overleaf
table(df_renan$SEXO)
table(df_renan$ETNIA)
table(df_renan$rf)
table(df_renan$ISAR)
table(df_renan$TRST)
table(df_renan$ICCI)

var = c(rep('Sexo',2), rep('Etnia', 3), rep('Renda Familiar', 4), rep("ISAR", 6),
        rep('TRST', 6), rep('ICCI', 9))
cat = c('Feminino', 'Masculino',
        'Branca', 'Parda', 'Preta',
        'Até 1', '(1 - 3]', '(3 - 5]', "Maior que 5",
        as.factor(0:5),
        as.factor(0:5),
        as.factor(0:8))
freq_abs = c(70, 27,
             39, 45, 12,
             35, 20, 5, 4,
             17, 26, 18, 17, 6, 3,
             23, 23, 22, 12, 6, 1,
             1, 1, 25, 28, 17, 9, 5, 6, 1)
freq_rel = c(72, 28,
             41, 47, 12,
             55, 31, 8, 6,
             20, 30, 21, 20, 7, 3,
             26, 26, 25, 14, 7, 1,
             1, 1, 27, 30, 18, 10, 5, 6, 1)
df_tabela = data.frame(var, cat, freq_abs, freq_rel)
View(df_tabela)


# Boxplots
ggplot(df_renan) +
  aes(
    x = SEXO,
    y = `Índ AGC`
  ) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Sexo", y = "AGC") +
  theme_estat()
ggsave("graph_breno_3.pdf", width = 158, height = 93, units = "mm")

df_renan %>% filter(!is.na(ETNIA)) %>%
  ggplot() +
  aes(
    x = ETNIA,
    y = `Índ AGC`
  ) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Etnia", y = "AGC") +
  theme_estat()
ggsave("graph_breno_4.pdf", width = 158, height = 93, units = "mm")


df_renan %>% filter(!is.na(rf)) %>%
  ggplot() +
  aes(
    x = rf,
    y = `Índ AGC`
  ) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Renda familiar (em salários mínimos)", y = "AGC") +
  theme_estat()
ggsave("graph_breno_9.pdf", width = 158, height = 93, units = "mm")


df_renan %>% filter(!is.na(ISAR)) %>%
  ggplot() +
  aes(
    x = as.factor(ISAR),
    y = `Índ AGC`
  ) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "ISAR", y = "AGC") +
  theme_estat()
ggsave("graph_breno_5.pdf", width = 158, height = 93, units = "mm")


df_renan %>% filter(!is.na(TRST)) %>%
  ggplot() +
  aes(
    x = as.factor(TRST),
    y = `Índ AGC`
  ) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "TRST", y = "AGC") +
  theme_estat()
ggsave("graph_breno_6.pdf", width = 158, height = 93, units = "mm")


df_renan %>% filter(!is.na(ICCI)) %>%
  ggplot() +
  aes(
    x = as.factor(ICCI),
    y = `Índ AGC`
  ) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "ICCI", y = "AGC") +
  theme_estat()
ggsave("graph_breno_7.pdf", width = 158, height = 93, units = "mm")

