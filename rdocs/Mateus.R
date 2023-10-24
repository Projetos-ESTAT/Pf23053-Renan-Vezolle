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

# carregando alguns pacotes
pacman::p_load(ggcorrplot, knitr,showtext, kableExtra, data.table, tidyr,SnowballC,
               wordcloud,tm,stringr,gridExtra)

df <- read_xlsx("banco/Tabela geral de resultados - corrigida(banco incompleto).xlsx", sheet = 1, range = "A1:V69")

sexo <- df %>%
  filter(!is.na(SEXO)) %>%
  count(SEXO) %>%
  mutate(
    freq = n %>% percent(),
  ) %>%
  mutate(
    freq = gsub("\\.", ",", freq) %>% paste("%", sep = ""),
    label = str_c(n, " (", freq, ")") %>% str_squish()
  )

levels(sexo$SEXO) <- list("Masculino" = "M","Feminino" = "F")

sexo_plot <- ggplot(sexo) +
  aes(
    x = fct_reorder(SEXO, n, .desc = T),
    y = n,
    label = label
  ) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, # hjust = .5,
    size = 2
  ) +
  ylim(0,50)+
  labs(x = "Sexo", y = "Frequência") +
  theme_estat() + ggtitle("Sexo")

etnia <- df %>%
  mutate(ETNIA = case_when(
    ETNIA %>% str_detect("Pardo") ~ "Pardo",
    ETNIA %>% str_detect("Parda") ~ "Pardo",
    ETNIA %>% str_detect("Preto") ~ "Preto",
    ETNIA %>% str_detect("Preta") ~ "Preto",
    ETNIA %>% str_detect("Branco") ~ "Branco",
    ETNIA %>% str_detect("Branca") ~ "Branco"
  )) %>%
  filter(!is.na(ETNIA)) %>%
  count(ETNIA) %>%
  mutate(
    freq = n %>% percent(),
  ) %>%
  mutate(
    freq = gsub("\\.", ",", freq) %>% paste("%", sep = ""),
    label = str_c(n, " (", freq, ")") %>% str_squish()
  )



etnia_plot <- ggplot(etnia) +
  aes(
    x = fct_reorder(ETNIA, n, .desc = T),
    y = n,
    label = label
  ) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, # hjust = .5,
    size = 2
  ) +
  ylim(0,40)+
  labs(x = "Etnia", y = "Frequência") +
  theme_estat() + ggtitle("ETNIA")

esc <- df %>%
  mutate(ESCOL = case_when(
    ESCOL %>% str_detect("E.M inc") ~ "EM.inc",
    ESCOL %>% str_detect("EM.inc") ~ "EM.inc",
    ESCOL %>% str_detect("EF.com") ~ "Fud.com",
    ESCOL %>% str_detect("Fund.com") ~ "Fud.com",
    ESCOL %>% str_detect("Fud.com") ~ "Fud.com",
    ESCOL %>% str_detect("EF.inc") ~ "Fud.inc",
    ESCOL %>% str_detect("Fund.inc") ~ "Fud.inc",
    ESCOL %>% str_detect("Fud.inc") ~ "Fud.inc",
    ESCOL %>% str_detect("EM.com") ~ "EM.com",
    ESCOL %>% str_detect("Sup.com") ~ "Sup.com",
    ESCOL %>% str_detect("Nunca") ~ "Nunca"
  )) %>%
  filter(!is.na(ESCOL)) %>%
  count(ESCOL) %>%
  mutate(
    freq = n %>% percent(),
  ) %>%
  mutate(
    freq = gsub("\\.", ",", freq) %>% paste("%", sep = ""),
    label = str_c(n, " (", freq, ")") %>% str_squish()
  )


esc_plot <- ggplot(esc) +
  aes(
    x = fct_reorder(ESCOL, n, .desc = T),
    y = n,
    label = label
  ) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, # hjust = .5,
    size = 2
  ) +
  ylim(0,40)+
  labs(x = "Escolaridade", y = "Frequência") +
  theme_estat(axis.text.x = element_text(angle = 30, vjust = .5)) + ggtitle("Escolaridade")

grid <- grid.arrange(sexo_plot,etnia_plot,esc_plot, ncol = 2)

ggsave("resultados/graph_analise1.pdf", grid,width = 158, height = 93, units = "mm")
