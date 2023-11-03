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
# banco de dados ----
df <- read_xlsx("banco/Tabela geral de resultados - corrigida (1).xlsx", sheet = 1, range = "A1:V70")

df <- df[-65,]

#### Primeiro grid ----

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
    size = 2.5
  ) +
  ylim(0,50)+
  labs(x = "Sexo", y = "Frequência") +
  theme_estat(plot.title = element_text(hjust = 0.5, size = 10,face = 'bold')) + ggtitle("Sexo")

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
    size = 2.5
  ) +
  ylim(0,40)+
  labs(x = "Etnia", y = "Frequência") +
  theme_estat(plot.title = element_text(hjust = 0.5, size = 10,face = 'bold')) + ggtitle("ETNIA")

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
    size = 2.5
  ) +
  ylim(0,40)+
  labs(x = "Escolaridade", y = "Frequência") +
  theme_estat(axis.text.x = element_text(angle = 30, vjust = .5),plot.title = element_text(hjust = 0.5, size = 10,face = 'bold')) + 
  ggtitle("Escolaridade")

ec <- df %>%
  mutate(EST.CIV = case_when(
    EST.CIV %>% str_detect("Viúva") ~ "Viúva(a)",
    EST.CIV %>% str_detect("Viuva") ~ "Viúva(a)",
    EST.CIV %>% str_detect("Viúvo") ~ "Viúva(a)",
    EST.CIV %>% str_detect("Casado") ~ "Casado(a)",
    EST.CIV %>% str_detect("Casada") ~ "Casado(a)",
    EST.CIV %>% str_detect("Solteiro") ~ "Solteiro(a)",
    EST.CIV %>% str_detect("Solteira") ~ "Solteiro(a)",
    EST.CIV %>% str_detect("Un.est") ~ "Un.est",
    EST.CIV %>% str_detect("Divorcia") ~ "Divorciado(a)"
  )) %>%
  filter(!is.na(EST.CIV)) %>%
  count(EST.CIV) %>%
  mutate(
    freq = n %>% percent(),
  ) %>%
  mutate(
    freq = gsub("\\.", ",", freq) %>% paste("%", sep = ""),
    label = str_c(n, " (", freq, ")") %>% str_squish()
  )

ec_plot <- ggplot(ec) +
  aes(
    x = fct_reorder(EST.CIV, n, .desc = T),
    y = n,
    label = label
  ) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, # hjust = .5,
    size = 2.5
  ) +
  ylim(0,40)+
  labs(x = "Estado Cívil", y = "Frequência") +
  theme_estat(axis.text.x = element_text(angle = 30, vjust = .5),plot.title = element_text(hjust = 0.5, size = 10,face = 'bold')) + 
  ggtitle("Estado Cívil")


grid <- grid.arrange(sexo_plot,etnia_plot,esc_plot,ec_plot, ncol = 2)

ggsave("resultados/tilda/graph_analise1.pdf", grid,width = 258, height = 193, units = "mm")

#### Idade e Renda ----

min(df$IDADE)
max(df$IDADE)
df$IDADE <- as.numeric(df$IDADE)

df$Faixa_Etaria <- cut(df$IDADE, breaks = seq(60,92,8),
                       labels = c('60 a 68',
                                  '68 a 76',
                                  '76 a 84',
                                  '84 a 92')) 

fe <- df %>%
  filter(!is.na(Faixa_Etaria)) %>%
  count(Faixa_Etaria) %>%
  mutate(
    freq = n %>% percent(),
  ) %>%
  mutate(
    freq = gsub("\\.", ",", freq) %>% paste("%", sep = ""),
    label = str_c(n, " (", freq, ")") %>% str_squish()
  )



fe_plot <- ggplot(fe) +
  aes(
    x = fct_reorder(Faixa_Etaria, n, .desc = T),
    y = n,
    label = label
  ) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, # hjust = .5,
    size = 2.5
  ) +
  ylim(0,50)+
  labs(x = "Faixa Etária", y = "Frequência") +
  theme_estat(plot.title = element_text(hjust = 0.5, size = 10,face = 'bold')) + ggtitle("Faixa Etária")

renda <- df %>% filter(!is.na(REN.FAM))

renda$REN.FAM <- as.numeric(renda$REN.FAM)
min(renda$REN.FAM)
max(renda$REN.FAM)

renda$Faixa_Renda <- cut(renda$REN.FAM, breaks = seq(0,6000,1200),
                         labels = c('R$0 a\n R$1.200',
                                    'R$1.200 a\n R$2.400',
                                    'R$2.400 a\n R$3.600',
                                    'R$3.600 a\n R$4.800',
                                    'R$4.800 a\n R$6.000'))

re <- renda %>%
  filter(!is.na(Faixa_Renda)) %>%
  count(Faixa_Renda) %>%
  mutate(
    freq = n %>% percent(),
  ) %>%
  mutate(
    freq = gsub("\\.", ",", freq) %>% paste("%", sep = ""),
    label = str_c(n, " (", freq, ")") %>% str_squish()
  )

re_plot <- ggplot(re) +
  aes(
    x = fct_reorder(Faixa_Renda, n, .desc = T),
    y = n,
    label = label
  ) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, # hjust = .5,
    size = 2.5
  ) +
  ylim(0,40)+
  labs(x = "Renda Familiar", y = "Frequência") +
  scale_x_discrete(labels= c('R$1.200\n a\n R$2.400',
                            'R$2.400\n a\n R$3.600',
                             'R$0\n a\n R$1.200',
                             'R$3.600\n a\n R$4.800',
                             'R$4.800\n a\n R$6.000'))+
  theme_estat(axis.text.x = element_text( vjust = .5,size = 8),plot.title = element_text(hjust = 0.5, size = 10,face = 'bold')) + 
  ggtitle("Renda Familiar")

grid2 <- grid.arrange(fe_plot,re_plot,ncol = 2)

ggsave("resultados/tilda/graph_analise1_1.pdf", grid2,width = 158, height = 93, units = "mm")
#### Identificar a prevalência dos agravos físicos, motores e psíquicos, além de riscos nutricionais e funcionais. ----

df1 <- df[,c(8:17)]

df1 <- df1 %>% mutate_all(as.factor)

temp <- pivot_longer(df1, cols = 1:10, names_to = "Prevalências", values_to = "Resultados") %>% filter(!is.na(Resultados))

ggplot(temp, aes(x = Prevalências, fill = Resultados))+
  geom_bar(stat = "count", position = "fill") +
  scale_fill_manual(values = cores_estat, name = "Resultados")+
  labs(x = "Prevalências", y = "Frequência")+
  scale_x_discrete(labels= c("A",'B','C','D','E','F','G','H','I','J'))+
  #coord_flip()+
  theme_estat() +
  theme(legend.position = "top") 
ggsave("resultados/tilda/graph_analise2.pdf",width = 158, height = 93, units = "mm")
  
#### Relação entre AGC-10 e o Tempo ----
temp3 <- df %>% select(`Índ AGC`,Tempo) %>% filter(!is.na(Tempo))
  