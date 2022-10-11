
# Tabagismo em países capitalistas e comunistas --------------------------------------------------------------------------------------------
# Autora do script: Jeanne Franco ----------------------------------------------------------------------------------------------------------
# Data: 05/10/22 ---------------------------------------------------------------------------------------------------------------------------
# Referência: https://ourworldindata.org/smoking -------------------------------------------------------------------------------------------

# Sobre os dados ---------------------------------------------------------------------------------------------------------------------------

### Tabagismo é um dos maiores problemas de saúde no mundo. Milhões de pessoas
### vivem em condições precárias de saúde por causa do tabagismo e pesquisadores
### estimam que a cada ano cerca de 8 milhões de pessoas morrem prematuramente
### devido ao tabagismo.

### Tabagismo tem sido o principal problema de saúde por muitas décadas. Por todo
### o século 20 é estimado que cerca de 100 milhões de pessoas morreram prematuramente
### por causa do tabagismo, a maioria delas em países mais ricos.

### A porcentagem de fumantes na população mundial está caindo e devido ao tabagismo
### ser um grande problema de saúde hoje, isto é um dos avanços mais positivos na
### saúde global. Isso torna possível que milhões de pessoas possam viver uma vida
### mais longa e mais saudável.

# Carregar pacotes -------------------------------------------------------------------------------------------------------------------------

library(tidyverse)
library(cols4all)
library(hrbrthemes)
library(ggthemes)

# Carregar dados ---------------------------------------------------------------------------------------------------------------------------

smok <- read.csv("share-deaths-smoking.csv")
view(smok)
names(smok)

# Manipular dados --------------------------------------------------------------------------------------------------------------------------

smok <- smok %>%
  select(-Code) %>%
  rename(por_tabag = Deaths...Cause..All.causes...Risk..Smoking...OWID...Sex..Both...Age..Age.standardized..Percent.) %>%
  view()

smok1 <- smok %>%
  filter(Entity %in% c("United States", "Germany", "Japan",
                       "China", "Cuba", "North Korea")) %>%
  group_by(Entity) %>%
  summarise(media = mean(por_tabag),
            sd = sd(por_tabag), n = n(),
            se = sd/sqrt(n)) %>%
  view()

smok2 <- smok %>%
  filter(Entity %in% c("United States", "Germany", "Japan",
                       "China", "Cuba", "North Korea")) %>%
  view()

smok3 <- smok %>%
  filter(Entity %in% c("United States", "China")) %>%
  view()

# Gráficos ---------------------------------------------------------------------------------------------------------------------------------

c4a("safe", 6)

ggplot(smok1, aes(x = fct_reorder(Entity, media), 
                  y = media, fill = Entity)) +
  geom_col(width = 0.9) +
  geom_errorbar(aes(ymin = media - se, ymax = media + se),
                size = 0.8, width = 0.2) +
  scale_fill_manual(values = c("#88CCEE", "#CC6677",
                               "#DDCC77", "#117733",
                                "#332288", "#AA4499")) +
  scale_y_continuous(expand = expansion(mult = c(0,0))) +
  scale_x_discrete(labels = c("Alemanha", "Coreia do Norte", "Japão", 
                              "Estados Unidos", "Cuba", "China")) +
  labs(x = "Países", y = "Porcentagem de mortes 
                         atribuídas ao tabagismo") +
  theme_ipsum(axis_title_size = 16,
              axis_text_size = 14) +
  theme(legend.position = "none",
        axis.text = element_text(color = "black"))

ggplot(smok2, aes(x = Year, y = por_tabag, group = Entity, col = Entity)) +
  geom_point(shape = 15, size = 2.5) +
  geom_line(size = 1.2) +
  scale_color_manual(values = c("#88CCEE", "#CC6677",
                               "#DDCC77", "#117733",
                                "#332288", "#AA4499"),
                     labels = c("China", "Cuba", "Alemanha",
                                "Japão", "Coreia do Norte", "Estados Unidos")) +
  labs(x = "Tempo (anos)", y = "Porcentagem de mortes 
                         atribuídas ao tabagismo", 
       color = "Países") +
  theme_ipsum(axis_title_size = 16,
              axis_text_size = 14) +
  theme(axis.text = element_text(color = "black"))

# Estados Unidos x China -------------------------------------------------------------------------------------------------------------------

c4a_gui()
c4a("dark2", 2)

ggplot(smok3, aes(x = Year, y = por_tabag, 
                  group = Entity, col = Entity)) +
  geom_line(size = 2.2) +
  scale_color_manual(values = c("#1B9E77", "#D95F02"),
                     labels = c("China", "Estados Unidos")) +
  labs(x = "Tempo (anos)", y = "Mortes por tabagismo (%)", 
       color = "Países") +
  theme_hc() +
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(color = "black", size = 15),
        legend.text = element_text(size = 12))
