
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
                       "China", "Cuba", "Noth Korea")) %>%
  group_by(Entity) %>%
  summarise(media = mean(por_tabag),
            sd = sd(por_tabag), n = n(),
            se = sd/sqrt(n)) %>%
  view()

smok2 <- smok %>%
  filter(Entity %in% c("United States", "Germany", "Japan",
                       "China", "Cuba", "Noth Korea")) %>%
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
  labs(x = "Países", y = "Porcentagem de mortes 
                         atribuídas ao tabagismo") +
  theme_ipsum(axis_title_size = 16,
              axis_text_size = 14) +
  theme(legend.position = "none",
        axis.text = element_text(color = "black"))



