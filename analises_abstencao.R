library(tidyverse)

library(sandwich)
library(lmtest)

#Queries para base de dados

# SELECT votacao.ano,
# votacao.sigla_uf,
# votacao.turno,
# sum(votacao.aptos) as total_aptos,
# sum(votacao.abstencoes) as total_abstencoes
#
# FROM `basedosdados.br_tse_eleicoes.detalhes_votacao_municipio` votacao
# where  votacao.cargo = "presidente"
# group by votacao.ano, votacao.sigla_uf, votacao.turno;
#
#
# A query abaixo foi submetida a tratamento posterior para filtar apenas os dados de segundo turno
# SELECT votacao.ano,
# votacao.sigla_uf,
# votacao.turno,
# sum(votacao.aptos) as total_aptos,
# sum(votacao.abstencoes) as total_abstencoes
#
# FROM `basedosdados.br_tse_eleicoes.detalhes_votacao_municipio` votacao
# where  votacao.cargo = "governador"
# group by votacao.ano, votacao.sigla_uf, votacao.turno;


#Dados de comparecimento para presidente primeiro e segundo turnos
comparecimentos_eleicoes <-
  read_csv("comparecimentos_eleicoes.csv")

#Dados de comparecimento para governador segundo turnos
comparecimento_governo <-
  read_csv("comparecimento_governo.csv") %>%
  mutate(houve_segundo_turno_governador = "sim")

library(tibble)

ufs_regioes <- tribble(
  ~sigla_uf, ~regiao,
  "AC", "Norte",
  "AL", "Nordeste",
  "AP", "Norte",
  "AM", "Norte",
  "BA", "Nordeste",
  "CE", "Nordeste",
  "DF", "Centro-Oeste",
  "ES", "Sudeste",
  "GO", "Centro-Oeste",
  "MA", "Nordeste",
  "MT", "Centro-Oeste",
  "MS", "Centro-Oeste",
  "MG", "Sudeste",
  "PA", "Norte",
  "PB", "Nordeste",
  "PR", "Sul",
  "PE", "Nordeste",
  "PI", "Nordeste",
  "RJ", "Sudeste",
  "RN", "Nordeste",
  "RS", "Sul",
  "RO", "Norte",
  "RR", "Norte",
  "SC", "Sul",
  "SP", "Sudeste",
  "SE", "Nordeste",
  "TO", "Norte"
)


dados_modelo<-
  comparecimentos_eleicoes %>%
  filter(!ano %in% c(1994,1998)) %>%
  left_join(comparecimento_governo %>%
              select(ano, sigla_uf,houve_segundo_turno_governador )) %>%
  inner_join(ufs_regioes) %>%
  mutate(turno = as.character(turno),
         ano =as.character(ano)) %>%
  mutate(houve_segundo_turno_governador = ifelse(is.na(houve_segundo_turno_governador),"não", "sim")) %>%
  mutate(proporcao_abstencoes = total_abstencoes/total_aptos)


modelo<- lm(proporcao_abstencoes~turno+regiao, data= dados_modelo)

summary(modelo)


modelo_2<- lm(proporcao_abstencoes~turno, data= dados_modelo)

summary(modelo_2)


modelo_3<- lm(proporcao_abstencoes~houve_segundo_turno_governador, data= dados_modelo %>% filter(turno == "2") )

summary(modelo_3)


modelo_4<- lm(proporcao_abstencoes~ano+houve_segundo_turno_governador, data= dados_modelo %>% filter(turno == "2") )

summary(modelo_4)


dados_modelo %>%
  ggplot() +
  geom_boxplot(aes(x= turno, y=  proporcao_abstencoes))



dados_modelo %>%
  filter(turno == "2") %>%
  ggplot() +
  geom_boxplot(aes(x= houve_segundo_turno_governador, y=  proporcao_abstencoes))


dados_modelo %>%
  filter(turno == "2",
         ano == 2022) %>%
  ggplot() +
  geom_boxplot(aes(x= houve_segundo_turno_governador, y=  proporcao_abstencoes))



dados_modelo %>%
  filter(turno == "2",
         ano == 2022) %>%
  summarise(quantidade = n(),
            media = mean(proporcao_abstencoes),
            median(proporcao_abstencoes),
            .by = houve_segundo_turno_governador)


dados_modelo %>%
  filter(turno == "2") %>%
  summarise(quantidade = n(),
            media = mean(proporcao_abstencoes),
            median(proporcao_abstencoes),
            .by = houve_segundo_turno_governador)

dados_teste <- dados_modelo %>%
  filter(turno == "2") %>%
  mutate(ano_2022 = if_else(ano == 2022, "2022", "outros"))

modelo_interacao_2022 <- lm(
  proporcao_abstencoes ~ ano_2022 * houve_segundo_turno_governador,
  data = dados_teste
)

summary(modelo_interacao_2022)



modelo_3 <- lm(
  proporcao_abstencoes ~ houve_segundo_turno_governador,
  data = dados_modelo %>% filter(turno == "2")
)

coeftest(
  modelo_3,
  vcov = vcovCL(
    modelo_3,
    cluster = ~ sigla_uf,
    type = "HC1"
  )
)
