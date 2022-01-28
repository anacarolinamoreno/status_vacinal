
#Carregar pacotes

library(tidyverse)
library(tidylog)
library(zoo)
library(data.table)
library(janitor)
library(lubridate)

#Criar funções

sf <- stamp("01/20")

sf(ymd("2020-12-05"))

end.of.epiweek <- function(x, end = 6) {
  offset <- (end - 4) %% 7
  num.x <- as.numeric(x)
  return(x - (num.x %% 7) + offset + ifelse(num.x %% 7 > offset, 7, 0))
}

options(scipen = 999)

#Abrir base Sivep de SP (É um bind_rows dos arquivos SRAG 2021 e 2022, filtrando por SG == "SP")

sivep_sp <- readRDS('data-raw/sivep_sp.rds')

#SÃO PAULO - consolidado internações janeiro, segundo esquema vacinal

internacoes_sp_janeiro <- sivep_sp %>%
  filter(HOSPITAL == 1
         & diagnostico == "Covid-19"
         & DT_INTERNA > "2021-12-31"
         & DT_INTERNA <= "2022-01-15"
         & !is.na(DT_INTERNA)) %>%
  mutate(
    situacao_vacina = case_when(
      is.na(VACINA_COV) ~ "informacao_ignorada",
      VACINA_COV == 9 ~ "informacao_ignorada",
      VACINA_COV == 2 ~ "nao_tomou_vacina",
      VACINA_COV == 1 & is.na(DOSE_1_COV) & is.na(DOSE_2_COV) ~ "vacinacao_incompleta",
      VACINA_COV == 1 & !is.na(DOSE_1_COV) & is.na(DOSE_2_COV) ~ "vacinacao_incompleta",
      VACINA_COV == 1 & is.na(DOSE_1_COV) & !is.na(DOSE_2_COV) ~ "vacinacao_incompleta",
      VACINA_COV == 1 & !is.na(DOSE_1_COV) & !is.na(DOSE_2_COV) ~ "tomou_duas_doses",
      VACINA_COV != 1 & !is.na(DOSE_1_COV) ~ "data_nao_preenchida",
      T ~ "NDA"
    )
  ) %>%
  group_by(situacao_vacina) %>%
  summarise(
    contagem = sum(contagem)
  ) %>%
  pivot_wider(names_from = situacao_vacina, values_from = contagem) %>%
  mutate(
    total = informacao_ignorada +
      nao_tomou_vacina +
      tomou_duas_doses +
      vacinacao_incompleta,
    pct_info_ignorada = round((( informacao_ignorada * 100 ) / total),0),
    total_menos_ignorados = nao_tomou_vacina +
      tomou_duas_doses +
      vacinacao_incompleta,
    pct_nao_vacinados = round((( nao_tomou_vacina * 100 ) / total_menos_ignorados),0),
    pct_esquema_incompleto = round((( vacinacao_incompleta * 100 ) / total_menos_ignorados),0),
    pct_esquema_completo = round((( tomou_duas_doses * 100 ) / total_menos_ignorados),0),
    distrib_zero_dose = round(( (informacao_ignorada * pct_nao_vacinados) / 100), 0),
    distrib_uma_dose = round(( (informacao_ignorada * pct_esquema_incompleto) / 100), 0),
    distrib_duas_doses = round(( (informacao_ignorada * pct_esquema_completo) / 100), 0),
    distrib_total = distrib_zero_dose + distrib_uma_dose + distrib_duas_doses,
    sem_vacina = nao_tomou_vacina + distrib_zero_dose,
    uma_dose = vacinacao_incompleta + distrib_uma_dose,
    vacina_incompleta = sem_vacina + uma_dose,
    vacina_completa = tomou_duas_doses + distrib_duas_doses,
    total_check = sem_vacina + uma_dose + vacina_completa,
    pct_sem_vacina = round((( sem_vacina * 100 ) / total_check),0),
    pct_uma_dose = round((( uma_dose * 100 ) / total_check),0),
    pct_vacina_incompleta = round((( vacina_incompleta * 100 ) / total_check),0),
    pct_vacina_completa = round((( vacina_completa * 100 ) / total_check),0)
  )

internados_sp_janeiro <- internacoes_sp_janeiro %>%
  select(sem_vacina,
         uma_dose,
         vacina_incompleta,
         vacina_completa,
         total) %>%
  mutate(
    total_pop = 46997428,
    acumulado_d1_aplicadas = 37921266,
    acumulado_d2_aplicadas = 36379773,
    pop_com_duas_doses = acumulado_d2_aplicadas,
    pop_com_so_uma_dose = acumulado_d1_aplicadas - acumulado_d2_aplicadas,
    pop_sem_vacina = total_pop - acumulado_d1_aplicadas,
    pop_vacina_incompleta = pop_com_so_uma_dose + pop_sem_vacina,
    taxa_esquema_completo = round((( vacina_completa / pop_com_duas_doses ) * 1000000),0),
    taxa_esquema_incompleto = round((( vacina_incompleta / pop_vacina_incompleta ) * 1000000),0),
    taxa_so_uma_dose = round((( uma_dose / pop_com_so_uma_dose ) * 1000000),0),
    taxa_sem_vacina = round((( sem_vacina / pop_sem_vacina ) * 1000000),0)
  )

