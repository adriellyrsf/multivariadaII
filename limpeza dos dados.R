#Tratamento dos dadois - Multivariada 

library(tidyverse)
library(readr)

ideb <- read_csv2("br_inep_ideb_municipio.csv")
atlas  <- read_csv2("mundo_onu_adh_municipio.csv")

unique(atlas$ano)

unique(ideb$ano)

atlas <- atlas %>%
  filter(ano == 2010)

ideb <- ideb %>%
  filter(ano == 2011)

unique(atlas$ano)
unique(ideb$ano)

unique(ideb$rede)
unique(ideb$ensino)
unique(ideb$anos_escolares)

ideb <- ideb %>%
  filter(
    rede == "publica",
    anos_escolares == "finais (6-9)"
  )

unique(ideb$rede)
unique(ideb$anos_escolares)

nrow(ideb)

names(ideb)

trad_atlas <- read_csv("traducao_atlas.csv")
trad_ideb  <- read_csv("traducao_ideb.csv")

names(trad_atlas)
names(trad_ideb)


atlas <- atlas %>%
  left_join(
    trad_atlas %>%
      select(id_municipio, nome, sigla_uf, nome_uf, nome_regiao),
    by = "id_municipio"
  )

ideb <- ideb %>%
  left_join(
    trad_atlas %>%
      select(id_municipio, nome, sigla_uf, nome_uf, nome_regiao),
    by = "id_municipio"
  )

unique(atlas$nome_regiao)

atlas <- atlas %>%
  filter(nome_regiao == "Nordeste")

ideb <- ideb %>%
  filter(nome_regiao == "Nordeste")

unique(atlas$nome_regiao)
unique(ideb$nome_regiao)

nrow(atlas)
nrow(ideb)


atlas_final <- atlas %>%
  select(
    id_municipio,
    nome,
    sigla_uf,
    idhm_e,
    idhm_r,
    renda_pc,
    indice_gini,
    prop_pobreza,
    taxa_analfabetismo_15_mais,
    expectativa_anos_estudo,
    taxa_desocupacao
  ) %>%
  mutate(
    renda_pc = renda_pc / 100,
    taxa_analfabetismo_15_mais =
      case_when(
        taxa_analfabetismo_15_mais >= 100 ~ taxa_analfabetismo_15_mais / 100,
        TRUE ~ taxa_analfabetismo_15_mais
      ),
    idhm_e = as.numeric(idhm_e),
    idhm_r = as.numeric(idhm_r),
    indice_gini = as.numeric(indice_gini),
    prop_pobreza = as.numeric(prop_pobreza),
    expectativa_anos_estudo = as.numeric(expectativa_anos_estudo),
    taxa_desocupacao = as.numeric(taxa_desocupacao)
  )


ideb_final <- ideb %>%
  select(
    id_municipio,
    taxa_aprovacao,
    ideb
  ) %>%
  transmute(
    id_municipio,
    ideb = case_when(
      ideb >= 10 ~ ideb / 10,
      TRUE ~ ideb
    )
  )
    

summary(ideb_final)
head(ideb_final, 20)

dados_final <- atlas_final %>%
  left_join(ideb_final, by = "id_municipio")

dim(dados_final)

sum(is.na(dados_final$ideb))

dados_final %>%
  filter(is.na(ideb)) %>%
  select(nome, sigla_uf) %>%
  head(20)

dados_final <- dados_final %>%
  filter(!is.na(ideb))

dim(dados_final)

sum(is.na(dados_final))


write_csv(
  dados_final,
  "dados_final_nordeste.csv"
)

