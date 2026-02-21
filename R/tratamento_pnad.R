##### PRIMEIRO PORTFOLIO

##### PROJETO PNAD COVID: DIFERENÇA DE INTERNAÇÕES E DESFECHOS NA REDE PUBLICA E PRIVADA #####


setwd("G:/Meu Drive/PNAD_COVID/")

##### SCRIPT 1: TRATAMENTO DOS DADOS  #####

# Pacotes utilizados ------------------------------------------------------

pacman::p_load(tidyverse,
               lubridate,
               ggplot2,
               viridis,
               RColorBrewer,
               patchwork,
               scales,
               geobr, 
               tinytex)




# Baixando todas as bases de dados para uma úniva variável 
bases_pnad_bruta <- list.files(pattern = "^PNAD_COVID_.*\\.csv$")  

dados_pnad_bruto <- bases_pnad_bruta %>% map_dfr(read.csv)
# As base de dados são de Maio até Novembro. 



# Mantendo na base de dados somente as variáveis de interesse -------------

dados_pnad_filtrado <- dados_pnad_bruto %>% 
  select(A001,A002, A003, A004, A005, C011A12,B0041, B0042, B0043,
         B0044, B0045, B0046,B005, B006,B007, UF, V1013)



# Renomeando as variáveis -------------------------------------------------
dados_pnad_filtrado <- dados_pnad_filtrado %>% rename(n_ordem = A001,
                                                      idade = A002,
                                                      sexo = A003,
                                                      atend_publico = B0041,
                                                      ps_sus = B0042,
                                                      hosp_sus = B0043,
                                                      atend_privado = B0044,
                                                      ps_privado = B0045,
                                                      hosp_privado = B0046,
                                                      internacao = B005,
                                                      renda = C011A12,
                                                      mes_pesquisa = V1013,
                                                      estado = UF)

str(dados_pnad_filtrado)




# Tratando e criando novas variáveis --------------------------------------
base_pnad <- dados_pnad_filtrado %>% 
  mutate(sexo = if_else(sexo == 1, "Homem","Mulher"),
         faixa_etaria = case_when(idade < 18 ~ "0 - 17",
                                  idade >= 18 & idade <= 30 ~ "18 - 30",
                                  idade >= 31 & idade <= 45 ~ "31 - 45",
                                  idade >= 46 & idade <= 60 ~ "46 - 60",
                                  idade >= 61 & idade <= 75 ~ "61 - 75",
                                  idade >= 76 & idade <= 90 ~ "76 - 90",
                                  idade > 90 ~ "+ 90"),
         atendimento_tipo = case_when(atend_publico == 1 | ps_sus == 1 | hosp_sus == 1 ~ "Público",
                                      atend_privado == 1| ps_privado == 1 | hosp_privado == 1 ~ "Privado"),
         internacao = case_when(internacao == 1 ~ "Sim",
                                internacao == 2 ~ "Não",
                                internacao == 9  ~ "Ignorado",
                                internacao == NA ~ "Não aplicável"),
         estado = case_when(estado == 11 ~ "Rondônia",
                            estado == 12 ~ "Acre",
                            estado == 13 ~ "Amazonas",
                            estado == 14 ~ "Roraima",
                            estado == 15 ~ "Pará",
                            estado == 16 ~ "Amapá",
                            estado == 17 ~ "Tocantins",
                            estado == 21 ~ "Maranhão",
                            estado == 22 ~ "Piauí",
                            estado == 23 ~ "Ceará",
                            estado == 24 ~ "Rio Grande Do Norte",
                            estado == 25 ~ "Paraíba",
                            estado == 26 ~ "Pernambuco",
                            estado == 27 ~ "Alagoas",
                            estado == 28 ~ "Sergipe",
                            estado == 29 ~ "Bahia",
                            estado == 31 ~ "Minas Gerais",
                            estado == 32 ~ "Espirito Santo",
                            estado == 33 ~ "Rio De Janeiro",
                            estado == 35 ~ "São Paulo",
                            estado == 41 ~ "Paraná",
                            estado == 42 ~ "Santa Catarina",
                            estado == 43 ~ "Rio Grande Do Sul",
                            estado == 50 ~ "Mato Grosso Do Sul",
                            estado == 51 ~ "Mato Grosso",
                            estado == 52 ~ "Goiás",
                            estado == 53 ~ "Distrito Federal"),
         região = case_when(estado %in% c("Acre", "Amapá", "Amazonas", "Pará", "Rondônia", "Roraima", "Tocantins") ~ "Norte",
                            estado %in% c("Alagoas", "Bahia", "Ceará", "Maranhão", "Paraíba", "Pernambuco", "Piauí", "Rio Grande do Norte", "Sergipe") ~ "Nordeste",
                            estado %in% c("Distrito Federal", "Goiás", "Mato Grosso", "Mato Grosso do Sul") ~ "Centro-Oeste",
                            estado %in% c("Espírito Santo", "Minas Gerais", "Rio de Janeiro", "São Paulo") ~ "Sudeste",
                            estado %in% c("Paraná", "Rio Grande do Sul", "Santa Catarina") ~ "Sul"),
         mes_pesquisa = case_when(mes_pesquisa == 5 ~ "Maio",
                                  mes_pesquisa == 6 ~ "Junho",
                                  mes_pesquisa == 7 ~ "Julho",
                                  mes_pesquisa == 8 ~ "Agosto",
                                  mes_pesquisa == 9 ~ "Setembro",
                                  mes_pesquisa == 10 ~ "Outubro",
                                  mes_pesquisa == 11 ~ "Novembro")) %>% 
  select(n_ordem, idade,faixa_etaria, sexo, renda,atendimento_tipo, internacao, estado, região, mes_pesquisa)

# Transformando o nome dos mêses em fator
base_pnad$mes_pesquisa <- factor(base_pnad$mes_pesquisa,
                                 levels = c("Maio","Junho","Julho","Agosto", "Setembro","Outubro","Novembro"))

# Transformando as faias etarias em fator
base_pnad$faixa_etaria <- factor(base_pnad$faixa_etaria,
                                 levels = c("0 - 17","18 - 30","31 - 45","46 - 60", "61 - 75", "76 - 90","+ 90" ))


# INICIO DAS ANALISES -----------------------------------------------------


#### Analise sociodemografica observando a proporção por sexo e as médias de idade e renda distribuidas pelo tipo de atendimento ####
pnad_sociodem <- base_pnad %>%
  filter(!is.na(atendimento_tipo)) %>% 
  group_by(atendimento_tipo) %>%
  summarise(quantidade = n(),
            homem_prop = round(mean(sexo == "Homem"),2),
            mulher_prop = round(mean(sexo == "Mulher"),2),
            idade_media = round(mean(idade, na.rm = T),0),
            renda_media = round(mean(renda, na.rm = T),2)) %>% 
  arrange(desc(quantidade)); pnad_sociodem
write.csv2(pnad_sociodem,paste0("G:/Meu Drive/PNAD_COVID/Base_Dados/SocioDemoc_PNAD.csv"), 
           row.names = F, fileEncoding = "latin1")


#### Distibuição do sexo pelos tipos de atendimento ####
pnad_sexo <- pnad_sociodem %>% 
  select(atendimento_tipo, homem_prop, mulher_prop) %>% 
  pivot_longer(cols = ends_with("_prop"),
               names_to = "sexo",
               values_to = "proporcao") %>% 
  mutate(sexo = recode(sexo, 
                       homem_prop = "Homens",
                       mulher_prop = "Mulheres"))
write.csv2(pnad_sexo,paste0("G:/Meu Drive/PNAD_COVID/Base_Dados/Sexo_PNAD.csv"), 
           row.names = F, fileEncoding = "latin1")


#### Distribuição da quantidade e das taxas de internações na rede publica x rede privada ####
internacoes_pubpriv <- base_pnad %>%
  filter(!is.na(atendimento_tipo)) %>% 
  group_by(atendimento_tipo) %>% 
  summarise(quantidade = n(),
            taxa_internacoes = round(mean(internacao == "Sim", na.rm=T) * 100,1));internacoes_pubpriv
write.csv2(internacoes_pubpriv,paste0("G:/Meu Drive/PNAD_COVID/Base_Dados/InternacoesPubPriv_PNAD.csv"), 
           row.names = F, fileEncoding = "latin1")



#### Distribuição da quantidade e das taxas de internações na rede publica x rede privada por mês ####
internacoes_pubpriv_mes <- base_pnad %>%
  filter(!is.na(atendimento_tipo)) %>% 
  group_by(mes_pesquisa, atendimento_tipo) %>% 
  summarise(quantidade = n(),
            taxa_internacoes = round(mean(internacao == "Sim", na.rm=T) * 100,1)); internacoes_pubpriv_mes
write.csv2(internacoes_pubpriv_mes,paste0("G:/Meu Drive/PNAD_COVID/Base_Dados/InternacoesPubPriv_Mes_PNAD.csv.csv"), 
           row.names = F, fileEncoding = "latin1")




#### Distribuição da quantidade de internações por faixa etaria e sexo na rede publica x rede privada ####
internacoes_pubpriv_iddsx <- base_pnad %>% 
  filter(!is.na(faixa_etaria), !is.na(atendimento_tipo)) %>% 
  group_by(sexo, faixa_etaria, atendimento_tipo) %>% 
  summarise(quantidade = n(),
            taxa_internacao = round(mean(internacao == "Sim", na.rm=T)*100,1)) %>% 
  arrange(desc(taxa_internacao)); internacoes_pubpriv_iddsx
write.csv2(internacoes_pubpriv_iddsx,paste0("G:/Meu Drive/PNAD_COVID/Base_Dados/InternacoesPubPriv_FxEtSx_PNAD.csv.csv"), 
           row.names = F, fileEncoding = "latin1")





#### Distribuição das internações na rede publica x rede privada por estado ####
internacoes_pubpriv_estado <- base_pnad %>% 
  filter(!is.na(estado), !is.na(atendimento_tipo)) %>% 
  group_by(estado, atendimento_tipo) %>% 
  summarise(quantidade = n(),
            taxa_internacao = mean(internacao == "Sim", na.rm=T),
            renda_media = mean(renda, na.rm=T)); internacoes_pubpriv_estado
write.csv2(internacoes_pubpriv_estado,paste0("G:/Meu Drive/PNAD_COVID/Base_Dados/InternacoesPubPriv_Estado_PNAD.csv.csv"), 
           row.names = F, fileEncoding = "latin1")


estados <- read_state()

mapa_internacoes <- estados %>% 
  left_join(internacoes_pubpriv_estado, by = c("name_state" = "estado"))


# Mapa de internações públicas
mapa_internacoes_publicas <- mapa_internacoes %>% 
  filter(atendimento_tipo == "Público")

# Mapa de internações privadas
mapa_internacoes_privadas <- mapa_internacoes %>% 
  filter(atendimento_tipo == "Privado")




#### Distribuição das internação por região na rede publica x rede privada ####
internacoes_pubpriv_regiao <- base_pnad %>% 
  filter(!is.na(região), !is.na(atendimento_tipo)) %>% 
  group_by(região, atendimento_tipo) %>% 
  summarise(quantidade = n(),
            taxa_internacao = mean(internacao == "Sim", na.rm=T)); internacoes_pubpriv_regiao
write.csv2(internacoes_pubpriv_regiao,paste0("G:/Meu Drive/PNAD_COVID/Base_Dados/InternacoesPubPriv_Regiao_PNAD.csv.csv"), 
           row.names = F, fileEncoding = "latin1")








source("G:/Meu Drive/PNAD_COVID/Script/grafico_pnad.R")

#source("G:/Meu Drive/PNAD_COVID/Script/PNAD_COVID__rmarkdown.Rmd")
