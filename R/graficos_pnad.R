##### PROJETO PNAD COVID: DIFERENÇA DE INTERNAÇÕES E DESFECHOS NA REDE PUBLICA E PRIVADA #####


##### SCRIPT 2: GERANDO OS GRÁFICOS  #####


# Instalando os pacotes a serem usados ------------------------------------

pacman::p_load(lubridate,
               ggplot2,
               viridis,
               RColorBrewer,
               patchwork,
               scales,
               geobr)


# Selecionando as cores que serão usadas nos gráficos 
display.brewer.all()
cores <- brewer.pal(9, "Paired")
minhas_cores <- cores[c(2, 4)]
sexo_cores <- cores[c(1,5)]

# GRAFICOS ----------------------------------------------------------------



#### Media de idade pelo tipo de atendimento  ####
ggplot(pnad_sociodem, aes(x = atendimento_tipo, y = idade_media, fill = atendimento_tipo))+
  geom_col(width = 0.7)+
  geom_text(aes(label = idade_media), vjust = -0.3,size = 4)+
  scale_fill_manual(values = minhas_cores)+
  labs(title = "Média de idade dos pacientes pelo tipo de atendimento",
       x = "Tipo de atendimento",
       y = "Idade média",
       fill = "Tipo de atendimento")+
  theme_classic(base_size = 13)
ggsave(".../Graficos/media_idade.png", width = 8, heigh = 4)



#### Renda média por tipo de atendimento: ####
ggplot(pnad_sociodem,aes(x = atendimento_tipo, y = renda_media, fill = atendimento_tipo))+
  geom_col(width = 0.7)+
  geom_text(aes(label = paste0("R$ ",round(renda_media,0))), vjust= -0.3, size = 4)+
  scale_fill_manual(values = minhas_cores)+
  labs(title = "Renda média por tipo de atendimento",
       x = "Tipo de atendimento",
       y = "Renda média",
       fill = "Tipo de atendimento")+
  theme_classic(base_size = 13)
ggsave(".../Graficos/renda_media.png", width = 8, heigh = 4)


#### Distibuição do sexo pelos tipos de atendimento ####
ggplot(pnad_sexo, aes(x = "", y = proporcao, fill = sexo)) +
  geom_col(width = 2) +
  coord_polar("y") +
  facet_wrap(~ atendimento_tipo) +
  scale_fill_manual(values = sexo_cores) +
  geom_text(aes(label = percent(proporcao, accuracy = 1)),
            position = position_stack(vjust = 0.5),
            size = 7) +
  labs(title = "Distribuição por sexo dentro de cada tipo de atendimento",
       fill = "Sexo") +
  theme_void(base_size = 13) +
  theme(
    legend.text = element_text(size = 14),      
    legend.title = element_text(size = 16),     
    plot.title = element_text(size = 16, face = "bold"),
    strip.text = element_text(size = 16, face = "bold"))
ggsave(".../Graficos/sexo_distribuicao.png", width = 8, heigh = 4)




#### Diferença nas internações entre as  redes públicas  privadas ####
ggplot(internacoes_pubpriv, aes(x = atendimento_tipo, y = taxa_internacoes, fill = atendimento_tipo))+
  geom_col()+
  geom_text(aes(label = paste0(taxa_internacoes, "% (n = ",quantidade, ")")),
            vjust = -0.5,
            size = 3.5)+
  scale_fill_manual(values = minhas_cores)+
  labs(title = "Taxa de internações por atendimento publico x privado",
       x = "Tipo de atendimento",
       y = "Taxa (%)",
       fill = "Tipo de atendimento")+
  theme_classic(base_size = 13)
ggsave(".../Graficos/taxa_internacoes.png", width = 8, heigh = 4)


#### Taxa de Internações por mes ####
# Grafico com a taxa de internação:
tx_int <- ggplot(internacoes_pubpriv_mes, aes(x = mes_pesquisa, y = taxa_internacoes, color = atendimento_tipo, group = atendimento_tipo))+
  geom_line(size = 1.2)+
  geom_point(size = 3)+
  geom_text(aes(label = paste0(taxa_internacoes, "%")),
            #vjust = -0.5,
            nudge_y = 0.3,
            size = 3.5,
            color = "gray0",
            fontface = "bold")+
  scale_color_manual(values = minhas_cores)+
  labs(title = "Taxa de internações por mês na rede pública e privada",
       x = "Mês",
       y = "Taxa (%)",
       color = "Tipo de atendimento")+
  theme_classic(base_size = 10)+
  theme(plot.title = element_text(face = "bold")); tx_int

# Grafico com a quantidade de internações por mês
qtd_int <- ggplot(internacoes_pubpriv_mes, aes(x = mes_pesquisa, y = quantidade, color = atendimento_tipo, group = atendimento_tipo))+
  geom_line(size = 1.2)+
  geom_point(size = 3)+
  geom_text(aes(label = quantidade),
            #vjust = -0.5,
            nudge_y = 0.3,
            size = 3.5,
            color = "gray0",
            fontface = "bold")+
  scale_color_manual(values = minhas_cores)+
  labs(title = "Quantidade de internações por mês na rede pública e privada",
       x = "Mês",
       y = "Quantidade",
       color = "Tipo de atendimento")+
  theme_classic(base_size = 10)+
  theme(plot.title = element_text(face = "bold")); qtd_int

# Gráfico com a taxa e a quantidade de internações por mês mesclados
internacoes_mes_linha <- tx_int / qtd_int; internacoes_mes_linha
ggsave(".../Graficos/taxa_internacao_mes.png", width = 8, heigh = 4)



#### Internações por faixa etaria, sexo e tipo de atendimento ####
ggplot(internacoes_pubpriv_iddsx, aes(x = faixa_etaria, y = taxa_internacao, fill = atendimento_tipo))+
  geom_col(position = position_dodge(width = 0.8), width = 0.5)+
  facet_wrap(~sexo)+
  geom_text(aes(label = paste0(round(taxa_internacao,1),"%")),
            position = position_dodge(width = 0.8),
            vjust = -0.9,
            size = 2.6,
            fontface = "bold",
            color = "gray20")+
  scale_fill_manual(values = minhas_cores)+
  scale_y_continuous(expand = expansion(mult = c(0.05,0.15)))+
  labs(title = "Taxa de internação por sexo, faixa etaria e tipo de atendimento",
       x = "Faixa Etaria",
       y = "Taxa de Internação (%)",
       fill = "Tipo de Atendimento")+
  theme_classic(base_size = 11)+
  theme(legend.position = "bottom")
ggsave(".../Graficos/taxa_internacao_sexo_faixaetaria.png")



#### Mapa de calor das internações público x privado ####
# Mapa de internações públicas
mapa_publico <- ggplot(mapa_internacoes_publicas)+
  geom_sf(aes(fill = quantidade), color = "white", size = 0.2)+
  scale_fill_gradient(low = "white",high  = "#33A02C")+
  labs(title = "Quantidade de internações na rede pública por estado")+
  theme_void()


# Mapa de internações privadas
mapa_privado <- ggplot(mapa_internacoes_privadas)+
  geom_sf(aes(fill = quantidade), color = "white", size = 0.2)+
  scale_fill_gradient(low = "white",high  = "#1F78B4")+
  labs(title = "Quantidade de internações na rede privada por estado")+
  theme_void()


mapas_int <- mapa_publico / mapa_privado
ggsave(".../Graficos/mapas_internacao.png", width = 10, heigh = 10)



#### Diferenças regionais no tipo de atendimento ####
ggplot(internacoes_pubpriv_regiao,
       aes(x = taxa_internacao, y = região, fill = atendimento_tipo)) +
  geom_col(position = position_dodge(width = 0.6)) +
  geom_text(aes(label = paste0(round(taxa_internacao,3)*100, "%")),
            position = position_dodge(width = 0.6),
            nudge_x = 0.002,
            size = 3, 
            fontfac = "bold",
            color = "grey0") +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                     expand = expansion(mult = c(0.02,0.15))) +
  scale_fill_manual(values = minhas_cores) +
  labs(title = "Taxa de internação por região e tipo de atendimento",
       x = "Taxa de internação (%)",
       y = "Região",
       fill = "Tipo de atendimento") +
  theme_classic(base_size = 11)+
  theme(legend.position = "bottom")
ggsave(".../Graficos/internacao_regiao.png", width = 8, heigh = 4)


