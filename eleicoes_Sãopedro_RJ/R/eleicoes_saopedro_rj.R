
# Pacotes -----------------------------------------------------------------
pacman::p_load("dplyr", "ggplot2", "janitor", "electionsBR", "gt", "litedown" )

# Dados -------------------------------------------------------------------

#coleta de dados

df <- elections_tse(
  year = 2024,
  type = "voter_profile",
  uf   = "RJ"
) %>%
  clean_names() %>%
  dplyr::select(
    nm_municipio,
    ds_grau_escolaridade,
    qt_eleitores_perfil
  ) %>%
  filter(nm_municipio == "SÃO PEDRO DA ALDEIA")


op <- df %>%
  group_by(ds_grau_escolaridade) %>%
  summarise(total = sum(qt_eleitores_perfil, na.rm = TRUE)) %>%
  mutate(percent = round((total/sum(total))*100,2))



# Tabela ------------------------------------------------------------------

install.packages("litedown")
library(dpl)
#construção da tabela

op %>%
  gt() %>%
  tab_header(  title = "Eleitorado em São Pedro da Aldeia",
               subtitle = "Escolaridade"
  ) %>%
  cols_label(ds_grau_escolaridade = "Grau de escolaridade",
             total = "Eleitorado",
             percent = "(%)") %>%
  tab_source_note(source_note = "**Fonte**: TSE (Via ElectionsBR")



# Gráfico -----------------------------------------------------------------


ggplot(op, aes(x = reorder(ds_grau_escolaridade, -percent), y = percent)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = paste0(percent, "%")), hjust = -0.1, size = 3.5) +
  labs(
    title = "Distribuição dos Eleitores por Grau de Escolaridade - São Pedro da Aldeia (2024)",
    x = "Grau de Escolaridade",
    y = "Percentual de Eleitores"
  ) +
  theme_minimal() +
  coord_flip()



# Dados zonas eleitorais --------------------------------------------------

dados <- elections_tse(year = 2024,
                       type = "vote_section",
                       uf = "RJ",
                       br_archive = TRUE) %>%
  clean_names()

df_p <- dados %>%
  select(cd_municipio, nm_municipio, ds_cargo, nm_votavel, qt_votos,
         nm_local_votacao, ds_local_votacao_endereco) %>%
  filter(nm_municipio == "SÃO PEDRO DA ALDEIA",
         ds_cargo == "Prefeito")


# Visualização ------------------------------------------------------------

ggplot(df_p, aes(x  = nm_local_votacao, y = qt_votos, fill = nm_votavel))+
  geom_bar(stat = "identity", position = "fill")+ #usa fill para normalizar
  theme_bw()+
  geom_hline(yintercept = .5, linetype = 2, size = 1)+
  coord_flip()+
  scale_y_continuous(labels = scales::percent_format()) + # mostra o eixo em %
  labs(
    title = "Distribuição percentual de votos por local de votação - São Pedro da Aldeia (2024)",
    subtitle = "Proporção de votos para cada candidato a Prefeito",
    x = "Local de votação",
    y = "Proporção dos votos (%)",
    fill = "Candidato"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.y = element_text(size = 9),
    axis.text.x = element_text(size = 9),
    legend.position = "bottom",  # 🔹 move a legenda para baixo
    legend.title = element_text(face = "bold"),
    panel.grid.major.y = element_blank(), # remove grade lateral
    panel.grid.minor = element_blank()
  )


