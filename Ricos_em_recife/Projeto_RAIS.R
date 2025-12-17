#-----------------------------------------------------------------------
#  CARRENGANDO PACOTES E DADOS
#-----------------------------------------------------------------------
if(!require(pacman)) install.packages("pacman")
pacman::p_load("gt", "janitor", "caTools", "effects", "pROC", "psych",
               "car", "DescTools", "QuantPsyc", "tidyverse", "forestplot", "stringr", "sjPlot")
df <- read.csv("RAIS_idade.csv")
dicionario <- read.csv("dicionario.csv")
#-----------------------------------------------------------------------#
#  ANÁLISE EXPLORATÓRIA E TRATAMENTO DOS DADOS                          #
#-----------------------------------------------------------------------#

# --- Mapeamento das variáveis categóricas (usando o dicionário) ---    #

mapa_sexo <- dicionario %>%
  filter(nome_coluna == "sexo") %>%
  dplyr::select(chave, valor) %>%
  deframe()

mapa_raca_cor <- dicionario %>%
  filter(nome_coluna == "raca_cor") %>%
  dplyr::select(chave, valor) %>%
  deframe()

mapa_grau_instrucao <- dicionario %>%
  filter(nome_coluna == "grau_instrucao_apos_2005") %>%
  dplyr::select(chave, valor) %>%
  deframe()

# --- Aplica o mapeamento e limpa os dados ---

df_tratado <- df %>%
  mutate(
    sexo = dplyr::recode(sexo, !!!mapa_sexo),
    raca_cor = dplyr::recode(raca_cor, !!!mapa_raca_cor),
    grau_instrucao_apos_2005 = dplyr::recode(grau_instrucao_apos_2005, !!!mapa_grau_instrucao)
  ) %>%
  # Filtra valores inválidos ou não identificados
  filter(
    sexo != "Código não encontrado nos dicionários oficiais.",
    raca_cor != "Não identificado",
    grau_instrucao_apos_2005 != "Código não encontrado nos dicionários oficiais.",
    !is.na(valor_remuneracao_media),
    !is.na(idade)
  )

# --- Análise Exploratória  ---

#### --- Remuneração média --- ### 
media_por_sexo <- df_tratado %>% 
  filter(sexo != "Código não encontrado nos dicionários oficiais.") %>% 
  group_by(sexo) %>% 
  summarise(media_remuneracao = mean(valor_remuneracao_media, na.rm = TRUE)) %>% 
  arrange(desc(media_remuneracao))

#agrupar por grau de instrução
media_instrucao <- df_tratado %>% 
  filter(grau_instrucao_apos_2005 != "Código não encontrado nos dicionários oficiais.") %>%
  group_by(grau_instrucao_apos_2005) %>% 
  summarise(media_remuneracao = mean(valor_remuneracao_media, na.rm = TRUE)) %>%
  arrange(desc(media_remuneracao))

#agrupar por raça/cor e calcular a remuneração média

media_por_raca <- df_tratado %>% 
  filter(raca_cor != "Não identificado" ) %>%
  group_by(raca_cor) %>% 
  summarise(media_remuneracao = mean(valor_remuneracao_media, na.rm = TRUE)) %>% 
  arrange(desc(media_remuneracao))

################# FUNÇÃO PARA GRÁFICO#######################33

# Exemplo de uma função para criar os gráficos
grafico_remuneracao <- function(data, eixo_x, titulo_grafico) {
  ggplot(data, aes(x = reorder({{eixo_x}}, media_remuneracao), y = media_remuneracao)) +
    geom_col(fill = "#2E86AB") +
    geom_text(aes(label = round(media_remuneracao, 0)), vjust = -0.4, size = 3.5) +
    labs(
      title = titulo_grafico,
      x = "", # Deixa o eixo x sem nome para ficar mais limpo
      y = "Remuneração Média (R$)",
      caption = "Fonte: RAIS"
    ) +
    theme_bw() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      axis.text.x = element_text(angle = 25, hjust = 1)
    ) +
    coord_flip()
}

grafico_remuneracao(media_por_raca, raca_cor, "Remuneração Média por Raça/Cor no Recife")
grafico_remuneracao(media_por_sexo, sexo, "Remuneração Média por Sexo no Recife")
grafico_remuneracao(media_instrucao, grau_instrucao_apos_2005,
                    "Remuneração Média por grau de instrução no Recife")
# Remuneração média por faixa etária ( a cada 10 anos)
df_tratado %>%
  mutate(faixa_etaria = cut(idade, breaks = seq(15, 75, by = 10), right = FALSE)) %>%
  group_by(faixa_etaria) %>%
  summarise(media_remuneracao = mean(valor_remuneracao_media, na.rm = TRUE)) %>%
  filter(!is.na(faixa_etaria)) %>%
  ggplot(aes(x = faixa_etaria, y = media_remuneracao, group = 1)) +
  geom_line(color = "darkgreen") +
  geom_point(color = "darkgreen") +
  labs(
    title = "Curva Salarial por Faixa Etária no Recife",
    x = "Faixa Etária",
    y = "Remuneração Média (R$)"
  ) +
  theme_bw()

# --- Preparação dos dados para o modelo ---

# Ponto de corte para ser considerado 10% mais ricos (percentil 90)
p90 <- quantile(df_tratado$valor_remuneracao_media, 0.9, na.rm = TRUE)
print(paste("Ponto de corte para 10% mais ricos (P90): R$", round(p90, 2)))

# Seleção final e criação da variável alvo
dados_modelo <- df_tratado %>%
  mutate(alta_renda = ifelse(valor_remuneracao_media >= p90, 1, 0)) %>%
  # Seleciona as colunas de interesse, incluindo a idade
  dplyr::select(alta_renda, sexo, raca_cor, grau_instrucao_apos_2005, idade) %>%
  # Transforma categóricas em fatores para o modelo
  mutate(
    sexo = as.factor(sexo),
    raca_cor = as.factor(raca_cor),
    grau_instrucao_apos_2005 = as.factor(grau_instrucao_apos_2005)
  )

# Verifica a estrutura dos dados que irão para o modelo
glimpse(dados_modelo)

#----------------------------------------------------------------------- #
# MODELAGEM PREDITIVA (REGRESSÃO LOGÍSTICA)                             #
#-----------------------------------------------------------------------#

# --- Divisão em treino e teste --- #
set.seed(777) # Para reprodutibilidade
split <- sample.split(dados_modelo$alta_renda, SplitRatio = 0.7)
treino <- subset(dados_modelo, split == TRUE)
teste <- subset(dados_modelo, split == FALSE)

# --- Treinamento do modelo --- #
modelo_logit <- glm(alta_renda ~ ., data = treino, family = binomial)


#-----------------------------------------------------------------------
# AVALIAÇÃO E INTERPRETAÇÃO DO MODELO #
#-----------------------------------------------------------------------

# --- Interpretação dos coeficientes ---
# Mostra o "peso" e a significância de cada variável
summary(modelo_logit)

# gerar tabela do modelo
tab_model(modelo_logit, file = "modelo_logit.html")

# ----------- Previsões e Matriz de Confusão --- #
previsoes <- predict(modelo_logit, newdata = teste, type = "response")

#descobrir o melhor ponto de corte
roc_curve <- roc(teste$alta_renda, previsoes)
melhor_corte <- coords(roc_curve, "best", ret = "threshold")
print(paste("Melhor ponto de corte:", round(melhor_corte, 3)))

# Usando o ponto de corte de 0.132 
classes_preditas <- ifelse(previsoes > 0.132, 1, 0)

matriz_confusao <- table(Observado = teste$alta_renda, Predito = classes_preditas)
print("Matriz de Confusão (com corte = 0.132):")
print(matriz_confusao)

# --- Métricas de Desempenho e robustez --- #

summary(stdres(modelo_logit))

# Adicionar os resíduos padronizados como uma nova coluna no df treino
treino$residuos_pad <- stdres(modelo_logit)

maiores_erros_treino <- treino %>%
  arrange(desc(residuos_pad)) %>%  
  head(5)                        

# observações que o modelo teve mais dificuldade para ajustar
print(maiores_erros_treino) 

#teste de mult.
vif(modelo_logit)

acuracia <- sum(diag(matriz_confusao)) / sum(matriz_confusao)
sensibilidade <- matriz_confusao[2, 2] / (matriz_confusao[2, 2] + matriz_confusao[2, 1])
precisao <- matriz_confusao[2, 2] / (matriz_confusao[2, 2] + matriz_confusao[1, 2])

print(paste("Acurácia:", round(acuracia, 4)))
print(paste("Sensibilidade (Recall):", round(sensibilidade, 4)))
print(paste("Precisão:", round(precisao, 4)))

f1_score <- 2 * (precisao * sensibilidade) / (precisao + sensibilidade)
print(f1_score)


# --- Curva ROC e AUC ---
roc_obj <- roc(response = teste$alta_renda, predictor = previsoes)
auc_valor <- auc(roc_obj)

  plot(roc_obj, col = "blue", lwd = 2, main = paste("Curva ROC - AUC =", round(auc_valor, 2)))
  abline(a = 0, b = 1, lty = 2, col = "gray")

print(paste("AUC:", round(auc_valor, 4)))


####################### GRÁFICO FOREST #####################################################
# Extrair os dados do modelo #
tabela_or <- exp(cbind(OR = coef(modelo_logit), confint(modelo_logit)))

# Formatar os dados para o gráfico #
tabela_or_df <- as.data.frame(tabela_or)
colnames(tabela_or_df) <- c("OR", "CI_low", "CI_high")
tabela_or_df$Variavel <- rownames(tabela_or_df)

tabela_or_df <- tabela_or_df %>%
  filter(Variavel != "(Intercept)")

# Ajustes nos labels #
tabela_or_df <- tabela_or_df %>%
  mutate(
    Variavel = str_replace(Variavel, "^(sexo|raca_cor|grau_instrucao_apos_2005)", "")
  )


# Preparar os dados de texto (lado esquerdo do gráfico)
tabela_or_df <- tabela_or_df %>%
  mutate(
    OR_formatado = sprintf("%.2f (%.2f - %.2f)", OR, CI_low, CI_high)
  )

label_texto <- tabela_or_df %>%
  dplyr::select(Variavel, OR_formatado) %>%
  rbind(c("Preditor", "OR (IC 95%)"), .)

# dados numéricos (lado direito do grafico) #
dados_plot <- tabela_or_df %>%
  dplyr::select(OR, CI_low, CI_high) %>%
  rbind(c(NA, NA, NA), .) %>%
  mutate_all(as.numeric)

# gerando o forest plot
forestplot(
  labeltext = as.matrix(label_texto),
  mean = dados_plot$OR,
  lower = dados_plot$CI_low,
  upper = dados_plot$CI_high,
  title = "Determinantes de 10% mais ricos (Odds Ratios)",
  is.summary = c(TRUE, rep(FALSE, nrow(tabela_or_df))),
  xlog = TRUE,
  xlab = "Odds Ratio (Escala Logarítmica)",
  zero = 1,
  boxsize = 0.25,
  col = fpColors(box = "royalblue", line = "darkblue", summary = "royalblue"),
  grid = TRUE
)









