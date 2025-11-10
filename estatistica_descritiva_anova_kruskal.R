# ================================================================
# Script: estatistica_descritiva_anova_kruskal.R
# Autor: Eng. Florestal MSc. Sally Deborah P. da Silva
#
# Descrição: Executa análise estatística descritiva incluindo testes
#             de normalidade (Shapiro-Wilk), ANOVA unidirecional com Tukey,
#             e teste não-paramétrico de Kruskal-Wallis com Dunn.
# Linguagem: R
# Dependências: tidyverse, car, FSA
# Data: 2025-10-27
# ================================================================

# ------------------------------------------------------------
# 1. Carregar pacotes necessários
# ------------------------------------------------------------
library(tidyverse)
library(car)
library(FSA)

# ------------------------------------------------------------
# 2. Importar dados
# ------------------------------------------------------------
arquivo <- "data/processed/Prot_raiz.csv"

dados <- read.csv(
  arquivo, header = TRUE, sep = ";", dec = ".",
  stringsAsFactors = FALSE
)

# ------------------------------------------------------------
# 3. Converter dados para formato longo
# ------------------------------------------------------------
dados_long <- dados %>%
  pivot_longer(cols = starts_with("T"), names_to = "grupo", values_to = "valor")

# ------------------------------------------------------------
# 4. Teste de normalidade (Shapiro-Wilk) por grupo
# ------------------------------------------------------------
shapiro_teste_resultados <- dados_long %>%
  group_by(grupo) %>%
  summarise(
    shapiro_p_value = shapiro.test(valor)$p.value,
    normalidade = ifelse(shapiro.test(valor)$p.value > 0.05, "Normal", "Não Normal")
  )

print(shapiro_teste_resultados)

# ------------------------------------------------------------
# 5. Selecionar método de comparação de médias
# ------------------------------------------------------------
if (all(shapiro_teste_resultados$normalidade == "Normal")) {
  cat("Todos os grupos apresentam distribuição normal. Executando ANOVA...\n")
  
  # ANOVA unidirecional
  anova_resultado <- aov(valor ~ grupo, data = dados_long)
  print(summary(anova_resultado))
  
  # Teste de Tukey
  tukey_resultado <- TukeyHSD(anova_resultado)
  print(tukey_resultado)
  plot(tukey_resultado)
  
  # Homogeneidade de variâncias (Levene)
  levene_resultado <- leveneTest(valor ~ grupo, data = dados_long)
  print(levene_resultado)
  
} else {
  cat("Pelo menos um grupo não é normal. Executando Kruskal-Wallis...\n")
  
  # Teste de Kruskal-Wallis
  kruskal_resultado <- kruskal.test(valor ~ grupo, data = dados_long)
  print(kruskal_resultado)
  
  # Teste post-hoc de Dunn (Bonferroni)
  dunn_resultado <- dunnTest(valor ~ grupo, data = dados_long, method = "bonferroni")
  print(dunn_resultado)
}

# ------------------------------------------------------------
# 6. Exportar resultados
# ------------------------------------------------------------
dir_saida <- "results/estatistica_descritiva"
if (!dir.exists(dir_saida)) dir.create(dir_saida, recursive = TRUE)

# Salvar resultados de normalidade
write.csv(shapiro_teste_resultados, file.path(dir_saida, "shapiro_resultados.csv"), row.names = FALSE)

cat("Resultados exportados para:", dir_saida, "\n")
