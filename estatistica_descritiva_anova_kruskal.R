# ================================================================
# Script: estatistica_descritiva_anova_kruskal.R
# Autor: Eng. Florestal MSc. Sally Deborah P. da Silva
#
# Descrição: Executa análise estatística descritiva incluindo testes
#             de normalidade (Shapiro-Wilk), ANOVA unidirecional com Tukey
#             e teste não-paramétrico de Kruskal-Wallis com Dunn.
# Linguagem: R
# Dependências: tidyverse, car, FSA
# Data: 2025-10-27
# ================================================================

# ------------------------------------------------------------
# 1. Carregar pacotes
# ------------------------------------------------------------
library(tidyverse)
library(car)
library(FSA)

# ------------------------------------------------------------
# 2. Importar dados
# ------------------------------------------------------------
arquivo <- "data/processed/dados_anova.csv"
dados <- read.csv(arquivo, header = TRUE, sep = ";", dec = ".", stringsAsFactors = FALSE)

# ------------------------------------------------------------
# 3. Converter para formato longo
# ------------------------------------------------------------
dados_long <- dados %>%
  pivot_longer(cols = starts_with("T"), names_to = "grupo", values_to = "valor")

# ------------------------------------------------------------
# 4. Teste de normalidade (Shapiro-Wilk)
# ------------------------------------------------------------
shapiro_res <- dados_long %>%
  group_by(grupo) %>%
  summarise(
    p_value = shapiro.test(valor)$p.value,
    normalidade = ifelse(p_value > 0.05, "Normal", "Não Normal"),
    .groups = "drop"
  )

print(shapiro_res)

# ------------------------------------------------------------
# 5. Escolher método de comparação
# ------------------------------------------------------------
if (all(shapiro_res$normalidade == "Normal")) {
  cat("Todos os grupos apresentam distribuição normal → ANOVA\n")
  
  anova_res <- aov(valor ~ grupo, data = dados_long)
  print(summary(anova_res))
  
  tukey_res <- TukeyHSD(anova_res)
  print(tukey_res)
  plot(tukey_res)
  
  levene_res <- leveneTest(valor ~ grupo, data = dados_long)
  print(levene_res)
  
} else {
  cat("Pelo menos um grupo não é normal → Kruskal-Wallis\n")
  
  kruskal_res <- kruskal.test(valor ~ grupo, data = dados_long)
  print(kruskal_res)
  
  dunn_res <- dunnTest(valor ~ grupo, data = dados_long, method = "bonferroni")
  print(dunn_res)
}

# ------------------------------------------------------------
# 6. Exportar resultados
# ------------------------------------------------------------
dir_saida <- "results/anova_kruskal"
if (!dir.exists(dir_saida)) dir.create(dir_saida, recursive = TRUE)
write.csv(shapiro_res, file.path(dir_saida, "normalidade.csv"), row.names = FALSE)

cat("✅ Resultados exportados para:", dir_saida, "\n")
