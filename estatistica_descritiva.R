# ================================================================
# Script: estatistica_descritiva.R
# Autor: Eng. Florestal MSc. Sally Deborah P. da Silva
#
# Descrição: Calcula estatísticas descritivas (média, mediana, desvio,
#             mínimo, máximo, coeficiente de variação) e testa a
#             normalidade das variáveis numéricas (Shapiro-Wilk).
# Linguagem: R
# Dependências: tidyverse
# Data: 2025-10-27
# ================================================================

# ------------------------------------------------------------
# 1. Carregar pacotes
# ------------------------------------------------------------
library(tidyverse)

# ------------------------------------------------------------
# 2. Importar dados
# ------------------------------------------------------------
arquivo <- "data/processed/xx.csv"

dados <- read.csv(
  arquivo, sep = ";", dec = ".", header = TRUE,
  stringsAsFactors = FALSE, check.names = FALSE
)

# ------------------------------------------------------------
# 3. Estatística descritiva com CV%
# ------------------------------------------------------------
estatisticas <- dados %>%
  select(where(is.numeric)) %>%
  summarise(across(
    everything(),
    list(
      media = mean,
      mediana = median,
      desvio = sd,
      minimo = min,
      maximo = max,
      cv_perc = ~ sd(.) / mean(.) * 100
    ),
    .names = "{.col}_{.fn}"
  ))

print("📊 Estatística descritiva com CV%:")
print(estatisticas)

# ------------------------------------------------------------
# 4. Teste de normalidade (Shapiro-Wilk)
# ------------------------------------------------------------
normalidade <- dados %>%
  select(where(is.numeric)) %>%
  map(~ shapiro.test(.)$p.value) %>%
  enframe(name = "Variavel", value = "Shapiro_p") %>%
  mutate(Normal = ifelse(Shapiro_p > 0.05, "Sim", "Não"))

print("🧪 Teste de normalidade (Shapiro-Wilk):")
print(normalidade)

# ------------------------------------------------------------
# 5. Exportar resultados
# ------------------------------------------------------------
dir_saida <- "results/estatistica_descritiva"
if (!dir.exists(dir_saida)) dir.create(dir_saida, recursive = TRUE)

write.csv(estatisticas, file.path(dir_saida, "estatisticas_descritivas.csv"), row.names = FALSE)
write.csv(normalidade, file.path(dir_saida, "normalidade_shapiro.csv"), row.names = FALSE)

cat("✅ Resultados exportados para:", dir_saida, "\n")

# ------------------------------------------------------------
# 6. (Opcional) ANOVA e Kruskal-Wallis
# ------------------------------------------------------------
# if (length(unique(dados$classe)) > 1) {
#   for (var in names(dados)[-1]) {
#     cat("\nAnalisando variável:", var, "\n")
#     anova_res <- aov(dados[[var]] ~ dados$classe)
#     print(summary(anova_res))
#     kruskal_res <- kruskal.test(dados[[var]] ~ dados$classe)
#     print(kruskal_res)
#   }
# } else {
#   cat("⚠️ Apenas uma classe presente. ANOVA/Kruskal-Wallis não aplicável.\n")
# }
