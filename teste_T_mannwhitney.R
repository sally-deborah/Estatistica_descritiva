# ================================================================
# Script: t-test_ou_Mann-Whitney.R
# Autor: Eng. Florestal MSc. Sally Deborah P. da Silva
#
# Descrição: Realiza análise estatística descritiva e comparativa
#             entre dois grupos (ex.: tratamentos, classes ou condições),
#             aplicando testes de normalidade, homogeneidade e
#             comparação (t-test ou Mann-Whitney). Gera gráficos tipo
#             violino e exporta planilhas.
# Linguagem: R
# Dependências: tidyverse, car, writexl
# Data: 2025-10-27
# ================================================================

# ------------------------------------------------------------
# 1. Carregar pacotes
# ------------------------------------------------------------
library(tidyverse)
library(car)
library(writexl)

# ------------------------------------------------------------
# 2. Importar dados
# ------------------------------------------------------------
# Caminho genérico — ajustar conforme estrutura local
arquivo_csv <- "data/processed/dados_comparativos.csv"

dados <- read.csv(
  arquivo_csv, sep = ";", dec = ".", fileEncoding = "UTF-8",
  header = TRUE, strip.white = TRUE, check.names = FALSE
)

# Ajustar nomes e remover colunas vazias
colnames(dados)[1] <- "classe"
dados <- dados[, colSums(!is.na(dados)) > 0]
dados$classe <- factor(dados$classe, levels = c("Grupo A", "Grupo B"))

# ------------------------------------------------------------
# 3. Estatística descritiva
# ------------------------------------------------------------
estat_descritiva <- dados %>%
  group_by(classe) %>%
  summarise(across(
    where(is.numeric),
    list(
      media = mean,
      mediana = median,
      desvio = sd,
      minimo = min,
      maximo = max,
      cv_perc = ~ sd(.) / mean(.) * 100
    ),
    .names = "{.col}_{.fn}"
  ), .groups = "drop")

# ------------------------------------------------------------
# 4. Reorganizar em formato longo
# ------------------------------------------------------------
dados_long <- dados %>%
  pivot_longer(-classe, names_to = "Variavel", values_to = "Valor")

# ------------------------------------------------------------
# 5. Função de comparação estatística
# ------------------------------------------------------------
comparar_um <- function(v_grupoA, v_grupoB) {
  sh_a <- shapiro.test(v_grupoA)$p.value
  sh_b <- shapiro.test(v_grupoB)$p.value
  lev <- oneway.test(
    valor ~ grupo,
    data = data.frame(valor = c(v_grupoA, v_grupoB),
                      grupo = rep(c("A", "B"), each = length(v_grupoA)))
  )$p.value
  
  if (sh_a > 0.05 && sh_b > 0.05) {
    ttest <- t.test(v_grupoA, v_grupoB, var.equal = (lev > 0.05))
    list(
      metodo = "t-test",
      p = ttest$p.value,
      T_statistic = ttest$statistic,
      W_statistic = NA,
      sh_a = sh_a, sh_b = sh_b, lev = lev,
      m_a = mean(v_grupoA), m_b = mean(v_grupoB),
      v_a = var(v_grupoA), v_b = var(v_grupoB),
      efeito = mean(v_grupoA) - mean(v_grupoB)
    )
  } else {
    wtest <- wilcox.test(v_grupoA, v_grupoB, exact = FALSE)
    list(
      metodo = "Mann-Whitney U",
      p = wtest$p.value,
      T_statistic = NA,
      W_statistic = wtest$statistic,
      sh_a = sh_a, sh_b = sh_b, lev = lev,
      m_a = mean(v_grupoA), m_b = mean(v_grupoB),
      v_a = var(v_grupoA), v_b = var(v_grupoB),
      efeito = median(v_grupoA) - median(v_grupoB)
    )
  }
}

# ------------------------------------------------------------
# 6. Resultados comparativos
# ------------------------------------------------------------
resultado <- dados_long %>%
  group_by(Variavel) %>%
  reframe({
    a <- Valor[classe == "Grupo A"]
    b <- Valor[classe == "Grupo B"]
    r <- comparar_um(a, b)
    tibble(
      Variavel = unique(Variavel),
      n_A = length(a), n_B = length(b),
      normal_A_p = r$sh_a, normal_B_p = r$sh_b, levene_p = r$lev,
      teste = r$metodo, p_value = r$p,
      T_statistic = r$T_statistic, W_statistic = r$W_statistic,
      media_A = r$m_a, media_B = r$m_b,
      variancia_A = r$v_a, variancia_B = r$v_b,
      efeito_diff = r$efeito
    )
  })

# ------------------------------------------------------------
# 7. Gráfico violino + boxplot
# ------------------------------------------------------------
p_ann <- resultado %>%
  mutate(
    signif = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01  ~ "**",
      p_value < 0.05  ~ "*",
      TRUE            ~ "ns"
    ),
    label = paste0(teste, "\n p=", formatC(p_value, digits = 4), "\n", signif)
  )

g <- ggplot(dados_long, aes(x = classe, y = Valor, fill = classe)) +
  geom_violin(trim = FALSE, alpha = 0.6) +
  geom_jitter(width = 0.15, size = 0.7, alpha = 0.5) +
  geom_boxplot(width = 0.12, outlier.shape = NA, alpha = 0.85) +
  facet_wrap(~Variavel, scales = "free_y") +
  geom_text(data = p_ann, aes(x = 1.5, y = max(dados_long$Valor) * 1.05, label = label),
            inherit.aes = FALSE, size = 3.2) +
  scale_fill_manual(values = c("Grupo A" = "#D9534F", "Grupo B" = "#5CB85C")) +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none", strip.text = element_text(face = "bold"))

print(g)

# ------------------------------------------------------------
# 8. Exportação
# ------------------------------------------------------------
dir_saida <- "results/comparacao_grupos"
if (!dir.exists(dir_saida)) dir.create(dir_saida, recursive = TRUE)

write_xlsx(
  list(Estatisticas_Descritivas = estat_descritiva, Resultados_Testes = resultado),
  file.path(dir_saida, "resultado_comparacao.xlsx")
)

ggsave(file.path(dir_saida, "violinos_comparacao.jpeg"), g, width = 15, height = 8, dpi = 300)

cat("✅ Gráfico e planilha salvos em:", dir_saida, "\n")
