# ================================================================
# Script: t-test ou Mann-Whitney.R
# Autor: Eng. Florestal MSc. Sally Deborah P. da Silva
#
# Descrição: Realiza análise estatística descritiva e comparativa
#             entre plantas doentes e saudáveis quanto a macronutrientes,
#             aplicando testes de normalidade, homogeneidade e
#             comparação (t-test ou Mann-Whitney). Gera gráfico tipo
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
arquivo_csv <- "data/processed/macronutrientes.csv"

dados <- read.csv(
  arquivo_csv, sep = ";", dec = ".", fileEncoding = "UTF-8",
  header = TRUE, strip.white = TRUE, check.names = FALSE
)

# Ajustar nomes e remover colunas vazias
colnames(dados)[1] <- "classe"
dados <- dados[, colSums(!is.na(dados)) > 0]
dados$classe <- factor(dados$classe, levels = c("Plantas doentes", "Plantas saudaveis"))

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
  pivot_longer(-classe, names_to = "Nutriente", values_to = "Valor")

labels_nutrientes <- c(
  "N" = "N [%]", "P" = "P [%]", "K" = "K [%]",
  "Ca" = "Ca [%]", "Mg" = "Mg [%]", "S" = "S [%]"
)
dados_long$Nutriente <- factor(
  dados_long$Nutriente,
  levels = names(labels_nutrientes),
  labels = labels_nutrientes
)

# ------------------------------------------------------------
# 5. Função de comparação estatística
# ------------------------------------------------------------
comparar_um <- function(v_sadia, v_doente) {
  sh_s <- shapiro.test(v_sadia)$p.value
  sh_d <- shapiro.test(v_doente)$p.value
  lev <- oneway.test(
    valor ~ grupo,
    data = data.frame(valor = c(v_sadia, v_doente),
                      grupo = rep(c("S", "E"), each = length(v_sadia)))
  )$p.value
  
  if (sh_s > 0.05 && sh_d > 0.05) {
    ttest <- t.test(v_sadia, v_doente, var.equal = (lev > 0.05))
    list(
      metodo = "t-test",
      p = ttest$p.value,
      T_statistic = ttest$statistic,
      W_statistic = NA,
      sh_s = sh_s, sh_e = sh_d, lev = lev,
      m_s = mean(v_sadia), m_e = mean(v_doente),
      v_s = var(v_sadia), v_e = var(v_doente),
      efeito = mean(v_sadia) - mean(v_doente)
    )
  } else {
    wtest <- wilcox.test(v_sadia, v_doente, exact = FALSE)
    list(
      metodo = "Mann-Whitney U",
      p = wtest$p.value,
      T_statistic = NA,
      W_statistic = wtest$statistic,
      sh_s = sh_s, sh_e = sh_d, lev = lev,
      m_s = mean(v_sadia), m_e = mean(v_doente),
      v_s = var(v_sadia), v_e = var(v_doente),
      efeito = median(v_sadia) - median(v_doente)
    )
  }
}

# ------------------------------------------------------------
# 6. Resultados estatísticos comparativos
# ------------------------------------------------------------
resultado <- dados_long %>%
  group_by(Nutriente) %>%
  reframe({
    s <- Valor[classe == "Plantas saudaveis"]
    d <- Valor[classe == "Plantas doentes"]
    r <- comparar_um(s, d)
    tibble(
      Nutriente = unique(Nutriente),
      n_sadias = length(s), n_doentes = length(d),
      normal_sadias_p = r$sh_s, normal_doentes_p = r$sh_e, levene_p = r$lev,
      teste = r$metodo, p_value = r$p,
      T_statistic = r$T_statistic, W_statistic = r$W_statistic,
      media_sadias = r$m_s, media_doentes = r$m_e,
      variancia_sadias = r$v_s, variancia_doentes = r$v_e,
      efeito_diff = r$efeito
    )
  })

resultado$Nutriente <- factor(resultado$Nutriente, levels = levels(dados_long$Nutriente))

# ------------------------------------------------------------
# 7. Gráfico violin + boxplot com anotações
# ------------------------------------------------------------
p_ann <- dados_long %>%
  group_by(Nutriente) %>%
  summarise(
    y = if (unique(Nutriente) == "P [%]") max(Valor) + 0.01 else max(Valor) + (max(Valor) - min(Valor)) * 0.15,
    .groups = "drop"
  ) %>%
  left_join(resultado, by = "Nutriente") %>%
  mutate(
    signif = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01  ~ "**",
      p_value < 0.05  ~ "*",
      TRUE            ~ "ns"
    ),
    stat_txt = case_when(
      teste == "t-test" & !is.na(T_statistic) ~ paste0("t = ", round(T_statistic, 3)),
      teste == "Mann-Whitney U" & !is.na(W_statistic) ~ paste0("W = ", round(W_statistic, 1)),
      TRUE ~ ""
    ),
    label = ifelse(
      p_value < 1e-4,
      paste0(teste, "\np < 0,0001\n", stat_txt, "\n", signif),
      paste0(teste, "\np = ", formatC(p_value, format = "f", digits = 4), "\n", stat_txt, "\n", signif)
    ),
    x = 1.5
  )

g <- ggplot(dados_long, aes(x = classe, y = Valor, fill = classe)) +
  geom_violin(trim = FALSE, alpha = 0.6) +
  geom_jitter(width = 0.15, size = 0.7, alpha = 0.5) +
  geom_boxplot(width = 0.12, outlier.shape = NA, alpha = 0.85) +
  facet_wrap(~Nutriente, scales = "free_y") +
  geom_text(data = p_ann, aes(x = x, y = y, label = label), inherit.aes = FALSE, size = 3.5) +
  scale_fill_manual(values = c("Plantas doentes" = "#D9534F", "Plantas saudaveis" = "#5CB85C")) +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold"),
    panel.grid = element_blank()
  )

print(g)

# ------------------------------------------------------------
# 8. Exportação
# ------------------------------------------------------------
dir_saida <- "results/macronutrientes"
if (!dir.exists(dir_saida)) dir.create(dir_saida, recursive = TRUE)

write_xlsx(
  list(Estatisticas_Descritivas = estat_descritiva, Resultados_Testes = resultado),
  file.path(dir_saida, "resultado_macronutrientes.xlsx")
)

ggsave(file.path(dir_saida, "violinos_macronutrientes.jpeg"), g, width = 15, height = 8, dpi = 300)

cat("✅ Gráfico e planilha salvos em:", dir_saida, "\n")
