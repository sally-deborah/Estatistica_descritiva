# ================================================================
# Script: testes_t_f_grupos.R
# Autor: Eng. Florestal MSc. Sally Deborah P. da Silva
#
# Descrição: Compara grupos (plantas saudáveis vs estressadas)
#             utilizando testes F (homogeneidade de variâncias) e
#             teste t de Student para variáveis espectrais e clorofila.
#             Gera gráfico de violino com anotações dos resultados.
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
arquivo_csv <- "data/processed/grupo.csv"

dados <- read.csv(
  arquivo_csv, sep = ";", dec = ".", header = TRUE,
  stringsAsFactors = FALSE, check.names = FALSE
)

# Conferir estrutura (opcional)
print(names(dados))
print(unique(dados$classe))

# ------------------------------------------------------------
# 3. Variáveis analisadas
# ------------------------------------------------------------
variaveis <- c("NDVI", "NDRE", "CCCI", "PSRI", "Teor.de.Clorofila")

# ------------------------------------------------------------
# 4. Testes F e t de Student
# ------------------------------------------------------------
resultados <- map_dfr(variaveis, function(var) {
  grupo_saudavel <- dados %>% filter(classe == "Plantas saudaveis") %>% pull(.data[[var]])
  grupo_estressado <- dados %>% filter(classe == "Plantas estressadas") %>% pull(.data[[var]])
  
  f_test <- var.test(grupo_saudavel, grupo_estressado)
  t_test <- t.test(grupo_saudavel, grupo_estressado, paired = FALSE, var.equal = FALSE)
  
  tibble(
    Variavel = var,
    Media_saudaveis = mean(grupo_saudavel, na.rm = TRUE),
    Media_estressadas = mean(grupo_estressado, na.rm = TRUE),
    F_statistic = round(f_test$statistic, 3),
    F_p_value = round(f_test$p.value, 4),
    T_statistic = round(t_test$statistic, 3),
    T_p_value = t_test$p.value
  )
})

print("📈 Resultados dos testes F e t:")
print(resultados)

# ------------------------------------------------------------
# 5. Reestruturação dos dados para gráfico
# ------------------------------------------------------------
dados_long <- dados %>%
  pivot_longer(cols = all_of(variaveis), names_to = "Variavel", values_to = "Valor") %>%
  mutate(
    Variavel_label = recode(
      Variavel,
      "NDVI" = "NDVI",
      "NDRE" = "NDRE",
      "CCCI" = "CCCI",
      "PSRI" = "PSRI",
      "Teor.de.Clorofila" = "Teor de Clorofila"
    )
  )

# ------------------------------------------------------------
# 6. Preparar anotações (p e t)
# ------------------------------------------------------------
p_vals <- resultados %>%
  mutate(
    Variavel_label = recode(
      Variavel,
      "NDVI" = "NDVI",
      "NDRE" = "NDRE",
      "CCCI" = "CCCI",
      "PSRI" = "PSRI",
      "Teor.de.Clorofila" = "Teor de Clorofila"
    ),
    label_p = ifelse(T_p_value < 0.001, "p < 0.001", paste0("p = ", round(T_p_value, 3))),
    label_t = paste0("t = ", T_statistic)
  ) %>%
  select(Variavel_label, label_p, label_t)

# ------------------------------------------------------------
# 7. Gráfico de violino
# ------------------------------------------------------------
p <- ggplot(dados_long, aes(x = classe, y = Valor, fill = classe)) +
  geom_violin(trim = FALSE, alpha = 0.4) +
  stat_summary(fun = mean, geom = "crossbar", width = 0.6, color = "black",
               fatten = 0, linetype = "dashed") +
  stat_summary(fun = median, geom = "point", shape = 23, size = 3,
               fill = "white", color = "black") +
  geom_jitter(width = 0.15, size = 1.5, alpha = 0.6) +
  facet_wrap(~Variavel_label, scales = "free_y", ncol = 2) +
  geom_text(
    data = p_vals, aes(x = 1.5, y = Inf, label = label_p),
    vjust = 2, size = 3.5, inherit.aes = FALSE
  ) +
  geom_text(
    data = p_vals, aes(x = 1.5, y = Inf, label = label_t),
    vjust = 4, size = 3.5, inherit.aes = FALSE
  ) +
  labs(
    title = "Distribuição das variáveis por classe",
    subtitle = "Média (linha tracejada), mediana (ponto), teste t e p-valor",
    x = "Classe", y = "Valor"
  ) +
  scale_fill_manual(values = c("Plantas saudaveis" = "#5CB85C", "Plantas estressadas" = "#D9534F")) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    strip.text = element_text(face = "bold")
  )

print(p)

# ------------------------------------------------------------
# 8. Exportar resultados
# ------------------------------------------------------------
dir_saida <- "results/comparacoes_grupos"
if (!dir.exists(dir_saida)) dir.create(dir_saida, recursive = TRUE)

write.csv(resultados, file.path(dir_saida, "testes_t_f_grupos.csv"), row.names = FALSE)
ggsave(file.path(dir_saida, "violinos_grupos_estressadas.jpeg"), p, width = 10, height = 8, dpi = 300)

cat("✅ Resultados e gráfico salvos em:", dir_saida, "\n")
