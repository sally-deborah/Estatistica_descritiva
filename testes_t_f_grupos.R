# ================================================================
# Script: testes_t_f_grupos.R
# Autor: Eng. Florestal MSc. Sally Deborah P. da Silva
#
# Descrição: Compara grupos (ex.: Grupo A vs Grupo B) utilizando testes
#             F (homogeneidade de variâncias) e t de Student.
#             Gera gráficos de violino com anotações dos resultados.
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
arquivo_csv <- "data/processed/dados_grupos.csv"

dados <- read.csv(
  arquivo_csv, sep = ";", dec = ".", header = TRUE,
  stringsAsFactors = FALSE, check.names = FALSE
)

# ------------------------------------------------------------
# 3. Variáveis analisadas (editar conforme dataset)
# ------------------------------------------------------------
variaveis <- c("Var1", "Var2", "Var3")

# ------------------------------------------------------------
# 4. Testes F e t de Student
# ------------------------------------------------------------
resultados <- map_dfr(variaveis, function(var) {
  grupoA <- dados %>% filter(classe == "Grupo A") %>% pull(.data[[var]])
  grupoB <- dados %>% filter(classe == "Grupo B") %>% pull(.data[[var]])
  
  f_test <- var.test(grupoA, grupoB)
  t_test <- t.test(grupoA, grupoB, paired = FALSE, var.equal = (f_test$p.value > 0.05))
  
  tibble(
    Variavel = var,
    Media_GrupoA = mean(grupoA, na.rm = TRUE),
    Media_GrupoB = mean(grupoB, na.rm = TRUE),
    F_statistic = round(f_test$statistic, 3),
    F_p_value = round(f_test$p.value, 4),
    T_statistic = round(t_test$statistic, 3),
    T_p_value = t_test$p.value
  )
})

print("📊 Resultados dos testes F e t:")
print(resultados)

# ------------------------------------------------------------
# 5. Reestruturar para gráfico
# ------------------------------------------------------------
dados_long <- dados %>%
  pivot_longer(cols = all_of(variaveis), names_to = "Variavel", values_to = "Valor")

# ------------------------------------------------------------
# 6. Gráfico de violino
# ------------------------------------------------------------
p <- ggplot(dados_long, aes(x = classe, y = Valor, fill = classe)) +
  geom_violin(trim = FALSE, alpha = 0.4) +
  stat_summary(fun = mean, geom = "crossbar", width = 0.6, color = "black", linetype = "dashed") +
  stat_summary(fun = median, geom = "point", shape = 23, size = 3, fill = "white") +
  geom_jitter(width = 0.15, size = 1.2, alpha = 0.6) +
  facet_wrap(~Variavel, scales = "free_y") +
  labs(title = "Comparação entre grupos", subtitle = "Teste F e t para cada variável") +
  scale_fill_manual(values = c("Grupo A" = "#5CB85C", "Grupo B" = "#D9534F")) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none", strip.text = element_text(face = "bold"))

print(p)

# ------------------------------------------------------------
# 7. Exportar resultados
# ------------------------------------------------------------
dir_saida <- "results/testes_t_f"
if (!dir.exists(dir_saida)) dir.create(dir_saida, recursive = TRUE)

write.csv(resultados, file.path(dir_saida, "resultados_testes_t_f.csv"), row.names = FALSE)
ggsave(file.path(dir_saida, "violinos_testes_t_f.jpeg"), p, width = 10, height = 8, dpi = 300)

cat("✅ Resultados e gráfico salvos em:", dir_saida, "\n")
