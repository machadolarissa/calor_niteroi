# Painel de Calor e Saúde de Niterói (modelo)

1. Rode `../coletar_dados_calor.R` (gera a pasta `dados/`) e copie `dados/` para dentro desta pasta.
2. Opcional: crie `dados/bairros_vulnerabilidade.csv` com as colunas
   `bairro, pop_65mais_pct, renda_mediana, cobertura_arborea_pct, n_refugios, cobertura_refugio_10min_pct`.
3. No R: `install.packages(c("shiny","bslib","plotly","dplyr","readr","lubridate","DT"))` e `shiny::runApp()`.
4. Sem arquivos em `dados/`, o painel abre com dados simulados e avisa na tela.
5. Publicação: `rsconnect::deployApp()` (Posit Connect Cloud, como no painel de tuberculose).

Cores: paleta do CISNit (laranja `#E04E05` / `#F75D07`, âmbar `#F7AA42`, verde-petróleo `#1A4146`, verde `#40A98A`, turquesa `#12B2AA`).
Os cortes dos níveis (32, 36, 40, 44 °C de índice de calor) são provisórios e editáveis.
