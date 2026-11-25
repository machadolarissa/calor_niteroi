# calor_niteroi

Calor extremo e saúde em Niterói (CISNit/SMS, cooperação com a OPAS).

- `coletar_dados_calor.R`: coleta dados públicos (Open-Meteo, INMET, SIM/SIH via DATASUS, InfoDengue) e gera `dados/`. A cidade é configurada no topo do script.
- `gerar_bairros_censo.R`: gera `dados/bairros_vulnerabilidade.csv` a partir do Censo 2022 (IBGE).
- `painel_calor_niteroi/`: painel Shiny (bslib, echarts4r). Lê os arquivos de `dados/`.

Os microdados (`*.rds`), o INMET bruto e a série horária não são versionados; rode o script de coleta para gerá-los.
