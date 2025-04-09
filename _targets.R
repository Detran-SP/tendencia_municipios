library(targets)
# library(tarchetypes) # Load other packages as needed.

tar_option_set(
    packages = c(
        "infosiga", "janitor", "tidyverse", "trend", "showtext", "zoo", "gt",
        "readxl", "geobr", "sf", "leaflet", "plotly"
    )
)

tar_source("R/data.R")
tar_source("R/modelo.R")
tar_source("R/results.R")

list(
    ## Dados
    tar_target(
        input_tipo_via,
        c("total", "Vias municipais", "Rodovias", "total", "total", "total")
    ),
    tar_target(
        input_tipo_modo,
        c("total", "total", "total", "Pedestre", "Bicicleta", "Motocicleta")
    ),
    tar_target(
        list_df_obitos,
        map2(
            input_tipo_via,
            input_tipo_modo,
            load_obitos,
            df_vitimas = infosiga_vitimas,
            df_sinistros = infosiga_sinistros
        )
    ),
    tar_target(base_path, "data/divisoes_regionais_esp.csv", format = "file"),
    tar_target(df_base, load_municipios(base_path)),
    tar_target(
        list_df_model_obitos,
        map(list_df_obitos, create_model_df, df_base = df_base)
    ),
    tar_target(
        list_df_sinistros,
        map2(
            input_tipo_via,
            input_tipo_modo,
            load_sinistros_vitimas,
            df_sinistros = infosiga_sinistros
        )
    ),
    tar_target(
        list_df_model_sinistros,
        map(list_df_sinistros, create_model_df, df_base = df_base)
    ),
    tar_target(
        df_populacao,
        load_populacao("https://ftp.ibge.gov.br/Estimativas_de_Populacao/Estimativas_2024/POP2024_20241230.xls")
    ),
    tar_target(sf_municipios, load_mun_sf()),
    tar_target(path_snt, "data/snt_municipios.xlsx", format = "file"),
    tar_target(df_snt, load_snt(path_snt))
    ## Modelagem
    ## Resultados
)
