load_obitos <- function(df_vitimas, df_sinistros) {
    df_vitimas |>
        filter(gravidade_lesao == "Fatal") |>
        mutate(
            ano_mes = format(data_obito, "%Y-%m"),
            ano_mes = ym(ano_mes)
        ) |>
        left_join(
            df_sinistros |> select(id_sinistro, cod_ibge),
            by = "id_sinistro"
        ) |>
        count(cod_ibge, ano_mes)
}

load_municipios <- function(path_municipios) {
    read_csv2(path_municipios, locale = locale(encoding = "latin1")) |>
        clean_names() |>
        mutate(cod_ibge = as.character(cod_ibge)) |>
        select(cod_ibge, municipio)
}

create_model_df <- function(df_base, df_obitos) {
    unique_period = sort(unique(df_obitos$ano_mes))

    df_base_final = expand_grid(df_base, ano_mes = unique_period) |>
        arrange(cod_ibge) |>
        left_join(df_obitos, by = c("cod_ibge", "ano_mes")) |>
        replace_na(list(n = 0))

    return(df_base_final)
}
