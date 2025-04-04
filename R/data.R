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

create_model_df <- function(df_base, df_analise) {
    unique_period = sort(unique(df_analise$ano_mes))

    df_base_final = expand_grid(df_base, ano_mes = unique_period) |>
        arrange(cod_ibge) |>
        left_join(df_analise, by = c("cod_ibge", "ano_mes")) |>
        replace_na(list(n = 0))

    return(df_base_final)
}

load_populacao <- function(url_pop) {
    temp = tempfile(fileext = ".xls")
    download.file(
        url = url_pop, 
        destfile = temp
    )
    df = read_excel(temp, sheet = 2, skip = 1, n_max = 5570)

    df_pop = df |> 
        clean_names() |> 
        filter(uf == "SP") |> 
        mutate(cod_ibge = paste0(cod_uf, cod_munic)) |> 
        select(cod_ibge, populacao_estimada)

    return(df_pop)
}

load_mun_sf <- function() {
    sf_municipios = read_municipality(code_muni = "SP", year = 2022)

    sf_municipios |> 
        select(cod_ibge = code_muni) |> 
        mutate(cod_ibge = as.character(cod_ibge))
}

load_sinistros_vitimas <- function(df_sinistros) {
    df_sinistros_vitimas = df_sinistros |> 
        filter(
            gravidade_fatal == 0 | gravidade_leve > 0 | gravidade_grave > 0,
            year(data_sinistro) > 2018
        ) |> 
        mutate(
            ano_mes = format(data_sinistro, "%Y-%m"),
            ano_mes = ym(ano_mes)
        ) |> 
        count(cod_ibge, ano_mes)

    return(df_sinistros_vitimas)
}
