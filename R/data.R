load_obitos <- function(
    df_vitimas,
    df_sinistros,
    via = "total",
    modo = "total"
) {
    df = df_vitimas |>
        filter(gravidade_lesao == "Fatal") |>
        mutate(
            ano_mes = format(data_obito, "%Y-%m"),
            ano_mes = ym(ano_mes)
        ) |>
        left_join(
            df_sinistros |> select(id_sinistro, cod_ibge, tipo_via),
            by = "id_sinistro"
        )

    if (via != "total") {
        df = df |> filter(tipo_via == via)
    }

    if (modo != "total") {
        df = df |> filter(tipo_veiculo_vitima == modo)
    }

    df |> count(cod_ibge, ano_mes)
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

load_sinistros_vitimas <- function(
    df_sinistros,
    via = "total",
    modo = "total"
) {
    df_sinistros_vitimas = df_sinistros |>
        filter(
            gravidade_fatal == 0 | gravidade_leve > 0 | gravidade_grave > 0,
            year(data_sinistro) > 2018
        ) |>
        mutate(
            ano_mes = format(data_sinistro, "%Y-%m"),
            ano_mes = ym(ano_mes)
        ) |>
        select(
            cod_ibge, ano_mes, tp_veiculo_bicicleta, tipo_via,
            tp_veiculo_motocicleta, tp_sinistro_atropelamento
        )

    if (via != "total") {
        df_sinistros_vitimas = df_sinistros_vitimas |>
            filter(tipo_via == via)
    }

    if (modo == "Bicicleta") {
        df_sinistros_vitimas = df_sinistros_vitimas |>
            filter(tp_veiculo_bicicleta > 0)
    }

    if (modo == "Motocicleta") {
        df_sinistros_vitimas = df_sinistros_vitimas |>
            filter(tp_veiculo_motocicleta > 0)
    }

    if (modo == "Pedestre") {
        df_sinistros_vitimas = df_sinistros_vitimas |>
            filter(
                tp_sinistro_atropelamento > 0 & tp_veiculo_bicicleta == 0
            )
    }

    df = df_sinistros_vitimas |>
        count(cod_ibge, ano_mes)

    return(df)
}

load_snt = function(path) {
    read_excel(path) |>
        clean_names() |>
        mutate(municipio = tolower(municipio), integracao_snt = "Sim") |>
        select(municipio, integracao_snt)
}
