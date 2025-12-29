#' Loads and processes traffic fatality data.
#'
#' @param df_vitimas A data frame of victims.
#' @param df_sinistros A data frame of accidents.
#' @param via The type of road to filter by (optional). Defaults to "total".
#' @param modo The mode of transport to filter by (optional). Defaults to "total".
#'
#' @return A data frame with the count of fatalities per municipality and month.
#'
#' @export
load_obitos <- function(
    df_vitimas,
    df_sinistros,
    via = "total",
    modo = "total"
) {
    df = df_vitimas |>
        filter(gravidade_lesao == "Fatal", data_obito <= "2025-02-28") |>
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
        if (modo == "Pedestre") {
            df <- df |> filter(tipo_de_vitima == modo)
        } else {
            df <- df |> filter(tipo_veiculo_vitima == modo)
        }
    }

    df |> count(cod_ibge, ano_mes)
}

#' Loads municipality data from a CSV file.
#'
#' @param path_municipios The path to the CSV file.
#'
#' @return A data frame with municipality codes and names.
#'
#' @export
load_municipios <- function(path_municipios) {
    read_csv2(path_municipios, locale = locale(encoding = "latin1")) |>
        clean_names() |>
        mutate(cod_ibge = as.character(cod_ibge)) |>
        select(cod_ibge, municipio)
}

#' Creates a data frame for modeling.
#'
#' This function ensures that all time periods are present for each municipality.
#'
#' @param df_base A base data frame with municipality information.
#' @param df_analise A data frame with the analysis data.
#'
#' @return A data frame ready for modeling.
#'
#' @export
create_model_df <- function(df_base, df_analise) {
    unique_period = sort(unique(df_analise$ano_mes))

    df_base_final = expand_grid(df_base, ano_mes = unique_period) |>
        arrange(cod_ibge) |>
        left_join(df_analise, by = c("cod_ibge", "ano_mes")) |>
        replace_na(list(n = 0))

    return(df_base_final)
}

#' Loads population data from a URL.
#'
#' @param url_pop The URL of the Excel file containing population data.
#'
#' @return A data frame with municipality codes and estimated population.
#'
#' @export
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

#' Loads spatial data for municipalities in São Paulo.
#'
#' @return An sf data frame with municipality codes.
#'
#' @export
load_mun_sf <- function() {
    sf_municipios = read_municipality(code_muni = "SP", year = 2022)

    sf_municipios |>
        select(cod_ibge = code_muni) |>
        mutate(cod_ibge = as.character(cod_ibge))
}

#' Loads and processes data on accidents and victims.
#'
#' @param df_sinistros A data frame of accidents.
#' @param via The type of road to filter by (optional). Defaults to "total".
#' @param modo The mode of transport to filter by (optional). Defaults to "total".
#'
#' @return A data frame with the count of accidents per municipality and month.
#'
#' @export
load_sinistros_vitimas <- function(
    df_sinistros,
    via = "total",
    modo = "total"
) {
    df_sinistros_vitimas = df_sinistros |>
        filter(
            qtd_gravidade_fatal == 0 |
                qtd_gravidade_leve > 0 |
                qtd_gravidade_grave > 0,
            year(data_sinistro) > 2018,
            data_sinistro <= "2025-02-28"
        ) |>
        mutate(
            ano_mes = format(data_sinistro, "%Y-%m"),
            ano_mes = ym(ano_mes)
        ) |>
        select(
            cod_ibge,
            ano_mes,
            qtd_bicicleta,
            tipo_via,
            qtd_motocicleta,
            tp_sinistro_atropelamento
        )

    if (via != "total") {
        df_sinistros_vitimas = df_sinistros_vitimas |>
            filter(tipo_via == via)
    }

    if (modo == "Bicicleta") {
        df_sinistros_vitimas = df_sinistros_vitimas |>
            filter(qtd_bicicleta > 0)
    }

    if (modo == "Motocicleta") {
        df_sinistros_vitimas = df_sinistros_vitimas |>
            filter(qtd_motocicleta > 0)
    }

    if (modo == "Pedestre") {
        df_sinistros_vitimas = df_sinistros_vitimas |>
            filter(
                tp_sinistro_atropelamento > 0 & qtd_bicicleta == 0
            )
    }

    df = df_sinistros_vitimas |>
        count(cod_ibge, ano_mes)

    return(df)
}

#' Loads data on SNT integration.
#'
#' @param path The path to the Excel file.
#'
#' @return A data frame with municipality codes and SNT integration status.
#'
#' @export
load_snt = function(path) {
    read_excel(path) |>
        mutate(
            cod_ibge = as.character(cod_ibge),
            integrado_snt = if_else(
                integrado_snt == "SIM",
                "Sim",
                "Não"
            )
        )
}

#' Exports the final data to a CSV file.
#'
#' @param df The data frame to export.
#' @param path The path for the output CSV file.
#'
#' @return The path to the exported file.
#'
#' @export
export_final_data = function(df, path) {
    df |>
        pivot_wider(names_from = metric, values_from = value) |>
        select(
            cod_ibge,
            # municipio,
            variavel,
            populacao_estimada,
            integrado_snt,
            p_value,
            tau
        ) |>
        write_csv(path)

    return(path)
}

#' Downloads and loads data from Infosiga.
#'
#' @param type The type of data to load ("sinistros" or "vitimas").
#' @param path Path of infosiga zip
#'
#' @return A cleaned data frame with the requested Infosiga data.
#'
#' @export
get_infosiga_data <- function(type, path) {
    df <- ost.utils::load_infosiga(type, path) |>
        ost.utils::clean_infosiga(type)

    if (type == "sinistros") {
        df <- df |> filter(tipo_registro != "Notificação")
    }

    return(df)
}
