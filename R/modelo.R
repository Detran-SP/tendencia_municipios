#' Create a time series object for a given municipality.
#'
#' @param df A data frame containing the time series data.
#' @param cod_input The municipality IBGE code to filter the data.
#'
#' @return A `ts` object representing the time series for the municipality.
#'
#' @export
create_time_series <- function(df, cod_input) {
    values = df |>
        filter(cod_ibge == cod_input) |>
        pull(n)

    ts(values, start = c(year(first(df$ano_mes)), 1), frequency = 12)
}

#' Arrange Mann-Kendall test results into a data frame.
#'
#' @param results A list containing the results from the Mann-Kendall test.
#' @param base_df The base data frame used to extract unique IBGE codes.
#'
#' @return A tibble with the organized results of the Mann-Kendall test.
#'
#' @export
arrange_mk_results <- function(results, base_df) {
    df = tibble(
        cod_ibge = NULL,
        nome = NULL,
        p_value = NULL,
        s = NULL,
        var_s = NULL,
        tau = NULL
    )

    for (i in 1:length(results)) {
        df_i = tibble(
            nome = names(results)[i],
            p_value = results[[i]]$p.value,
            s = results[[i]]$estimates["S"],
            var_s = results[[i]]$estimates["varS"],
            tau = results[[i]]$estimates["tau"]
        )
        df = bind_rows(df, df_i)
    }

    df$cod_ibge = unique(base_df$cod_ibge)

    return(df)
}

#' Arrange the final results into a long format data frame.
#'
#' @param df_results A data frame with the Mann-Kendall test results.
#' @param df_model The data frame used for the model, containing counts per month.
#' @param var The name of the variable being analyzed.
#'
#' @return A data frame in long format with the final results.
#'
#' @export
arrange_final_results <- function(
    df_results,
    df_model,
    var
) {
    df_ts = df_model |>
        group_by(cod_ibge, year(ano_mes)) |>
        summarise(n = sum(n)) |>
        filter(`year(ano_mes)` < 2025) |>
        group_by(cod_ibge) |>
        summarise(ts = paste(n, collapse = ","))

    df_results |>
        pivot_longer(
            cols = p_value:tau,
            names_to = "metric",
            values_to = "value"
        ) |>
        mutate(variavel = var) |>
        left_join(df_ts, by = "cod_ibge")
}

#' Set the names of a list.
#'
#' @param list A list whose names are to be set.
#' @param city_names A vector of names to assign to the list.
#'
#' @return The list with the names assigned.
#'
#' @export
set_list_names <- function(list, city_names) {
    for (i in 1:length(list)) {
        setNames(list[[i]], city_names)
    }
    return(list)
}

#' Join final data with population and SNT data.
#'
#' @param df_final_raw The raw final data frame.
#' @param df_pop The population data frame.
#' @param df_snt The SNT (National Traffic System) data frame.
#'
#' @return A data frame resulting from the join of the three input data frames.
#'
#' @export
join_final_df <- function(df_final_raw, df_pop, df_snt) {
    df_final_raw |>
        left_join(df_pop, by = "cod_ibge") |>
        left_join(df_snt, by = "cod_ibge")
}
