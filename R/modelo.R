create_time_series <- function(df, cod_input) {
    values = df |>
        filter(cod_ibge == cod_input) |>
        pull(n)

    ts(values, start = c(year(first(df$ano_mes)), 1), frequency = 12)
}

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

set_list_names <- function(list, names) {
    for (i in 1:length(list)) {
        names(list[[i]]) <- names
    }
    return(list)
}

join_final_df <- function(df_final_raw, df_pop, df_snt) {
    df_final_raw |>
        left_join(df_pop, by = "cod_ibge") |>
        left_join(df_snt, by = "cod_ibge")
}
