create_time_series <- function(df, cod_input) {
    values = df |> 
        filter(cod_ibge == cod_input) |> 
        pull(n)

    ts(values, start = c(2015, 1), frequency = 12)
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
