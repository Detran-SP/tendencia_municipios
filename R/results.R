#' Creates a gt table with trend data.
#'
#' @param df The input data frame.
#' @param direcao The direction of the trend ("pos" or "neg").
#' @param var The variable to be analyzed.
#' @param color_pal The color palette to be used.
#' @param df_base The base data frame.
#'
#' @return A gt table.
#'
#' @export
make_tendencia_gt = function(
    df,
    direcao = c("pos", "neg"),
    var,
    color_pal,
    df_base
) {
    df = df |> pivot_wider(names_from = metric, values_from = value)

    if (direcao == "pos") {
        df = df |>
            filter(tau > 0) |>
            arrange(-tau)
    } else {
        df = df |>
            filter(tau < 0) |>
            arrange(tau)
    }
    footnote_text = ifelse(
        grepl("Óbitos", var),
        "Período entre 2015 e 2024",
        "Período entre 2019 e 2024"
    )

    footnote_plot = "A linha tracejada representa a média da série temporal."

    df |>
        filter(p_value < 0.05, variavel == var) |>
        left_join(df_base, by = "cod_ibge") |>
        select(municipio, populacao_estimada, tau, ts) |>
        gt() |>
        cols_nanoplot(
            columns = ts,
            reference_line = "mean",
            options = nanoplot_options(
                data_point_fill_color = color_pal$darkblue,
                data_line_stroke_color = color_pal$darkblue,
                data_area_fill_color = NULL
            )
        ) |>
        cols_label(
            municipio = "Município",
            populacao_estimada = "População",
            tau = "Índice de tendência (Tau)",
            #integrado_snt = "Integrado ao SNT",
            nanoplots = "Série temporal anual"
        ) |>
        fmt_number(
            columns = tau,
            decimals = 4,
            dec_mark = ",",
            sep_mark = "."
        ) |>
        fmt_number(
            columns = populacao_estimada,
            decimals = 0,
            sep_mark = ".",
            dec_mark = ","
        ) |>
        tab_options(table.font.size = "11pt") |>
        cols_align(
            columns = c(tau, nanoplots),
            align = "center"
        ) |>
        tab_footnote(
            footnote = footnote_text,
            locations = cells_column_labels(columns = nanoplots)
        ) |>
        tab_footnote(
            footnote = footnote_plot,
            locations = cells_column_labels(columns = nanoplots)
        ) |>
        opt_interactive(
            use_pagination = TRUE,
            use_sorting = FALSE,
            page_size_default = 15,
            use_compact_mode = TRUE,
            use_highlight = TRUE,
            use_filters = TRUE
        )
}

#' Arranges Mann-Kendall results for spatial data.
#'
#' @param sf_sp The spatial data frame for São Paulo.
#' @param df_results The data frame with the Mann-Kendall results.
#' @param var The variable to be analyzed.
#' @param df_base The base data frame.
#'
#' @return A spatial data frame with the Mann-Kendall results.
#'
#' @export
arrange_mk_sf = function(sf_sp, df_results, var, df_base) {
    df = df_results |>
        filter(metric %in% c("p_value", "tau"), variavel == var) |>
        pivot_wider(
            names_from = metric,
            values_from = value
        ) |>
        mutate(
            significancia = if_else(
                p_value < 0.05,
                "(Significativa)",
                "(Não significativa)"
            ),
            tendencia = if_else(
                tau > 0,
                "Tendência de aumento",
                "Tendência de redução"
            ),
            status = paste0(tendencia, " ", significancia),
            status = if_else(status == "NA NA", "Sem tendência", status)
        )

    sf_mapa = sf_sp |>
        left_join(df, by = "cod_ibge") |>
        left_join(df_base, by = "cod_ibge")

    return(sf_mapa)
}

#' Plots a leaflet map.
#'
#' @param sf The spatial data frame.
#' @param color_pal The color palette to be used.
#'
#' @return A leaflet map.
#'
#' @export
plot_leaflet_map = function(sf, color_pal) {
    sf = sf |>
        mutate(
            status = case_match(
                status,
                "Tendência de aumento (Significativa)" ~ "Tendência de aumento",
                "Tendência de redução (Significativa)" ~ "Tendência de redução",
                "Tendência de aumento (Não significativa)" ~
                    "Sem tendência significativa",
                "Tendência de redução (Não significativa)" ~
                    "Sem tendência significativa",
                "Sem tendência" ~ "Sem tendência significativa",
                .default = status
            )
        )

    # if ("Sem tendência" %in% unique(sf$status)) {
    #     pal = colorFactor(
    #         palette = c(
    #             "grey50",
    #             #color_pal$lightpurple,
    #             color_pal$purple,
    #             #color_pal$lightblue,
    #             color_pal$blue
    #         ),
    #         domain = unique(sf$status)
    #     )
    # } else {
    #     pal = colorFactor(
    #         palette = c(
    #             #color_pal$lightpurple,
    #             color_pal$purple,
    #             #color_pal$lightblue,
    #             color_pal$blue
    #         ),
    #         domain = unique(sf$status)
    #     )
    # }

    pal = colorFactor(
        palette = c(
            "grey70",
            color_pal$purple,
            color_pal$blue
        ),
        domain = unique(sf$status)
    )

    labels = sprintf(
        "<strong>%s</strong><br/>População: %d<br/>%s",
        sf$municipio,
        sf$populacao_estimada,
        sf$status
    ) |>
        lapply(htmltools::HTML)

    leaflet(sf, options = leafletOptions(preferCanvas = TRUE)) |>
        addProviderTiles(providers$CartoDB.PositronNoLabels) |>
        addPolygons(
            fillColor = ~ pal(status),
            stroke = TRUE,
            color = "white",
            fillOpacity = 1,
            weight = 1,
            label = labels,
            labelOptions = labelOptions(
                style = list("font-weight" = "normal"),
                textsize = "12px",
                direction = "auto"
            ),
            highlightOptions = highlightOptions(
                color = "black",
                weight = 3,
                bringToFront = TRUE
            ),
            layerId = ~municipio
        ) |>
        addLegend(
            pal = pal,
            values = sf$status,
            position = "bottomleft",
            opacity = 1,
            title = "Tendência calculada:"
        ) |>
        leaflet.extras::addFullscreenControl()
}

#' Creates a plotly chart.
#'
#' @param df The input data frame.
#' @param mun_input The municipality to be plotted.
#' @param type The type of data to be plotted ("obitos" or "sinistros").
#'
#' @return A plotly chart.
#'
#' @export
make_plotly <- function(df, mun_input, type = c("obitos", "sinistros")) {
    if (type == "obitos") {
        df = df |>
            mutate(
                tooltip = paste0(
                    "Período: ",
                    str_sub(ano_mes, 1, 7),
                    "<br>Óbitos:",
                    n
                )
            )
        y_label = "Qtd. de óbitos"
    } else {
        df = df |>
            mutate(
                tooltip = paste0(
                    "Período: ",
                    str_sub(ano_mes, 1, 7),
                    "<br>Sinistros:",
                    n
                )
            )
        y_label = "Qtd. de sinistros com vítimas"
    }

    plot = df |>
        filter(municipio == mun_input) |>
        ggplot(aes(x = ano_mes, y = n, group = 1, text = tooltip)) +
        geom_line(color = detran_palette$darkblue, lwd = 0.3) +
        scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
        scale_y_continuous(
            labels = scales::number_format(
                decimal.mark = ","
            )
        ) +
        theme_bw() +
        labs(x = NULL, y = y_label)

    ggplotly(plot, tooltip = "text")
}

#' Filters municipalities with critical trends.
#'
#' @param df_results The data frame with the results.
#'
#' @return A data frame with the municipalities with critical trends.
#'
#' @export
filter_mun_criticos <- function(df_results) {
    df_tendencias = df_results |>
        filter(p_value < 0.05, tau > 0) |>
        mutate(aumento = "Sim") |>
        select(nome, aumento)
}

#' Creates a gt table with a summary of the results.
#'
#' @param df_final The final data frame.
#' @param df_base The base data frame.
#' @param df_populacao The population data frame.
#' @param df_snt The SNT data frame.
#'
#' @return A gt table with the summary of the results.
#'
#' @export
make_gt_resumo <- function(df_final, df_base, df_populacao, df_snt) {
    df_tbl = df_final |>
        filter(metric %in% c("p_value", "tau")) |>
        pivot_wider(names_from = metric, values_from = value) |>
        filter(p_value < 0.05, tau > 0) |>
        mutate(aumento = "Sim") |>
        select(cod_ibge, variavel, aumento) |>
        pivot_wider(
            names_from = variavel,
            values_from = aumento,
            values_fill = "-"
        )

    df_base |>
        arrange(municipio) |>
        left_join(df_populacao, by = "cod_ibge") |>
        left_join(df_tbl, by = "cod_ibge") |>
        left_join(df_snt, by = "cod_ibge") |>
        mutate(
            across(
                .cols = `Óbitos totais`:`Sinistros com vítimas feridas - ocupantes de motocicleta`,
                .fns = ~ if_else(is.na(.x), "-", .x)
            )
        ) |>
        select(
            municipio,
            populacao_estimada,
            integrado_snt,
            everything(),
            -cod_ibge
        ) |>
        gt() |>
        cols_label(
            municipio = "Município",
            populacao_estimada = "População",
            integrado_snt = "Integrado ao SNT",
            `Óbitos totais` = "Total",
            `Óbitos em vias urbanas` = "Vias urbanas",
            `Óbitos em estradas e rodovias` = "Estradas e rodovias",
            `Óbitos - pedestres` = "Pedestres",
            `Óbitos - ciclistas` = "Ciclistas",
            `Óbitos - ocupantes de motocicleta` = "Motociclistas",
            `Sinistros com vítimas feridas` = "Total",
            `Sinistros com vítimas feridas (vias urbanas)` = "Vias urbanas",
            `Sinistros com vítimas feridas (estradas e rodovias)` = "Estradas e rodovias",
            `Sinistros com vítimas feridas - pedestres` = "Pedestres",
            `Sinistros com vítimas feridas - ciclistas` = "Ciclistas",
            `Sinistros com vítimas feridas - ocupantes de motocicleta` = "Motociclistas"
        ) |>
        cols_align(
            columns = integrado_snt:`Sinistros com vítimas feridas - ocupantes de motocicleta`,
            align = "center"
        ) |>
        tab_spanner(
            label = "Óbitos",
            columns = `Óbitos totais`:`Óbitos - ocupantes de motocicleta`,
            id = "obitos"
        ) |>
        tab_spanner(
            label = "Sinistros com vítimas feridas",
            columns = `Sinistros com vítimas feridas`:`Sinistros com vítimas feridas - ocupantes de motocicleta`,
            id = "sinistros"
        ) |>
        fmt_number(
            columns = populacao_estimada,
            decimals = 0,
            sep_mark = ".",
            dec_mark = ","
        ) |>
        tab_style(
            style = cell_borders(
                sides = "right",
                color = "grey"
            ),
            location = cells_body(
                columns = c(
                    integrado_snt,
                    `Óbitos - ocupantes de motocicleta`
                )
            )
        ) |>
        opt_interactive(
            use_pagination = TRUE,
            use_sorting = TRUE,
            page_size_default = 20,
            use_compact_mode = TRUE,
            use_highlight = TRUE,
            use_filters = TRUE
        ) |>
        tab_options(table.font.size = "11pt")
}

#' Extracts the length of a data frame.
#'
#' @param df The input data frame.
#' @param var The variable to be analyzed.
#' @param tendencia The direction of the trend ("pos" or "neg").
#'
#' @return The number of rows in the data frame.
#'
#' @export
extract_df_len <- function(df, var, tendencia = c("pos", "neg")) {
    df = df |>
        filter(variavel == var) |>
        pivot_wider(names_from = metric, values_from = value)

    if (tendencia == "pos") {
        df = df |>
            filter(tau > 0)
    } else {
        df = df |>
            filter(tau < 0)
    }

    df |>
        filter(p_value < 0.05) |>
        nrow()
}
