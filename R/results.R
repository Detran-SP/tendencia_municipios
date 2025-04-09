make_tendencia_gt = function(df, direcao = c("pos", "neg"), var) {
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

    df |>
        filter(p_value < 0.05, variavel == var) |>
        select(nome, populacao_estimada, integrado_snt, tau, ts) |>
        gt() |>
        cols_nanoplot(
            columns = ts,
            reference_line = "mean",
            options = nanoplot_options(
                data_point_fill_color = detran_palette$darkblue,
                data_line_stroke_color = detran_palette$darkblue,
                data_area_fill_color = NULL
            )
        ) |>
        cols_label(
            nome = "Município",
            populacao_estimada = "População",
            tau = "Tau",
            integrado_snt = "Integrado ao SNT",
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
            columns = nanoplots,
            align = "right"
        ) |>
        tab_footnote(
            footnote = footnote_text,
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

arrange_mk_sf = function(sf_sp, df_results, var) {
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
            status = if_else(status == "NA NA", "Sem valores", status)
        )

    sf_mapa = sf_sp |>
        left_join(df, by = "cod_ibge")

    return(sf_mapa)
}

plot_leaflet_map = function(sf) {

    if ("Sem valores" %in% unique(sf$status)) {
        pal = colorFactor(
            palette = c(
                "grey50",
                detran_palette$lightpurple,
                detran_palette$purple,
                detran_palette$lightblue,
                detran_palette$blue
            ),
            domain = unique(sf$status)
        )
    } else {
        pal = colorFactor(
            palette = c(
                detran_palette$lightpurple,
                detran_palette$purple,
                detran_palette$lightblue,
                detran_palette$blue
            ),
            domain = unique(sf$status)
        )
    }

    labels = sprintf(
        "<strong>%s</strong><br/>População: %d<br/>%s",
        sf$nome,
        sf$populacao_estimada,
        sf$status
    ) |> lapply(htmltools::HTML)

    leaflet(sf) |>
        addProviderTiles(providers$CartoDB.PositronNoLabels) |>
        addPolygons(
            fillColor = ~pal(status),
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
            layerId = ~nome
        ) |>
        addLegend(
            pal = pal,
            values = sf$status,
            position = "bottomleft",
            opacity = 1,
            title = "Tendência calculada:"
        )
}

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

filter_mun_criticos <- function(df_results) {
    df_tendencias = df_results |>
        filter(p_value < 0.05, tau > 0) |>
        mutate(aumento = "Sim") |>
        select(nome, aumento)
}

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
            municipio, populacao_estimada,
            integrado_snt, everything(), -cod_ibge
        ) |>
        gt() |>
        cols_label(
            municipio = "Município",
            populacao_estimada = "População",
            integrado_snt = "Integrado ao SNT",
            `Óbitos totais` = "Total",
            `Óbitos em vias municipais` = "Vias municipais",
            `Óbitos em rodovias` = "Rodovias",
            `Óbitos - pedestres` = "Pedestres",
            `Óbitos - ciclistas` = "Ciclistas",
            `Óbitos - ocupantes de motocicleta` = "Motociclistas",
            `Sinistros com vítimas feridas` = "Total",
            `Sinistros com vítimas feridas (vias municipais)` = "Vias municipais",
            `Sinistros com vítimas feridas (rodovias)` = "Rodovias",
            `Sinistros com vítimas feridas - pedestres` = "Pedestres",
            `Sinistros com vítimas feridas - ciclistas` = "Ciclistas",
            `Sinistros com vítimas feridas - ocupantes de motocicleta` = "Motociclistas"
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

