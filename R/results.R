make_tendencia_gt = function(df, direcao = c("pos", "neg")) {
    if (direcao == "pos") {
        df = df |>
            filter(tau > 0) |>
            arrange(-tau)
    } else {
        df = df |>
            filter(tau < 0) |>
            arrange(tau)
    }
    df |>
        filter(p_value < 0.05) |>
        select(nome, populacao_estimada, p_value, tau) |>
        gt() |>
        cols_label(
            nome = "Nome",
            populacao_estimada = "População",
            p_value = "p-valor",
            tau = "Tau"
        ) |>
        fmt_number(
            columns = -nome,
            decimals = 4,
            dec_mark = ",",
            sep_mark = "."
        ) |>
        fmt_number(
            columns = populacao_estimada,
            decimals = 0,
            sep_mark = "."
        ) |>
        tab_options(table.font.size = "11pt") |>
        opt_interactive(
            use_pagination = TRUE,
            use_sorting = FALSE,
            page_size_default = 15,
            use_compact_mode = TRUE,
            use_highlight = TRUE
        )
}

arrange_mk_sf = function(sf_sp, df_results) {
    sf_mapa = sf_sp |>
        left_join(df_results, by = "cod_ibge") |>
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
            status = if_else(status == "NA NA", "Sem óbitos", status)
        )
    return(sf_mapa)
}

plot_leaflet_map = function(sf, type = c("obitos", "sinistros")) {

    if (type == "obitos") {
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
