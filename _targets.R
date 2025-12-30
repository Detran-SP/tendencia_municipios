library(targets)
library(tarchetypes)

tar_option_set(
    packages = c(
        "janitor",
        "tidyverse",
        "trend",
        "showtext",
        "zoo",
        "gt",
        "readxl",
        "geobr",
        "sf",
        "leaflet",
        "plotly",
        'ost.utils',
        "leaflet.extras"
    )
)

tar_source("R/data.R")
tar_source("R/modelo.R")
tar_source("R/results.R")

list(
    ## Setup
    tar_target(
        detran_palette,
        list(
            "blue" = "#005ca8",
            "lightblue" = "#3490ce",
            "darkblue" = "#004077",
            "purple" = "#B456E0",
            "lightpurple" = "#D3A1FA"
        )
    ),
    ## Dados
    tar_target(
        input_tipo_via,
        c(
            "total",
            "Vias urbanas",
            "Estradas e rodovias",
            "total",
            "total",
            "total"
        )
    ),
    tar_target(
        input_tipo_modo,
        c("total", "total", "total", "Pedestre", "Bicicleta", "Motocicleta")
    ),
    tar_target(
        path_infosiga,
        "data/dados_infosiga.zip",
        format = "file"
    ),
    tar_target(
        infosiga_vitimas,
        get_infosiga_data("pessoas", path_infosiga)
    ),
    tar_target(
        infosiga_sinistros,
        get_infosiga_data('sinistros', path_infosiga)
    ),
    tar_target(
        list_df_obitos,
        map2(
            input_tipo_via,
            input_tipo_modo,
            load_obitos,
            df_vitimas = infosiga_vitimas,
            df_sinistros = infosiga_sinistros
        )
    ),
    tar_target(base_path, "data/divisoes_regionais_esp.csv", format = "file"),
    tar_target(df_base, load_municipios(base_path)),
    tar_target(
        list_df_model_obitos,
        map(list_df_obitos, create_model_df, df_base = df_base)
    ),
    tar_target(
        list_df_sinistros,
        map2(
            input_tipo_via,
            input_tipo_modo,
            load_sinistros_vitimas,
            df_sinistros = infosiga_sinistros
        )
    ),
    tar_target(
        list_df_model_sinistros,
        map(list_df_sinistros, create_model_df, df_base = df_base)
    ),
    tar_target(
        df_populacao,
        load_populacao(
            "https://ftp.ibge.gov.br/Estimativas_de_Populacao/Estimativas_2024/POP2024_20241230.xls"
        )
    ),
    tar_target(sf_municipios, load_mun_sf()),
    tar_target(path_snt, "data/snt_municipios.xlsx", format = "file"),
    tar_target(df_snt, load_snt(path_snt)),
    ## Modelagem
    tar_target(list_df_model, c(list_df_model_obitos, list_df_model_sinistros)),
    tar_target(
        list_time_series_raw,
        map(
            list_df_model,
            function(x) {
                map(
                    unique(x$cod_ibge),
                    create_time_series,
                    df = x
                )
            }
        )
    ),
    tar_target(city_names, unique(list_df_model[[1]]$municipio)),
    tar_target(
        list_time_series,
        set_list_names(list_time_series_raw, city_names)
    ),
    tar_target(
        list_mk_results,
        map(list_time_series, function(x) map(x, mk.test))
    ),
    tar_target(
        list_df_mk_results,
        map2(list_mk_results, list_df_model, arrange_mk_results)
    ),
    tar_target(
        input_list,
        list(
            list_df_mk_results,
            list_df_model,
            list(
                "Óbitos totais",
                "Óbitos em vias urbanas",
                "Óbitos em estradas e rodovias",
                "Óbitos - pedestres",
                "Óbitos - ciclistas",
                "Óbitos - ocupantes de motocicleta",
                "Sinistros com vítimas feridas",
                "Sinistros com vítimas feridas (vias urbanas)",
                "Sinistros com vítimas feridas (estradas e rodovias)",
                "Sinistros com vítimas feridas - pedestres",
                "Sinistros com vítimas feridas - ciclistas",
                "Sinistros com vítimas feridas - ocupantes de motocicleta"
            )
        )
    ),
    tar_target(
        df_final_raw,
        pmap(input_list, arrange_final_results) |> reduce(bind_rows)
    ),
    tar_target(df_final, join_final_df(df_final_raw, df_populacao, df_snt)),
    ## Resultados
    tar_target(
        list_gt_tendencia_pos,
        map(
            input_list[[3]],
            make_tendencia_gt,
            df = df_final,
            direcao = "pos",
            color_pal = detran_palette,
            df_base = df_base
        )
    ),
    tar_target(
        list_gt_tendencia_neg,
        map(
            input_list[[3]],
            make_tendencia_gt,
            df = df_final,
            direcao = "neg",
            color_pal = detran_palette,
            df_base = df_base
        )
    ),
    tar_target(
        list_sf,
        map(
            input_list[[3]],
            arrange_mk_sf,
            df_results = df_final,
            sf_sp = sf_municipios,
            df_base = df_base
        )
    ),
    tar_target(
        tendencia_maps,
        map(list_sf, plot_leaflet_map, color_pal = detran_palette)
    ),
    tar_target(
        list_mun_count_pos,
        map(
            input_list[[3]],
            extract_df_len,
            df = df_final,
            tendencia = "pos"
        )
    ),
    tar_target(
        list_mun_count_neg,
        map(
            input_list[[3]],
            extract_df_len,
            df = df_final,
            tendencia = "neg"
        )
    ),
    tar_target(
        gt_resumo,
        make_gt_resumo(
            df_final,
            df_base,
            df_populacao,
            df_snt
        )
    ),
    ## Report
    tar_quarto(report, "."),
    ## Export
    tar_target(path_export_csv, "data/df_final.csv", format = "file"),
    tar_target(
        df_final_export,
        export_final_data(df_final, path_export_csv),
        format = "file"
    )
)
