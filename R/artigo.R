library(targets)
library(ost.utils)
library(tidyverse)
library(sf)
library(patchwork)
library(ggrepel)

tar_load(df_final)
tar_load(sf_municipios)
tar_load(infosiga_sinistros)

df_names <- infosiga_sinistros |>
    select(cod_ibge, nome_municipio) |>
    unique() |>
    drop_na()

sign_results <- df_final |>
    filter(
        variavel %in% c('Óbitos totais', 'Sinistros com vítimas feridas'),
        metric %in% c('p_value', 'tau')
    ) |>
    pivot_wider(names_from = metric, values_from = value) |>
    filter(p_value < 0.05)

df_sinistros_mun <- infosiga_sinistros |>
    filter(tipo_registro == "Sinistro não fatal", year(data_sinistro) > 2018) |>
    count(cod_ibge, name = 'sinistros') |>
    drop_na()

df_obitos_mun <- infosiga_sinistros |>
    group_by(cod_ibge) |>
    summarise(obitos = sum(gravidade_fatal)) |>
    drop_na()

df_results <- sign_results |>
    left_join(df_obitos_mun, by = 'cod_ibge') |>
    left_join(df_sinistros_mun, by = 'cod_ibge') |>
    left_join(df_names, by = 'cod_ibge') |>
    mutate(
        label_obitos = if_else(
            obitos > 250 | abs(tau) > 0.25,
            nome_municipio,
            NA
        ),
        label_sinistros = if_else(
            sinistros > 9000 | tau < -0.4 | tau > 0.55,
            nome_municipio,
            NA
        )
    )

plot_obitos <- df_results |>
    filter(variavel == 'Óbitos totais') |>
    ggplot(aes(y = tau, x = obitos)) +
    geom_point(size = 0.55, aes(color = tau)) +
    geom_text_repel(aes(label = label_obitos), size = 2, family = 'Times') +
    theme_classic(base_family = 'Times', base_size = 8) +
    theme(axis.ticks = element_blank(), legend.position = "none") +
    labs(x = 'Óbitos', y = 'Tau') +
    scale_color_distiller(palette = 'RdBu')

plot_sinistros <- df_results |>
    filter(variavel == 'Sinistros com vítimas feridas') |>
    ggplot(aes(y = tau, x = sinistros)) +
    geom_point(aes(color = tau), size = 0.55, alpha = 1) +
    geom_text_repel(aes(label = label_sinistros), size = 2, family = 'Times') +
    theme_classic(base_family = 'Times', base_size = 8) +
    theme(axis.ticks = element_blank(), legend.position = "none") +
    labs(x = 'Sinistros com vítimas feridas', y = 'Tau') +
    scale_color_distiller(palette = 'RdBu')

scatter_tendencia <- plot_obitos + plot_sinistros
ggsave(
    'img/scatter_tendencia.png',
    plot = scatter_tendencia,
    width = 7,
    height = 3,
    dpi = 300
)

ggsave(
    'img/scatter_tendencia.svg',
    plot = scatter_tendencia,
    width = 7,
    height = 3,
    dpi = 300
)


results_obitos <- df_results |>
    filter(variavel == 'Óbitos totais') |>
    select(cod_ibge, tau)

sf_results_obitos <- sf_municipios |>
    left_join(results_obitos, by = 'cod_ibge')

map_obitos <- ggplot() +
    geom_sf(
        data = sf_results_obitos,
        aes(fill = tau),
        color = 'white',
        lwd = 0.1
    ) +
    theme_void(base_family = 'Times', base_size = 6) +
    scale_fill_distiller(palette = 'RdBu', na.value = 'grey80') +
    theme(legend.position = "top", legend.key.height = unit(0.3, 'cm')) +
    labs(fill = "Tau")

results_sinistros <- df_results |>
    filter(variavel == 'Sinistros com vítimas feridas') |>
    select(cod_ibge, tau)

sf_results_sinistros <- sf_municipios |>
    left_join(results_sinistros, by = 'cod_ibge')

map_sinistros <- ggplot() +
    geom_sf(
        data = sf_results_sinistros,
        aes(fill = tau),
        color = 'white',
        lwd = 0.1
    ) +
    theme_void(base_family = 'Times', base_size = 6) +
    scale_fill_distiller(
        palette = 'RdBu',
        na.value = 'grey80',
        limits = c(-0.6, 0.6)
    ) +
    theme(legend.position = "top", legend.key.height = unit(0.3, 'cm')) +
    labs(fill = "Tau")

maps <- map_obitos + map_sinistros

ggsave(filename = "img/maps.png", plot = maps, width = 8, height = 4, dpi = 300)
ggsave(filename = "img/maps.svg", plot = maps, width = 8, height = 4, dpi = 300)
