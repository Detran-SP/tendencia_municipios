library(targets)
library(tidyverse)
library(ggplot2)
library(ggview)
library(ost.utils)
library(ggrepel)
library(ggiraph)
library(patchwork)

tar_load(df_final)
tar_load(list_df_model)

set.seed(42)

# df_mk_obitos <- df_final |>
#     filter(
#         variavel %in% c("Óbitos totais", "Sinistros com vítimas feridas"),
#         metric == "tau"
#     ) |>
#     #select(-metric) |>
#     pivot_wider(names_from = variavel, values_from = value) |>
#     select(-ts, -integrado_snt)

# df_obitos <- list_df_model[[1]] |>
#     group_by(cod_ibge) |>
#     summarise(obitos = sum(n))

# df_sinistros <- list_df_model[[7]] |>
#     group_by(cod_ibge) |>
#     summarise(sinistros = sum(n))

# df_cluster_mk_obitos <- df_mk_obitos |>
#     left_join(df_obitos, by = 'cod_ibge') |>
#     left_join(df_sinistros, by = "cod_ibge")

# plot(df_cluster_mk_obitos$tau, log10(df_cluster_mk_obitos$populacao_estimada))

df_mk <- df_final |>
    select(cod_ibge, nome, populacao_estimada, variavel, metric, value) |>
    filter(
        metric == "tau",
        variavel %in% c("Óbitos totais", "Sinistros com vítimas feridas"),
        cod_ibge != 3550308
    ) |>
    pivot_wider(
        names_from = variavel,
        values_from = value,
        names_prefix = 'tau'
    ) |>
    janitor::clean_names() |>
    select(-metric) |>
    mutate(across(starts_with("tau"), \(x) if_else(is.na(x), 0, x)))

df_cluster <- df_mk |>
    left_join(df_obitos, by = 'cod_ibge') |>
    left_join(df_sinistros, by = "cod_ibge")

pca_results <- prcomp(
    df_cluster |> select(-cod_ibge, -nome),
    scale = TRUE,
    center = TRUE
)

df_cluster_results <- bind_cols(df_cluster, as_tibble(pca_results$x[, 1:2])) |>
    mutate(nome_id = gsub("'", "", nome))

ggplot(df_cluster_results, aes(x = PC1, y = PC2)) +
    geom_point(alpha = 0.5, color = "darkblue") +
    coord_fixed() +
    geom_hline(yintercept = 0, lwd = 0.5, lty = "dashed") +
    geom_vline(xintercept = 0, lwd = 0.5, lty = "dashed")

summary(pca_results)

wss <- numeric(10)
for (k in 1:10) {
    set.seed(123)
    wss[k] <- kmeans(
        df_cluster_results |>
            select(
                tau_obitos_totais,
                obitos
            ),
        centers = k,
        nstart = 25
    )$tot.withinss
}

plot(1:10, wss, type = "b")


km_results <- kmeans(
    df_cluster_results |>
        select(
            tau_obitos_totais,
            obitos
        ),
    centers = 3
)

df_cluster_results$cluster <- as.character(km_results$cluster)


loadings <- as_tibble(pca_results$rotation)
loadings$var <- c(
    "População",
    "Tau (óbitos)",
    "Tau (sinistros)",
    "Óbitos",
    "Sinistros"
)

colors <- palette_detran()

p1 <- ggplot(df_cluster_results, aes(x = PC1, y = PC2)) +
    geom_point_interactive(
        alpha = 0.9,
        aes(color = cluster, tooltip = nome_id, data_id = nome_id)
    ) +
    coord_fixed() +
    #geom_text(aes(label = nome)) +
    geom_hline(yintercept = 0, lwd = 0.5, lty = "dashed", color = "grey40") +
    geom_vline(xintercept = 0, lwd = 0.5, lty = "dashed", color = "grey40") +
    geom_segment(
        data = loadings,
        aes(x = 0, y = 0, xend = PC1 * 5, yend = PC2 * 5),
        arrow = arrow(length = unit(0.2, "cm")),
        color = "grey20"
    ) +
    geom_label_repel(
        data = loadings,
        aes(x = PC1 * 5, y = PC2 * 5, label = var),
        color = "grey20"
    ) +
    labs(x = "PC1 (59,39%)", y = "PC2 (21,05%)", color = "Cluster:") +
    scale_color_manual(
        values = c(
            colors$blue,
            colors$lightblue,
            colors$lightpurple
        )
    ) +
    theme_detran()

p2 <- sf_municipios |>
    left_join(df_cluster_results, by = "cod_ibge") |>
    ggplot() +
    geom_sf_interactive(
        aes(fill = cluster, tooltip = nome_id, data_id = nome_id),
        lwd = 0.05,
        color = "white"
    ) +
    theme_detran() +
    scale_fill_manual(
        values = c(
            colors$blue,
            colors$lightblue,
            colors$lightpurple
        )
    ) +
    labs(fill = "Cluster")

combined_plot <- p1 + p2

interactive_plot <- girafe(
    ggobj = combined_plot,
    width_svg = 16,
    height_svg = 10,
    options = list(opts_tooltip(use_fill = TRUE))
)

htmltools::save_html(interactive_plot, "plot.html")


plot(
    df_cluster_results |>
        select(
            populacao_estimada,
            tau_obitos_totais,
            tau_sinistros_com_vitimas_feridas,
            obitos,
            sinistros
        ),
    col = df_cluster_results$cluster
)

p1 <- ggplot(df_cluster_results, aes(x = obitos, y = tau_obitos_totais)) +
    geom_point_interactive(
        alpha = 0.9,
        aes(color = cluster, tooltip = nome_id, data_id = nome_id)
    ) +
    #coord_fixed() +
    #geom_text(aes(label = nome)) +
    #geom_hline(yintercept = 0, lwd = 0.5, lty = "dashed", color = "grey40") +
    #geom_vline(xintercept = 0, lwd = 0.5, lty = "dashed", color = "grey40") +
    # geom_segment(
    #     data = loadings,
    #     aes(x = 0, y = 0, xend = PC1 * 5, yend = PC2 * 5),
    #     arrow = arrow(length = unit(0.2, "cm")),
    #     color = "grey20"
    # ) +
    # geom_label_repel(
    #     data = loadings,
    #     aes(x = PC1 * 5, y = PC2 * 5, label = var),
    #     color = "grey20"
    # ) +
    #labs(x = "PC1 (59,39%)", y = "PC2 (21,05%)", color = "Cluster:") +
    scale_color_manual(
        values = c(
            colors$blue,
            colors$lightblue,
            colors$lightpurple
        )
    ) +
    theme_detran()

p2 <- sf_municipios |>
    left_join(df_cluster_results, by = "cod_ibge") |>
    ggplot() +
    geom_sf_interactive(
        aes(fill = cluster, tooltip = nome_id, data_id = nome_id),
        lwd = 0.05,
        color = "white"
    ) +
    theme_detran() +
    scale_fill_manual(
        values = c(
            colors$blue,
            colors$lightblue,
            colors$lightpurple
        )
    ) +
    labs(fill = "Cluster")

combined_plot <- p1 + p2

interactive_plot <- girafe(
    ggobj = combined_plot,
    width_svg = 16,
    height_svg = 10,
    options = list(opts_tooltip(use_fill = TRUE))
)

htmltools::save_html(interactive_plot, "plot2.html")

# Considerando só p-valor significativo
