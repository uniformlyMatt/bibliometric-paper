library(corrplot)
library(ggplot2)

plot_correlations <- function(correlation_matrix, sav) {
    # Plot a correlation matrix

    dev.new()
    cplot <- corrplot.mixed(
        correlation_matrix,
        lower = "circle",
        upper = "number",
        upper.col = "black",
        number.cex = .7,
        order = "hclust"
    )

    cplot <- corrplot(
        correlation_matrix,
        type = "lower",
        order = "hclust"
    )
    if (sav == TRUE) {
        ggsave(filename = "results/correlation_plot.pdf")
    }
}

plot_loadings <- function(loadings, sav) {
    # Plot the loadings from a single PCA model
    # TODO: fix the title so it shows the model name
    title <- sprintf("Loadings for PC1-%d", ncol(loadings))

    load_vars <- rownames(loadings)
    loadings <- melt(
        data.frame(load_vars, loadings),
        id.vars = c("load_vars"),
        variable.name = "loading"
    )
    colnames(loadings) <- c("Variable", "PC", "Loading")
    dev.new()
    loadings_plot <- ggplot(
            loadings,
            aes(Loading, Variable)
        ) +
        geom_bar(
            stat = "identity",
            fill = "#4682B4"
        ) +
        xlab("Loading") +
        ylab("Variable") +
        theme_bw() +
        theme(
            axis.text.x = element_text(angle = 45, hjust = 1)
        ) +
        facet_wrap(~PC, nrow = 1) +
        ggtitle(title)
    if (sav == TRUE) {
        ggsave(filename = "results/pc_loadings.pdf")
    }
    plot(loadings_plot)
}

density_plot <- function(column_name, data, sav) {
    # Plot the estimated pdf of PC1

    dev.new()
    if (column_name == "PC1") {
        dist_plot <- ggplot(data, aes(x = scale(PC1))) # nolint
    }

    dist_plot <- dist_plot +
    geom_density(
        aes(fill = factor(affil))
    ) +
    scale_fill_manual(
        values = c(
        "coral",
        "cadetblue1",
        "darkorchid",
        "pink1",
        "#22120a",
        "darkorange",
        "blue3",
        "darkolivegreen2"
        )
    ) +
    facet_wrap(~affil, ncol = 1) +
    labs(
        title = c(
            "Estimated Density of PC1 (Overall Publication Performance) by Site"
        ),
        x = "PC Score",
        fill = "Group",
        size = 1
    ) +
    theme_bw() +
    theme(legend.position = "none") +
    theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
    )
    plot(dist_plot)
    if (sav == TRUE) {
        ggsave(filename = "results/pc1_density.pdf")
    }
}

pc1_vs_pc2 <- function(data, sav) {
    # Plot PC1 vs PC2 for the PCA model
    dev.new()

    pc_plot <- ggplot(data, aes(
        PC1,
        PC2,
        color = factor(affil)
    ))
    pc_plot <- pc_plot +
        geom_point() +
        scale_colour_manual(
            values = c(
            "coral",
            "cadetblue1",
            "darkorchid",
            "pink1",
            "#22120a",
            "darkorange",
            "blue3",
            "darkolivegreen2"
            )
        )
    pc_plot <- pc_plot +
        theme_bw() +
        geom_density2d(aes(colour = factor(affil)), size = 0.1) +
        stat_density2d(aes(fill = ..level..), geom = "polygon", alpha = 0.1) +
        xlab("PC1") +
        ylab("PC2") +
        ggtitle("PC1 vs PC2") +
        theme(plot.title = element_text(
            color = "black",
            face = "bold",
            size = 14,
            hjust = -0.15)) +
        theme(axis.title = element_text(color = "black", size = 10))
    plot(pc_plot)
    if (sav == TRUE) {
        ggsave("results/pc1_vs_pc2.pdf")
    }
}