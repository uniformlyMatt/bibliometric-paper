# Performing PCA with author count variables

# load the required packages
library("ggplot2")
library("corrplot")
library("reshape")
library("future")

# get today's date
today <- format(Sys.time(), "%b_%d_%Y")

# load the author profiles
df_profiles <- read.csv("data/PCA-inputs.csv") # nolint

# log transform the count columns
cols <- colnames(df_profiles)[2:10] # first get the names of the count columns
df_profiles[cols] <- log(df_profiles[cols] + 1)

# select columns to use for PCA
df_inputs <- df_profiles[, 2:18]

#########
# Model #
#########

standard_pca <- prcomp(
    df_inputs,
    scale = TRUE,
    rank = 7
)

# get the PCA loadings and scores
loadings <- standard_pca$rotation[, 1:7]
scores <- standard_pca$x[, 1:7]

# store the scores from the various models with the profiles
score_column_names <- sprintf(
    "PC%d",
    seq_len(ncol(scores))
)
colnames(scores) <- score_column_names

df_scores <- cbind(
    df_profiles,
    scores
)

# comment these lines if you don't want to save the PC scores to CSV
result_name <- paste("data/profiles_with_pc_scores_", today, ".csv", sep = "")
write.csv(
    df_scores,
    result_name
)

# save the PC loadings
loadings_name <- paste("results/pc_loadings_", today, ".csv", sep = "")
write.csv(
    cbind(loadings),
    loadings_name
)

# save the variance explained by the model
explained_variance <- summary(standard_pca)$importance[2:3, 1:7]
explained_variance_name <- paste(
    "results/explained_variance_",
    today,
    ".csv",
    sep = ""
)
write.csv(
    cbind(explained_variance),
    explained_variance_name
)

#########
# Plots #
#########
source("src/pca_plots.r")

# plot the correlation matrix of the variables
correlation_matrix <- cor(df_inputs)
plot_correlations(correlation_matrix, sav = TRUE)

# plot the loadings from the PCA model
plot_loadings(cbind(loadings), sav = TRUE)

# plot estimated pdfs for PC1 from the PCA model
density_plot("PC1", df_scores, sav = TRUE)

# plot PC1 vs PC2 for PCA model
pc1_vs_pc2(data = df_scores, sav = TRUE)