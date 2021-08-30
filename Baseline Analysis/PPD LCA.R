library(easypackages)
libraries("magrittr", "poLCA", "tidyverse", "conflicted")
walk(c("filter", "select", "count", "group_by"), ~ conflict_prefer(., "dplyr"))

rm(list = ls())

df <- readRDS(file = "G:\\Shared drives\\Psychology_JSLab\\Projects\\In_Progress\\TRACK_to_TREAT_P1\\Data\\Processed_Data\\PPD Data\\Clean.rds")

vars <- names(df)[grepl("^def.*_child$", names(df))]
vars <- vars[!vars %in% c("def.none_child", "def.other_child")]

temp <- df %>%
  mutate_if(names(.) %in% vars,
            ~ . + 1)

fmla <- paste0("cbind(", paste0(vars, collapse = ", "), ") ~ 1")

lca.2 <- poLCA(data = temp, formula = as.formula(fmla), nclass = 2, graphs = F, nrep = 100)
lca.3 <- poLCA(data = temp, formula = as.formula(fmla), nclass = 3, graphs = T, nrep = 100)
lca.4 <- poLCA(data = temp, formula = as.formula(fmla), nclass = 4, graphs = F, nrep = 100)
lca.5 <- poLCA(data = temp, formula = as.formula(fmla), nclass = 5, graphs = F, nrep = 100)

diagnostics <- data.frame(
  classes = 2:5,
  AIC = c(lca.2$aic, lca.3$aic, lca.4$aic, lca.5$aic),
  BIC = c(lca.2$bic, lca.3$bic, lca.4$bic, lca.5$bic),
  G2 = c(lca.2$Gsq, lca.3$Gsq, lca.4$Gsq, lca.5$Gsq),
  X2 = c(lca.2$Chisq, lca.3$Chisq, lca.4$Chisq, lca.5$Chisq)
)

diagnostics

#Three class model minimizes AIC, BIC

df$class <- apply(lca.3$posterior, 1, function(x) which(x == max(x)))

df <- select(df, LSMHID, class)

saveRDS(lca.3, "G:\\Shared drives\\Psychology_JSLab\\Projects\\In_Progress\\TRACK_to_TREAT_P1\\Data\\Processed_Data\\PPD Data\\LCA.rds")
saveRDS(df, "G:\\Shared drives\\Psychology_JSLab\\Projects\\In_Progress\\TRACK_to_TREAT_P1\\Data\\Processed_Data\\PPD Data\\LCA Classes.rds")
