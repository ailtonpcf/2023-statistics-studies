library(tidyverse)

bac <- read_csv("raw/bacteria.genus.raw.csv")
fun <- read_csv("raw/fungi.genus.raw.csv")
valid <- read_csv("raw/valid_samples.csv")
biosam <- read_tsv("raw/biosamples.tsv")

soil <- biosam %>% 
  select(sample_id, environment_group) %>% 
  drop_na() %>% 
  distinct() %>% 
  filter(environment_group == "soil") %>% 
  pull(sample_id)

bac_long <- bac %>% 
  pivot_longer(-sample_id, names_to = "genus", values_to = "abundance") %>% 
  filter(abundance > 0) %>% 
  filter(sample_id %in% soil)

fun_long <- fun %>% 
  pivot_longer(-sample_id, names_to = "genus", values_to = "abundance") %>% 
  filter(abundance > 0) %>% 
  filter(sample_id %in% soil)

common <- intersect(bac_long$sample_id, fun_long$sample_id)

valid_bac <- valid %>%
  distinct(sample_id, bioproject_id) %>% 
  left_join(bac_long, multiple = "all")

valid_fun <-  valid %>%
  distinct(sample_id, bioproject_id) %>% 
  left_join(fun_long, multiple = "all")

valid_fun_bac <- bind_rows(valid_bac, valid_fun)

v_fun_bac_wid <- valid_fun_bac %>% 
  filter(sample_id %in% common) %>% 
  pivot_wider(names_from = genus, values_from = abundance, values_fill = 0) %>% 
  select(-sample_id, -bioproject_id)

norm_fun_bac <- v_fun_bac_wid %>% 
  as.matrix() %>% 
  scale()


cor_fun_bac <- cor(norm_fun_bac)

pca_fun_bac <- princomp(cor_fun_bac)
pca_fun_bac <- prcomp(cor_fun_bac)

pca_fun_bac$x[,1:2] %>% 
  as.data.frame() %>% 
  rownames_to_column("genus") %>% 
  as_tibble()
# Define function to replace underscores with spaces
# replace_underscore <- function(x) {
#   gsub("_", " ", x)
# }
# 
# v_fun_bac_fix <- v_fun_bac_wid %>% rename_with(~ str_replace(., " ", "_"), .cols = everything())



# # Extract column names
# col_names <- names(v_fun_bac_fix)
# 
# # Remove the response variable from the list of predictors
# response_var <- "Aspergillus"
# predictor_vars <- setdiff(col_names, response_var) %>%
#   discard(str_detect(., "-"))
# 
# new <- predictor_vars %>% 
#   as_tibble() %>% 
#   mutate(new = str_replace_all(value, " ", "_")) %>% 
#   filter(!str_detect(new, "\\[")) %>% 
#   pull(new) %>% 
#   .[1:745]
# 
# new[1]
# 
# form <- str_c(response_var, " ~ ", str_c(new, collapse = " + ")) %>% 
#   as.formula()
# 
# predictor_vars <- predictor_vars[15:20]
# 
# predictor_vars[15]
# 
# form <- str_c(response_var, " ~ ", str_c(predictor_vars, collapse = " + ")) %>% 
#   as.formula()
# 
# lm(formula = form, data = v_fun_bac_fix)
# 
# # Create linear model with all predictor variables
# model <- lm(paste("`", response_var, "` ~", paste("`", predictor_vars, "`", collapse = " + ")), data = v_fun_bac_fix)
# 
# 
# 
# ######
# 
# 
# # Add backticks to variable names
# predictor_vars_backticks <- paste0("`", predictor_vars, "`")
# 
# # Create linear model formula
# formula_str <- paste0("Aspergillus ~ ", paste(predictor_vars_backticks, collapse = " + "))
# formula_obj <- as.formula(formula_str)
# 
# # Fit linear model
# model <- lm(formula_obj, data = df)