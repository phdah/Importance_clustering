############################################
# Automatic Data Exploration Process (ADEP)
# Author: Philip Sjöberg
# Date: 2020
############################################
library(rpart)
library(rpart.plot)
library(dplyr)
library(haven)
library(vip)
library(ggfortify)

# Clean the environment
rm(list = ls(all = TRUE))
cat("\014")

#######################################

wd = "C:/Users/" # Windows

file = "file.sav" # The name of the database file

Maindatabase = read_sav(paste0(wd, file), encoding = "UTF8")

# # Filter for var (exclude this if looking at all datapoints)
# unique(Maindatabase$var)
# var_filter = c(1, 2)
# Maindatabase = Maindatabase %>%
#   filter(var %in% var_filter)

# Clean data
Suffix_scale <- "_scale" #  Continuous variable
Suffix_factor <- "_factor" # Categorical variable
depentend_var = "dep"

# Set values
nmiss = c(11, 98) # Missing 
stringmiss = "Vet ej" # String missing

#######################################

factor_var = Maindatabase %>%
  select(ends_with(Suffix_factor)) %>%
  select(-NPS_factor) %>%
  mutate_each((as_factor))

factor_var[factor_var == stringmiss] = NA

num_var = Maindatabase %>%
  select(ends_with(Suffix_scale)) %>% 
  mutate_each((as.numeric))

data = cbind(Maindatabase[depentend_var], num_var, factor_var)

data[data == nmiss[1]] = NA
data[data == nmiss[2]] = NA

rm(factor_var); rm(num_var)

# Class tree (categorical) https://bradleyboehmke.github.io/HOML/DT.html
classTree = function(data, depth, n = nrow(data), min_split, depentend_var){
  start_time = Sys.time()
  print("**** Running ****")
  print(paste0("Removed value: ", nmiss))
  print(paste0("Removed string: ", stringmiss))
  data_cluster = data %>%
    sample_n(n)
  data_cluster[,depentend_var] = as_factor(data_cluster[,depentend_var])
  cont = rpart.control(minsplit = min_split, maxdepth = depth)
  fit = rpart(as.formula(paste0(depentend_var , " ~ .")), data = data_cluster, method='class', control = cont)
  print(Sys.time() - start_time)
  print(paste0("Number of observations: ", nrow(data_cluster)))
  print(paste0("Dependent variable is: ", depentend_var))
  print(paste0("Level: ", levels(data_cluster[,depentend_var])))
  print("**** Finished ****")
  return(fit)
  
}

#######################################

# Set min split and depth
fit = classTree(data, depth = 30, min_split = 30, depentend_var = depentend_var)

#######################################
# Plot cp and vip
plotcp(fit)
vip(fit, num_features = length(fit$variable.importance), geom = "point", aesthetics = list(colour = "blue", size = 2))

# Plot dendrogram
rpart.plot(fit, extra = 109, type = 1, main = (paste0("Tree for ", depentend_var, "\nNumber of observations: ", fit$frame[2][[1]][1])), roundint = F)

# Print variable importance
data.frame(fit$variable.importance)

frame = fit$frame
frame[['gini']] = 1 - (frame[['dev']] / frame[['n']])^2 - (1 - frame[['dev']] / frame[['n']])^2

cols = c('var','n','dev','gini')
frame = data.frame(frame[,cols])

# Print lable text
attri = unique(as.character(frame$var[frame$var!="<leaf>"]))
label = data.frame(cbind(Split = attri, label = sapply(1:length(attri), function(x)attributes(Maindatabase[[attri[x]]])$label)))
label_missing = data.frame(Missing = round(sapply(as.vector(label$Split), function(x)sum(is.na(data[x]))/nrow(data)), 2))
# Gini
print(frame)
# Tree splits
print(label)
print(label_missing)
