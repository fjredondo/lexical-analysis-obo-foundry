library(tidyr)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(caret)
library(outForest)
library(pvclust)
library(stringr)

 
### FUNCTIONS ###


# Tries to identify the (path and) file name of the r script that is being
# debugged. Returns empty string if file name cannot be identified
getRootPath <- function() {
  path <- ""
  if(rstudioapi::isAvailable()==TRUE) {
    path <- rstudioapi::getActiveDocumentContext()$path
  }
  if(path == "") {
    path <- rstudioapi::getSourceEditorContext()$path
    if(is.null(path)) path <- ""
  }
  return(path)
}

## Combines the contents of files from a given path (param: folder_path) 
## and with a name that follows a pattern (param: pattern)
mergeFiles <- function(folder_path, pattern){

  dataset = data.frame()
  file_list <- list.files(folder_path, pattern)
  for (file_name in file_list){
    absolute_file_name = file.path(folder_path,"/",file_name)
    # if the merged dataset doesn't exist, create it
    if (!exists("dataset")){
      dataset <- read.csv2(absolute_file_name, header = T, sep = "\t", na.strings = "NaN", stringsAsFactors = F)
    }
    
    # if the merged dataset does exist, append to it
    if (exists("dataset")){
      temp_dataset <- read.csv2(absolute_file_name, header = T, sep = "\t", na.strings = "NaN", stringsAsFactors = F)
      dataset<-rbind(dataset, temp_dataset)
      rm(temp_dataset)
    }
  }
  return (dataset)
}

## Call mergeFiles to combine the contents of files with the IRIs corresponding to a metric.
## Calculate the metric of the whole repository and 
## return it as a list containing the following fields: metric, dividend, divisor and ratio
getWholeRepositoryMetric <- function(folder_path, pattern, orthogonal_view){
  
  # Combine the contents of files with the IRIs
  EntityWithNoGroup <- mergeFiles(folder_path, pattern)
  EntityWithNoGroup[[3]] = as.logical(EntityWithNoGroup[[3]])
  
  # Filter entities with no annotations
  EntityWithNoGroupTrue <- EntityWithNoGroup %>% filter(EntityWithNoGroup[[3]])
  EntityWithNoGroupTrue[[2]] = tolower(EntityWithNoGroupTrue[[2]])
  
  if (orthogonal_view){
    EntityWithNoGroupTrue <- unique(EntityWithNoGroupTrue)
  }
  
  EntityWithNoGroupTrue <- EntityWithNoGroupTrue[,2]
  
  # Filter entities with annotations
  EntityWithNoGroupFalse <- EntityWithNoGroup %>% filter(!EntityWithNoGroup[[3]])
  EntityWithNoGroupFalse[[2]] = tolower(EntityWithNoGroupFalse[[2]])
  
  if (orthogonal_view){
    EntityWithNoGroupFalse <- unique(EntityWithNoGroupFalse)
  }
  
  EntityWithNoGroupFalse <- EntityWithNoGroupFalse[,2]
  
  if (orthogonal_view){
    # Exclude the IRIs with annotation (at least one occurrence in the whole repository) from the set of entities with no annotations
    EntityWithNoGroupTrue <- setdiff(EntityWithNoGroupTrue, EntityWithNoGroupFalse)
    # At this point we have two disjoint sets of entities. 
    # One set with no annotations in the whole repository and one set with, at least one annotation.
  }
  

  
  metric <- EntityWithNoGroup[1,1]
  dividend <- length(EntityWithNoGroupTrue)
  divisor <- sum(length(EntityWithNoGroupTrue),length(EntityWithNoGroupFalse))
  ratio <- dividend / divisor
  
  rm(EntityWithNoGroup)
  rm(EntityWithNoGroupFalse)
  rm(EntityWithNoGroupTrue)
  gc()
  
  dr <- list(metric, dividend,divisor, ratio)
  
  return (dr)
}


## Call getWholeRepositoryMetric with the pattern "entity.*group*" relative to each metric.
## Return a dataframe with all metrics calculated considering the whole repository
getWholeRepositoryMetrics <- function(folder_path, orthogonal_view){

  df <- data.frame(metric=character(), dividend=numeric(), divisor=numeric(), ratio=double())
  dr <- getWholeRepositoryMetric(folder_path, pattern="Classes.*name", orthogonal_view)
  df <- rbind(df,setNames(data.frame(dr), colnames(df)))
  dr <- getWholeRepositoryMetric(folder_path, pattern="Classes.*synonym", orthogonal_view)
  df <- rbind(df,setNames(data.frame(dr), colnames(df)))
  dr <- getWholeRepositoryMetric(folder_path, pattern="Classes.*description", orthogonal_view)
  df <- rbind(df,setNames(data.frame(dr), colnames(df)))
  dr <- getWholeRepositoryMetric(folder_path, pattern="ObjectProperties.*name", orthogonal_view)
  df <- rbind(df,setNames(data.frame(dr), colnames(df)))
  dr <- getWholeRepositoryMetric(folder_path, pattern="ObjectProperties.*synonym", orthogonal_view)
  df <- rbind(df,setNames(data.frame(dr), colnames(df)))
  dr <- getWholeRepositoryMetric(folder_path, pattern="ObjectProperties.*description", orthogonal_view)
  df <- rbind(df,setNames(data.frame(dr), colnames(df)))  
  dr <- getWholeRepositoryMetric(folder_path, pattern="DataProperties.*name", orthogonal_view)
  df <- rbind(df,setNames(data.frame(dr), colnames(df)))
  dr <- getWholeRepositoryMetric(folder_path, pattern="DataProperties.*synonym", orthogonal_view)
  df <- rbind(df,setNames(data.frame(dr), colnames(df)))
  dr <- getWholeRepositoryMetric(folder_path, pattern="DataProperties.*description", orthogonal_view)
  df <- rbind(df,setNames(data.frame(dr), colnames(df)))    
  dr <- getWholeRepositoryMetric(folder_path, pattern="AnnotationProperties.*name", orthogonal_view)
  df <- rbind(df,setNames(data.frame(dr), colnames(df)))
  dr <- getWholeRepositoryMetric(folder_path, pattern="AnnotationProperties.*synonym", orthogonal_view)
  df <- rbind(df,setNames(data.frame(dr), colnames(df)))
  dr <- getWholeRepositoryMetric(folder_path, pattern="AnnotationProperties.*description", orthogonal_view)
  df <- rbind(df,setNames(data.frame(dr), colnames(df)))    
  
  return (df)
  
}


### LOAD DATA ###

# Get data paths
rootPath = dirname(dirname(getRootPath()))
# Other solution: fs::path_norm(fs::path(getRootPath(), "..",".."))
candidateResultsPath = file.path(rootPath, 'results', 'candidates_results', 'allMetrics.tsv')
memberResultsPath = file.path(rootPath, 'results', 'members_results', 'allMetrics.tsv')

# Read source files and compose the dataset to be analysed
candidates = read.csv2(candidateResultsPath, header = T, sep = "\t", na.strings = "NaN", stringsAsFactors = F)
members = read.csv2(memberResultsPath, header = T, sep = "\t", na.strings = "NaN", stringsAsFactors = F)
candidates$Member = F
members$Member = T
all = rbind(members, candidates)
all$Value = as.numeric(all$Value)
all$File = as.character(all$File)

### DESCRIPTIVE ANALYSIS ###

# Summary of the readability metrics regarding classes
summary(spread(all, Metric, Value) %>% select(File, Member, contains('Classes with')))
# Complete data of the readability metrics regarding classes
View(spread(all, Metric, Value) %>% select(File, Member, contains('Classes with')))

# Summary of the readability metrics regarding ObjectProperties
summary(spread(all, Metric, Value) %>% select(File, Member, contains('ObjectProperties with')))
# Complete data of the readability metrics regarding ObjectProperties
View(spread(all, Metric, Value) %>% select(File, Member, contains('ObjectProperties with')))

# Summary of the readability metrics regarding DataProperties
summary(spread(all, Metric, Value) %>% select(File, Member, contains('DataProperties with')))
# Complete data of the readability metrics regarding DataProperties
View(spread(all, Metric, Value) %>% select(File, Member, contains('DataProperties with')))

# Summary of the readability metrics regarding AnnotationProperties
summary(spread(all, Metric, Value) %>% select(File, Member, contains('AnnotationProperties with')))
# Complete data of the readability metrics regarding AnnotationProperties
View(spread(all, Metric, Value) %>% select(File, Member, contains('AnnotationProperties with')))


### WHOLE REPOSITORY ANALYSIS ###

#METHOD 1: Calculate the metrics taking into account the IRIs of entities in the whole repository.

# Set of metric with orthogonal view of the repository. Only one annotated entity in the repository makes the value of its metrics zero.
# In addition, there can be no repeated IRIs
dataset_orthogonal_true = getWholeRepositoryMetrics(folder_path=file.path(rootPath, 'results', 'detailed_files'),orthogonal_view = TRUE)
View(dataset_orthogonal_true)
#write.table(dataset,file= file.path(rootPath, 'results', 'detailed_files','output_metrics_orthogonal.tsv'), sep='\t', row.names=FALSE)

# Set of metric with no orthogonal view of the repository. Metrics are zero only if the number of non-annotated entities is zero. 
# In addition, repeated IRIs may exist
dataset_orthogonal_false = getWholeRepositoryMetrics(folder_path=file.path(rootPath, 'results', 'detailed_files'), orthogonal_view = FALSE)
View(dataset_orthogonal_false)
#write.table(dataset2,file= file.path(rootPath, 'results', 'detailed_files','output_metrics_all_ontologies.tsv'), sep='\t', row.names=FALSE)

to_plot = merge(x = dataset_orthogonal_true, y = dataset_orthogonal_false, by="metric") %>% na.omit()
to_plot = to_plot %>% select(metric, ratio.x, ratio.y)
colnames(to_plot) = c("Metric", "Orthogonal", "Non_Orthogonal")
colors = c("blue", "red")
names(colors) = c('Orthogonal', 'Non Orthogonal')

ggplot(data=to_plot, aes(x= reorder(Metric, -Orthogonal, decreasing = TRUE))) + 
  geom_line(aes(y = Non_Orthogonal, color = "Non Orthogonal", group = 1)) + 
  geom_line(aes(y = Orthogonal, color  = "Orthogonal", group = 1)) + 
  labs(x = "Metric", y = "Value", color = "Legend") + 
  scale_color_manual(values = colors) +
  theme(axis.text.x = element_text(angle = 70, hjust=1))



# 
# # Combine the contents of files with the IRIs
# EntityWithNoGroup <- mergeFiles(folder_path=file.path(rootPath, 'results', 'detailed_files'), pattern="ObjectProperties.*description")
# EntityWithNoGroup[[2]] = tolower(EntityWithNoGroup[[2]])
# EntityWithNoGroup[[3]] = as.logical(EntityWithNoGroup[[3]])
# # Filter entities with no annotations
# EntityWithNoGroupTrue <- EntityWithNoGroup %>% filter(EntityWithNoGroup[[3]])
# EntityWithNoGroupTrue <- EntityWithNoGroupTrue[,2]
# 
# # Filter entities with annotations
# EntityWithNoGroupFalse <- EntityWithNoGroup %>% filter(!EntityWithNoGroup[[3]])
# EntityWithNoGroupFalse <- EntityWithNoGroupFalse[,2]
# 
# summary(EntityWithNoGroup)
# EntityFrec <- EntityWithNoGroup %>% select(2, 3) %>% table() %>% as.data.frame()
# EntityFrecSpread <- spread(EntityFrec, WithNoDescription, Freq)
# write.table(EntityFrecSpread,file= file.path(rootPath, 'results', 'detailed_files','output_ObjectPropertiesWithNoDescription.tsv'), sep='\t', row.names=FALSE)

