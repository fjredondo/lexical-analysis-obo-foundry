library(tidyr)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(caret)
library(outForest)
library(pvclust)
library(stringr)

 
### FUNCTIONS ###

# Creates a figure from 'obj' in bmp format with the size specified
# by 'w' (width) and 'h' (height). The figure is saved in the specified path.
exportImage <- function(path, w, h, obj){
  setEPS()
  bmp(paste(path, ".bmp", sep = ""), width = w, height = h)
  plot(obj)
  dev.off()
}

# Creates a figure from the dendrogram in 'obj' in bmp format with the size specified
# by 'w' (width) and 'h' (height). The dendrogram is saved in the specified path.
# Clusters supported by a significance higher than 0.95 are marked.
exportDendrogram <- function(path, w, h, obj){
  setEPS()
  bmp(paste(path, ".bmp", sep = ""), width = w, height = h)
  plot(obj)
  pvrect(obj, alpha = 0.95)
  dev.off()
}


# Calculates the cosine distance
cosine_wrap <- function(x) {
  res = philentropy::distance(as.data.frame(as.matrix(t(x))), method = "cosine", use.row.names = TRUE)
  res = 1 - res
  res = as.dist(res)
  attr(res, "method") <- "cosine"
  return(res)
}
attr(cosine_wrap, 'name') <- 'cosine'





# Preprocess the data. This function receives the data from the study in long format
# together with the metrics to take into account.
# 1. Remove near zero variance predictors.
# 2. Scale the data.
# 3. Remove outliers
# 4. Remove near zero variance predictors.
# 5. Return the original data together with the processed data.
preprocessData <- function(longData, metricsToUse){
  wideData = spread(filter(longData, Metric %in% metricsToUse), Metric, Value)
  row.names(wideData) = tools::file_path_sans_ext(wideData$File)
  x = select(wideData, -File, -Member)
  print(paste("preprocessedData ROW COUNT: ", nrow(x)))
  print(unique(rownames(which(is.na(x), arr.ind=TRUE))))
  x = na.omit(x)
  print(paste("Removing near zero var columns:", colnames(x)[nearZeroVar(x)]))
  x = select(x, -nearZeroVar(x))
  originalData = data.frame(x)
  print(paste("originalData ROW COUNT: ", nrow(originalData)))
  #x = scale(x)
  x = as.data.frame(as.matrix(x))
  # Outliers removal
  col_names = colnames(x)
  colnames(x) = gsub(" ", "_", colnames(x))
  outliers = outForest(x, replace = "NA", seed = 12345)
  print(paste("Removing outliers:\n", toString(outliers(outliers))))
  View(outliers(outliers))
  x = Data(outliers)
  colnames(x) = col_names
  x = drop_na(x)
  #print(paste("Removing near zero var columns after removing outliers:", colnames(x)[nearZeroVar(x)]))
  #x = select(x, -nearZeroVar(x))
  print(paste("preprocessedData ROW COUNT: ", nrow(x)))
  #print(col_names)
  return (list("originalData" = originalData, "preprocessedData" = x))
}

# Receives the groups formed by a clustering algorithm, the dataframe with the original data
# and compare the groups according to the variables. Then, a figure including boxplots depicting
# the comparisons between each group for each metric is generated in 'plotFileOutDir'.
compareClusters <- function (groups, df, plotFileOutDir) {
  all_metrics = c("Classes with no name", 
                  "Classes with no synonym", 
                  "Classes with no description", 
                  "ObjectProperties with no name", 
                  "ObjectProperties with no synonym", 
                  "ObjectProperties with no description", 
                  "DataProperties with no name", 
                  "DataProperties with no synonym", 
                  "DataProperties with no description", 
                  "AnnotationProperties with no name", 
                  "AnnotationProperties with no synonym", 
                  "AnnotationProperties with no description")
  x = data.frame(df)
  x$cluster = NA
  for (row_name in rownames(x)){
    if(row_name %in% names(groups)){
      x[row_name, "cluster"] = groups[[row_name]]
    }
  }
  x = na.omit(x)
  # Get the comparison table per cluster and metric.
  col_names = str_trim(gsub("\\.", " ", colnames(x)), side = "both")
  colnames(x) = col_names
  summary = x %>% group_by(cluster)  %>% summarise_all(mean) %>% select(any_of(all_metrics))
  
  # Draw figures comparing clusters
  metrics = colnames(x)[colnames(x) %in% metricsToShow]
  x = gather(x, key = 'Metric', value = 'Value',  metrics)
  x$cluster = as.factor(x$cluster)
  
  comparisons = list()
  for (i in 1:(max(groups)-1)){
    for (j in (i+1):max(groups)){
      comparisons[[length(comparisons) + 1]] = c(as.character(i), as.character(j))
    }
  }
  setEPS()
  figure = ggplot(data = x, aes(x=cluster, y=Value)) + facet_wrap(~x$Metric) + geom_boxplot() + stat_compare_means(comparisons = comparisons)
  exportImage(plotFileOutDir, w=800, h=1200, figure)
  return (t(summary))
}


# This function receives the original data in long format, a list of metrics to take into account,
# a list of distances measures, a list of clustering methods, a base folder to put the generated figures,
# and a boolean 'clusterMetrics' indicating that the metrics should be clustered instead of the ontologies.
# 1. Preprocess the data.
# 2. Generate a clustering for each (distanceMethod, clusteringMethod).
# 3. Save the generated dendrogram in 'baseFolder' with a name indicating the used method.
# 4. If clusterMetrics is false, evaluate the clustering by generating boxplots comparing the formed groups
#    according to each evaluated metric.
# 5. Save the evaluation plot in the 'baseFolder'.
generateAndEvaluateClusters <- function(longData, metricsToUse, distancesToEval, clustMethodsToEval, baseFolder, clusterMetrics=F){
  nboot=1000
  parallel=TRUE
  seed = 563
  if (!dir.exists(file.path(baseFolder))){
    dir.create(file.path(baseFolder))
  }
  
  preprocess_info = preprocessData(longData, metricsToUse)
 # preprocess_info = list("originalData" = originalData, "preprocessedData" = x)
  x = preprocess_info$preprocessedData
  originalData = preprocess_info$originalData
  
  if(clusterMetrics){
    x = as.data.frame(as.matrix(t(x)))
  }
  
  for (distMethod in distancesToEval){
    if (is.function(distMethod)){
      distName = attr(distMethod, 'name')
    } else {
      distName = distMethod
    }
    
    for (hclustMethod in clustMethodsToEval){
      info = paste(distName,hclustMethod, sep = "-")
      clustering = pvclust(as.data.frame(as.matrix(t(x))), method.dist=distMethod, method.hclust=hclustMethod, nboot=nboot, parallel=parallel, iseed = seed)
      # TODO: Obtener matriz de distancias
      # TODO: Obtener la silueta mediante la matriz de distancias 
      # y los grupos obtenidos (funcion silhouette del paquete cluster)
      
      #dendrogramPlot = paste(getwd(), "/", baseFolder, "/", info, sep = "")
      #evalPlot = paste(getwd(), "/", baseFolder, "/", info, '-eval', sep = "")
      dendrogramPlot = paste(baseFolder, "/", info, sep = "")
      evalPlot = paste(baseFolder, "/", info, '-eval', sep = "")
      exportDendrogram(dendrogramPlot, w=1400, h=600, clustering)
      if(!clusterMetrics){
        aux = pvpick(clustering, alpha=0.95)
        groups = rep(seq_along(aux$clusters), times = sapply(aux$clusters, length))
        names(groups) = unlist(aux$clusters)
        if (length(groups) != 0){
          compareClusters(groups, originalData, evalPlot)  
        }
      }
    }
    
    
  }
}

# Provides formatted mean data to stat_summary
formatted_mean <- function(x){
  return(data.frame(y=mean(x),label=sprintf("%0.1f", round(mean(x,na.rm=T)*100, digits = 1))))}

# Receives the original data in long format and compares candidate and member ontologies
# from OBO Foundry according to each metric. Wilcoxon test is also used to show the significance.
compareCandidatesAndMembers <- function(all){
  
  all_i = filter(all, Metric %in% metricsToShow)
  x = spread(all_i, Metric, Value)
  pvalues = c()
  metrics = c()
  memberMean = c()
  candidateMean = c()
  for (metric in unique(all$Metric)) {
    metrics = c(metrics, metric)
    
    x_member = x %>% select(File, Member, matches(metric)) %>% filter(Member==T) 
    x_member = x_member[[metric]]
    memberMean = c(memberMean, mean(x_member, na.rm=T))
    
    x_candidate = x %>% select(File, Member, matches(metric)) %>% filter(Member==F)
    x_candidate = x_candidate[[metric]]
    candidateMean = c(candidateMean, mean(x_candidate, na.rm=T))
    
    p = wilcox.test(x_member, x_candidate)$p.value
    pvalues = c(pvalues, p)
  }
  comparison = as.data.frame(cbind(metrics, memberMean, candidateMean, pvalues))
  comparison = data.frame(metrics=metrics, memberMean=memberMean, candidateMean=candidateMean, pvalues=pvalues)
  colnames(comparison) = c('Metric', 'Mean of members', 'Mean of candidates', 'p-value')
  
  return (comparison)
}


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
getWholeRepositoryMetric <- function(folder_path, pattern){
  
  # Combine the contents of files with the IRIs
  EntityWithNoGroup <- mergeFiles(folder_path, pattern)
  EntityWithNoGroup[[3]] = as.logical(EntityWithNoGroup[[3]])
  
  # Filter entities with no annotations
  EntityWithNoGroupTrue <- EntityWithNoGroup %>% filter(EntityWithNoGroup[[3]])
  EntityWithNoGroupTrue[[2]] = tolower(EntityWithNoGroupTrue[[2]])
  EntityWithNoGroupTrue <- unique(EntityWithNoGroupTrue)
  EntityWithNoGroupTrue <- EntityWithNoGroupTrue[,2]
  
  # Filter entities with annotations
  EntityWithNoGroupFalse <- EntityWithNoGroup %>% filter(!EntityWithNoGroup[[3]])
  EntityWithNoGroupFalse[[2]] = tolower(EntityWithNoGroupFalse[[2]])
  EntityWithNoGroupFalse <- unique(EntityWithNoGroupFalse)
  EntityWithNoGroupFalse <- EntityWithNoGroupFalse[,2]
  
  # Exclude the IRIs with annotation (at least one occurrence in the whole repository) from the set of entities with no annotations
  EntityWithNoGroupTrue <- setdiff(EntityWithNoGroupTrue, EntityWithNoGroupFalse)
  
  # At this point we have two disjoint sets of entities. 
  # One set with no annotations in the whole repository and one set with, at least one annotation.
  
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
getWholeRepositoryMetrics <- function(folder_path){

  df <- data.frame(metric=character(), dividend=numeric(), divisor=numeric(), ratio=double())
  dr <- getWholeRepositoryMetric(folder_path, pattern="Classes.*name")
  df <- rbind(df,setNames(data.frame(dr), colnames(df)))
  dr <- getWholeRepositoryMetric(folder_path, pattern="Classes.*synonym")
  df <- rbind(df,setNames(data.frame(dr), colnames(df)))
  dr <- getWholeRepositoryMetric(folder_path, pattern="Classes.*description")
  df <- rbind(df,setNames(data.frame(dr), colnames(df)))
  dr <- getWholeRepositoryMetric(folder_path, pattern="ObjectProperties.*name")
  df <- rbind(df,setNames(data.frame(dr), colnames(df)))
  dr <- getWholeRepositoryMetric(folder_path, pattern="ObjectProperties.*synonym")
  df <- rbind(df,setNames(data.frame(dr), colnames(df)))
  dr <- getWholeRepositoryMetric(folder_path, pattern="ObjectProperties.*description")
  df <- rbind(df,setNames(data.frame(dr), colnames(df)))  
  dr <- getWholeRepositoryMetric(folder_path, pattern="DataProperties.*name")
  df <- rbind(df,setNames(data.frame(dr), colnames(df)))
  dr <- getWholeRepositoryMetric(folder_path, pattern="DataProperties.*synonym")
  df <- rbind(df,setNames(data.frame(dr), colnames(df)))
  dr <- getWholeRepositoryMetric(folder_path, pattern="DataProperties.*description")
  df <- rbind(df,setNames(data.frame(dr), colnames(df)))    
  dr <- getWholeRepositoryMetric(folder_path, pattern="AnnotationProperties.*name")
  df <- rbind(df,setNames(data.frame(dr), colnames(df)))
  dr <- getWholeRepositoryMetric(folder_path, pattern="AnnotationProperties.*synonym")
  df <- rbind(df,setNames(data.frame(dr), colnames(df)))
  dr <- getWholeRepositoryMetric(folder_path, pattern="AnnotationProperties.*description")
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

# Comparison between member and candidate ontologies according to the considered metrics (readability)
metricsToShow = unique(all$Metric)
View(compareCandidatesAndMembers(filter(all, Metric %in% metricsToShow)))

#boxplot to compare candidates and member.
ggplot(all, aes(x=Metric, y=Value, fill=Member)) + 
  geom_boxplot() + 
  scale_fill_manual(values = c("White","grey80"), name="", labels=c("not member","member")) +
  stat_compare_means(method = "wilcox.test", label = "p.format", label.y = 1.05) +
  theme(axis.text.x = element_text(angle = 65, hjust=1))

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

# Metrics ranking by mean.
metricsToShow = unique(all$Metric)

x = filter(all, Metric %in% metricsToShow)
summary(x)
x$Metric = factor(x$Metric, levels=metricsToShow)

x_order_mean = select(x, -File, -Member)

x_order_mean = x_order_mean %>%  
  group_by(Metric) %>% summarise(mean = mean(Value, na.rm=TRUE)) %>% 
  arrange(mean) 

metrics <- unlist(lapply(x_order_mean[-2], as.character))
metrics_list = rbind(metrics[-length(metrics)],metrics[-1])
comparisons = split(metrics_list, col(metrics_list))

x%>%
  group_by(Metric)%>%
  mutate(mean_metric=mean(Value)*100)%>%
ggplot(aes(x=reorder(Metric,-Value, na.rm = TRUE, decreasing = TRUE), y=Value, na.rm = TRUE)) + 
  geom_boxplot() + 
  stat_summary(fun=mean, geom="point", shape=4, size=1, color="black", fill="black") +
  stat_summary(fun.data = formatted_mean, geom="text", vjust=-0.7) +  
  stat_compare_means(comparisons = comparisons) + 
  theme(axis.text.x = element_text(angle = 45, hjust=1))

### CLUSTERING ANALYSIS ###

# Ontology clustering according to the readability metrics
metricsToShow = unique(all$Metric)
# DataProperties metrics are excluded
metricsToShow <- metricsToShow[!grepl('DataProperties', metricsToShow)]

# #distances = c("euclidean", "correlation", "maximum", "manhattan", "canberra", "minkowski", cosine_wrap)
distances = c("maximum")
# #clustMethods = c("ward.D", "single", "complete", "average", "mcquitty", "median", "centroid")
clustMethods = c("ward.D")
base_dir = file.path(rootPath, 'results', 'dendrograms', 'readability_clustering')


generateAndEvaluateClusters(all, metricsToShow, distances, clustMethods, base_dir)


### WHOLE REPOSITORY ANALYSIS ###

#METHOD 1: Calculate the metrics taking into account the IRIs of entities in the whole repository.
dataset = getWholeRepositoryMetrics(folder_path=file.path(rootPath, 'results', 'detailed_files'))
View(dataset)

#METHOD 2: Mean metric weighted by TotalEntities of each ontology.

#Merge all files with candidate ontologies in a dataframe. Files contains the following fields: File; Metric; Value; NumberOfEntities; TotalEntities
detailed_allMetrics_candidates <- mergeFiles(folder_path=file.path(candidateResultsPath,"/.."),pattern="detailed_")
#Merge all files with member ontologies in a dataframe. Files contains the following fields: File; Metric; Value; NumberOfEntities; TotalEntities
detailed_allMetrics_members <- mergeFiles(folder_path=file.path(memberResultsPath,"/.."),pattern="detailed_")
#Set a dataframe with the metrics of all repository 
all_detailed <- rbind(detailed_allMetrics_members,detailed_allMetrics_candidates)
rm(detailed_allMetrics_candidates)
rm(detailed_allMetrics_members)
all_detailed$Value = as.numeric(all_detailed$Value)
all_detailed$NumberOfEntities = as.numeric(all_detailed$NumberOfEntities)
all_detailed$TotalEntities = as.numeric(all_detailed$TotalEntities)
all_detailed$File = as.character(all_detailed$File)

#Calculate the mean of metrics weighted by TotalEntities
df_summary <- 
  all_detailed %>%
  group_by(Metric) %>% 
  summarise(weighted_metric_meanvalue= weighted.mean(Value, TotalEntities))
view(df_summary)


