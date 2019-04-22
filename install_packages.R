packages_list <- c('readxl',
                   'arm',
                   'rpart.plot',
                   'rpart',
                   'MLmetrics',
                   'ROCR',
                   'tidyr',
                   'ggplot2',
                   'corrplot',
                   'InformationValue',
                   'GGally',
                   'gridExtra',
                   'tree',
                   'leaflet',
                   'jtools',
                   'lattice',
                   'car',
                   'caret',
                   'MASS',
                   'ggthemes',
                   'RColorBrewer',
                   'reshape',
                   'tidyverse',
                   'glmnet',
                   'dummies',
                   'fastDummies',
                   'e1071',
                   'dplyr',
                   'anchors',
                   'mlbench',
                   'boot',
                   'gridExtra',
                   'datasets',
                   'scales',
                   'ggplot2',
                   'fpc',
                   'gbm',
                   'data.table'
)

for (i in packages_list){
  if(!i%in%installed.packages()){
    install.packages(i, dependencies = TRUE)
    library(i, character.only = TRUE)
    print(paste0(i, ' has been installed'))
  } else {
    print(paste0(i, ' is already installed'))
    library(i, character.only = TRUE)
  }
}
