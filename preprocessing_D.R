# by Eric Wang
# 20230130

if(!require('devtools')) install.packages('devtools')
if(!require('R.matlab')) install.packages('R.matlab')
if(!require('corrplot')) install.packages('corrplot')
if(!require('Hmisc')) install.packages('Hmisc')
if(!require('gplots')) install.packages('gplots')
if(!require('gridGraphics')) install.packages('gridGraphics')

library(R.matlab)
library(readr)
library(Hmisc)
library(tidyr)
library(dplyr)
library(tibble)
library(corrplot)
library(gplots) # enhanced heatmap
library(ggplot2)

setwd("G:/My Drive/U/2022_WORK_UNISA/WORK_UNISA/Digital Capabilities Program/project_rehabSwift/stroke-sensors")


# 1. First run ----

## load data ---------
data <- readMat('G:\\My Drive\\U\\2022_WORK_UNISA\\WORK_UNISA\\Digital Capabilities Program\\project_rehabSwift\\stroke-sensors\\data\\master_data.mat')
str(data)

participants <- c('Tulio', 'Sophie', 'Roland', 'Richard', 'Renee', 'Preet', 'Morgan19', 
                  'Michael', 'Maneesh', 'Luke', 'Jason', 'Hugh', 'Helen', 'Flynn',
                  'Connor', 'Ben19', 'Amelie', 'Daniel') %>% tolower()
length(participants)

classes <- c('Amuse', 'Anger', 'Disgust', 'Fear', 'Sadness', 'Surprise1', 'Surprise2', 
             'Relax', 'Relax2', 'count', 'focus') %>% tolower()
length(classes)


## define functions ----------
## function
depth <- function(this, thisdepth = 0) {
  if (!is.list(this)) {
    return(thisdepth)
  } else {
    return(max(unlist(lapply(this, depth, thisdepth = thisdepth + 1))))
  }
}

set_person_class_sensor <- function(cell_df, p, c, s) {
  rownames(cell_df) <- paste0(c, '.', p, '.', seq(1:dim(cell_df)[1]))
  colnames(cell_df) <- paste0(s, '.', colnames(cell_df))
  cell_df <- rownames_to_column(cell_df, 'id')
  return(cell_df)
}

## p*.384
set_p384 <- function(p, write_stage_file=FALSE, write_final_file=TRUE) {
  
  if((dim(p)[1] != 11) & (dim(p)[2] != 6)) {
    return(print('incorrect input, expect shape (11, 6)'))
  }
  
  pn <<- deparse(substitute(p))
  print(paste('INFO: pn:', pn))
  
  # get N*64 for each class
  i <- 1
  for(c in classes) {
    assign(paste0(pn,'.',c,'.s1'), p[i,][[1]][[1]] %>% data.frame() %>% set_person_class_sensor(pn,c,'s1'), envir = .GlobalEnv )
    assign(paste0(pn,'.',c,'.s2'), p[i,][[2]][[1]] %>% data.frame() %>% set_person_class_sensor(pn,c,'s2'), envir = .GlobalEnv )
    assign(paste0(pn,'.',c,'.s3'), p[i,][[3]][[1]] %>% data.frame() %>% set_person_class_sensor(pn,c,'s3'), envir = .GlobalEnv )
    assign(paste0(pn,'.',c,'.s4'), p[i,][[4]][[1]] %>% data.frame() %>% set_person_class_sensor(pn,c,'s4'), envir = .GlobalEnv )
    assign(paste0(pn,'.',c,'.s5'), p[i,][[5]][[1]] %>% data.frame() %>% set_person_class_sensor(pn,c,'s5'), envir = .GlobalEnv )
    assign(paste0(pn,'.',c,'.s6'), p[i,][[6]][[1]] %>% data.frame() %>% set_person_class_sensor(pn,c,'s6'), envir = .GlobalEnv )
    i <- i+1
  }
  print('INFO: Step 1/5 completed (N*64)')
  
  # get n*384 for each class
  for(c in classes){
    df_list <<- list()
    df_list <<- mget(ls(pattern=glob2rx(paste0(pn,'.', c, '.s*')), envir = .GlobalEnv), .GlobalEnv)
    print(paste('INFO: length of df_list:', length(names(df_list))))
    if(length(names(df_list)) == 6){
      # print(names(df_list))
      # reduce to 384, then expand to 1152
      assign(paste0(pn,'.',c,'.384'), 
             Reduce(function(x,y) merge(x,y,all=FALSE), df_list), 
             envir = .GlobalEnv)
    } else {
      print('ERROR: (get n*384) incorrect pattern')
      stop()
    }
  }
  print('INFO: Step 2/5 completed (n*384)')
  
  # write to files
  if(write_stage_file){
    for(c in classes){
      file_ <<- list()
      file_ <<- mget(ls(pattern = glob2rx(paste0(pn,'.', c, '.384')), envir = .GlobalEnv), .GlobalEnv)
      if(length(names(file_)) == 1) {
        print(paste('INFO: files to be written:', names(file_)))
        write.csv(file_, paste0('dataset/participant/',pn, '.', c, '.384.csv'))
      } else {
        print('ERROR: (write to files) found multiple filenames, no file saved')
        stop()
      }
    }
    print('INFO: Step 3/5 completed (write p*.class.384.csv to files)')  
  } else {
    print('INFO: Step 3/5 skipped (write p*.class.384.csv to files')
  }
  
  # rbind p together
  p_list <<- list()
  p_list <<- mget(ls(pattern = glob2rx(paste0(pn,'.*.384')), envir = .GlobalEnv), .GlobalEnv)
  print(paste('INFO: p_list to be rbind:', names(p_list)))
  ifelse(length(p_list) == 11, p.384 <<- do.call('rbind', p_list), print('ERROR: (rbind) incorrect pattern'))
  print(paste('INFO: current p dim:', dim(p.384)))
  print(paste0('INFO: Step 4/5 completed (rbind to ', pn, '.384)'))
  
  # write to file
  if(write_final_file) {
    write.csv(p.384, paste0('dataset/participant/',pn,'.384.csv'))
    print(paste0('INFO: Step 5/5 completed (write ', pn, '.384.csv to file)'))  
  } else {
    print('INFO: Step 5/5 skipped (write p*.384 to file')
  }
  
  print('INFO: all steps completed!')
  
  return(p.384)
}

## expand to 1152
expand_rows <- function(p) {
  
  if(dim(p)[2] != 385) {
    return(print('ERROR: (expand_rows) incorrect input, expect shape (n,385)'))
  }
  
  n <- 3
  tmp <- p %>% 
    # subset to number of rows that are dividable by 3 (remove last 1 or 2 rows if necessary, for given p)
    slice(1: (n*floor(n()/n))) %>%
    # add two new columns as preparation (define the grouping and sequences)
    mutate(cols = rep(1:n, n()/n), id2 = rep(1:(n()/n), each=n)) %>% 
    # expand from 384 to 1152 features, plus 1 id, totally 1153 columns for given p
    pivot_wider(id_cols = id2, 
                names_from = cols, 
                values_from = colnames(.)[1:385], 
                names_prefix = 'mv' ) %>%
    # remove "id2", "id_mv2", "id_mv3
    select(-c(1,3,4)) %>%
    # rename "id_mv1" back to "id"
    rename(id = id_mv1) %>%
    data.frame()
  
  return(tmp)
}

## p*.1152
set_p1152 <- function(p) {
  
  # check
  if(dim(p)[2]-1 != 384) {
    return(print('incorrect input, expect shape (n, 384)'))
  }

  # rename (remove ".384")
  pn <<- deparse(substitute(p))
  if(pn == '.') {
    # get the name of the passed in variable (including pipeline passed)
    first_call <- sys.calls()[[1]]
    lhs <- first_call[[2]]
    pn <<- rlang::as_name(lhs)
  }
  
  pn <<- sub('.384','', pn)
  print(paste('INFO: pn:', pn))

  # expand to 1152
  assign(paste0(pn, '.1152'), expand_rows(p), envir = .GlobalEnv)
  print(paste('INFO: Step 1/2 completed (expanded', pn, 'rows from', dim(p)[2], 'to 1152'))

  # write to file
  file_1152 <<- mget(ls(pattern = glob2rx(paste0(pn, '.1152')), envir = .GlobalEnv), .GlobalEnv)
  if(length(names(file_1152))==1){
    write.csv(file_1152[[1]], paste0('dataset/participant/',pn,'.1152.csv'))
    print(paste0('INFO: Step 2/2 completed (wrote ', pn, '.1152.csv to file)'))  
  } else {
    print('ERROR: (write 1152 file) multiple names in file_1152, expect only 1 file')
  }

  return(file_1152[[1]])
}

# plot df384 correlation by sensor
plot_pc384_by_sensor <- function(df.pc384){
  s1 <- 1:64
  s2 <- 65:128
  s3 <- 129:192
  s4 <- 193:256
  s5 <- 257:320
  s6 <- 321:384
  par(mfrow=c(2,3))
  mar <- c(1,0,2,0)
  cor.matrix <- cor(df.pc384[,s1])
  diag(cor.matrix) <- 0
  corrplot(cor.matrix, method='square', type='lower', title='sensor1', tl.pos='l', tl.cex=.8, mar=mar)
  cor.matrix <- cor(df.pc384[,s2])
  diag(cor.matrix) <- 0
  corrplot(cor.matrix, method='square', type='lower', title='sensor2', tl.pos='n', tl.cex=.8, mar=mar)
  cor.matrix <- cor(df.pc384[,s3])
  diag(cor.matrix) <- 0
  corrplot(cor.matrix, method='square', type='lower', title='sensor3', tl.pos='n', tl.cex=.8, mar=mar)
  cor.matrix <- cor(df.pc384[,s4])
  diag(cor.matrix) <- 0
  corrplot(cor.matrix, method='square', type='lower', title='sensor4', tl.pos='l', tl.cex=.8, mar=mar)
  cor.matrix <- cor(df.pc384[,s5])
  diag(cor.matrix) <- 0
  corrplot(cor.matrix, method='square', type='lower', title='sensor5', tl.pos='n', tl.cex=.8, mar=mar)
  cor.matrix <- cor(df.pc384[,s6])
  diag(cor.matrix) <- 0
  corrplot(cor.matrix, method='square', type='lower', title='sensor6', tl.pos='n', tl.cex=.8, mar=mar)
  par(mfrow=c(1,1))
}

# flatten correlation matrix result (for `Hmisc` result)
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

## get person ----
p1 <- data$master.data[1,,]
p2 <- data$master.data[2,,]
p3 <- data$master.data[3,,]
p4 <- data$master.data[4,,]
p5 <- data$master.data[5,,]
p6 <- data$master.data[6,,]
p7 <- data$master.data[7,,]
p8 <- data$master.data[8,,]
p9 <- data$master.data[9,,]
p10 <- data$master.data[10,,]
p11 <- data$master.data[11,,]
p12 <- data$master.data[12,,]
p13 <- data$master.data[13,,]
p14 <- data$master.data[14,,]
p15 <- data$master.data[15,,]
p16 <- data$master.data[16,,]
p17 <- data$master.data[17,,]
p18 <- data$master.data[18,,]


## get n*384 ----------------
p1.384 <- set_p384(p1, write_stage_file = FALSE, write_final_file = TRUE)
p2.384 <- set_p384(p2, write_stage_file = FALSE, write_final_file = TRUE)
p3.384 <- set_p384(p3, write_stage_file = FALSE, write_final_file = TRUE)
p4.384 <- set_p384(p4, write_stage_file = FALSE, write_final_file = TRUE)
p5.384 <- set_p384(p5, write_stage_file = FALSE, write_final_file = TRUE)
p6.384 <- set_p384(p6, write_stage_file = FALSE, write_final_file = TRUE)
p7.384 <- set_p384(p7, write_stage_file = FALSE, write_final_file = TRUE)
p8.384 <- set_p384(p8, write_stage_file = FALSE, write_final_file = TRUE)
p9.384 <- set_p384(p9, write_stage_file = FALSE, write_final_file = TRUE)
p10.384 <- set_p384(p10, write_stage_file = FALSE, write_final_file = TRUE)
p11.384 <- set_p384(p11, write_stage_file = FALSE, write_final_file = TRUE)
p12.384 <- set_p384(p12, write_stage_file = FALSE, write_final_file = TRUE)
p13.384 <- set_p384(p13, write_stage_file = FALSE, write_final_file = TRUE)
p14.384 <- set_p384(p14, write_stage_file = FALSE, write_final_file = TRUE)
p15.384 <- set_p384(p15, write_stage_file = FALSE, write_final_file = TRUE)
p16.384 <- set_p384(p16, write_stage_file = FALSE, write_final_file = TRUE)
p17.384 <- set_p384(p17, write_stage_file = FALSE, write_final_file = TRUE)
p18.384 <- set_p384(p18, write_stage_file = FALSE, write_final_file = TRUE)


## get n*1152 ----
p1.1152 <- p1.384 %>% set_p1152()
p2.1152 <- p2.384 %>% set_p1152()
p3.1152 <- p3.384 %>% set_p1152()
p4.1152 <- p4.384 %>% set_p1152()
p5.1152 <- p5.384 %>% set_p1152()
p6.1152 <- p6.384 %>% set_p1152()
p7.1152 <- p7.384 %>% set_p1152()
p8.1152 <- p8.384 %>% set_p1152()
p9.1152 <- p9.384 %>% set_p1152()
p10.1152 <- p10.384 %>% set_p1152()
p11.1152 <- p11.384 %>% set_p1152()
p12.1152 <- p12.384 %>% set_p1152()
p13.1152 <- p13.384 %>% set_p1152()
p14.1152 <- p14.384 %>% set_p1152()
p15.1152 <- p15.384 %>% set_p1152()
p16.1152 <- p16.384 %>% set_p1152()
p17.1152 <- p17.384 %>% set_p1152()
p18.1152 <- p18.384 %>% set_p1152()


## verify -------
print(paste(dim(p7.sadness.s4),
            dim(p7.anger.s1),
            dim(p7.disgust.s5),
            dim(p7.surprise1.s3),
            dim(p7.count.s2),
            dim(p7.fear.s6)))
print(paste(dim(p7.amuse.384),
            dim(p7.anger.384),
            dim(p7.disgust.384),
            dim(p7.surprise1.384),
            dim(p7.relax.384),
            dim(p7.relax2.384),
            dim(p7.fear.384)))
head(p2.384)
summary(p2.384)
str(p7.384)


## correlation plots -------------
plot_pc384_by_sensor(p1.amuse.384)
plot_pc384_by_sensor(p1.surprise2.384)
plot_pc384_by_sensor(p1.fear.384)
plot_pc384_by_sensor(p3.amuse.384)
plot_pc384_by_sensor(p10.surprise1.384)
plot_pc384_by_sensor(p5.fear.384)



## final dataset ---------
## merge all
p.list <- mget(ls(pattern = glob2rx('p\\d+.1152'), envir = .GlobalEnv), .GlobalEnv)
XY <- do.call('rbind', p.list)
rownames(XY) <- NULL
dim(XY)
write.csv(XY, 'dataset/participant/XY1152.csv')

# ## split train/test sets
# train <- XY %>% dplyr::sample_frac(0.7)
# test <- dplyr::anti_join(XY, train, by='id')
# dim(train)
# dim(test)
# 
# ## split X, Y
# train_X <- train[, -1]
# train_Y <- train[, 1]
# test_X <- test[, -1]
# test_Y <- test[, 1]
# dim(train_X)
# dim(test_X)
# length(train_Y)
# length(test_Y)
# 
# # write to files
# write.csv(train_X, 'dataset/train/train_X.csv')
# write.csv(train_Y, 'dataset/train/train_Y.csv')
# write.csv(test_X, 'dataset/test/test_X.csv')
# write.csv(test_Y, 'dataset/test/test_Y.csv')


# 2. Reload to start -------------

setwd("G:/My Drive/U/2022_WORK_UNISA/WORK_UNISA/Digital Capabilities Program/project_rehabSwift/stroke-sensors")

files384 <- list.files(path = "dataset/participant_384", pattern = "*.csv", full.names = T) 
data384 <- read_csv(files384)
colnames(data384)[1] <- 'index'
data384$id_ <- data384$id 
data384 <- data384 %>% separate(id_, c('class','p','delete'))
data384$delete <- NULL
data384$class <- as.factor(data384$class)
data384$p <- as.factor(data384$p)
data384 <- data384 %>% relocate(index, .after = last_col()) # move index to the end
data384 <- data384 %>% relocate(id, .after = last_col()) # move index to the end
colnames(data384)

# check NA
any(sapply(data384, is.na))
colnames(data384)[colSums(is.na(data384)) >0]
rownames(data384)[rowSums(is.na(data384)) >0]
# only 3 rows with na, drop them
data384 <- na.omit(data384)
any(sapply(data384, is.na))
dim(data384)
summary(data384)

# save to file
write.csv(data384, 'dataset/participant_384/XY384.csv')

cor.matrix <- cor(data384[,1:64])
cor.matrix

unique(data384$class)
unique(data384$p)

# hist
ggplot(gather(data384[, c(1:64)]), aes(value)) +
  geom_boxplot() +
  facet_wrap(~key, scales='free_x')

# all p, s1, boxplots by emotion
p1 <- data384 %>% select(c(1:64, 385, 386)) %>% 
  pivot_longer(c(1:64), names_to = 'emotion', values_to = 'value') %>%
  ggplot(aes(emotion, value)) +
  geom_boxplot() +
  facet_wrap(~class) +
  labs(title='boxplot of all participants, sensor1 by emotion')

# p1, all sensors, boxplots by emotion
p2 <- data384 %>% filter(p=='p1') %>% select(c(1:384, 385, 386)) %>% 
  pivot_longer(c(1:384), names_to = 'emotion', values_to = 'value') %>%
  ggplot(aes(emotion, value)) +
  geom_boxplot() +
  facet_wrap(~class) +
  labs(title='boxplots of p1, all sensors (384 channels) by emotion')

# corr plot, s1
s1 <- 1:64
s2 <- 65:128
s3 <- 129:192
s4 <- 193:256
s5 <- 257:320
s6 <- 321:384

# corrplot, given class, p, by sensor
data384 %>% select(c(1:386)) %>% filter(class=='amuse', p=='p1') %>% plot_pc384_by_sensor()
data384 %>% select(c(1:386)) %>% filter(class=='fear', p=='p1') %>% plot_pc384_by_sensor()
data384 %>% select(c(1:386)) %>% filter(class=='count', p=='p1') %>% plot_pc384_by_sensor()
data384 %>% select(c(1:386)) %>% filter(class=='relax', p=='p1') %>% plot_pc384_by_sensor()
data384 %>% select(c(1:386)) %>% filter(class=='surprise1', p=='p3') %>% plot_pc384_by_sensor()


# corrplot, given p, by class

sp <- split(data384[,1:384], data384$class)
cm <- lapply(sp, function(x) rcorr(as.matrix(x)))
par(mfrow=c(3,4))
lapply(cm, function(x) {
  corrplot(x$r, method='square', type='lower', tl.pos='n', p.mat = x$P)
})

cm.amuse <- data384 %>% select(c(1:385)) %>% filter(class=='amuse') %>% select(c(1:384)) %>% cor()
summary(cm.amuse)
str(cm.amuse)
cm.amuse[,1]




# all p, all sensors, amuse
cm.amuse <- data384 %>% filter(class=='amuse') %>% select(c(1:384)) %>% as.matrix() %>% rcorr()
flat.cor <- flattenCorrMatrix(cm.amuse$r, cm.amuse$P)
flat.cor.pairs <- flat.cor %>% filter(cor>.9) %>% select(c('row', 'column', 'cor'))
# split columns by "."
flat.cor.pairs %<>% separate(col = row, into = c('sensor.l','channel.l'), sep = '\\.', remove = FALSE) %>%
  separate(col = column, into = c('sensor.r', 'channel.r'), sep = '\\.', remove = FALSE)
# change type to factor
flat.cor.pairs[sapply(flat.cor.pairs, is.character)] <- lapply(flat.cor.pairs[sapply(flat.cor.pairs, is.character)], as.factor)
# heatmap
heatmap(cm.amuse$r, Colv = NA, Rowv = NA, main = 'Sensor.Channel correlation map - Amuse')
summary(flat.cor.pairs, maxsum=11)
head(flat.cor.pairs, 20)

# find out 
flat.cor.pairs %>% filter(row =='s5.X11') %>% head(20)


summarise_flattened_corr <- function(classname, threshold) {
  
  if((threshold >= 1) || (threshold <=0)) {
    return(print('expect threshold to be between 0 and 1'))
  }
  
  # all p, all sensors
  cm <- data384 %>% filter(class==classname) %>% select(c(1:384)) %>% as.matrix() %>% rcorr()
  flat.cor <- flattenCorrMatrix(cm$r, cm$P)
  flat.cor.pairs <- flat.cor %>% filter(cor>threshold) %>% select(c('row', 'column', 'cor'))
  # split columns by "."
  flat.cor.pairs %<>% separate(col = row, into = c('sensor.left','channel.left'), sep = '\\.', remove = FALSE) %>%
    separate(col = column, into = c('sensor.right', 'channel.right'), sep = '\\.', remove = FALSE)
  # change type to factor
  flat.cor.pairs[sapply(flat.cor.pairs, is.character)] <- lapply(flat.cor.pairs[sapply(flat.cor.pairs, is.character)], as.factor)
  
  return(c(cm=cm, flatcor=data.frame(flat.cor.pairs)))
}

cor.anger <-summarise_flattened_corr("anger", 0.9)
cm.anger <- cor.anger$cm.r
flat.cor.pairs.anger <- cor.anger[c(4:10)] %>% data.frame()
# summary(flat.cor.pairs.fear, maxsum=11)

cor.disgust <-summarise_flattened_corr('disgust', 0.9)
cm.disgust <- cor.disgust$cm.r
flat.cor.pairs.disgust <- cor.disgust[c(4:10)] %>% data.frame()

cor.fear <-summarise_flattened_corr('fear', 0.9)
cm.fear <- cor.fear$cm.r
flat.cor.pairs.fear <- cor.fear[c(4:10)] %>% data.frame()

cor.sadness <-summarise_flattened_corr('sadness', 0.9)
cm.sadness <- cor.sadness$cm.r
flat.cor.pairs.sadness <- cor.sadness[c(4:10)] %>% data.frame()

cor.surprise1 <-summarise_flattened_corr('surprise1', 0.9)
cm.surprise1 <- cor.surprise1$cm.r
flat.cor.pairs.surprise1 <- cor.surprise1[c(4:10)] %>% data.frame()


cor.surprise2 <-summarise_flattened_corr('surprise2', 0.9)
cm.surprise2 <- cor.surprise2$cm.r
flat.cor.pairs.surprise2 <- cor.surprise2[c(4:10)] %>% data.frame()

cor.relax <-summarise_flattened_corr('relax', 0.9)
cm.relax <- cor.relax$cm.r
flat.cor.pairs.relax <- cor.relax[c(4:10)] %>% data.frame()

cor.relax2 <-summarise_flattened_corr('relax2', 0.9)
cm.relax2 <- cor.relax2$cm.r
flat.cor.pairs.relax2 <- cor.relax2[c(4:10)] %>% data.frame()

cor.count <-summarise_flattened_corr('count', 0.9)
cm.count <- cor.count$cm.r
flat.cor.pairs.count <- cor.count[c(4:10)] %>% data.frame()

cor.focus <-summarise_flattened_corr('focus', 0.9)
cm.focus <- cor.focus$cm.r
flat.cor.pairs.focus <- cor.focus[c(4:10)] %>% data.frame()


print(classes)
# "amuse"     "anger"     "disgust"   "fear"      "sadness"   "surprise1" "surprise2" "relax"    
# "relax2"    "count"     "focus"  

# get results
for(class in classes[1:11]) {
  corr <-summarise_flattened_corr(class, 0.9)
  assign(paste0('cm.', class), corr$cm.r)
  assign(paste0('flat.cor.pairs.', class), corr[c(4:10)] %>% data.frame())
}

## flat corr results -------------
cm.sadness
re1 <-summary(flat.cor.pairs.amuse, maxsum=16) %>% data.frame() %>% .[c(1:10,17:22, 49:58), ]
re2 <-summary(flat.cor.pairs.anger, maxsum=16) %>% data.frame() %>% .[c(1:10,17:22, 49:58), ]
re3 <-summary(flat.cor.pairs.disgust, maxsum=16) %>% data.frame() %>% .[c(1:10,17:22, 49:58), ]
re4 <-summary(flat.cor.pairs.fear, maxsum=16) %>% data.frame() %>% .[c(1:10,17:22, 49:58), ]
re5 <-summary(flat.cor.pairs.sadness, maxsum=16) %>% data.frame() %>% .[c(1:10,17:22, 49:58), ]
re6 <-summary(flat.cor.pairs.surprise1, maxsum=16) %>% data.frame() %>% .[c(1:10,17:22, 49:58), ]
re7 <-summary(flat.cor.pairs.surprise2, maxsum=16) %>% data.frame() %>% .[c(1:10,17:22, 49:58), ]
re8 <-summary(flat.cor.pairs.relax, maxsum=16) %>% data.frame() %>% .[c(1:10,17:22, 49:58), ]
re9 <-summary(flat.cor.pairs.relax2, maxsum=16) %>% data.frame() %>% .[c(1:10,17:22, 49:58), ]
re10 <-summary(flat.cor.pairs.count, maxsum=16) %>% data.frame() %>% .[c(1:10,17:22, 49:58), ]
re11 <-summary(flat.cor.pairs.focus, maxsum=16) %>% data.frame() %>% .[c(1:10,17:22, 49:58), ]
re1 <- re1 %>% separate(col=Freq, into=c('amuse','Cnt'), sep = ':')
re2 <- re2 %>% separate(col=Freq, into=c('anger','Cnt'), sep = ':')
re3 <- re3 %>% separate(col=Freq, into=c('disgust','Cnt'), sep = ':')
re4 <- re4 %>% separate(col=Freq, into=c('fear','Cnt'), sep = ':')
re5 <- re5 %>% separate(col=Freq, into=c('sadness','Cnt'), sep = ':')
re6 <- re6 %>% separate(col=Freq, into=c('surprise1','Cnt'), sep = ':')
re7 <- re7 %>% separate(col=Freq, into=c('surprise2','Cnt'), sep = ':')
re8 <- re8 %>% separate(col=Freq, into=c('relax','Cnt'), sep = ':')
re9 <- re9 %>% separate(col=Freq, into=c('relax2','Cnt'), sep = ':')
re10 <- re10 %>% separate(col=Freq, into=c('count','Cnt'), sep = ':')
re11 <- re11 %>% separate(col=Freq, into=c('focus','Cnt'), sep = ':')
re1[,1:2] <- NULL
re2[,1:2] <- NULL
re3[,1:2] <- NULL
re4[,1:2] <- NULL
re5[,1:2] <- NULL
re6[,1:2] <- NULL
re7[,1:2] <- NULL
re8[,1:2] <- NULL
re9[,1:2] <- NULL
re10[,1:2] <- NULL
re11[,1:2] <- NULL
re <- cbind(re1,re2,re3,re4,re5,re6,re7,re8,re9,re10,re11)
re$Cnt <- as.numeric(re$Cnt)

# write results to file
write.csv(re, 'corr_summary.csv', row.names = F)


re.s <- re[16:21, ] 
re.s <- read.csv('corr_summary2.csv')
ggplot(re.s, aes(x=Emotion, y=High.Correlation.Freq, fill=Sensor)) +
  geom_bar(stat = 'identity', position='stack') +
  scale_fill_grey() + 
  ggtitle('High correlation occurrence by sensor by emotion'
          , subtitle = 'Correlation coefficient > 0.9')

matrix.amuse <- as.matrix(cm.amuse)
matrix.anger <- as.matrix(cm.anger)
matrix.disgust <- as.matrix(cm.disgust)
matrix.fear <- as.matrix(cm.fear)
matrix.sadness <- as.matrix(cm.sadness)
matrix.surprise1 <- as.matrix(cm.surprise1)
matrix.surprise2 <- as.matrix(cm.surprise2)
matrix.relax <- as.matrix(cm.relax)
matrix.relax2 <- as.matrix(cm.relax2)
matrix.count <- as.matrix(cm.count)
matrix.focus <- as.matrix(cm.focus)

matrices <- list(matrix.amuse, matrix.anger, matrix.disgust, matrix.fear, matrix.sadness, matrix.surprise1
                 , matrix.surprise2, matrix.relax, matrix.relax2, matrix.count, matrix.focus)

library(gridGraphics)
library(grid)
grab_grob <- function(){
  grid.echo()
  grid.grab()
}

dev.off()
gl <- lapply(1:11, function(i) {
  if(i<11){
    heatmap.2(matrices[[i]]
              , symm = TRUE
              , Rowv = FALSE
              , Colv = FALSE
              , dendrogram='none'
              , density.info = 'none'
              , trace='none'
              , scale = 'none'
              , main = classes[[i]]
              , key = FALSE
              , margins = c(4,8)
              # , lmat = rbind( c(0, 3, 0), c(2, 1, 4), c(0, 0, 0) )
              , lhei = c(0.5, 1.5) # Alter dimensions of display array cell heights
              , lwid = c(0.3, 2.5) # Alter dimensions of display array cell widths
    )
  } else {
    heatmap.2(matrices[[i]]
              , symm = TRUE
              , Rowv = FALSE
              , Colv = FALSE
              , dendrogram='none'
              , density.info = 'none'
              , trace='none'
              , scale = 'none'
              , main = classes[[i]]
              , key = TRUE
              # , margins = c(5,5)
              # , lmat = rbind( c(0, 3, 0), c(2, 1, 4), c(0, 0, 0) )
              # , lhei = c(0.5, 2, 0.4) # Alter dimensions of display array cell heights
              # , lwid = c(0.2, 4, 0.5) # Alter dimensions of display array cell widths
    )  
  }
  grab_grob()
})

grid.newpage()
# lgd <- legendGrob(c(-1,0,1), pch=21:23, gp=gpar(col = 2:4))
gridExtra::grid.arrange(grobs=gl, ncol=4, clip=TRUE)

