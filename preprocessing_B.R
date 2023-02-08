# by Eric Wang
# 20230130

# setup ------------
if(!require('R.matlab')) install.packages('R.matlab')
if(!require('corrplot')) install.packages('corrplot')
library(R.matlab)
library(tidyr)
library(dplyr)
library(tibble)
library(corrplot)

setwd("G:/My Drive/U/2022_WORK_UNISA/WORK_UNISA/Digital Capabilities Program/project_rehabSwift/stroke-sensors")


# load data ------------
data <- readMat('G:\\My Drive\\U\\2022_WORK_UNISA\\WORK_UNISA\\Digital Capabilities Program\\project_rehabSwift\\stroke-sensors\\data\\master_data.mat')
str(data)

participants <- c('Tulio', 'Sophie', 'Roland', 'Richard', 'Renee', 'Preet', 'Morgan19', 
                  'Michael', 'Maneesh', 'Luke', 'Jason', 'Hugh', 'Helen', 'Flynn',
                  'Connor', 'Ben19', 'Amelie', 'Daniel') %>% tolower()
length(participants)

classes <- c('Amuse', 'Anger', 'Disgust', 'Fear', 'Sadness', 'Surprise1', 'Surprise2', 
             'Relax', 'Relax2', 'count', 'focus') %>% tolower()
length(classes)


# functions ----------
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

plot_pc384_by_sensor <- function(df.pc384){
  par(mfrow=c(2,3))
  mar <- c(1,0,2,0)
  cor.matrix <- cor(df.pc384[,2:65])
  diag(cor.matrix) <- 0
  corrplot(cor.matrix, method = 'square', type='lower', title = 'sensor1', tl.pos = 'l', tl.cex = .8, mar = mar)
  
  cor.matrix <- cor(df.pc384[,66:129])
  diag(cor.matrix) <- 0
  corrplot(cor.matrix, method = 'square', type='lower', title = 'sensor2', tl.pos = 'n', tl.cex = .8, mar = mar)
  
  cor.matrix <- cor(df.pc384[,130:193])
  diag(cor.matrix) <- 0
  corrplot(cor.matrix, method = 'square', type='lower', title = 'sensor3', tl.pos = 'n', tl.cex = .8, mar = mar)
  
  cor.matrix <- cor(df.pc384[,194:257])
  diag(cor.matrix) <- 0
  corrplot(cor.matrix, method = 'square', type='lower', title = 'sensor4', tl.pos = 'l', tl.cex = .8, mar = mar)
  
  cor.matrix <- cor(df.pc384[,258:321])
  diag(cor.matrix) <- 0
  corrplot(cor.matrix, method = 'square', type='lower', title = 'sensor5', tl.pos = 'n', tl.cex = .8, mar = mar)
  
  cor.matrix <- cor(df.pc384[,322:385])
  diag(cor.matrix) <- 0
  corrplot(cor.matrix, method = 'square', type='lower', title = 'sensor6', tl.pos = 'n', tl.cex = .8, mar = mar)
  
  par(mfrow=c(1,1))
}
# plot_pc384_by_sensor(p1.amuse.384)


# get person -----------
## get person
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



# p1 ----------------------
## p1
## get N*64 for each class
i <- 1
for(c in classes) {
  assign(paste0('p1.',c,'.s1'), p1[i,][[1]][[1]] %>% data.frame() %>% set_person_class_sensor('p1',c,'s1') )
  assign(paste0('p1.',c,'.s2'), p1[i,][[2]][[1]] %>% data.frame() %>% set_person_class_sensor('p1',c,'s2') )
  assign(paste0('p1.',c,'.s3'), p1[i,][[3]][[1]] %>% data.frame() %>% set_person_class_sensor('p1',c,'s3') )
  assign(paste0('p1.',c,'.s4'), p1[i,][[4]][[1]] %>% data.frame() %>% set_person_class_sensor('p1',c,'s4') )
  assign(paste0('p1.',c,'.s5'), p1[i,][[5]][[1]] %>% data.frame() %>% set_person_class_sensor('p1',c,'s5') )
  assign(paste0('p1.',c,'.s6'), p1[i,][[6]][[1]] %>% data.frame() %>% set_person_class_sensor('p1',c,'s6') )
  i <- i+1
}

dim(p1.sadness.s4)
dim(p1.anger.s1)
dim(p1.disgust.s1)
dim(p1.surprise1.s1)
dim(p1.count.s2)

## get n*384 for each class
for(c in classes){
  # df_list <- list(p1.amuse.s1, p1.amuse.s2, p1.amuse.s3, p1.amuse.s4, p1.amuse.s5, p1.amuse.s6)
  df_list <- mget(ls(pattern=glob2rx(paste0('p*', c, '.s*'))), .GlobalEnv)
  print(names(df_list))
  assign(paste0('p1.',c,'.384'), Reduce(function(x,y) merge(x,y,all=FALSE), df_list))
}

dim(p1.amuse.384)
dim(p1.disgust.384)
dim(p1.anger.384)
dim(p1.surprise1.384)
dim(p1.relax.384)
dim(p1.relax2.384)
dim(p1.fear.384)

## write to files
for(c in classes){
  file <- mget(ls(pattern = glob2rx(paste0('p1.', c, '.384'))), .GlobalEnv)
  if(length(names(file)) == 1) {
    print(names(file))
    write.csv(file, paste0('dataset/train/participant/p1.', c, '.384.csv'))
  } else {
    print('found multiple filenames, no file saved')
  }
}

## rbind p1 together
p1.list <- mget(ls(pattern = glob2rx('p1.*.384')), .GlobalEnv)
ifelse(length(p1.list) == 11, p1.384 <- do.call('rbind', p1.list), print('incorrect pattern'))
dim(p1.384)
head(p1.384)
summary(p1.384)
str(p1.384)

## write to file
write.csv(p1.384, 'dataset/train/participant/p1.384.csv')


# p2 ----------------------
## p2
## get N*64 for each class
i <- 1
for(c in classes) {
  assign(paste0('p2.',c,'.s1'), p2[i,][[1]][[1]] %>% data.frame() %>% set_person_class_sensor('p2',c,'s1') )
  assign(paste0('p2.',c,'.s2'), p2[i,][[2]][[1]] %>% data.frame() %>% set_person_class_sensor('p2',c,'s2') )
  assign(paste0('p2.',c,'.s3'), p2[i,][[3]][[1]] %>% data.frame() %>% set_person_class_sensor('p2',c,'s3') )
  assign(paste0('p2.',c,'.s4'), p2[i,][[4]][[1]] %>% data.frame() %>% set_person_class_sensor('p2',c,'s4') )
  assign(paste0('p2.',c,'.s5'), p2[i,][[5]][[1]] %>% data.frame() %>% set_person_class_sensor('p2',c,'s5') )
  assign(paste0('p2.',c,'.s6'), p2[i,][[6]][[1]] %>% data.frame() %>% set_person_class_sensor('p2',c,'s6') )
  i <- i+1
}

dim(p2.sadness.s4)
dim(p2.anger.s1)
dim(p2.disgust.s1)
dim(p2.surprise1.s1)
dim(p2.count.s2)

## get n*384 for each class
for(c in classes){
  df_list <- mget(ls(pattern=glob2rx(paste0('p2*', c, '.s*'))), .GlobalEnv)
  if(length(names(df_list)) == 6){
    print(names(df_list))
    assign(paste0('p2.',c,'.384'), Reduce(function(x,y) merge(x,y,all=FALSE), df_list))
  } else {
    print('incorrect pattern')
  }
}

dim(p2.amuse.384)
dim(p2.disgust.384)
dim(p2.anger.384)
dim(p2.surprise1.384)
dim(p2.relax.384)
dim(p2.relax2.384)
dim(p2.fear.384)

## write to files
for(c in classes){
  file <- mget(ls(pattern = glob2rx(paste0('p2.', c, '.384'))), .GlobalEnv)
  if(length(names(file)) == 1) {
    print(names(file))
    write.csv(file, paste0('dataset/train/participant/p2.', c, '.384.csv'))
  } else {
    print('found multiple filenames, no file saved')
  }
}

## rbind p2 together
p2.list <- mget(ls(pattern = glob2rx('p2.*.384')), .GlobalEnv)
ifelse(length(p2.list) == 11, p2.384 <- do.call('rbind', p2.list), print('incorrect pattern'))
dim(p2.384)
dim(p1.384)
head(p2.384)
summary(p2.384)
str(p2.384)

## write to file
write.csv(p2.384, 'dataset/train/participant/p2.384.csv')


# p3 ----------------------
## p3
## get N*64 for each class
i <- 1
for(c in classes) {
  assign(paste0('p3.',c,'.s1'), p3[i,][[1]][[1]] %>% data.frame() %>% set_person_class_sensor('p3',c,'s1') )
  assign(paste0('p3.',c,'.s2'), p3[i,][[2]][[1]] %>% data.frame() %>% set_person_class_sensor('p3',c,'s2') )
  assign(paste0('p3.',c,'.s3'), p3[i,][[3]][[1]] %>% data.frame() %>% set_person_class_sensor('p3',c,'s3') )
  assign(paste0('p3.',c,'.s4'), p3[i,][[4]][[1]] %>% data.frame() %>% set_person_class_sensor('p3',c,'s4') )
  assign(paste0('p3.',c,'.s5'), p3[i,][[5]][[1]] %>% data.frame() %>% set_person_class_sensor('p3',c,'s5') )
  assign(paste0('p3.',c,'.s6'), p3[i,][[6]][[1]] %>% data.frame() %>% set_person_class_sensor('p3',c,'s6') )
  i <- i+1
}

dim(p3.sadness.s4)
dim(p3.anger.s1)
dim(p3.disgust.s5)
dim(p3.surprise1.s3)
dim(p3.count.s2)
dim(p3.fear.s6)

## get n*384 for each class
for(c in classes){
  df_list <- mget(ls(pattern=glob2rx(paste0('p3*', c, '.s*'))), .GlobalEnv)
  if(length(names(df_list)) == 6){
    print(names(df_list))
    assign(paste0('p3.',c,'.384'), Reduce(function(x,y) merge(x,y,all=FALSE), df_list))
  } else {
    print('incorrect pattern')
  }
}

dim(p3.amuse.384)
dim(p3.disgust.384)
dim(p3.anger.384)
dim(p3.surprise1.384)
dim(p3.relax.384)
dim(p3.relax2.384)
dim(p3.fear.384)

## write to files
for(c in classes){
  file <- mget(ls(pattern = glob2rx(paste0('p3.', c, '.384'))), .GlobalEnv)
  if(length(names(file)) == 1) {
    print(names(file))
    write.csv(file, paste0('dataset/train/participant/p3.', c, '.384.csv'))
  } else {
    print('found multiple filenames, no file saved')
  }
}

## rbind p3 together
p.list <- mget(ls(pattern = glob2rx('p3.*.384')), .GlobalEnv)
print(names(p.list))
ifelse(length(p.list) == 11, p3.384 <- do.call('rbind', p.list), print('incorrect pattern'))
dim(p3.384)
dim(p2.384)
dim(p1.384)
head(p3.384)
summary(p3.384)
str(p3.384)

## write to file
write.csv(p3.384, 'dataset/train/participant/p3.384.csv')


## need a function for the rest p4:p18

# p function -----------------
## p384 function
set_p384 <- function(p) {
  
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
      assign(paste0(pn,'.',c,'.384'), Reduce(function(x,y) merge(x,y,all=FALSE), df_list), envir = .GlobalEnv)
    } else {
      print('ERROR: (get n*384) incorrect pattern')
      stop()
    }
  }
  print('INFO: Step 2/5 completed (n*384)')
  
  # write to files
  for(c in classes){
    file <<- list()
    file <<- mget(ls(pattern = glob2rx(paste0(pn,'.', c, '.384')), envir = .GlobalEnv), .GlobalEnv)
    if(length(names(file)) == 1) {
      print(paste('INFO: files to be written:', names(file)))
      write.csv(file, paste0('dataset/train/participant/',pn, '.', c, '.384.csv'))
    } else {
      print('ERROR: (write to files) found multiple filenames, no file saved')
      stop()
    }
  }
  print('INFO: Step 3/5 completed (write p*.class.csv to files)')
  
  # rbind p together
  p_list <<- list()
  p_list <<- mget(ls(pattern = glob2rx(paste0(pn,'.*.384')), envir = .GlobalEnv), .GlobalEnv)
  print(paste('INFO: p_list to be rbind:', names(p_list)))
  ifelse(length(p_list) == 11, p.384 <<- do.call('rbind', p_list), print('ERROR: (rbind) incorrect pattern'))
  print(paste('INFO: current p dim:', dim(p.384)))
  print(paste0('INFO: Step 4/5 completed (rbind to ', pn, '.384)'))
  
  # write to file
  write.csv(p.384, paste0('dataset/train/participant/',pn,'.384.csv'))
  print(paste0('INFO: Step 5/5 completed (write ', pn, '.384.csv to file)'))
  
  return(p.384)
}


# p4:p18 ----------------
p4.384 <- set_p384(p4)
p5.384 <- set_p384(p5)
p6.384 <- set_p384(p6)
p7.384 <- set_p384(p7)
p8.384 <- set_p384(p8)
p9.384 <- set_p384(p9)
p10.384 <- set_p384(p10)
p11.384 <- set_p384(p11)
p12.384 <- set_p384(p12)
p13.384 <- set_p384(p13)
p14.384 <- set_p384(p14)
p15.384 <- set_p384(p15)
p16.384 <- set_p384(p16)
p17.384 <- set_p384(p17)
p18.384 <- set_p384(p18)

## verify 
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

# correlation plots -------------
plot_pc384_by_sensor(p1.amuse.384)
plot_pc384_by_sensor(p1.surprise2.384)
plot_pc384_by_sensor(p1.fear.384)
plot_pc384_by_sensor(p3.amuse.384)
plot_pc384_by_sensor(p10.surprise1.384)
plot_pc384_by_sensor(p5.fear.384)


# final dataset ---------
## merge all
## (need sort by p number?)
p.list <- mget(ls(pattern = glob2rx('p\\d+.384'), envir = .GlobalEnv), .GlobalEnv)
# p.list <- list(p1.384, p2.384, p3.384, p4.384, p5.384, p6.384, p7.384, p8.384, p9.384, 
               # p10.384, p11.384, p12.384, p13.384, p14.384, p15.384, p16.384, p17.384, p18.384)
XY <- do.call('rbind', p.list)
rownames(XY) <- NULL
dim(XY)

## split train/test sets
train <- XY %>% dplyr::sample_frac(0.7)
test <- dplyr::anti_join(XY, train, by='id')
dim(train)
dim(test)

## split X, Y
train_X <- train[, -1]
train_Y <- train[, 1]
test_X <- test[, -1]
test_Y <- test[, 1]
dim(train_X)
dim(test_X)
length(train_Y)
length(test_Y)

# write to files
write.csv(train_X, 'dataset/train/train_X.csv')
write.csv(train_Y, 'dataset/train/train_Y.csv')
write.csv(test_X, 'dataset/test/test_X.csv')
write.csv(test_Y, 'dataset/test/test_Y.csv')
