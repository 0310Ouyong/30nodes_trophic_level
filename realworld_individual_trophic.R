rm(list=ls())
getwd()
setwd('C:/data/Hsi')
load('realworld.matrix.RData')
library(dplyr)
library(ggplot2)
library(ggpubr)
library(ggpmisc)
library(gridExtra)
library(cowplot)
library(stringr)

matrix_list <- list()
for (i in 1:74){
  a <- c()
  for (j in 1:dim(realworld.matrix[[i]])[1]){
    a <- c(a,realworld.matrix[[i]][j,])
  }
  matrix_list[[i]] <- a
}

data <- data.frame()
for(i in 1:74){
  data[i,1] <- str_c(matrix_list[[i]],collapse=" ")
}

write.table(data,'trophic_level.txt',row.names=FALSE,col.names=FALSE)

dim(realworld.matrix[[14]])


matrix_list <- list()
for (i in c(1:13,15:74)){
  a <- c()
  for (j in 1:dim(realworld.matrix[[i]])[1]){
    a <- c(a,realworld.matrix[[i]][j,])
  }
  matrix_list[[i]] <- a
}

data <- data.frame()
for(i in 1:74){
  data[i,1] <- str_c(matrix_list[[i]],collapse=" ")
}


write.table(data,'trophic_level2.txt',row.names=FALSE,col.names=FALSE)

list_key <- read.delim('list_key.txt',header = F)
trophic_level <- list_key[1,1]
library(stringr)
data <- strsplit(trophic_level,',')
realworld_trophic_level <- c()

for(i in 1:4093){
  realworld_trophic_level[i] <- as.numeric(data[[1]][i])
}
write.table(realworld_trophic_level,'realworld_total_trophiclevel.txt',row.names=FALSE,col.names=FALSE)

realworld_length <- c()
for(i in 1:74){
  realworld_length[i] <- dim(realworld.matrix[[i]])[1]
}
realworld_length <- c(realworld_length[1:13],realworld_length[15:74])
number <- c()
for(i in 1:73){
  number[1] <- 0
  number[i+1] <- sum(realworld_length[1:i])
}

realworld_trophic_list <- list()
for(i in 1:73){
  realworld_trophic_list[[i]] <- realworld_trophic_level[(number[i]+1):(number[i+1])]
}
save(realworld_trophic_list,file = 'realworld_trophic_list.RData')




realworld <- as.data.frame(realworld_trophic_list[[1]])
length <- rep(1,dim(realworld)[[1]])
realworld$group <- length
names(realworld) <- c('x','group')
for(i in 2:73){
  data <- as.data.frame(realworld_trophic_list[[i]])
  length <- rep(i,dim(data)[[1]])
  data$group <- length
  names(data) <- c('x','group')
  realworld <- rbind(realworld,data)
}

#plot
for(i in 1:73){
  realworld2 =subset(realworld,group==i)
  assign(paste0('p',i),
         ggplot(data=realworld2,aes(x=x))+
           geom_histogram(aes(y=..density..),
                          color='#e9ecef',fill='#69b3a2',
                          alpha=0.9,
                          binwidth = 1,
                          center=0)+
           geom_density(lwd=1,color='#558ebd')+
           labs(x='trophic level',y='density'))
}


pdf('realworld_individual_trophic1.pdf',width=8,height = 10)
plot_grid(p1, p2, p3, p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15,p16,p17,p18,p19,p20,ncol = 4, nrow = 5)
dev.off()

pdf('realworld_individual_trophic2.pdf',width=8,height = 10)
plot_grid(p21,p22,p23,p24,p25,p26,p27,p28,p29,p30,p31,p32,p33,p34,p35,p36,p37,p38,p39,p40,ncol = 4, nrow = 5)
dev.off()

pdf('realworld_individual_trophic3.pdf',width=8,height = 10)
plot_grid(p41,p42,p43,p44,p45,p46,p47,p48,p49,p50,p51,p52,p53,p54,p55,p56,p57,p58,p59,p60,ncol = 4, nrow = 5)
dev.off()

pdf('realworld_individual_trophic4.pdf',width=8,height = 8)
plot_grid(p61,p62,p63,p64,p65,p66,p67,p68,p69,p70,p71,p72,p73,ncol = 4, nrow = 4)
dev.off()