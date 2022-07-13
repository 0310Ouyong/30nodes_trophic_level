rm(list=ls())
getwd()
setwd('C:/data')
load('EcoBuilder30nodes_matrix.RData')
library(dplyr)
library(ggplot2)
library(ggpubr)
library(ggpmisc)
library(gridExtra)
library(cowplot)
library(stringr)

a <- c()
for (i in 1:33){
  a <- c(a,EcoBuilder_matrix[[1]][i,])
}

matrix_list <- list()
for (i in 1:112){
  a <- c()
  for (j in 1:dim(EcoBuilder_matrix[[i]])[1]){
    a <- c(a,EcoBuilder_matrix[[i]][j,])
  }
  matrix_list[[i]] <- a
}

data <- data.frame()
for(i in 1:112){
  data[i,1] <- str_c(matrix_list[[i]],collapse=" ")
}

write.table(data,'30trophic_level.txt',row.names=FALSE,col.names=FALSE)


list_key <- read.delim('30list_key.txt',header = F)
trophic_level <- list_key[1,1]

library(stringr)
data <- strsplit(trophic_level,',')
EcoBuilder_trophic_level <- c()
a <- 0
for(i in 1:112){
  a <- a+sum(dim(EcoBuilder_matrix[[i]])[1])
}
for(i in 1:4512){
  EcoBuilder_trophic_level[i] <- as.numeric(data[[1]][i])
}
EcoBuilder_trophic_level[1] <- 1
write.table(EcoBuilder_trophic_level,'30EcoBuilder_total_trophiclevel.txt',row.names=FALSE,col.names=FALSE)
load('EcoBuilder30nodes_matrix.RData')

EcoBuilder_length <- c()
for (i in 1:112){
  EcoBuilder_length[i] <- dim(EcoBuilder_matrix[[i]])[1]
}

number <- c()
for(i in 1:112){
  number[1] <- 0
  number[i+1] <- sum(EcoBuilder_length[1:i])
}    



EcoBuilder_trophic_list <- list()
for(i in 1:112){
  EcoBuilder_trophic_list[[i]] <- EcoBuilder_trophic_level[(number[i]+1):(number[i+1])]
}
save(EcoBuilder_trophic_list,file = '30EcoBuilder_trophic_list.RData')

library(ggplot2)
EcoBuilder_trophic_list[[1]]

EcoBuilder <- as.data.frame(EcoBuilder_trophic_list[[1]])
length <- rep(1,dim(EcoBuilder)[[1]])
EcoBuilder$group <- length
names(EcoBuilder) <- c('x','group')
for(i in 2:112){
  data <- as.data.frame(EcoBuilder_trophic_list[[i]])
  length <- rep(i,dim(data)[[1]])
  data$group <- length
  names(data) <- c('x','group')
  EcoBuilder <- rbind(EcoBuilder,data)
}

#plot
for(i in 1:112){
  EcoBuilder2 =subset(EcoBuilder,group==i)
  assign(paste0('p',i),
         ggplot(data=EcoBuilder2,aes(x=x))+
           geom_histogram(aes(y=..density..),
                          color='#e9ecef',fill='#69b3a2',
                          alpha=0.9,
                          binwidth = 1,
                          center=0)+
           geom_density(lwd=1,color='#558ebd')+
           labs(x='trophic level',y='density'))
}


pdf('30EcoBuilder_individual_trophic1.pdf',width=8,height = 10)
plot_grid(p1, p2, p3, p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15,p16,p17,p18,p19,p20,ncol = 4, nrow = 5)
dev.off()

pdf('30EcoBuilder_individual_trophic2.pdf',width=8,height = 10)
plot_grid(p21,p22,p23,p24,p25,p26,p27,p28,p29,p30,p31,p32,p33,p34,p35,p36,p37,p38,p39,p40,ncol = 4, nrow = 5)
dev.off()

pdf('30EcoBuilder_individual_trophic3.pdf',width=8,height = 10)
plot_grid(p41,p42,p43,p44,p45,p46,p47,p48,p49,p50,p51,p52,p53,p54,p55,p56,p57,p58,p59,p60,ncol = 4, nrow = 5)
dev.off()

pdf('30EcoBuilder_individual_trophic4.pdf',width=8,height = 10)
plot_grid(p61,p62,p63,p64,p65,p66,p67,p68,p69,p70,p71,p72,p73,p74,p75,p76,p77,p78,p79,p80,ncol = 4, nrow = 5)
dev.off()

pdf('30EcoBuilder_individual_trophic5.pdf',width=8,height = 10)
plot_grid(p81,p82,p83,p84,p85,p86,p87,p88,p89,p90,p91,p92,p93,p94,p95,p96,p97,p98,p99,p100,ncol = 4, nrow = 5)
dev.off()


pdf('30EcoBuilder_individual_trophic6.pdf',width=8,height = 6)
plot_grid(p101, p102, p103, p104,p105,p106,p107,p108,p109,p110,p111,p112,ncol = 4, nrow = 3)
dev.off()
