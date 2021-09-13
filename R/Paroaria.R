library(ggpubr)
library(cowplot)
library(magick)
library(png)
library(grid)
library(patchwork)
library(here)

col_a<-read.csv(here("data", "col_a.csv"), header=T)
col_a
#Tamanho
col_a[1,]
prop.table(col_a)

col_a[,1]
col_a[1,]
data.frame(col_a)

#plot multi
col_b<-read.csv(here("data", "col_b.csv"), header=T)
col_b

###Plot
par(bty="l")
boxplot(col_b$vig~col_b$tb, #to see the relationship between 
     xlab="flock size",
     ylab = "watchers number")
    
lm_wnumber <- lm(col_b$vig~col_b$tb)
summary(lm_wnumber)
abline(lm_wnumber )
text(12,6,"R²=0.4625 p<0.001")

#ponderado
coletivo<-
    ggplot(col_b, aes(y=pond, x=tb,group=tb))+
    geom_boxplot()+
    geom_smooth(method=lm,se=F, color="black",aes(group=1))+
    labs(y="Relative number of watchers ",x="Flock size",size=10)+
    geom_dotplot(binaxis='y', stackdir='center', dotsize=0.5)+
    scale_x_discrete(limits=factor(1:13))+
    annotate("text",x=11,y=0,size=8, label="b=-0.023, SE=0.008, t=-2.74, r²=0.09 p=0.008")+
    theme_classic(base_size=24)

png(here("outputs", "figures", "coletivo"),1200,800)
coletivo
dev.off()

lm_wnumber_pond <- lm(col_b$pond~col_b$tb)
abline(lm_wnumber_pond)
summary(lm_wnumber_pond)


#Individual
ind <- read.table(here("data", "ind_b.csv"), h=T)

desenho <- readPNG(here("outputs", "figures", "Paroaria.png"), native = T)

individual<-ggplot(ind, aes(y=tx, x=tb, group=tb))+
 geom_boxplot()+
    geom_smooth(method=lm,se=F, color="black",aes(group=1))+
labs(y="Time watching (seg)",x="Flock size",size=10)+
    scale_x_discrete(limits=factor(1:13))+
geom_dotplot(binaxis='y', stackdir='center', dotsize=0.5)+
    annotate("text",x=4,y=1,size=8, label="b=-0.915, SE=0.04, t=-22.95, r²=0.8193 p<0.001")+
    theme_classic(base_size=24)+
    inset_element(desenho, left = 0.02, bottom = 0.1, right = 0.3, top = 0.6)
individual

png(here("outputs", "figures", "individual.png"), 1200,800)
individual
dev.off()

#####################3
boxplot(ind$tx~ind$tb,
        xlab="flock size",
        ylab = "Time watching (seg)"
        )
text(4,5,"b=-0.915, SE=0.04, t=-22.95, r²=0.8193 p<0.001")
lm_ind<- lm(ind$tx~ind$tb)
summary(lm_ind)
abline(lm_ind)