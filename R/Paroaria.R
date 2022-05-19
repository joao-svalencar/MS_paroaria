library(png)
library(here)
library(ggplot2)
library(patchwork)

# Proportion of vigilants -------------------------------------------------

watch <- read.csv(here("data", "watchers.csv"), header=T)
watch$prop <- cbind(sucess = watch$watchers, fail = watch$flock_size - watch$watchers)

# Generalized Linear Model (GLM) ------------------------------------------

glm <- glm(prop~flock_size, data=watch, family=binomial)
summary(glm)

unique(predict(glm, watch, type="response"))


exp(glm$coefficients)

watch$per <- (watch$watchers/watch$flock_size)

# Figure 1 ----------------------------------------------------------------

a <-ggplot(watch, aes(y=per, x=flock_size))+
    geom_jitter(size=2)+
    labs(y="Proportion of vigilant individuals", x="Flock size",size=10)+
    scale_x_discrete(limits=factor(1:13))+
    theme_classic(base_size=24)
a + geom_smooth(method = "glm", se=FALSE, color="red")

png(here("outputs", "figures", "Figure 1.png"),800,600)
a + geom_smooth(method = "glm", se=FALSE, color="red")
dev.off()
table(watch$flock_size)

# Vigilance time ----------------------------------------------------------

time <- read.csv(here("data", "time.csv"), header=T)
shapiro.test(time$time) #normality test

lm <- lm(time~flock_size, data=time)
summary(lm)

unique(predict(lm, time, type="response"))

# Figure 2 ----------------------------------------------------------------

img <- readPNG(here("outputs", "figures", "Paroaria.png"), native = T)

b <-ggplot(time, aes(y=time, x=flock_size, group=flock_size))+
    geom_boxplot()+
    labs(y="Time watching (s)",x="Flock size",size=10)+
    scale_x_discrete(limits=factor(1:13))+
    annotate("text",x=5,y=0,size=8, label="b=-1.012, SE=0.06, t=-17.02, R²=0.82 p<0.001")+
    geom_dotplot(binaxis='y', stackdir='center', dotsize=0.5)+
    theme_classic(base_size=24)+
    inset_element(img, left = 0.02, bottom = 0.07, right = 0.3, top = 0.5)
b

png(here("outputs", "figures", "Figure 2_new.png"),800,600)
b
dev.off()
