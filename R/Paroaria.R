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

fig1 <-ggplot(watch, aes(y=per, x=flock_size))+
    geom_jitter(size=.6)+
    labs(y="Proportion of vigilant individuals", x="Flock size")+
    scale_x_discrete(limits=factor(1:13))+
    theme_classic(base_size=8, base_family = "Arial")
fig1 + geom_smooth(method = "glm", se=FALSE, color="red")

ggsave("Fig 1.png",
       device = png,
       plot = fig1 + geom_smooth(method = "glm", se=FALSE, color="red"),
       path = here::here("outputs", "figures"),
       width = 89,
       height = 80,
       units = "mm",
       dpi = 300
)

table(watch$flock_size)

# Vigilance time ----------------------------------------------------------

time <- read.csv(here("data", "time.csv"), header=T)
shapiro.test(time$time) #normality test

lm <- lm(time~flock_size, data=time)
summary(lm)

unique(predict(lm, time, type="response"))

# Figure 2 ----------------------------------------------------------------

img <- readPNG(here("outputs", "figures", "Paroaria.png"), native = T)

fig2 <-ggplot(time, aes(y=time, x=flock_size, group=flock_size))+
    geom_boxplot()+
    labs(y="Time watching (s)",x="Flock size")+
    scale_x_discrete(limits=factor(1:13))+
    annotate("text", x = 5, y = 0, size = 2,
             label="b = -1.012, SE = 0.06, t = -17.02, R² = 0.82 p < 0.001")+
    geom_dotplot(binaxis='y', stackdir='center', dotsize=0.5)+
    theme_classic(base_size=8, base_family = "Arial")+
    inset_element(img, left = 0.01, bottom = 0.06, right = 0.4, top = 0.55)
fig2

ggsave("Fig 2.png",
       device = png,
       plot = fig2,
       path = here::here("outputs", "figures"),
       width = 89,
       height = 80,
       units = "mm",
       dpi = 300
)



