library(bnlearn);library(readxl);library(dplyr);library(ggplot2);library(tidyr)#install.packages("BiocManager") BiocManager::install("Rgraphviz")
library(cowplot);theme_set(theme_cowplot())
setwd("C:/Users/walkerro/Desktop/r scripts/terras")
df <- read.csv("terrasmais-2017.csv")
colSums(is.na(df))

hist(df$Legal.stability)
hist(df$Environmental.integrity)
hist(df$Environmental.integrity.in.the.buffer.zone)
hist(df$Territorial.integrity)
hist(df$Absence.of.threats.due.to.infrastructure.projects)
hist(df$Absence.of.pressure.from.infrastructure.projects)
hist(df$Governance)

ggplot(df, aes(x=Legal.stability, y = Environmental.integrity)) +
  geom_point() + geom_smooth(method=lm)

ggplot(df, aes(x=Environmental.integrity, y = Environmental.integrity.in.the.buffer.zone)) +
  geom_point() + geom_smooth(method=lm)

ggplot(df, aes(x=Legal.stability, y = Governance)) +
  geom_jitter() + geom_smooth(method=lm) #beta reg?

#ggsave('scatter.pdf')

df$lat <- as.numeric(df$lat)
df$population <- log(df$population)
df$area <- log(df$area)

ggplot(df, aes(x=population, y = Governance)) +
  geom_jitter() + geom_smooth(method=lm) #beta reg?

ggplot(df, aes(y=area, x = population)) +
  geom_point() + geom_smooth(method=lm) #beta reg?

summary(lm(area~population,df))

library(ggplot2);library(sf);library(rnaturalearth);library(dplyr)
library(rnaturalearthdata);library(ggspatial);library(cowplot);theme_set(theme_cowplot())
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world);library(maps)

library(ggmap)
gm1 <- get_stamenmap(bbox = c(left = -75, bottom = -20, right = -40, top = 8), 
                     zoom = 7, maptype = "terrain-background")

#ggplot() + #data = world) +   
ggmap::ggmap(gm1) +
  #basemap_gglayer(ext) +
  #annotation_map_tile(zoom = 5, type = 'thunderforestoutdoors') +
  #geom_sf(fill="antiquewhite") + #[!is.na(df$rivtype),] 
  #geom_sf(data = white, colour = "red") +
  #geom_sf(data = black, colour = "black") +
  #geom_sf(data = merged[merged$River_Order > 7,], colour = "lightblue") +
  geom_point(data = df, aes(x = long, y = lat, 
                            fill=Environmental.integrity, 
                            size = area) , 
             colour="black",pch=21, alpha = 1) +
  guides(size=guide_legend(title="Area (ha)", title.hjust = 0.5)) +
  #scale_fill_manual(values=c('green','blue', "gray50", "yellow", 'red'), name="Agricultural intensity") + 
  #scale_color_continuous( name="Area (ha)") + 
  #geom_sf(data = st_as_sf(river3), colour = "blue") +
  #geom_tile(data = dfpred, mapping = aes(x = x, y = y, fill=Prediction), alpha = .7, size = 1) +
  scale_fill_viridis_c(option="B", name = "Environmental\nintegrity", direction =1) + #A - E...
  coord_sf(xlim = c(-75, -40), ylim = c(8, -20), expand = FALSE) +
  #annotation_scale(location = "bl", width_hint = 0.4) +
  #annotation_north_arrow(location = "bl", which_north = "true", 
  #                       pad_x = unit(.3, "in"), pad_y = unit(.3, "in"),
  #                       style = north_arrow_fancy_orienteering) +
  #xlab("") + ylab("") +
  #ggtitle("Manioc map", subtitle = "(with phosphorus)") +
  theme(legend.position = c(0.87, 0.7),
        legend.text=element_text(size=6),
        legend.title=element_text(size=7),
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        #panel.background = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", size = 0.5), 
        panel.background = element_rect(fill = "aliceblue"))
#ggsave("map.pdf") #, units = "in", width = 7, height=5)

#look at corr matrix
library(corrplot)
res <- cor(na.omit(df[,c(2:8,11,12)]))
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

#rename nodes
#names(all) <- c("Agricultural intensity", 'Technology', 'Material culture', 'Subsistence', 'Ritual',

#blacklist t1 can't lead to t0
#t0.nodes <- c('area', 'population')
#t1.nodes <- c('Territorial.integrity', 'Absence.of.threats.due.to.infrastructure.projects',
#              'Environmental.integrity', 'Governance', 'Legal.stability', 'Environmental.integrity.in.the.buffer.zone',
#              'Absence.of.pressure.from.infrastructure.projects')
#bl <- set2blacklist(t0.nodes)
#bl <- rbind(bl, tiers2blacklist(list(t0.nodes, t1.nodes)))
#bl

#bootstrap multiple network structures with missing data
#set.seed(1)
#start = random.graph(nodes = names(df[,c(2:8,11,12)]), num = 1001)
#netlist = lapply(start, function(net) {
#  newdf <- df[sample(nrow(df),replace=TRUE),c(2:8,11,12)]
#  structural.em(newdf, maximize = "tabu", maximize.args=list(blacklist=bl), start = net)}) 
#boot = custom.strength(netlist, nodes = names(df[,c(2:8,11,12)]), cpdag = FALSE)

boot <- boot.strength(df[,c(2:8)], R = 10001, cpdag = FALSE,
                      algorithm = "mmhc") #, algorithm.args=list(blacklist=bl))
boot[boot$strength > 0.5 & boot$direction >= 0.5, ]
avg.boot <- averaged.network(boot, threshold = .8)
plot(boot)
avg.boot

#plot strength plot
strength.plot(avg.boot, boot, threshold=.8, #highlight = c('Politics'),
              layout = "dot", #dot, neato, twopi, circo and fdp
              shape = "rectangle") #circle ellipse rectangle

fit = bn.fit(avg.boot, df[,c(2:8)])
fit
print(bn.cv(df[,c(2:8)], bn = avg.boot, k = nrow(df)) )

#saveRDS(boot, file='boot')
boot = readRDS(file='boot')

#compare algorithms
usedf2 <- df[,c(2:8)]
print(bn.cv(usedf2, 'hc', k = nrow(usedf2) ))
print(bn.cv(usedf2, 'tabu', k = nrow(usedf2)) )
print(bn.cv(usedf2, 'mmhc', k = nrow(usedf2)) )
print(bn.cv(usedf2, 'h2pc', k = nrow(usedf2)) )
print(bn.cv(usedf2, 'rsmax2', k = nrow(usedf2)) )
print(bn.cv(usedf2, 'chow.liu', k = nrow(usedf2)) )
print(bn.cv(usedf2, 'aracne', k = nrow(usedf2)) )
print(bn.cv(usedf2, 'pc.stable', k = nrow(usedf2)) )
print(bn.cv(usedf2, 'gs', k = nrow(usedf2)) )
print(bn.cv(usedf2, 'iamb.fdr', k = nrow(usedf2)) )

#run gaussian model
mod1 <- bf(Environmental.integrity  ~ 1 + Legal.stability + Environmental.integrity.in.the.buffer.zone + s(lat, long))
mod2 <- bf(Governance ~ 1 + Legal.stability + s(lat, long) )
mod3 <- bf(Absence.of.threats.due.to.infrastructure.projects ~ 1 + Territorial.integrity + s(lat, long))
mod4 <- bf(Environmental.integrity.in.the.buffer.zone ~ 1 + Absence.of.threats.due.to.infrastructure.projects + s(lat, long))
mod5 <- bf(Absence.of.pressure.from.infrastructure.projects ~ 1 + Absence.of.threats.due.to.infrastructure.projects + s(lat, long))
mv <- mvbf( mod1 + mod2 + mod3 + mod4 + mod5,
            rescor=FALSE )
m1 <- brm(data = df, 
         mv, prior = set_prior("normal(0,.1)", class = "b"), # group = ""),
         iter =  1e4, chains = 4, cores = 4, #save_all_pars = TRUE,
         control = list(adapt_delta = .999, max_treedepth = 20),
         seed = 1, backend = "cmdstanr")
prior_summary(m1)
m1
pl <- plot(m1, N = 4, ask = FALSE) #Trace and Density Plots for MCMC Samples
posterior_summary(m1)
bayes_R2(m1) # 
conditional_effects(m1, points=T)
saveRDS(m1,"m1.Rds")
m1 <- readRDS("m1.Rds") 
#ranef(m1) pp_check(m1)
plot(hypothesis(m1, "Governance_Legal.stability > 0"))



#reverse code variables
df$Legal.stability <- 1- df$Legal.stability
df$Environmental.integrity <- 1- df$Environmental.integrity
df$Territorial.integrity <- ifelse(df$Territorial.integrity == 1, .99, df$Territorial.integrity)
df$Environmental.integrity.in.the.buffer.zone <- 1- df$Environmental.integrity.in.the.buffer.zone
df$Territorial.integrity <- 1- df$Territorial.integrity
df$Absence.of.threats.due.to.infrastructure.projects <- 1-df$Absence.of.threats.due.to.infrastructure.projects
df$Absence.of.pressure.from.infrastructure.projects <- 1- df$Absence.of.pressure.from.infrastructure.projects
df$Governance <- 1- df$Governance
df$Governance <- ifelse(df$Governance == 1, .99, df$Governance)

#run bayesian path model (zero inflated beta model)
library(brms);library(rstan);options(mc.cores = parallel::detectCores());rstan_options(auto_write = TRUE)
mod1 <- bf(Environmental.integrity  ~ 1 + Legal.stability + Environmental.integrity.in.the.buffer.zone + s(lat, long),
           phi ~ 1 + Legal.stability + Environmental.integrity.in.the.buffer.zone+ s(lat, long),
           zi ~ 1 + Legal.stability + Environmental.integrity.in.the.buffer.zone+ s(lat, long)
           #coi ~ 1 + Legal.stability + Environmental.integrity.in.the.buffer.zone+ s(lat, long)
)
mod2 <- bf(Governance ~ 1 + Legal.stability + s(lat, long),
           phi ~ 1 + Legal.stability + s(lat, long),
           zi ~ 1 + Legal.stability + s(lat, long)
           #coi ~ 1 + Legal.stability + s(lat, long) 
)
mod3 <- bf(Absence.of.threats.due.to.infrastructure.projects ~ 1 + Territorial.integrity + s(lat, long),
           phi ~ 1 + Territorial.integrity + s(lat, long),
           zi ~ 1 + Territorial.integrity + s(lat, long)
           #coi ~ 1 + Territorial.integrity + s(lat, long)
)
mod4 <- bf(Environmental.integrity.in.the.buffer.zone ~ 1 + Absence.of.threats.due.to.infrastructure.projects + s(lat, long),
           phi ~ 1 + Absence.of.threats.due.to.infrastructure.projects + s(lat, long),
           zi ~ 1 + Absence.of.threats.due.to.infrastructure.projects + s(lat, long)
           #coi ~ 1 + Absence.of.threats.due.to.infrastructure.projects + s(lat, long)
)
mod5 <- bf(Absence.of.pressure.from.infrastructure.projects ~ 1 + Absence.of.threats.due.to.infrastructure.projects + s(lat, long),
           phi ~ 1 + Absence.of.threats.due.to.infrastructure.projects + s(lat, long),
           zi ~ 1 + Absence.of.threats.due.to.infrastructure.projects + s(lat, long)
           #coi ~ 1 + Absence.of.threats.due.to.infrastructure.projects + s(lat, long)
)
mv <- mvbf( mod1 + mod2 + mod3 + mod4 + mod5,
            rescor=FALSE )
m <- brm(data = df, family = zero_inflated_beta(), #https://mvuorre.github.io/posts/2019-02-18-analyze-analog-scale-ratings-with-zero-one-inflated-beta-models/
         mv, #prior = set_prior("normal(0,.1)", class = "b"), # group = ""),
         iter =  1e4, chains = 4, cores = 4, #save_all_pars = TRUE,
         control = list(adapt_delta = .999, max_treedepth = 20),
         seed = 10, backend = "cmdstanr")
prior_summary(m)
m
pl <- plot(m, N = 4, ask = FALSE) #Trace and Density Plots for MCMC Samples
posterior_summary(m)
bayes_R2(m) # 
conditional_effects(m, points=T)
saveRDS(m,"m.Rds")
m <- readRDS("m.Rds") 
#ranef(m) pp_check(m)
plot(hypothesis(m, "Governance_Legal.stability > 0"))

