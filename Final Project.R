###################################
setwd("D:/data/testdata")

library(tidyverse)
library(randomForest)
library(rfUtilities)
library(rfPermute)
library(ggpp)
library(dplyr)

#########
RF<-read.csv("/Users/jamesm./Desktop/THT_Data_5082.csv")
RF<-na.omit(RF)
names(RF)

RF=RF[,c(20,3:10)]
names(RF)

#   
set.seed(123)
richness_rf <- randomForest(RF$Physical.Activity.from.Transportation..Score ~ ., data= RF,importance=TRUE,proximity=TRUE)
richness_rf

###################
set.seed(123)
richness_perm <- rf.significance(richness_rf, RF, nperm=99, ntree=501)
richness_perm

#
set.seed(123)
richness_rfP<- rfPermute(RF$Physical.Activity.from.Transportation..Score ~ ., data = RF, ntree = 500,
                         na.action = na.omit, nrep = 99,num.cores = 3)

##
richness_dat <- importance(richness_rfP, sort.by = NULL, decreasing = TRUE)

#
richness_dat=as.data.frame(richness_dat)

##########
richness_dat$group2 <- row.names(richness_dat)

unique(richness_dat$group2)
#######################
richness_dat %>%
  as_tibble(rownames = "names") %>%
  data.frame() %>%
  mutate(label = if_else(X.IncMSE.pval < 0.001,"***",
                         if_else(X.IncMSE.pval <0.01,"**",
                                 if_else(X.IncMSE.pval<0.05,"*","  "))),
         X.IncMSE = as.numeric(X.IncMSE)) %>%
  mutate(group = if_else(label=="ns","In_sig","Sig"),
         names = forcats::fct_inorder(names)) %>%
  dplyr::arrange(X.IncMSE)  %>% 
  mutate( group2 = factor(group2, levels = unique(group2),
                          labels=c("Without cover cropping","Grass","Legume"))) %>%  
  ggplot(aes(x = group2, y = X.IncMSE))+
  geom_bar(fill="tan2",stat = "identity",width=0.6,color="black")+
  geom_text(aes(y = X.IncMSE + 1,label = label),size=4)+
  labs(x = "", y = "%IncMSE", title="")+ 
  theme_classic()+ 
  #theme_bw()+
  coord_flip()+
  ####细节
  theme(
    panel.grid = element_blank(),
    panel.background =element_blank(),
    #legend.position = "none",
    legend.title = element_blank(),
    legend.text = element_text(color="black", 
                               size = 10, 
                               family = "serif",
                               face = "bold"),
    axis.title.y = element_text(size = 12, 
                                family = "serif",  
                                color = "black",
                                face = "bold"),
    axis.title.x = element_text(size = 12, 
                                family = "serif",  
                                color = "black",
                                face = "bold"),
    axis.text.x = element_text(size = 10, 
                               family = "serif",  
                               color = "black",
                               face = "bold"),
    axis.text.y = element_text(size = 10, 
                               family = "serif",  
                               color = "black",
                               face = "bold"))


#####
ggsave(filename = 'random forest.jpeg',width = 5,height = 6,dpi=600)
ggsave(filename = 'random forest.pdf',width = 5,height = 6,dpi=600)


###################
my_packages <- c("tidyverse", "broom", "coefplot", "cowplot",
                 "gapminder", "GGally", "ggrepel", "ggridges", "gridExtra",
                 "here", "interplot", "margins", "maps", "mapproj",
                 "mapdata", "MASS", "quantreg", "rlang", "scales",
                 "survey", "srvyr", "viridis", "viridisLite", "devtools","ggplot2",
                 "ggthemes")
for (package in my_packages) {
  if (!require(package,character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

us_states <- map_data("state")
head(us_states)

election<-read.csv("THT_Data_5082.csv")
names(election)
us_states_elec <- left_join(us_states, election,by="region")
head(us_states_elec)


library(plotly)
#
p0 <- ggplot(data = us_states_elec,
             mapping = aes(x = long, y = lat,
                           group = group, fill = us_states_elec$Physical.Activity.from.Transportation..Score))+
  geom_polygon(color = "gray90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme(legend.title = element_blank())+
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(11, "RdBu")) +
  #scale_fill_gradientn(colors = c('#2D6DB1', 'white', '#DC1623'), limit = c(-0.5, 0.5)) +
  labs(title = "Physical Activity from Transportation Score",fill="")+
  theme_bw()
p0

ggplotly(p0)

###
p0 <- ggplot(data = us_states_elec,
             mapping = aes(x = long, y = lat,
                           group = group, fill = Commute.Mode.Share...Auto..Raw.Value))+
  geom_polygon(color = "gray90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme(legend.title = element_blank())+
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(11, "RdBu")) +
  #scale_fill_gradientn(colors = c('#2D6DB1', 'white', '#DC1623'), limit = c(-0.5, 0.5)) +
  labs(title = "Commute Mode.Share Auto Raw Value",fill="")+
  theme_bw()

p0

ggplotly(p0)

####
p0 <- ggplot(data = us_states_elec,
             mapping = aes(x = long, y = lat,
                           group = group, fill = Commute.Mode.Share...Auto..Score))+
  geom_polygon(color = "gray90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme(legend.title = element_blank())+
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(11, "RdBu")) +
  #scale_fill_gradientn(colors = c('#2D6DB1', 'white', '#DC1623'), limit = c(-0.5, 0.5)) +
  labs(title = "Commute Mode Share Auto Score",fill="")+
  theme_bw()
p0

ggplotly(p0)

#
# p <- ggplot(data = us_states,
#             mapping = aes(x = long, y = lat,
#                           group = group))
# 
# p + geom_polygon(fill = "white", color = "black")
# 
# 
#
# p <- ggplot(data = us_states,
#             aes(x = long, y = lat,
#                 group = group, fill = region))
# 
# p + geom_polygon(color = "gray90", size = 0.1) + guides(fill = FALSE)
# 
# 
# #
# p <- ggplot(data = us_states,
#             mapping = aes(x = long, y = lat,
#                           group = group, fill = region))
# 
# p + geom_polygon(color = "gray90", size = 0.1) +
#   coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
#   guides(fill = FALSE)


election<-read.csv("THT_Data_5082.csv")

#election$region <- tolower(election$state)
us_states_elec <- left_join(us_states, election,by="region")

# p <- ggplot(data = us_states_elec,
#             aes(x = long, y = lat,
#                 group = group, fill = us_states_elec$Physical.Activity.from.Transportation..Score))
# 
# p + geom_polygon(color = "gray90", size = 0.1) +
#   coord_map(projection = "albers", lat0 = 39, lat1 = 45)+
#   theme(legend.title = element_blank())


#
# p0 <- ggplot(data = us_states_elec,
#              mapping = aes(x = long, y = lat,
#                            group = group, fill = us_states_elec$Physical.Activity.from.Transportation..Score))
# p1 <- p0 + geom_polygon(color = "gray90", size = 0.1) +
#   coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
#   theme(legend.title = element_blank())
# 
# p2 <- p1 + 
#   scale_fill_gradientn(colours = RColorBrewer::brewer.pal(11, "RdBu")) +
#   #scale_fill_gradientn(colors = c('#2D6DB1', 'white', '#DC1623'), limit = c(-0.5, 0.5)) +
#   labs(title = "", fill = NULL)
# p2 + theme_map()


library(plotly)
#
p0 <- ggplot(data = us_states_elec,
             mapping = aes(x = long, y = lat,
                           group = group, fill = us_states_elec$Physical.Activity.from.Transportation..Score))+
  geom_polygon(color = "gray90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme(legend.title = element_blank())+
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(11, "RdBu")) +
  #scale_fill_gradientn(colors = c('#2D6DB1', 'white', '#DC1623'), limit = c(-0.5, 0.5)) +
  labs(title = "Physical Activity from Transportation Score",fill="")+
  theme_bw()

ggplotly(p0)