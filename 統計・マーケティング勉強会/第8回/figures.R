x1 = rnorm(10000)
x2 = x1*0.8+rnorm(10000,0,0.6)
g = ggplot(data.frame(x1=x1,x2=x2),aes(x1,x2))+
  geom_point()+
  lims(x=c(-5,5),y=c(-5,5))+
  ggtitle("Model1 (e1,e2 ~ Gaussian)")
ggsave("C:/Users/mirai/Dropbox/materials/統計・マーケティング勉強会/第8回/model1_gauss.png",g)

x2 = rnorm(10000)
x1 = x2*0.8+rnorm(10000,0,0.6)
g = ggplot(data.frame(x1=x1,x2=x2),aes(x1,x2))+
  geom_point()+
  lims(x=c(-5,5),y=c(-5,5))+
  ggtitle("Model2 (e1,e2 ~ Gaussian)")
ggsave("C:/Users/mirai/Dropbox/materials/統計・マーケティング勉強会/第8回/model2_gauss.png",g)

x1 = runif(10000,-1,1)
x2 = x1*0.8+runif(10000,-1,1)
g = ggplot(data.frame(x1=x1,x2=x2),aes(x1,x2))+
  geom_point()+
  lims(x=c(-5,5),y=c(-5,5))+
  ggtitle("Model1 (e1,e2 ~ Uniform)")
ggsave("C:/Users/mirai/Dropbox/materials/統計・マーケティング勉強会/第8回/model1_unif.png",g)

x2 = runif(10000,-1,1)
x1 = x2*0.8+runif(10000,-1,1)
g = ggplot(data.frame(x1=x1,x2=x2),aes(x1,x2))+
  geom_point()+
  lims(x=c(-5,5),y=c(-5,5))+
  ggtitle("Model2 (e1,e2 ~ Uniform)")
ggsave("C:/Users/mirai/Dropbox/materials/統計・マーケティング勉強会/第8回/model2_unif.png",g)

