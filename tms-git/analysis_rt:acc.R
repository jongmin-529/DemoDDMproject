setwd("/Users/jongminlee/Desktop/Files/nee_lab/DDM/tms3")

data=read.csv("DDM_data_tms.csv")

visit=c(1,2,3,4)
cond=c("Dual", "Delay", "Base", "Switch")

result=array(NA, dim = c(1,4,length(visit)))
for (idx in 1:length(visit)) {
  for (idx2 in 1:length(cond)) {
    result[1,idx2,idx] = mean(data[data$visit==idx & data$cond==cond[idx2] & data$acc==1, ]$rt)
  }
}

colnames(result)=cond
d=data.frame(visit1=result[,,1], visit2=result[,,2], visit3=result[,,3], visit4=result[,,4])
barplot(as.matrix(d), beside=T, ylim = c(0, 1), col=gray.colors(nrow(d)))
legend("topright", cond, fill=gray.colors(nrow(d)))




library(ggplot2)
data_simul=read.csv("/Users/jongminlee/Desktop/Files/nee_lab/DDM/tms3/PPC/m04_va_visit1_simul_ppc-visit1.csv")
data_simul$rt=abs(data_simul$rt_sampled)

data=data[data$visit==1,]
data_plot=data.frame(rt=data$rt, cond=data$cond, ppc="actual")
data_simul_plot=data.frame(rt=data_simul$rt, cond=data_simul$cond, ppc="simulation"); remove(data_simul)
data_ggplot=bind_rows(data_plot, data_simul_plot)

p=ggplot(data_ggplot, aes(x=rt))+
  theme_minimal()
p+geom_density(aes(colour=cond, linetype=ppc), alpha=0.3)+
  facet_wrap(~cond)+
  ggtitle("Baseline")+
  ggsave("Baseline.pdf", height=7, width=7, units="in")


data_simul=read.csv("/Users/jongminlee/Desktop/Files/nee_lab/DDM/tms3/PPC/m04_va_visit2_simul_ppc-visit2.csv")
data_simul$rt=abs(data_simul$rt_sampled)

data=data[data$visit==2,]
data_plot=data.frame(rt=data$rt, cond=data$cond, ppc="actual")
data_simul_plot=data.frame(rt=data_simul$rt, cond=data_simul$cond, ppc="simulation"); remove(data_simul)
data_ggplot=bind_rows(data_plot, data_simul_plot)

p=ggplot(data_ggplot, aes(x=rt))+
  theme_minimal()
p+geom_density(aes(colour=cond, linetype=ppc), alpha=0.3)+
  facet_wrap(~cond)+
  ggtitle("MFG Target")+
  ggsave("MFG.pdf", height=7, width=7, units="in")


data_simul=read.csv("/Users/jongminlee/Desktop/Files/nee_lab/DDM/tms3/PPC/m04_va_visit3_simul_ppc-visit3.csv")
data_simul$rt=abs(data_simul$rt_sampled)

data=data[data$visit==3,]
data_plot=data.frame(rt=data$rt, cond=data$cond, ppc="actual")
data_simul_plot=data.frame(rt=data_simul$rt, cond=data_simul$cond, ppc="simulation"); remove(data_simul)
data_ggplot=bind_rows(data_plot, data_simul_plot)

p=ggplot(data_ggplot, aes(x=rt))+
  theme_minimal()
p+geom_density(aes(colour=cond, linetype=ppc), alpha=0.3)+
  facet_wrap(~cond)+
  ggtitle("FPl Target")+
  ggsave("FPl.pdf", height=7, width=7, units="in")



data_simul=read.csv("/Users/jongminlee/Desktop/Files/nee_lab/DDM/tms3/PPC/m04_va_visit4_simul_ppc-visit4.csv")
data_simul$rt=abs(data_simul$rt_sampled)

data=data[data$visit==4,]
data_plot=data.frame(rt=data$rt, cond=data$cond, ppc="actual")
data_simul_plot=data.frame(rt=data_simul$rt, cond=data_simul$cond, ppc="simulation"); remove(data_simul)
data_ggplot=bind_rows(data_plot, data_simul_plot)

p=ggplot(data_ggplot, aes(x=rt))+
  theme_minimal()
p+geom_density(aes(colour=cond, linetype=ppc), alpha=0.3)+
  facet_wrap(~cond)+
  ggtitle("Control Target")+
  ggsave("Control.pdf", height=7, width=7, units="in")



