setwd("/Users/jongminlee/Desktop/Files/nee_lab/DDM/tms3/PPC")

data=read.csv("m04_va_visit1_simul_ppc-visit1.csv")
data$rt_abs=abs(data$rt_sampled)

visit=c(1,2,3,4)
cond=c("Dual", "Delay", "Base", "Switch")

result=matrix(NA, 1, 4)

for (idx2 in 1:length(cond)) {
    result[1,idx2] = mean(data[data$cond==cond[idx2] & data$acc==1, ]$rt_abs)
}

write.csv(result, "simul_behavior-visit1.csv")







