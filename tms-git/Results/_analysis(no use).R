setwd("/Users/jongminlee/Desktop/Files/nee_lab/DDM/tms/Results")

data_visit2=read.csv("/Users/jongminlee/Desktop/Files/nee_lab/DDM/tms/Results/m04_va_visit2_traces.csv")
data_visit3=read.csv("/Users/jongminlee/Desktop/Files/nee_lab/DDM/tms/Results/m04_va_visit3_traces.csv")
data_visit4=read.csv("/Users/jongminlee/Desktop/Files/nee_lab/DDM/tms/Results/m04_va_visit4_traces.csv")

df_visit2=data.frame(v_Base=data_visit2$v_Intercept, 
                     v_Delay=data_visit2$v_C.cond..Treatment..Base....T.Delay.,
                     v_Dual=data_visit2$v_C.cond..Treatment..Base....T.Dual.,
                     v_Switch=data_visit2$v_C.cond..Treatment..Base....T.Switch.,
                     a_Base=data_visit2$a_Intercept, 
                     a_Delay=data_visit2$a_C.cond..Treatment..Base....T.Delay.,
                     a_Dual=data_visit2$a_C.cond..Treatment..Base....T.Dual.,
                     a_Switch=data_visit2$a_C.cond..Treatment..Base....T.Switch.)
remove(data_visit2)

df_visit3=data.frame(v_Base=data_visit3$v_Intercept, 
                     v_Delay=data_visit3$v_C.cond..Treatment..Base....T.Delay.,
                     v_Dual=data_visit3$v_C.cond..Treatment..Base....T.Dual.,
                     v_Switch=data_visit3$v_C.cond..Treatment..Base....T.Switch.,
                     a_Base=data_visit3$a_Intercept, 
                     a_Delay=data_visit3$a_C.cond..Treatment..Base....T.Delay.,
                     a_Dual=data_visit3$a_C.cond..Treatment..Base....T.Dual.,
                     a_Switch=data_visit3$a_C.cond..Treatment..Base....T.Switch.)
remove(data_visit3)

df_visit4=data.frame(v_Base=data_visit4$v_Intercept, 
                     v_Delay=data_visit4$v_C.cond..Treatment..Base....T.Delay.,
                     v_Dual=data_visit4$v_C.cond..Treatment..Base....T.Dual.,
                     v_Switch=data_visit4$v_C.cond..Treatment..Base....T.Switch.,
                     a_Base=data_visit4$a_Intercept, 
                     a_Delay=data_visit4$a_C.cond..Treatment..Base....T.Delay.,
                     a_Dual=data_visit4$a_C.cond..Treatment..Base....T.Dual.,
                     a_Switch=data_visit4$a_C.cond..Treatment..Base....T.Switch.)
remove(data_visit4)

estimate_visit2=colMeans(df_visit2)
estimate_visit3=colMeans(df_visit3)
estimate_visit4=colMeans(df_visit4)

p_visit2=colMeans(ifelse(df_visit2 > 0, 1 ,0))
p_visit3=colMeans(ifelse(df_visit3 > 0, 1 ,0))
p_visit4=colMeans(ifelse(df_visit4 > 0, 1 ,0))

p_visit2=ifelse(round(p_visit2)==1, 1-p_visit2, p_visit2)
p_visit3=ifelse(round(p_visit3)==1, 1-p_visit3, p_visit3)
p_visit4=ifelse(round(p_visit4)==1, 1-p_visit4, p_visit4)

df_visit2_parameter_sig=data.frame(visit="visit2", estimate_visit2, p_visit2, sig=ifelse(p_visit2 < .05, "*", "n.s."))
df_visit3_parameter_sig=data.frame(visit="visit3", estimate_visit3, p_visit3, sig=ifelse(p_visit2 < .05, "*", "n.s."))
df_visit4_parameter_sig=data.frame(visit="visit4", estimate_visit4, p_visit4, sig=ifelse(p_visit2 < .05, "*", "n.s."))

base_dealy_p_visit2=ifelse(round(mean(df_visit2$v_Base>df_visit2$v_Delay))==1, 1-mean(df_visit2$v_Base>df_visit2$v_Delay), mean(df_visit2$v_Base>df_visit2$v_Delay))
base_dual_p_visit2=ifelse(round(mean(df_visit2$v_Base>df_visit2$v_Dual))==1, 1-mean(df_visit2$v_Base>df_visit2$v_Dual), mean(df_visit2$v_Base>df_visit2$v_Dual))
base_switch_p_visit2=ifelse(round(mean(df_visit2$v_Base>df_visit2$v_Switch))==1, 1-mean(df_visit2$v_Base>df_visit2$v_Switch), mean(df_visit2$v_Base>df_visit2$v_Switch))
delay_dual_p_visit2=ifelse(round(mean(df_visit2$v_Delay>df_visit2$v_Dual))==1, 1-mean(df_visit2$v_Delay>df_visit2$v_Dual), mean(df_visit2$v_Delay>df_visit2$v_Dual))
delay_swtich_p_visit2=ifelse(round(mean(df_visit2$v_Delay>df_visit2$v_Switch))==1, 1-mean(df_visit2$v_Delay>df_visit2$v_Switch), mean(df_visit2$v_Delay>df_visit2$v_Switch))
dual_switch_p_visit2=ifelse(round(mean(df_visit2$v_Dual>df_visit2$v_Switch))==1, 1-mean(df_visit2$v_Dual>df_visit2$v_Switch), mean(df_visit2$v_Dual>df_visit2$v_Switch))
