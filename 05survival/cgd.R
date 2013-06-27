library(survival)
data(cgd)


ph1 = coxph(Surv(tstart, tstop, status) ~ treat + steroids + cluster(id), data=cgd)