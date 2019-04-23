library(deldir)
load(reduced_files.RData)

x593988 <- reduced_files[[1]]
x594107 <- reduced_files[[2]]
x597830 <- reduced_files[[3]] 
x602958<- reduced_files[[4]]
x603055<- reduced_files[[5]]
x603976<- reduced_files[[6]]
x619487<- reduced_files[[7]]
x619508<- reduced_files[[8]]
x619932<- reduced_files[[9]]
x625908<- reduced_files[[10]]


vtess_pcr1 <- deldir(x593988$X1, x593988$X2)
vtess_pcr2 <- deldir(x594107$X1, x594107$X2)
vtess_rcb3_1 <- deldir(x597830$X1, x597830$X2)
vtess_pcr3 <- deldir(x602958$X1, x602958$X2)
vtess_rcb3_2 <- deldir(x603055$X1, x603055$X2)
vtess_pcr4 <- deldir(x603976$X1, x603976$X2)
vtess_pcr5 <- deldir(x619487$X1, x619487$X2)
vtess_rcb3_3 <- deldir(x619508$X1, x619508$X2)
vtess_rcb3_4 <- deldir(x619932$X1, x619932$X2)
vtess_rcb3_5 <- deldir(x625908$X1, x625908$X2)


save(vtess_pcr1, file="vtess_pcr1.RData")
save(vtess_pcr2, file="vtess_pcr2.RData")
save(vtess_pcr3, file="vtess_pcr3.RData")
save(vtess_pcr4, file="vtess_pcr4.RData")
save(vtess_pcr5, file="vtess_pcr5.RData")
save(vtess_rcb3_1, file="vtess_rcb3_1.RData")
save(vtess_rcb3_2, file="vtess_rcb3_2.RData")
save(vtess_rcb3_3, file="vtess_rcb3_3.RData")
save(vtess_rcb3_4, file="vtess_rcb3_4.RData")
save(vtess_rcb3_5, file="vtess_rcb3_5.RData")
