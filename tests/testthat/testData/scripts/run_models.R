library(NMdata)
library(NMsim)
NMdataConf(path.nonmem = "/opt/NONMEM/nm75/run/nmfe75") ## path to NONMEM executable

NMexec("../nonmem/xgxr053.mod",sge=F)
NMexec("../nonmem/xgxr057.mod",sge=F)
