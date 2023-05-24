if (!require(devtools)) {
    install.packages("devtools")
    require(devtools)
}
install_github("pbiecek/PISA2012lite")

library(PISA2012lite)



mathstudent<-student2012[, c("CNT","SUBNATIO","STRATUM","OECD","NC","SCHOOLID","STIDSTD","ST04Q01", "ST14Q02" ,"ST18Q02", "MATHEFF","OPENPS","PV1MATH","PV2MATH","PV3MATH","PV4MATH","PV5MATH","W_FSTUWT")]

mathschool<-school2012[, c("SUBNATIO","SCHOOLID", "SC35Q02", "PCGIRLS","PROPMA5A","ABGMATH","SMRATIO","W_FSCHWT")]

mathdata<-subset(merge(mathstudent,mathschool,by=c("SUBNATIO","SCHOOLID")),CNT %in% c("Australia","New Zealand","Canada","United Kingdom"))


library(svylme)
library(lme4)

mathdata$schlid<-with(mathdata, interaction(CNT, SCHOOLID,drop=TRUE))
mathdata$condwt<-with(mathdata, W_FSTUWT/W_FSCHWT)

mathdata<-subset(mathdata, !(STRATUM %in% c("CAN0435","CAN0543","CAN1091","CAN0876","GBR1112","CAN0655","CAN0875","GBR1229","GBR1317", "GBR1318", "GBR2006", "GBR2007","NZL0102")))
mathdata<-na.omit(mathdata[,c("ST04Q01","schlid","STIDSTD","STRATUM","W_FSCHWT","condwt","W_FSTUWT",
"PV1MATH","PV2MATH","PV3MATH","PV4MATH","PV5MATH","ST04Q01","PCGIRLS","SMRATIO","MATHEFF","OPENPS")])
mathdata$STRATUM<-as.factor(as.character(mathdata$STRATUM))


des<-svydesign(id=~schlid+STIDSTD, strata=~STRATUM, nest=TRUE, weights=~W_FSCHWT+condwt, data=mathdata)

m0<-svyglm(PV1MATH~ ST04Q01*(PCGIRLS+SMRATIO)+MATHEFF+OPENPS, design=des)

m1<-svy2lme(PV1MATH~ (1+ ST04Q01 |schlid)+ST04Q01*(PCGIRLS+SMRATIO)+MATHEFF+OPENPS, design=des)


l1<-lmer(PV1MATH~ (1+ ST04Q01 |schlid)+ST04Q01*(PCGIRLS+SMRATIO)+MATHEFF+OPENPS, data=mathdata)

l1sim<-simulate(l1)

mathdata$Y<-l1sim[[1]]
des<-svydesign(id=~schlid+STIDSTD, strata=~STRATUM, nest=TRUE, weights=~W_FSCHWT+condwt, data=mathdata)

l2<-lmer(Y~ (1+ ST04Q01 |schlid)+ST04Q01*(PCGIRLS+SMRATIO)+MATHEFF+OPENPS, data=mathdata)
m2<-svy2lme(Y~ (1+ ST04Q01 |schlid)+ST04Q01*(PCGIRLS+SMRATIO)+MATHEFF+OPENPS, design=des)




