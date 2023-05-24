
one.simeffinf2<-function(){
pop<-data.frame(x=rnorm(10000))
pop$z<-rgamma(10000,2)
pop$id<-1:10000
pop$g<-rep(1:1000,each=10)
pop$j<-rep(1:10, 1000)
pop$strat<-rep(1:10,each=1000)
pop$u<-rnorm(2000)[pop$g]
pop$v<-rnorm(2000)[pop$g]+pop$u
pop$e<-rnorm(10000,s=1.5)
pop$y<-with(pop, 
	x*5+z+e+rnorm(2000,s=.7)[g]+ u*z)


m0<-lme4:::lmer(y~(1+z|g)+z+x,data=pop)


v<-rep(100,10)

names(v)<-1:10
psus<-stratsample(rep(1:10,each=100),v)

stage1<-subset(pop, g %in% psus)
stage1$n1<-rep(100,nrow(stage1))
stage1$n2<-rep(10,nrow(stage1))

umed<-median(abs(pop$u))
stage1$ns<-with(stage1, ifelse(abs(u)<umed, 2, 6))

n<-with(stage1, ns[!duplicated(g)])

in2<-mapply(function(gi,ni) sample(which(stage1$g==gi),ni), unique(stage1$g), n)

stage2<-stage1[unlist(in2),]

des<-svydesign(id=~g+id, strata=~strat, fpc=~n1+n2,data=stage2)

m1<-lme4:::lmer(y~(1+z|g)+z+x,data=stage2)

m2<-svy2lme(y~(1+z|g)+z+x,design=des)

stage2$w1<-2
stage2$w2<-(des$fpc$popsize/des$fpc$sampsize)[,2]
haven::write_dta(stage2,path="/tmp/stage2i.dta")

system("/Applications/Stata/StataSE.app/Contents/MacOS/stata-se < lmesimi-eff.do >/dev/null")

smcl<-grep("^\\{res\\}\\{txt\\} \\{res\\}", readLines("/tmp/stage2i.smcl"),value=TRUE)

list(stata=smcl, pop=c(lme4::fixef(m0),diag(nlme::VarCorr(m0)$g), m0@devcomp$cmp["sigmaML"]^2),
    sample=c(lme4::fixef(m1),diag(nlme::VarCorr(m1)$g),m1@devcomp$cmp["sigmaML"]^2),
    svy=c(coef(m2), diag(coef(m2,random=TRUE)$varb), m2$s2))
}

rvalinf2<-vector("list",1000)


set.seed(2019-1-7)
for(i in 1:1000){
	print(i)
	rvalinf2[[i]]<-one.simeffinf2()
}




