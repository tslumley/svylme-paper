lmefromstata<-function(text){
 values<-as.numeric(strsplit(text,"[[:space:]]+")[[1]])[-(1:2)]
 c(values[c(3,2,1)], exp(values[5]*2), exp(values[4]*2),exp(values[7]*2))
}

lmeconvert<-function(text,n=3){
	if (length(text)==n)
		t(sapply(text,lmefromstata,USE.NAMES=FALSE))
	else matrix(NA,nrow=n,ncol=6)
}


madmed<-function(x) c(med=median(x,na.rm=TRUE),sd=mad(x,na.rm=TRUE))

cvtsum<-function(vals){
rbind(
apply(t(sapply(vals, function(i) i$sample-i$pop )), 2, madmed),
apply(t(sapply(vals, function(i) i$svy-i$pop )),2, madmed),
apply(t(sapply(vals, function(i) lmeconvert(i$stata)[1,]-i$pop )),2, madmed),
apply(t(sapply(vals, function(i) lmeconvert(i$stata)[2,]-i$pop )),2, madmed),
apply(t(sapply(vals, function(i) lmeconvert(i$stata)[3,]-i$pop )),2, madmed)
)
}


lmefromstata2<-function(text){
 values<-as.numeric(strsplit(text,"[[:space:]]+")[[1]])[-(1:2)]
 c(values[c(3,2,1)], exp(values[5]*2), exp(values[4]*2),exp(values[6]*2))
}

lmeconvert2<-function(text,n=3){
	if (length(text)==n)
		t(sapply(text,lmefromstata2,USE.NAMES=FALSE))
	else matrix(NA,nrow=n,ncol=6)
}


cvtsum2<-function(vals){
rbind(
apply(t(sapply(vals, function(i) i$sample-i$pop )), 2, madmed),
apply(t(sapply(vals, function(i) i$svy-i$pop )),2, madmed),
apply(t(sapply(vals, function(i) lmeconvert2(i$stata)[1,]-i$pop )),2, madmed),
apply(t(sapply(vals, function(i) lmeconvert2(i$stata)[2,]-i$pop )),2, madmed),
apply(t(sapply(vals, function(i) lmeconvert2(i$stata)[3,]-i$pop )),2, madmed)
)
}