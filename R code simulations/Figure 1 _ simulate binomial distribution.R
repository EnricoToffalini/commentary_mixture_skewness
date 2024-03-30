
############################################

rm(list=ls())
library(ggplot2)
library(moments)
generateSumScores = function(zscores=NA,nitems=48,items_M=1,items_SD=1){
  item_difficulty = rnorm(n=nitems,mean=items_M,sd=items_SD)*(-1)
  item_difficulty = t(matrix(rep(item_difficulty,length(zscores)),ncol=length(zscores)))
  probit_scores = item_difficulty + zscores
  probs = pnorm(probit_scores)
  scores = apply(probs,1:2,function(prob)rbinom(1,1,prob))
  return(rowSums(scores))
}

############################################

set.seed(10)

N = 5e4
true_zscores = rnorm(N,0,1)
ss = generateSumScores(true_zscores)
median(ss)
mean(ss)
sd(ss)
skewness(ss)

df = data.frame(score=c(true_zscores,ss),type=rep(c("`True` z-score (latent)","Sum score (observed)"),each=N))
vline = data.frame(type=c("`True` z-score (latent)","Sum score (observed)","Sum score (observed)"),xint=c(NA,-0.3,48.3))

bws = rep(c("`True` z-score (latent)"=.2,"Sum score (observed)"=.1),each=N)
(gg = ggplot(df)+
  geom_histogram(data=df[df$type=="`True` z-score (latent)",],aes(x=score,y=after_stat(density)),binwidth=.15,fill="blue",alpha=.7)+
  geom_histogram(data=df[df$type=="Sum score (observed)",],aes(x=score,y=after_stat(density)),binwidth=.50,fill="blue",alpha=.7)+
  facet_wrap(.~type,scales="free")+
  geom_vline(data=vline,aes(xintercept=xint),size=1,linetype=2)+
  theme(text=element_text(size=24),axis.text.y=element_text(size=14))
)
pdf("z-vs-sumscore.pdf",width=10,height=4)
gg
dev.off()

############################################


