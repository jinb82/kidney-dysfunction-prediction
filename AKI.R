# load library
library(tidyverse)
library(pROC)
library(ResourceSelection)

# load data
df=read.csv('SNUH_AKI.csv', row.names = NULL)

# C_HTN
log_df=df %>% mutate(C_HTN=as.factor(ifelse((B_SBP>=140|B_DBP>=90|B_HTN==1), 1, 0))) %>% 
  select(A_AGE, A_SEX, L_HB, L_ALB, L_NA, L_UALB, B_SBP, B_DBP, B_MAP, 
         F_DM, B_HTN, M_ARB, M_NSAID, M_FUROSEMIDE, BMI, URIC_ACID, egfr_base,
         O_AKI, egfr_60_3m_6m, egfr_60_1year, egfr_50_1year, egfr_50_3m_6m, C_HTN)

log_df$A_SEX=as.factor(log_df$A_SEX)
log_df$F_DM=as.factor(log_df$F_DM)
log_df$B_HTN=as.factor(log_df$B_HTN)
log_df$M_ARB=as.factor(log_df$M_ARB)
log_df$M_NSAID=as.factor(log_df$M_NSAID)
log_df$M_FUROSEMIDE=as.factor(log_df$M_FUROSEMIDE)

summary(log_df)

# function
calSens=function(x, y){
  newx=sort(c(unique(x), max(x, na.rm=T)+1))
  completeTable=function(res){
    if(nrow(res)==1){
      res1=matrix(c(0, 0), nrow=1)
      temp=setdiff(c("TRUE", "FALSE"), attr(res, "dimnames")[[1]][1])
      if(temp=="FALSE") res=rbind(res1, res)
      else res=rbind(res, res1)
      res
    }
    res
  }
  getSens=function(cut){
    res=table(x>=cut, y)
    res=completeTable(res)
    sens=res[2, 2]/sum(res[, 2])
    spec=res[1, 1]/sum(res[, 1])
    ppv=res[2, 2]/sum(res[2, ])
    npv=res[1, 1]/sum(res[1, ])
    data.frame(x=cut, sens=sens, spec=spec, fpr=1-spec, ppv=ppv, npv=npv, sum=sens+spec)
  }
  map_dfr(newx, function(cut){getSens(cut)})
}

# check coefficients
fit=glm(egfr_50_3m_6m~A_AGE+A_SEX+BMI+URIC_ACID+egfr_base, data=df, family="binomial")

cat(paste(as.numeric(paste(coef(summary(fit))[, 1])), collapse = "\n"))
cat(paste(as.numeric(paste(coef(summary(fit))[, 4])), collapse = "\n"))
cat(paste(as.numeric(paste(round(exp(coef(fit)), 3))), collapse = "\n"))
cat(paste(as.numeric(paste(round(confint(fit), 3)[, 1])), collapse = "\n"))
cat(paste(as.numeric(paste(round(confint(fit), 3)[, 2])), collapse = "\n"))

# uni
result=lapply(var_name, function(var){
  formula=as.formula(paste("egfr_50_3m_6m ~ ", var))
  uni=glm(formula, data=log_df, family="binomial")
  # coef(summary(uni))[8]
  # round(exp(coef(uni)), 3)[2]
  round(confint(uni), 3)[4]
})
cat(paste(as.numeric(paste(unlist(result))), collapse = "\n"))

# multi
multi=glm(egfr_60_3m_6m~A_AGE+A_SEX+L_HB+L_ALB+L_NA+L_UALB+B_MAP+ 
            F_DM+M_ARB+M_NSAID+M_FUROSEMIDE+BMI+C_HTN+URIC_ACID+egfr_base, 
          data=log_df, family = "binomial")
summary(multi)

cat(paste(as.numeric(paste(coef(summary(multi))[, 1])), collapse = "\n"))
cat(paste(as.numeric(paste(coef(summary(multi))[, 4])), collapse = "\n"))
cat(paste(round(exp(coef(multi)), 3), collapse = "\n"))
cat(paste(as.numeric(paste(round(confint(multi), 3)[, 1])), collapse = "\n"))
cat(paste(as.numeric(paste(round(confint(multi), 3)[, 2])), collapse = "\n"))

# new aki vs old aki
new_df=df %>% mutate(N_AKI=as.factor(ifelse((CR_7D_ABS>=0.5|CR_7D_REL>=1.5), 1, 0)),
                     B_DM=as.factor(B_DM)) %>% 
  select(A_AGE, A_SEX, L_HB, L_ALB, L_NA, L_UALB, B_SBP, B_DBP, B_MAP, 
         F_DM, B_HTN, M_ARB, M_NSAID, M_FUROSEMIDE, BMI, URIC_ACID, egfr_base,
         O_AKI, egfr_60_3m_6m, egfr_60_1year, egfr_50_1year, egfr_50_3m_6m, N_AKI, B_DM)
m1=glm(egfr_60_1year~A_AGE+A_SEX+BMI+URIC_ACID+egfr_base, family="binomial", data=new_df)
m2=glm(egfr_60_1year~A_AGE+A_SEX+BMI+URIC_ACID+egfr_base+O_AKI, family="binomial", data=new_df)
pred1=predict(m1, new_df, type="response")
pred2=predict(m2, new_df, type="response")
roc1=roc(egfr_60_1year~pred1, new_df)
roc2=roc(egfr_60_1year~pred2, new_df)

par(pty="s")
plot.roc(roc1, col="red", print.auc = T, print.auc.adj = c(-.5, 0), main="egfr_60_1year")
plot.roc(roc2, col="blue", print.auc = T, print.auc.adj = c(-.5, 2), add=T)
legend("bottomright", legend=c("Without AKI", "With AKI"), col=c("red", "blue"), lwd=2, cex=.7)

roc.test(roc1, roc2)

# cutoff plot
rel_df=calSens(df$CR_7D_REL, df$egfr_60_1year)
abs_df=calSens(df$CR_7D_ABS, df$egfr_60_1year)

par(mfrow=c(1, 2))
plot(rel_df$x, rel_df$sens, col="red", xlab="CR_REL", ylab="Sensitivity", xlim=c(1.118, 2.795),
     type="l")
lines(rel_df$x, rel_df$spec, col="blue", type="l")
abline(v=1.580, lty="dashed")
mtext("Specificity", side=4)
mtext("1.580", at=1.580)

plot(abs_df$x, abs_df$sens, col="red", xlab="CR_ABS", ylab="Sensitivity", xlim=c(0.09, 1.31),
     type="l")
lines(abs_df$x, abs_df$spec, col="blue", type="l")
abline(v=0.500, lty="dashed")
mtext("Specificity", side=4)
mtext("0.500", at=0.500)

mtext("egfr_60_1year", cex=1.5, outer=T, side=3, line=-2)

# hoslem test
fit=glm(egfr_60_3m_6m~A_AGE+A_SEX+BMI+URIC_ACID+egfr_base, data=df, family="binomial")
hoslem.test(fit$y, fitted(fit))
