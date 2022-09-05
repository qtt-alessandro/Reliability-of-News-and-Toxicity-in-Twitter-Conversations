library(poweRlaw)
library(igraph)
library(MASS)
options(scipen = 999)

##############################################################################################
#posts comments 

comment_df=data.frame("action"=rep(NA,4),"type"=rep(NA,4),"country"=rep(NA,4),"poisson"=rep(NA,4),"lognorm"=rep(NA,4),"exponential"=rep(NA,4),"powerlaw"=rep(NA,4),"xmin"=rep(NA,4),"alpha"=rep(NA,4))

load("ccdf posts comments/fr_post_comments_count.RData")
load("ccdf posts comments/ge_post_comments_count.RData")
load("ccdf posts comments/it_post_comments_count.RData")
load("ccdf posts comments/sp_post_comments_count.RData")

comment_df$action="comments"
comment_df$type="posts"

comment_df$country[1]="FR"
comment_df$country[2]="DE"
comment_df$country[3]="IT"
comment_df$country[4]="ES"

fr=fit_power_law(fr_pcomments)
ge=fit_power_law(ge_pcomments)
it=fit_power_law(it_pcomments)
sp=fit_power_law(sp_pcomments)

comment_df$powerlaw[1]=fr$logLik
comment_df$xmin[1]=fr$xmin
comment_df$alpha[1]=fr$alpha

comment_df$powerlaw[2]=ge$logLik
comment_df$xmin[2]=ge$xmin
comment_df$alpha[2]=ge$alpha

comment_df$powerlaw[3]=it$logLik
comment_df$xmin[3]=it$xmin
comment_df$alpha[3]=it$alpha

comment_df$powerlaw[4]=sp$logLik
comment_df$xmin[4]=sp$xmin
comment_df$alpha[4]=sp$alpha
rm(fr,ge,it,sp)

fr=fitdistr(x = fr_pcomments, densfun = "exponential")
ge=fitdistr(x = ge_pcomments, densfun = "exponential")
it=fitdistr(x = it_pcomments, densfun = "exponential")
sp=fitdistr(x = sp_pcomments, densfun = "exponential")

comment_df$exponential[1]=fr$loglik
comment_df$exponential[2]=ge$loglik
comment_df$exponential[3]=it$loglik
comment_df$exponential[4]=sp$loglik
rm(fr,ge,it,sp)

fr=fitdistr(x = fr_pcomments, densfun = "Poisson")
ge=fitdistr(x = ge_pcomments, densfun = "Poisson")
it=fitdistr(x = it_pcomments, densfun = "Poisson")
sp=fitdistr(x = sp_pcomments, densfun = "Poisson")

comment_df$poisson[1]=fr$loglik
comment_df$poisson[2]=ge$loglik
comment_df$poisson[3]=it$loglik
comment_df$poisson[4]=sp$loglik
rm(fr,ge,it,sp)

fr_pcomments=fr_pcomments[fr_pcomments>0]
ge_pcomments=ge_pcomments[ge_pcomments>0]
it_pcomments=it_pcomments[it_pcomments>0]
sp_pcomments=sp_pcomments[sp_pcomments>0]

fr=fitdistr(x = fr_pcomments, densfun = "log-normal")
ge=fitdistr(x = ge_pcomments, densfun = "log-normal")
it=fitdistr(x = it_pcomments, densfun = "log-normal")
sp=fitdistr(x = sp_pcomments, densfun = "log-normal")

comment_df$lognorm[1]=fr$loglik
comment_df$lognorm[2]=ge$loglik
comment_df$lognorm[3]=it$loglik
comment_df$lognorm[4]=sp$loglik
rm(fr_pcomments, ge_pcomments, it_pcomments, sp_pcomments)
rm(fr,ge,it,sp)

##############################################################################################
#posts likes 

like_df=data.frame("action"=rep(NA,4),"type"=rep(NA,4),"country"=rep(NA,4),"poisson"=rep(NA,4),"lognorm"=rep(NA,4),"exponential"=rep(NA,4),"powerlaw"=rep(NA,4),"xmin"=rep(NA,4),"alpha"=rep(NA,4))

load("ccdf posts likes/fr_post_likes_count.RData")
load("ccdf posts likes/ge_post_likes_count.RData")
load("ccdf posts likes/it_post_likes_count.RData")
load("ccdf posts likes/sp_post_likes_count.RData")

like_df$action="likes"
like_df$type="posts"

like_df$country[1]="FR"
like_df$country[2]="DE"
like_df$country[3]="IT"
like_df$country[4]="ES"

fr=fit_power_law(fr_plikes)
ge=fit_power_law(ge_plikes)
it=fit_power_law(it_plikes)
sp=fit_power_law(sp_plikes)

like_df$powerlaw[1]=fr$logLik
like_df$xmin[1]=fr$xmin
like_df$alpha[1]=fr$alpha

like_df$powerlaw[2]=ge$logLik
like_df$xmin[2]=ge$xmin
like_df$alpha[2]=ge$alpha

like_df$powerlaw[3]=it$logLik
like_df$xmin[3]=it$xmin
like_df$alpha[3]=it$alpha

like_df$powerlaw[4]=sp$logLik
like_df$xmin[4]=sp$xmin
like_df$alpha[4]=sp$alpha
rm(fr,ge,it,sp)

fr=fitdistr(x = fr_plikes, densfun = "exponential")
ge=fitdistr(x = ge_plikes, densfun = "exponential")
it=fitdistr(x = it_plikes, densfun = "exponential")
sp=fitdistr(x = sp_plikes, densfun = "exponential")

like_df$exponential[1]=fr$loglik
like_df$exponential[2]=ge$loglik
like_df$exponential[3]=it$loglik
like_df$exponential[4]=sp$loglik
rm(fr,ge,it,sp)

fr=fitdistr(x = fr_plikes, densfun = "Poisson")
ge=fitdistr(x = ge_plikes, densfun = "Poisson")
it=fitdistr(x = it_plikes, densfun = "Poisson")
sp=fitdistr(x = sp_plikes, densfun = "Poisson")

like_df$poisson[1]=fr$loglik
like_df$poisson[2]=ge$loglik
like_df$poisson[3]=it$loglik
like_df$poisson[4]=sp$loglik
rm(fr,ge,it,sp)

fr_plikes=fr_plikes[fr_plikes>0]
ge_plikes=ge_plikes[ge_plikes>0]
it_plikes=it_plikes[it_plikes>0]
sp_plikes=sp_plikes[sp_plikes>0]

fr=fitdistr(x = fr_plikes, densfun = "log-normal")
ge=fitdistr(x = ge_plikes, densfun = "log-normal")
it=fitdistr(x = it_plikes, densfun = "log-normal")
sp=fitdistr(x = sp_plikes, densfun = "log-normal")

like_df$lognorm[1]=fr$loglik
like_df$lognorm[2]=ge$loglik
like_df$lognorm[3]=it$loglik
like_df$lognorm[4]=sp$loglik
rm(fr_plikes, ge_plikes, it_plikes, sp_plikes)
rm(fr,ge,it,sp)

##############################################################################################
#posts shares 

share_df=data.frame("action"=rep(NA,4),"type"=rep(NA,4),"country"=rep(NA,4),"poisson"=rep(NA,4),"lognorm"=rep(NA,4),"exponential"=rep(NA,4),"powerlaw"=rep(NA,4),"xmin"=rep(NA,4),"alpha"=rep(NA,4))

load("ccdf posts shares/fr_post_shares_count.RData")
load("ccdf posts shares/ge_post_shares_count.RData")
load("ccdf posts shares/it_post_shares_count.RData")
load("ccdf posts shares/sp_post_shares_count.RData")

share_df$action="shares"
share_df$type="posts"

share_df$country[1]="FR"
share_df$country[2]="DE"
share_df$country[3]="IT"
share_df$country[4]="ES"

fr=fit_power_law(fr_pshares)
ge=fit_power_law(ge_pshares)
it=fit_power_law(it_pshares)
sp=fit_power_law(sp_pshares)

share_df$powerlaw[1]=fr$logLik
share_df$xmin[1]=fr$xmin
share_df$alpha[1]=fr$alpha

share_df$powerlaw[2]=ge$logLik
share_df$xmin[2]=ge$xmin
share_df$alpha[2]=ge$alpha

share_df$powerlaw[3]=it$logLik
share_df$xmin[3]=it$xmin
share_df$alpha[3]=it$alpha

share_df$powerlaw[4]=sp$logLik
share_df$xmin[4]=sp$xmin
share_df$alpha[4]=sp$alpha

fr=fitdistr(x = fr_pshares, densfun = "exponential")
ge=fitdistr(x = ge_pshares, densfun = "exponential")
it=fitdistr(x = it_pshares, densfun = "exponential")
sp=fitdistr(x = sp_pshares, densfun = "exponential")

share_df$exponential[1]=fr$loglik
share_df$exponential[2]=ge$loglik
share_df$exponential[3]=it$loglik
share_df$exponential[4]=sp$loglik
rm(fr,ge,it,sp)

fr=fitdistr(x = fr_pshares, densfun = "Poisson")
ge=fitdistr(x = ge_pshares, densfun = "Poisson")
it=fitdistr(x = it_pshares, densfun = "Poisson")
sp=fitdistr(x = sp_pshares, densfun = "Poisson")

share_df$poisson[1]=fr$loglik
share_df$poisson[2]=ge$loglik
share_df$poisson[3]=it$loglik
share_df$poisson[4]=sp$loglik
rm(fr,ge,it,sp)

fr_pshares=fr_pshares[fr_pshares>0]
ge_pshares=ge_pshares[ge_pshares>0]
it_pshares=it_pshares[it_pshares>0]
sp_pshares=sp_pshares[sp_pshares>0]

fr=fitdistr(x = fr_pshares, densfun = "log-normal")
ge=fitdistr(x = ge_pshares, densfun = "log-normal")
it=fitdistr(x = it_pshares, densfun = "log-normal")
sp=fitdistr(x = sp_pshares, densfun = "log-normal")

share_df$lognorm[1]=fr$loglik
share_df$lognorm[2]=ge$loglik
share_df$lognorm[3]=it$loglik
share_df$lognorm[4]=sp$loglik
rm(fr_pshares, ge_pshares, it_pshares, sp_pshares)
rm(fr,ge,it,sp)

#################################################################################
posts_dist=rbind(comment_df, like_df, share_df)
save(posts_dist, file="posts_fit.RData")
rm(list=ls())
#################################################################################

##############################################################################################
#user comments 

comment_df=data.frame("action"=rep(NA,4),"type"=rep(NA,4),"country"=rep(NA,4),"poisson"=rep(NA,4),"lognorm"=rep(NA,4),"exponential"=rep(NA,4),"powerlaw"=rep(NA,4),"xmin"=rep(NA,4),"alpha"=rep(NA,4))

load("ccdf users comments/fr_user_comment_frequency.RData")
fr_freq=freq
load("ccdf users comments/ge_user_comment_frequency.RData")
ge_freq=freq
load("ccdf users comments/it_user_comment_frequency.RData")
it_freq=freq
load("ccdf users comments/sp_user_comment_frequency.RData")
sp_freq=freq
rm(freq)

comment_df$action="comments"
comment_df$type="users"

comment_df$country[1]="FR"
comment_df$country[2]="DE"
comment_df$country[3]="IT"
comment_df$country[4]="ES"

fr=fit_power_law(fr_freq)
ge=fit_power_law(ge_freq)
it=fit_power_law(it_freq)
sp=fit_power_law(sp_freq)

comment_df$powerlaw[1]=fr$logLik
comment_df$xmin[1]=fr$xmin
comment_df$alpha[1]=fr$alpha

comment_df$powerlaw[2]=ge$logLik
comment_df$xmin[2]=ge$xmin
comment_df$alpha[2]=ge$alpha

comment_df$powerlaw[3]=it$logLik
comment_df$xmin[3]=it$xmin
comment_df$alpha[3]=it$alpha

comment_df$powerlaw[4]=sp$logLik
comment_df$xmin[4]=sp$xmin
comment_df$alpha[4]=sp$alpha

fr=fitdistr(x = fr_freq, densfun = "exponential")
ge=fitdistr(x = ge_freq, densfun = "exponential")
it=fitdistr(x = it_freq, densfun = "exponential")
sp=fitdistr(x = sp_freq, densfun = "exponential")

comment_df$exponential[1]=fr$loglik
comment_df$exponential[2]=ge$loglik
comment_df$exponential[3]=it$loglik
comment_df$exponential[4]=sp$loglik
rm(fr,ge,it,sp)

fr=fitdistr(x = fr_freq, densfun = "Poisson")
ge=fitdistr(x = ge_freq, densfun = "Poisson")
it=fitdistr(x = it_freq, densfun = "Poisson")
sp=fitdistr(x = sp_freq, densfun = "Poisson")

comment_df$poisson[1]=fr$loglik
comment_df$poisson[2]=ge$loglik
comment_df$poisson[3]=it$loglik
comment_df$poisson[4]=sp$loglik
rm(fr,ge,it,sp)

fr_freq=fr_freq[fr_freq>0]
ge_freq=ge_freq[ge_freq>0]
it_freq=it_freq[it_freq>0]
sp_freq=sp_freq[sp_freq>0]

fr=fitdistr(x = fr_freq, densfun = "log-normal")
ge=fitdistr(x = ge_freq, densfun = "log-normal")
it=fitdistr(x = it_freq, densfun = "log-normal")
sp=fitdistr(x = sp_freq, densfun = "log-normal")

comment_df$lognorm[1]=fr$loglik
comment_df$lognorm[2]=ge$loglik
comment_df$lognorm[3]=it$loglik
comment_df$lognorm[4]=sp$loglik
rm(fr_freq, ge_freq, it_freq, sp_freq)
rm(fr,ge,it,sp)

##############################################################################################
#user comments 

like_df=data.frame("action"=rep(NA,4),"type"=rep(NA,4),"country"=rep(NA,4),"poisson"=rep(NA,4),"lognorm"=rep(NA,4),"exponential"=rep(NA,4),"powerlaw"=rep(NA,4),"xmin"=rep(NA,4),"alpha"=rep(NA,4))

load("ccdf users likes/fr_user_like_frequency.RData")
fr_freq=freq
load("ccdf users likes/ge_user_like_frequency.RData")
ge_freq=freq
load("ccdf users likes/it_user_like_frequency.RData")
it_freq=freq
load("ccdf users likes/sp_user_like_frequency.RData")
sp_freq=freq
rm(freq)

like_df$action="likes"
like_df$type="users"

like_df$country[1]="FR"
like_df$country[2]="DE"
like_df$country[3]="IT"
like_df$country[4]="ES"

fr=fit_power_law(fr_freq)
ge=fit_power_law(ge_freq)
it=fit_power_law(it_freq)
sp=fit_power_law(sp_freq)

like_df$powerlaw[1]=fr$logLik
like_df$xmin[1]=fr$xmin
like_df$alpha[1]=fr$alpha

like_df$powerlaw[2]=ge$logLik
like_df$xmin[2]=ge$xmin
like_df$alpha[2]=ge$alpha

like_df$powerlaw[3]=it$logLik
like_df$xmin[3]=it$xmin
like_df$alpha[3]=it$alpha

like_df$powerlaw[4]=sp$logLik
like_df$xmin[4]=sp$xmin
like_df$alpha[4]=sp$alpha

fr=fitdistr(x = fr_freq, densfun = "exponential")
ge=fitdistr(x = ge_freq, densfun = "exponential")
it=fitdistr(x = it_freq, densfun = "exponential")
sp=fitdistr(x = sp_freq, densfun = "exponential")

like_df$exponential[1]=fr$loglik
like_df$exponential[2]=ge$loglik
like_df$exponential[3]=it$loglik
like_df$exponential[4]=sp$loglik
rm(fr,ge,it,sp)

fr=fitdistr(x = fr_freq, densfun = "Poisson")
ge=fitdistr(x = ge_freq, densfun = "Poisson")
it=fitdistr(x = it_freq, densfun = "Poisson")
sp=fitdistr(x = sp_freq, densfun = "Poisson")

like_df$poisson[1]=fr$loglik
like_df$poisson[2]=ge$loglik
like_df$poisson[3]=it$loglik
like_df$poisson[4]=sp$loglik
rm(fr,ge,it,sp)

fr_freq=fr_freq[fr_freq>0]
ge_freq=ge_freq[ge_freq>0]
it_freq=it_freq[it_freq>0]
sp_freq=sp_freq[sp_freq>0]

fr=fitdistr(x = fr_freq, densfun = "log-normal")
ge=fitdistr(x = ge_freq, densfun = "log-normal")
it=fitdistr(x = it_freq, densfun = "log-normal")
sp=fitdistr(x = sp_freq, densfun = "log-normal")

like_df$lognorm[1]=fr$loglik
like_df$lognorm[2]=ge$loglik
like_df$lognorm[3]=it$loglik
like_df$lognorm[4]=sp$loglik
rm(fr_freq, ge_freq, it_freq, sp_freq)
rm(fr,ge,it,sp)

#################################################################################
users_dist=rbind(comment_df, like_df)
save(users_dist, file="users_fit.RData")
rm(list=ls())
#################################################################################

load("posts_fit.RData")
posts_dist$min_min <- apply(posts_dist[,4:7],1,max)
write.table(posts_dist, file="fits.txt")

load("users_fit.RData")
users_dist$min_min <- apply(users_dist[,4:7],1,max)
write.table(users_dist, file="fits.txt", append = T)
