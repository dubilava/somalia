demean <- function(x){
  x-mean(x,na.rm=T)
}

my_pstr <- function(d,y=y,x=x,w=NULL,tv,g_start=2,c_start=.5,tf=1,max_it=100){
  
  d[,y_t:=demean(y),by=Dyad]
  d[,x_t:=demean(x),by=Dyad]

  if(!is.null(w)){
    d[,paste0(w,"_t") := lapply(.SD,demean),.SDcols=w,by=Dyad]
  }

  tv1 <- as.matrix(d[,..tv])
  
  ghat1 <- g_start
  chat1 <- quantile(tv1,c_start)
  if(tf==2){
    chat1 <- quantile(tv1,c(c_start-.2,c_start+.2))
  }
  sig1 <- sd(tv1)
  
  itnum = 0
  
  repeat{
    
    itnum = itnum+1
    
    tf1 <- as.numeric((1+exp(-(ghat1/(sig1))*(tv1-chat1)))^(-1))
    if(tf==2){
      tf1 <- as.numeric((1+exp(-(ghat1/(sig1^2))*(tv1-chat1[1])*(tv1-chat1[2])))^(-1))
    }
    
    # x0 <- d$x_t
    
    d[,xg:=(x*tf1)]
    d[,xg_t:=demean(xg),by=Dyad]
    d[,xg_bar:=mean(xg),by=Dyad]
    
    if(!is.null(w)){
      fmla <- as.formula(paste("y_t~x_t+xg_t+",paste0(w,collapse="+"),"-1",sep=""))
      bhat <- round(lm(fmla,d)$coef,4)
    }else{
      fmla <- as.formula(paste("y_t~x_t+xg_t-1",sep=""))
      bhat <- round(lm(y_t~x_t+xg_t-1,d)$coef,4)
    }
    
    b <- as.numeric(c(bhat,ghat1,chat1))

    func1 <- paste0("((1+exp(-g1*(",tv,"-c1)/",sig1,"))^(-1))")
    if(tf==2){
      func1 <- paste0("((1+exp(-g1*(",tv,"-c11)*(",tv,"-c12)/",sig1^2,"))^(-1))")
    }
    
    options(warn=-1)
    
    if(!is.null(w)){
      if(tf==1){
        names(b) <- c(sprintf("a%d",1),sprintf("b%d",1),sprintf("d%d",1:length(w)),"g1","c1")
        fmla <- as.formula(paste0("y_t~",b["a1"],"*x_t+",b["b1"],"*(x_t*",func1,"-xg_bar)+",paste0(paste(b[3:(length(b)-2)],paste0(w,"_t"),sep="*"),collapse="+")))
        stcoef <- coef(nls(fmla,data=d,start=b[-c(1:(length(b)-2))],control=nls.control(maxiter=1,warnOnly=T),lower=c(.1,quantile(tv1,.1)),upper=c(1000,quantile(tv1,.9)),algorithm = "port"))
      }else{
        names(b) <- c(sprintf("a%d",1),sprintf("b%d",1),sprintf("d%d",1:length(w)),"g1","c11","c12")
        fmla <- as.formula(paste0("y_t~",b["a1"],"*x_t+",b["b1"],"*(x_t*",func1,"-xg_bar)+",paste0(paste(b[3:(length(b)-2)],paste0(w,"_t"),sep="*"),collapse="+")))
        stcoef <- coef(nls(fmla,data=d,start=b[-c(1:(length(b)-3))],control=nls.control(maxiter=1,warnOnly=T),lower=c(.1,quantile(tv1,c(.1,.4))),upper=c(1000,quantile(tv1,c(.6,.9))),algorithm = "port"))
      }
    }else{
      if(tf==1){
        names(b) <- c(sprintf("a%d",1),sprintf("b%d",1),"g1","c1")
        fmla <- as.formula(paste0("y_t~",b["a1"],"*x_t+",b["b1"],"*(x_t*",func1,"-xg_bar)"))
        stcoef <- coef(nls(fmla,data=d,start=b[-c(1:(length(b)-2))],control=nls.control(maxiter=1,warnOnly=T),lower=c(.1,quantile(tv1,.1)),upper=c(1000,quantile(tv1,.9)),algorithm = "port"))
      }else{
        names(b) <- c(sprintf("a%d",1),sprintf("b%d",1),"g1","c11","c12")
        fmla <- as.formula(paste0("y_t~",b["a1"],"*x_t+",b["b1"],"*(x_t*",func1,"-xg_bar)"))
        stcoef <- coef(nls(fmla,data=d,start=b[-c(1:(length(b)-3))],control=nls.control(maxiter=1,warnOnly=T),lower=c(.1,quantile(tv1,c(.1,.4))),upper=c(1000,quantile(tv1,c(.6,.9))),algorithm = "port"))
      }
    }
    
    if(tf==1){
      if(crossprod(c(ghat1-stcoef["g1"],chat1-stcoef["c1"]))<1e-6){
        break
      }
      ghat1=stcoef["g1"]
      chat1=stcoef["c1"]
    }else{
      if(crossprod(c(ghat1-stcoef["g1"],chat1[1]-stcoef["c11"],chat1[2]-stcoef["c12"]))<1e-6){
        break
      }
      ghat1=stcoef["g1"]
      chat1=stcoef[c("c11","c12")]
    }

    if(ghat1==1000){
      break
    }
    
    if(itnum==max_it){
      break
    }
    
  }
  
  return(stcoef)
  
}