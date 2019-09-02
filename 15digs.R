sufor=function(x,chunks=NULL) {
  options(digits=15)
  if (x==0) {
    comma=0
    out="0"
    if (!is.null(chunks)) {
      if (length(out)>chunks) {
        stop("chunk size too big")
      } else if (length(out)<chunks) {
        out=c(out,rep("0",chunks-length(out)))
      }
    }
    out=c(comma,out)
    return(out)
  }
  
  y=ifelse(x<0,-x,x)
  firstdig=floor(log10(y))
  if (firstdig>100) {
    stop("number too large to format")
  } else if (-firstdig>90) {
    stop("number too small to format")
  }
  yform=unlist(strsplit(format(y,scientific = FALSE),NULL))
  if (firstdig<0) {
    yform=yform[-c(1:(1-firstdig))]
  }
  
  if (firstdig<0|all(yform!=".")) {
    if (length(yform)>15) {
      yform=yform[1:16]
      carryover=ifelse(as.numeric(yform[16])>=5,TRUE,FALSE)
      yform=yform[1:15]
      tmpdig=15
      while (carryover&&tmpdig>1) {
        yform[tmpdig]=as.numeric(yform[tmpdig])+1
        carryover=ifelse(as.numeric(yform[tmpdig])>9,TRUE,FALSE)
        if (carryover) {
          yform=yform[-tmpdig]
        }
        tmpdig=tmpdig-1
      }
      if (carryover&&tmpdig==1) {
        yform="1"
        firstdig=firstdig+1
      }
    }
    if (firstdig<0) {
      comma=0
      yform=c(rep("0",-firstdig-1),yform)
      if (length(yform)%%15!=0) {
        yform=c(yform,rep("0",15-length(yform)%%15))
      }
    } else if (firstdig>15) {
      comma=firstdig
      yform=c(yform,rep("0",(firstdig%/%15+1)*15-length(yform)))
    } else {
      comma=15
    }
  } else {
    comma=15+which(yform==".")-length(yform)
    yform=yform[-which(yform==".")]
  }
  
  if (length(yform)>15) {
    out=matrix(yform,ncol=15,byrow = TRUE)
    out=apply(out,1,function(a){if (all(a=="0")) {return("0")} else {return(paste0(a[cumsum(as.numeric(a))>0],collapse = ""))}})
  } else {
    out=paste(yform,collapse = "")
  }
  if (x<0) {
    out=paste0("-",out)
  }
  if (!is.null(chunks)) {
    if (length(out)>chunks) {
      stop("chunk size too big")
    } else if (length(out)<chunks) {
      out=c(out,rep("0",chunks-length(out)))
    }
  }
  out=c(comma,out)
  options(digits=7)
  return(out)
}

suali=function(x,newcom,chunks=NULL) {
  options(digits=15)
  if (all(x=="0")) {
    comma=0
    out="0"
    if (!is.null(chunks)) {
      if (length(out)>chunks) {
        stop("chunk size too big")
      } else if (length(out)<chunks) {
        out=c(out,rep("0",chunks-length(out)))
      }
    }
    out=c(comma,out)
    return(out)
  }
  
  difference=as.numeric(x[1])-newcom
  if (difference==0) {
    return(x)
  }
  y=x[-1]
  negative=any(sapply(y,function(z){unlist(strsplit(z,NULL))[1]=="-"}))
  num=lapply(y,function(a){unlist(strsplit(a,NULL))})
  if (negative) {
    num=lapply(num,function(a){a[which(a!="-")]})
  }
  num=unlist(lapply(num,function(a){c(rep("0",15-length(a)),a)}))
  
  if (difference<0) {
    trailzeroes=which(rev(num)!="0")[1]-1
    if (-difference>trailzeroes) {
      num=c(num,rep(rep("0",15),(-difference-trailzeroes)%/%15+1))
    }
    num=c(rep("0",-difference),num[1:(length(num)+difference)])
  } else {
    leadzeroes=which(num!="0")[1]-1
    if (leadzeroes<difference) {
      stop("Not enough leading zeroes, cannot shift up.")
    }
    num=c(num[(difference+1):length(num)],rep("0",difference))
  }
  
  out=matrix(num,ncol=15,byrow = TRUE)
  out=apply(out,1,function(a){if (all(a=="0")) {return("0")} else {return(paste0(a[cumsum(as.numeric(a))>0],collapse = ""))}})
  if (negative) {
    out[out!="0"]=paste0("-",out[out!="0"])
  }
  if (!is.null(chunks)) {
    if (length(out)>chunks) {
      stop("Not enough chunks, number too large")
    } else if (length(out)<chunks) {
      out=c(out,rep("0",chunks-length(out)))
    }
  }
  options(digits=7)
  return(c(as.character(newcom),out))
}

suadd=function(x,chunks=NULL,maxdigs=40,sual=suali) {
  options(digits=15)
  if (any(apply(x,2,function(a){all(a=="0")}))) {
    if (all(apply(x,2,function(a){all(a=="0")}))) {
      comma=0
      out="0"
      if (!is.null(chunks)) {
        if (length(out)>chunks) {
          stop("chunk size too big")
        } else if (length(out)<chunks) {
          out=c(out,rep("0",chunks-length(out)))
        }
      }
      out=c(comma,out)
      return(out)
    } else {
      y=x[,-which(apply(x,2,function(a){all(a=="0")})),drop=FALSE]
    }
  } else {
    y=x
  }
  
  if (any(y[1,]!=y[1,1])) {
    y2=apply(y,2,sual,newcom=max(as.numeric(y[1,])))
    if (is.list(y2)) {
      y=apply(y,2,sual,newcom=max(as.numeric(y[1,])),chunks=max(lengths(y))-1)
    } else {
      y=y2
    }
  }
  
  comma=as.numeric(y[1,1])
  y=y[-1,,drop=FALSE]
  y=apply(y,2,strsplit,split=NULL)
  negative=rep(FALSE,length(y))
  negative[which(sapply(y,function(a){any(unlist(a)=="-")}))]=TRUE
  if (any(negative)) {
    y[negative]=lapply(y[negative],function(a){lapply(a,function(b){b[which(b!="-")]})})
  }
  y=lapply(y,function(a){lapply(a,function(b){if (length(b)<15) {return(c(rep("0",15-length(b)),b))} else {return(b)}})})
  y=t(sapply(y,unlist))
  y[negative,]=paste0("-",y[negative,])
  y=apply(y,c(1,2),as.numeric)
  sums=colSums(y)
  leadingzeroes=FALSE
  if (!all(abs(sums)<10)) {
    while (!all(abs(sums)<10)) {
      tens=sapply(as.character(sums),strsplit,split=NULL)
      ones=sapply(tens,function(a){a[length(a)]})
      tens[lengths(tens)<2]=0
      tens[lengths(tens)>1]=lapply(tens[lengths(tens)>1],function(a){paste(a[-length(a)],collapse="")})
      tens=unlist(tens)
      if (tens[1]!="0") {
        leadingzeroes=TRUE
        tens=c(rep("0",15),tens)
        ones=c(rep("0",15),ones)
        comma=comma+15
      }
      ones=as.numeric(ones)
      tensneg=grepl("-",tens,fixed = TRUE)
      ones[tensneg]=-ones[tensneg]
      tens=c(tens[-1],"0")
      tens[tens=="-"]="0"
      sums=ones+as.numeric(tens)
    }
  }
  
  if (!all(sums>=0)&!all(sums<=0)) {
    numsign=sign(sums[which(sums!="0")[1]])
    while (any(sign(sums)!=numsign&sums!="0")) {
      signdif=which(sign(sums)!=numsign&sums!="0")
      sums[signdif]=10*numsign+sums[signdif]
      sums[signdif-1]=sums[signdif-1]-numsign
    }
  }
  if (leadingzeroes&all(sums[1:15]=="0")) {
    sums=sums[16:length(sums)]
    comma=comma-15
  }
  
  firstdig=which(cumsum(sums)!=0)[1]
  lastdig=length(sums)-which(cumsum(rev(sums))!=0)[1]
  if (lastdig-firstdig>maxdigs) {
    if (sums[firstdig+maxdigs]>=5) {
      if (sums[firstdig+maxdigs-1]==9) {
        if (all(sums[1:(firstdig+maxdigs-1)]==9)) {
          sums=c(1,sums-sums)
        } else {
          firstnonnine=which(rev(sums[1:(firstdig+maxdigs-1)])!=9)[1]
          sums[firstdig+maxdigs-firstnonnine]=sums[firstdig+maxdigs-firstnonnine]+1
          sums[(firstdig+maxdigs-firstnonnine+1):length(sums)]=0
        }
      } else {
        sums[firstdig+maxdigs-1]=sums[firstdig+maxdigs-1]+1
        sums[(firstdig+maxdigs+1):length(sums)]=0
      }
    }
  }
  lastdig=length(sums)-which(cumsum(rev(sums))!=0)[1]
  if (length(sums)-max(comma,lastdig+1)>=15) {
    for (i in 1:floor((length(sums)-max(comma,lastdig+1))/15)) {
      sums=sums[-c((length(sums)-14):length(sums))]
    }
  }
  sums=as.character(sums)
  
  negative=FALSE
  if (any(sums!="0")&&as.numeric(sums)[which(sums!="0")[1]]<0) {
    negative=TRUE
    sums[as.numeric(sums)<0]=as.character(-as.numeric(sums[as.numeric(sums)<0]))
  }
  out=matrix(sums,ncol=15,byrow = TRUE)
  out=apply(out,1,function(a){if (all(a=="0")) {return("0")} else {return(paste0(a[cumsum(as.numeric(a))>0],collapse = ""))}})
  if (negative) {
    out[out!="0"]=paste0("-",out[out!="0"])
  }
  if (out[length(out)]=="0"&&(comma-1)%/%15+1<length(out)) {
    out=out[-which(rev(cumsum(abs(as.numeric(rev(out))))==0)&1:length(out)>(comma-1)%/%15+1)]
  }
  if (!is.null(chunks)) {
    if (length(out)>chunks) {
      stop("Not enough chunks, number too large")
    } else if (length(out)<chunks) {
      out=c(out,rep("0",chunks-length(out)))
    }
  }
  options(digits=7)
  return(c(as.character(comma),out))
}

sumult=function(x1,x2,maxdigs=40,chunks=NULL) {
  options(digits=15)
  if (all(x1=="0")||all(x2=="0")) {
    comma=0
    out="0"
    if (!is.null(chunks)) {
      if (length(out)>chunks) {
        stop("chunk size too big")
      } else if (length(out)<chunks) {
        out=c(out,rep("0",chunks-length(out)))
      }
    }
    out=c(comma,out)
    return(out)
  }
  
  com1=15*(length(x1)-1)-as.numeric(x1[1])
  com2=15*(length(x2)-1)-as.numeric(x2[1])
  y1=x1[-1]
  y2=x2[-1]
  totcom=com1+com2
  negative=xor(any(gsub("-","",y1)!=y1),any(gsub("-","",y2)!=y2))
  if (any(gsub("-","",y1)!=y1)) {
    y1=gsub("-","",y1)
  }
  if (any(gsub("-","",y2)!=y2)) {
    y2=gsub("-","",y2)
  }
  y1=unlist(lapply(y1,function(a){if(a=="0") {return(rep("0",15))} else {tmp=unlist(strsplit(a,split=NULL));return(c(rep("0",15-length(tmp)),tmp))}}))
  y2=unlist(lapply(y2,function(a){if(a=="0") {return(rep("0",15))} else {tmp=unlist(strsplit(a,split=NULL));return(c(rep("0",15-length(tmp)),tmp))}}))
  y1=as.numeric(y1)
  y2=as.numeric(y2)
  
  square=rev(y1)%*%t(y2)
  sums=sapply((-nrow(square)+1):(ncol(square)-1),function(a){sum(square[col(square)==(row(square)+a)])})
  
  if (!all(sums<10)) {
    while (!all(sums<10)) {
      tens=sapply(as.character(sums),strsplit,split=NULL)
      ones=sapply(tens,function(a){a[length(a)]})
      tens[lengths(tens)<2]=0
      tens[lengths(tens)>1]=lapply(tens[lengths(tens)>1],function(a){paste(a[-length(a)],collapse = "")})
      tens=unlist(tens)
      if (tens[1]!="0") {
        tens=c("0",tens)
        ones=c("0",ones)
      }
      tens=c(tens[-1],"0")
      sums=as.numeric(ones)+as.numeric(tens)
    }
  }
  
  firstdig=which(cumsum(sums)!=0)[1]
  lastdig=length(sums)-which(cumsum(rev(sums))!=0)[1]+1
  if (lastdig-firstdig>maxdigs) {
    if (sums[firstdig+maxdigs]>=5) {
      if (sums[firstdig+maxdigs-1]==9) {
        if (all(sums[1:(firstdig+maxdigs-1)]==9)) {
          sums=c(1,sums-sums)
        } else {
          firstnonnine=which(rev(sums[1:(firstdig+maxdigs-1)])!=9)[1]
          sums[firstdig+maxdigs-firstnonnine]=sums[firstdig+maxdigs-firstnonnine]+1
          sums[(firstdig+maxdigs-firstnonnine+1):length(sums)]=0
        }
      } else {
        sums[firstdig+maxdigs-1]=sums[firstdig+maxdigs-1]+1
        sums[(firstdig+maxdigs+1):length(sums)]=0
      }
    }
    totcom=totcom-length(sums[(firstdig+maxdigs):length(sums)])
    sums=sums[1:max(firstdig+maxdigs-1,totcom)]
  }
  sums=as.character(sums)
  
  if (length(sums)%%15!=0) {
    sums=c(rep("0",15-length(sums)%%15),sums)
  }
  comma=length(sums)-totcom
  
  out=matrix(sums,ncol=15,byrow = TRUE)
  out=apply(out,1,function(a){if (all(a=="0")) {return("0")} else {return(paste0(a[cumsum(as.numeric(a))>0],collapse = ""))}})
  while (out[1]==0&length(out)>ceiling(totcom/15)) {
    out=out[-1]
    comma=comma-15
  }
  if (negative) {
    out[out!="0"]=paste0("-",out[out!="0"])
  }
  if (out[length(out)]=="0"&&(comma-1)%/%15+1<length(out)) {
    out=out[-which(rev(cumsum(abs(as.numeric(rev(out))))==0)&1:length(out)>(comma-1)%/%15+1)]
  }
  if (!is.null(chunks)) {
    if (length(out)>chunks) {
      stop("Not enough chunks, number too large")
    } else if (length(out)<chunks) {
      out=c(out,rep("0",chunks-length(out)))
    }
  }
  options(digits=7)
  return(c(as.character(comma),out))
}
