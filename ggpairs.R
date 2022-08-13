ggpairs <- function(df,selection,color=NULL,shape=NULL,alpha=1.0,size=NULL,triangle="lower") {
  df_temp = dplyr::select(df,!!rlang::enexpr(selection))

  for (i in 1:ncol(df_temp)) {
    df[[paste(colnames(df_temp)[i],"_temp",sep="")]]=df_temp[[i]]
  }
  indices=match(colnames(df_temp),colnames(df))
  df2=pivot_longer(df,all_of(indices),names_to="attribx",values_to="xval")

  indices2=c((ncol(df2)-1-ncol(df_temp)):(ncol(df2)-2))

  for (i in 1:ncol(df_temp)) {
    colnames(df2)[indices2[i]]=colnames(df_temp)[[i]]
  }

  df2=pivot_longer(df2,all_of(indices2),names_to="attriby",values_to="yval")

  df2$attribx=factor(df2$attribx,levels=colnames(df_temp))
  df2$attriby=factor(df2$attriby,levels=colnames(df_temp))

  to_delete=c()
  for (i in 1:nrow(df2)) {
    if(triangle=="lower") {
      if(which(colnames(df_temp)==df2$attribx[i])<=which(colnames(df_temp)==df2$attriby[i])) to_delete=c(to_delete,i)
    }
    else if(triangle=="upper") {
      if(which(colnames(df_temp)==df2$attribx[i])>=which(colnames(df_temp)==df2$attriby[i])) to_delete=c(to_delete,i)
    }
    else if(triangle=="both") {
      if(which(colnames(df_temp)==df2$attribx[i])==which(colnames(df_temp)==df2$attriby[i])) to_delete=c(to_delete,i)
    }
    else {
      stop("Invalid argument for triangle - this can be lower, upper or both.")
    }
  }
  df2=df2[-to_delete,]

  g=ggplot(df2,aes(x=yval,y=xval))+geom_point(colour = "grey70")
  
  temp=substitute(size)
  if(typeof(temp)=="double" || typeof(temp)=="NULL") {
    temp=substitute(alpha)
    if(typeof(temp)=="double" || typeof(temp)=="NULL") {
      if(is.null(size)) {
        g=g+geom_point(aes(color=!!rlang::enexpr(color),shape=!!rlang::enexpr(shape)),alpha=alpha)
      }
      else {
        g=g+geom_point(aes(color=!!rlang::enexpr(color),shape=!!rlang::enexpr(shape)),alpha=alpha,size=size)
      }
    }
    else {
      if(is.null(size)) {
        g=g+geom_point(aes(color=!!rlang::enexpr(color),shape=!!rlang::enexpr(shape),alpha=!!rlang::enexpr(alpha)))
      }
      else {
        g=g+geom_point(aes(color=!!rlang::enexpr(color),shape=!!rlang::enexpr(shape),alpha=!!rlang::enexpr(alpha)),size=size)
      }
    }
    
  }
  else {
    temp=substitute(alpha)
    if(typeof(temp)=="double" || typeof(temp)=="NULL") {
      g=g+geom_point(aes(color=!!rlang::enexpr(color),shape=!!rlang::enexpr(shape),size=!!rlang::enexpr(size)),alpha=alpha)
    }
    else {
      g=g+geom_point(aes(color=!!rlang::enexpr(color),shape=!!rlang::enexpr(shape),size=!!rlang::enexpr(size),alpha=!!rlang::enexpr(alpha)))
    }
  }

  g=g+facet_grid(attribx~attriby,scale="free")+xlab("")+ylab("")
  g
}
