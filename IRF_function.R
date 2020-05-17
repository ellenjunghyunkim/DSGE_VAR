ellen <- function(obj, periods=10, cumulative=FALSE, cumul_inds=NULL, var_names=NULL, percentiles=c(.05,.50,.95), 
                     which_shock=NULL, which_response=NULL, shocks_row_order=TRUE, save=FALSE, save_format=c("pdf","eps"), 
                     save_title=NULL, height=13, width=13)
{
  #
  
  if (periods <= 0) {
    stop("error: need periods to be > 0")
  }
  
  #
  
  M <- obj$M
  n_draws <- dim(obj$beta_draws)[3]
  
  irf_temp <- obj$IRF(periods)$irf_vals
  
  # put the IRFs in a tesseract-type format
  
  irf_tess <- array(NA,dim=c(M,M,periods,n_draws))
  
  for (i in 1:n_draws) {
    irf_tess[,,,i] <- irf_temp[,,((i-1)*periods+1):(i*periods)]
  }
  
  if (cumulative)
  {
    if (is.null(cumul_inds)) {
      cumul_inds <- 1:M
    }
    
    for (i in 1:n_draws) {
      irfs_draw_i <- irf_tess[,,,i]
      
      for (ll in cumul_inds) {
        for (jj in 2:periods) {
          irfs_draw_i[ll,,jj] <- irfs_draw_i[ll,,jj] + irfs_draw_i[ll,,jj-1]
        }
      }
      
      irf_tess[,,,i] <- irfs_draw_i
    }
  }
  
  irf_tess_1 <- apply(irf_tess,c(3,1,2),sort) # fix annoying bug
  irf_tess <- aperm(irf_tess_1,c(2,3,1,4))
  
  rm("irf_temp","irf_tess_1")
  
  irf_upper <- min(round(percentiles[3]*n_draws),n_draws)
  irf_mid <- round(percentiles[2]*n_draws)
  irf_lower <- max(round(percentiles[1]*n_draws),1)
  
  #
  
  vplayout <- function(x,y){viewport(layout.pos.row=x, layout.pos.col=y)}
  
  #
  
  if (is.null(which_shock)) {
    which_shock <- 1:M
  }
  
  if (is.null(which_response)) {
    which_response <- 1:M
  }
  
  n_response <- length(which_response)
  n_shocks   <- length(which_shock)
  
  if (class(var_names) != "character") {
    var_names <- character(length=M)
    for (i in 1:M) {  
      var_names[i] <- paste("VAR",i,sep="")
    }
  }
  
  #
  
  plot_vals <- array(NA,dim=c(periods,4,M,M))
  IRFPData <- 0
  
  for (i in 1:M) {
    for (k in 1:M) {
      IRFPData <- data.frame(irf_tess[,k,irf_lower,i],irf_tess[,k,irf_mid,i],irf_tess[,k,irf_upper,i],1:(periods))
      IRFPData <- as.matrix(IRFPData)
      plot_vals[,,k,i] <- IRFPData
    }
  }
  
  #
  # plot IRFs
  
  save_format <- match.arg(save_format)
  
  IRFL <- IRFM <- IRFU <- Time <- NULL # CRAN check workaround
  
  if (n_response == M && n_shocks == M) {
    
    if (save==TRUE)
    {
      if(class(dev.list()) != "NULL"){dev.off()}
      
      save_name <- ""
      if (!is.null(save_title)) {
        save_name <- paste(save_title,".",save_format,sep="")
      } else {
        save_name <- paste("IRFs.",save_format,sep="")
      }
      
      if (save_format=="eps") {
        cairo_ps(filename=save_name,height=height,width=width)
      } else {
        cairo_pdf(filename=save_name,height=height,width=width)
      }
    }
    
    grid.newpage()
    pushViewport(viewport(layout=grid.layout(M,M)))
    
    for (i in 1:M) {
      for (k in 1:M) {
        NameResponse <- var_names[k]
        NameImpulse  <- var_names[i]
        
        IRFDF <- plot_vals[,,k,i]
        IRFDF <- data.frame(IRFDF)
        colnames(IRFDF) <- c("IRFL","IRFM","IRFU","Time")
        
        #
        
        gg1 <- ggplot(data=(IRFDF),aes(x=Time)) + xlab("") + ylab("") 
        gg2 <- gg1 +  geom_hline(yintercept=0) + geom_line(aes(y=IRFM),color="red",size=1) 
        gg3 <- gg2 + theme(panel.background = element_rect(fill='white', colour='grey5')) + theme(panel.grid.major = element_line(colour = 'grey89'))
        
        if (shocks_row_order) {
          print(gg3,vp = vplayout(i,k))
        } else {
          print(gg3,vp = vplayout(k,i))
        }
        
        #
        
        Sys.sleep(0.3)
      }
    }
    
    if (save==TRUE) {dev.off()}
    
  } else {
    
    if (n_response < 4) {
      MR <- n_response; MC <- 1
    } else if (n_response == 4) {
      MR <- 2; MC <-2
    } else if (n_response > 4 && n_response < 7) {
      MR <- 3; MC <- 2
    } else if (n_response > 6 && n_response < 10) {
      MR <- 3; MC <- 3
    } else if (n_response > 9 && n_response < 13) {
      MR <- 4; MC <- 3
    }else if (n_response > 12 && n_response < 17) {
      MR <- 4; MC <- 4
    }else if (n_response > 17 && n_response < 21) {
      MR <- 5; MC <- 4
    }else if (n_response > 20 && n_response < 26) {
      MR <- 5; MC <- 5
    } else if (n_response > 25 && n_response < 31) {
      MR <- 5; MC <- 6
    } else if (n_response > 30 && n_response < 37) {
      MR <- 6; MC <- 6
    } else {
      stop("You have too many IRFs to plot!")
    }
    
    #
    
    for (i in which_shock) {
      plot_ind_r <- 1
      plot_ind_c <- 1
      
      if (save==TRUE) {
        if(class(dev.list()) != "NULL"){dev.off()}
        
        if (n_shocks==1) {
          cairo_pdf(filename="IRFs.pdf",height=height,width=width)
        } else {
          SaveIRF <- paste(var_names[i])
          cairo_pdf(filename=SaveIRF,height=height,width=width)
        }
      }
      
      if (save==TRUE)
      {
        if(class(dev.list()) != "NULL"){dev.off()}
        
        save_name <- ""
        if (n_shocks==1) {
          if (!is.null(save_title)) {
            save_name <- paste(save_title,".",save_format,sep="")
          } else {
            save_name <- paste("IRFs.",save_format,sep="")
          }
        } else {
          save_name <- paste(var_names[i])
        }
        
        if (save_format=="eps") {
          cairo_ps(filename=save_name,height=height,width=width)
        } else {
          cairo_pdf(filename=save_name,height=height,width=width)
        }
      }
      
      grid.newpage()
      pushViewport(viewport(layout=grid.layout(MR,MC)))
      
      for (k in which_response) {
        NameResponse <- var_names[k]
        NameImpulse  <- var_names[i]
        
        IRFDF <- plot_vals[,,k,i]
        IRFDF <- data.frame(IRFDF)
        
        colnames(IRFDF) <- c("IRFL","IRFM","IRFU","Time")
        
        #
        
        gg1 <- ggplot(data=(IRFDF),aes(x=Time)) + xlab("") + ylab("") 
        gg2 <- gg1 + geom_ribbon(aes(ymin=IRFL,ymax=IRFU),color="blue",lty=1,fill="blue",alpha=0.2,size=0.1) + geom_hline(yintercept=0) + geom_line(aes(y=IRFM),color="red",size=2) 
        gg3 <- gg2 + theme(panel.background = element_rect(fill='white', colour='grey5')) + theme(panel.grid.major = element_line(colour = 'grey89'))
        
        print(gg3,vp = vplayout(plot_ind_r,plot_ind_c))
        
        Sys.sleep(0.3)
        
        #
        
        plot_ind_c <- plot_ind_c + 1
        
        if (plot_ind_c > MC) {
          plot_ind_c <- 1
          plot_ind_r <- plot_ind_r + 1
        }
      }
    }
    
    if(save==TRUE){dev.off()}
    
  }
  
  #
  
  return=list(plot_vals=plot_vals)
}

