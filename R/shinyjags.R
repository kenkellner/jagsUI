require(shiny)
require(DT)


shinyjags <- function(object){
  
  check_class(object)

  #Check for required packages
  check_namespace <- function(pkg){ 
    if(!requireNamespace("shiny", quietly = TRUE)){
      stop("Package \"pkg\" needed for this function to work. Please install it.",
        call. = FALSE)
    }
  }
  sapply(c("shiny", "DT", "coda"), check_namespace)

  params_all <- param_names(object$samples)
  mod_name <- deparse(substitute(object))
  ess_suggest <- object$mcmc.info$n.chains*100
  if(!is.null(names(object$model))){
    data_list <- object$model$data()
  }else { 
    data_list <- object$model[[1]]$data()
  }
  data_names <- names(data_list)

  shiny::shinyApp(
    ui = shiny::fluidPage(
         shiny::tags$head(
          shiny::tags$style(shiny::HTML("hr {border-top: 1px solid #000000;}"))
          ),
        shiny::titlePanel(paste("JAGS model:", mod_name)),
        shiny::tabsetPanel(type="tabs",
        shiny::tabPanel("Inference",
                 shiny::fluidRow(
                  shiny::column(6,
                    shiny::h3("Output Summary"),
                    shiny::h5("Click on rows to add them to plot"),
                    DT::dataTableOutput('summary'),
                    shiny::actionButton('reset_sel', label='Deselect all rows'),
                    shiny::br(), shiny::br(),
                    shiny::HTML(paste("Rhat should be < 1.05 and bulk ESS should be >",
                          object$mcmc.info$n.chains*100,"(no. chains * 100).<br>",
                          "Diagnostic calculations are based on Vehtari et al.,",
                          '"Rank-normalization, folding, and localization:',
                          'An improved Rhat for assessing convergence of MCMC" (2019).'))
                    ),
                  shiny::column(5,
                    shiny::plotOutput('whisker',height='750px'),
                    shiny::div(style="text-align:center",
                    shiny::div(style="display: inline-block;vertical-align:top; width: 250px;",
                    shiny::radioButtons("CI","Credible interval", 
                                 c("95%","90%","80%","68%"),inline=TRUE)),
                    shiny::div(style="display: inline-block;vertical-align:top; width: 200px;",
                    shiny::checkboxInput("zeroline", "Draw line at zero", FALSE))
                    )
                    ))),
        shiny::tabPanel("Model Fit",
        shiny::sidebarLayout(
          shiny::sidebarPanel(
            shiny::h4("Deviance"),
            shiny::htmlOutput("dic_out"),
            shiny::htmlOutput("pD_out"),
            shiny::br(),
            shiny::h4("Posterior predictive check"),
            shiny::selectInput("obs", "Observed parameter", c("",params_all)),
            shiny::selectInput("sim", "Simulated parameter", c("",params_all)),
            shiny::br(),
            shiny::htmlOutput("bpval")
          ),
          shiny::mainPanel(shiny::plotOutput("postpred", height='750px'))
        )),
        shiny::tabPanel("Diagnostics",
        shiny::sidebarLayout(
        shiny::sidebarPanel(
                     shiny::h4('Diagnostics Summary'),
                     shiny::htmlOutput("suf_adapt"),
                     shiny::htmlOutput("rhat_sum"),
                     shiny::htmlOutput("ess_sum"),
                     shiny::br(),
                     shiny::h4('Individual Parameters'),
                     shiny::checkboxInput("rhat",
                        "Only show parameters with Rhat above:", 
                                   FALSE),
                     shiny::sliderInput("rhat_lim", "Rhat min", 1, 3, 1.05,
                                 step=0.05),
                     shiny::checkboxInput("ess", 
                          "Only show parameters with effective sample size below:", 
                                   FALSE),
                     shiny::sliderInput("ess_lim", "ESS max", 0, 
                                 ess_suggest*2, ess_suggest),
                     shiny::hr(),
                     shiny::selectInput("prm", "Choose parameter", params_all,
                                 params_all[1]),
                     shiny::htmlOutput("rhat_val"),
                     shiny::htmlOutput("samples_val"),
                     shiny::htmlOutput("ess_val")),
                     
        shiny::mainPanel(shiny::splitLayout(cellWidths=c('50%','50%'),
                  shiny::plotOutput('trace'),
                  shiny::plotOutput('autocor')),
                  shiny::splitLayout(cellWidths=c('50%','50%'),
                    shiny::plotOutput('dens'), 
                    shiny::plotOutput('post')
                  )
        ))),
        shiny::tabPanel("Model Info", 
                 shiny::fluidRow(
                 shiny::column(3,
                  shiny::h4("Run info"),
                  shiny::HTML(paste("<b>Run on:</b>",
                    format(object$run.info$start.time,'%b %d %Y'),
                    'at',format(object$run.info$start.time,'%H:%M'))),
                  shiny::br(),
                  shiny::HTML(paste("<b>Runtime:</b>",get_runtime(object))),
                  shiny::br(),
                  shiny::HTML(paste("<b>Parallel:</b>",
                             ifelse(object$run.info$parallel,
                              paste0('Yes (',object$run.info$n.cores,' cores)'),'No'))),
                  shiny::br(),
                  shiny::br(),
                  shiny::h4("MCMC info"),
                  shiny::HTML(paste0("<b>Chains:</b> ",object$mcmc.info$n.chains,', each with:')),
                  shiny::br(),
                  shiny::HTML(paste("<b>Adaptation iterations:</b>",object$mcmc.info$n.adapt)),
                  shiny::br(),
                  shiny::HTML(paste0("<b>Regular iterations:</b> ",
                    object$mcmc.info$n.iter,' (',object$mcmc.info$n.burnin,
                    ' burn-in and ',
                    object$mcmc.info$n.iter-object$mcmc.info$n.burnin,' saved)')),
                  shiny::br(),
                  shiny::HTML(paste("<b>Thinning rate:</b>",object$mcmc.info$n.thin)),
                  shiny::br(),
                  shiny::br(),
                  shiny::HTML(paste("<b>Total draws:</b>",object$mcmc.info$n.draws))
                  ),
                 shiny::column(9,
                  shiny::h4(paste("Model file:", object$modfile)),
                  shiny::h4("BUGS code:"),
                  shiny::verbatimTextOutput("model")
                 ))
                 ),
        shiny::tabPanel("Input Data",
          shiny::sidebarLayout(
          shiny::sidebarPanel(shiny::selectInput("data_el", "Data component", data_names),
                       shiny::HTML("<b>Type (dim):</b> "),
                       shiny::textOutput("data_type",inline=TRUE),
                       shiny::br(),
                       shiny::br(),
                       shiny::HTML("<b>Data Summary:</b>"),
                       shiny::br(),
                       shiny::tableOutput("data_sum")
                       ),
          shiny::mainPanel(shiny::splitLayout(cellWidths=c('50%','50%'),
                      shiny::plotOutput('data_box', height='750px'),
                      shiny::plotOutput('data_hist', height='750px')
                    )
          )
        ))
        )),

    
    server = function(input, output, session){
      
      shiny::observe({
        r <- input$rhat
        rl <- input$rhat_lim
        e <- input$ess
        el <- input$ess_lim
      
        params_sub <- param_limit(object, r, rl, e, el)
        shiny::updateSelectInput(session, "prm", choices=params_sub, 
                          selected=params_sub[1])

      })

      shiny::observeEvent(input$reset_sel, 
                   DT::selectRows(DT::dataTableProxy('summary'), NULL))
      output$suf_adapt <- shiny::renderText({
        val <- ifelse(object$mcmc.info$sufficient.adapt,
              '<font color="#000000">Yes</font>',
               '<font color="#fa4b25">No</font>')
        paste('<b>Sufficient adaptation iterations:</b>',val)})
      output$rhat_sum <- shiny::renderText(rhat_sum(object))
      output$ess_sum <- shiny::renderText(ess_sum(object))
      output$rhat_val <- shiny::renderText(diag_text(object, 'Rhat',input$prm))
      output$samples_val <- shiny::renderText(paste('<b>Samples:</b>',
                                             object$mcmc.info$n.draws))
      output$ess_val <- shiny::renderText(diag_text(object, 'ess',input$prm))
      output$trace <- shiny::renderPlot(traceplot(object, input$prm))
      output$dens <- shiny::renderPlot(densityplot(object, input$prm))
      output$post <- shiny::renderPlot(posterior_hist(object, input$prm))
      output$autocor <- shiny::renderPlot(autocor_plot(object, input$prm))
      output$model <- shiny::renderText({
        if(object$run.info$parallel){
          paste(object$model[[1]]$model(),collapse='\n')
        }else{
          paste(object$model$model(),collapse='\n')}})
      output$summary <- DT::renderDataTable(sum_table(object))
      output$whisker <- shiny::renderPlot({inds <- input$summary_rows_selected
                                   whisk_plot(object, inds, input$CI,
                                              input$zeroline)})
      output$postpred <- shiny::renderPlot(postpred_plot(object, 
                                      input$obs, input$sim))
      output$bpval <- shiny::renderText(get_bpval(object, input$obs, input$sim))
      output$dic_out <- shiny::renderText(paste("<b>DIC:</b>",
                                    round(object$DIC,3)))
      output$pD_out <- shiny::renderText(paste("<b>pD:</b>",
                                    round(object$pD,3)))
      output$data_hist <- shiny::renderPlot(data_hist(data_list, input$data_el))
      output$data_box <- shiny::renderPlot(data_box(data_list, input$data_el))
      output$data_sum <- shiny::renderTable(data_sum(data_list, input$data_el))
      output$data_type <- shiny::renderText(get_type(data_list, input$data_el))
    }
  )
}

diag_text <- function(object, type, input){
  hc <- '<font color="#000000">'
  if(type=='Rhat'){
    val <- object$summary[input,'Rhat']
    if(val > 1.05) hc <- '<font color="#fa4b25">'
    val <- round(val,3)
    title <- 'Rhat: '
  } else if(type=='ess'){
    val <- round(object$summary[input,'n.eff'])
    if(val < 300) hc <- '<font color="#fa4b25">'
    title <- 'Effective sample size: '
  }
  paste0('<b>',title,'</b>',hc,val,'</font>')
}

rhat_sum <- function(object){
  rh <- object$summary[,'Rhat'] <= 1.05
  fnt <- ifelse(mean(rh)==1, '<font color="#000000">',
                '<font color="#fa4b25">')
  paste0('<b>Rhat:</b> ',fnt,sum(rh),'/',length(rh),'</font> parameters (',
                 round(mean(rh)*100),'%) had split Rhat < 1.05')
}

ess_sum <- function(object){
  ef_lim <- 100*length(object$samples)
  ef <- object$summary[,'n.eff'] >= ef_lim
  fnt <- ifelse(mean(ef)==1, '<font color="#000000">',
                '<font color="#fa4b25">')
  paste0('<b>ESS:</b>  ',fnt,sum(ef),'/',length(ef),'</font> parameters (',
                 round(mean(ef)*100),'%) had bulk ESS > ',ef_lim)
}

sum_table <- function(object){
  out <- as.data.frame(round(object$summary,3))
  out <- cbind(Order=1:nrow(out),Parameter=rownames(out), out)
  rownames(out) <- NULL
  out <- out[,-c(6,8,13)]
  out$overlap0 <- out$overlap0==1
  out$n.eff <- round(out$n.eff)
  names(out) <- c('Order','Parameter','Mean','SD','2.5%','50%',
                  '97.5%','Rhat','Bulk ESS','overlap0')
  out <- DT::datatable(out, rownames=FALSE)

  out <- DT::formatStyle(out, columns='Rhat',
            backgroundColor=DT::styleInterval(1.05000,c('fake','#fa4b25')))
  ess_lim <- length(object$samples)*100
  out <- DT::formatStyle(out, columns='Bulk ESS',
            color=DT::styleInterval(ess_lim,c('#fa4b25','fake')))
  
  out
}

param_limit <- function(object, rhat, rhat_lim, ess, ess_lim){
  param <- rownames(object$summary)

  if(rhat){
    rhat_vals <- object$summary[,'Rhat']
    param <- param[rhat_vals>rhat_lim]
  }
  if(ess){
    ess_vals <- object$summary[,'n.eff']
    param <- param[ess_vals<ess_lim]
  }
  param

}

posterior_hist <- function(object, param){
  old_par <- graphics::par(no.readonly=TRUE)
  on.exit(graphics::par(old_par))
  graphics::par(mar=c(4,4,2.5,1))
  tryCatch({
    samples <- mcmc_to_mat(object$samples, param)
  }, error=function(e) stop("Parameter not found in output"))
  graphics::hist(samples, col='lightgray', xlab='Value', ylab='Density',
       prob=TRUE,main=paste('Posterior of', param))
  graphics::lines(stats::density(samples),lwd=2)
  graphics::abline(v=mean(samples,na.rm=T), col='red', lwd=2)
  graphics::abline(v=stats::quantile(samples,0.5,na.rm=T),col='blue',lwd=2)
  graphics::legend('topleft',lwd=2,col=c('red','blue'),
                   legend=c('Mean','Median'), bg='white')
  graphics::box()
}

autocor_plot <- function(object, param){
  old_par <- graphics::par(no.readonly=TRUE)
  on.exit(graphics::par(old_par))
  graphics::par(mar=c(4,4,2.5,1))
  tryCatch({
    psub <- select_cols(object$samples, param)
  }, error=function(e) stop("Parameter not found in output"))
  for (i in 1:length(psub)) colnames(psub[[i]]) <- NULL
  coda::autocorr.plot(psub, auto.layout=FALSE, ask=FALSE,
                       main=paste('Autocorrelation of',param), lwd=3)
}

whisk_plot <- function(object, inds, CI, zeroline){
  old_par <- graphics::par(no.readonly=TRUE)
  on.exit(graphics::par(old_par))
  graphics::par(mar=c(4,5,2.5,0))
  sum_prms <- rownames(object$summary)[inds]
  if(!length(sum_prms)){
    graphics::plot(1, type='n',xlab='Parameters',
         ylab=paste('Mean and',CI,'credible interval'),
         xaxt='n', yaxt='n', cex.lab=1.5)
    graphics::box()
    return(invisible())
  }

  quants <- c(0.025,0.975)
  if(CI=='80%') quants <- c(0.1,0.9)
  if(CI=='90%') quants <- c(0.15,0.95)
  if(CI=='68%') quants <- c(0.16,0.84)

  post_stats <- sapply(sum_prms, 
                       function(i){
                          sims <- mcmc_to_mat(object$samples, i)
                          c(mean(sims,na.rm=TRUE), 
                            stats::quantile(sims,na.rm=TRUE,quants))
                        })
 
  #Plot parameter means
  n <- length(sum_prms)
  ylims <- range(post_stats)
  if(zeroline) ylims <- range(c(0, post_stats))

  graphics::plot(1:n, post_stats[1,], xaxt="n", 
                 ylim=ylims, xlim=c(0,n+1),
                 xlab="Parameters", 
                 ylab=paste('Mean and',CI,'credible interval'),
                 pch=19, cex=1.5, cex.lab=1.5)
  graphics::axis(side=1, at=1:n, labels=sum_prms)
  graphics::box()
  
  #Draw line at zero
  if(zeroline) graphics::abline(h=0, lty=2)
  
  #Draw error bars
  wd <- (n+2)/60
  graphics::segments(1:n, post_stats[2,], 1:n, post_stats[3,], lwd=2)
  graphics::segments(1:n-wd, post_stats[2,], 1:n+wd, post_stats[2,])
  graphics::segments(1:n-wd, post_stats[3,], 1:n+wd, post_stats[3,])

}

postpred_plot <- function(object, obs, sim){
  
  if(obs==""|sim==""){ 
    graphics::plot(1, type='n',xlab='Observed',
         ylab=paste('Simulated'),
         xaxt='n', yaxt='n', cex.lab=1.5)
    graphics::box()
    return(invisible())
  }
  pp.check(object, obs, sim, cex.lab=1.5)

}

get_bpval <- function(object, obs, sim){
  if(obs==""|sim==""){
    return("")
  } 
  obs <- c(mcmc_to_mat(object$samples, obs))
  sim <- c(mcmc_to_mat(object$samples, sim))
  paste("<b>Bayesian p-value =</b>",round(mean(sim>obs),2))
}

data_sum <- function(data_list, data_element){
  out <- as.matrix(summary(data_list[[data_element]]))
  out <- round(out, 3)
  data.frame(Statistic=rownames(out), Value=out[,1])
}

data_hist <- function(data_list, data_element){

  dat <- data_list[[data_element]]
  if(length(c(dat))<2){ 
    graphics::plot(1, type='n',xlab='Value',
         ylab=paste('Density'),
         xaxt='n', yaxt='n', cex.lab=1.5,
         main=paste('Distribution of',data_element))
    graphics::box()
    return(invisible())
  }

  graphics::hist(dat, col='lightgray', xlab='Value', ylab='Density',
       prob=TRUE,main=paste('Distribution of', data_element), cex.lab=1.5)
  #lines(density(dat),lwd=2)
  graphics::abline(v=mean(dat,na.rm=T), col='red', lwd=2)
  graphics::abline(v=stats::quantile(dat,0.5,na.rm=T),col='blue',lwd=2)
  graphics::legend('topleft',lwd=2,col=c('red','blue'),
                   legend=c('Mean','Median'), bg='white')
  graphics::box()
}

data_box <- function(data_list, data_element){

  dat <- data_list[[data_element]]
  if(length(c(dat))<2){ 
    graphics::plot(1, type='n',xlab='',
         xaxt='n', ylab='Value', cex.lab=1.5,
         main=paste('Boxplot of', data_element)
    )
    graphics::box()
    return(invisible())
  }

  graphics::boxplot(dat,main=paste('Boxplot of',data_element),ylab='Value',
          cex.lab=1.5)
  graphics::points(stats::runif(length(dat),0.8,1.2),dat,
                   col='gray',pch=19,cex=1.2)


}

get_runtime <- function(object){

  rt <- difftime(object$run.info$end.time, object$run.info$start.time,
                 units='secs')
  if(rt<60){
    return(paste(round(rt,2),'sec'))
  } else if(rt<3600){
    return(paste(round(rt/60,2),'min'))
  }
  return(paste(round(rt/3600,2)),'hr')
}

get_type <- function(data_list, data_el){
  dat <- data_list[[data_el]]
  if(length(dat)==1){ return('scalar')}
  if(is.null(dim(dat))){ 
    return(paste0('vector (',length(dat),')'))
  }
  if(length(dim(dat))==2){ 
    return(paste0('matrix (',paste(dim(dat),collapse=','),')'))
  }
  paste0('array (',paste(dim(dat),collapse=','),')')
}
