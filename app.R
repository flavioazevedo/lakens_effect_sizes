library(shiny)
library(shinythemes)

ui <- fluidPage(theme=shinytheme('flatly'),
                titlePanel('Calculating Effect Sizes'),
                tabsetPanel(type='tabs', id='tabset',
                            tabPanel('t Tests (Independent Samples)', value='indep',
                                     h3('Results', align='center'),
                                     fluidRow(
                                       column(12, align='center',
                                              tableOutput("ti")
                                       )
                                     ),
                                     br(),
                                     h4('Enter whatever information you have',
                                        'about the samples.'),
                                     wellPanel(fluidRow(
                                       column(3,
                                              numericInput('n1', 'Group 1 Sample Size', 10)),
                                       column(3,
                                              numericInput('n2', 'Group 2 Sample Size', 10)),
                                       column(3,
                                              numericInput('nt', 'Total N', NA)),
                                       column(3,
                                              numericInput('itval', 't Value', 2.5175, step=.1))
                                     ),
                                     fluidRow(
                                       column(3, 
                                              numericInput('im1', 'Mean for Group 1', NA)),
                                       column(3,
                                              numericInput('im2', 'Mean for Group 2', NA)),
                                       column(3,
                                              numericInput('isd1', 'SD for Group 1', NA)),
                                       column(3,
                                              numericInput('isd2', 'SD for Group 2', NA))
                                     ),
                                     fluidRow(align='center',
                                              div(style="display:inline-block; padding-right:10px", actionButton('calc_ti', 'Calculate',
                                                                                                                 style="color: #fff; background-color: #ff5656; border-color: #ff5656")),
                                              div(style="display:inline-block",actionButton('r_ti', 'Clear', 
                                                                                            style="color: #fff; background-color: #337ab7; border-color: #337ab7"))
                                     )
                                     )
                            ),
                            tabPanel('t Tests (Dependent or Correlated Samples)', value='dep',
                                     h3('Results', align='center'),
                                     fluidRow(
                                       column(12, align='center',
                                              tableOutput("td")
                                       )
                                     ),
                                     br(),
                                     h4('Enter whatever information you have',
                                        'about the samples.'),
                                     wellPanel(
                                       fluidRow(
                                         column(3,
                                                numericInput('pairs', 'Number of Pairs', 10)),
                                         column(3,
                                                numericInput('dm1', 'Mean for Group 1', NA)),
                                         column(3,
                                                numericInput('dm2', 'Mean for Group 2', NA)),
                                         column(3,
                                                numericInput('r', 'Correlation between Groups', NA))
                                       ),
                                       fluidRow(
                                         column(3, 
                                                numericInput('dtval', 'T Value', 4.7, step=.1)),
                                         column(3,
                                                numericInput('dsd1', 'SD for Group 1', NA)),                         
                                         column(3,
                                                numericInput('dsd2', 'SD for Group 2', NA)),
                                         column(3, offset=.5, align='bottom',
                                                div(style="display:inline-block; padding-top:25px; padding-right:10px", 
                                                    actionButton('calc_td', 'Calculate',
                                                                 style="color: #fff; background-color: #ff5656; border-color: #ff5656")),
                                                div(style="display:inline-block; padding-top:25px",
                                                    actionButton('r_td', 'Clear', 
                                                                 style="color: #fff; background-color: #337ab7; border-color: #337ab7")))
                                         )
                                     )
                            ),
                            tabPanel('F Tests',
                                     sidebarLayout(
                                       sidebarPanel(
                                         id='ftests',
                                         numericInput('fstat', 'F Statistic', 6.34, step=.1),
                                         numericInput('dfh', 'Treatment Degrees of Freedom', 1),
                                         numericInput('dfe', 'Residual Degrees of Freedom', 18),
                                         div(style="display:inline-block;padding-right:10px", actionButton('calc_f', 'Calculate',
                                                      style="color: #fff; background-color: #ff5656; border-color: #ff5656;")),
                                         div(style="display:inline-block;padding-right:10px", actionButton('r_f', 'Clear',
                                                      style="color: #fff; background-color: #337ab7; border-color: #337ab7"))
                                       ),
                                       mainPanel(align='center',
                                                 h3('Results'),
                                                 tableOutput("f_es")
                                       )
                                     )
                            ),
                            tabPanel('Correlations',
                                     sidebarLayout(
                                       sidebarPanel(
                                         h5("Enter two of three values to calculate the third."),
                                         numericInput('r1', 'Correlation 1', .5, step=.1),
                                         numericInput('r2', 'Correlation 2', .42, step=.1),
                                         numericInput('q', "Cohen's q", NA, step=.1),
                                         div(style="display:inline-block;padding-right:10px", actionButton('calc_q', 'Calculate',
                                                                                                           style="color: #fff; background-color: #ff5656; border-color: #ff5656;")),
                                         div(style="display:inline-block;padding-right:10px", actionButton('r_q', 'Clear',
                                                                                                           style="color: #fff; background-color: #337ab7; border-color: #337ab7"))
                                       ),
                                       mainPanel(align='center',
                                                 h3('Results'),
                                                 tableOutput("c_q")
                                       )
                                     )
                            )
                ),
                hr(),
                fluidRow(
                  column(width=11, offset=.5, h5("This is adapted from Daniel Lakens's original spreadsheets.",
                                                   "To cite, please see:", 
                                                    a('Lakens, 2013.', 
                                                   href='http://journal.frontiersin.org/article/10.3389/fpsyg.2013.00863/full'), 
                                                   'and the accompanying', a('OSF page.', href='http://openscienceframework.org/project/ixGcd',
                                                                             target="_blank"))),
                   column(width=11, offset=.5, h5('Application created by Katherine Wood', a('(@kathmwood)', href="https://twitter.com/kathmwood",
                                                                                             target="_blank")))
                  )
                )

server <- function(input, output, session) {
  get_ti <- function(n1, n2, nt, im1, im2, isd1, isd2, itval) {
    if (sum(!is.na(c(n1, n2))) == 0 & is.na(nt)){
      print('Sample size required.')
    }
    if (sum(is.na(c(im1, im2, isd1, isd2, n1, n2))) == 0){
      df <- n1 + n2 - 2
      mdiff <- abs(im1 - im2)
      sdiff <- sqrt((((n1-1)*isd1^2) + ((n2-1)*isd2^2))/df)
      sediff <- sqrt(((isd1^2)/n1) + ((isd2^2)/n2))
      t <- mdiff/(sdiff*sqrt((1/n1) + (1/n2)))
      p <- pt(t, n1+n2-2, lower.tail=FALSE)*2
      cl <- pnorm(abs(mdiff/sqrt(isd1^2+isd2^2)))
      ds <- mdiff/sqrt((((n1-1)*isd1^2) + ((n2-1)*isd2^2))/df)
      d <- mdiff/sqrt((((n1-1)*isd1^2) + ((n2-1)*isd2^2))/(n1+n2))
      hgs <- ds*(1-(3/(4*df - 1)))
      ci_width <- qt(.025, df, lower.tail=FALSE)*sediff
      td <- data.frame('Mean Difference'=mdiff, '95% CI (lower)'=mdiff-ci_width,
                       '95% CI (upper)'=mdiff+ci_width, '\U1D461 Value'=t, '\U1D45D Value'=p,
                       "Cohen's ds"=ds, "Cohen's d"=d, 'Hedges gs'=hgs,
                       'CL Effect Size'=cl, check.names=FALSE)
    } else if (sum(is.na(c(n1, n2, itval))) == 0) {
      ds <- (itval*sqrt(n1+n2))/sqrt(n1*n2)
      data.frame("Cohen's ds"=ds,
                 '\U1D45D Value' = pt(itval, n1+n2-2, lower.tail=FALSE)*2,
                 'Hedges gs'=ds*(1-(3/(4*(n1+n2) - 9))),
                 'CL Effect Size'=pnorm(ds/sqrt(2)), check.names=FALSE)
    } else if (sum(is.na(c(nt, itval))) == 0) {
      ds <- (2*itval)/sqrt(nt)
      data.frame("Cohen's ds"=ds,
                 '\U1D45D value' = pt(itval, nt-2, lower.tail=FALSE)*2,
                 'Hedges gs'=ds*(1-(3/(4*nt - 9))),
                 'CL Effect Size'=pnorm(ds/sqrt(2)), check.names=FALSE)
    } else {
      print('Sorry, not enough information.')
    }
  }
  
  get_td <- function(pairs, dm1, dm2, dsd1, dsd2, r, dtval) {
    if (is.na(pairs)){
      print('Sample size required.')
    }
    if (sum(is.na(c(r, dm1, dm2, dsd1, dsd2, pairs))) == 0){
      df <- pairs - 1
      mdiff <- abs(dm1 - dm2)
      sdiff <- sqrt(dsd1^2 + dsd2^2 - (2*r*dsd1*dsd2))
      sediff <- sqrt(((dsd1^2)/pairs) + ((dsd2^2)/pairs) - (2*r*(dsd1/sqrt(pairs))*(dsd2/sqrt(pairs))))
      t <- mdiff/(sdiff/sqrt(pairs))
      p <- pt(t, pairs-1, lower.tail=FALSE)*2
      cl <- pnorm(mdiff/sdiff)
      ci_width <- qt(.025, df, lower.tail=FALSE)*sediff
      dz <- mdiff/sdiff
      drm <- dz*sqrt(2*(1-r))
      dav <- mdiff/sqrt((dsd1^2+dsd2^2)/2)
      hgav <- dav*(1 - (3/(4*df - 1)))
      hgrm <- drm*(1 - (3/(4*df - 1)))
      corr_factor <- mdiff/sqrt((df*(dsd1^2 + dsd2^2))/2*df)
      rec <- ifelse(abs(corr_factor - drm) < abs(corr_factor - dav), 
                    'Hedges grm', 'Hedges gav')
      data.frame('Mean difference'=mdiff, '95% CI (lower)'=mdiff-ci_width,
                 '95% CI (upper)'=mdiff+ci_width, '\U1D461 value'=t,
                 '\U1D45D value'=p, "Cohen's dz"=dz, "Cohen's drm"=drm,  'Hedges grm'=hgrm,
                 "Cohen's dav"=dav, 'Hedges gav'=hgav, 'Rec. ES'=rec, 
                 'CL Effect Size'=cl,check.names=FALSE)
    } else if (sum(is.na(c(dtval, pairs))) == 0){
      dz <- dtval/sqrt(pairs)
      data.frame('\U1D45D value'=pt(dtval, pairs-1, lower.tail=FALSE)*2,
                 "Cohen's dz"=dz, 'CL Effect Size' = pnorm(dz), check.names=FALSE)
    } else {
      print('Sorry, not enough information.')
    }
  }
  
  get_f <- function(fstat, dfh, dfe) {
    data.frame('Partial \U03B7\U00B2'=(fstat*dfh)/(fstat*dfh + dfe),
               'Partial \U03C9\U00B2'=(fstat-1)/(fstat+((dfe+1)/dfh)),
               'Partial \U03B5\U00B2'=(fstat-1)/(fstat+(dfe/dfh)),
               '\U1D45D value'=pf(fstat, dfh, dfe, lower.tail=FALSE), check.names=FALSE)
  }
  get_q <- function(r1, r2, q) {
    corrs <- c(r1, r2)
    if (is.na(q) | sum(is.na(c(r1, r2, q))) == 0)  {
      q <- .5*log((1+r1)/(1-r1)) - .5*log((1+r2)/(1-r2))
    } else {
      r1 <- corrs[!is.na(corrs)]
      r2 <- (exp(2*q+log((1+r1)/(1-r1))) - 1)/(exp(2*q+log((1+r1)/(1-r1))) + 1)
    }
    data.frame('r\U2081'=r1, 'r\U2082'=r2, "Cohen's \U1D45E"=q, check.names=FALSE)
  }
  
  reset <- function(session, fields, value=NA) {
    sapply(fields, function(field) updateNumericInput(session, field, value=value))
  }
  
  dataframes <- reactiveValues(c_q=NULL, ti=NULL, td=NULL, f_es=NULL, 
                               lcq=0, lcti=0, lctd=0, lcf=0,
                               rcq=0, rcti=0, rctd=0, rcf=0)
  observe({
    if (input$calc_ti > dataframes$lcti | input$calc_ti==0) {
      dataframes$lcti <- input$calc_ti
      dataframes$ti <- isolate(get_ti(input$n1, input$n2, input$nt, input$im1, input$im2, 
                                      input$isd1, input$isd2, input$itval))
    }
    if (input$r_ti > dataframes$rcti) {
      dataframes$rcti <- input$r_ti
      reset(session, c('n1', 'n2', 'nt', 'itval', 'im1', 'im2', 'isd1', 'isd2'))
    }
    if (input$calc_q > dataframes$lcq | input$calc_q == 0) {
      dataframes$lcq <- input$calc_q
      dataframes$c_q <- isolate(get_q(input$r1, input$r2, input$q))
    }
    if(input$r_q > dataframes$rcq) {
      dataframes$rcq <- input$r_q
      reset(session, c('r1', 'r2', 'q'))
    }
    if (input$calc_f > dataframes$lcf | input$calc_f == 0) {
      dataframes$lcf <- input$calc_f
      dataframes$f_es <- isolate(get_f(input$fstat, input$dfh, input$dfe))
    }
    if (input$r_f > dataframes$rcf) {
      dataframes$rcf <- input$r_f
      reset(session, c('fstat', 'dfh', 'dfe'))
    }
    if (input$calc_td > dataframes$lctd | input$calc_td==0) {
      dataframes$lctd <- input$calc_td
      dataframes$td <- isolate(get_td(input$pairs, input$dm1, input$dm2, input$dsd1,
                                      input$dsd2, input$r, input$dtval))
    }
    if (input$r_td > dataframes$rctd) {
      dataframes$rctd <- input$r_td
      reset(session, c('pairs', 'dm1', 'dm2', 'r', 'dtval', 'dsd1', 'dsd2'))
    }
  })
  
  output$ti <- renderTable(dataframes$ti, striped=TRUE, digits=5, bordered=TRUE)
  output$c_q <- renderTable(dataframes$c_q, striped=TRUE, digits=5, bordered=TRUE)
  output$f_es <- renderTable(dataframes$f_es, striped=TRUE, digits=5, bordered=TRUE)
  output$td <- renderTable(dataframes$td, striped=TRUE, digits=5, bordered=TRUE)
}

# Run the application 
shinyApp(ui = ui, server = server)

