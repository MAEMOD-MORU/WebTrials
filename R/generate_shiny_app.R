#' @title Generate Shiny app
#'
#' @description This function launches a simple Shiny app with a text input and output.
#'
#' @param dir set directory of output files.
#' @param name set name of Shiny app.
#' @return A Shiny app file,logo pic and report Rmd files.
#' @export
generate_shiny_app <- function(dir = NULL,
                               name = "shiny_app",
                               create_token=T,
                               login_page=T
                               ) {

  shiny_app_code <- "
library(shiny)
library(shinymanager)
library(rdrop2)
library(dplyr)
library(rmarkdown)
library(shinyWidgets)
library(shinyalert)
library(shinyjs)

### Change Name of tokenfile,logfile, ###
### drop.folder(Dropbox folder)       ###
# this token should be put in the same folder with app.R or on the Shiny server
tokenfile <- file.path('data', 'token.rds')
logfile <- 'log.csv'
drop.folder <- 'folder'   # folder for keeping all the data files
drop_auth(rdstoken = tokenfile)


# credential file should be save in a dropbox folder.
# get credentials list
credentials <- drop_read_csv(file.path(drop.folder, 'Credentials.csv'))

# example code for creating the credential file
# define some credentials
# credentials <- data.frame(
#   user = c('test1','test2','test3'),
#   password = c('test12345','test12345','test12345'),
#   site = c('a','b','c'),
#   admin = c(T, F,F),
#   email = c('test1@email.com','test2@email.com','test3@email.com')
# )
#
# write.csv(x = credentials, file = 'credentials.csv')


# for adding user loging time
# logfile headers : user, timestamp
addLog <- function(user,site,file = file.path(drop.folder,logfile)){

  time.stamp <- date()

  tmplog <- rbind(data.frame(login=user,timestamp=time.stamp), drop_read_csv(file))

  write.csv(tmplog, file=logfile,row.names = F)

  drop_upload(logfile, path = drop.folder)

  paste('You are logged in as ',user,'\non ',time.stamp, '\nfrom site ',site,'.',sep='')
}


# for recording the users and their treatments
# data headers: site, screeningID, age, sex, randomizationID, symptomDay, vaccine, Treatment, Date, user
addPID <- function(site, screenID, age, sex, randID, symptomDay, vaccine, treatment, file, timestamp, user){

  tmpdat <- rbind(drop_read_csv(file.path(drop.folder, file)),
                  data.frame(site = site,
                             screeningID = screenID,
                             age = age, sex = sex,
                             symptomDay=symptomDay,
                             vaccine=vaccine,
                             randomizationID = randID,
                             Treatment = treatment,
                             Date = timestamp,
                             user = user))

  write.csv(tmpdat, file = file, row.names = F)

  drop_upload(file, path = drop.folder)

  return(tmpdat)
}

# check if the patient has been recorded in the data file already
# return F if not and return the data if recorded
existQ <- function(site, pid, age, sex){
  data.file <- paste0('data-',site,'.csv')
  tmp.dat <- drop_read_csv(file.path(drop.folder,data.file))
  tmp <- tmp.dat[tmp.dat$screeningID == pid & tmp.dat$age == age & tmp.dat$sex == sex ,]
  tmp2 <- tmp.dat[tmp.dat$screeningID == pid & tmp.dat$age != age | tmp.dat$sex != sex ,]
  outlist <- list()
  if(nrow(tmp)==1){
    outlist$exist <- TRUE
    outlist$data <- tmp
  }else{
    outlist$exist <- FALSE
    outlist$data <- NULL
  }

  # for checking if the entered PID exist but age and sex are not matched.
  if(nrow(tmp2==1)){
    outlist$exist2 <- TRUE
  }else{
    outlist$exist2 <- FALSE
  }
  return(outlist)
}

## get treatment (Subject No)
# rand headers : site, randomization, Treatment
getTreatment <- function(site, PID){
  rand.file <- paste0('rand-',site,'.csv')
  data.file <- paste0('data-',site,'.csv')

  output <- list()

  # how many patients have been added?
  tmp.data <- drop_read_csv(file.path(drop.folder,data.file))
  is.assigned <- PID %in% tmp.data[,2]
  output$Assigned <- is.assigned

  npid <- nrow(tmp.data)
  tmp.rand <- drop_read_csv(file.path(drop.folder,rand.file))
  n.rand <- nrow(tmp.rand)

  # get the new treatment
  if ((is.assigned == F) && (npid+1 <= n.rand)){
    output$site <- site
    output$screeningID <- PID
    output$randomizationID <- tmp.rand[npid+1,2]
    output$Treatment <- tmp.rand[npid + 1, 3]
  } else {
    output$Treatment <- NULL
  }
  return(output)
}

## check screening ID is in correct format
check_screening_ID <- function(id){
  if(is.na(id)) return (FALSE)
  if(!is.numeric(id)) return (FALSE)
  xx = unlist(strsplit(as.character(id), split = ''))
  if(!xx[1] == '1') return (FALSE)
  if(!length(xx) == 5) return (FALSE)
  return (TRUE)
}

## check age is in thre correct format
check_age <- function(age){
  if(is.null(age)) return(FALSE)
  if(is.na(age)) return(FALSE)
  if(!is.numeric(age)) return(FALSE)
  return(TRUE)
}

## check days of symptom format
check_symptomDay <- function(symptomDay){
  if(is.null(symptomDay)) return(FALSE)
  if(is.na(symptomDay)) return(FALSE)
  if(!is.numeric(symptomDay)) return(FALSE)
  return(TRUE)
}

## check character(0) or NULL on Sex, vaccine
## if no item selected return FALSE
check_selected <- function(par){
  if(identical(par, character(0)) == TRUE) return(FALSE)
  if(is.null(par) == TRUE) return(FALSE)
  return(TRUE)
}

# the main UI
ui <- fluidPage(
  auth_ui(
    id = 'auth',
    # add image on top
    tags_top =
      tags$div(
        tags$img(src = 'logo.png', width = '70%'),
        tags$h4('WebTrials', style = 'align:center')
      ),
    # add information on bottom ?
    tags_bottom = tags$div(
      tags$p(
        'For any question, please  contact',
        tags$a(
          href = 'mailto:james@tropmedres.ac',
          target='_top', 'James Watson'
        )
      )
    ),
    # change auth ui background ?
    # https://developer.mozilla.org/fr/docs/Web/CSS/background
    background  = 'linear-gradient(rgba(199, 199, 199, 0.5),
                       rgba(155, 155, 155, 0.5));',
    # set language ?
    lan = use_language('en')
  ),
  titlePanel('WebTrials'),
  tagList(

    verbatimTextOutput('auth_output')),

  sidebarLayout(
    sidebarPanel(
      useShinyjs(),
      uiOutput('reset_input'),
      actionButton('validate.button',label = 'Validate')

    ),
    mainPanel(
    )
  )
)### end UI


# server side
server <- function(input, output, session){

  values <- reactiveValues()

  observe({
    if(is.null(input$sex)){
      shinyjs::disable('validate.button')
    }else{
      shinyjs::enable('validate.button')
    }
  })

  output$reset_input <- renderUI({
    div(
      numericInput('PID','Screening ID (1XXXX)',value = NULL),
      radioButtons('sex','Sex:',c('Male','Female'), selected = character(0), inline=T),
      numericInput('age','Age:',value = NULL,min=18,max = 64),
      numericInput('symptomDay','How long has the patient been symptomatic? :',value = NULL,min=0,max = 4),
      radioButtons('vaccine','Has the patient had a vaccine in the last 12 months? :',c('No','Yes'), selected = character(0), inline=T)
    )

  })

  # call the server part
  # check_credentials returns a function to authenticate users
  res_auth <- callModule(
    module = auth_server,
    id = 'auth',
    check_credentials = check_credentials(credentials)
  )

  # show the login info
  output$auth_output <- renderText({
    req(!is.null(res_auth$user_info))
    addLog(user = res_auth$user_info[,'user'],site = res_auth$user_info[,'site'])
  })

  #check if validate.button is clicked
  observeEvent(input$validate.button,{
    values$site <- res_auth$user_info[,'site']
    values$user <- res_auth$user_info[,'user']

    values$site.file <- paste0('rand-',values$site,'.csv')
    values$data.file <- paste0('data-',values$site,'.csv')
    values$PID <- input$PID

    values$age <- input$age
    values$sex <- input$sex

    values$symptomDay <-input$symptomDay
    values$vaccine <-input$vaccine

    ## check if the inputs are ok
    values$check.screenID <- check_screening_ID(input$PID)
    values$check.age <- check_age(input$age)
    values$check.symptomDay <- check_symptomDay(input$symptomDay)
    values$check.sex <- check_selected(input$sex)
    values$check.vaccine <- check_selected(input$vaccine) & input$vaccine == 'No'

    if(values$check.screenID == T && values$check.age == T && values$check.symptomDay== T &&
       between(values$age,18,64) ==T && between(values$symptomDay,0,4) ==T &&
       values$check.sex ==T && values$check.vaccine == T){
      ask_confirmation(
        inputId = 'validate.confirm',
        type = 'warning',
        title = 'Please confirm your inputs:',
        text = tags$div(
          tags$p(),tags$b('Screening ID: ARS-'),values$site,'-SCR-',values$PID,
          tags$p(),tags$b('Age: '),values$age,tags$b('Sex: '),values$sex,
          tags$p(),tags$b('Days with symptoms: '),values$symptomDay,
          tags$p(),tags$b('Had RSV vaccine in last 12 months: '),values$vaccine,
          tags$p(),
          'Click OK to proceed or click Cancel to go back.'
        ),
        btn_labels = c('Cancel', 'OK'),
        btn_colors = c('#FE642E', '#04B404'),
        html = TRUE
      )
    }else{
      ask_confirmation(
        inputId = 'checking.input',
        type = 'error',
        title = 'Please check your inputs!',
        text = 'Screening ID must be 5 digits; \nage must be  ≥ 18 and <65 years; \n RSV symptoms duration between 0 and 4 days; \nNo RSV vaccination within the past 12 months.',
        btn_labels = c(NULL,'OK')
      )
    }

  })


  # check if the OK/Cancel button in validate window is clicked
  observeEvent(input$validate.confirm,{

    values$validate.confirm <- input$validate.confirm

    ## click ok after in the validate windows
    if(values$validate.confirm == TRUE){
      # check if patient has been randomized
      # matching pid/age/sex/site/user
      exist.check <- existQ(pid = values$PID,
                            age = values$age,
                            sex = values$sex,
                            site = values$site)

      values$exist.data <- exist.check$data
      values$exist.check <- exist.check$exist
      values$exist2.check <- exist.check$exist2

      treatment.lst <- getTreatment(values$site,values$PID)
      values$assigned <- treatment.lst$Assigned
      values$screeningID <- treatment.lst$screeningID
      values$randomizationID <- treatment.lst$randomizationID

      values$treatment <- treatment.lst$Treatment
      values$timestamp <- date()

      # if all inputs are matched with the existed one then show the information as well as the download button
      if(values$exist.check == TRUE){
        ask_confirmation(
          inputId = 'exist.info',
          type = 'success',
          title = 'This patient has been randomized!',
          text = tags$div(
            tags$b('Registered by: '),values$exist.data$user,
            tags$p(),tags$b('Screening ID: ARS-'),values$exist.data$site,'-SCR-',values$exist.data$screeningID,
            tags$p(),tags$b('Age: '),values$exist.data$age,tags$b('Sex: '),values$exist.data$sex,
            tags$p(),tags$b('Days of symptom: '),values$exist.data$symptomDay,
            tags$p(),tags$b('Had a vaccine in the last 12 months: '),values$exist.data$vaccine,
            tags$p(),
            tags$p(),tags$b('Subject No: ARS-'),values$exist.data$site,'-',
            sprintf('%03d', as.numeric(values$exist.data$randomizationID)),
            tags$p(),tags$b('Treatment: '),values$exist.data$Treatment,
            tags$p(),tags$b('Date and Time: '),values$exist.data$Date,tags$p(),
            tagList(downloadButton('downloadPDF','Download as PDF'))
          ),
          btn_labels = c(NULL, 'OK'),
          btn_colors = c('#04B404', '#04B404'),
          html = TRUE
        )
      }else{
        # only PID that match with the existed one
        if(values$exist2.check == TRUE && values$assigned == TRUE){

          ask_confirmation(
            inputId = 'exist2.info',
            type = 'warning',
            title = 'This patient has been randomized but the sex or age does not match with the existing record.',
            text = 'Please check your inputs.',
            btn_labels = c(NULL, 'OK'),
            btn_colors = c('#FE642E', '#04B404')
          )
        }else{
          # if PID is new then add
          if(values$assigned == FALSE){

            values$newdata <- addPID(site = values$site,
                                     screenID = values$PID,
                                     age = values$age,
                                     sex = values$sex,
                                     randID = values$randomizationID,
                                     treatment = values$treatment,
                                     symptomDay = values$symptomDay,
                                     vaccine = values$vaccine,
                                     file = values$data.file,
                                     timestamp = values$timestamp,
                                     user = res_auth$user
            )

            ask_confirmation(
              inputId = 'add.new',
              type = 'success',
              title = values$treatment,
              text = tags$div(
                tags$p(),tags$b('Screening ID: ARS-'),values$site,'-SCR-',values$PID,
                tags$p(),tags$b('Age: '),values$age,tags$b('Sex: '),values$sex,
                tags$p(),tags$b('Days of symptom: '),values$symptomDay,
                tags$p(),tags$b('Had a vaccine in the last 12 months: '),values$vaccine,
                tags$p(),
                tags$p(),tags$b('Subject No: ARS-'),values$site,'-',
                sprintf('%03d', as.numeric(values$randomizationID)),
                tags$p(),tags$b('Treatment: '),values$treatment,
                tags$p(),tags$b('Date and Time: '),values$timestamp,tags$p(),
                tagList(downloadButton('downloadPDF2','Download as PDF'))
              ),
              btn_labels = c(NULL, 'OK'),
              btn_colors = c('#FE642E', '#04B404'),
              html = TRUE
            )

          }

        }


      }## else


    }
  })

  # for reporting the existed
  output$downloadPDF <- downloadHandler(

    filename = function() {
      paste0('ARS-',values$site,'-SCR-',values$PID,'.pdf',sep='')
      #format(Sys.time(), ' %d-%b-%Y %H.%M.%S'), '.pdf')
    },
    #content in pdf file look more details in report.Rmd
    content = function(file) {
      #Use template form report-pdf.Rmd
      src <- normalizePath('report.Rmd')
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'report.Rmd')

      #render pdf doc by using report-pdf.Rmd template and latex engine is xelatex
      outpdf <- render(input = 'report.Rmd',
                       output_format = pdf_document(latex_engine = 'xelatex')
      )
      #rename file
      file.rename(outpdf, file)
    }
  )

  # for reporting the new PID
  output$downloadPDF2 <- downloadHandler(

    filename = function() {
      paste0('ARS-',values$site,'-SCR-',values$PID,'.pdf',sep='')
    },
    #content in pdf file look more details in report.Rmd
    content = function(file) {
      #Use template form report-pdf.Rmd
      src <- normalizePath('report2.Rmd')
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'report2.Rmd')

      #render pdf doc by using report-pdf.Rmd template and latex engine is xelatex
      outpdf <- render(input = 'report2.Rmd',
                       output_format = pdf_document(latex_engine = 'xelatex')
      )
      #rename file
      file.rename(outpdf, file)
    }
  )


  ### clear the input fields
  observeEvent(input$exist.info,{

    values$exist.info <- input$exist.info

    if(values$exist.info == FALSE){
      output$reset_input <- renderUI({
        div(
          numericInput('PID','Screening ID (1XXXX)',value = NULL),
          radioButtons('sex','Sex:',c('Male','Female'), selected = character(0), inline=T),
          numericInput('age','Age:',value = NULL,min=18,max = 64),
          numericInput('symptomDay','How long has the patient been symptomatic with RSV? :',value = NULL,min=0,max = 4),
          radioButtons('vaccine','Has the patient had a vaccine in the last 12 months? :',c('No','Yes'), selected = character(0), inline=T)        )
      })
    }

  })

  ### clear the input fields
  observeEvent(input$exist2.info,{

    values$exist2.info <- input$exist2.info

    if(values$exist2.info == FALSE){
      output$reset_input <- renderUI({
        div(
          numericInput('PID','Screening ID (1XXXX)',value = NULL),
          radioButtons('sex','Sex:',c('Male','Female'), selected = character(0), inline=T),
          numericInput('age','Age:',value = NULL,min=18,max = 64),
          numericInput('symptomDay','How long has the patient been symptomatic with RSV? :',value = NULL,min=0,max = 4),
          radioButtons('vaccine','Has the patient had a vaccine in the last 12 months? :',c('No','Yes'), selected = character(0), inline=T)        )
      })
    }

  })


  ### clear the input fields
  observeEvent(input$add.new,{

    values$add.new <- input$add.new

    if(values$add.new == FALSE){
      output$reset_input <- renderUI({
        div(
          numericInput('PID','Screening ID (1XXXX)',value = NULL),
          radioButtons('sex','Sex:',c('Male','Female'), selected = character(0), inline=T),
          numericInput('age','Age:',value = NULL,min=18,max = 64),
          numericInput('symptomDay','How long has the patient been symptomatic with RSV? :',value = NULL,min=0,max = 4),
          radioButtons('vaccine','Has the patient had a vaccine in the last 12 months? :',c('No','Yes'), selected = character(0), inline=T)        )
      })
    }

  })


}### end SERVER


# run the app
shinyApp(ui, server)

  "
  rmd_content1 <- "
---
title: \"Report\"
author: 'Requested by: `r res_auth$user`'
date: '`r date()`'
output: pdf_document
---

**Screening ID:** ARS-`r values$exist.data$site`-SCR-`r values$PID`

**Subject No:** ARS-`r values$exist.data$site`-`r sprintf(\"%03d\", as.numeric(values$exist.data$randomizationID))`

**Age:** `r values$exist.data$age`

**Sex:** `r values$exist.data$sex`

**Days of symptom:** `r values$exist.data$symptomDay`

**Given a vaccine in last 12 months:** `r values$exist.data$vaccine`

**Treatment:** `r values$exist.data$Treatment`

**Registered Date:** `r values$exist.data$Date`

**Registered Site:** `r values$exist.data$site`

**Registered by:** `r values$exist.data$user`
  "

  rmd_content2 <- "
  ---
title: \"Report2\"
author: 'Requested by: `r res_auth$user`'
date: '`r date()`'
output: pdf_document
---

**Screening ID: **ARS-`r values$site`-SCR-`r values$PID`

**Subject No: **ARS-`r values$site`-`r sprintf(\"%03d\", as.numeric(values$randomizationID))`

**Age: **`r values$age`

**Sex: **`r values$sex`

**Days of symptom: **`r values$symptomDay`

**Given a vaccine in last 12 months: **`r values$vaccine`

**Treatment: **`r values$treatment`

**Registered Date: **`r values$timestamp`

**Registered Site: **`r values$site`

**Registered by: **`r values$user`


  "
  if(login==F){

  }


  if(is.null(dir)){
    writeLines(shiny_app_code, paste0(name,".R"))
    writeLines(rmd_content1, paste0("report.Rmd"))
    writeLines(rmd_content2, paste0("report2.Rmd"))
    dir.create("www")
    file.copy(paste0(.libPaths()[1],"/WebTrials/pic/logo.png"),
              paste0(getwd(),"/www"))
    print(paste0())
    if(create_token){
      dropbox_token()
    }

  }
  else{
    if(dir.exists(dir)){
      writeLines(shiny_app_code, paste0(dir,"/",name,".R"))
      writeLines(rmd_content1, paste0(dir,"/","report.Rmd"))
      writeLines(rmd_content2, paste0(dir,"/","report2.Rmd"))
      dir.create(paste0(dir,"/www"))
      file.copy(paste0(.libPaths()[1],"/WebTrials/pic/logo.png"),
                paste0(dir,"/www"))
        if(create_token){
          dropbox_token(dir=dir)
        }
    }else{
      warning("Directory or folder isn't exists")
    }
  }
}

