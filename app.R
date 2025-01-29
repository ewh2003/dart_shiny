setwd(getwd())
library(shiny)
library(dartR)
library(openxlsx)
EMIBD9 <- readRDS(file = "emibd9_full.rds")
full_recode <- readRDS(file = "full_recode.rds")
og_fixed <- readRDS(file = "og_fixed.rds")
og_pedigree <- readxl::read_excel(path = "Full_colony_pedigree_15Oct.xlsx") %>%
  as.data.frame()

matches <- c("I", "FS/PO","HS","1C","HC","2C","H2C","3C","U")

# Compares two pedigrees passed through recode_embid9 
ped_compare <- function(ped1, ped2, whole_df = F ,selected.ind = NA, listed.ind = NA,mm.deg = F,just.mism =F){
  
  # Direct comparison of pedigrees
  overlap <- rownames(ped2[which(rownames(ped2) %in% rownames(ped1))])
  
  matches <- c("I","FS/PO","HS","1C","HC","2C","H2C","3C","U")
  
  stripped.ped1 <- ped1[overlap,overlap]
  stripped.ped2 <- ped2[rownames(stripped.ped1), colnames(stripped.ped1)] %>%
    as.matrix()
  
  mismatchs <- stripped.ped1
  degree.mm <- stripped.ped1

  
  for(i in 1:nrow(stripped.ped1)){
    for(j in 1:ncol(stripped.ped1)){
      if(stripped.ped1[i,j] == stripped.ped2[i,j]){
        mismatchs[i,j] <- "M"
        degree.mm[i,j] <- 0
      }else{
        st2 <- which(matches %in% stripped.ped2[i,j])[1]
        mismatchs[i,j] <- "MisM"
        degree.mm[i,j] <- abs(which(matches%in% stripped.ped1[i,j]) - which(matches %in% stripped.ped2[i,j])[1])
      }
      
    }
  }
  
  if(whole_df==T & mm.deg==T){
    df <- mismatchs
    return(list(df, degree.mm))
  }else if(whole_df == T & mm.deg==F){
    return(mismatchs)
  }

  
  # Return df of list of individuals
  if(is.na(selected.ind)==F){
    select_char <- selected.ind
    df <- cbind(as.vector(rownames(stripped.ped1)),
                as.vector(stripped.ped1[select_char,]),
                as.vector(stripped.ped2[select_char,]), 
                as.vector(mismatchs[select_char,]), 
                as.vector(degree.mm[select_char,]))
    colnames(df) <- c("ID","Ped1","Ped2","Mismatches","Degree MM")
    return(df)
  } else{
    
    df <- mismatchs[listed.ind, listed.ind]
    rownames(df) <- listed.ind
    degree.select <- degree.mm[listed.ind,listed.ind]
    
    
    return(list(df,degree.select))  
  }
}

rownames(trial2)


sdf[[1]][-which(rownames(sdf[[1]]) == "P_82008")]

rownames(sdf[[1]])[-which(rownames(sdf[[1]]) == "P_82008")]

recode.trunc <- readRDS(file = "recode_trunc.rds")


pit_names <- rownames(og_fixed)

trial1 <- recode.trunc[rownames(og_fixed),rownames(og_fixed)]
trial2 <- og_fixed

tab.choice <- c("Head","Full")


ui <- fluidPage(
  navlistPanel(
    id = "tabset",
    tabPanel("Individuals",
              selectInput("code", "Individual", choices = pit_names),
             fluidRow(
               column(6,
                      radioButtons("head", choices= tab.choice, label="") 
                ),
               column(6,
                      checkboxInput("justmm","Just Mismatches?", value = F)
                      )
             ),
             tableOutput("ped_compare")
    ),
    tabPanel("Groups", 
             fluidPage(
               fluidRow(
                 column(8,
                    selectizeInput("indlist", "Type individuals (must be separated by comma)", 
                          multiple = T, choices=rownames(trial1))
                  ),
                 column(2, 
                    actionButton("go", "Run"),
                  ), 
               ), 
               fluidRow(
                 column(4,
                        radioButtons("dfchoice","", choices = c("Simple df", "Degree of MM"))
                        ), 
                 column(4, 
                        downloadButton("download", "Download .xslx")
                        ), 
                 column(4, 
                        textInput("dataname","Datasetname")
                        )
                  ),
               fluidRow(
                 column(6,
                        tableOutput("ped1")
                  )
                 ), 
              fluidRow(
                 column(6, 
                        tableOutput("ped2")
                  )
               ),
               fluidRow(
                 column(6,
                 tableOutput("multipedcomp")
                 )
               )
  )
)
)
)

server <- function(input, output, session) {
  selected.ind <- reactive(ped_compare(trial1, trial2, 
                                           input$code, listed.ind = NA, whole_df=F, just.mism = input$justmm))
 

  output$ped_compare <- renderTable(
    if(input$justmm == T){
    if (input$head == "Head"){
      head(as.data.frame(selected.ind())[as.data.frame(selected.ind())["Mismatches"]=="MisM",])
    }else{
      as.data.frame(selected.ind())[as.data.frame(selected.ind())["Mismatches"]=="MisM",]
    }}else{
      if (input$head == "Head"){
        head(selected.ind())
      }else{
        selected.ind()
    }})

  
  
  listed.sep <- eventReactive(input$go, {
    as.vector(input$indlist)
  })
  
  
  ped1 <- reactive(trial1[listed.sep(),listed.sep()])
  ped2 <- reactive(trial2[listed.sep(),listed.sep()])
  
  
  select.group <- reactive(ped_compare(trial1,trial2, selected.ind = NA, 
                                listed.ind = listed.sep(), whole_df = F, mm.deg = F))

  
  output$ped1 <- renderTable({
    data = ped1()
  }, rownames = T, caption="EMBID9")
  
  output$ped2 <- renderTable({
    data = ped2()
  }, rownames = T, caption="Stud book")
  
  
  output$multipedcomp <- renderTable(
    if (input$dfchoice == "Simple df"){
      cbind(colnames(select.group()[[1]]),select.group()[[1]])
    }else{
      cbind(colnames(select.group()[[2]]),select.group()[[2]])
    }
  )
  
  output$download <- downloadHandler(
    filename = function() {
      paste0(input$dataname, ".xlsx")
    },
    content = function(file) {
      write.xlsx(list(ped1(),ped2(),cbind(colnames(select.group()[[2]]),select.group()[[2]])),sheetName=c("Studbook","EMIBD9","Mismatches")
                 ,file = file)
    }
  )


}

shinyApp(ui, server)















