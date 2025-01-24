setwd()
library(shiny)
library(dartR)
EMIBD9 <- readRDS(file = "emibd9_full.rds")
full_recode <- readRDS(file = "full_recode.rds")
og_fixed <- readRDS(file = "og_fixed.rds")
og_pedigree <- readxl::read_excel(path = "Full_colony_pedigree_15Oct.xlsx") %>%
  as.data.frame()


# Compares two pedigrees passed through recode_embid9 
ped_compare <- function(ped1, ped2, whole_df = F ,selected.ind = NA, listed.ind = NA){
  
  overlap <- rownames(ped2[which(rownames(ped2) %in% rownames(ped1))])
  
  stripped.ped1 <- ped1[overlap,overlap]
  stripped.ped2 <- ped2[rownames(stripped.ped1), colnames(stripped.ped1)]
  
  mismatchs <- stripped.ped1
  
  for(i in 1:ncol(stripped.ped1)){
    for(j in 1:nrow(stripped.ped1)){
      if(stripped.ped1[i,j] == stripped.ped2[i,j]){
        mismatchs[i,j] <- "M"
      }else{
        mismatchs[i,j] <- "MisM"
      }
      
    }
  }
  
  if(whole_df == T){
    return(mismatchs)
  }
  
  if(is.na(selected.ind)==F){
    select_char <- selected.ind
    df <- cbind(as.vector(stripped.ped1[select_char,]),
                as.vector(stripped.ped2[select_char,]), 
                as.vector(mismatchs[select_char,]))
    colnames(df) <- c("Ped1","Ped2","Mismatches")
    return(df)
    
  }else{
    
    df <- mismatchs[listed.ind, listed.ind]
    rownames(df) <- listed.ind
     
    
    return(df)  
  }
  
}

recode.trunc <- readRDS(file = "recode_trunc.rds")

og_pedigree[which(rownames(og_pedigree) %in% rownames(recode.trunc))]

pit_names <- rownames(og_fixed)

trial1 <- recode.trunc[rownames(og_fixed),rownames(og_fixed)]
trial2 <- og_fixed
tab.choice <- c("Head","Full")



ui <- fluidPage(
  navlistPanel(
    id = "tabset",
    tabPanel("Individuals",
              selectInput("code", "Individual", choices = pit_names),
              radioButtons("head", choices= tab.choice, label=""),
             tableOutput("ped_compare")
    ),
    tabPanel("Groups", 
             fluidPage(
               fluidRow(
                 column(8,
                    textInput("indlist", "Type individuals (must be separated by comma)", 
                          placeholder = NA)
                  ),
                 column(2, 
                    actionButton("go", "Run"),
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
                                       input$code, listed.ind = NA, whole_df=F))
  output$ped_compare <- renderTable(
    if (input$head == "Head"){
      head(selected.ind())
    }else{
      selected.ind()
    }
  )
  
  listed.sep <- eventReactive(input$go, {
    strsplit(gsub(" ", "", input$indlist),",")[[1]]
  })
  
  ped1 <- reactive(trial1[listed.sep(),listed.sep()])
  ped2 <- reactive(trial2[listed.sep(),listed.sep()])
  
  
  select.group <- reactive(ped_compare(trial1,trial2, selected.ind = NA, 
                                       listed.ind = listed.sep(), whole_df = F))
  
  output$ped1 <- renderTable({
    data = ped1()
  }, rownames = T, caption="EMBID9")
  
  output$ped2 <- renderTable({
    data = ped2()
  }, rownames = T, caption="Stud book")
  
  output$multipedcomp <- renderTable({
    data=select.group()
  }, rownames=T, caption = "Mismatches")
  
}

shinyApp(ui, server)











