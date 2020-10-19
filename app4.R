library(shiny)
library(rsconnect)
library(shinydashboard)
library("googlesheets4")
library("DT")
library(ggplot2)
library(dplyr)

# UI is what is seen or created in the interface
ui <- tagList(
#navAppend is a created java script which creates the order of the tabs 
  tags$head(includeScript("navAppend.js")),
#sets the type of navigation type desires, in our case tabsets were chosen and made fluid (edit)
  navbarPage(
    title = "DBT ADHERE",
    fluid = TRUE, 
    collapsible = TRUE,
    
    tabPanel("Home", 
             includeHTML("html/index.html")
    ),
    
    tabPanel("What is Adherence?",
             includeHTML("html/what_is_adherence_tab.html")
    ),
    
    
    
    tabPanel("Adherence Questions",
             titlePanel("Adherence Questions"),
             includeHTML("html/adherence_questions.html"),
              div(
               id = "form",
               textInput("ID", "Therapist ID", ""),
               textInput("pt", "Patient ID"),
               textInput("session", "Session Number"),
               sliderInput("question_1", "Therapist collaboratively set an agenda that accounted for treatment hierarchy (e.g., suicidal behavior, life-threatening behavior, quality-of-life-interfering behavior).", 0, 3, 0, ticks = FALSE),
               sliderInput("question_2", "Therapist balanced treatment strategies moving between acceptance and change.", 0, 3, 0, ticks = FALSE),
               sliderInput("question_3", "Therapist models dialectical thinking by looking for what's left out, highlighting polarities, finding synthesis.", 0, 3, 0, ticks = FALSE),
               sliderInput("question_4", "Therapist uses reciprocal communication strategies by acting towards the client in a genuine manner, that is attentive, and demonstrates engagement in client's agenda.", 0, 3, 0, ticks = FALSE),
               sliderInput("question_5", "Therapist discussed maladaptive behavior in a matter-of-fact manner.", 0, 3, 0, ticks = FALSE),
               sliderInput("question_6", "Therapist accurately reflects client behavior as method of demonstrating understanding and to clarify analysis.", 0, 3, 0, ticks = FALSE),
               sliderInput("question_7", "Therapist searches for and articulates how the clients responses make sense given the current circumstances described.", 0, 3, 0, ticks = FALSE),
               sliderInput("question_8", "Therapist conducts a behavioral analysis of problems with behaviorally specific language (thoughts, emotions, behaviors) and conducts a chain analysis as needed.", 0, 3, 0, ticks = FALSE),
               sliderInput("question_9", "Therapist highlights and describes behavioral patterns of client behavior consistent with DBT theory and presented as hypotheses describing functional relationship.", 0, 3, 0, ticks = FALSE),
               sliderInput("question_10", "Therapist conducts a solution analysis where therapist and client brainstorm solution, evaluate, choose, and then trouble-shoot solutions using behavioral language.", 0, 3, 0, ticks = FALSE),
               sliderInput("question_11", "Therapist used sesscion-ending strategies including ending with sufficient time.", 0, 3, 0, ticks = FALSE),
              
               actionButton("submit", "Submit", class = "btn-primary")
             )),
  
    tabPanel("Analysis",
            titlePanel("Analysis of Results"),
            selectInput(
                "Therapist",label = "Choose Therapist",
               choices =  c("A",
                     "B",
                     "C",
                     "D")),
            selectInput(inputId ="Patient", label = "Choose Patient",
              choices=c("",1:100)),
            
              # Output: Table summarizing the values entered ----
              plotOutput("Q1"),
              plotOutput("Q2"),
              plotOutput("Q3"),
              plotOutput("Q4"),
              plotOutput("Q5"),
              plotOutput("Q6"),
              plotOutput("Q7"),
              plotOutput("Q8"),
              plotOutput("Q9"),
              plotOutput("Q10"),
              plotOutput("Q11")
              )),
)

server <- function(input, output, session) {
  gs4_deauth()
  gs4_auth(
    cache = ".secrets",
    email = "docstephen44@gmail.com"
  )
  
  formData <- reactive({
    data.frame(
      ID = c(input$ID),
      patient = c(input$pt),
      session = c(input$session),
      q_1 = c(input$question_1),
      q_2 = c(input$question_2),
      q_3 = c(input$question_3),
      q_4 = c(input$question_4),
      q_5 = c(input$question_5),
      q_6 = c(input$question_6),
      q_7 = c(input$question_7),
      q_8 = c(input$question_8),
      q_9 = c(input$question_9),
      q_10 = c(input$question_10),
      q_11 = c(input$question_11)
    )
  })
  saveData <- function(data) {
    sheet_append("https://docs.google.com/spreadsheets/d/1VHeKLdFT6q4p0Gt-GhLQxVPZkpa05GmEM5YlmTbY72A/edit#gid=0",
                 formData(),
                 sheet = 1)
  }
  
  observeEvent(input$submit, {
    saveData(formData)
  })
  fulldata1<-read_sheet("https://docs.google.com/spreadsheets/d/1VHeKLdFT6q4p0Gt-GhLQxVPZkpa05GmEM5YlmTbY72A/edit#gid=0")
  
  fulldata1$Session<-unlist(fulldata1$Session)
  fulldata1$Therapist<-unlist(fulldata1$Therapist)
  fulldata1$Patient<-unlist(fulldata1$Patient)

 observe({
    
    therpatients<-sort(unique(fulldata1$Patient[fulldata1$Therapist==input$Therapist]))
    updateSelectInput(session=session, inputId = "Patient", choices=c("",therpatients))
  })
  
  selectedT<-reactive({fulldata1 %>% dplyr::filter(Therapist == input$Therapist)
  })
  pt<-reactive({ ppt<-selectedT() %>% dplyr::filter(Patient == as.numeric(input$Patient))
  
  })
   
  output$Q1 <- renderPlot({
    
    trial1<- ggplot(pt(), aes(x=pt()$Session, y=pt()$Q1))+
      geom_jitter(alpha=.3, width=.05, height=.05, color= "blue")+
      stat_summary(aes(x = pt()$Session, y= pt()$Q1), fun= mean, color= "red")+
      xlab("Session") +
      ylab("Hieracrchy") +
      ggtitle("Therapist Hierarchy over Session") 
    trial1
  })

output$Q2 <- renderPlot({
      trial2<- ggplot(pt(), aes(x=Session, y=Q2))+
        geom_jitter(alpha=.3, width=.05, height=.05, color= "blue")+
        stat_summary(aes(x = Session, y= Q2), fun= mean, color= "red")+
        xlab("Session") +
        ylab("Dialectics") +
        ggtitle("Therapist Dialectics over Session") 
      trial2
})
  
output$Q3 <- renderPlot({
  trial3<- ggplot(pt(), aes(x=Session, y=Q3))+
    geom_jitter(alpha=.3, width=.05, height=.05, color= "blue")+
    stat_summary(aes(x = Session, y= Q3), fun= mean, color= "red")+
    xlab("Session") +
    ylab("Dialectical Thinking") +
    ggtitle("Therapist Dialectical Thinking over Session") 
  trial3
})

output$Q4 <- renderPlot({
  trial4<- ggplot(pt(), aes(x=Session, y=Q4))+
    geom_jitter(alpha=.3, width=.05, height=.05, color= "blue")+
    stat_summary(aes(x = Session, y= Q4), fun= mean, color= "red")+
    xlab("Session") +
    ylab("Reciprocal Communication") +
    ggtitle("Therapist Reciprocal Communication over Session") 
  trial4
})
output$Q5 <- renderPlot({
  trial5<- ggplot(pt(), aes(x=Session, y=Q5))+
    geom_jitter(alpha=.3, width=.05, height=.05, color= "blue")+
    stat_summary(aes(x = Session, y= Q5), fun= mean, color= "red")+
    xlab("Session") +
    ylab("Irreverent Communication") +
    ggtitle("Therapist Irreverent Communication over Session") 
  trial5
})
output$Q6 <- renderPlot({
  trial6<- ggplot(pt(), aes(x=Session, y=Q6))+
    geom_jitter(alpha=.3, width=.05, height=.05, color= "blue")+
    stat_summary(aes(x = Session, y= Q6), fun= mean, color= "red")+
    xlab("Session") +
    ylab("V2") +
    ggtitle("Therapist V2 over Session") 
  trial6
})
output$Q7 <- renderPlot({
  trial7<- ggplot(pt(), aes(x=Session, y=Q7))+
    geom_jitter(alpha=.3, width=.05, height=.05, color= "blue")+
    stat_summary(aes(x = Session, y= Q7), fun= mean, color= "red")+
    xlab("Session") +
    ylab("V5") +
    ggtitle("Therapist V5 over Session") 
  trial7
})
output$Q8 <- renderPlot({
  trial8<- ggplot(pt(), aes(x=Session, y=Q8))+
    geom_jitter(alpha=.3, width=.05, height=.05, color= "blue")+
    stat_summary(aes(x = Session, y= Q8), fun= mean, color= "red")+
    xlab("Session") +
    ylab("Behavioral Analysis") +
    ggtitle("Therapist Behavioral Analysis over Session") 
  trial8
})
output$Q9 <- renderPlot({
  trial9<- ggplot(pt(), aes(x=Session, y=Q9))+
    geom_jitter(alpha=.3, width=.05, height=.05, color= "blue")+
    stat_summary(aes(x = Session, y= Q9), fun= mean, color= "red")+
    xlab("Session") +
    ylab("Insight") +
    ggtitle("Therapist Insight over Session") 
  trial9
})
output$Q10 <- renderPlot({
  trial10<- ggplot(pt(), aes(x=Session, y=Q9))+
    geom_jitter(alpha=.3, width=.05, height=.05, color= "blue")+
    stat_summary(aes(x = Session, y= Q10), fun= mean, color= "red")+
    xlab("Session") +
    ylab("Solution Analysis") +
    ggtitle("Therapist Solution Analysis over Session") 
  trial10
})
output$Q11 <- renderPlot({
  trial11<- ggplot(pt(), aes(x=Session, y=Q11))+
    geom_jitter(alpha=.3, width=.05, height=.05, color= "blue")+
    stat_summary(aes(x = Session, y= Q11), fun= mean, color= "red")+
    xlab("Session") +
    ylab("Session Ending Strategies") +
    ggtitle("Therapist Session Ending Strategies over Session") 
  trial11
  
})
}
shinyApp(ui = ui, server = server)
