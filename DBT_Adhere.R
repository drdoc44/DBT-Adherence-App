library(shiny)
library(rsconnect)
library(shinydashboard)
library("googlesheets4")
library("DT")
library(ggplot2)
library(dplyr)
# UI is what is seen or created in the interface
ui <- tagList(
#navAppend.js is used to attach a HTML link to an outside link to one of the tabs in the tabset. In our case
# this is used to link to the outside HIPPA Compliant Video Therapy Recording Platform Panopto
#tag attaches an object to the user interface, in order.
  tags$head(includeScript("navAppend.js")),
#sets the type of navigation type desires, in our case tabsets were chosen and made fluid
#The Title can be changed here
#The parentheses are not closed directyly after navbarPage, this allows for tabs to be included
#in the navigation bar
  navbarPage(
    title = "DBT ADHERE",
    fluid = TRUE, 
    collapsible = TRUE,
  #This is the first Tab created as well as the default that loads.
  #Title can be changed here as well as content
  #For this app the body text is written and edited in HTML and CSS and included using the fxn includeHTML
  #This provides greater flexibility with appearance and broadens what can be done in r-shiny
    tabPanel("Home", 
             includeHTML("html/index.html")
    ),
    #Second Tab created similarly makes use of an included HTML file to create body text
    tabPanel("What is Adherence?",
             includeHTML("html/what_is_adherence_tab.html")
    ),
    
    
# Title Panel is used to create the space for an object to be input, in our case questions
#Div creates a container formatted to our specifications.
# In this case order, and uniformity
#Sliderinput is the input type chosen for response to question
#Questions can be edited here as well as range of responses accepted
#0-3 was the chosen range in this case, with the default being set to 0
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
 
 #Action button is used to create an action, in this case submitting the above responses             
               actionButton("submit", "Submit", class = "btn-primary")
             )),
  #This is the analysis tab which produces the graphs
  #Selectinput is used choose a therapist from a pre-populated list
  #This can be made as long as necessary, for this instance a 4 therapist pool was chosen
  #choices provides the available choices under this title, choose therapist
  #select input is used to also select the patient ID which can range from 1-999
  #Choices here is left blank becuase we use a reactive function in the server section
  # to populate client's based on the respective therapists as not each therapist may
  #have the same number or use the same numbers. This can be lengthened as necessary
    tabPanel("Analysis",
            titlePanel("Analysis of Results"),
            selectInput(
                "Therapist",label = "Choose Therapist",
               choices =  c("A",
                     "B",
                     "C",
                     "D")),
            selectInput(inputId ="Patient", label = "Choose Patient",
              choices=c("",1:999)),
            
              # Output: will render someinput based on specifer. In this case graphs are called for
              #One graph is called for based on each question, as each question represents a different 
              #criteria of adherence
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
#Server is where the backroom work takes place
#Reading data, creating graphs, creating datasets
#This is also where reactive expressions can occur
server <- function(input, output, session) {

#In this case the data is uploaded and downloaded from a google sheets link
#gs4_deauth() calls for any prior authorizations to an email to be cancelled
  gs4_deauth()
#This allows us to specify which email to provide authorization from
#A specific previously approved authorization key and email can be specified
#This can prevent the need for constant authentication checks
#More of an explanation on this can be found on the GITHUB
   gs4_auth(
    cache = ".secrets",
    email = "docstephen44@gmail.com"
  )
 #this creates a new set of data based on the input selections
#from the questions in the adherence question tab and organizes to
#match a existing columns in the data set, that exists on google drive
#This is considered a reactive form, as it will only create the dataframe
#if instructed by another factor, in this case the submit button
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
#The observeEvent will wait until it sees a specific event to work
#In this case, it waits until the submit button is pressed to save 
#the input from the questions
  observeEvent(input$submit, {
    saveData(formData)
    })
#This will save the new data to our existing dataset on 
#googledrive. The html link is a direct link to the existing sheet
  saveData <- function(data) {
    sheet_append("https://docs.google.com/spreadsheets/d/1VHeKLdFT6q4p0Gt-GhLQxVPZkpa05GmEM5YlmTbY72A/edit#gid=0",
                 formData(),
                 sheet = 1)
  }
#This reads our dataset from the drive. This will update periodically in order to have
#the most up to date information for analysis
   fulldata1<-read_sheet("https://docs.google.com/spreadsheets/d/1VHeKLdFT6q4p0Gt-GhLQxVPZkpa05GmEM5YlmTbY72A/edit#gid=0")
#These functions unlist the data and place them into a more readable form for further manipulation
  fulldata1$Session<-unlist(fulldata1$Session)
  fulldata1$Therapist<-unlist(fulldata1$Therapist)
  fulldata1$Patient<-unlist(fulldata1$Patient)
#These lines will update our selector in the UI to provide an up to date list of the patients for analysis
#based on selected therapist.
 observe({
    
    therpatients<-sort(unique(fulldata1$Patient[fulldata1$Therapist==input$Therapist]))
    updateSelectInput(session=session, inputId = "Patient", choices=c("",therpatients))
  })
#This will manipulate the data set to create a new data set consisting only of the specified
#therapists patients. Then based on that new data set will allow for further manipulation
#to create a data set for the specified patient
  selectedT<-reactive({fulldata1 %>% dplyr::filter(Therapist == input$Therapist)
  })
  pt<-reactive({ ppt<-selectedT() %>% dplyr::filter(Patient == as.numeric(input$Patient))
  
  })
# The following lines of code will create the plots presented in the UI
# Here due to the use of reactive expressions or expressions that can
# be constantly updated through user influence.
#graphs are made in order to show all scores across number of sessions
#for a particular patient. Furthermore it is parsed by question in order to have 
# a better understanding of the adherence criteria the question represents
#the geom_jitter function is used to provide a better visual of how each
# rater scored adherence. This is necessary due to the small scale used
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
