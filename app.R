
library(shiny)
library(ggplot2)

data <- read.csv("student_performance.csv")

ui <- fluidPage(
  titlePanel("Student Performance Analysis Using Discrete Mathematics"),
  sidebarLayout(
    sidebarPanel(
      h3("Enter Student Details"),
      numericInput("attendance","Attendance (%)",85,min=0,max=100),
      numericInput("study","Study Hours per Day",5,min=0,max=24),
      numericInput("screen","Screen Time per Day",4,min=0,max=24),
      numericInput("sleep","Sleep Hours",7,min=0,max=24),
      numericInput("physical","Physical Health (1-5)",4,min=1,max=5),
      numericInput("mental","Mental Well-being (1-5)",4,min=1,max=5),
      numericInput("activities","Extra Activities",2,min=0,max=10),
      numericInput("assignment","Assignment Completion (%)",85,min=0,max=100),
      numericInput("previous","Previous CGPA",8,min=0,max=10),
      br(),
      actionButton("analyze","Analyze Performance"),
      br(),
      br(),
      actionButton("save","Save Student")
    ),
    mainPanel(
      h2("Performance Result"),
      textOutput("studentID"),
      textOutput("cgpa"),
      textOutput("performance"),
      br(),
      h3("Comparison with Previous Students"),
      tableOutput("comparison"),
      br(),
      h3("Relationship Analysis"),
      plotOutput("relationshipPlot",height="400px"),
      br(),
      h3("Discrete Mathematics"),
      textOutput("sets"),
      br(),
      textOutput("relation"),
      br(),
      textOutput("logic"),
      br(),
      textOutput("functionResult")
    )
  )
)

server <- function(input,output,session) {

  analyzed_student <- eventReactive(input$analyze, {

    model <- lm(
      CGPA ~ Attendance + Study_Hours + Screen_Time + Sleep_Hours +
        Physical_Health + Mental_Wellbeing + Extra_Activities +
        Assignment_Completion + Previous_CGPA,
      data=data
    )

    student <- data.frame(
      Attendance=input$attendance,
      Study_Hours=input$study,
      Screen_Time=input$screen,
      Sleep_Hours=input$sleep,
      Physical_Health=input$physical,
      Mental_Wellbeing=input$mental,
      Extra_Activities=input$activities,
      Assignment_Completion=input$assignment,
      Previous_CGPA=input$previous
    )

    prediction <- predict(model,newdata=student)

    list(
      student=student,
      prediction=as.numeric(prediction)
    )
  })

  output$studentID <- renderText({
    req(analyzed_student())
    paste("Student ID:","S",sprintf("%03d",nrow(data)+1),sep="")
  })

  output$cgpa <- renderText({
    req(analyzed_student())
    paste("Estimated CGPA:",round(analyzed_student()$prediction,2))
  })

  output$performance <- renderText({
    req(analyzed_student())
    cgpa <- analyzed_student()$prediction

    if(cgpa>=8.5) {
      "Performance: EXCELLENT"
    } else if(cgpa>=7.5) {
      "Performance: GOOD"
    } else if(cgpa>=6.5) {
      "Performance: AVERAGE"
    } else {
      "Performance: NEEDS IMPROVEMENT"
    }
  })

  output$comparison <- renderTable({
    req(analyzed_student())

    data.frame(
      Factor=c(
        "Attendance",
        "Study Hours",
        "Screen Time",
        "Sleep Hours",
        "Physical Health",
        "Mental Well-being",
        "Extra Activities",
        "Assignment Completion"
      ),
      Student=c(
        input$attendance,
        input$study,
        input$screen,
        input$sleep,
        input$physical,
        input$mental,
        input$activities,
        input$assignment
      ),
      Dataset_Average=c(
        round(mean(data$Attendance),2),
        round(mean(data$Study_Hours),2),
        round(mean(data$Screen_Time),2),
        round(mean(data$Sleep_Hours),2),
        round(mean(data$Physical_Health),2),
        round(mean(data$Mental_Wellbeing),2),
        round(mean(data$Extra_Activities),2),
        round(mean(data$Assignment_Completion),2)
      )
    )
  })

  output$relationshipPlot <- renderPlot({
    req(analyzed_student())

    correlations <- c(
      Attendance=cor(data$Attendance,data$CGPA),
      Study_Hours=cor(data$Study_Hours,data$CGPA),
      Screen_Time=cor(data$Screen_Time,data$CGPA),
      Sleep_Hours=cor(data$Sleep_Hours,data$CGPA),
      Physical_Health=cor(data$Physical_Health,data$CGPA),
      Mental_Wellbeing=cor(data$Mental_Wellbeing,data$CGPA),
      Extra_Activities=cor(data$Extra_Activities,data$CGPA),
      Assignment_Completion=cor(data$Assignment_Completion,data$CGPA)
    )

    plot_data <- data.frame(
      Factor=names(correlations),
      Correlation=as.numeric(correlations)
    )

    ggplot(plot_data,aes(x=Factor,y=Correlation))+
      geom_col()+
      labs(
        title="Relationship Between Factors and CGPA",
        x="Factor",
        y="Correlation with CGPA"
      )+
      theme(axis.text.x=element_text(angle=45,hjust=1))
  })

  output$sets <- renderText({
    req(analyzed_student())

    high_cgpa <- sum(data$CGPA>=8.5)
    high_attendance <- sum(data$Attendance>=85)
    intersection <- sum(data$CGPA>=8.5 & data$Attendance>=85)

    paste(
      "SET THEORY: Set A = CGPA >= 8.5:",
      high_cgpa,
      "| Set B = Attendance >= 85%:",
      high_attendance,
      "| A intersection B:",
      intersection
    )
  })

  output$relation <- renderText({
    req(analyzed_student())
    "RELATION: Student performance factors are related to the student's CGPA."
  })

  output$logic <- renderText({
    req(analyzed_student())

    if(
      input$attendance>=85 &&
      input$study>=5 &&
      input$screen<=5
    ) {
      "LOGIC: TRUE - Attendance >= 85 AND Study Hours >= 5 AND Screen Time <= 5."
    } else {
      "LOGIC: FALSE - The student does not satisfy all conditions."
    }
  })

  output$functionResult <- renderText({
    req(analyzed_student())

    cgpa <- analyzed_student()$prediction

    if(cgpa>=8.5) {
      "FUNCTION: CGPA -> EXCELLENT"
    } else if(cgpa>=7.5) {
      "FUNCTION: CGPA -> GOOD"
    } else if(cgpa>=6.5) {
      "FUNCTION: CGPA -> AVERAGE"
    } else {
      "FUNCTION: CGPA -> NEEDS IMPROVEMENT"
    }
  })

  observeEvent(input$save, {

    req(analyzed_student())

    student <- analyzed_student()$student
    prediction <- analyzed_student()$prediction

    new_student <- data.frame(
      Student_ID=paste0("S",sprintf("%03d",nrow(data)+1)),
      Attendance=student$Attendance,
      Study_Hours=student$Study_Hours,
      Screen_Time=student$Screen_Time,
      Sleep_Hours=student$Sleep_Hours,
      Physical_Health=student$Physical_Health,
      Mental_Wellbeing=student$Mental_Wellbeing,
      Extra_Activities=student$Extra_Activities,
      Assignment_Completion=student$Assignment_Completion,
      Previous_CGPA=student$Previous_CGPA,
      CGPA=round(prediction,2)
    )

    data <<- rbind(data,new_student)

    write.csv(
      data,
      "student_performance.csv",
      row.names=FALSE,
      quote=FALSE
    )

    showNotification(
      paste(new_student$Student_ID,"saved successfully!"),
      type="message"
    )
  })
}

shinyApp(
  ui=ui,
  server=server,
  options=list(launch.browser=TRUE)
)