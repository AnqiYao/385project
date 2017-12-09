#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(scales)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Predict your employee's turnover"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        sliderInput(inputId = "sl", label = h5("Enter satisfaction level"), 
                    min = 0.09, max = 1, value = 0.09),
        sliderInput(inputId = "tsc", label = h5("Number of years in company"), 
                    min = 2, max = 10, value = 2),
        sliderInput(inputId = "amh", label = h5("Average monthly hour"), 
                    min = 96, max = 310, value = 96),
        sliderInput(inputId = "le", label = h5("Grade your employee"), 
                    min = 0.36, max = 1, value = 0.36),
        sliderInput(inputId = "np", label = h5("Number of projects"), 
                    min = 2, max = 7, value = 2),
        submitButton(text = "Go to predict")
      ),
    # Show a plot of the generated distribution
      mainPanel(
        h3("This employee has a turnover rate of:"),
        verbatimTextOutput("rate"),
        h3("Want to keep your employee? Try:"),
        verbatimTextOutput("tip")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  active_input = reactive({
    req(input$sl)
    req(input$tsc)
    req(input$amh)
    req(input$le)
    req(input$np)
  })
  predict_rate = function(a,b,c,d,e){
    if(a == 0.09 && b==2 && c ==96 && d==0.36 && e ==2){
      return(" Let's make a prediction!")
    }
    if (a>=0.46){
      if(b>=4.5){
        if(d>=0.81){
          if(c>=214){
            if(b<6.5){
              return(percent(499/538))
            }else{
              return(percent(0))
            }
          }else{
            return(percent(1-119/128))
          }
        }else{
          return(percent(1-451/475))
        }
      }else{
        return(percent(1-5233/5312))
      }
    }else{
      if(e>=2.5){
        if(a>=0.11){
          return(percent(1-881/949))
        }else return(percent(534/534)) 
      }else{
        if(d>=0.57){
          return(percent(1-75/80))
        }else{
          if(d<0.45){
            return(percent(1-22/22))
          }else return(percent(898/930))
        }
      }
    }
  }
  predict_tip = function(a,b,c,d,e){
    if(a == 0.09 && b==2 && c ==96 && d==0.36 && e ==2){
      return(" Let's make a prediction!")
    }
    if (a>=0.46){
      if(b>=4.5){
        if(d>=0.81){
          if(c>=214){
            if(b<6.5){
              return(c(" REDUCE the employee's WORKLOAD.\n", "Let's have a break!"))
            }else{
              return("CONGRATULATIONS! You won't lose your man.")
            }
          }else{
            return(c(" NICE! Your employee is not very likely to leave.\n","Treat your OLD RELIABLE."))
          }
        }else{
          return(c(" NICE! Your employee is not very likely to leave.\n","Your employee needs your ENCOURAGEMENT."))
        }
      }else{
        return(c(" NICE! Your employee is not very likely to leave.\n","May your employee stay with you."))
      }
    }else{
      if(e>=2.5){
        if(a>=0.11){
          return(c(" NICE! Your employee is not very likely to leave.\n","Try to increase your employee's satisfaction level.\n","Maybe a little Christmax gift?"))
        }else return(c(" SORRY... Your employee is not happy to stay here.\n","Good luck...")) 
      }else{
        if(d>=0.57){
          return(c(" NICE! Your employee is not very likely to leave.\n","Keep going!"))
        }else{
          if(d<0.45){
            return("CONGRATULATIONS! You won't lose your man.")
          }else return(c(" SORRY... You may lose a good employee.\n","Your employee needs more attention.\n", "Get him more involved."))
        }
      }
    }
  }
  output$rate = renderText({
    predict_rate(input$sl, input$tsc, input$amh, input$le, input$np)
  })
  output$tip = renderText({
    predict_tip(input$sl, input$tsc, input$amh, input$le, input$np)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

