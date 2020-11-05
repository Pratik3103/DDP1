library(shiny)
library(datasets)

mghectordata <- mtcars
mghectordata$am <- factor(mghectordata$am, labels = c("Aato", "Manewal"))

shinyServer(function(input, output) {
    
    fTxt <- reactive({
        paste("mpg ~", input$variable)
    })
    
    fTP <- reactive({
        paste("mpg ~", "as.integer(", input$variable, ")")
    })
    
    fat <- reactive({
        lm(as.formula(fTP()), data=mghectordata)
    })
    
    out$cap <- renderText({
        fTxt()
    })
    
    out$mpgBP <- renderPlot({
        boxplot(as.formula(fTxt()), 
                data = mghectordata,
                outline = input$outliers)
    })
    
    out$ft <- renderPrint({
        summary(fat())
    })
    
    out$mpgP <- renderPlot({
        with(mghectordata, {
            plot(as.formula(fTP()))
            abline(fat(), col=2)
        })
    })
    
})