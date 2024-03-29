library(data.table)
library(shiny)

ui <- fluidPage(
    includeHTML("www/page_header.html"),
    includeHTML("www/page_top.html"),
    includeCSS("www/style.css"),

    # Get functional threshold power and weight values 
    # - used tags over numericInput() to skip append of placeholder
    tags$input(name="ftp", placeholder="FTP (W)",    type="number", value=NA, min=1),
    tags$input(name="kg",  placeholder="Weight (kg)", type="number", value=NA, min=1),
    
    # Return the main table
    tableOutput("power_table"),
    
    # Return secondary outputs
    tagAppendAttributes(textOutput("ftp_wkg"), class="zp"),
    textOutput("sweetspot")#,
    #textOutput("zp_cat")
)

server <- function(input, output) {
    
    # constants
    Zone <- paste(7:1, c("Neuromuscular", "Anaerobic", "VO2 Max", "Threshold", 
                         "Tempo", "Endurance", "Recovery"), sep=": ")
    bounds <- c(NA, 1.5, 1.2, 1.05, 0.9, 0.75, 0.55, 0)

    # primary output - table of power zones
    output$power_table <- renderTable({
        
        # return nothing unless these values are present
        req(input$ftp, input$kg)
        
        # initiate table
        power_zones <- data.table(Zone,
                                  low_w=as.integer(bounds[-1]*input$ftp), 
                                  upp_w=as.integer(bounds[-length(bounds)]*input$ftp))
        
        # bounds for watts per kg
        power_zones[, low_wkg:=low_w/input$kg]
        power_zones[, upp_wkg:=upp_w/input$kg]
        
        # formatting
        power_zones[, Watts:=ifelse(is.na(upp_w), sprintf(">%.f", low_w-1), 
                                    ifelse(low_w==0, sprintf("<%.f", upp_w), 
                                           sprintf("%3.f-%3.f", low_w, upp_w-1)))]
        
        power_zones[, `W/kg`:=ifelse(is.na(upp_wkg), sprintf(">%.2f", low_wkg-0.01), 
                                     ifelse(low_wkg==0, sprintf("<%.2f", upp_wkg), 
                                            sprintf("%.2f-%.2f", low_wkg, upp_wkg-0.01)))]
        
        # output
        power_zones[, .(Zone, Watts, `W/kg`)]})
    
    # secondary output: w/kg at ftp
    output$ftp_wkg <- renderText({
        req(input$ftp, input$kg)
        sprintf("W/kg at FTP: %.2f", input$ftp/input$kg)
    })
    
    # secondary output: watts for sweetspot training
    output$sweetspot <- renderText({
        req(input$ftp, input$kg)
        sprintf("Sweetspot range (W): %.f - %.f", input$ftp*0.87, input$ftp*0.93)
    })

    # secondary output: 95% w/kg at ftp
    output$zp_cat <- renderText({
        req(input$ftp, input$kg)
        sprintf("95%% of FTP: %.2f", input$ftp/input$kg*0.95)
    })
}

shinyApp(ui=ui, server=server)