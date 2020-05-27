library(shiny)

## text message ----

text1 <- tags$div(
        tags$p("This dashboard was initially motivated by the book of Wolfgang Streeck 'Buying Time: The delayed crisis of democratic capitalism'. In his book, Streeck argues that governments are 'buying time' through inflation, public debt and private indebtedness which masks any potential issue of societal disruption. When looking at the absolute amount that countries borrow, the driving forces behind the European Union (e.g. Germany, France and until recently the United Kingdom) are those countries lending by far the largest sum of money. This in turn is masked by the reformulating of state debt in the form of the Gross Domestic Product of the country. This masking of the actual amount of state debt holds the hidden message that borrowing money is normal, as long as you are able to make 'enough' money'. The notion of earning money became inherently tied to borrowing more, thus creating a surplus. Countries excessively take this course of action, while simultaneously setting the example for all their citizens.
"), 
        br(),
        tags$p("Through this application, I invite you to further investigate the state debt of the countries within the European Union through two distinct lenses:
"),
        br(),
        tags$li("State debt in absolute Euros"),
        tags$li("State debt in GDP")
)
