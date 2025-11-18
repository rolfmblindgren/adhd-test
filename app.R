library(shiny)
library(tibble)

# Spørsmål
items <- tibble(
  id = paste0("item", 1:10),
  tekst = c(
    "Jeg klarer vanligvis å starte på kjedelige oppgaver uten særlig friksjon.",
    "Jeg mister sjelden viktige ting, og finner dem raskt hvis det skjer.",
    "Jeg blir stort sett oppfattet som pålitelig når det gjelder avtaler og frister.",
    "Jeg klarer som regel å holde fokus på én oppgave til den er fullført.",
    "Jeg er ikke avhengig av kaos eller siste liten for å komme i gang.",
    "Jeg blir ikke så rastløs at jeg må bevege meg eller avbryte andre.",
    "Jeg glemmer ikke avtaler eller beskjeder i en grad som skaper problemer.",
    "Når noe er viktig, klarer jeg å organisere tiden så det får plass.",
    "Tankene løper ikke så ofte løpsk at det hindrer meg i hverdagen.",
    "Jeg trenger ikke uvanlig mye ytre struktur for å fungere greit."
  )
)

ui <- fluidPage(
  titlePanel("Dette er ikke en ADHD-test"),

  sidebarLayout(
    sidebarPanel(
      h4("Kort om testen"),
      p("Denne testen viser hvordan fravær av oppmerksomhets- og reguleringsvansker kan se ut."),
      p("Den er ikke diagnostisk. Den kan verken bekrefte eller avkrefte ADHD."),
      br(),
      actionButton("beregn", "Beregn resultat")
    ),

    mainPanel(
      tabsetPanel(
        tabPanel(
          "Spørsmål",
          br(),
          p("Dra i hver slider for å velge hvor godt utsagnet har stemt for deg over tid."),
          hr(),

lapply(seq_len(nrow(items)), function(i) {
  item <- items[i, ]

  div(style = "width: 100%; max-width: 600px;",   # slider + labels får felles ramme
      tagList(
        strong(item$tekst),

        div(style = "margin-bottom: 4px; font-size: 0.9em; color: #666; width: 100%;",
            HTML("
              <span style='display:inline-block; width:20%; text-align:left;'>Stemmer ikke</span>
              <span style='display:inline-block; width:19%; text-align:center;'>Litt</span>
              <span style='display:inline-block; width:19%; text-align:center;'>Delvis</span>
              <span style='display:inline-block; width:19%; text-align:center;'>Ganske</span>
              <span style='display:inline-block; width:20%; text-align:right;'>Stemmer helt</span>
            ")
        ),

        div(style = "width: 100%;",
            sliderInput(
              inputId = item$id,
              label = NULL,
              min = 1,
              max = 5,
              value = 3,
              step = 1,
              ticks = FALSE,
              width = "100%"
            )
        ),

        br()
      )
  )
})
        ),
        tabPanel(
          "Resultat",
          br(),
          h3("Tolkning"),
          textOutput("resultat_tekst"),
          br(),
          h4("Gjennomsnittsskår"),
          textOutput("score_tekst"),
          br(),
          h4("Forbehold"),
          p("En klinisk vurdering innebærer utviklingshistorie, funksjon og faglig skjønn."),
          p("Mennesker kan ha lav struktur eller høy fart uten at det handler om ADHD.")
        )
      )
    )
  )
)

server <- function(input, output, session) {

  score_reaktiv <- eventReactive(input$beregn, {

    # hent alle svar som liste
    svar <- lapply(items$id, function(id) input[[id]])

    # sjekk ubesvarte
    mangler <- vapply(svar, function(x) is.null(x) || is.na(x), logical(1))

    if (any(mangler)) {
      return(list(
        gyldig = FALSE,
        beskjed = "Du må svare på alle utsagn før resultatet kan beregnes.",
        score = NA
      ))
    }

    # flate ut og beregne
    svar_num <- as.numeric(unlist(svar))
    mean_score <- mean(svar_num)

    list(
      gyldig = TRUE,
      beskjed = NULL,
      score = mean_score
    )
  })

  output$score_tekst <- renderText({
    res <- score_reaktiv()
    if (!res$gyldig) return(res$beskjed)
    paste0(round(res$score, 2), " av 5")
  })

  output$resultat_tekst <- renderText({
    res <- score_reaktiv()
    if (!res$gyldig) return("")

    s <- res$score

    if (s >= 4.0) {
      "Svarene dine viser få trekk som ligner de reguleringsvanskene man ser ved ADHD. Hverdagen virker stabil og forutsigbar."
    } else if (s >= 3.2) {
      "Mønsteret ditt ligger godt innenfor normal variasjon: noen styrker, litt friksjon, men ingenting som peker klart i én retning."
    } else if (s >= 2.5) {
      "Du rapporterer en del trekk som kan minne om ADHD, men dette kan like gjerne handle om personlighet, vaner eller livssituasjon."
    } else {
      "Du beskriver flere områder som ofte skaper vansker i ADHD. Dette er fortsatt ikke diagnostikk, men det kan være verdt en mer formell vurdering dersom dette skaper problemer i hverdagen."
    }
  })
}

shinyApp(ui, server)
