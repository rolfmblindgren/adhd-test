# app.R
library(shiny)

items <- tibble::tibble(
  id = paste0("item", 1:10),
  tekst = c(
    "Jeg klarer som regel å starte på kjedelige oppgaver uten at det blir et stort nummer ut av det.",
    "Jeg mister sjelden viktige ting (nøkler, mobil, lommebok), og når jeg gjør det, finner jeg dem som regel raskt.",
    "Folk rundt meg beskriver meg ofte som til å stole på når det gjelder avtaler og frister.",
    "Jeg klarer vanligvis å holde oppmerksomheten på én oppgave til jeg er ferdig.",
    "Jeg er ikke avhengig av kaos eller siste liten for å komme i gang.",
    "Jeg blir sjelden så rastløs at jeg må bevege meg eller avbryte andre midt i noe.",
    "Jeg glemmer ikke avtaler eller beskjeder i en grad som skaper problemer for meg.",
    "Når noe er viktig for meg, klarer jeg stort sett å organisere hverdagen så det får plass.",
    "Jeg opplever ikke at tankene «løper løpsk» så ofte at det hindrer meg i dagliglivet.",
    "Jeg trenger ikke uvanlig mye ekstern struktur (påminnelser, andre mennesker) for å fungere greit."
  )
)

likert_vals <- c(
  "Stemmer ikke" = 1,
  "Stemmer litt" = 2,
  "Stemmer delvis" = 3,
  "Stemmer ganske godt" = 4,
  "Stemmer helt" = 5
)

ui <- fluidPage(
  titlePanel("Dette er ikke en ADHD-test"),

  sidebarLayout(
    sidebarPanel(
      h4("Hva er dette?"),
      p("Denne lille testen er ment som en illustrasjon på hvordan fravær av typiske ADHD-vansker kan se ut i hverdagen."),
      p("Den kan ikke brukes til å stille diagnose, og et høyt eller lavt resultat sier ingenting sikkert om deg."),
      p("Tenk på den som en faglig kommentert nett-lekeplass."),
      actionButton("beregn", "Beregn resultat")
    ),

    mainPanel(
      tabsetPanel(
        tabPanel(
          "Spørsmål",
          br(),
          p("Les hvert utsagn og velg hvor godt det stemmer for deg de siste årene, ikke bare de siste ukene."),
          hr(),
          # Dynamisk generering av items
          lapply(seq_len(nrow(items)), function(i) {
            item <- items[i, ]
            radioButtons(
              inputId = item$id,
              label = item$tekst,
              choices = likert_vals,
              selected = character(0)
            )
          })
        ),
        tabPanel(
          "Resultat",
          br(),
          h3("Tolkning"),
          textOutput("resultat_tekst"),
          br(),
          h4("Skår (jo høyere, jo mer taler mot ADHD)"),
          textOutput("score_tekst"),
          br(),
          h4("Viktig forbehold"),
          p("ADHD stilles på bakgrunn av en grundig utredning, utviklingshistorikk og klinisk skjønn. "),
          p("Mange mennesker har enkeltsymptomer uten at det betyr at de har en diagnose, og noen med ADHD har lært seg gode strategier som gjør at de skårer «pent» på denne typen skjema.")
        )
      )
    )
  )
)

server <- function(input, output, session) {

  score_reaktiv <- eventReactive(input$beregn, {
    # Hent svar
    svar <- sapply(items$id, function(id) input[[id]])

    # Sjekk om noen er ubesvart
    if (any(is.null(svar) | svar == "")) {
      return(list(
        gyldig = FALSE,
        beskjed = "Du må svare på alle utsagn før du får et resultat.",
        score = NA
      ))
    }

    svar_num <- as.numeric(svar)
    mean_score <- mean(svar_num)

    list(
      gyldig = TRUE,
      beskjed = NULL,
      score = mean_score
    )
  })

  output$score_tekst <- renderText({
    res <- score_reaktiv()
    if (isFALSE(res$gyldig)) return(res$beskjed)
    paste0("Gjennomsnittsskår: ", round(res$score, 2), " (av 5)")
  })

  output$resultat_tekst <- renderText({
    res <- score_reaktiv()
    if (isFALSE(res$gyldig)) return("")

    s <- res$score

    if (s >= 4.2) {
      paste(
        "Svarene dine tyder på at du har lite av de oppmerksomhets- og reguleringsvanskene",
        "man typisk ser ved ADHD. Det betyr ikke at livet er friksjonsfritt, men mye taler mot",
        "at ADHD er en hovedforklaring på eventuelle utfordringer."
      )
    } else if (s >= 3.2) {
      paste(
        "Bildet er ganske nøytralt. Du beskriver noen styrker og noen områder med friksjon.",
        "Dette er et vanlig mønster, også hos personer uten ADHD. En nett-test kan uansett",
        "ikke si noe sikkert – det krever en ordentlig klinisk vurdering."
      )
    } else {
      paste(
        "Du rapporterer flere vansker som kan ligne på det man ser ved ADHD, særlig når det",
        "gjelder oppmerksomhet, organisering og utholdenhet. Denne testen kan likevel ikke",
        "avgjøre om du har ADHD eller ikke. Dersom dette skaper betydelige problemer i hverdagen,",
        "er det mer meningsfullt å snakke med noen som faktisk kan utrede deg enn å ta flere",
        "nett-tester – også denne."
      )
    }
  })
}

shinyApp(ui, server)
