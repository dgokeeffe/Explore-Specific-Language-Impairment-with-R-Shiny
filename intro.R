introUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(12,
          # Introduction ----
          h2("Welcome!"),
          p("Specific Language Impairment (SLI) is a speech disorder that approximately 1 in 20 children suffer from. 
            It is defined as having a language ability that is lower in comparison to your peers but there's no obvious reason for it 
            (i.e., no hearing loss, no brain damage, normal IQ, etc.)."),
          br(),
          p("The dataset I'm using has been engineered in Python",
            a(href="https://github.com/dgokeeffe/Automating-Specific-Language-Impairment", "(see code here)"),
            "from several corpora available at the",
            a(href="https://childes.talkbank.org/", "CHILDES talkbank."),
            "These corpora are transcripts of children telling a story from a wordless picture book such as \"Frog Where Are You?\" a               page of which you can see below: "),
          div(img(src = "frog_1.500x500.jpg", height = 500, width = 500, align='center'),style="text-align: center;"),
          br(),
          p("As you can imagine you can wield a number of narratives from this picture. The higher your language ability, the better the story you can tell. It's a test that can be done quickly and it tests both how a child is recognizing the scene and expressing it. Unfortunately diagnosing a language impairment through such a narrative is currently a time consuming process as a specialist will have to go over these transcripts and recognize the tell tale signs of SLI. If we engineer a way to quantitatively extract these features computationally, we can train a machine learning algorithm to predict whether a child has SLI or is Typically Developing (TD), avoiding this overhead all together."),
          br(),
          
          # Corpora ----
          h2("Corpora"),
          p("Before moving forward important to understand that this dataset is made using three sets of transcripts. They are different children, from different places, telling different stories in different conditions. As you move through this application you can explore through these different corpora to tell the differences or put them all together."),
          
          h3("ENNI"),
          a(href="https://childes.talkbank.org/browser/index.php?url=Clinical-MOR/ENNI/SLI/531.cha", "Example Transcript"),
          p("P. Schneider, D. Hayward, and R. V. Dub,",
          em("“Storytelling from pictures using the edmonton narrative norms instrument,”")," 2006."),
          p("Origin: French-Candadian: Edmondton, Alberta"),
          p("Age Range: 4 to 9"),
          p("300 TD: 77 SLI"),
          
          h3("Gillam"),
          a(href="https://childes.talkbank.org/browser/index.php?url=Clinical-MOR/Gillam/LangImpaired/6f/6y57237ks-l.cha",
            "Example Transcript"),
          p("R. Gillam and N. Pearson,",
            em("Test of Narrative Language."),
            "Austin, TX: Pro-Ed Inc., 2004"),
          p("Origin: South-West American (USA)"),
          p("Age Range: 5 to 12"),
          p("497 TD: 171 SLI"),
          
          h3("Conti-Ramsden-4"),
          a(href="https://childes.talkbank.org/browser/index.php?url=Clinical-MOR/Conti/Conti4/SLI-narrative/fssli113.cha",
            "Example Transcript"),
          p("D. Wetherell, N. Botting, and G. Conti-Ramsden,",
          em("“Narrative skills in adolescents with a history of SLI in relation to non-verbal IQ scores,”"),
          "Child Language Teaching and Therapy, vol. 23, no. 1, pp. 95–113, 2007"),
          p("Origin: Central England"),
          p("Age Range: 13 to 16"),
          p("99 TD: 19 SLI"),
          br(),
          
          h2("References"),
          p("The idea for this project originated from a paper published by a team of researchers from The University of Texas at Austin. They created a classifier with an LOOCV F1 score of 86.49% on the Conti-Ramsden-4 corpus. My best classifier scored 92% under the same conditions."),
          br(),
          p("K. Gabani, T. Solorio, Y. Liu, K.-n. Hassanali, and C. A. Dollaghan,",
          em("“Exploring a corpus-based approach for detecting language impairment in monolingual english-speaking children,”"),
            "Artificial Intelligence in Medicine, vol. 53, no. 3, pp. 161–170, 2011")
      )
    )
  )
}
