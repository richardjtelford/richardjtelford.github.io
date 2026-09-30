# full thesis

quarto::quarto_render(
  input = "cv/cv.qmd",
  output_file = "cv_long.pdf", 

  execute_params = list(#publications = c("Velle2023", "Vandvik2023", "Telford2007"), 
                        short = FALSE,
                        showTeaching = TRUE, 
                        showFunding =TRUE,
                        phone = .phone, # stored in project R profile 
                        hindex = 43,
                        isiCitations = 7880)
)


quarto::quarto_render(
  input = "cv/cv.qmd",
  output_file = "cv_short.pdf", 
  
  execute_params = list(publications = c("Althuizen2026", "Gaudard2025", "Maitner2023", "Vandvik2025", "Halbritter2024", "Velle2023"), 
    short = TRUE,
    showTeaching = FALSE, 
    showFunding = FALSE,
    phone = .phone, # stored in project R profile
    hindex = 43,
    isiCitations = 7880)
)
