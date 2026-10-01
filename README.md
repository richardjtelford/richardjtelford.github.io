# richardjtelford.github.io

Richard Telford's homepage. 

This site uses quarto.

Also Richard Telford's CV.

CV is made using quarto with a typst template. 
The typst template was modified from https://github.com/cwimpy/my-cv

Important files:

- cv/make_cv.R Render the CV, controlling with sections and references get printed
- cv/cv.qmd The CV. Many things are controlled through the YAML at the top of the file.
- cv/cv.typ The typst template
- R/bibstyler.R Code to modify how RefManager prints the references. Code modified from a forgotten repo on GitHub.
- R/cv_entries.R Code to process the entries into the CV
- resources/conference.bib Bibtex file with conferences
- resources/works.bib Bibtex file with publications
- resources/cv-entries.yaml yaml file with cv entries

ALso needed, .R.profile with phone number. To make this file, use
``` r
usethis::edit_r_profile(scope = "project")
```
and add the line 

``` r
.phone = "+47 12345678" # my phone number
```
