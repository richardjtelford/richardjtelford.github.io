// cv-template.typ - Clean version with hanging indent for publications

#import "@preview/fontawesome:0.5.0": *
#import "@preview/use-academicons:0.1.0": *

//------------------------------------------------------------------------------
// Style and Colors
//------------------------------------------------------------------------------

#let color-darknight = rgb("#131A28")
#let color-darkgray = rgb("#333333")
#let color-gray = rgb("#5d5d5d")
#let color-lightgray = rgb("#999999")
#let color-accent = rgb("#333333")

#let font-header = ("Liberation Sans")
#let font-text = ("Liberation Sans")

#show heading.where(level: 2): it => {
  block(width: 100%, below: 0.1em)[
    
    // Display the heading content
    #it.body
    
    // Add a thin line underneath
    #v(-10pt) 
    #line(length: 100%, stroke: 0.5pt + gray)
     #v(0.8em) 
  ]
}

//------------------------------------------------------------------------------
// Helper Functions
//------------------------------------------------------------------------------

// Layout utility
#let __justify_align(left_body, right_body) = {
  block[
    #box(width: 4fr)[#left_body]
    #box(width: 1fr)[
      #align(right)[
        #right_body
      ]
    ]
  ]
}

// Reference entry
#let reference-entry(
  name: "",
  title: "",
  subtitle: "",
  institution: "",
  address: "",
  phone: "",
  email: ""
) = {
  set text(size: 10pt)
  [
    #text(weight: "bold", fill: color-darkgray)[#name]
    #linebreak()
    #text(fill: color-gray)[
      #title
      #if subtitle != "" [
        #linebreak()
        #subtitle
      ]
      #linebreak()
      #institution
      #if address != "" [
        #linebreak()
        #fa-icon("location-dot") #address
      ]
      #if phone != "" [
        #linebreak()
        #fa-icon("phone") #phone
      ]
      #if email != "" [
        #linebreak()
        #fa-icon("envelope") #link("mailto:" + email)[#text(fill: color-accent)[#email]]
      ]
    ]
  ]
  v(0.8em)
}

// Two-column references layout with toggle support
#let references-section(references, show-references: true) = {
  if show-references and references.len() > 0 {
    let num-refs = references.len()
    let left-refs = references.slice(0, calc.ceil(num-refs / 2))
    let right-refs = references.slice(calc.ceil(num-refs / 2))
    
    grid(
      columns: (1fr, 1fr),
      column-gutter: 2em,
      align: (left, left),
      [
        #for ref in left-refs [
          #reference-entry(
            name: ref.name,
            title: ref.title,
            subtitle: if "subtitle" in ref { ref.subtitle } else { "" },
            institution: ref.institution,
            address: ref.address,
            phone: ref.phone,
            email: ref.email
          )
        ]
      ],
      [
        #for ref in right-refs [
          #reference-entry(
            name: ref.name,
            title: ref.title,
            subtitle: if "subtitle" in ref { ref.subtitle } else { "" },
            institution: ref.institution,
            address: ref.address,
            phone: ref.phone,
            email: ref.email
          )
        ]
      ]
    )
  } else if not show-references {
    text(size: 11pt, style: "italic", fill: color-gray)[References available upon request.]
  }
}

// Header styles
#let secondary-right-header(body) = {
  set text(
    size: 11pt,
    weight: "thin",
    style: "italic",
    fill: color-accent,
  )
  body
}

#let tertiary-right-header(body) = {
  set text(
    weight: "light",
    size: 9pt,
    style: "italic",
    fill: color-gray,
  )
  body
}

// Justified headers
#let justified-header(primary, secondary, amount: none) = {
  set block(
    above: 0.7em,
    below: 0.7em,
  )
  pad[
    #__justify_align[
      #set text(
        size: 11pt,
        weight: "bold",
        fill: color-darkgray,
      )
      #primary
      #if amount != none [
        #text(weight: "bold")[ (#amount)]
      ]
    ][
      #secondary-right-header[#secondary]
    ]
  ]
}

#let secondary-justified-header(primary, secondary) = {
  __justify_align[
     #set text(
      size: 11pt,
      weight: "regular",
      fill: color-gray,
    )
    #primary
  ][
    #tertiary-right-header[#secondary]
  ]
}

//------------------------------------------------------------------------------
// CV Functions
//------------------------------------------------------------------------------

// Section heading
#let section(title) = {
  set block(
    above: 1.5em,
    below: 1em,
  )
  set text(
    size: 16pt,
    weight: "regular",
  )
  
  stack(
    spacing: 0.3em,
    text(color-accent, weight: "bold")[#title],
    line(length: 100%)
  )
}

// CV entry
#let cv-entry(
  title: "",
  organization: "",
  location: "",
  dates: "",
  description: "",
  details: (),
  indent: false,
  amount: none
) = {
  let left-margin = if indent { 1em } else { 0em }
  
  pad(left: left-margin)[
    #justified-header(title, location, amount: amount)
    #secondary-justified-header(if organization != "" { organization } else { description }, dates)
    #if description != "" and organization != "" [
      #v(0.2em)
      #pad(
        left: 1em,
        text(size: 11pt, fill: color-gray)[
          - #description
        ]
      )
    ]
  ]
  
  if details.len() > 0 {
    v(0.3em)
    pad(left: left-margin)[
      #set text(
        size: 11pt,
        style: "normal",
        weight: "light",
        fill: color-darknight,
      )
      #set par(leading: 0.65em)
      #for detail in details {
        [- #detail]
        linebreak()
      }
    ]
  }
  
  v(0.5em)
}


// Teaching entry
#let teaching-entry(
  course: "",
  institution: "",
  terms: "",
  urls: (),
) = {
  pad[
    #justified-header(course, terms)
    #secondary-justified-header(institution, "")
    #if urls.len() > 0 [
      #v(0.3em)
      #set text(size: 9pt, fill: color-accent)
      #for (i, url) in urls.enumerate() [
        #link(url)#if i < urls.len() - 1 [#linebreak()]
      ]
    ]
  ]
  v(0.3em)
}

// Service list
#let service-list(items) = {
  set text(size: 11pt, fill: color-darknight)
  block[
    #items.join("; ")
  ]
  v(0.5em)
}


// Publication summary
#let pub-summary(
  ncitations: "",
  hindex: "",
  npapers: "",
  nchapters: "",
  npackages: ""
) = {
  grid(
    columns: (1fr, 1fr, 1fr, 1fr),
    column-gutter: 0.5em,
    align: (left, left, left, left),
    [
      ISI Citations: \
      H-Index: 
    ],
    [
      #h(-2em) #ncitations \
      #h(-2em) #hindex 
    ],
    [
      #h(-0.5em) Number Papers:\
      #h(-0.5em) Number Chapters:\
      #h(-0.5em) Number Packages:
    ],
    [
      #h(-2em) #npapers \
      #h(-2em) #nchapters\
      #h(-2em) #npackages
    ],
    
  )


}

//------------------------------------------------------------------------------
// Document Setup
//------------------------------------------------------------------------------

#set document(
  title: " - CV", 
  author: ""
)

#set text(
  font: font-text,
  size: 11pt,
  lang: "en",
  fill: color-darkgray,
  fallback: true
)

#set par(
  justify: true,
  leading: 0.65em
)

#set page(
  paper: "a4",
  margin: (left: 15mm, right: 15mm, top: 10mm, bottom: 10mm),
  footer: context [
    #set text(fill: color-lightgray, size: 8pt)
    #grid(
      columns: (1fr, 1fr, 1fr),
      align(left)[Revised #datetime.today().display("[month repr:long] [year]")],
      align(center)[Richard James Telford · CV],
      align(right)[#counter(page).display("1 / 1", both: true)]
    )
  ]
)

#show heading.where(level: 1): it => section(it.body)

#show heading: it => {
  block(breakable: false, it)
  v(0.3em, weak: true)
}

#show link: it => {
  set text(fill: color-accent)
  it
}

//------------------------------------------------------------------------------
// Header
//------------------------------------------------------------------------------

#align(left)[
  #pad(bottom: 5pt)[
    #block[
      #set text(
        size: 32pt,
        style: "normal",
        font: font-header,
      )
      #text(weight: "bold")[Richard James]
      #text(weight: "bold")[Telford]
    ]
  ]
  
  #set block(above: 0.75em, below: 0.75em)
  #set text(color-darkgray, size: 12pt, weight: "regular")
  #block[Department of Biological Sciences | University of Bergen]
  
  #v(0.5em)
  
  #set text(size: 9pt, weight: "regular", style: "normal", fill: color-darkgray)
  #grid(
    columns: (1fr, 1fr, 1fr),
    column-gutter: 0.5em,
    align: (left, left, left),
    [
      #fa-icon("location-dot") Postboks 7803, 5020 Bergen, Norway \
      #fa-icon("envelope") #link("mailto:richard.telford\@uib.no")[#text(fill: color-darkgray)[richard.telford\@uib.no]] \
      #fa-icon("orcid", font: "Font Awesome 6 Brands") #link("https:/\/orcid.org/0000-0001-9826-3076")[#text(fill: color-darkgray)[0000-0001-9826-3076]]
    ],
    [
      #h(2em) #fa-icon("phone") +47 9412xxxx \
      #h(2em) #fa-icon("earth-europe") #link("https:/\/richardjtelford.github.io")[#text(fill: color-darkgray)[richardjtelford.github.io]] \
      #h(2em) #fa-icon("linkedin", font: "Font Awesome 6 Brands") #link("https:/\/www.linkedin.com/in/richard-telford-11b4bb24/")[#text(fill: color-darkgray)[richard-telford]]
    ],
    [
      #h(-0.5em) #fa-icon("twitter", font: "Font Awesome 6 Brands") #link("https:/\/x.com/richardjtelford")[#text(fill: color-darkgray)[\@richardjtelford]] \
      #h(-0.5em) #fa-icon("github", font: "Font Awesome 6 Brands") #link("https:/\/github.com/richardjtelford")[#text(fill: color-darkgray)[richardjtelford]] \
      #h(-0.5em) #ai-icon("google-scholar") #link("https:/\/scholar.google.com/citations?user=XoVtEAYAAAAJ&hl=en")[#text(fill: color-darkgray)[Google Scholar]]
    ]
  )
]

#v(1em)

= Employment History
<employment-history>
#cv-entry(
  title: "Associate Professor of Plant Ecology",
  organization: "University of Bergen",
  location: "Bergen, Norway",
  dates: "2007–present",
)
#cv-entry(
  title: "Postdoctoral Researcher (Forsker II)",
  organization: "Bjerknes Centre for Climate Research",
  location: "Bergen, Norway",
  dates: "2003–2006",
)
#cv-entry(
  title: "Research Associate and Teaching Fellow",
  organization: "Newcastle University",
  location: "Newcastle, U.K.",
  dates: "2000–2002",
)
#cv-entry(
  title: "Research Associate and Teaching Fellow",
  organization: "Lancaster University",
  location: "Lancaster, U.K.",
  dates: "1998–2000",
)
= Education
<education>
#cv-entry(
  title: "Certificate in Learning and Teaching in Higher Education ",
  organization: "Newcastle University",
  location: "Newcastle, U.K.",
  dates: "2000–2001",
)
#cv-entry(
  title: "Ph.D. in Earth Sciences",
  organization: "University of Wales, Aberystwyth",
  location: "Aberystwyth, Wales",
  dates: "1995–1998",
  description: "\"Diatom stratigraphies of Lakes Awassa and Tilo, Ethiopia: Holocene records of groundwater variability and climate change.\" Supervised by Dr H. F. Lamb.",
)
#cv-entry(
  title: "B.A. (Hons) 2.1 Natural Sciences (Plant Sciences) ",
  organization: "Clare College, Cambridge University",
  location: "Cambridge, U.K.",
  dates: "1991–1994",
  description: "Secondary subjects: Geology and Ecology",
)
= Research
<research>
== Research Interests
<research-interests>
#service-list(("Palaeoecology", "Quantitative palaeoecological reconstruction", "Climate change", "Climate impacts", "Reproducible science"))
== Publications
<publications>
#pub-summary(
  ncitations: [0],
  hindex: [0],
  npapers: 108,
  nchapters: 8,
  npackages: 3
) 
== Journal Articles
<journal-articles>
Althuizen, I. H. J., Gya, R., Jaroszynska, F., Lee, H., #strong[Telford, R. J.], Chipperfield, J., Enquist, B. J., Goldberg, D. E., and Vandvik, V. (2026) Climate‐induced shifts in plant investment strategies regulate ecosystem carbon cycling across alpine grasslands. #emph[Journal of Ecology] 114, e70364. #link("https://doi.org/10.1111/1365-2745.70364")[#box(image("cv_files/mediabag/1365--2745.70364-gre.svg", height: 0.14583in))]

Gaudard, J., #strong[Telford, R. J.], Chacon‐Labella, J., Dawson, H. R., Enquist, B. J., Töpper, J. P., and others. (2025) fluxible: An R package to process ecosystem gas fluxes from closed‐loop chambers in an automated and reproducible way. #emph[Methods in Ecology and Evolution] 16, 2560-2568. #link("https://doi.org/10.1111/2041-210x.70161")[#box(image("cv_files/mediabag/2041--210x.70161-gre.svg", height: 0.14583in))]

Gould, E., Fraser, H. S., Parker, T. H., Nakagawa, S., and others including #strong[Telford, R. J.] (2025) Same data, different analysts: variation in effect sizes due to analytical decisions in ecology and evolutionary biology. #emph[BMC Biology] 23, 35. #link("https://doi.org/10.1186/s12915-024-02101-x")[#box(image("cv_files/mediabag/s12915--024--02101--.svg", height: 0.14583in))]

Vandvik, V., Halbritter, A. H., Macias-Fauria, M., Maitner, B. S., Michaletz, S. T., #strong[Telford, R. J.], and others. (2025) Plant traits and associated ecological data from global change experiments and climate gradients in Norway. #emph[Scientific Data] 12, 1477. #link("https://doi.org/10.1038/s41597-025-05509-4")[#box(image("cv_files/mediabag/s41597--025--05509--.svg", height: 0.14583in))]

Halbritter, A. H., Vandvik, V., Cotner, S. H., Farfan-Rios, W., and others including #strong[Telford, R. J.] (2024) Plant trait and vegetation data along a 1314 m elevation gradient with fire history in Puna grasslands, Perú. #emph[Scientific Data] 11, 225. #link("https://doi.org/10.1038/s41597-024-02980-3")[#box(image("cv_files/mediabag/s41597--024--02980--.svg", height: 0.14583in))]

Jaroszynska, F., Lie Olsen, S., Gya, R., Klanderud, K., #strong[Telford, R. J.], and Vandvik, V. (2024) Plant functional group interactions intensify with warming in alpine grasslands. #emph[Ecography] 2024, e07018. #link("https://doi.org/10.1111/ecog.07018")[#box(image("cv_files/mediabag/ecog.07018-green.svg", height: 0.14583in))]

Jaroszynska, F., Althuizen, I., Halbritter, A. H., Klanderud, K., Lee, H., #strong[Telford, R. J.], and Vandvik, V. (2023) Bryophytes dominate plant regulation of soil microclimate in alpine grasslands. #emph[Oikos] 2023, e10091. #link("https://doi.org/10.1111/oik.10091")[#box(image("cv_files/mediabag/oik.10091-green.svg", height: 0.14583in))]

Lynn, J. S., Gya, R., Klanderud, K., #strong[Telford, R. J.], Goldberg, D. E., and Vandvik, V. (2023) Traits help explain species' performance away from their climate niche centre. #emph[Diversity and Distributions] 29, 962-978. #link("https://doi.org/10.1111/ddi.13718")[#box(image("cv_files/mediabag/ddi.13718-green.svg", height: 0.14583in))]

Maitner, B. S., Halbritter, A. H., #strong[Telford, R. J.], Strydom, T., Chacon, J., Lamanna, C., and others. (2023) Bootstrapping outperforms community‐weighted approaches for estimating the shapes of phenotypic distributions. #emph[Methods in Ecology and Evolution] 14, 2592-2610. #link("https://doi.org/10.1111/2041-210x.14160")[#box(image("cv_files/mediabag/2041--210x.14160-gre.svg", height: 0.14583in))]

Vandvik, V., Halbritter, A. H., Althuizen, I. H. J., Christiansen, C. T., and others including #strong[Telford, R. J.] (2023) Plant traits and associated data from a warming experiment, a seabird colony, and along elevation in Svalbard. #emph[Scientific Data] 10, 578. #link("https://doi.org/10.1038/s41597-023-02467-7")[#box(image("cv_files/mediabag/s41597--023--02467--.svg", height: 0.14583in))]

Velle, L. G., Haugum, S. V., #strong[Telford, R. J.], Thorvaldsen, P., and Vandvik, V. (2023) Prescribed burning can promote recovery of Atlantic coastal heathlands suffering dieback after extreme drought events. #emph[Applied Vegetation Science] 26, e12760. #link("https://doi.org/10.1111/avsc.12760")[#box(image("cv_files/mediabag/avsc.12760-green.svg", height: 0.14583in))]

Cao, X., Chen, J., Tian, F., Xu, Q., Herzschuh, U., #strong[Telford, R. J.], Huang, X., Zheng, Z., Shen, C., and Li, W. (2022) Long-distance modern analogues bias results of pollen-based precipitation reconstructions. #emph[Science Bulletin] 67, 1115-1117. #link("https://doi.org/10.1016/j.scib.2022.01.003")[#box(image("cv_files/mediabag/j.scib.2022.01.003-g.svg", height: 0.14583in))]

Herzschuh, U., Böhmer, T., Li, C., Cao, X., Hébert, R., Dallmeyer, A., #strong[Telford, R. J.], and Kruse, S. (2022) Reversals in Temperature-Precipitation Correlations in the Northern Hemisphere Extratropics During the Holocene. #emph[Geophysical Research Letters] 49, e2022GL099730. #link("https://doi.org/10.1029/2022gl099730")[#box(image("cv_files/mediabag/2022gl099730-green.svg", height: 0.14583in))]

Strømme, C. B., Lane, A. K., Halbritter, A. H., Law, E., and others including #strong[Telford, R. J.] (2022) Close to open-Factors that hinder and promote open science in ecology research and education. #emph[PLoS ONE] 17, e0278339. #link("https://doi.org/10.1371/journal.pone.0278339")[#box(image("cv_files/mediabag/journal.pone.0278339.svg", height: 0.14583in))]

Vandvik, V., Althuizen, I. H. J., Jaroszynska, F., Krüger, L. C., and others including #strong[Telford, R. J.] (2022) The role of plant functional groups mediating climate impacts on carbon and biodiversity of alpine grasslands. #emph[Scientific Data] 9, 451. #link("https://doi.org/10.1038/s41597-022-01559-0")[#box(image("cv_files/mediabag/s41597--022--01559--.svg", height: 0.14583in))]

Geange, S. R., Oppen, J. von, Strydom, T., Boakye, M., and others including #strong[Telford, R. J.] (2021) Next-generation field courses: Integrating Open Science and online learning. #emph[Ecology and Evolution] 11, 3577-3587. #link("https://doi.org/10.1002/ece3.7009")[#box(image("cv_files/mediabag/ece3.7009-green.svg", height: 0.14583in))]

Lynn, J. S., Klanderud, K., #strong[Telford, R. J.], Goldberg, D. E., and Vandvik, V. (2021) Macroecological context predicts species' responses to climate warming. #emph[Global Change Biology] 27, 2088-2101. #link("https://doi.org/10.1111/gcb.15532")[#box(image("cv_files/mediabag/gcb.15532-green.svg", height: 0.14583in))]

Moros, M., Deckker, P. D., Perner, K., Ninnemann, U. S., Wacker, L., #strong[Telford, R. J.], Jansen, E., Blanz, T., and Schneider, R. (2021) Hydrographic shifts south of Australia over the last deglaciation and possible interhemispheric linkages. #emph[Quaternary Research] 102, 130-141. #link("https://doi.org/10.1017/qua.2021.12")[#box(image("cv_files/mediabag/qua.2021.12-green.svg", height: 0.14583in))]

Thomson, E. R., Spiegel, M. P., Althuizen, I. H. J., Bass, P., and others including #strong[Telford, R. J.] (2021) Multiscale mapping of plant functional groups and plant traits in the High Arctic using field spectroscopy, UAV imagery and Sentinel-2A data. #emph[Environmental Research Letters] 16, 055006. #link("https://doi.org/10.1088/1748-9326/abf464")[#box(image("cv_files/mediabag/abf464-green.svg", height: 0.14583in))]

Carlson, C. J., Chipperfield, J. D., Benito, B. M., #strong[Telford, R. J.], and O'Hara, R. B. (2020) Don't gamble the COVID-19 response on ecological hypotheses. #emph[Nature Ecology & Evolution] 4, 1155-1155. #link("https://doi.org/10.1038/s41559-020-1279-2")[#box(image("cv_files/mediabag/s41559--020--1279--2.svg", height: 0.14583in))]

Carlson, C. J., Chipperfield, J. D., Benito, B. M., #strong[Telford, R. J.], and O'Hara, R. B. (2020) Species distribution models are inappropriate for COVID-19. #emph[Nature Ecology & Evolution] 4, 770-771. #link("https://doi.org/10.1038/s41559-020-1212-8")[#box(image("cv_files/mediabag/s41559--020--1212--8.svg", height: 0.14583in))]

Chevalier, M., Davis, B. A., Heiri, O., Seppä, H., and others including #strong[Telford, R. J.] (2020) Pollen-based climate reconstruction techniques for late Quaternary studies. #emph[Earth-Science Reviews] 210, 103384. #link("https://doi.org/10.1016/j.earscirev.2020.103384")[#box(image("cv_files/mediabag/j.earscirev.2020.103.svg", height: 0.14583in))]

Contina, A., Yanco, S. W., Pierce, A. K., DePrenger-Levin, M., and others including #strong[Telford, R. J.] (2020) Comment on Ä global-scale ecological niche model to predict SARS-CoV-2 coronavirus infection rate. #emph[Ecological Modelling] 436, 109288. #link("https://doi.org/10.1016/j.ecolmodel.2020.109288")[#box(image("cv_files/mediabag/j.ecolmodel.2020.109.svg", height: 0.14583in))]

Gallagher, R. V., Falster, D. S., Maitner, B. S., Salguero-Gómez, R., and others including #strong[Telford, R. J.] (2020) Open Science principles for accelerating trait-based science across the Tree of Life. #emph[Nature Ecology & Evolution] 4, 294-303. #link("https://doi.org/10.1038/s41559-020-1109-6")[#box(image("cv_files/mediabag/s41559--020--1109--6.svg", height: 0.14583in))]

Vandvik, V., Halbritter, A. H., Yang, Y., He, H., and others including #strong[Telford, R. J.] (2020) Plant traits and vegetation data from climate warming experiments along an 1100 m elevation gradient in Gongga Mountains, China. #emph[Scientific Data] 7, 189. #link("https://doi.org/10.1038/s41597-020-0529-0")[#box(image("cv_files/mediabag/s41597--020--0529--0.svg", height: 0.14583in))]

Vandvik, V., Skarpaas, O., Klanderud, K., #strong[Telford, R. J.], Halbritter, A. H., and Goldberg, D. E. (2020) Biotic rescaling reveals importance of species interactions for variation in biodiversity responses to climate change. #emph[Proceedings of the National Academy of Sciences] 117, 22858-22865. #link("https://doi.org/10.1073/pnas.2003377117")[#box(image("cv_files/mediabag/pnas.2003377117-gree.svg", height: 0.14583in))]

Halbritter, A. H., De Boeck, H. J., Eycott, A. E., Reinsch, S., Robinson, D. A., Vicca, S., Berauer, B., Christiansen, C. T., Estiarte, M., Grünzweig, J. M., Gya, R., Hansen, K., Jentsch, A., Lee, H., Linder, S., Marshall, J., Peñuelas, J., Kappel Schmidt, I., Stuart-Haëntjens, E., Wilfahrt, P., the ClimMani Working Group, and Vandvik, V. (2019) The handbook for standardized field and laboratory measurements in terrestrial climate change experiments and observational studies (ClimEx). #emph[Methods in Ecology and Evolution] 11, 22-37. #link("https://doi.org/10.1111/2041-210X.13331")[#box(image("cv_files/mediabag/2041--210X.13331-gre.svg", height: 0.14583in))]

Herzschuh, U., Cao, X., Laepple, T., Dallmeyer, A., #strong[Telford, R. J.], Ni, J., and others. (2019) Position and orientation of the westerly jet determined Holocene rainfall patterns in China. #emph[Nature Communications] 10, 2376. #link("https://doi.org/10.1038/s41467-019-09866-8")[#box(image("cv_files/mediabag/s41467--019--09866--.svg", height: 0.14583in))]

Khider, D., Emile-Geay, J., McKay, N. P., Gil, Y., and others including #strong[Telford, R. J.] (2019) PaCTS 1.0: A crowdsourced reporting standard for paleoclimate data. #emph[Paleoceanography and Paleoclimatology] 34, 1570-1596. #link("https://doi.org/10.1029/2019PA003632")[#box(image("cv_files/mediabag/2019PA003632-green.svg", height: 0.14583in))]

#strong[Telford, R. J.] (2019) Review and test of reproducibility of subdecadal resolution palaeoenvironmental reconstructions from microfossil assemblages. #emph[Quaternary Science Reviews] 222, 105893. #link("https://doi.org/10.1016/j.quascirev.2019.105893")[#box(image("cv_files/mediabag/j.quascirev.2019.105.svg", height: 0.14583in))]

Al-Sabouni, N., Fenton, I. S., #strong[Telford, R. J.], and Kučera, M. (2018) Reproducibility of species recognition in modern planktonic foraminifera and its implications for analyses of community structure. #emph[Journal of Micropalaeontology] 37, 519-534. #link("https://doi.org/10.5194/jm-37-519-2018")[#box(image("cv_files/mediabag/jm--37--519--2018-gr.svg", height: 0.14583in))]

Bouchet, V. M., #strong[Telford, R. J.], Rygg, B., Oug, E., and Alve, E. (2018) Can benthic foraminifera serve as proxies for changes in benthic macrofaunal community structure? Implications for the definition of reference conditions. #emph[Marine Environmental Research] 137, 24-36. #link("https://doi.org/10.1016/j.marenvres.2018.02.023")[#box(image("cv_files/mediabag/j.marenvres.2018.02..svg", height: 0.14583in))]

Henn, J. J., Buzzard, V., Enquist, B. J., Halbritter, A. H., and others including #strong[Telford, R. J.] (2018) Intraspecific trait variation and phenotypic plasticity mediate alpine plant species response to climate change. #emph[Frontiers in Plant Science] 9, 1548. #link("https://doi.org/10.3389/fpls.2018.01548")[#box(image("cv_files/mediabag/fpls.2018.01548-gree.svg", height: 0.14583in))]

Perner, K., Moros, M., Deckker, P. D., Blanz, T., Wacker, L., #strong[Telford, R. J.], Siegel, H., Schneider, R., and Jansen, E. (2018) Heat export from the tropics drives mid to late Holocene palaeoceanographic changes offshore southern Australia. #emph[Quaternary Science Reviews] 180, 96-110. #link("https://doi.org/10.1016/j.quascirev.2017.11.033")[#box(image("cv_files/mediabag/j.quascirev.2017.11..svg", height: 0.14583in))]

Uwimbabazi, M., Eycott, A. E., Babweteera, F., Sande, E., #strong[Telford, R. J.], and Vandvik, V. (2018) Avian guild assemblages in forest fragments around Budongo Forest Reserve, western Uganda. #emph[Ostrich] 88, 267-276. #link("https://doi.org/10.2989/00306525.2017.1318186")[#box(image("cv_files/mediabag/00306525.2017.131818.svg", height: 0.14583in))]

Vandvik, V., Halbritter, A. H., and #strong[Telford, R. J.] (2018) Greening up the mountain. #emph[Proceedings of the National Academy of Sciences] 115, 883-885. #link("https://doi.org/10.1073/pnas.1721285115")[#box(image("cv_files/mediabag/pnas.1721285115-gree.svg", height: 0.14583in))]

Yang, Y., Halbritter, A. H., Klanderud, K., #strong[Telford, R. J.], Wang, G., and Vandvik, V. (2018) Transplants, open top chambers (OTCs) and gradient studies ask different questions in climate change effects studies. #emph[Frontiers in Plant Science] 9, 1574. #link("https://doi.org/10.3389/fpls.2018.01574")[#box(image("cv_files/mediabag/fpls.2018.01574-gree.svg", height: 0.14583in))]

Andrén, E., #strong[Telford, R. J.], and Jonsson, P. (2017) Reconstructing the history of eutrophication and quantifying total nitrogen reference conditions in Bothnian Sea coastal waters. #emph[Estuarine, Coastal and Shelf Science] 198, 320-328. #link("https://doi.org/10.1016/j.ecss.2016.07.015")[#box(image("cv_files/mediabag/j.ecss.2016.07.015-g.svg", height: 0.14583in))]

Cao, X., Tian, F., #strong[Telford, R. J.], Ni, J., Xu, Q., Chen, F., Liu, X., Stebich, M., Zhao, Y., and Herzschuh, U. (2017) Impacts of the spatial extent of pollen-climate calibration-set on the absolute values, range and trends of reconstructed Holocene precipitation. #emph[Quaternary Science Reviews] 178, 37-53. #link("https://doi.org/10.1016/j.quascirev.2017.10.030")[#box(image("cv_files/mediabag/j.quascirev.2017.10..svg", height: 0.14583in))]

Chen, J., Lv, F., Huang, X., Birks, H. J. B., #strong[Telford, R. J.], Zhang, S., and others. (2017) A novel procedure for pollen-based quantitative paleoclimate reconstructions and its application in China. #emph[Science China Earth Sciences] 60, 2059-2066. #link("https://doi.org/10.1007/s11430-017-9095-1")[#box(image("cv_files/mediabag/s11430--017--9095--1.svg", height: 0.14583in))]

Oksman, M., Weckström, K., Miettinen, A., Juggins, S., Divine, D. V., Jackson, R., #strong[Telford, R. J.], Korsgaard, N. J., and Kucera, M. (2017) Younger Dryas ice margin retreat triggered by ocean surface warming in central-eastern Baffin Bay. #emph[Nature Communications] 8, 1017. #link("https://doi.org/10.1038/s41467-017-01155-6")[#box(image("cv_files/mediabag/s41467--017--01155--.svg", height: 0.14583in))]

St.~George, S. and #strong[Telford, R. J.] (2017) Fossil forest reveals sunspot activity in the early Permian: COMMENT. #emph[Geology] 45, e427. #link("https://doi.org/10.1130/G39414C.1")[#box(image("cv_files/mediabag/G39414C.1-green.svg", height: 0.14583in))]

Tegzes, A. D., Jansen, E., Lorentzen, T., and #strong[Telford, R. J.] (2017) Northward oceanic heat transport in the main branch of the Norwegian Atlantic Current over the late Holocene. #emph[The Holocene] 27, 1034-1044. #link("https://doi.org/10.1177/0959683616683251")[#box(image("cv_files/mediabag/0959683616683251-gre.svg", height: 0.14583in))]

Trachsel, M. and #strong[Telford, R. J.] (2017) All age--depth models are wrong, but are getting better. #emph[The Holocene] 27, 860-869. #link("https://doi.org/10.1177/0959683616675939")[#box(image("cv_files/mediabag/0959683616675939-gre.svg", height: 0.14583in))]

Eycott, A. E., Esaete, J., Reiniö, J., #strong[Telford, R. J.], and Vandvik, V. (2016) Plant functional group responses in an African tropical forest recovering from disturbance. #emph[Plant Ecology & Diversity] 9, 69-80. #link("https://doi.org/10.1080/17550874.2016.1143535")[#box(image("cv_files/mediabag/17550874.2016.114353.svg", height: 0.14583in))]

Guittar, J., Goldberg, D., Klanderud, K., #strong[Telford, R. J.], and Vandvik, V. (2016) Can trait patterns along gradients predict plant community responses to climate change? #emph[Ecology] 97, 2791-2801. #link("https://doi.org/10.1002/ecy.1500")[#box(image("cv_files/mediabag/ecy.1500-green.svg", height: 0.14583in))]

Payne, R. J., Babeshko, K. V., Bellen, S. van, Blackford, J. J., and others including #strong[Telford, R. J.] (2016) Significance testing testate amoeba water table reconstructions. #emph[Quaternary Science Reviews] 138, 131-135. #link("https://doi.org/10.1016/j.quascirev.2016.01.030")[#box(image("cv_files/mediabag/j.quascirev.2016.01..svg", height: 0.14583in))]

Rehfeld, K., Trachsel, M., #strong[Telford, R. J.], and Laepple, T. (2016) Assessing performance and seasonal bias of pollen-based climate reconstructions in a perfect model world. #emph[Climate of the Past] 12, 2255-2270. #link("https://doi.org/10.5194/cp-12-2255-2016")[#box(image("cv_files/mediabag/cp--12--2255--2016-g.svg", height: 0.14583in))]

#strong[Telford, R. J.], Chipperfield, J. D., Birks, H. H., and Birks, H. J. B. (2016) How foreign is the past? #emph[Nature] 538, E1-E2. #link("https://doi.org/10.1038/nature16447")[#box(image("cv_files/mediabag/nature16447-green.svg", height: 0.14583in))]

Trachsel, M. and #strong[Telford, R. J.] (2016) Technical note: Estimating unbiased transfer-function performances in spatially structured environments. #emph[Climate of the Past] 12, 1215-1223. #link("https://doi.org/10.5194/cp-12-1215-2016")[#box(image("cv_files/mediabag/cp--12--1215--2016-g.svg", height: 0.14583in))]

Akite, P., #strong[Telford, R. J.], Waring, P., Akol, A. M., and Vandvik, V. (2015) Temporal patterns in Saturnidae (silk moth) and Sphingidae (hawk moth) assemblages in protected forests of central Uganda. #emph[Ecology and Evolution] 5, 1746-1757. #link("https://doi.org/10.1002/ece3.1477")[#box(image("cv_files/mediabag/ece3.1477-green.svg", height: 0.14583in))]

Bjune, A. E., Grytnes, J., Jenks, C. R., #strong[Telford, R. J.], and Vandvik, V. (2015) Is palaeoecology a 'special branch' of ecology? #emph[The Holocene] 25, 17-24. #link("https://doi.org/10.1177/0959683614556386")[#box(image("cv_files/mediabag/0959683614556386-gre.svg", height: 0.14583in))]

Chen, F., Xu, Q., Chen, J., Birks, H. J. B., and others including #strong[Telford, R. J.] (2015) East Asian summer monsoon precipitation variability since the last deglaciation. #emph[Scientific Reports] 5, 11186. #link("https://doi.org/10.1038/srep11186")[#box(image("cv_files/mediabag/srep11186-green.svg", height: 0.14583in))]

Juggins, S., Simpson, G. L., and #strong[Telford, R. J.] (2015) Taxon selection using statistical learning techniques to improve transfer function prediction. #emph[The Holocene] 25, 130-136. #link("https://doi.org/10.1177/0959683614556388")[#box(image("cv_files/mediabag/0959683614556388-gre.svg", height: 0.14583in))]

Tegzes, A. D., Jansen, E., and #strong[Telford, R. J.] (2015) Which is the better proxy for paleo-current strength: Sortable-silt mean size (SS) or sortable-silt mean grain diameter (dSS)? A case study from the Nordic Seas. #emph[Geochemistry, Geophysics, Geosystems] 16, 3456-3471. #link("https://doi.org/10.1002/2014GC005655")[#box(image("cv_files/mediabag/2014GC005655-green.svg", height: 0.14583in))]

Cao, X., Herzschuh, U., #strong[Telford, R. J.], and Ni, J. (2014) A modern pollen-climate dataset from China and Mongolia: Assessing its potential for climate reconstruction. #emph[Review of Palaeobotany and Palynology] 211, 87-96. #link("https://doi.org/10.1016/j.revpalbo.2014.08.007")[#box(image("cv_files/mediabag/j.revpalbo.2014.08.0.svg", height: 0.14583in))]

Esaete, J., Eycott, A. E., Reiniö, J., #strong[Telford, R. J.], and Vandvik, V. (2014) The seed and fern spore bank of a recovering African tropical forest. #emph[Biotropica] 46, 677-686. #link("https://doi.org/10.1111/btp.12167")[#box(image("cv_files/mediabag/btp.12167-green.svg", height: 0.14583in))]

Salonen, J. S., Luoto, M., Alenius, T., Heikkilä, M., Seppä, H., #strong[Telford, R. J.], and Birks, H. J. B. (2014) Reconstructing palaeoclimatic variables from fossil pollen using boosted regression trees: comparison and synthesis with other quantitative reconstruction methods. #emph[Quaternary Science Reviews] 88, 69-81. #link("https://doi.org/10.1016/j.quascirev.2014.01.011")[#box(image("cv_files/mediabag/j.quascirev.2014.01..svg", height: 0.14583in))]

Tegzes, A. D., Jansen, E., and #strong[Telford, R. J.] (2014) The role of the northward-directed (sub)surface limb of the Atlantic Meridional Overturning Circulation during the 8.2 ka event. #emph[Climate of the Past] 10, 1887-1904. #link("https://doi.org/10.5194/cp-10-1887-2014")[#box(image("cv_files/mediabag/cp--10--1887--2014-g.svg", height: 0.14583in))]

Tian, F., Herzschuh, U., #strong[Telford, R. J.], Mischke, S., Meeren, T. V. der, and Krengel, M. (2014) A modern pollen-climate calibration set from central-western Mongolia and its application to a late glacial-Holocene record. #emph[Journal of Biogeography] 41, 1909-1922. #link("https://doi.org/10.1111/jbi.12338")[#box(image("cv_files/mediabag/jbi.12338-green.svg", height: 0.14583in))]

Bulafu, C., Barang, D., Eycott, A. E., Mucunguzi, P., #strong[Telford, R. J.], and Vandvik, V. (2013) Structural changes are more important than compositional changes in driving biomass loss in Ugandan forest fragments. #emph[Journal of Tropical Forestry and Environment] 3, 23-38. #link("https://doi.org/10.31357/jtfe.v3i2.1840")[#box(image("cv_files/mediabag/jtfe.v3i2.1840-green.svg", height: 0.14583in))]

Bulafu, C., Baranga, D., Mucunguzi, P., #strong[Telford, R. J.], and Vandvik, V. (2013) Massive structural and compositional changes over two decades in forest fragments near Kampala, Uganda. #emph[Ecology and Evolution] 3, 3804-3823. #link("https://doi.org/10.1002/ece3.747")[#box(image("cv_files/mediabag/ece3.747-green.svg", height: 0.14583in))]

Carstensen, J., #strong[Telford, R. J.], and Birks, H. J. B. (2013) Diatom flickering prior to regime shift. #emph[Nature] 498, E11-E12. #link("https://doi.org/10.1038/nature12272")[#box(image("cv_files/mediabag/nature12272-green.svg", height: 0.14583in))]

Kemp, A. C., #strong[Telford, R. J.], Horton, B. P., Anisfeld, S. C., and Sommerfield, C. K. (2013) Reconstructing Holocene sea level using salt-marsh foraminifera and transfer functions: lessons from New Jersey, USA. #emph[Journal of Quaternary Science] 28, 617-629. #link("https://doi.org/10.1002/jqs.2657")[#box(image("cv_files/mediabag/jqs.2657-green.svg", height: 0.14583in))]

Klemm, J., Herzschuh, U., Pisaric, M. F. J., #strong[Telford, R. J.], Heim, B., and Pestryakova, L. A. (2013) A pollen-climate transfer function from the tundra and taiga vegetation in Arctic Siberia and its applicability to a Holocene record. #emph[Palaeogeography, Palaeoclimatology, Palaeoecology] 386, 702-713. #link("https://doi.org/10.1016/j.palaeo.2013.06.033")[#box(image("cv_files/mediabag/j.palaeo.2013.06.033.svg", height: 0.14583in))]

#strong[Telford, R. J.], Li, C., and Kucera, M. (2013) Mismatch between the depth habitat of planktonic foraminifera and the calibration depth of SST transfer functions may bias reconstructions. #emph[Climate of the Past] 9, 859-870. #link("https://doi.org/10.5194/cp-9-859-2013")[#box(image("cv_files/mediabag/cp--9--859--2013-gre.svg", height: 0.14583in))]

Birks, H. H., Jones, V. J., Brooks, S. J., Birks, H. J. B., #strong[Telford, R. J.], Juggins, S., and Peglar, S. M. (2012) From cold to cool in northernmost Norway: Lateglacial and early Holocene multi-proxy environmental and climate reconstructions from Jansvatnet, Hammerfest. #emph[Quaternary Science Reviews] 33, 100-120. #link("https://doi.org/10.1016/j.quascirev.2011.11.013")[#box(image("cv_files/mediabag/j.quascirev.2011.11..svg", height: 0.14583in))]

Bouchet, V. M. P., Alve, E., Rygg, B., and #strong[Telford, R. J.] (2012) Benthic foraminifera provide a promising tool for ecological quality assessment of marine waters. #emph[Ecological Indicators] 23, 66-75. #link("https://doi.org/10.1016/j.ecolind.2012.03.011")[#box(image("cv_files/mediabag/j.ecolind.2012.03.01.svg", height: 0.14583in))]

Brooks, S. J., Jones, V. J., #strong[Telford, R. J.], Appleby, P. G., Watson, E., McGowan, S., and Benn, S. (2012) Population trends in the Slavonian grebe #emph[Podiceps auritus] (L.) and Chironomidae (Diptera) at a Scottish loch. #emph[Journal of Paleolimnology] 47, 631-644. #link("https://doi.org/10.1007/s10933-012-9587-4")[#box(image("cv_files/mediabag/s10933--012--9587--4.svg", height: 0.14583in))]

Payne\*, R. J., Telford\*, R. J., Blackford, J. J., Blundell, A., Booth, R. K., Charman, D. J., and others. (2012) Testing peatland testate amoeba transfer functions: Appropriate methods for clustered training-sets. #emph[The Holocene] 22, 819-825. #link("https://doi.org/10.1177/0959683611430412")[#box(image("cv_files/mediabag/0959683611430412-gre.svg", height: 0.14583in))]

Salonen, J. S., Ilvonen, L., Seppä, H., Holmström, L., #strong[Telford, R. J.], Gaidamavičius, A., Stančikaite, M., and Subetto, D. (2012) Comparing different calibration methods (WA/WA-PLS regression and Bayesian modelling) and different-sized calibration sets in pollen-based quantitative climate reconstruction. #emph[The Holocene] 22, 413-424. #link("https://doi.org/10.1177/0959683611425548")[#box(image("cv_files/mediabag/0959683611425548-gre.svg", height: 0.14583in))]

Velle, G., #strong[Telford, R. J.], Heiri, O., Kurek, J., and Birks, H. J. B. (2012) Testing intra-site transfer functions: an example using chironomids and water depth. #emph[Journal of Paleolimnology] 48, 545-558. #link("https://doi.org/10.1007/s10933-012-9630-5")[#box(image("cv_files/mediabag/s10933--012--9630--5.svg", height: 0.14583in))]

Austin, W. E. N., #strong[Telford, R. J.], Ninnemann, U. S., Brown, L., Wilson, L. J., Small, D. P., and Bryant, C. L. (2011) North Atlantic reservoir ages linked to high Younger Dryas atmospheric radiocarbon concentrations. #emph[Global and Planetary Change] 79, 226-233. #link("https://doi.org/10.1016/j.gloplacha.2011.06.011")[#box(image("cv_files/mediabag/j.gloplacha.2011.06..svg", height: 0.14583in))]

Hoogakker, B. A. A., Chapman, M. R., McCave, I. N., Hillaire-Marcel, C., Ellison, C. R. W., Hall, I. R., and #strong[Telford, R. J.] (2011) Dynamics of North Atlantic Deep Water masses during the Holocene. #emph[Paleoceanography] 26, PA4214. #link("https://doi.org/10.1029/2011PA002155")[#box(image("cv_files/mediabag/2011PA002155-green.svg", height: 0.14583in))]

Lloyd, J., Moros, M., Perner, K., #strong[Telford, R. J.], Kuijpers, A., Jansen, E., and McCarthy, D. (2011) A 100 yr record of ocean temperature control on the stability of Jakobshavn Isbrae, West Greenland. #emph[Geology] 39, 867-870. #link("https://doi.org/10.1130/G32076.1")[#box(image("cv_files/mediabag/G32076.1-green.svg", height: 0.14583in))]

Perner, K., Moros, M., Lloyd, J. M., Kuijpers, A., #strong[Telford, R. J.], and Harff, J. (2011) Centennial scale benthic foraminiferal record of late Holocene oceanographic variability in Disko Bugt, West Greenland. #emph[Quaternary Science Reviews] 30, 2815-2826. #link("https://doi.org/10.1016/j.quascirev.2011.06.018")[#box(image("cv_files/mediabag/j.quascirev.2011.06..svg", height: 0.14583in))]

#strong[Telford, R. J.] and Birks, H. J. B. (2011) QSR Correspondence "Is spatial autocorrelation introducing biases in the apparent accuracy of palaeoclimatic reconstructions?". #emph[Quaternary Science Reviews] 30, 3210-3213. #link("https://doi.org/10.1016/j.quascirev.2011.07.019")[#box(image("cv_files/mediabag/j.quascirev.2011.07..svg", height: 0.14583in))]

#strong[Telford, R. J.] and Birks, H. J. B. (2011) A novel method for assessing the statistical significance of quantitative reconstructions inferred from biotic assemblages. #emph[Quaternary Science Reviews] 30, 1272-1278. #link("https://doi.org/10.1016/j.quascirev.2011.03.002")[#box(image("cv_files/mediabag/j.quascirev.2011.03..svg", height: 0.14583in))]

#strong[Telford, R. J.] and Birks, H. J. B. (2011) Effect of uneven sampling along an environmental gradient on transfer-function performance. #emph[Journal of Paleolimnology] 46, 99-106. #link("https://doi.org/10.1007/s10933-011-9523-z")[#box(image("cv_files/mediabag/s10933--011--9523--z.svg", height: 0.14583in))]

Andersson, C., Pausata, F. S. R., Jansen, E., Risebrobakken, B., and #strong[Telford, R. J.] (2010) Holocene trends in the foraminifer record from the Norwegian Sea and the North Atlantic Ocean. #emph[Climate of the Past] 6, 179-193. #link("https://doi.org/10.5194/cp-6-179-2010")[#box(image("cv_files/mediabag/cp--6--179--2010-gre.svg", height: 0.14583in))]

Hobbs, W. O., #strong[Telford, R. J.], Birks, H. J. B., Saros, J. E., Hazewinkel, R. R. O., Perren, B. B., Saulnier-Talbot, É., and Wolfe, A. P. (2010) Quantifying recent ecological changes in remote lakes of North America and Greenland using sediment diatom assemblages. #emph[PLOS ONE] 5, 1-12. #link("https://doi.org/10.1371/journal.pone.0010026")[#box(image("cv_files/mediabag/journal.pone.0010026.svg", height: 0.14583in))]

Moros, M., Deckker, P. D., Jansen, E., Perner, K., and #strong[Telford, R. J.] (2009) Holocene climate variability in the Southern Ocean recorded in a deep-sea sediment core off South Australia. #emph[Quaternary Science Reviews] 28, 1932-1940. #link("https://doi.org/10.1016/j.quascirev.2009.04.007")[#box(image("cv_files/mediabag/j.quascirev.2009.04..svg", height: 0.14583in))]

Seppä, H., Bjune, A. E., #strong[Telford, R. J.], Birks, H. J. B., and Veski, S. (2009) Last nine-thousand years of temperature variability in Northern Europe. #emph[Climate of the Past] 5, 523-535. #link("https://doi.org/10.5194/cp-5-523-2009")[#box(image("cv_files/mediabag/cp--5--523--2009-gre.svg", height: 0.14583in))]

#strong[Telford, R. J.] and Birks, H. J. B. (2009) Evaluation of transfer functions in spatially structured environments. #emph[Quaternary Science Reviews] 28, 1309-1316. #link("https://doi.org/10.1016/j.quascirev.2008.12.020")[#box(image("cv_files/mediabag/j.quascirev.2008.12..svg", height: 0.14583in))]

Hald, M., Andersson, C., Ebbesen, H., Jansen, E., Klitgaard-Kristensen, D., Risebrobakken, B., Salomonsen, G. R., Sarnthein, M., Sejrup, H. P., and #strong[Telford, R. J.] (2007) Variations in temperature and extent of Atlantic Water in the northern North Atlantic during the Holocene. #emph[Quaternary Science Reviews] 26, 3423-3440. #link("https://doi.org/10.1016/j.quascirev.2007.10.005")[#box(image("cv_files/mediabag/j.quascirev.2007.10..svg", height: 0.14583in))]

Lamb, H. F., Leng, M. J., #strong[Telford, R. J.], Ayenew, T., and Umer, M. (2007) Oxygen and carbon isotope composition of authigenic carbonate from an Ethiopian lake: a climate record of the last 2000 years. #emph[The Holocene] 17, 517-526. #link("https://doi.org/10.1177/0959683607076452")[#box(image("cv_files/mediabag/0959683607076452-gre.svg", height: 0.14583in))]

Seppä, H., Birks, H. J. B., Giesecke, T., Hammarlund, D., and others including #strong[Telford, R. J.] (2007) Spatial structure of the 8200 cal yr BP event in northern Europe. #emph[Climate of the Past] 3, 225-236. #link("https://doi.org/10.5194/cp-3-225-2007")[#box(image("cv_files/mediabag/cp--3--225--2007-gre.svg", height: 0.14583in))]

#strong[Telford, R. J.], Vandvik, V., and Birks, H. J. B. (2007) Response to comment on \"Dispersal limitations matter for microbial morphospecies. #emph[Science] 316, 1124-1124. #link("https://doi.org/10.1126/science.1137697")[#box(image("cv_files/mediabag/science.1137697-gree.svg", height: 0.14583in))]

Antonsson, K., Brooks, S. J., Seppä, H., #strong[Telford, R. J.], and Birks, H. J. B. (2006) Quantitative palaeotemperature records inferred from fossil pollen and chironomid assemblages from Lake Gilltjärnen, northern central Sweden. #emph[Journal of Quaternary Science] 21, 831-841. #link("https://doi.org/10.1002/jqs.1004")[#box(image("cv_files/mediabag/jqs.1004-green.svg", height: 0.14583in))]

Clarke, A. L., Weckström, K., Conley, D. J., Anderson, N. J., and others including #strong[Telford, R. J.] (2006) Long-term trends in eutrophication and nutrients in the coastal zone. #emph[Limnology and Oceanography] 51, 385-397. #link("https://doi.org/10.4319/lo.2006.51.1_part_2.0385")[#box(image("cv_files/mediabag/lo.2006.51.1_part_2..svg", height: 0.14583in))]

#strong[Telford, R. J.] (2006) Limitations of dinoflagellate cyst transfer functions. #emph[Quaternary Science Reviews] 25, 1375-1382. #link("https://doi.org/10.1016/j.quascirev.2006.02.012")[#box(image("cv_files/mediabag/j.quascirev.2006.02..svg", height: 0.14583in))]

#strong[Telford, R. J.], Vandvik, V., and Birks, H. J. B. (2006) Dispersal limitations matter for microbial morphospecies. #emph[Science] 312, 1015-1015. #link("https://doi.org/10.1126/science.1125669")[#box(image("cv_files/mediabag/science.1125669-gree.svg", height: 0.14583in))]

#strong[Telford, R. J.], Vandvik, V., and Birks, H. J. B. (2006) How many freshwater diatoms are pH specialists? A response to Pither & Aarssen (2005). #emph[Ecology Letters] 9, E1-E5. #link("https://doi.org/10.1111/j.1461-0248.2005.00875.x")[#box(image("cv_files/mediabag/j.1461--0248.2005.00.svg", height: 0.14583in))]

Heegaard, E., Birks, H. J. B., and #strong[Telford, R. J.] (2005) Relationships between calibrated ages and depth in stratigraphical sequences: an estimation procedure by mixed-effect regression. #emph[The Holocene] 15, 612-618. #link("https://doi.org/10.1191/0959683605hl836rr")[#box(image("cv_files/mediabag/0959683605hl836rr-gr.svg", height: 0.14583in))]

Lamb, A. L., Leng, M. J., Sloane, H. J., and #strong[Telford, R. J.] (2005) A comparison of the palaeoclimate signals from diatom oxygen isotope ratios and carbonate oxygen isotope ratios from a low latitude crater lake. #emph[Palaeogeography, Palaeoclimatology, Palaeoecology] 223, 290-302. #link("https://doi.org/10.1016/j.palaeo.2005.04.011")[#box(image("cv_files/mediabag/j.palaeo.2005.04.011.svg", height: 0.14583in))]

Newton, A. J., Metcalfe, S. E., Davies, S. J., Cook, G., Barker, P., and #strong[Telford, R. J.] (2005) Late Quaternary volcanic record from lakes of Michoacán, central Mexico. #emph[Quaternary Science Reviews] 24, 91-104. #link("https://doi.org/10.1016/j.quascirev.2004.07.008")[#box(image("cv_files/mediabag/j.quascirev.2004.07..svg", height: 0.14583in))]

#strong[Telford, R. J.] and Birks, H. J. B. (2005) The secret assumption of transfer functions: problems with spatial autocorrelation in evaluating model performance. #emph[Quaternary Science Reviews] 24, 2173-2179. #link("https://doi.org/10.1016/j.quascirev.2005.05.001")[#box(image("cv_files/mediabag/j.quascirev.2005.05..svg", height: 0.14583in))]

#strong[Telford, R. J.], Andersson, C., Birks, H. J. B., and Juggins, S. (2004) Biases in the estimation of transfer function prediction errors. #emph[Paleoceanography] 19, PA4014. #link("https://doi.org/10.1029/2004PA001072")[#box(image("cv_files/mediabag/2004PA001072-green.svg", height: 0.14583in))]

#strong[Telford, R. J.], Barker, P., Metcalfe, S., and Newton, A. (2004) Lacustrine responses to tephra deposition: examples from Mexico. #emph[Quaternary Science Reviews] 23, 2337-2353. #link("https://doi.org/10.1016/j.quascirev.2004.03.014")[#box(image("cv_files/mediabag/j.quascirev.2004.03..svg", height: 0.14583in))]

#strong[Telford, R. J.], Heegaard, E., and Birks, H. J. B. (2004) All age-depth models are wrong: but how badly? #emph[Quaternary Science Reviews] 23, 1-5. #link("https://doi.org/10.1016/j.quascirev.2003.11.003")[#box(image("cv_files/mediabag/j.quascirev.2003.11..svg", height: 0.14583in))]

#strong[Telford, R. J.], Heegaard, E., and Birks, H. J. B. (2004) The intercept is a poor estimate of a calibrated radiocarbon age. #emph[The Holocene] 14, 296-298. #link("https://doi.org/10.1191/0959683604hl707fa")[#box(image("cv_files/mediabag/0959683604hl707fa-gr.svg", height: 0.14583in))]

Barker, P., #strong[Telford, R. J.], Gasse, F., and Thevenon, F. (2002) Late Pleistocene and Holocene palaeohydrology of Lake Rukwa, Tanzania, inferred from diatom analysis. #emph[Palaeogeography, Palaeoclimatology, Palaeoecology] 187, 295-305. #link("https://doi.org/10.1016/S0031-0182(02)00482-0")[#box(image("cv_files/mediabag/S0031--0182-02-00482.svg", height: 0.14583in))]

Lamb, A. L., Leng, M. J., Lamb, H. F., #strong[Telford, R. J.], and Mohammed, M. U. (2002) Climatic and non-climatic effects on the delta-18O and delta-13C compositions of Lake Awassa, Ethiopia, during the last 6.5ka. #emph[Quaternary Science Reviews] 21, 2199-2211. #link("https://doi.org/10.1016/S0277-3791(02)00087-2")[#box(image("cv_files/mediabag/S0277--3791-02-00087.svg", height: 0.14583in))]

Barker, P. A., Street-Perrott, F. A., Leng, M. J., Greenwood, P. B., Swain, D. L., Perrott, R. A., #strong[Telford, R. J.], and Ficken, K. J. (2001) A 14,000-year oxygen isotope record from diatom silica in two alpine lakes on Mt. Kenya. #emph[Science] 292, 2307-2310. #link("https://doi.org/10.1126/science.1059612")[#box(image("cv_files/mediabag/science.1059612-gree.svg", height: 0.14583in))]

Barker, P., #strong[Telford, R. J.], Merdaci, O., Williamson, D., Taieb, M., Vincens, A., and Gibert, E. (2000) The sensitivity of a Tanzanian crater lake to catastrophic tephra input and four millennia of climate change. #emph[The Holocene] 10, 303-310. #link("https://doi.org/10.1191/095968300672848582")[#box(image("cv_files/mediabag/095968300672848582-g.svg", height: 0.14583in))]

Leng, M. J., Lamb, A. L., Lamb, H. F., and #strong[Telford, R. J.] (1999) Palaeoclimatic implications of isotopic data from modern and early Holocene shells of the freshwater snail #emph[Melanoides tuberculata], from lakes in the Ethiopian Rift Valley. #emph[Journal of Paleolimnology] 21, 97-106. #link("https://doi.org/10.1023/A:1008079219280")[#box(image("cv_files/mediabag/A-1008079219280-gree.svg", height: 0.14583in))]

#strong[Telford, R. J.] and Lamb, H. F. (1999) Groundwater-mediated response to Holocene climatic change recorded by the diatom stratigraphy of an Ethiopian crater lake. #emph[Quaternary Research] 52, 63-75. #link("https://doi.org/10.1006/qres.1999.2034")[#box(image("cv_files/mediabag/qres.1999.2034-green.svg", height: 0.14583in))]

#strong[Telford, R. J.], Lamb, H. F., and Umer Mohammed, M. (1999) Diatom-derived palaeoconductivity estimates for Lake Awassa, Ethiopia: evidence for pulsed inflows of saline groundwater. #emph[Journal of Paleolimnology] 21, 409-422. #link("https://doi.org/10.1023/A:1008092823410")[#box(image("cv_files/mediabag/A-1008092823410-gree.svg", height: 0.14583in))]

== Book Chapters
<book-chapters>
Weckström, K., Lewis, J. P., Andrén, E., Ellegaard, M., Rasmussen, P., and #strong[Telford, R. J.] (2017) Palaeoenvironmental History of the Baltic Sea: One of the Largest Brackish-Water Ecosystems in the World. In: #emph[Applications of Paleoenvironmental Techniques in Estuarine Studies]. Ed. by Weckström, K., Saunders, K. M., Gell, P. A. and Skilbeck, C. G. Dordrecht: Springer Netherlands, pp.~615-662. #link("https://doi.org/10.1007/978-94-024-0990-1_24")[#box(image("cv_files/mediabag/978--94--024--0990--.svg", height: 0.14583in))]

Kemp, A. C. and #strong[Telford, R. J.] (2015) Transfer functions. In: #emph[Handbook of Sea-Level Research]. John Wiley & Sons, Ltd, pp.~470-499. #link("https://doi.org/10.1002/9781118452547.ch31")[#box(image("cv_files/mediabag/9781118452547.ch31-g.svg", height: 0.14583in))]

Juggins, S. and #strong[Telford, R. J.] (2012) Exploratory data analysis and data display. In: #emph[Tracking Environmental Change Using Lake Sediments: Data Handling and Numerical Techniques]. Ed. by Birks, H. J. B., Lotter, A. F., Juggins, S. and Smol, J. P. Dordrecht: Springer Netherlands, pp.~123-141. #link("https://doi.org/10.1007/978-94-007-2745-8_5")[#box(image("cv_files/mediabag/978--94--007--2745--.svg", height: 0.14583in))]

Jansen, E., Andersson, C., Moros, M., Nisancioglu, K. H., Nyland, B. F., and #strong[Telford, R. J.] (2009) The early to mid-Holocene thermal optimum in the North Atlantic. In: #emph[Natural Climate Variability and Global Warming]. Wiley Blackwell, pp.~123-137. #link("https://doi.org/10.1002/9781444300932.ch5")[#box(image("cv_files/mediabag/9781444300932.ch5-gr.svg", height: 0.14583in))]

Kebede, S., Lamb, H., #strong[Telford, R. J.], Leng, M., and Umer, M. (2002) Lake - groundwater relationships, oxygen isotope balance and climate sensitivity of the Bishoftu Crater Lakes, Ethiopia. In: #emph[The East African Great Lakes: Limnology, Palaeolimnology and Biodiversity]. Ed. by Odada, E. O. and Olago, D. O. Dordrecht: Springer Netherlands, pp. 261-275. #link("https://doi.org/10.1007/0-306-48201-0_9")[#box(image("cv_files/mediabag/0--306--48201--0_9-g.svg", height: 0.14583in))]

Kelly, M. G., Bayer, M. M., Hürlimann, J., and #strong[Telford, R. J.] (2002) Human error and quality assurance in diatom analysis. In: #emph[Automatic Diatom Identification]. Ed. by Buf, J. M. H. du and Bayer, M. M. Singapore: World Scientific, pp.~75-92. #link("https://doi.org/10.1142/9789812777867_0005")[#box(image("cv_files/mediabag/9789812777867_0005-g.svg", height: 0.14583in))]

Lamb, H., Kebede, S., Leng, M., Ricketts, D., #strong[Telford, R. J.], and Umer, M. (2002) Origin and isotopic composition of aragonite laminae in an Ethiopian crater lake. In: #emph[The East African Great Lakes: Limnology, Palaeolimnology and Biodiversity]. Ed. by Odada, E. O. and Olago, D. O. Dordrecht: Springer Netherlands, pp.~487-508. #link("https://doi.org/10.1007/0-306-48201-0_20")[#box(image("cv_files/mediabag/0--306--48201--0_20-.svg", height: 0.14583in))]

#strong[Telford, R. J.], Juggins, S., Kelly, M., and Ludes, B. (2002) Diatom applications. In: #emph[Automatic Diatom Identification]. Ed. by Buf, J. M. H. du and Bayer, M. M. Singapore: World Scientific, pp.~41-53. #link("https://doi.org/10.1142/9789812777867_0003")[#box(image("cv_files/mediabag/9789812777867_0003-g.svg", height: 0.14583in))]

== R packages
<r-packages>
#strong[Telford, R. J.] (2026) #emph[checker: Checks R Configuration Set Up Correctly Before Class]. R package version 0.1.5. #link("https://cran.r-project.org/package=checker")[#box(image("cv_files/mediabag/checker-color=green.svg", height: 0.14583in))]

#strong[Telford, R. J.], Halbritter, A. H., and Maitner, B. S. (2023) #emph[traitstrap: Bootstrap Trait Values to Calculate Moments]. R package version 0.1.0. #link("https://cran.r-project.org/package=traitstrap")[#box(image("cv_files/mediabag/traitstrap-color=gre.svg", height: 0.14583in))]

#strong[Telford, R. J.] and Trachsel, M. (2023) #emph[palaeoSig: Significance Tests of Quantitative Palaeoenvironmental Reconstructions]. R package version 2.1-3. #link("https://cran.r-project.org/package=palaeoSig")[#box(image("cv_files/mediabag/palaeoSig-color=gree.svg", height: 0.14583in))]

= Funding
<funding>
#cv-entry(
  title: "PalaeoDrivers: Quantifying the Drivers of Palaeoecological Change",
  organization: "Research Council of Norway",
  dates: "2012-2016",
  location: "PI",
  amount: "1400k NOK",
)
= Teaching
<teaching>
== Teaching Interests
<teaching-interests>
#service-list(("Open and Reproducible Science", "Quantitative Palaeoecology", "Multivariate Statistics", "Biostatistics with R"))
== Courses Taught
<courses-taught>
#cv-entry(
  title: "Bio300b Biostatistics",
  location: "Course leader",
  organization: "University of Bergen",
  dates: "2022–present",
)
#cv-entry(
  title: "Bio302 Methods for Open and Reproducible Science",
  organization: "University of Bergen",
  location: "Course leader",
  dates: "2025–present",
)
#cv-entry(
  title: "Bio303 Ordination and Gradient Analysis",
  organization: "University of Bergen",
  dates: "2007–present",
  location: "Course leader",
)
#cv-entry(
  title: "Bio250 Palaeoecology",
  organization: "University of Bergen",
  dates: "2007–present",
  location: "Team member",
)
#cv-entry(
  title: "Bio940 Open, Reproducible and Transparent Science in Ecology",
  organization: "University of Bergen/Living Norway",
  dates: "2022–present",
  location: "Team member",
)
#cv-entry(
  title: "Plant Functional Trait Course",
  organization: "University of Bergen/University of Arizona",
  dates: "2015–2023",
  location: "Team member",
)
#cv-entry(
  title: "Bio302 Biological data analysis II",
  organization: "University of Bergen",
  location: "Course leader",
  dates: "2007–2024",
)
#cv-entry(
  title: "R for Palaeoecologists",
  organization: "University of Cape Town",
  dates: "2019",
  location: "Course leader",
)
#cv-entry(
  title: "Recent Environmental Change ",
  organization: "University of Newcastle",
  dates: "2002",
  location: "Temp course leader",
)
#cv-entry(
  title: "Holocene Environmental Change",
  organization: "University of Lancaster",
  dates: "2000",
  location: "Temp course leader",
)
#cv-entry(
  title: "Physical Geography of the British Isles",
  organization: "University of Lancaster",
  dates: "2000",
  location: "Temp course leader",
)
== Workshops Taught
<workshops-taught>
#cv-entry(
  title: "Reproducible data analysis",
  organization: "Københavns Universitet",
  dates: "2018",
  location: "Workshop leader",
)
#cv-entry(
  title: "Age-depth modelling",
  organization: "2nd COST INTIMATE training school",
  dates: "2014",
  location: "Session leader",
)
#cv-entry(
  title: "Age-depth modelling",
  organization: "ECORD summer school",
  dates: "2013",
  location: "Session leader",
)
= Supervision
<supervision>
== Postdoc
<postdoc>
#cv-entry(
  title: "Amy Eycott",
  organization: "Matrix Project",
  dates: "2009–2015",
  location: "Co-supervisor",
)
#cv-entry(
  title: "Mathias Trachsel",
  organization: "PalaeoDrivers Project",
  dates: "2012–2016",
  location: "Supervisor",
)
== Ph.D
<ph.d>
#cv-entry(
  title: "Nadine Michaela Arzt",
  organization: "Mechanisms underlying the success of range-expanding species under climate change and their impacts on biodiversity and ecosystem functioning",
  dates: "2023–Ongoing",
  location: "Co-supervisor",
)
#cv-entry(
  title: "Cecilie Iden Nilsen",
  organization: "Acoustic telemetry of salmonids in lakes",
  dates: "2025–Ongoing",
  location: "Co-supervisor",
)
#cv-entry(
  title: "Mari Lie Larsen",
  organization: "Evidence-based policymaking: Exploring the effectiveness of salmon aquaculture management in Norway",
  dates: "2022–2025",
  location: "Co-supervisor",
)
#cv-entry(
  title: "Siri Vatsø Haugum",
  organization: "Land-use and climate impacts on drought resistance and resilience in coastal heathland ecosystems",
  dates: "2016–2021",
  location: "Co-supervisor",
)
#cv-entry(
  title: "Shad Kenneth Mahlum",
  organization: "From the fjords to the rivers: Evaluating the spatial distribution of escaped farmed salmon to inform ecologically relevant management strategies",
  dates: "2015–2020",
  location: "Co-supervisor",
)
#cv-entry(
  title: "Perpetra Akite",
  organization: "Spatial and matrix influences on the biogeography of insect taxa in forest fragments in central Uganda",
  dates: "2009–2015",
  location: "Co-supervisor",
)
#cv-entry(
  title: "Collins Bulafu",
  organization: "Diversity and reproductive traits of woody species in forest fragments in and around Kampala area, Uganda",
  dates: "2009–2014",
  location: "Co-supervisor",
)
== Masters
<masters>
#cv-entry(
  title: "Kirsti Rindal",
  organization: "Brain-gut-microbiota-axis study",
  dates: "2023–2024",
  location: "Co-supervisor",
)
#cv-entry(
  title: "Moss Tesfayesus",
  organization: "Mangrove forest extent and status along the Eritrean coast",
  dates: "2013–2014",
  location: "Co-supervisor",
)
#cv-entry(
  title: "Sofie Söderström",
  organization: "Interspecific whale associations in the Norwegian Seas",
  dates: "2011–2012",
  location: "Supervisor",
)
#cv-entry(
  title: "Gunnar Kvifte",
  organization: "MATRIX: Psychodidae of Budongo Forest",
  dates: "2009–2011",
  location: "Supervisor",
)
#cv-entry(
  title: "Moreen Uwimbabazi",
  organization: "MATRIX: Bird diversity in forest fragments around Budongo Forest",
  dates: "2008–2010",
  location: "Co-supervisor",
)
#cv-entry(
  title: "Kristoffer Hauge",
  organization: "MATRIX: Bat biodiversity in tropical forest fragments",
  dates: "2008–2010",
  location: "Supervisor",
)
#cv-entry(
  title: "Therese Kronstad",
  organization: "MATRIX: Butterfly biodiversity in Ugandan tropical forest fragments",
  dates: "2008–2010",
  location: "Supervisor",
)
#cv-entry(
  title: "Wang Manfei",
  organization: "Population dynamics of trees colonizing coastal heathlands on Lurøy",
  dates: "2007–2008",
  location: "Co-supervisor",
)
= Professional Memberships & Service
<professional-memberships-service>
234-345 2345--3456

== Reviewing
<reviewing>
#text(style: "italic")[African Journal of Ecology], #text(style: "italic")[Aquaculture], #text(style: "italic")[Boreas], #text(style: "italic")[Dendrochrologia], #text(style: "italic")[Ecography], #text(style: "italic")[Ecology and Evolution], #text(style: "italic")[Ecology Letters], #text(style: "italic")[Environmental Science and Pollution Research International], #text(style: "italic")[Flora], #text(style: "italic")[Geophysical Research Letters], #text(style: "italic")[Global Change Biology], #text(style: "italic")[Global Ecology and Biogeography], #text(style: "italic")[Journal of Biogeography], #text(style: "italic")[Journal of Paleolimnology], #text(style: "italic")[Methods in Ecology and Evolution], #text(style: "italic")[Nature Communications], #text(style: "italic")[Nature Geoscience], #text(style: "italic")[PloS one], #text(style: "italic")[Science of the Total Environment], #text(style: "italic")[Scientific Reports], #text(style: "italic")[The Holocene], #text(style: "italic")[The International Journal of Sustainable Development and World Ecology]
= Skills & Software
<skills-software>

#cv-entry(
  title: "Methods",
  description: "Quantitative palaeoecology, age-depth modelling, trait-based ecology, biostatistics"
)

#cv-entry(
  title: "Software",
  description: {
    "R (expert), shiny (expert), Git (proficient), Quarto (expert), R package development (expert)"
  }
)