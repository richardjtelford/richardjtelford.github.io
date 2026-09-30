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
  title: "$name$ - CV", 
  author: "$name$"
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
  paper: "$papersize$",
  margin: (left: 15mm, right: 15mm, top: 10mm, bottom: 10mm),
  footer: context [
    #set text(fill: color-lightgray, size: 8pt)
    #grid(
      columns: (1fr, 1fr, 1fr),
      align(left)[Revised #datetime.today().display("[month repr:long] [year]")],
      align(center)[$firstname$ $lastname$ · CV],
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
      #text(weight: "bold")[$firstname$]
      #text(weight: "bold")[$lastname$]
    ]
  ]
  
  #set block(above: 0.75em, below: 0.75em)
  #set text(color-darkgray, size: 12pt, weight: "regular")
  #block[$affiliation$]
  
  #v(0.5em)
  
  #set text(size: 9pt, weight: "regular", style: "normal", fill: color-darkgray)
  #grid(
    columns: (1fr, 1fr, 1fr),
    column-gutter: 0.5em,
    align: (left, left, left),
    [
      #fa-icon("location-dot") $address$ \
      #fa-icon("envelope") #link("mailto:$email$")[#text(fill: color-darkgray)[$email$]] \
      #fa-icon("orcid", font: "Font Awesome 6 Brands") #link("$orcidurl$")[#text(fill: color-darkgray)[$orcidhandle$]]
    ],
    [
      #h(2em) #fa-icon("phone") $params.phone$ \
      #h(2em) #fa-icon("earth-europe") #link("$websiteurl$")[#text(fill: color-darkgray)[$websitedisplay$]] \
      #h(2em) #fa-icon("linkedin", font: "Font Awesome 6 Brands") #link("$linkedin$")[#text(fill: color-darkgray)[$linkedinhandle$]]
    ],
    [
      #h(-0.5em) #fa-icon("twitter", font: "Font Awesome 6 Brands") #link("$twitter$")[#text(fill: color-darkgray)[\@$twitterhandle$]] \
      #h(-0.5em) #fa-icon("github", font: "Font Awesome 6 Brands") #link("$github$")[#text(fill: color-darkgray)[$githubhandle$]] \
      #h(-0.5em) #ai-icon("google-scholar") #link("$google-scholar$")[#text(fill: color-darkgray)[$google-scholarhandle$]]
    ]
  )
]

#v(1em)

$body$