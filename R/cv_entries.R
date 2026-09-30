
process_entries <- function(yaml, section) {
  entries <- yaml[[section]]
  entries |> 
    fix_short() |> 
    write_section()
}

fix_short <- function(entries, short = params$short) {
  if(params$short) {# remove elements only for long CV
    entries <- entries |>
      keep(\(x)!is.null(x$short) && x$short) |> 
      map(\(x)list_modify(x, description = zap()))
  }
  entries |> 
    map(\(x)list_modify(x, short = zap()))
}

write_entry <- function(entry) {
  cat("#cv-entry(\n")
  purrr::walk2(.x = names(entry), .y = entry, \(names, values){paste0("  ", names, ": \"", values, "\",\n") |> cat()})
  cat(")\n")
}

write_section <- function(entries) {
  cat("```{=typst}\n")
  entries |> purrr::walk(write_entry)
  
  cat("```\n")
}

reviewing <- function(journals) {
  paste0('#text(style: "italic")[', journals, ']') |> paste(collapse = ", ")
}
