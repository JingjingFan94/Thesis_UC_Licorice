# Render the six Rmd documents to HTML. Run from the repository root.
if (!requireNamespace("rmarkdown", quietly = TRUE))
  stop("Install 'rmarkdown' first: install.packages('rmarkdown')")
for (i in 1:6) {
  file <- sprintf("fig%d/fig%d.Rmd", i, i)
  message("Rendering ", file)
  rmarkdown::render(file, output_format = "html_document", quiet = FALSE)
}
message("All Rmd files rendered. Check each figN/ folder for the HTML output.")
