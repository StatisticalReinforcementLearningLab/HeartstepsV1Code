## compile the summary document
## usage: Rscript --vanilla summary.R [delete-local]

args <- commandArgs(TRUE)

library(knitr)

knit("summary.Rnw")
system("pdflatex summary")
system("pdflatex summary")


if ("delete-local" %in% args) {
  file.copy("summary.pdf", sys.var$mbox, overwrite = TRUE)
  unlink(list.files(pattern = "(^figure$|^summary.[^R]+)"), recursive = TRUE)
  file.copy("workspace.csv.Rout", sys.var$mbox.data, overwrite = TRUE)
  file.copy("workspace.analysis.Rout", sys.var$mbox.data, overwrite = TRUE)
  unlink(list.files(pattern = "^workspace.*.Rout"), recursive = TRUE)
}
