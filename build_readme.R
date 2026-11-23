# PLANTILLA DENGUEVERSE · genera README.md (inglés) y README.es.md (español)
# Uso (desde la raíz del paquete, con el paquete instalado):
#   source("build_readme.R")
for (lang in c("en", "es")) {
    rmarkdown::render("README.Rmd",
                      params = list(lang = lang),
                      output_file = if (lang == "en") "README.md" else "README.es.md",
                      quiet = TRUE)
}
unlink(c("README.html", "README.es.html"))
message("Listo: README.md y README.es.md")
