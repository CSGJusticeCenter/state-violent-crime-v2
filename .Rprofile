source("renv/activate.R")

### RStudio on Windows can start without Git on PATH; add the standard install if so
if (.Platform$OS.type == "windows" && !nzchar(Sys.which("git"))) {
  local({
    git_bin <- file.path(Sys.getenv("ProgramFiles"), "Git", "bin")
    if (dir.exists(git_bin)) {
      Sys.setenv(PATH = paste(git_bin, Sys.getenv("PATH"), sep = .Platform$path.sep))
    }
  })
}
