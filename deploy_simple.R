library(connectapi)
client <- connect(server = Sys.getenv("CONNECT_SERVER"),
                  api_key = Sys.getenv("CONNECT_API_KEY"))

file.copy(from = "./public/articles/logo.png", 
          to = "./public/reference/logo.png")

rsconnect::writeManifest(appDir = "./inst/shiny")
bundle <- bundle_dir("./inst/shiny")

content <- client %>%
  deploy(bundle, name = "HASP_aquifers") %>%
  poll_task()

rsconnect::writeManifest(appDir = "./inst/single_site")
bundle <- bundle_dir("./inst/single_site")

content <- client %>%
  deploy(bundle, name = "HASP_Single_Site") %>%
  poll_task()

