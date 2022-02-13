library(googledrive)

# authorize google drive
drive_auth(
  email = gargle::gargle_oauth_email(),
  path = NULL,
  scopes = "https://www.googleapis.com/auth/drive",
  cache = gargle::gargle_oauth_cache(),
  use_oob = gargle::gargle_oob_default(),
  token = NULL
)

drive_ls(n_max = 2)

folder <- drive_get("Aksara Jawa/train")
folder
folder$id

drive_find(id = folder$id)

f <- drive_get(id = folder$id) %>% view()



img <- drive_get("Aksara Jawa/train/ba/ba.0.jpg")
img$id

drive_get(id = img$id)
drive_download("Aksara Jawa/train/ba/ba.0.jpg")
