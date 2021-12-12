library(tuber)

#belum verifikasi
yt_oauth("847825789552-7uh5t2glt407g28nruvedldf9ldqhktn.apps.googleusercontent.com",
         "XOUC-O5SvmQpFmDMFA_quJay", token = "")


#yt scraping2 
yt_oauth(app_id = "123117760113-61nkct1lijno2kf936l8ocpv0vs0s9j4.apps.googleusercontent.com",
         app_secret = "Sow5-XmyzBUUVDADZTcICoih", token = "")


get_channel_stats(channel_id = "UC1e0zbVMqAANE6z-i91pX_A")

get_stats(video_id="UCwljrkoI5jsfvAKgW3zNC7Q")


get_video_details(video_id="N708P-A45D0")

get_captions(video_id="yJXTXN4xrI8")

res <- yt_search("Barack Obama")
head(res[, 1:3])


comment <- get_comment_threads(c(video_id="N708P-A45D0"))
head(comment)



a <- list_channel_resources(filter = c(channel_id = "UClEZXxVmQTI5o-ZFT_HDXvA"), part="contentDetails")

# Uploaded playlists:
playlist_id <- a$items[[1]]$contentDetails$relatedPlaylists$uploads

# Get videos on the playlist
vids <- get_playlist_items(filter= c(playlist_id=playlist_id)) 

# Video ids
vid_ids <- as.vector(vids$contentDetails.videoId)

# Function to scrape stats for all vids
get_all_stats <- function(id) {
  get_stats(id)
} 

# Get stats and convert results to data frame 
res <- lapply(vid_ids, get_all_stats)
res_df <- do.call(rbind, lapply(res, data.frame))

head(res_df)