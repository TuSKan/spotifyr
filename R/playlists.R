#' Get a List of a User’s Playlists
#' Get a list of the playlists owned or followed by a Spotify user
#'
#' @references \href{https://developer.spotify.com/web-api/get-list-users-playlists/}{API documentation}
#'
#' @param user_id The user's Spotify user ID.
#'
#' @export
#'
#' @examples
#' set_tokens()
#' get_user_playlists(user_id = 'rweyant')
get_user_playlists <- function(user_id, ...){
  response <- GET(url = glue('{USER_URL}/{user_id}/playlists'),
                  add_headers(Authorization = glue('Bearer {access_token}')),
                  query = list(...))
  content <- get_response_content(response)

  df <- dplyr::bind_cols( content$items %>% purrr::map_df(magrittr::extract, c("name","id","public","collaborative","uri","href")),
                          content$items %>% purrr::map("owner") %>% purrr::map_df(magrittr::extract, c("id")) %>% purrr::set_names("owner.id"),
                          content$items %>% purrr::map("tracks") %>% purrr::map_df(magrittr::extract, c("total")) %>% purrr::set_names("tracks.total"))

  df[c("name","id","owner.id","tracks.total","public","collaborative","uri","href")]
}


#' Get a Playlist
#' Get a playlist owned by a Spotify user.
#'
#' @references \href{https://developer.spotify.com/web-api/get-playlist/}{API documentation}
#'
#' @param user_id The user's Spotify user ID.
#' @param playlist_id The Spotify ID for the playlist.
#'
#' @export
#'
#' @examples
#' set_tokens()
#' get_playlist(user_id = 'spotify', playlist_id = '59ZbFPES4DQwEjBpWHzrtC')
get_playlist <- function(user_id, playlist_id,...){
  response <- GET(url = glue('{USER_URL}/{user_id}/playlists/{playlist_id}'),
                  add_headers(Authorization = glue('Bearer {access_token}')),
                  query = list(...))
  content <- get_response_content(response)
  df <- dplyr::bind_cols( content %>% purrr::map(1L) %>%
                      purrr::flatten() %>%
                      tibble::as_tibble() %>%
                      magrittr::extract(c("name","id","description","public","collaborative")),

                    tibble::tibble(owner.id = content$owner$id,
                                   tracks.total = length(content$tracks$items),
                                   followers.total = content$followers$total)
                    )
  df[c("name","id","description","tracks.total","owner.id","followers.total","public","collaborative")]
}


#' Get a Playlist’s Tracks
#' Get full details of the tracks of a playlist owned by a Spotify user.
#'
#' @references \href{https://developer.spotify.com/web-api/get-playlists-tracks/}{API documentation}
#'
#' @param user_id The user's Spotify user ID.
#' @param playlist_id The Spotify ID for the playlist.
#'
#' @export
#'
#' @examples
#' set_tokens()
#' get_playlist_tracks(user_id = 'spotify', playlist_id = '59ZbFPES4DQwEjBpWHzrtC')
get_playlist_tracks <- function(user_id,playlist_id,...){
  response <- GET(url = glue('{USER_URL}/{user_id}/playlists/{playlist_id}/tracks/'),
                  add_headers(Authorization = glue('Bearer {access_token}')),
                  query = list(...))
  content <- get_response_content(response)
  content
  dplyr::bind_cols( content$items %>%  purrr::map("track") %>% purrr::map_df(magrittr::extract, c("name","id","track_number","disc_number","duration_ms","popularity","explicit")),
                    content$items %>%  purrr::map("track") %>% purrr::map("album") %>% purrr::map_df(magrittr::extract, c("name","id")) %>% purrr::set_names(c("album.name", "album.id")),
                    content$items %>%  purrr::map("track") %>% purrr::map("artists") %>% purrr::map_df(bind_cols) %>% select(matches("name[0-9]?|id[0-9]?")) %>% purrr::set_names(paste0("artists.", colnames(.)) )
  )
}


#' Create a Playlist
#' Create a playlist for a Spotify user. (The playlist will be empty until you add tracks.)
#'
#' @references \href{https://developer.spotify.com/web-api/create-playlist/}{API documentation}
#'
#' @param user_id The user's Spotify user ID.
#' @param name Required. The name for the new playlist, for example "Your
#'   Coolest Playlist". This name does not need to be unique; a user may have
#'   several playlists with the same name.
#'
#' @export
#'
#' @examples
#' set_tokens()
#' user_auth()
#' create_playlist(user_id = 'rweyant', name = 'spotifyr-test')
create_playlist <- function(user_id, name,...){
  response <- POST(url = glue('{USER_URL}/{user_id}/playlists/'),
                   config(token = user_token),
                   body = list(name = name,...),
                   encode = 'json')
  get_response_content(response)
}


#' Add Tracks to a Playlist
#' Add one or more tracks to a users playlist
#'
#' @references \href{https://developer.spotify.com/web-api/add-tracks-to-playlist/}{API documentation}
#'
#' @param user_id The user's Spotify user ID.
#' @param name Required. The name for the new playlist, for example "Your
#'   Coolest Playlist". This name does not need to be unique; a user may have
#'   several playlists with the same name.
#' @param uris Optional. A comma-separated list of Spotify track URIs to add.
#'   For example: uris=spotify:track:4iV5W9uYEdYUVa79Axb7Rh,
#'   spotify:track:1301WleyT98MSxVHPZCA6M
#'
#' @export
#'
#' @examples
#' set_tokens()
#' user_auth()
#' add_tracks_to_playlist(user_id = 'rweyant', playlist_id = '4Ej1T0FVGBCmwXapPGK2eP', uris = 'spotify:track:4S47FRAZ3VcyZS87FnQtmk')
#' add_tracks_to_playlist(user_id = 'rweyant',
#'                        playlist_id = '4Ej1T0FVGBCmwXapPGK2eP',
#'                        uris = c('spotify:track:4S47FRAZ3VcyZS87FnQtmk', 'spotify:track:3MXA2BkBk0lSuMpRoM7SK2'))
add_tracks_to_playlist <- function(user_id, playlist_id, uris){

  # TODO fix this hack
  if (length(uris) == 1) {
    payload_uris <- glue('{{"uris": ["{uris}"]}}')
  } else {
    payload_uris = list(uris = uris)
  }

  response <- POST(url = glue('{USER_URL}/{user_id}/playlists/{playlist_id}/tracks'),
                   config(token = user_token),
                   body = payload_uris,
                   encode = 'json')
  get_response_content(response)
}

#' Remove Tracks from a Playlist
#' Remove one or more tracks from a user’s playlist.
#'
#' @references \href{https://developer.spotify.com/web-api/remove-tracks-playlist/}{API documentation}
#'
#' @param user_id The user's Spotify user ID.
#' @param name Required. The name for the new playlist, for example "Your
#'   Coolest Playlist". This name does not need to be unique; a user may have
#'   several playlists with the same name.
#' @param uris Optional. A comma-separated list of Spotify track URIs to add.
#'   For example: uris=spotify:track:4iV5W9uYEdYUVa79Axb7Rh,
#'   spotify:track:1301WleyT98MSxVHPZCA6M
#'
#' @export
#'
#' @examples
#' set_tokens()
#' user_auth()
#' remove_tracks_from_playlist(user_id = 'rweyant', playlist_id = '4Ej1T0FVGBCmwXapPGK2eP', uris = 'spotify:track:4S47FRAZ3VcyZS87FnQtmk')
#' remove_tracks_from_playlist(user_id = 'rweyant',
#'                             playlist_id = '4Ej1T0FVGBCmwXapPGK2eP',
#'                             uris = c('spotify:track:4S47FRAZ3VcyZS87FnQtmk', 'spotify:track:3MXA2BkBk0lSuMpRoM7SK2'))
remove_tracks_from_playlist <- function(user_id,playlist_id, uris){

  response <- DELETE(url = glue('{USER_URL}/{user_id}/playlists/{playlist_id}/tracks'),
                     config(token = user_token),
                     body = list(tracks = data.frame(uri = uris)),
                     encode = 'json')

  get_response_content(response)
}


# TODO https://developer.spotify.com/web-api/reorder-playlists-tracks/
# TODO https://developer.spotify.com/web-api/replace-playlists-tracks/
# TODO https://developer.spotify.com/web-api/change-playlist-details/
