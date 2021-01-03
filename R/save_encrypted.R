#' save_encrypted
#'
#' A function to encrypt and save Rdata files
#' @return NULL (invisible)
#' @param data the object to encrypt
#' @param file the file path to save to
#' @importFrom sodium data_encrypt
#' @export
#'
save_encrypted <- function(data, file = "") {
  secrets <- load_key_and_nonce()
  msg <- serialize(data, NULL)
  cipher <- data_encrypt(msg, secrets$key, secrets$nonce)
  save(cipher, file = file)
  return(invisible(NULL))
}

#' load_encrypted
#'
#' A function to load encrypted .Rdata files
#' @return the decrypted file
#' @param file the file to load
#' @importFrom magrittr %>%
#' @importFrom sodium data_decrypt
#' @export
#'
load_encrypted <- function(file = "") {
  secrets <- load_key_and_nonce()
  load(file, envir = environment())
  cipher <- data_decrypt(cipher, secrets$key, secrets$nonce)
  data <- unserialize(cipher)
  return(data)
}

#' load_key_and_nonce
#'
#' A function to load the secret key and nonce
#' @return the the key and nonce
#' @importFrom magrittr %>%
#' @importFrom sodium data_decrypt
#' @export
#'
load_key_and_nonce <- function() {
  if (Sys.getenv('ENVIRONMENT') == 'production') {
    key <- Sys.getenv("ENCRYPTION_KEY")
    nonce <- Sys.getenv("NONCE")
  } else {
    load('inst/keys/sodium_key.Rdata', envir = environment())
  }

  key <- convert_secret(key)
  nonce <- convert_secret(nonce)
  return(list(key = key,
              nonce = nonce))
}

#' convert_secret
#'
#' A function to convert the secret from char to raw
#' @return the raw secret
#' @param secret the string secret
#' @importFrom magrittr %>%
#' @importFrom stringr str_split
#'
convert_secret <- function(secret) {
  secret %>%
    str_split(' ') %>%
    unlist() %>%
    as.integer() %>%
    packBits()
}


