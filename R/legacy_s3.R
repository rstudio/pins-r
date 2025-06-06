#' S3 board (legacy API)
#'
#' @description
#' `r lifecycle::badge('deprecated')`
#'
#' To use an Amazon S3 Storage board, you need an Amazon S3 bucket and a user
#' with enough permissions to access the S3 bucket. You can sign-up and create
#' those at <https://aws.amazon.com/>. Note that it can take a few minutes
#' after you've created it before a bucket is usable.
#'
#' See [board_s3()] for a modern version of this legacy board.
#'
#' @inheritParams legacy_datatxt
#' @param bucket The name of the Amazon S3 bucket.
#' @param key,secret The key and secret for your space. You can create
#'   a key and secret in the "Spaces access keys" in your API settings.
#'
#'  The `secret` is equivalent to a password, so generally should not be stored
#'  in your script. The easiest alternative is to store it in the
#'  `AWS_SECRET_ACCESS_KEY` environment variable, which `board_s3()` will
#'  use by default.
#' @param host The host to use for storage, defaults to `"s3.amazonaws.com"`.
#' @param region The region to use, required in some AWS regions and to
#'   enable V4 signatures.
#' @examples
#' \dontrun{
#' # the following example requires an Amazon S3 API key
#' board <- legacy_s3(bucket = "s3bucket")
#' }
#' @export
#' @keywords internal
legacy_s3 <- function(
  bucket = Sys.getenv("AWS_BUCKET"),
  key = Sys.getenv("AWS_ACCESS_KEY_ID"),
  secret = Sys.getenv("AWS_SECRET_ACCESS_KEY"),
  cache = NULL,
  region = NULL,
  host = "s3.amazonaws.com",
  name = "s3",
  ...
) {
  if (nchar(bucket) == 0) stop("The 's3' board requires a 'bucket' parameter.")
  if (nchar(key) == 0) stop("The 's3' board requires a 'key' parameter.")
  if (nchar(secret) == 0) stop("The 's3' board requires a 'secret' parameter.")

  legacy_datatxt(
    name = name,
    url = paste0("https://", bucket, ".", host),
    cache = cache,
    headers = s3_headers,
    needs_index = FALSE,
    key = key,
    secret = secret,
    bucket = bucket,
    region = region,
    connect = FALSE,
    browse_url = paste0(
      "https://s3.console.aws.amazon.com/s3/buckets/",
      bucket,
      "/"
    ),
    host = host,
    ...
  )
}

#' @rdname legacy_s3
#' @export
board_register_s3 <- function(
  name = "s3",
  bucket = Sys.getenv("AWS_BUCKET"),
  key = Sys.getenv("AWS_ACCESS_KEY_ID"),
  secret = Sys.getenv("AWS_SECRET_ACCESS_KEY"),
  cache = NULL,
  host = "s3.amazonaws.com",
  region = NULL,
  path = NULL,
  ...
) {
  lifecycle::deprecate_warn(
    "1.4.0",
    "board_register_s3()",
    details = 'Learn more at <https://pins.rstudio.com/articles/pins-update.html>'
  )

  legacy_s3(
    name = name,
    bucket = bucket,
    key = key,
    secret = secret,
    cache = cache,
    region = region,
    path = path,
    ...
  )
}


# See https://docs.amazonaws.cn/en_us/general/latest/gr/sigv4-signed-request-examples.html#sig-v4-examples-get-auth-header
# httr::GET("https://ec2.amazonaws.com?Action=DescribeRegions&Version=2013-10-15", pins:::s3_headers_v4()) %>% httr::text_content()
s3_headers_v4 <- function(board, verb, path, filepath) {
  check_installed("openssl")
  service <- "s3"
  method <- verb
  bucket <- board$bucket
  host <- paste0(bucket, ".", board$host)
  region <- board$region
  request_parameters <- ""
  amz_storage_class <- "REDUCED_REDUNDANCY"
  if (!is.null(filepath)) {
    amz_content_sha256 <- digest::digest(filepath, file = TRUE, algo = "sha256")
  } else {
    amz_content_sha256 <- digest::digest(
      enc2utf8(""),
      serialize = FALSE,
      algo = "sha256"
    )
  }
  content_type <- "application/octet-stream"

  # Key derivation functions. See:
  # http://docs.aws.amazon.com/general/latest/gr/signature-v4-examples.html#signature-v4-examples-python
  sign <- function(key, msg) {
    openssl::sha256(charToRaw(enc2utf8(msg)), key = key)
  }

  getSignatureKey <- function(key, dateStamp, regionName, serviceName) {
    kDate <- sign(paste0("AWS4", key), dateStamp)
    kRegion <- sign(kDate, regionName)
    kService <- sign(kRegion, serviceName)
    kSigning <- sign(kService, "aws4_request")
    kSigning
  }

  # Read AWS access key from env. variables or configuration file. Best practice is NOT
  # to embed credentials in code.
  access_key <- board$key
  secret_key <- board$secret

  # Create a date for headers and the credential string
  amzdate <- format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "GMT")
  datestamp <- format(Sys.time(), "%Y%m%d", tz = "GMT")

  # ************* TASK 1: CREATE A CANONICAL REQUEST *************
  # http://docs.aws.amazon.com/general/latest/gr/sigv4-create-canonical-request.html

  # Step 1 is to define the verb (GET, POST, etc.)--already done.

  # Step 2: Create canonical URI--the part of the URI from domain to query
  # string (use "/" if no path)
  canonical_uri <- file.path("", path)

  # Step 3: Create the canonical query string. In this example (a GET request),
  # request parameters are in the query string. Query string values must
  # be URL-encoded (space=%20). The parameters must be sorted by name.
  # For this example, the query string is pre-formatted in the request_parameters variable.
  canonical_querystring <- request_parameters

  # Step 4: Create the canonical headers. Header names must be trimmed
  # and lowercase, and sorted in code point order from low to high.
  # Note that there is a trailing \n.
  canonical_headers <- paste0(
    # "content-type:", content_type, "\n",
    "host:",
    host,
    "\n",
    "x-amz-content-sha256:",
    amz_content_sha256,
    "\n",
    "x-amz-date:",
    amzdate,
    "\n",
    # "x-amz-storage-class:", amz_storage_class, "\n",
    ""
  )

  # Step 5: Create the list of signed headers. This lists the headers
  # signed_headers <- "content-type;host;x-amz-content-sha256;x-amz-date;x-amz-storage-class"
  signed_headers <- "host;x-amz-content-sha256;x-amz-date"

  # Step 6: Create payload hash. In this example, the payload (body of
  # the request) contains the request parameters.
  payload_hash <- amz_content_sha256

  # Step 7: Combine elements to create canonical request
  canonical_request <- paste0(
    method,
    "\n",
    canonical_uri,
    "\n",
    canonical_querystring,
    "\n",
    canonical_headers,
    "\n",
    signed_headers,
    "\n",
    payload_hash
  )

  # ************* TASK 2: CREATE THE STRING TO SIGN*************
  # Match the algorithm to the hashing algorithm you use, either SHA-1 or
  # SHA-256 (recommended)
  algorithm <- "AWS4-HMAC-SHA256"
  credential_scope <- paste0(
    datestamp,
    "/",
    board$region,
    "/",
    service,
    "/",
    "aws4_request"
  )
  string_to_sign <- paste0(
    algorithm,
    "\n",
    amzdate,
    "\n",
    credential_scope,
    "\n",
    digest::digest(
      enc2utf8(canonical_request),
      serialize = FALSE,
      algo = "sha256"
    )
  )

  # ************* TASK 3: CALCULATE THE SIGNATURE *************
  # Create the signing key using the function defined above.
  signing_key <- getSignatureKey(secret_key, datestamp, region, service)

  # Sign the string_to_sign using the signing_key
  signature <- openssl::sha256(string_to_sign, key = signing_key) %>%
    as.character()

  # ************* TASK 4: ADD SIGNING INFORMATION TO THE REQUEST *************
  # Put the signature information in a header named Authorization.
  authorization_header <- paste0(
    algorithm,
    " ",
    "Credential=",
    board$key,
    "/",
    credential_scope,
    ", ",
    "SignedHeaders=",
    signed_headers,
    ", ",
    "Signature=",
    signature
  )

  headers <- httr::add_headers(
    # "Host" = host,
    # "Content-Type" = content_type,
    "x-amz-content-sha256" = amz_content_sha256,
    "x-amz-date" = amzdate,
    # "x-amz-storage-class" = amz_storage_class,
    "Authorization" = authorization_header
  )

  headers
}

s3_headers <- function(board, verb, path, file) {
  check_installed("openssl")
  date <- http_date()

  # allow full urls to allow arbitrary file downloads
  bucket <- board$bucket
  if (grepl("^https?://", path)) {
    path_nohttp <- gsub("^https?://", "", path)
    path <- gsub("^[^/]+/", "", path_nohttp)
    bucket <- gsub("\\..*", "", path_nohttp)
  }

  if (!identical(board$region, NULL)) {
    headers <- s3_headers_v4(board, verb, path, file)
  } else {
    content <- paste(
      verb,
      "",
      "application/octet-stream",
      date,
      file.path("", bucket, path),
      sep = "\n"
    )

    signature <- openssl::sha1(charToRaw(content), key = board$secret) %>%
      jsonlite::base64_enc()

    headers <- httr::add_headers(
      Host = paste0(bucket, ".", board$host),
      Date = date,
      `Content-Type` = "application/octet-stream",
      Authorization = paste0("AWS ", board$key, ":", signature)
    )
  }

  headers
}
