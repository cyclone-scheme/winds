((array-list
   (((cyclone array-list)
     (array-list?
       array-list-delete!
       array-list-empty?
       array-list-insert!
       array-list-length
       array-list-ref
       array-list-set!
       make-array-list))))
 (base64
   (((cyclone base64)
     (base64-encode
       base64-encode-string
       base64-encode-bytevector
       base64-decode
       base64-decode-string
       base64-decode-bytevector
       base64-encode-header))))
 (bytevector
   (((cyclone bytevector)
     (bytevector-u16-ref-le
       bytevector-u16-ref-be
       bytevector-u32-ref-le
       bytevector-u32-ref-be
       bytevector-ber-ref
       bytevector-ber-set!
       bytevector-pad-left
       integer->bytevector
       bytevector->integer
       integer->hex-string
       hex-string->integer
       bytevector->hex-string
       hex-string->bytevector
       bytevector-ieee-single-native-ref
       bytevector-ieee-single-set!
       bytevector-ieee-single-native-set!
       bytevector-ieee-double-ref
       bytevector-ieee-double-native-ref
       bytevector-ieee-double-set!
       bytevector-ieee-double-native-set!))))
 (char-set
   (((cyclone char-set)
     (Char-Set
       char-set?
       char-set-contains?
       char-set
       ucs-range->char-set
       char-set-copy
       char-set-size
       char-set-fold
       char-set-for-each
       list->char-set
       char-set->list
       string->char-set
       char-set->string
       char-set-adjoin!
       char-set-adjoin
       char-set-union
       char-set-union!
       char-set-intersection
       char-set-intersection!
       char-set-difference
       char-set-difference!
       immutable-char-set
       char-set-complement
       char-set:empty
       char-set:ascii
       char-set:full))
    ((cyclone char-set full)
     (char-set:lower-case
       char-set:upper-case
       char-set:title-case
       char-set:letter
       char-set:digit
       char-set:letter+digit
       char-set:graphic
       char-set:printing
       char-set:whitespace
       char-set:iso-control
       char-set:punctuation
       char-set:symbol
       char-set:hex-digit
       char-set:blank))
    ((cyclone char-set boundary)
     (char-set:regional-indicator
       char-set:extend-or-spacing-mark
       char-set:hangul-l
       char-set:hangul-v
       char-set:hangul-t
       char-set:hangul-lv
       char-set:hangul-lvt))
    ((cyclone char-set ascii)
     (char-set:lower-case
       char-set:upper-case
       char-set:title-case
       char-set:letter
       char-set:digit
       char-set:letter+digit
       char-set:graphic
       char-set:printing
       char-set:whitespace
       char-set:iso-control
       char-set:punctuation
       char-set:symbol
       char-set:hex-digit
       char-set:blank))
    ((cyclone char-set base)
     (Char-Set
       char-set?
       immutable-char-set
       char-set-contains?))
    ((cyclone char-set extras)
     (char-set
       ucs-range->char-set
       char-set-copy
       char-set-size
       char-set-fold
       char-set-for-each
       list->char-set
       char-set->list
       string->char-set
       char-set->string
       char-set-adjoin!
       char-set-adjoin
       char-set-union
       char-set-union!
       char-set-intersection
       char-set-intersection!
       char-set-difference
       char-set-difference!
       char-set-complement
       char-set:empty
       char-set:ascii
       char-set:full))))
 (clojurian
   (((cyclone clojurian)
     (doto as-> and-> -> ->* ->> ->>* if-let if-let*))))
 (csv (((cyclone csv)
        (make-csv-reader
          csv-read
          csv-read-all
          make-csv-writer
          csv-write
          csv-write-all))))
 (curl (((cyclone curl)
         (curl-global-init
           curl-easy-init
           curl-easy-cleanup
           curl-easy-setopt
           curl-easy-perform
           curl-version
           CURLOPT_ACCEPTTIMEOUT_MS
           CURLOPT_ACCEPT_ENCODING
           CURLOPT_ADDRESS_SCOPE
           CURLOPT_APPEND
           CURLOPT_AUTOREFERER
           CURLOPT_BUFFERSIZE
           CURLOPT_CAINFO
           CURLOPT_CAPATH
           CURLOPT_CERTINFO
           CURLOPT_CHUNK_BGN_FUNCTION
           CURLOPT_CHUNK_DATA
           CURLOPT_CHUNK_END_FUNCTION
           CURLOPT_CLOSESOCKETDATA
           CURLOPT_CLOSESOCKETFUNCTION
           CURLOPT_CONNECTTIMEOUT
           CURLOPT_CONNECTTIMEOUT_MS
           CURLOPT_CONNECT_ONLY
           CURLOPT_CONV_FROM_NETWORK_FUNCTION
           CURLOPT_CONV_FROM_UTF8_FUNCTION
           CURLOPT_CONV_TO_NETWORK_FUNCTION
           CURLOPT_COOKIE
           CURLOPT_COOKIEFILE
           CURLOPT_COOKIEJAR
           CURLOPT_COOKIELIST
           CURLOPT_COOKIESESSION
           CURLOPT_COPYPOSTFIELDS
           CURLOPT_CRLF
           CURLOPT_CRLFILE
           CURLOPT_CUSTOMREQUEST
           CURLOPT_DEBUGDATA
           CURLOPT_DEBUGFUNCTION
           CURLOPT_DEFAULT_PROTOCOL
           CURLOPT_DIRLISTONLY
           CURLOPT_DNS_CACHE_TIMEOUT
           CURLOPT_DNS_INTERFACE
           CURLOPT_DNS_LOCAL_IP4
           CURLOPT_DNS_LOCAL_IP6
           CURLOPT_DNS_SERVERS
           CURLOPT_DNS_USE_GLOBAL_CACHE
           CURLOPT_EGDSOCKET
           CURLOPT_ERRORBUFFER
           CURLOPT_EXPECT_100_TIMEOUT_MS
           CURLOPT_FAILONERROR
           CURLOPT_FILETIME
           CURLOPT_FNMATCH_DATA
           CURLOPT_FNMATCH_FUNCTION
           CURLOPT_FOLLOWLOCATION
           CURLOPT_FORBID_REUSE
           CURLOPT_FRESH_CONNECT
           CURLOPT_FTPPORT
           CURLOPT_FTPSSLAUTH
           CURLOPT_FTP_ACCOUNT
           CURLOPT_FTP_ALTERNATIVE_TO_USER
           CURLOPT_FTP_CREATE_MISSING_DIRS
           CURLOPT_FTP_FILEMETHOD
           CURLOPT_FTP_RESPONSE_TIMEOUT
           CURLOPT_FTP_SKIP_PASV_IP
           CURLOPT_FTP_SSL_CCC
           CURLOPT_FTP_USE_EPRT
           CURLOPT_FTP_USE_EPSV
           CURLOPT_FTP_USE_PRET
           CURLOPT_GSSAPI_DELEGATION
           CURLOPT_HEADER
           CURLOPT_HEADERDATA
           CURLOPT_HEADERFUNCTION
           CURLOPT_HEADEROPT
           CURLOPT_HTTP200ALIASES
           CURLOPT_HTTPAUTH
           CURLOPT_HTTPGET
           CURLOPT_HTTPHEADER
           CURLOPT_HTTPPOST
           CURLOPT_HTTPPROXYTUNNEL
           CURLOPT_HTTP_CONTENT_DECODING
           CURLOPT_HTTP_TRANSFER_DECODING
           CURLOPT_HTTP_VERSION
           CURLOPT_IGNORE_CONTENT_LENGTH
           CURLOPT_INFILESIZE
           CURLOPT_INFILESIZE_LARGE
           CURLOPT_INTERFACE
           CURLOPT_INTERLEAVEDATA
           CURLOPT_INTERLEAVEFUNCTION
           CURLOPT_IOCTLDATA
           CURLOPT_IOCTLFUNCTION
           CURLOPT_IPRESOLVE
           CURLOPT_ISSUERCERT
           CURLOPT_KEYPASSWD
           CURLOPT_KRBLEVEL
           CURLOPT_LOCALPORT
           CURLOPT_LOCALPORTRANGE
           CURLOPT_LOGIN_OPTIONS
           CURLOPT_LOW_SPEED_LIMIT
           CURLOPT_LOW_SPEED_TIME
           CURLOPT_MAIL_AUTH
           CURLOPT_MAIL_FROM
           CURLOPT_MAIL_RCPT
           CURLOPT_MAXCONNECTS
           CURLOPT_MAXFILESIZE
           CURLOPT_MAXFILESIZE_LARGE
           CURLOPT_MAXREDIRS
           CURLOPT_MAX_RECV_SPEED_LARGE
           CURLOPT_MAX_SEND_SPEED_LARGE
           CURLOPT_NETRC
           CURLOPT_NETRC_FILE
           CURLOPT_NEW_DIRECTORY_PERMS
           CURLOPT_NEW_FILE_PERMS
           CURLOPT_NOBODY
           CURLOPT_NOPROGRESS
           CURLOPT_NOPROXY
           CURLOPT_NOSIGNAL
           CURLOPT_OPENSOCKETDATA
           CURLOPT_OPENSOCKETFUNCTION
           CURLOPT_PASSWORD
           CURLOPT_PATH_AS_IS
           CURLOPT_PINNEDPUBLICKEY
           CURLOPT_PIPEWAIT
           CURLOPT_PORT
           CURLOPT_POST
           CURLOPT_POSTFIELDS
           CURLOPT_POSTFIELDSIZE
           CURLOPT_POSTFIELDSIZE_LARGE
           CURLOPT_POSTQUOTE
           CURLOPT_POSTREDIR
           CURLOPT_PREQUOTE
           CURLOPT_PRIVATE
           CURLOPT_PROGRESSDATA
           CURLOPT_PROGRESSFUNCTION
           CURLOPT_PROTOCOLS
           CURLOPT_PROXY
           CURLOPT_PROXYAUTH
           CURLOPT_PROXYHEADER
           CURLOPT_PROXYPASSWORD
           CURLOPT_PROXYPORT
           CURLOPT_PROXYTYPE
           CURLOPT_PROXYUSERNAME
           CURLOPT_PROXYUSERPWD
           CURLOPT_PROXY_SERVICE_NAME
           CURLOPT_PROXY_TRANSFER_MODE
           CURLOPT_PUT
           CURLOPT_QUOTE
           CURLOPT_RANDOM_FILE
           CURLOPT_RANGE
           CURLOPT_READDATA
           CURLOPT_READFUNCTION
           CURLOPT_REDIR_PROTOCOLS
           CURLOPT_REFERER
           CURLOPT_RESOLVE
           CURLOPT_RESUME_FROM
           CURLOPT_RESUME_FROM_LARGE
           CURLOPT_RTSP_CLIENT_CSEQ
           CURLOPT_RTSP_REQUEST
           CURLOPT_RTSP_SERVER_CSEQ
           CURLOPT_RTSP_SESSION_ID
           CURLOPT_RTSP_STREAM_URI
           CURLOPT_RTSP_TRANSPORT
           CURLOPT_SASL_IR
           CURLOPT_SEEKDATA
           CURLOPT_SEEKFUNCTION
           CURLOPT_SERVICE_NAME
           CURLOPT_SHARE
           CURLOPT_SOCKOPTDATA
           CURLOPT_SOCKOPTFUNCTION
           CURLOPT_SOCKS5_GSSAPI_NEC
           CURLOPT_SOCKS5_GSSAPI_SERVICE
           CURLOPT_SSH_AUTH_TYPES
           CURLOPT_SSH_HOST_PUBLIC_KEY_MD5
           CURLOPT_SSH_KEYDATA
           CURLOPT_SSH_KEYFUNCTION
           CURLOPT_SSH_KNOWNHOSTS
           CURLOPT_SSH_PRIVATE_KEYFILE
           CURLOPT_SSH_PUBLIC_KEYFILE
           CURLOPT_SSLCERT
           CURLOPT_SSLCERTTYPE
           CURLOPT_SSLENGINE
           CURLOPT_SSLENGINE_DEFAULT
           CURLOPT_SSLKEY
           CURLOPT_SSLKEYTYPE
           CURLOPT_SSLVERSION
           CURLOPT_SSL_CIPHER_LIST
           CURLOPT_SSL_CTX_DATA
           CURLOPT_SSL_CTX_FUNCTION
           CURLOPT_SSL_ENABLE_ALPN
           CURLOPT_SSL_ENABLE_NPN
           CURLOPT_SSL_FALSESTART
           CURLOPT_SSL_OPTIONS
           CURLOPT_SSL_SESSIONID_CACHE
           CURLOPT_SSL_VERIFYHOST
           CURLOPT_SSL_VERIFYPEER
           CURLOPT_SSL_VERIFYSTATUS
           CURLOPT_STDERR
           CURLOPT_STREAM_DEPENDS
           CURLOPT_STREAM_DEPENDS_E
           CURLOPT_STREAM_WEIGHT
           CURLOPT_TCP_KEEPALIVE
           CURLOPT_TCP_KEEPIDLE
           CURLOPT_TCP_KEEPINTVL
           CURLOPT_TCP_NODELAY
           CURLOPT_TELNETOPTIONS
           CURLOPT_TFTP_BLKSIZE
           CURLOPT_TIMECONDITION
           CURLOPT_TIMEOUT
           CURLOPT_TIMEOUT_MS
           CURLOPT_TIMEVALUE
           CURLOPT_TLSAUTH_PASSWORD
           CURLOPT_TLSAUTH_TYPE
           CURLOPT_TLSAUTH_USERNAME
           CURLOPT_TRANSFERTEXT
           CURLOPT_TRANSFER_ENCODING
           CURLOPT_UNIX_SOCKET_PATH
           CURLOPT_UNRESTRICTED_AUTH
           CURLOPT_UPLOAD
           CURLOPT_URL
           CURLOPT_USERAGENT
           CURLOPT_USERNAME
           CURLOPT_USERPWD
           CURLOPT_USE_SSL
           CURLOPT_VERBOSE
           CURLOPT_WILDCARDMATCH
           CURLOPT_WRITEDATA
           CURLOPT_WRITEFUNCTION
           CURLOPT_XFERINFODATA
           CURLOPT_XFERINFOFUNCTION
           CURLOPT_XOAUTH2_BEARER))))
 (irregex
   (((cyclone irregex)
     (irregex
       string->irregex
       sre->irregex
       string->sre
       maybe-string->sre
       irregex?
       irregex-match-data?
       irregex-new-matches
       irregex-reset-matches!
       irregex-search
       irregex-search/matches
       irregex-match
       irregex-search/chunked
       irregex-match/chunked
       make-irregex-chunker
       irregex-match-substring
       irregex-match-subchunk
       irregex-match-start-chunk
       irregex-match-start-index
       irregex-match-end-chunk
       irregex-match-end-index
       irregex-match-num-submatches
       irregex-match-names
       irregex-match-valid-index?
       irregex-match
       irregex-search
       irregex-fold
       irregex-replace
       irregex-replace/all
       irregex-dfa
       irregex-dfa/search
       irregex-nfa
       irregex-flags
       irregex-lengths
       irregex-names
       irregex-num-submatches
       irregex-extract
       irregex-split))))
 (iset (((cyclone iset)
         (%make-iset
           make-iset
           iset?
           iset-contains?
           Integer-Set
           iset
           iset-copy
           list->iset
           list->iset!
           iset-adjoin
           iset-adjoin!
           iset-delete
           iset-delete!
           iset-union
           iset-union!
           iset-intersection
           iset-intersection!
           iset-difference
           iset-difference!
           iset-empty?
           iset-fold
           iset-fold-node
           iset-for-each
           iset-for-each-node
           iset-map
           iset->list
           iset-size
           iset=
           iset<=
           iset>=
           iset-cursor
           iset-cursor?
           iset-cursor-next
           iset-ref
           end-of-iset?))
        ((cyclone iset constructors)
         (iset iset-copy
               list->iset
               list->iset!
               iset-map
               iset-adjoin
               iset-adjoin!
               iset-delete
               iset-delete!
               iset-union
               iset-union!
               iset-intersection
               iset-intersection!
               iset-difference
               iset-difference!
               iset-copy-node
               iset-squash-bits!
               iset-insert-left!
               iset-insert-right!))
        ((cyclone iset optimize)
         (iset-balance
           iset-balance!
           iset-optimize
           iset-optimize!
           iset->code))
        ((cyclone iset base)
         (%make-iset
           make-iset
           iset?
           iset-contains?
           Integer-Set
           iset-start
           iset-end
           iset-bits
           iset-left
           iset-right
           iset-start-set!
           iset-end-set!
           iset-bits-set!
           iset-left-set!
           iset-right-set!))
        ((cyclone iset iterators)
         (iset-empty?
           iset-fold
           iset-fold-node
           iset-for-each
           iset-for-each-node
           iset->list
           iset-size
           iset=
           iset<=
           iset>=
           iset-cursor
           iset-cursor?
           iset-cursor-next
           iset-ref
           end-of-iset?))))
 (json (((cyclone json)
         (json-write json-read json->scm scm->json))))
 (md5 (((cyclone crypto md5) (md5))))
 (mime (((cyclone mime)
         (assq-ref
           mime-header-fold
           mime-headers->list
           mime-parse-content-type
           mime-decode-header
           mime-message-fold
           mime-message->sxml
           mime-write-headers))))
 (packrat
   (((cyclone packrat)
     (parse-result?
       parse-result-successful?
       parse-result-semantic-value
       parse-result-next
       parse-result-error
       parse-results?
       parse-results-position
       parse-results-base
       parse-results-next
       parse-error?
       parse-error-position
       parse-error-expected
       parse-error-messages
       make-parse-position
       parse-position?
       parse-position-file
       parse-position-line
       parse-position-column
       top-parse-position
       update-parse-position
       parse-position->string
       make-error-expected
       make-error-message
       make-result
       make-expected-result
       make-message-result
       prepend-base
       prepend-semantic-value
       base-generator->results
       results->result
       parse-position>?
       parse-error-empty?
       merge-parse-errors
       merge-result-errors
       parse-results-token-kind
       parse-results-token-value
       packrat-check-base
       packrat-check
       packrat-or
       packrat-unless
       packrat-parser))))
 (pathname
   (((cyclone pathname)
     (path-strip-directory
       path-directory
       path-extension
       path-strip-extension
       path-replace-extension
       path-absolute?
       path-relative?
       path-strip-leading-parents
       path-relative-to
       path-resolve
       path-normalize
       make-path))))
 (popen (((cyclone io popen) (popen pclose))))
 (postgresql
   (((cyclone postgresql)
     (make-postgresql-connection
       postgresql-connection?
       postgresql-open-connection!
       postgresql-secure-connection!
       postgresql-login!
       postgresql-terminate!
       postgresql-query?
       postgresql-query-descriptions
       postgresql-execute-sql!
       postgresql-prepared-statement?
       postgresql-prepared-statement
       postgresql-prepared-statement-sql
       postgresql-bind-parameters!
       postgresql-execute!
       postgresql-close-prepared-statement!
       postgresql-fetch-query!
       postgresql-start-transaction!
       postgresql-commit!
       postgresql-rollback!
       *postgresql-maximum-results*
       *postgresql-date-format*
       *postgresql-time-format*
       *postgresql-timestamp-format*
       *postgresql-copy-data-handler*
       *postgresql-write-data-handler*
       *postgresql-notice-handler*
       *postgresql-unknown-type-handler*
       postgresql-transaction-mode
       postgresql-isolation-level-serializable
       postgresql-isolation-level-repeatable-read
       postgresql-isolation-level-read-committed
       postgresql-isolation-level-read-uncommitted
       postgresql-access-mode-read-write
       postgresql-access-mode-read-only
       postgresql-deferrable-on
       postgresql-deferrable-off
       postgresql-error?
       postgresql-error-severity
       postgresql-error-code
       postgresql-error-schema
       postgresql-error-table
       postgresql-error-column
       postgresql-error-data-type
       postgresql-error-constraint))
    ((cyclone postgresql conditions)
     (raise-postgresql-error
       postgresql-error?
       postgresql-error-severity
       postgresql-error-code
       postgresql-error-schema
       postgresql-error-table
       postgresql-error-column
       postgresql-error-data-type
       postgresql-error-constraint))
    ((cyclone postgresql messages)
     (postgresql-send-ssl-request
       postgresql-send-startup-message
       postgresql-send-password-message
       postgresql-send-terminate-message
       postgresql-send-sync-message
       postgresql-send-flush-message
       postgresql-send-query-message
       postgresql-send-parse-message
       postgresql-send-bind-message
       postgresql-send-describe-message
       postgresql-send-execute-message
       postgresql-send-close-message
       postgresql-send-copy-data-message
       postgresql-send-copy-fail-message
       postgresql-send-copy-done-message
       postgresql-read-response))
    ((cyclone postgresql apis)
     (make-postgresql-connection
       postgresql-connection?
       postgresql-open-connection!
       postgresql-secure-connection!
       postgresql-login!
       postgresql-terminate!
       postgresql-query?
       postgresql-query-descriptions
       postgresql-execute-sql!
       postgresql-prepared-statement?
       postgresql-prepared-statement
       postgresql-prepared-statement-sql
       postgresql-bind-parameters!
       postgresql-execute!
       postgresql-close-prepared-statement!
       *postgresql-maximum-results*
       *postgresql-date-format*
       *postgresql-time-format*
       *postgresql-timestamp-format*
       *postgresql-copy-data-handler*
       *postgresql-write-data-handler*
       *postgresql-notice-handler*
       *postgresql-unknown-type-handler*
       postgresql-fetch-query!
       postgresql-start-transaction!
       postgresql-commit!
       postgresql-rollback!
       postgresql-transaction-mode
       postgresql-isolation-level-serializable
       postgresql-isolation-level-repeatable-read
       postgresql-isolation-level-read-committed
       postgresql-isolation-level-read-uncommitted
       postgresql-access-mode-read-write
       postgresql-access-mode-read-only
       postgresql-deferrable-on
       postgresql-deferrable-off))
    ((cyclone postgresql buffer)
     (postgresql-send-ssl-request
       postgresql-send-startup-message
       postgresql-send-password-message
       postgresql-send-terminate-message
       postgresql-send-sync-message
       postgresql-send-flush-message
       postgresql-send-query-message
       postgresql-send-parse-message
       postgresql-send-bind-message
       postgresql-send-describe-message
       postgresql-send-execute-message
       postgresql-send-close-message
       postgresql-send-copy-data-message
       postgresql-send-copy-fail-message
       postgresql-send-copy-done-message
       postgresql-read-response
       make-postgresql-out-buffer))
    ((cyclone postgresql misc io)
     (write-u16-be write-u32-be))
    ((cyclone postgresql misc ssl)
     (socket->ssl-socket
       ssl-socket-input-port
       ssl-socket-output-port
       ssl-socket?
       ssl-socket-close))))
 (python
   (((cyclone python)
     (py-start
       py-stop
       py-import
       py-run-simple-string
       py-run-file
       py-value
       py-def
       py-def-attribute
       py-def-method
       py
       ->string
       %py-initialize
       %py-finalize
       %py-incref
       %py-decref
       %py-callable-check
       %py-err-occurred
       %py-err-clear
       %py-err-as-string
       %raise-python-exception
       %python-exception-string
       %py-error
       %py-object-call-object
       %py-object-call
       %py-object-get-attr-string
       %py-object-set-attr-string
       %py-object-str
       %py-object-type
       %long->py-bool
       %py-bool->bool
       %long->py-long
       %py-long->long
       %py-float->double
       %py-float-from-double
       %string->py-str
       %py-str->string
       %py-str->string-and-size
       %py-dict-new
       %py-dict-keys
       %py-dict-size
       %py-dict-get-item
       %py-dict-get-item-string
       %py-dict-items
       %py-dict-set-item
       %py-list-new
       %py-list-size
       %py-list-get-item
       %py-list-set-item
       %py-tuple-new
       %py-tuple-size
       %py-tuple-get-item
       %py-tuple-set-item
       %py-object-get-buffer
       %py-buffer->contiguous
       %contiguous->py-buffer
       %py-buffer-release
       %py-buffer-size
       %py-import
       %py-import-module
       %py-import-module-ex
       %py-import-get-module-dict
       %py-import-add-module
       %py-module-get-dict
       %py-module-get-dict-as-ptr
       %py-module-add-object
       %py-run-simple-string
       %py-run-string
       %py-run-file
       %py-eval
       %py-apply
       *py-functions*
       %scm->python
       %python->scm))))
 (quoted-printable
   (((cyclone quoted-printable)
     (quoted-printable-encode
       quoted-printable-encode-string
       quoted-printable-encode-bytevector
       quoted-printable-encode-header
       quoted-printable-decode
       quoted-printable-decode-string
       quoted-printable-decode-bytevector))))
 (sha2 (((cyclone crypto sha2) (sha-224 sha-256))))
 (srfi-145 (((srfi 145) (assume))))
 (srfi-152
   (((srfi 152)
     (string=?
       string<?
       string>?
       string<=?
       string>=?
       string-ci=?
       string-ci<?
       string-ci>?
       string-ci<=?
       string-ci>=?
       string-null?
       string-every
       string-any
       string-tabulate
       string-unfold
       string-unfold-right
       reverse-list->string
       string-take
       string-drop
       string-take-right
       string-drop-right
       string-pad
       string-pad-right
       string-trim
       string-trim-right
       string-trim-both
       string-replace
       string-prefix-length
       string-suffix-length
       string-prefix?
       string-suffix?
       string-index
       string-index-right
       string-skip
       string-skip-right
       string-contains
       string-contains-right
       string-take-while
       string-take-while-right
       string-drop-while
       string-drop-while-right
       string-break
       string-span
       string-concatenate
       string-concatenate-reverse
       string-join
       string-fold
       string-fold-right
       string-count
       string-filter
       string-remove
       string-replicate
       string-segment
       string-split))))
 (srfi-173
   (((srfi 173)
     (make-hook
       hook?
       list->hook
       list->hook!
       hook-add!
       hook-delete!
       hook-reset!
       hook->list
       hook-run))))
 (srfi-197
   (((srfi 197)
     (chain chain-and
            chain-when
            chain-lambda
            nest
            nest-reverse))))
 (srfi-26 (((srfi 26) (cut cute))))
 (srfi-41
   (((srfi 41)
     (make-stream
       make-stream-pair
       make-stream-null
       stream-promise
       stream-null
       stream-cons
       stream?
       stream-null?
       stream-pair?
       stream-car
       stream-cdr
       stream-lambda
       define-stream
       list->stream
       port->stream
       stream
       stream->list
       stream-append
       stream-concat
       stream-constant
       stream-drop
       stream-drop-while
       stream-filter
       stream-fold
       stream-for-each
       stream-from
       stream-iterate
       stream-length
       stream-let
       stream-map
       stream-match
       stream-of
       stream-range
       stream-ref
       stream-reverse
       stream-scan
       stream-take
       stream-take-while
       stream-unfold
       stream-unfolds
       stream-zip))))
 (string
   (((cyclone string)
     (string-cursor?
       string-cursor-start
       string-cursor-end
       string-cursor-ref
       string-cursor<?
       string-cursor<=?
       string-cursor>?
       string-cursor>=?
       string-cursor=?
       string-cursor-next
       string-cursor-prev
       substring-cursor
       string-cursor->index
       string-index->cursor
       string-cursor-forward
       string-cursor-back
       string-null?
       string-every
       string-any
       string-join
       string-split
       string-count
       string-trim
       string-trim-left
       string-trim-right
       string-mismatch
       string-mismatch-right
       string-prefix?
       string-suffix?
       string-find
       string-find-right
       string-find?
       string-skip
       string-skip-right
       string-fold
       string-fold-right
       string-map
       string-for-each
       string-contains
       make-string-searcher
       string-downcase-ascii
       string-upcase-ascii
       call-with-input-string
       call-with-output-string))))
 (sxml (((cyclone sxml)
         (sxml->xml
           sxml-display-as-html
           sxml-display-as-text
           sxml-strip
           html-escape
           html-tag->string))))
 (syslog
   (((cyclone syslog)
     (open-log
       send-log
       openlog
       closelog
       syslog
       setlogmask
       LOG_PID
       LOG_USER
       EMERG
       ALERT
       CRIT
       ERR
       WARNING
       NOTICE
       INFO
       DEBUG))))
 (temple
   (((cyclone web temple)
     (render get-parse-tree build-parse-tree))
    ((cyclone web temple parser)
     (parse *read-size* string-pos))
    ((cyclone web temple trace)
     (trace set-trace-level!))))
 (uri (((cyclone uri)
        (uri? uri->string
              make-uri
              string->uri
              string->path-uri
              uri-has-scheme?
              uri-scheme
              uri-user
              uri-host
              uri-port
              uri-path
              uri-query
              uri-fragment
              uri-with-scheme
              uri-with-user
              uri-with-host
              uri-with-path
              uri-with-query
              uri-with-fragment
              uri-resolve
              uri-encode
              uri-decode
              uri-query->alist
              uri-alist->query)))))
