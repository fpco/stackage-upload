# stackage-upload

[![Build Status](https://travis-ci.org/fpco/stackage-upload.svg?branch=master)](https://travis-ci.org/fpco/stackage-upload)

`stackage-upload` provides a more secure version of the `cabal upload` command
by using HTTPS. When uploading a package to Hackage, `cabal upload` will
perform the upload in plain-text via unencrypted HTTP, using [basic
authentication](https://github.com/haskell/cabal/blob/2b3a69490a2dbd6dfa3905b8a50ebad8aec7b276/cabal-install/Distribution/Client/Upload.hs#L56),
which is vulnerable to both man in the middle (MITM) and eavesdropping attacks
(anyone sniffing your packets can get your username and password). This package
instead uses secure HTTPS to upload to avoid both of these attacks.

To install, simply run `cabal update && cabal install stackage-upload`. Usage
is quite similar to  `cabal upload`: just call `stackage-upload` and pass in a list
of tarballs to upload. (If you have
[stackage-cli](https://github.com/fpco/stackage-cli) installed, you can call
`stk upload` instead.) `stackage-upload --help` will provide full options.

## Why not fix cabal?

I'd be happy to add TLS support to cabal-install directly (using Vincent's
`tls` package), but the two last times this topic came up, I have been unable
to find a proposal that is acceptable to the Cabal project (mostly around
Haskell Platform requirements). I made an open offer to send the pull request
myself to move cabal-install over to http-client to get TLS support (either via
http-client-tls or http-client-openssl).

## Why Stackage?

See [the same question and its answer on stackage-update](https://github.com/fpco/stackage-update#why-stackage).

## Future enhancements

* Store passwords securely via GPG encryption
* Upload documentation to Hackage (work around the sometimes-broken doc builder)
* Perform pre-upload checks, such as running the test suite from the tarball to check for missing files

## History

This tool was something that I (Michael Snoyman) wrote for myself a while back,
and decided to rebrand as `stackage-upload` when the severity of the insecure
upload situation became apparent to me, and it became obvious that there was no
path forward for getting `cabal-install` fixed.

I actually consider this situation to be so dangerous that I would like to ask the
Hackage Server team to consider turning off insecure uploads to Hackage. The
current possibility for corrupted uploads to infect all users of a package is
alarmingly high.
