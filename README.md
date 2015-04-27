# stackage-upload

A more secure version of cabal upload which uses HTTPS. When uploading a
package to Hackage, `cabal-install` will perform the upload in plain-text,
unencrypted HTTP, which is vulnerable to man in the middle (MITM) attacks. This
package instead uses secure HTTPS upload to avoid both MITM attacks, and
possibly eavesdropping attacks (though the latter are as yet unproven). In the
future, additionally functionality may be added.

To install, simply run `cabal update && cabal install stackage-upload`. Usage
is quite similar to  `cabal upload`: just call `stackage-upload` and pass in a list
of tarballs to upload. (If you have
[stackage-cli](https://github.com/fpco/stackage-cli) installed, you can call
`stk upload` instead.) `stackage-upload --help` will provide full options.

## Why not fix cabal?

A legitimate question is why not add HTTPS support to cabal-install? The answer
is that I tried. At least as of April 2015, there was no proposal I was able to
make that allowed TLS support to be added to cabal-install, due to policies
regarding dependencies in the Cabal project. I would be much happier to add
this support there (and, at the same time, add secure *download* support, which
is also severely lacking). I made an open offer in April 2015, and the offer
stands: if the Cabal project gives me the green light to add http-client as a
dependency to cabal-install, I'll send the pull request myself.

To give some more background: Cabal currently requires that all dependencies
be part of the Haskell Platform. I disagree with this decision, since
distributing a binary does not require that the libraries be available as well.
The last time TLS support in the Platform was raised, the best option for this
support (Vincent's wonderful [tls
package](https://www.stackage.org/package/tls)) was vetoed because [it didn't
follow the Package Versioning Policy's strict upper bounds
approach](https://mail.haskell.org/pipermail/libraries/2014-April/022554.html).
(Ironically, the alternative package mentioned there, http-streams, *also*
doesn't have upper bounds on all dependencies.)

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
