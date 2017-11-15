# Three Words in Elm

[![Build Status][travis-svg]][travis]

![what 3 words logo][image]

## [Demo][demo]

[what3words][words] is the simplest way to talk about any precise location. This
system has divided the world into a grid of three meters by three meters squares
and assigned each one a unique address made of just 3 words. Now everyone and
everywhere has a reliable address.

With this library, you can create HTTP requests that will convert three word
addresses to latitude, longitude coordinates and vice-versa.

To use this library, you will need [the core HTTP library][http] and have an
understanding on how to use `Cmd`s.

[demo]: https://elm-three-words.netlify.com/ "what3words • Elm"
[http]: http://package.elm-lang.org/packages/elm-lang/http/1.0.0 "HTTP in Elm"
[image]: https://what3words.com/wp-content/uploads/2017/01/what3words-logo-horizontal-RGB-styleguide-PNG-1.png "what3words logo"
[travis]: https://travis-ci.org/cjduncana/three-words
[travis-svg]: https://travis-ci.org/cjduncana/three-words.svg?branch=master
[words]: http://what3words.com/ "what3words | Addressing the world"
