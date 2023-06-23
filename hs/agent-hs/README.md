# agent-hs

A web feed retriever / analyzer in haskell


## Background

This is a translation of an old python newsagent program written ages ago and lost. It used to run automatically every interval and download and scrape link text from news websites while checking for certain specified keywords. It would also preserve link data in an RDBMS and then serve summary webpages of the analysis results.

Cycle many years later and my robust interest in learning haskell has led to an attempt to possibly replicate and improve the program of old with a haskell functional flavour.


## Features

Not very functional or well documented right now: downloads feeds which are then parsed using the feed and tagsoup packages; some basic keyword analysis is performed and feed titles are dumped to the console and an html file.


## Customizing, Building & Running

To customize, modify Config.hs as appropriate.

To build:

	cabal configure
	cabal build

To get command line help:

	newsagent --help

    Usage: newsagent [options]
      -h           --help               display help
      -v           --verbose            show verbose output
      -n           --nohtml             skip html output
      -m[MISSION]  --mission[=MISSION]  set mission

    options: [Help]

To run (in the src directory):

    ln -s ./dist/build/newsagent/newsagent .
    
    ./newsagent



## TODO

These are possible future plans in no particular order and importance:

- [done] feed item to be displayed sorted by relevance
- [done] render feeds in html
- [done] add command-line option support (preliminary)
- [done] add decent css styling to html output (using twitter bootstrap)
- [done] show entry level relevance
- [done] make Console.hs, Html.hs hierarchical -> Views.Console, Views.Html
- [done] add navbar at top for html boostrap interface
- parallelization / scheduling of agent dispatch
    - rough support at present (may need to compile with threads or set proper
      flags in cabal build)
- auto-classification of links


## External credits:

* Obviously all external modules and dependencies. Thanks!
* The Stemming.hs module is written by Dmitry Antonyuk, based on Martin Porter's stemming algorithm.
* I nicked the stopWords list from someone on the web. If you are reading this. Thank you!
