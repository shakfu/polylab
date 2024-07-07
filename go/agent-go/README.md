# Agent
- author: Shakeeb Alireza (github user: shakfu)
- copyleft: gpl 2016
- version: 0.1

## Description
Agent is a web framework to develop analytical
application based around rss feeds and web scrapings.

It integrates a number of open-source libraries in the
go language to facilitate this task.

## Usage
A typical agent deployment requires a postgres db server

The deployment looks as follows:

- root
    + agent (static binary)
    + public (static files [css, js, img, ...])
    + config.toml (configuration file)

Note: agent is only tested on 64bit linux and is intended
to be deployed using docker.



## TODO
dockerfile / docker-compose.yml
- to write

code generator
- output to directory [done] needs further work

rss feeder
-

configuration
- models need to be configured from agents.sql not config.toml


## Dependencies

## database (postgres)
- go get github.com/lib/pq

## gorm (orm)
- go get github.com/jinzhu/gorm

### ansi
- ANSI colored strings and codes
    - go get -u github.com/mgutz/ansi/cmd/ansi-mgutz

### rss
- go get github.com/jteeuwen/go-pkg-rss
- go get github.com/SlyMarbo/rss

### command line args
- go get github.com/codegangsta/cli

### authentication
- password input
    - go get github.com/howeyc/gopass

- bcrypt / salting
    - go get github.com/jameskeane/bcrypt

### templates
- go get github.com/unrolled/render

### bayesian classification
- go get github.com/jbrukh/bayesian

### event listener
- go get github.com/pocke/goevent

### html parser
- go get golang.org/x/net/html
- go get github.com/PuerkitoBio/goquery

### yaml
- go get gopkg.in/yaml.v2

### strings
- validation
    - go get github.com/asaskevich/govalidator
- misc string functions
    - go get github.com/huandu/xstrings

### pluralization
- go get github.com/gedex/inflector

### stemming
- go get github.com/reiver/go-porterstemmer

### web server
- go get github.com/gin-gonic/gin

### config (toml)
- go get github.com/BurntSushi/toml
