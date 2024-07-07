package main

/*
This is entry point to the agent application demonstrating how to set
up an RSS feed for regular polling of new channels/items.

Build & run with:

$ go run agent.go

*/

import (
	"./model"
	"./util/log"
	// "./util/system"
	"./web"

	"github.com/codegangsta/cli"
	"os"
)

func main() {
	// command line app interface
	app := cli.NewApp()

	// get configuration
	config := new(model.Config)
	config.Setup("config.toml")
	config.Verify()

	app.Name = config.Application.Name
	app.Usage = config.Application.Usage
	app.Version = config.Application.Version

	// other cli options
	// app.EnableBashCompletion = true

	// global flags
	app.Flags = []cli.Flag{
		cli.BoolFlag{
			Name:  "bool, b",
			Usage: "enter bool value",
		},
		cli.StringFlag{
			Name:  "string, s",
			Usage: "enter string value",
		},
		cli.StringSliceFlag{
			Name:  "stringslice",
			Usage: "enter stringslice value",
		},
		cli.Float64Flag{
			Name:  "float, f",
			Usage: "enter float value",
		},
		cli.IntFlag{
			Name:  "int, i",
			Usage: "enter int value",
		},
		cli.DurationFlag{
			Name:  "duration, d",
			Usage: "enter duration value (e.g 2h45m)",
		},
	}

	// default application entry point
	app.Action = func(c *cli.Context) {
		log.Info("%s version %s starting...", app.Name, app.Version)
		log.Info("bool: %v", c.Bool("bool"))
		log.Info("string: %v", c.String("string"))
		log.Info("stringslice: %v", c.StringSlice("stringslice"))
		log.Info("float: %v", c.Float64("float"))
		log.Info("int: %v", c.Int("int"))
		log.Info("duration: %v", c.Duration("duration"))
		config.Dump()
		log.Info("Done")
	}

	// optional commands
	app.Commands = []cli.Command{
		{
			Name:        "web",
			Aliases:     []string{"w"},
			Usage:       "run web server",
			Description: "A longer description of the use",
			Action: func(c *cli.Context) {
				log.Info("flag: name: %s", c.String("name"))
				log.Info("running web server")
				web.Server(config)
			},
		},
		{
			Name:    "update",
			Aliases: []string{"u"},
			Usage:   "update rss feeds",
			Action: func(c *cli.Context) {
				log.Info("updating rss feeds")
				for _, mission := range config.Missions {
					mission.Run(&config.DB)
				}
			},
		},
		{
			Name:    "generate",
			Aliases: []string{"gen"},
			Usage:   "generate code from templates",
			Subcommands: []cli.Command{
				{
					Name:  "demo",
					Usage: "generate demo rendering",
					Action: func(c *cli.Context) {
						config.Generator.LoadYAML()
						for _, arg := range c.Args() {
							config.Generator.RenderDemo(arg)
						}
					},
				},
				{
					Name:  "model",
					Usage: "generate model from template",
					Action: func(c *cli.Context) {
						config.Generator.LoadYAML()
						for _, arg := range c.Args() {
							config.Generator.RenderModel(arg)
						}
					},
				},
				{
					Name:  "controller",
					Usage: "generate controller from template",
					Action: func(c *cli.Context) {
						config.Generator.LoadYAML()
						for _, arg := range c.Args() {
							config.Generator.RenderController(arg)
						}
					},
				},
				{
					Name:  "test",
					Usage: "generate test from template",
					Flags: []cli.Flag{
						cli.BoolFlag{
							Name:  "move, m",
							Usage: "move generated file to model directory",
						},
					},
					Action: func(c *cli.Context) {
						config.Generator.LoadYAML()
						for _, arg := range c.Args() {
							config.Generator.RenderModelTest(arg)
						}
					},
				},
				{
					Name:  "clean",
					Usage: "remove all generated files from output directory",
					Action: func(c *cli.Context) {
						config.Generator.CleanOutput()
					},
				},
			},
		},
	}
	app.Run(os.Args)
}
