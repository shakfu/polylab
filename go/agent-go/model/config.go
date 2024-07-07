package model

import (
	// "github.com/koding/multiconfig"
	"../core"
	"../util"
	"../util/log"
	"../util/system"
	"fmt"
	"github.com/BurntSushi/toml"
	"github.com/pocke/goevent"
	"os"
	"strings"
	"text/template"
)

const view = `
id                          {{.Id}}

app.name                    {{.Application.Name}}
app.version                 {{.Application.Version}}
app.usage                   {{.Application.Usage}}

db.type                     {{.DB.Type}}
db.ip                       {{.DB.Ip}}
db.port                     {{.DB.Port}}
db.user                     {{.DB.User}}
db.password                 {{.DB.Password}}

server.name                 {{.Server.Name}}
server.port                 {{.Server.Port}}
server.enabled              {{.Server.Enabled}}
server.users                {{.Server.Users}}
server.staticdir            {{.Server.StaticDir}}
server.favicon              {{.Server.Favicon}}

template_manager.root       {{.TemplateManager.Root}}
template_manager.layout     {{.TemplateManager.Layouts}}
template_manager.include    {{.TemplateManager.Includes}}

gen.templatepath            {{.Generator.TemplatePath}}
gen.configpath              {{.Generator.ConfigPath}}
gen.outputpath              {{.Generator.OutputPath}}

missions:
    {{ range .Missions }}
    mission.name:           {{ .Name }}
    mission.targets:
        {{ range .Targets }}
        target.name:        {{ .Name }}
        target.keywords:    {{ .Keywords }}
        target.url          {{ .Url }}
        {{ end}}
    {{ end }}
`

type Config struct {
	Id              string
	Application     Application
	Server          Server
	Generator       Generator
	DB              Database
	TemplateManager TemplateManager
	Events          goevent.Table
	Missions        []Mission
}

func (c *Config) Setup(path string) {
	_, err := toml.DecodeFile(path, c)
	util.Check(err)
	log.Info("application configured")
	c.Missions = c.DB.GetMissions()
	log.Info("missions loaded")
	c.Events = core.EventDispatcher()
	log.Info("events loaded")
}

func (c *Config) Dump() {
	tmpl := template.Must(template.New("config").Parse(view))
	err := tmpl.Execute(os.Stdout, c)
	util.Check(err)
}

func (c *Config) String() string {
	return fmt.Sprintf("Config{App.Name: %s, App.Version: %s}",
		c.Application.Name, c.Application.Version)
}

func (c *Config) Display() {
	log.Info(c.String())
}

func (c *Config) Verify() {
	out := system.Capture("md5sum", "./agent")
	list := strings.Split(out, " ")
	if list[0] != c.Id {
		log.Warn("app mdsum %s != config.Id %s", list[0], c.Id)
	} else {
		log.Debug("app mdsum verified")
	}
}
