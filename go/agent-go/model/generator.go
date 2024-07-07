package model

import (
	"../util"
	"../util/log"
	"../util/system"
	"bytes"
	"fmt"
	"gopkg.in/yaml.v2"
	"io/ioutil"
	"path/filepath"
	"strings"
	"text/template"
)

type Generator struct {
	TemplatePath string
	ConfigPath   string
	OutputPath   string
	Models       []MetaModel
}

// functions for templates
var templateFuncs = template.FuncMap{
	"lower": func(s string) string { return strings.ToLower(s) },
}

func (g *Generator) String() string {
	return fmt.Sprintf("Generator{TemplatePath: '%s', ConfigPath: '%s', OutputPath: '%s'}",
		g.TemplatePath, g.ConfigPath, g.OutputPath)
}

func (g *Generator) Display() {
	log.Info(g.String())
}

func (g *Generator) GetModels() []string {
	var models []string
	for _, m := range g.Models {
		models = append(models, m.Name)
	}
	return models
}

// loads models from yaml file
func (g *Generator) LoadYAML() {
	yml, err := ioutil.ReadFile(g.ConfigPath)
	util.Check(err)
	err = yaml.Unmarshal(yml, &g.Models)
	util.Check(err)
}

func (g *Generator) DumpYAML() string {
	yml, err := yaml.Marshal(g.Models)
	util.Check(err)
	return string(yml)
}

func (g *Generator) Render(templateName string, model *MetaModel) string {
	// log.Debug("entering Render")
	var doc bytes.Buffer
	tmpl := template.Must(
		template.New("").Funcs(templateFuncs).ParseGlob(g.TemplatePath))
	err := tmpl.ExecuteTemplate(&doc, templateName, model)
	util.Check(err)
	return doc.String()
}

func (g *Generator) RenderToFile(templateName string, dir string, mask string, model *MetaModel) {
	// log.Debug("entering RenderToFile")
	rendered := g.Render(templateName, model)
	filename := fmt.Sprintf(mask, strings.ToLower(model.Name))
	path := filepath.Join(g.OutputPath, dir, filename)
	log.Info("writing: %s", path)
	system.WriteFile(path, rendered)
	log.Info("formatting: %s", path)
	system.Execute("gofmt", "-w", path)
	// log.Info(output)
	log.Info("done: %s", path)
}

func (g *Generator) renderToFileByName(name string, dir string, mask string, templateName string) {
	success := false
	for _, model := range g.Models {
		if name == model.Name {
			g.RenderToFile(templateName, dir, mask, &model)
			success = true
		}
	}
	if !success {
		log.Error("%s not available. Must be one or more of %v", name, g.GetModels())
	}
}

func (g *Generator) RenderDemo(name string) {
	// log.Debug("entering RenderModel")
	g.renderToFileByName(name, "demos", "%s_demo.go", "demo.go")
}

func (g *Generator) RenderModel(name string) {
	// log.Debug("entering RenderModel")
	g.renderToFileByName(name, "models", "%s.go", "model.go")
}

func (g *Generator) RenderModelTest(name string) {
	// log.Debug("entering RenderModelTest")
	g.renderToFileByName(name, "tests", "%s_test.go", "model_test.go")
}

func (g *Generator) RenderController(name string) {
	// log.Debug("entering RenderController")
	g.renderToFileByName(name, "controllers", "%s.go", "controller.go")
}

func (g *Generator) RenderDemos() {
	for _, model := range g.Models {
		g.RenderToFile("demo.go", "demos", "%s_demo.go", &model)
	}
}

func (g *Generator) RenderModels() {
	for _, model := range g.Models {
		g.RenderToFile("model.go", "models", "%s.go", &model)
	}
}

func (g *Generator) RenderModelTests() {
	for _, model := range g.Models {
		g.RenderToFile("model_test.go", "tests", "%s_test.go", &model)
	}
}

func (g *Generator) RenderControllers() {
	for _, model := range g.Models {
		g.RenderToFile("controller.go", "controllers", "%s.go", &model)
	}
}

func (g *Generator) RenderAll() {
	g.RenderModels()
	g.RenderModelTests()
	g.RenderControllers()
}

// remove generated files from output subfolders
func (g *Generator) CleanOutput() {
	folders := []string{"models", "controllers", "demos", "tests"}
	var path string
	for _, dir := range folders {
		path = filepath.Join(g.OutputPath, dir)
		log.Info("removing generated files from %s", path)
		system.RemoveFiles(path)
	}
}
