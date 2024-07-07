package model

import (
	// "text/template"
    "html/template"
    "path/filepath"
    "log"
    "bytes"
)

type TemplateManager struct {
	Root      string
	Layouts   string
	Includes  string
	Templates map[string]*template.Template
}

// see: https://elithrar.github.io/article/approximating-html-template-inheritance/
// and  https://stackoverflow.com/questions/11467731/is-it-possible-to-have-nested-templates-in-go-using-the-standard-library-googl



var templates map[string]*template.Template

// Load templates on program initialisation
func (tm *TemplateManager) Init() {
    if tm.Templates == nil {
        templates = make(map[string]*template.Template)
    }

    templatesDir := tm.Root

    layouts, err := filepath.Glob(templatesDir + "layouts/*.tmpl")
    if err != nil {
        log.Fatal(err)
    }

    includes, err := filepath.Glob(templatesDir + "includes/*.tmpl")
    if err != nil {
        log.Fatal(err)
    }

    // Generate our templates map from our layouts/ and includes/ directories
    for _, layout := range layouts {
        files := append(includes, layout)
        templates[filepath.Base(layout)] = template.Must(template.ParseFiles(files...))
    }
}

// renderTemplate is a wrapper around template.ExecuteTemplate.
func (tm *TemplateManager) renderTemplate(name string, data map[string]interface{}) string {
    // Ensure the template exists in the map.
    var buffer bytes.Buffer
    tmpl, ok := tm.Templates[name]
    if !ok {
        log.Fatal("The template %s does not exist.", name)
    }
    tmpl.ExecuteTemplate(&buffer, "base", data)
    return buffer.String()
}

