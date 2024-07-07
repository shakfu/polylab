package controller

import (
    "../helper"
    "../../model"
    "github.com/gin-gonic/gin"
)

type {{.Lower}}Controller struct {
    Router *gin.Engine
}

func (controller *{{.Lower}}Controller) create{{.Name}}(c *gin.Context) {
    c.Request.ParseForm()

    entry := &model.{{.Name}}{}
    entry.SetAttributes(c.Request.Form)

    if entry.Save() {
        helper.JSONResponseObject(c, 200, entry)
    } else {
        helper.JSONResponse(c, 400, entry.ErrorMessages())
    }
}

func (controller *{{.Lower}}Controller) list{{.Name}}(c *gin.Context) {
    {{.Plural}} := model.FindAll{{.Name}}
    {{.Plural}}Response := make([]helpers.ResponseMap, len({{.Plural}}))
    for i, {{.Lower}} := range {{.Plural}} {
        {{.Plural}}Response[i] = {{.Lower}}.ToResponseMap()
    }

    helpers.JSONResponseArray(c, 200, {{.Plural}}Response)
}

func (controller *{{.Lower}}Controller) get{{.Name}}(c *gin.Context) {
    {{.Lower}} := model.FindOne{{.Name}}ById(c.Params.ByName("id"))
    if {{.Lower}} != nil {
        helper.JSONResponseObject(c, 200, {{.Lower}})
    } else {
        helper.JSONResponse(c, 404, helper.ResponseMap{})
    }
}

func (controller *{{.Lower}}Controller) update{{.Name}}(c *gin.Context) {
    {{.Lower}}, err := find{{.Name}}ById(c)

    if err != nil {
        helpers.JSONResponseError(c, err)
        return
    }

    {{.Lower}}.SetAttributes(c.Request.Form)

    if {{.Lower}}.Save() {
        helper.JSONResponseObject(c, 200, {{.Lower}})
    } else {
        helper.JSONResponse(c, 400, {{.Lower}}.ErrorMessages())
    }
}

func (controller *{{.Lower}}Controller) Setup() {
    controller.Router.POST("/{{.Lower}}",    controller.create{{.Name}})
    controller.Router.GET("/{{.Lower}}",     controller.list{{.Name}})
    controller.Router.GET("/{{.Lower}}/:id", controller.get{{.Name}})
    controller.Router.PUT("/{{.Lower}}/:id", controller.update{{.Name}})
}
