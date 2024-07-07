package controller

import (
	// "../helper"
	// "../../model"
	"github.com/gin-gonic/gin"
)

type entryController struct {
	Router *gin.Engine
}

func (controller *entryController) createEntry(c *gin.Context) {
	// c.Request.ParseForm()

	// entry := &model.Entry{}
	// entry.SetAttributes(c.Request.Form)

	// if entry.Save() {
	//     helper.JSONResponseObject(c, 200, entry)
	// } else {
	//     helper.JSONResponse(c, 400, entry.ErrorMessages())
	// }
}

func (controller *entryController) listEntry(c *gin.Context) {

}

func (controller *entryController) getEntry(c *gin.Context) {

}

func (controller *entryController) updateEntry(c *gin.Context) {

}

func (controller *entryController) Setup() {
	// controller.Router.POST("/entry", controller.createEntry)
	// controller.Router.GET("/entry", controller.listEntry)
	// controller.Router.GET("/entry/:id", controller.getEntry)
	// controller.Router.PUT("/entry/:id", controller.updateEntry)
}

/*** Helpers ***/
// func findEntryById(c *gin.Context) (*models.entry, *helper.ResponseError) {
//     c.Request.ParseForm()

//     entry := models.FindOneentryById(c.Params.ByName("id"))
//     if entry == nil {
//         err := helper.NewResponseError(404, "entry not found.")
//         return nil, err
//     }

//     return entry, nil
// }

// vi:syntax=go
