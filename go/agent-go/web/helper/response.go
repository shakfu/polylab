package helper

import (
	"fmt"
	"github.com/gin-gonic/gin"
)

type ResponseError struct {
	Code    int
	Message string
}

type ResponseMap map[string]interface{}
type Responsible interface {
	ToResponseMap() ResponseMap
}

func NewResponseError(code int, message string) *ResponseError {
	err := &ResponseError{Code: code, Message: message}

	return err
}

func (err *ResponseError) Error() string {
	return fmt.Sprintf("Response error: %d %s", err.Code, err.Message)
}

func JSONResponseObject(c *gin.Context, statusCode int, object Responsible) {
	body := object.ToResponseMap()
	JSONResponse(c, statusCode, body)
}

func JSONResponse(c *gin.Context, statusCode int, body ResponseMap) {
	if statusCode != 200 {
		body["success"] = false
	} else {
		body["success"] = true
	}
	c.JSON(statusCode, body)
}

func JSONResponseArray(c *gin.Context, statusCode int, collection []ResponseMap) {
	body := ResponseMap{}
	body["items"] = collection
	body["count"] = len(collection)

	JSONResponse(c, statusCode, body)
}

func JSONResponseError(c *gin.Context, err *ResponseError) {
	body := ResponseMap{"success": false, "message": err.Message}

	JSONResponse(c, err.Code, body)
}

// vi:syntax=go
