package middleware

import (
	"github.com/gin-gonic/gin"
	"log"
)

func DummyMiddleware() gin.HandlerFunc {
	log.Println("entering dummy.middleware")
	return func(c *gin.Context) {
		log.Println("dummy.action middleware")
		c.Next()
	}
}
