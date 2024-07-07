package web

import (
	"./controller"
	"./middleware"
	// "./template"
	"../model"
	"github.com/gin-gonic/gin"
	renderer "github.com/unrolled/render"
	"github.com/leekchan/gtf"
	"html/template"
	"log"
	"net/http"
)

func Server(config *model.Config) {
	router := gin.Default()

	// setup middleware
	router.Use(
		middleware.Logger(),
		// middleware.DummyMiddleware(),
	)

	// setup controllers
	controller.Setup(router)

	// setup templates
	render := renderer.New(renderer.Options{
		Directory: "view",
		Layout: "layout",
		Funcs: []template.FuncMap{gtf.GtfFuncMap},
	})
	// router.LoadHTMLGlob("./view/*.tmpl")
	// router.LoadHTMLFiles(files ...string)
	// router.SetHTMLTemplate(template.ROOT)

	// setup static files
	// router.Static("/static", "./public")
	router.Static("/static", config.Server.StaticDir)
	router.StaticFile("/favicon.ico", config.Server.Favicon)
	// router.StaticFS("/directory", http.Dir("./public"))

	// index
	router.GET("/", func(c *gin.Context) {
		missions := config.DB.GetMissions()
		render.HTML(c.Writer, http.StatusOK, "index", gin.H{
			"title":    "Agent",
			"missions": missions,
		})
	})

	// test
	router.GET("/test", func(c *gin.Context) {
		example := c.MustGet("example").(string)
		log.Println(example)
	})

	// config
	router.GET("/config", func(c *gin.Context) {
		missions := config.DB.GetMissions()
		render.HTML(c.Writer, http.StatusOK, "config", gin.H{
			"title":  "Config",
			"config": config,
			"missions": missions,
		})
	})

	// This handler will match /user/john but will not match neither /user/ or /user
	router.GET("/user/:name", func(c *gin.Context) {
		name := c.Param("name")
		c.String(http.StatusOK, "Hello %s", name)
	})

	// However, this one will match /user/john/ and also /user/john/send
	// If no other routers match /user/john, it will redirect to /user/join/
	router.GET("/user/:name/*action", func(c *gin.Context) {
		name := c.Param("name")
		action := c.Param("action")
		message := name + " is " + action
		c.String(http.StatusOK, message)
	})

	router.Run(config.Server.Address())
}
