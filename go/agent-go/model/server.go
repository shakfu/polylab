package model

import (
	"fmt"
)

type Server struct {
	Name      string `required:"true"`
	Port      int    `default:6060`
	Enabled   bool
	Users     []string
	StaticDir string
	Favicon   string
}

func NewServer(name string, port int) *Server {
	return &Server{name, port, true, []string{"sa"}, "public", "public/img/favicon.ico"}
}

func (s *Server) Address() string {
	return fmt.Sprintf(":%d", s.Port)
}
