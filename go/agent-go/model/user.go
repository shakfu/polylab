package model

import (
	"github.com/jameskeane/bcrypt"
)

type User struct {
	Name     string
	Password string
}

func NewUser(name string, password string) *User {
	return &User{name, password}
}

func (u *User) SetPassword(password string) {
	hash, _ := bcrypt.Hash(password)
	u.Password = hash
}

func (u *User) CheckPassword(password string) bool {
	return bcrypt.Match(password, u.Password)
}
