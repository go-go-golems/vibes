package services

import (
	"errors"
	"fmt"
)

type User struct {
	ID    int    `json:"id"`
	Name  string `json:"name"`
	Email string `json:"email"`
}

type UserService struct {
	users []User
}

func NewUserService() *UserService {
	return &UserService{
		users: []User{
			{ID: 1, Name: "John Doe", Email: "john@example.com"},
			{ID: 2, Name: "Jane Smith", Email: "jane@example.com"},
			{ID: 3, Name: "Bob Johnson", Email: "bob@example.com"},
		},
	}
}

func (s *UserService) GetAllUsers() []User {
	return s.users
}

func (s *UserService) GetUserByID(id int) (*User, error) {
	for _, user := range s.users {
		if user.ID == id {
			return &user, nil
		}
	}
	return nil, errors.New("user not found")
}

func (s *UserService) CreateUser(name, email string) (*User, error) {
	if name == "" || email == "" {
		return nil, errors.New("name and email are required")
	}
	
	// Check if email already exists
	for _, user := range s.users {
		if user.Email == email {
			return nil, errors.New("email already exists")
		}
	}
	
	newUser := User{
		ID:    len(s.users) + 1,
		Name:  name,
		Email: email,
	}
	
	s.users = append(s.users, newUser)
	return &newUser, nil
}

func (s *UserService) UpdateUser(id int, name, email string) (*User, error) {
	for i, user := range s.users {
		if user.ID == id {
			if name != "" {
				s.users[i].Name = name
			}
			if email != "" {
				s.users[i].Email = email
			}
			return &s.users[i], nil
		}
	}
	return nil, errors.New("user not found")
}

func (s *UserService) DeleteUser(id int) error {
	for i, user := range s.users {
		if user.ID == id {
			s.users = append(s.users[:i], s.users[i+1:]...)
			return nil
		}
	}
	return fmt.Errorf("user with ID %d not found", id)
}

