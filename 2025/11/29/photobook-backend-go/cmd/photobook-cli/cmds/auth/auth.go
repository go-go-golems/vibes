package auth

import (
	"context"
	"fmt"

	"github.com/go-go-golems/glazed/pkg/cmds"
	"github.com/go-go-golems/glazed/pkg/cmds/layers"
	"github.com/go-go-golems/glazed/pkg/cmds/parameters"
	"github.com/go-go-golems/glazed/pkg/middlewares"
	"github.com/go-go-golems/glazed/pkg/settings"
	"github.com/go-go-golems/glazed/pkg/types"
	"photobook-backend-go/internal/auth"
	"photobook-backend-go/internal/config"
	"photobook-backend-go/internal/db"
)

type AuthRegisterCommand struct {
	*cmds.CommandDescription
}

type AuthRegisterSettings struct {
	Email    string `glazed.parameter:"email"`
	Password string `glazed.parameter:"password"`
	Name     string `glazed.parameter:"name"`
}

func (c *AuthRegisterCommand) RunIntoGlazeProcessor(
	ctx context.Context,
	parsedLayers *layers.ParsedLayers,
	gp middlewares.Processor,
) error {
	settings := &AuthRegisterSettings{}
	if err := parsedLayers.InitializeStruct(layers.DefaultSlug, settings); err != nil {
		return err
	}

	cfg, err := config.LoadConfig()
	if err != nil {
		return fmt.Errorf("failed to load config: %w", err)
	}

	database, err := db.OpenDB(cfg.DatabaseURL)
	if err != nil {
		return fmt.Errorf("failed to open database: %w", err)
	}
	defer database.Close()

	repo := auth.NewSQLiteUserRepository(database)
	adapter := auth.NewEmailPasswordAdapter(repo)
	authService := auth.NewAuthService(repo, adapter, cfg.JWTSecret, cfg)

	user, token, err := authService.Register(ctx, settings.Email, settings.Password, settings.Name)
	if err != nil {
		return fmt.Errorf("failed to register: %w", err)
	}

	row := types.NewRow(
		types.MRP("id", user.ID),
		types.MRP("open_id", user.OpenID),
		types.MRP("name", user.Name),
		types.MRP("email", user.Email),
		types.MRP("role", user.Role),
		types.MRP("token", token),
		types.MRP("success", true),
	)

	return gp.AddRow(ctx, row)
}

func NewAuthRegisterCommand() (*AuthRegisterCommand, error) {
	glazedLayer, err := settings.NewGlazedParameterLayers()
	if err != nil {
		return nil, err
	}

	cmdDesc := cmds.NewCommandDescription(
		"register",
		cmds.WithShort("Register a new user"),
		cmds.WithLong("Registers a new user with email/password authentication"),
		cmds.WithFlags(
			parameters.NewParameterDefinition(
				"email",
				parameters.ParameterTypeString,
				parameters.WithRequired(true),
				parameters.WithHelp("User email address"),
			),
			parameters.NewParameterDefinition(
				"password",
				parameters.ParameterTypeString,
				parameters.WithRequired(true),
				parameters.WithHelp("User password"),
			),
			parameters.NewParameterDefinition(
				"name",
				parameters.ParameterTypeString,
				parameters.WithRequired(true),
				parameters.WithHelp("User name"),
			),
		),
		cmds.WithLayersList(glazedLayer),
	)

	return &AuthRegisterCommand{
		CommandDescription: cmdDesc,
	}, nil
}

type AuthLoginCommand struct {
	*cmds.CommandDescription
}

type AuthLoginSettings struct {
	Email    string `glazed.parameter:"email"`
	Password string `glazed.parameter:"password"`
}

func (c *AuthLoginCommand) RunIntoGlazeProcessor(
	ctx context.Context,
	parsedLayers *layers.ParsedLayers,
	gp middlewares.Processor,
) error {
	settings := &AuthLoginSettings{}
	if err := parsedLayers.InitializeStruct(layers.DefaultSlug, settings); err != nil {
		return err
	}

	cfg, err := config.LoadConfig()
	if err != nil {
		return fmt.Errorf("failed to load config: %w", err)
	}

	database, err := db.OpenDB(cfg.DatabaseURL)
	if err != nil {
		return fmt.Errorf("failed to open database: %w", err)
	}
	defer database.Close()

	repo := auth.NewSQLiteUserRepository(database)
	adapter := auth.NewEmailPasswordAdapter(repo)
	authService := auth.NewAuthService(repo, adapter, cfg.JWTSecret, cfg)

	user, token, err := authService.Login(ctx, settings.Email, settings.Password)
	if err != nil {
		return fmt.Errorf("failed to login: %w", err)
	}

	row := types.NewRow(
		types.MRP("id", user.ID),
		types.MRP("open_id", user.OpenID),
		types.MRP("name", user.Name),
		types.MRP("email", user.Email),
		types.MRP("role", user.Role),
		types.MRP("token", token),
		types.MRP("success", true),
	)

	return gp.AddRow(ctx, row)
}

func NewAuthLoginCommand() (*AuthLoginCommand, error) {
	glazedLayer, err := settings.NewGlazedParameterLayers()
	if err != nil {
		return nil, err
	}

	cmdDesc := cmds.NewCommandDescription(
		"login",
		cmds.WithShort("Login with email/password"),
		cmds.WithLong("Authenticates a user and returns a session token"),
		cmds.WithFlags(
			parameters.NewParameterDefinition(
				"email",
				parameters.ParameterTypeString,
				parameters.WithRequired(true),
				parameters.WithHelp("User email address"),
			),
			parameters.NewParameterDefinition(
				"password",
				parameters.ParameterTypeString,
				parameters.WithRequired(true),
				parameters.WithHelp("User password"),
			),
		),
		cmds.WithLayersList(glazedLayer),
	)

	return &AuthLoginCommand{
		CommandDescription: cmdDesc,
	}, nil
}

type AuthMeCommand struct {
	*cmds.CommandDescription
}

type AuthMeSettings struct {
	Token string `glazed.parameter:"token"`
}

func (c *AuthMeCommand) RunIntoGlazeProcessor(
	ctx context.Context,
	parsedLayers *layers.ParsedLayers,
	gp middlewares.Processor,
) error {
	settings := &AuthMeSettings{}
	if err := parsedLayers.InitializeStruct(layers.DefaultSlug, settings); err != nil {
		return err
	}

	cfg, err := config.LoadConfig()
	if err != nil {
		return fmt.Errorf("failed to load config: %w", err)
	}

	database, err := db.OpenDB(cfg.DatabaseURL)
	if err != nil {
		return fmt.Errorf("failed to open database: %w", err)
	}
	defer database.Close()

	repo := auth.NewSQLiteUserRepository(database)
	adapter := auth.NewEmailPasswordAdapter(repo)
	authService := auth.NewAuthService(repo, adapter, cfg.JWTSecret, cfg)

	user, err := authService.Me(ctx, settings.Token)
	if err != nil {
		return fmt.Errorf("failed to get user: %w", err)
	}

	row := types.NewRow(
		types.MRP("id", user.ID),
		types.MRP("open_id", user.OpenID),
		types.MRP("name", user.Name),
		types.MRP("email", user.Email),
		types.MRP("role", user.Role),
		types.MRP("login_method", user.LoginMethod),
		types.MRP("last_signed_in", user.LastSignedIn),
	)

	return gp.AddRow(ctx, row)
}

func NewAuthMeCommand() (*AuthMeCommand, error) {
	glazedLayer, err := settings.NewGlazedParameterLayers()
	if err != nil {
		return nil, err
	}

	cmdDesc := cmds.NewCommandDescription(
		"me",
		cmds.WithShort("Get current user from session token"),
		cmds.WithLong("Verifies a session token and returns the current user"),
		cmds.WithFlags(
			parameters.NewParameterDefinition(
				"token",
				parameters.ParameterTypeString,
				parameters.WithRequired(true),
				parameters.WithHelp("Session token (JWT)"),
			),
		),
		cmds.WithLayersList(glazedLayer),
	)

	return &AuthMeCommand{
		CommandDescription: cmdDesc,
	}, nil
}

var _ cmds.GlazeCommand = &AuthRegisterCommand{}
var _ cmds.GlazeCommand = &AuthLoginCommand{}
var _ cmds.GlazeCommand = &AuthMeCommand{}

