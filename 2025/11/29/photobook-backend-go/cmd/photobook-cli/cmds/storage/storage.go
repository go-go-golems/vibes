package storage

import (
	"context"
	"fmt"
	"os"
	"path/filepath"
	"strings"

	"github.com/go-go-golems/glazed/pkg/cmds"
	"github.com/go-go-golems/glazed/pkg/cmds/layers"
	"github.com/go-go-golems/glazed/pkg/cmds/parameters"
	"github.com/go-go-golems/glazed/pkg/middlewares"
	"github.com/go-go-golems/glazed/pkg/settings"
	"github.com/go-go-golems/glazed/pkg/types"
	"photobook-backend-go/internal/config"
	"photobook-backend-go/internal/storage"
)

type StoragePutCommand struct {
	*cmds.CommandDescription
}

type StoragePutSettings struct {
	File     string `glazed.parameter:"file"`
	Key      string `glazed.parameter:"key"`
	ContentType string `glazed.parameter:"content-type"`
}

func (c *StoragePutCommand) RunIntoGlazeProcessor(
	ctx context.Context,
	parsedLayers *layers.ParsedLayers,
	gp middlewares.Processor,
) error {
	settings := &StoragePutSettings{}
	if err := parsedLayers.InitializeStruct(layers.DefaultSlug, settings); err != nil {
		return err
	}

	if settings.File == "" {
		return fmt.Errorf("--file is required")
	}
	if settings.Key == "" {
		return fmt.Errorf("--key is required")
	}

	cfg, err := config.LoadConfig()
	if err != nil {
		return fmt.Errorf("failed to load config: %w", err)
	}

	store, err := storage.NewDiskStorage(cfg.StoragePath, cfg.BaseURL)
	if err != nil {
		return fmt.Errorf("failed to create storage: %w", err)
	}

	file, err := os.Open(settings.File)
	if err != nil {
		return fmt.Errorf("failed to open file: %w", err)
	}
	defer file.Close()

	contentType := settings.ContentType
	if contentType == "" {
		// Try to guess from extension
		ext := strings.ToLower(filepath.Ext(settings.File))
		switch ext {
		case ".jpg", ".jpeg":
			contentType = "image/jpeg"
		case ".png":
			contentType = "image/png"
		case ".pdf":
			contentType = "application/pdf"
		default:
			contentType = "application/octet-stream"
		}
	}

	url, err := store.Put(ctx, settings.Key, file, contentType)
	if err != nil {
		return fmt.Errorf("failed to put file: %w", err)
	}

	row := types.NewRow(
		types.MRP("key", settings.Key),
		types.MRP("url", url),
		types.MRP("content_type", contentType),
		types.MRP("success", true),
	)

	return gp.AddRow(ctx, row)
}

func NewStoragePutCommand() (*StoragePutCommand, error) {
	glazedLayer, err := settings.NewGlazedParameterLayers()
	if err != nil {
		return nil, err
	}

	cmdDesc := cmds.NewCommandDescription(
		"put",
		cmds.WithShort("Upload a file to storage"),
		cmds.WithLong("Uploads a file to storage and returns its URL"),
		cmds.WithFlags(
			parameters.NewParameterDefinition(
				"file",
				parameters.ParameterTypeString,
				parameters.WithRequired(true),
				parameters.WithHelp("Path to file to upload"),
			),
			parameters.NewParameterDefinition(
				"key",
				parameters.ParameterTypeString,
				parameters.WithRequired(true),
				parameters.WithHelp("Storage key (relative path)"),
			),
			parameters.NewParameterDefinition(
				"content-type",
				parameters.ParameterTypeString,
				parameters.WithDefault(""),
				parameters.WithHelp("Content type (auto-detected if not provided)"),
			),
		),
		cmds.WithLayersList(glazedLayer),
	)

	return &StoragePutCommand{
		CommandDescription: cmdDesc,
	}, nil
}

type StorageListCommand struct {
	*cmds.CommandDescription
}

type StorageListSettings struct {
	Prefix string `glazed.parameter:"prefix"`
}

func (c *StorageListCommand) RunIntoGlazeProcessor(
	ctx context.Context,
	parsedLayers *layers.ParsedLayers,
	gp middlewares.Processor,
) error {
	settings := &StorageListSettings{}
	if err := parsedLayers.InitializeStruct(layers.DefaultSlug, settings); err != nil {
		return err
	}

	cfg, err := config.LoadConfig()
	if err != nil {
		return fmt.Errorf("failed to load config: %w", err)
	}

	basePath := cfg.StoragePath
	if settings.Prefix != "" {
		basePath = filepath.Join(basePath, settings.Prefix)
	}

	err = filepath.Walk(basePath, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			return err
		}
		if info.IsDir() {
			return nil
		}

		relPath, err := filepath.Rel(cfg.StoragePath, path)
		if err != nil {
			return err
		}

		row := types.NewRow(
			types.MRP("key", relPath),
			types.MRP("size", info.Size()),
			types.MRP("modified", info.ModTime()),
		)

		return gp.AddRow(ctx, row)
	})

	if err != nil {
		return fmt.Errorf("failed to list files: %w", err)
	}

	return nil
}

func NewStorageListCommand() (*StorageListCommand, error) {
	glazedLayer, err := settings.NewGlazedParameterLayers()
	if err != nil {
		return nil, err
	}

	cmdDesc := cmds.NewCommandDescription(
		"list",
		cmds.WithShort("List files in storage"),
		cmds.WithLong("Lists all files in storage, optionally filtered by prefix"),
		cmds.WithFlags(
			parameters.NewParameterDefinition(
				"prefix",
				parameters.ParameterTypeString,
				parameters.WithDefault(""),
				parameters.WithHelp("Prefix to filter files"),
			),
		),
		cmds.WithLayersList(glazedLayer),
	)

	return &StorageListCommand{
		CommandDescription: cmdDesc,
	}, nil
}

var _ cmds.GlazeCommand = &StoragePutCommand{}
var _ cmds.GlazeCommand = &StorageListCommand{}

