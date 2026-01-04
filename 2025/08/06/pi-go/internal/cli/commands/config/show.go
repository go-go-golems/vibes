package config

import (
	"fmt"

	"github.com/pi-go/pi/pkg/types"
	"github.com/sirupsen/logrus"
	"github.com/spf13/cobra"
)

func NewShowCommand(cfg *types.Config, logger *logrus.Logger) *cobra.Command {
	return &cobra.Command{
		Use:   "show",
		Short: "Show configuration",
		RunE: func(cmd *cobra.Command, args []string) error {
			return fmt.Errorf("not implemented yet")
		},
	}
}

func NewSetCommand(cfg *types.Config, logger *logrus.Logger) *cobra.Command {
	return &cobra.Command{
		Use:   "set",
		Short: "Set configuration value",
		RunE: func(cmd *cobra.Command, args []string) error {
			return fmt.Errorf("not implemented yet")
		},
	}
}

func NewTemplateCommand(cfg *types.Config, logger *logrus.Logger) *cobra.Command {
	return &cobra.Command{
		Use:   "template",
		Short: "Manage model templates",
		RunE: func(cmd *cobra.Command, args []string) error {
			return fmt.Errorf("not implemented yet")
		},
	}
}

