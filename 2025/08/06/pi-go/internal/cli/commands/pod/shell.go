package pod

import (
	"fmt"

	"github.com/pi-go/pi/pkg/types"
	"github.com/sirupsen/logrus"
	"github.com/spf13/cobra"
)

func NewShellCommand(cfg *types.Config, logger *logrus.Logger) *cobra.Command {
	return &cobra.Command{
		Use:   "shell",
		Short: "Open shell on pod",
		RunE: func(cmd *cobra.Command, args []string) error {
			return fmt.Errorf("not implemented yet")
		},
	}
}

func NewStatusCommand(cfg *types.Config, logger *logrus.Logger) *cobra.Command {
	return &cobra.Command{
		Use:   "status",
		Short: "Show pod status",
		RunE: func(cmd *cobra.Command, args []string) error {
			return fmt.Errorf("not implemented yet")
		},
	}
}

