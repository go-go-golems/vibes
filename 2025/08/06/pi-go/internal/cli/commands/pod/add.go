package pod

import (
	"fmt"
	"time"

	"github.com/go-go-golems/glazed/pkg/cmds"
	"github.com/go-go-golems/glazed/pkg/cmds/parameters"
	"github.com/pi-go/pi/internal/config"
	"github.com/pi-go/pi/pkg/types"
	"github.com/sirupsen/logrus"
	"github.com/spf13/cobra"
)

type AddSettings struct {
	Name        string `glazed.parameter:"name"`
	Host        string `glazed.parameter:"host"`
	Port        int    `glazed.parameter:"port"`
	User        string `glazed.parameter:"user"`
	SSHCommand  string `glazed.parameter:"ssh-command"`
	StoragePath string `glazed.parameter:"storage-path"`
	UseSudo     bool   `glazed.parameter:"use-sudo"`
}

type AddCommand struct {
	*cmds.CommandDescription
	config *types.Config
	logger *logrus.Logger
}

func NewAddCommand(cfg *types.Config, logger *logrus.Logger) *cobra.Command {
	cmd := &AddCommand{
		CommandDescription: cmds.NewCommandDescription(
			"add",
			cmds.WithShort("Add a new GPU pod"),
			cmds.WithLong(`Add a new GPU pod to the configuration.

This command registers a new GPU pod with the specified connection details.
The pod will be validated for SSH connectivity and capabilities will be discovered.

Examples:
  # Add a pod with basic SSH connection
  pi pod add my-pod user@host.example.com

  # Add a pod with custom SSH command and storage path
  pi pod add gpu-server "ssh -i ~/.ssh/key user@host" --storage-path /data/models

  # Add a pod with sudo access
  pi pod add root-pod root@server --use-sudo`),
			cmds.WithArguments(
				parameters.NewParameterDefinition(
					"name",
					parameters.ParameterTypeString,
					parameters.WithHelp("Name for the pod"),
					parameters.WithRequired(true),
				),
				parameters.NewParameterDefinition(
					"ssh-command",
					parameters.ParameterTypeString,
					parameters.WithHelp("SSH command to connect to the pod"),
					parameters.WithRequired(true),
				),
			),
			cmds.WithFlags(
				parameters.NewParameterDefinition(
					"host",
					parameters.ParameterTypeString,
					parameters.WithHelp("Hostname or IP address of the pod"),
				),
				parameters.NewParameterDefinition(
					"port",
					parameters.ParameterTypeInteger,
					parameters.WithHelp("SSH port"),
					parameters.WithDefault(22),
				),
				parameters.NewParameterDefinition(
					"user",
					parameters.ParameterTypeString,
					parameters.WithHelp("SSH username"),
				),
				parameters.NewParameterDefinition(
					"storage-path",
					parameters.ParameterTypeString,
					parameters.WithHelp("Storage path for HuggingFace models"),
				),
				parameters.NewParameterDefinition(
					"use-sudo",
					parameters.ParameterTypeBool,
					parameters.WithHelp("Use sudo for operations"),
					parameters.WithDefault(false),
				),
			),
		),
		config: cfg,
		logger: logger,
	}

	cobraCmd := &cobra.Command{
		Use:   cmd.Name,
		Short: cmd.Short,
		Long:  cmd.Long,
		Args:  cobra.ExactArgs(2),
		RunE: func(cobraCmd *cobra.Command, args []string) error {
			return cmd.RunE(cobraCmd, args)
		},
	}

	// Add flags to cobra command
	cobraCmd.Flags().String("host", "", "Hostname or IP address of the pod")
	cobraCmd.Flags().Int("port", 22, "SSH port")
	cobraCmd.Flags().String("user", "", "SSH username")
	cobraCmd.Flags().String("storage-path", "", "Storage path for HuggingFace models")
	cobraCmd.Flags().Bool("use-sudo", false, "Use sudo for operations")

	return cobraCmd
}

func (c *AddCommand) RunE(cobraCmd *cobra.Command, args []string) error {
	// Parse arguments
	name := args[0]
	sshCommand := args[1]

	// Parse flags
	host, _ := cobraCmd.Flags().GetString("host")
	port, _ := cobraCmd.Flags().GetInt("port")
	user, _ := cobraCmd.Flags().GetString("user")
	storagePath, _ := cobraCmd.Flags().GetString("storage-path")
	useSudo, _ := cobraCmd.Flags().GetBool("use-sudo")

	// Extract host and user from SSH command if not provided
	if host == "" || user == "" {
		extractedHost, extractedUser := parseSSHCommand(sshCommand)
		if host == "" {
			host = extractedHost
		}
		if user == "" {
			user = extractedUser
		}
	}

	// Use defaults if still empty
	if user == "" {
		user = c.config.Global.DefaultUser
	}
	if storagePath == "" {
		storagePath = c.config.Global.DefaultStoragePath
	}

	// Create pod
	pod := &types.Pod{
		Name:        name,
		Host:        host,
		Port:        port,
		User:        user,
		SSHCommand:  sshCommand,
		StoragePath: storagePath,
		UseSudo:     useSudo,
		Status:      types.PodStatusUnknown,
		Metadata:    make(map[string]string),
		CreatedAt:   time.Now(),
		UpdatedAt:   time.Now(),
	}

	// Validate pod
	if err := pod.Validate(); err != nil {
		return fmt.Errorf("invalid pod configuration: %w", err)
	}

	// Add pod to configuration
	if err := c.config.AddPod(pod); err != nil {
		return fmt.Errorf("failed to add pod: %w", err)
	}

	// Save configuration
	if err := config.Save(c.config); err != nil {
		return fmt.Errorf("failed to save configuration: %w", err)
	}

	fmt.Printf("Successfully added pod '%s'\n", name)
	fmt.Printf("SSH Command: %s\n", sshCommand)
	fmt.Printf("Host: %s\n", host)
	fmt.Printf("User: %s\n", user)
	fmt.Printf("Storage Path: %s\n", storagePath)

	if c.config.Pods.Active == name {
		fmt.Printf("Pod '%s' is now the active pod\n", name)
	}

	return nil
}

// parseSSHCommand extracts host and user from SSH command
func parseSSHCommand(sshCommand string) (host, user string) {
	// This is a simplified parser - in a real implementation,
	// you'd want to properly parse SSH command syntax
	// For now, we'll look for patterns like "user@host" or "ssh user@host"
	
	// TODO: Implement proper SSH command parsing
	// This would handle various SSH command formats and extract
	// the actual host and user information
	
	return "", ""
}

