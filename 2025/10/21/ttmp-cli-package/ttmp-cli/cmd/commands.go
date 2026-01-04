package cmd

import (
	"github.com/spf13/cobra"
)

// AddCommands adds all ttmp commands to the root command
func AddCommands(rootCmd *cobra.Command) error {
	// Create init command
	initCmd, err := NewInitCommand()
	if err != nil {
		return err
	}
	rootCmd.AddCommand(initCmd)

	// Create add command
	addCmd, err := NewAddCommand()
	if err != nil {
		return err
	}
	rootCmd.AddCommand(addCmd)

	// Create relate command
	relateCmd, err := NewRelateCommand()
	if err != nil {
		return err
	}
	rootCmd.AddCommand(relateCmd)

	// Create meta command group
	metaCmd := &cobra.Command{
		Use:   "meta",
		Short: "Manage document metadata",
	}
	
	metaUpdateCmd, err := NewMetaUpdateCommand()
	if err != nil {
		return err
	}
	metaCmd.AddCommand(metaUpdateCmd)
	rootCmd.AddCommand(metaCmd)

	// Create vocab command group
	vocabCmd := &cobra.Command{
		Use:   "vocab",
		Short: "Manage vocabulary definitions",
	}
	
	vocabListCmd, err := NewVocabListCommand()
	if err != nil {
		return err
	}
	vocabCmd.AddCommand(vocabListCmd)
	
	vocabAddCmd, err := NewVocabAddCommand()
	if err != nil {
		return err
	}
	vocabCmd.AddCommand(vocabAddCmd)
	
	vocabAssignCmd, err := NewVocabAssignCommand()
	if err != nil {
		return err
	}
	vocabCmd.AddCommand(vocabAssignCmd)
	
	rootCmd.AddCommand(vocabCmd)

	// Create list command group
	listCmd := &cobra.Command{
		Use:   "list",
		Short: "List tickets and documents",
	}
	
	listTicketsCmd, err := NewListTicketsCommand()
	if err != nil {
		return err
	}
	listCmd.AddCommand(listTicketsCmd)
	
	listDocsCmd, err := NewListDocsCommand()
	if err != nil {
		return err
	}
	listCmd.AddCommand(listDocsCmd)
	
	rootCmd.AddCommand(listCmd)

	// Create doctor command
	doctorCmd, err := NewDoctorCommand()
	if err != nil {
		return err
	}
	rootCmd.AddCommand(doctorCmd)

	// Create search command
	searchCmd, err := NewSearchCommand()
	if err != nil {
		return err
	}
	rootCmd.AddCommand(searchCmd)

	return nil
}

