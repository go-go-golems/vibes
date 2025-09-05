package main

import (
    "github.com/go-go-golems/glazed/pkg/cli"
    "github.com/go-go-golems/glazed/pkg/cmds/logging"
    "github.com/go-go-golems/glazed/pkg/help"
    help_cmd "github.com/go-go-golems/glazed/pkg/help/cmd"
    "github.com/spf13/cobra"
    "github.com/spf13/viper"

    appcmds "vault-envrc-generator/cmds"
    appdoc "vault-envrc-generator/pkg/doc"
)

var version = "dev"

func main() {
    rootCmd := &cobra.Command{
        Use:     "vault-envrc-generator",
        Short:   "Generate envrc and seed Vault via glazed commands",
        Version: version,
        PersistentPreRun: func(cmd *cobra.Command, args []string) {
            _ = logging.InitLoggerFromViper()
        },
    }

    // Add logging layer flags on root
    _ = logging.AddLoggingLayerToRootCommand(rootCmd, "vault-envrc-generator")
    _ = viper.BindPFlags(rootCmd.PersistentFlags())
    _ = logging.InitLoggerFromViper()

    // Help system
    hs := help.NewHelpSystem()
    _ = appdoc.AddDocToHelpSystem(hs)
    help_cmd.SetupCobraRootCommand(hs, rootCmd)

    // Register commands
    if bc, err := appcmds.NewBatchCommand(); err == nil {
        cmd, err := cli.BuildCobraCommand(bc)
        cobra.CheckErr(err)
        rootCmd.AddCommand(cmd)
    } else { cobra.CheckErr(err) }

    if sc, err := appcmds.NewSeedCommand(); err == nil {
        cmd, err := cli.BuildCobraCommand(sc)
        cobra.CheckErr(err)
        rootCmd.AddCommand(cmd)
    } else { cobra.CheckErr(err) }

    if gc, err := appcmds.NewGenerateCommand(); err == nil {
        cmd, err := cli.BuildCobraCommand(gc)
        cobra.CheckErr(err)
        rootCmd.AddCommand(cmd)
    } else { cobra.CheckErr(err) }

    if lc, err := appcmds.NewListCommand(); err == nil {
        cmd, err := cli.BuildCobraCommand(lc)
        cobra.CheckErr(err)
        rootCmd.AddCommand(cmd)
    } else { cobra.CheckErr(err) }

    if ic, err := appcmds.NewInteractiveCommand(); err == nil {
        cmd, err := cli.BuildCobraCommand(ic)
        cobra.CheckErr(err)
        rootCmd.AddCommand(cmd)
    } else { cobra.CheckErr(err) }

    cobra.CheckErr(rootCmd.Execute())
}
