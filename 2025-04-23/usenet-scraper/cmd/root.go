package cmd

import (
	"fmt"
	"os"

	"github.com/spf13/cobra"
	"github.com/usenet-scraper/internal/config"
	"github.com/usenet-scraper/internal/output"
	"github.com/usenet-scraper/internal/parser"
	"github.com/usenet-scraper/internal/scraper"
	"github.com/usenet-scraper/internal/web"
)

var (
	cfgFile string
	webMode bool
)

// rootCmd represents the base command
var rootCmd = &cobra.Command{
	Use:   "usenet-scraper",
	Short: "A Usenet scraper based on YAML DSL",
	Long: `UsenetScraper is a tool for scraping Usenet newsgroups based on a YAML DSL configuration.
It supports various transformations, output formats, and a web interface.`,
	Run: func(cmd *cobra.Command, args []string) {
		// Load configuration
		cfg, err := config.LoadConfig(cfgFile)
		if err != nil {
			fmt.Printf("Error loading configuration: %v\n", err)
			os.Exit(1)
		}

		// Create parser
		p, err := parser.NewParser(cfg)
		if err != nil {
			fmt.Printf("Error creating parser: %v\n", err)
			os.Exit(1)
		}

		// Create scraper
		s, err := scraper.NewScraper(cfg, p)
		if err != nil {
			fmt.Printf("Error creating scraper: %v\n", err)
			os.Exit(1)
		}

		// Create output writer
		w, err := output.NewWriter(cfg.Output)
		if err != nil {
			fmt.Printf("Error creating output writer: %v\n", err)
			os.Exit(1)
		}
		defer w.Close()

		// Start web interface if enabled
		if webMode || (cfg.WebInterface != nil && cfg.WebInterface.Enabled) {
			webCfg := cfg.WebInterface
			if webCfg == nil {
				webCfg = &config.WebInterfaceConfig{
					Enabled: true,
					Port:    8080,
					Host:    "localhost",
				}
			}

			webServer := web.NewServer(webCfg, s, p)
			if err := webServer.Start(); err != nil {
				fmt.Printf("Error starting web interface: %v\n", err)
				os.Exit(1)
			}

			fmt.Printf("Web interface started at http://%s:%d\n", webCfg.Host, webCfg.Port)
			fmt.Println("Press Ctrl+C to stop")

			// Wait for interrupt
			c := make(chan os.Signal, 1)
			<-c

			// Stop the web interface
			if err := webServer.Stop(); err != nil {
				fmt.Printf("Error stopping web interface: %v\n", err)
			}
		} else {
			// Start scraping
			if err := s.Start(); err != nil {
				fmt.Printf("Error starting scraper: %v\n", err)
				os.Exit(1)
			}

			// Wait for completion
			s.Wait()
		}
	},
}

// Execute adds all child commands to the root command and sets flags appropriately.
func Execute() {
	if err := rootCmd.Execute(); err != nil {
		fmt.Println(err)
		os.Exit(1)
	}
}

func init() {
	rootCmd.PersistentFlags().StringVar(&cfgFile, "config", "usenet_scraper.yaml", "config file path")
	rootCmd.PersistentFlags().BoolVar(&webMode, "web", false, "start in web interface mode")
}
