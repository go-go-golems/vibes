package main

import (
	"context"
	"flag"
	"fmt"
	"log"
	"os"
	"os/signal"
	"syscall"
	"time"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/wish"
	"github.com/charmbracelet/wish/bubbletea"
	"github.com/charmbracelet/wish/logging"
	"github.com/charmbracelet/ssh"

	"dotvote-ssh/pkg/auth"
	"dotvote-ssh/pkg/session"
	"dotvote-ssh/pkg/ui/model"
)

var (
	host      = flag.String("host", "localhost", "Host to listen on")
	port      = flag.Int("port", 2323, "Port to listen on")
	rolesFile = flag.String("roles", "roles.json", "Path to roles configuration file")
)

func main() {
	flag.Parse()

	// Initialize managers
	authManager := auth.NewAuthManager()
	sessionManager := session.NewManager()

	// Load roles configuration
	if err := authManager.LoadRoles(*rolesFile); err != nil {
		log.Fatalf("Failed to load roles: %v", err)
	}

	// Create SSH server
	srv, err := wish.NewServer(
		wish.WithAddress(fmt.Sprintf("%s:%d", *host, *port)),
		wish.WithHostKeyPath("host_ed25519"),
		wish.WithPublicKeyAuth(func(ctx ssh.Context, key ssh.PublicKey) bool {
			userInfo, authenticated := authManager.AuthenticateKey(key)
			if !authenticated {
				return false
			}

			// Store user info in context
			ctx.SetValue("user", userInfo)
			return true
		}),
		wish.WithMiddleware(
			bubbletea.Middleware(func(s ssh.Session) (tea.Model, []tea.ProgramOption) {
				// Get user info from context
				userInfo, ok := s.Context().Value("user").(*auth.UserInfo)
				if !ok {
					log.Printf("Failed to get user info from context")
					return nil, nil
				}

				// Create the main model
				m := model.NewMainModel(userInfo, sessionManager, authManager)

				// Configure tea program options
				opts := []tea.ProgramOption{
					tea.WithAltScreen(),
				}

				return m, opts
			}),
			logging.Middleware(),
		),
	)
	if err != nil {
		log.Fatalf("Failed to create server: %v", err)
	}

	// Start cleanup routine
	go func() {
		ticker := time.NewTicker(1 * time.Hour)
		defer ticker.Stop()
		
		for range ticker.C {
			sessionManager.CleanupInactiveSessions()
		}
	}()

	// Handle graceful shutdown
	done := make(chan os.Signal, 1)
	signal.Notify(done, os.Interrupt, syscall.SIGINT, syscall.SIGTERM)

	log.Printf("Starting SSH server on %s:%d", *host, *port)
	log.Printf("Roles configuration: %s", *rolesFile)

	go func() {
		if err := srv.ListenAndServe(); err != nil {
			log.Fatalf("Failed to start server: %v", err)
		}
	}()

	<-done
	log.Println("Stopping SSH server")
	ctx, cancel := context.WithTimeout(context.Background(), 30*time.Second)
	defer cancel()
	if err := srv.Shutdown(ctx); err != nil {
		log.Fatalf("Failed to shutdown server: %v", err)
	}
}

