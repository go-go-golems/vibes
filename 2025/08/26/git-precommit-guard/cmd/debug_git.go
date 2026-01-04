package cmd

import (
    "context"
    "fmt"
    "sort"

    "github.com/go-go-golems/glazed/pkg/cmds"
    "github.com/go-go-golems/glazed/pkg/cmds/layers"
    "github.com/go-go-golems/glazed/pkg/cmds/parameters"
    "github.com/pkg/errors"

    gitpkg "github.com/user/git-precommit-guard/pkg/git"
    gogit "github.com/go-git/go-git/v5"
)

// DebugGitCommand prints repository info and status as seen by go-git and our helpers.
type DebugGitCommand struct {
    *cmds.CommandDescription
}

type DebugGitSettings struct {
    Limit int `glazed.parameter:"limit"`
}

func NewDebugGitCommand() (*DebugGitCommand, error) {
    cd := cmds.NewCommandDescription(
        "git",
        cmds.WithShort("Show go-git view of repo, roots and staged files"),
        cmds.WithFlags(
            parameters.NewParameterDefinition("limit", parameters.ParameterTypeInteger,
                parameters.WithDefault(0), parameters.WithHelp("Limit number of files printed (0 = no limit)")),
        ),
    )
    return &DebugGitCommand{CommandDescription: cd}, nil
}

func (c *DebugGitCommand) Run(ctx context.Context, pl *layers.ParsedLayers) error {
    settings := &DebugGitSettings{}
    if err := pl.InitializeStruct(layers.DefaultSlug, settings); err != nil {
        return errors.Wrap(err, "parse settings")
    }
    fmt.Println("go-git repository diagnostics\n==============================")

    fmt.Printf("IsGitRepository: %v\n", gitpkg.IsGitRepository())

    root, err := gitpkg.GetRepositoryRoot()
    if err != nil {
        return errors.Wrap(err, "GetRepositoryRoot")
    }
    fmt.Printf("RepositoryRoot: %s\n", root)

    gitDir, err := gitpkg.GetGitDir()
    if err != nil {
        return errors.Wrap(err, "GetGitDir")
    }
    fmt.Printf("GitDir: %s\n", gitDir)

    // Open repo and print raw status (staging/worktree)
    repo, err := gogit.PlainOpenWithOptions(root, &gogit.PlainOpenOptions{DetectDotGit: true})
    if err != nil {
        return errors.Wrap(err, "PlainOpenWithOptions")
    }
    wt, err := repo.Worktree()
    if err != nil {
        return errors.Wrap(err, "Worktree")
    }
    st, err := wt.Status()
    if err != nil {
        return errors.Wrap(err, "Status")
    }

    // Stable output
    keys := make([]string, 0, len(st))
    for k := range st { keys = append(keys, k) }
    sort.Strings(keys)

    fmt.Println("\nRaw Status (<staging><worktree> path):")
    printed := 0
    for _, k := range keys {
        fs := st[k]
        if fs.Staging == gogit.Unmodified && fs.Worktree == gogit.Unmodified { continue }
        extra := ""
        if fs.Extra != "" { extra = " (extra: " + fs.Extra + ")" }
        fmt.Printf("  %c%c %s%s\n", fs.Staging, fs.Worktree, k, extra)
        printed++
        if settings.Limit > 0 && printed >= settings.Limit { break }
    }

    // Our helpers
    fmt.Println("\nGetStagedFiles():")
    files, err := gitpkg.GetStagedFiles()
    if err != nil { return errors.Wrap(err, "GetStagedFiles") }
    printed = 0
    for _, f := range files {
        fmt.Printf("  %s %s\n", f.Status, f.Path)
        printed++
        if settings.Limit > 0 && printed >= settings.Limit { break }
    }

    fmt.Println("\nGetStagedFilePaths():")
    paths, err := gitpkg.GetStagedFilePaths()
    if err != nil { return errors.Wrap(err, "GetStagedFilePaths") }
    printed = 0
    for _, p := range paths { 
        fmt.Printf("  %s\n", p)
        printed++
        if settings.Limit > 0 && printed >= settings.Limit { break }
    }

    has, err := gitpkg.HasStagedChanges()
    if err != nil { return errors.Wrap(err, "HasStagedChanges") }
    fmt.Printf("\nHasStagedChanges: %v\n", has)

    return nil
}

var _ cmds.BareCommand = &DebugGitCommand{}


