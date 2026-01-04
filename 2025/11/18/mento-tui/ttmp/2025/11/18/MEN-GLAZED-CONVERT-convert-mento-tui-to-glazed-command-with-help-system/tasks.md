# Tasks

## TODO

- [ ] Add Glazed dependencies to `go.mod` (glazed/pkg/cmds, glazed/pkg/cmds/layers, glazed/pkg/cmds/parameters, glazed/pkg/help, glazed/pkg/help/cmd, spf13/cobra) and run `go mod tidy`

- [ ] Create `MentoTuiCommand` struct that implements `BareCommand` interface, embedding `*cmds.CommandDescription`

- [ ] Create settings struct for command parameters (e.g., `ConfigPath`) with `glazed.parameter` tags

- [ ] Implement `Run()` method on `MentoTuiCommand` that starts the TUI (extract current main logic)

- [ ] Create constructor function `NewMentoTuiCommand()` that sets up command description, defines `--config` parameter, and adds command settings layer

- [ ] Update `cmd/main.go` to create root Cobra command, build Glazed command using `cli.BuildCobraCommand()`, add to root, and execute

- [ ] Create `internal/doc/` package with `doc.go` containing embed directive for markdown files

- [ ] Initialize help system in `main.go`, load documentation from embedded filesystem, and register with Cobra root using `help_cmd.SetupCobraRootCommand()`

- [ ] Review documentation style guidelines (`glaze help how-to-write-good-documentation-pages`)

- [ ] Write `config-yaml-reference.md` help topic documenting YAML structure, service fields, global fields, with examples and proper frontmatter (Topics: [config, yaml], Commands: [mento-tui], SectionType: GeneralTopic)

- [ ] Write `usage.md` help topic documenting command usage and `--config` flag with examples (Topics: [usage, cli], Commands: [mento-tui], SectionType: GeneralTopic)

- [ ] Write `service-management.md` help topic documenting dashboard features, keyboard shortcuts, log viewer, and configuration viewer (Topics: [tui, dashboard, services], Commands: [mento-tui], SectionType: GeneralTopic)

- [ ] Write `getting-started.md` tutorial with step-by-step setup guide, configuration file creation, running the application, and basic usage examples (Topics: [tutorial, getting-started], Commands: [mento-tui], SectionType: Tutorial)

- [ ] Test `mento-tui help` command shows help system

- [ ] Test `mento-tui help config-yaml-reference` shows config docs

- [ ] Test `mento-tui help usage` shows usage guide

- [ ] Test `mento-tui help service-management` shows service guide

- [ ] Test `mento-tui help getting-started` shows tutorial

- [ ] Test `mento-tui --help` shows command help

- [ ] Test `mento-tui --config <path>` still works

- [ ] Verify TUI functionality unchanged after conversion

- [ ] Test help system query functionality (e.g., `mento-tui help --list`)

- [ ] Update `README.md` to mention help system and add section about accessing help documentation

- [ ] Update installation/build instructions if needed and document new Glazed-based architecture
