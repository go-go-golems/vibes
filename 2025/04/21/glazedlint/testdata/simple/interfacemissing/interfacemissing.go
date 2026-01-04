package interfacemissing

// CmdInterface represents a simplified version of the GlazeCommand interface
type CmdInterface interface {
	Run() error
	RunIntoProcessor() error
}

// GoodCmd implements both required methods
type GoodCmd struct{}

func (c *GoodCmd) Run() error {
	return nil
}

func (c *GoodCmd) RunIntoProcessor() error {
	return nil
}

// BadCmd only implements Run but not RunIntoProcessor
type BadCmd struct{}

func (c *BadCmd) Run() error {
	return nil
}

// This should trigger our linter for missing RunIntoProcessor method
