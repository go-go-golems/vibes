package seed

type Spec struct {
	BasePath string `yaml:"base_path"`
	Sets     []Set  `yaml:"sets"`
}

type Set struct {
	Path  string            `yaml:"path"`
	Data  map[string]string `yaml:"data"`
	Env   map[string]string `yaml:"env"`
	Files map[string]string `yaml:"files"`
}
