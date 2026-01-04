# Glazed API Fix Notes

## Correct Function Names
- Use `settings.NewGlazedParameterLayers()` (not NewGlazeParameterLayers)
- Use `layers.DefaultSlug` for parameter initialization

## Correct Parameter Initialization Pattern
```go
// In NewCommand function:
glazedParameterLayer, err := settings.NewGlazedParameterLayers()
if err != nil {
    return nil, err
}

glazedLayers := layers.NewParameterLayers()
glazedLayers.Set(settings.GlazedSlug, glazedParameterLayer)

// Add to command description:
cmds.WithLayers(glazedLayers),

// In RunIntoGlazeProcessor:
s := struct {
    URL    string `glazed.parameter:"url"`
    Output string `glazed.parameter:"output"`
    // ... other fields
}{}

err := parsedLayers.InitializeStruct(layers.DefaultSlug, s)
if err != nil {
    return errors.Wrap(err, "failed to initialize parameters")
}
```

## Key Points
1. Must include glazed parameter layers in command description
2. Use struct tags with `glazed.parameter:"param-name"`
3. Initialize with `layers.DefaultSlug`
4. Parameter names in tags must match flag names exactly

