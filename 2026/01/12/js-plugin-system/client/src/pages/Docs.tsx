import { Button } from "@/components/ui/button";
import { Card, CardContent, CardHeader, CardTitle } from "@/components/ui/card";
import {
  ArrowLeft,
  BookOpen,
  Cable,
  Code2,
  Layers,
  Zap,
} from "lucide-react";
import { useLocation } from "wouter";

const QUICK_START = `definePlugin(({ ui, createActions }) => {
  const actions = createActions("plugin.example", ["requested"]);

  return {
    id: "example",
    title: "Example Plugin",
    description: "Short summary",
    widgets: {
      ExampleWidget: {
        title: "Widget Title",
        render({ state }) {
          return ui.panel([
            ui.text("Hello"),
            ui.button("Run", { onClick: { handler: "run", args: "ping" } })
          ]);
        },
        handlers: {
          run({ dispatch, event }) {
            dispatch(actions.requested(event.args));
          }
        }
      }
    }
  };
});`;

const INPUT_EXAMPLE = `render({ state }) {
  const name = state.plugins.greeter?.name || "";
  return ui.panel([
    ui.text("Enter your name:"),
    ui.input(name, { onChange: { handler: "nameChanged" } })
  ]);
},
handlers: {
  nameChanged({ dispatch, event }) {
    dispatch(actions.nameChanged(event.value));
  }
}`;

const COUNTER_EXAMPLE = `render({ state }) {
  const count = state.plugins.counter || 0;
  return ui.panel([
    ui.counter(count, {
      onIncrement: { handler: "increment" },
      onDecrement: { handler: "decrement" }
    })
  ]);
},
handlers: {
  increment({ dispatch }) { dispatch(actions.incremented()); },
  decrement({ dispatch }) { dispatch(actions.decremented()); }
}`;

const STATE_SHAPE = `state.plugins.counter
state.plugins.greeter
state.plugins.calculator
state.plugins.plugins`;

const UI_HELPERS = [
  { name: "ui.panel(children, props?)", detail: "Panel container" },
  { name: "ui.row(children, props?)", detail: "Horizontal layout" },
  { name: "ui.column(children, props?)", detail: "Vertical layout" },
  { name: "ui.text(text, props?)", detail: "Monospace text" },
  { name: "ui.badge(text, props?)", detail: "Status badge" },
  { name: "ui.button(label, { onClick, variant })", detail: "Button with handler" },
  { name: "ui.input(value, { placeholder, onChange })", detail: "Text input" },
  { name: "ui.counter(value, { onIncrement, onDecrement })", detail: "Counter control" },
  { name: "ui.table(rows, { headers })", detail: "Read-only table" },
];

export default function Docs() {
  const [, setLocation] = useLocation();

  return (
    <div className="min-h-screen bg-[radial-gradient(circle_at_top,_rgba(12,23,32,0.9),_rgba(7,12,18,0.95)_60%,_rgba(5,8,13,1)_100%)] text-foreground">
      <header className="border-b border-accent/30 bg-card/50 shadow-[0_2px_24px_rgba(0,255,255,0.12)]">
        <div className="container py-6 flex flex-col gap-4 md:flex-row md:items-center md:justify-between">
          <div>
            <p className="font-mono text-xs uppercase tracking-wider text-accent/70">
              Plugin Playground
            </p>
            <h1 className="font-mono text-3xl font-bold uppercase tracking-wider text-accent">
              Plugin Scripting Reference
            </h1>
            <p className="font-mono text-xs text-muted-foreground mt-2 max-w-2xl">
              QuickJS plugin DSL guide, handler contract, state shape, and UI building blocks.
            </p>
          </div>
          <div className="flex flex-wrap gap-2">
            <Button
              variant="outline"
              className="font-mono text-xs uppercase tracking-wide border-accent/50"
              onClick={() => setLocation("/")}
            >
              <ArrowLeft className="w-4 h-4 mr-2" />
              Back to Playground
            </Button>
          </div>
        </div>
      </header>

      <main className="container py-10 space-y-10">
        <section className="grid gap-6 lg:grid-cols-[1.2fr_1fr]">
          <Card className="border-accent/30 bg-card/60">
            <CardHeader className="border-b border-accent/20">
              <CardTitle className="flex items-center gap-3 font-mono text-sm uppercase tracking-wider text-accent">
                <Code2 className="w-4 h-4" />
                Quick Start
              </CardTitle>
            </CardHeader>
            <CardContent className="pt-6">
              <pre className="text-xs font-mono text-accent/90 bg-black/40 border border-accent/20 rounded-sm p-4 overflow-x-auto whitespace-pre">
                {QUICK_START}
              </pre>
            </CardContent>
          </Card>

          <Card className="border-accent/30 bg-card/60">
            <CardHeader className="border-b border-accent/20">
              <CardTitle className="flex items-center gap-3 font-mono text-sm uppercase tracking-wider text-accent">
                <Layers className="w-4 h-4" />
                Plugin Anatomy
              </CardTitle>
            </CardHeader>
            <CardContent className="pt-6 space-y-3 text-sm">
              <p className="font-mono text-muted-foreground">
                Plugins are pure data + handlers. They run in a QuickJS sandbox and can only
                interact with the app through Redux actions.
              </p>
              <ul className="space-y-2 font-mono text-xs">
                <li className="text-foreground">
                  <span className="text-accent">id</span>: unique plugin id (also used in action namespace)
                </li>
                <li className="text-foreground">
                  <span className="text-accent">widgets</span>: map of widget definitions
                </li>
                <li className="text-foreground">
                  <span className="text-accent">render</span>: returns a UI tree built from <code>ui.*</code>
                </li>
                <li className="text-foreground">
                  <span className="text-accent">handlers</span>: functions invoked by UI events
                </li>
              </ul>
            </CardContent>
          </Card>
        </section>

        <section className="grid gap-6 lg:grid-cols-3">
          <Card className="border-accent/30 bg-card/60">
            <CardHeader className="border-b border-accent/20">
              <CardTitle className="flex items-center gap-3 font-mono text-sm uppercase tracking-wider text-accent">
                <Cable className="w-4 h-4" />
                Handlers + Events
              </CardTitle>
            </CardHeader>
            <CardContent className="pt-6 space-y-3 text-xs font-mono text-muted-foreground">
              <p>
                Handlers receive <code>dispatch</code>, <code>state</code>, and <code>event</code>.
                Buttons send <code>event.args</code>. Inputs send <code>event.value</code>.
              </p>
              <pre className="text-xs text-accent/90 bg-black/40 border border-accent/20 rounded-sm p-3 overflow-x-auto whitespace-pre">
                {INPUT_EXAMPLE}
              </pre>
            </CardContent>
          </Card>

          <Card className="border-accent/30 bg-card/60">
            <CardHeader className="border-b border-accent/20">
              <CardTitle className="flex items-center gap-3 font-mono text-sm uppercase tracking-wider text-accent">
                <Zap className="w-4 h-4" />
                Action Namespace
              </CardTitle>
            </CardHeader>
            <CardContent className="pt-6 space-y-3 text-xs font-mono text-muted-foreground">
              <p>
                Only actions with <code>plugin.&lt;pluginId&gt;/</code> are accepted by default.
              </p>
              <pre className="text-xs text-accent/90 bg-black/40 border border-accent/20 rounded-sm p-3 overflow-x-auto whitespace-pre">
                createActions("plugin.greeter", ["nameChanged"])
              </pre>
              <p>
                Match your <code>id</code> and namespace to avoid dispatch blocks.
              </p>
            </CardContent>
          </Card>

          <Card className="border-accent/30 bg-card/60">
            <CardHeader className="border-b border-accent/20">
              <CardTitle className="flex items-center gap-3 font-mono text-sm uppercase tracking-wider text-accent">
                <BookOpen className="w-4 h-4" />
                State Shape
              </CardTitle>
            </CardHeader>
            <CardContent className="pt-6 space-y-3 text-xs font-mono text-muted-foreground">
              <p>
                Redux state passed to <code>render</code> and handlers sits under <code>state.plugins</code>.
              </p>
              <pre className="text-xs text-accent/90 bg-black/40 border border-accent/20 rounded-sm p-3 overflow-x-auto whitespace-pre">
                {STATE_SHAPE}
              </pre>
            </CardContent>
          </Card>
        </section>

        <section className="grid gap-6 lg:grid-cols-[1.1fr_0.9fr]">
          <Card className="border-accent/30 bg-card/60">
            <CardHeader className="border-b border-accent/20">
              <CardTitle className="flex items-center gap-3 font-mono text-sm uppercase tracking-wider text-accent">
                <Code2 className="w-4 h-4" />
                UI DSL Reference
              </CardTitle>
            </CardHeader>
            <CardContent className="pt-6 space-y-3">
              <div className="grid gap-2 text-xs font-mono">
                {UI_HELPERS.map((helper) => (
                  <div
                    key={helper.name}
                    className="flex flex-col gap-1 border border-accent/20 rounded-sm p-3 bg-black/30"
                  >
                    <span className="text-accent">{helper.name}</span>
                    <span className="text-muted-foreground">{helper.detail}</span>
                  </div>
                ))}
              </div>
            </CardContent>
          </Card>

          <Card className="border-accent/30 bg-card/60">
            <CardHeader className="border-b border-accent/20">
              <CardTitle className="flex items-center gap-3 font-mono text-sm uppercase tracking-wider text-accent">
                <Layers className="w-4 h-4" />
                Counter Widget
              </CardTitle>
            </CardHeader>
            <CardContent className="pt-6 space-y-3 text-xs font-mono text-muted-foreground">
              <p>Use the built-in counter node to get +/- behavior with minimal wiring.</p>
              <pre className="text-xs text-accent/90 bg-black/40 border border-accent/20 rounded-sm p-3 overflow-x-auto whitespace-pre">
                {COUNTER_EXAMPLE}
              </pre>
            </CardContent>
          </Card>
        </section>

        <section>
          <Card className="border-accent/30 bg-card/60">
            <CardHeader className="border-b border-accent/20">
              <CardTitle className="flex items-center gap-3 font-mono text-sm uppercase tracking-wider text-accent">
                <Zap className="w-4 h-4" />
                Sandbox Lifecycle + Editor Tips
              </CardTitle>
            </CardHeader>
            <CardContent className="pt-6 space-y-3 text-xs font-mono text-muted-foreground">
              <ul className="space-y-2">
                <li>Load: code is evaluated once with <code>definePlugin</code>.</li>
                <li>Render: the host calls your widget <code>render</code> with Redux state.</li>
                <li>Event: handler executes with <code>dispatch</code> and the event payload.</li>
                <li>Unload: VM context is disposed when the plugin is removed.</li>
              </ul>
              <p>
                The editor ships with snippet completion for <code>definePlugin</code> and
                <code>ui.*</code> helpers. Type hints come from an embedded plugin API d.ts.
              </p>
            </CardContent>
          </Card>
        </section>
      </main>
    </div>
  );
}
