// Design Philosophy: Technical Brutalism - Raw code editor with monospace aesthetic
// Monaco editor with dark theme matching terminal aesthetic

import React from "react";
import Editor from "@monaco-editor/react";
import { Button } from "@/components/ui/button";
import { Play, Save, X } from "lucide-react";

const PLUGIN_API_DTS = `
type UIEventRef = { handler: string; args?: any };
type UINode =
  | { kind: "panel" | "row" | "column"; props?: any; children?: UINode[] }
  | { kind: "text" | "badge"; props?: any; text: string }
  | { kind: "button"; props: { label: string; onClick?: UIEventRef; variant?: string } }
  | { kind: "input"; props: { value: string; placeholder?: string; onChange?: UIEventRef } }
  | { kind: "counter"; props: { value: number; onIncrement?: UIEventRef; onDecrement?: UIEventRef } }
  | { kind: "table"; props: { headers: string[]; rows: any[][] } };

type UIBuilder = {
  panel(children: UINode[], props?: any): UINode;
  row(children: UINode[], props?: any): UINode;
  column(children: UINode[], props?: any): UINode;
  text(text: string, props?: any): UINode;
  badge(text: string, props?: any): UINode;
  button(label: string, opts?: { onClick?: UIEventRef; variant?: string }): UINode;
  input(value: string, opts?: { placeholder?: string; onChange?: UIEventRef }): UINode;
  counter(value: number, opts?: { onIncrement?: UIEventRef; onDecrement?: UIEventRef }): UINode;
  table(rows: any[][], opts?: { headers?: string[] }): UINode;
};

type ActionCreator = (payload?: any) => { type: string; payload?: any };

type PluginContext = {
  ui: UIBuilder;
  createActions(namespace: string, names: string[]): Record<string, ActionCreator>;
};

type PluginWidget = {
  title?: string;
  render(context: { state: any }): UINode;
  handlers: Record<string, (context: { dispatch: (action: any) => void; state: any; event: any }, args?: any) => void>;
};

type PluginDefinition = {
  id: string;
  title?: string;
  description?: string;
  widgets: Record<string, PluginWidget>;
};

declare function definePlugin(def: (context: PluginContext) => PluginDefinition): void;
`;

const PLUGIN_SNIPPET = `definePlugin(({ ui, createActions }) => {
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
            dispatch(actions.requested({ payload: event.args }));
          }
        }
      }
    }
  };
});
`;

interface PluginEditorProps {
  code: string;
  onChange: (code: string) => void;
  onRun: () => void;
  onClose?: () => void;
  readOnly?: boolean;
}

export function PluginEditor({ code, onChange, onRun, onClose, readOnly = false }: PluginEditorProps) {
  const monacoSetupRef = React.useRef(false);

  return (
    <div className="flex flex-col h-full border border-accent/30 rounded-sm overflow-hidden bg-card shadow-[0_0_20px_rgba(0,255,255,0.1)]">
      <div className="flex items-center justify-between px-4 py-2 border-b border-accent/30 bg-accent/5">
        <div className="flex items-center gap-2">
          <span className="font-mono text-xs uppercase tracking-wider text-accent font-bold">
            Plugin Editor
          </span>
          <span className="font-mono text-xs text-muted-foreground">
            [QuickJS VM]
          </span>
        </div>
        <div className="flex items-center gap-2">
          {!readOnly && (
            <Button
              onClick={onRun}
              size="sm"
              variant="outline"
              className="font-mono text-xs uppercase tracking-wide border-accent/50 hover:shadow-[0_0_10px_rgba(0,255,255,0.4)]"
            >
              <Play className="w-3 h-3 mr-1" />
              Load Plugin
            </Button>
          )}
          {onClose && (
            <Button
              onClick={onClose}
              size="sm"
              variant="ghost"
              className="font-mono text-xs"
            >
              <X className="w-4 h-4" />
            </Button>
          )}
        </div>
      </div>
      
      <div className="flex-1 min-h-0">
        <Editor
          height="100%"
          defaultLanguage="javascript"
          value={code}
          onChange={(value) => onChange(value || "")}
          onMount={(_, monaco) => {
            if (monacoSetupRef.current) return;
            monacoSetupRef.current = true;

            monaco.languages.typescript.javascriptDefaults.setCompilerOptions({
              allowNonTsExtensions: true,
              target: monaco.languages.typescript.ScriptTarget.ES2020,
            });
            monaco.languages.typescript.javascriptDefaults.addExtraLib(PLUGIN_API_DTS, "ts:plugin-api.d.ts");
            monaco.languages.registerCompletionItemProvider("javascript", {
              provideCompletionItems: () => ({
                suggestions: [
                  {
                    label: "definePlugin",
                    kind: monaco.languages.CompletionItemKind.Snippet,
                    insertText: PLUGIN_SNIPPET,
                    insertTextRules: monaco.languages.CompletionItemInsertTextRule.InsertAsSnippet,
                    detail: "Plugin scaffold",
                  },
                  {
                    label: "ui.panel",
                    kind: monaco.languages.CompletionItemKind.Function,
                    insertText: "ui.panel([\\n  $0\\n])",
                    insertTextRules: monaco.languages.CompletionItemInsertTextRule.InsertAsSnippet,
                    detail: "Panel container",
                  },
                  {
                    label: "ui.row",
                    kind: monaco.languages.CompletionItemKind.Function,
                    insertText: "ui.row([\\n  $0\\n])",
                    insertTextRules: monaco.languages.CompletionItemInsertTextRule.InsertAsSnippet,
                    detail: "Row container",
                  },
                  {
                    label: "ui.column",
                    kind: monaco.languages.CompletionItemKind.Function,
                    insertText: "ui.column([\\n  $0\\n])",
                    insertTextRules: monaco.languages.CompletionItemInsertTextRule.InsertAsSnippet,
                    detail: "Column container",
                  },
                  {
                    label: "ui.text",
                    kind: monaco.languages.CompletionItemKind.Function,
                    insertText: "ui.text(\"$0\")",
                    insertTextRules: monaco.languages.CompletionItemInsertTextRule.InsertAsSnippet,
                    detail: "Text node",
                  },
                  {
                    label: "ui.badge",
                    kind: monaco.languages.CompletionItemKind.Function,
                    insertText: "ui.badge(\"$0\")",
                    insertTextRules: monaco.languages.CompletionItemInsertTextRule.InsertAsSnippet,
                    detail: "Badge node",
                  },
                  {
                    label: "ui.button",
                    kind: monaco.languages.CompletionItemKind.Function,
                    insertText: "ui.button(\"$1\", { onClick: { handler: \"$2\" } })",
                    insertTextRules: monaco.languages.CompletionItemInsertTextRule.InsertAsSnippet,
                    detail: "Button node",
                  },
                  {
                    label: "ui.input",
                    kind: monaco.languages.CompletionItemKind.Function,
                    insertText: "ui.input(\"$1\", { placeholder: \"$2\", onChange: { handler: \"$3\" } })",
                    insertTextRules: monaco.languages.CompletionItemInsertTextRule.InsertAsSnippet,
                    detail: "Input node",
                  },
                  {
                    label: "ui.counter",
                    kind: monaco.languages.CompletionItemKind.Function,
                    insertText: "ui.counter($1, { onIncrement: { handler: \"$2\" }, onDecrement: { handler: \"$3\" } })",
                    insertTextRules: monaco.languages.CompletionItemInsertTextRule.InsertAsSnippet,
                    detail: "Counter node",
                  },
                  {
                    label: "ui.table",
                    kind: monaco.languages.CompletionItemKind.Function,
                    insertText: "ui.table([\\n  $0\\n], { headers: [] })",
                    insertTextRules: monaco.languages.CompletionItemInsertTextRule.InsertAsSnippet,
                    detail: "Table widget",
                  },
                ],
              }),
            });
          }}
          theme="vs-dark"
          options={{
            minimap: { enabled: false },
            fontSize: 13,
            fontFamily: "'JetBrains Mono', 'Courier New', monospace",
            lineNumbers: "on",
            scrollBeyondLastLine: false,
            automaticLayout: true,
            tabSize: 2,
            readOnly,
            wordWrap: "on",
            padding: { top: 16, bottom: 16 },
          }}
        />
      </div>
    </div>
  );
}
