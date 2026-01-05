import { 
  Type, 
  ListChecks, 
  CircleDot, 
  CheckSquare, 
  AlignLeft, 
  ChevronDown,
  ToggleLeft,
  FileText
} from 'lucide-react';

export interface Widget {
  id: string;
  name: string;
  icon: typeof Type;
  description: string;
  snippet: string;
}

export const widgets: Widget[] = [
  {
    id: 'text-input',
    name: 'Text Input',
    icon: Type,
    description: 'Single line text field',
    snippet: `  - name: field_name
    label: Your question here
    type: text
    placeholder: Enter your answer`,
  },
  {
    id: 'textarea',
    name: 'Text Area',
    icon: AlignLeft,
    description: 'Multi-line text field',
    snippet: `  - name: field_name
    label: Your question here
    type: textarea
    placeholder: Enter your response...
    rows: 3`,
  },
  {
    id: 'radio',
    name: 'Radio Buttons',
    icon: CircleDot,
    description: 'Single choice from options',
    snippet: `  - name: field_name
    label: Your question here
    type: radio
    options:
      - { label: "Option A", value: "a" }
      - { label: "Option B", value: "b" }
      - { label: "Option C", value: "c" }
    correct: "a"`,
  },
  {
    id: 'checkbox',
    name: 'Checkboxes',
    icon: CheckSquare,
    description: 'Multiple choice selection',
    snippet: `  - name: field_name
    label: Select all that apply
    type: checkbox
    options:
      - { label: "Option A", value: "a" }
      - { label: "Option B", value: "b" }
      - { label: "Option C", value: "c" }
    correct: ["a", "b"]`,
  },
  {
    id: 'select',
    name: 'Dropdown',
    icon: ChevronDown,
    description: 'Dropdown selection',
    snippet: `  - name: field_name
    label: Choose an option
    type: select
    options:
      - { label: "Option 1", value: "1" }
      - { label: "Option 2", value: "2" }
      - { label: "Option 3", value: "3" }`,
  },
  {
    id: 'confirm',
    name: 'Confirm Toggle',
    icon: ToggleLeft,
    description: 'Yes/No confirmation',
    snippet: `  - name: field_name
    label: Do you agree?
    type: confirm
    placeholder: Yes, I confirm`,
  },
];

export const formWrapper = {
  id: 'form-wrapper',
  name: 'New Form',
  icon: FileText,
  description: 'Create a new form block',
  snippet: `<form id="quiz-name">
name: Quiz Title
description: Quiz description here
fields:
</form>`,
};

export function getWidgetById(id: string): Widget | undefined {
  return widgets.find(w => w.id === id);
}
