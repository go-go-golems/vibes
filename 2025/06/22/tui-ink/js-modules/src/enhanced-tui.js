// Enhanced TUI library compatible with goja
// This creates a more sophisticated text-based interface

function SimpleTUI() {
  this.components = [];
  this.currentScreen = '';
  this.inputHandlers = [];
  this.focusedComponent = 0;
}

SimpleTUI.prototype.addComponent = function(component) {
  this.components.push(component);
};

SimpleTUI.prototype.render = function() {
  var output = '';
  for (var i = 0; i < this.components.length; i++) {
    var component = this.components[i];
    if (component.focused && this.focusedComponent === i) {
      component.focused = true;
    } else {
      component.focused = false;
    }
    output += this.components[i].render() + '\n';
  }
  this.currentScreen = output;
  return output;
};

SimpleTUI.prototype.onInput = function(handler) {
  this.inputHandlers.push(handler);
};

SimpleTUI.prototype.handleInput = function(input) {
  for (var i = 0; i < this.inputHandlers.length; i++) {
    var result = this.inputHandlers[i](input);
    if (result) return result;
  }
};

// Text component
function TextComponent(text, color, bold) {
  this.text = text || '';
  this.color = color || 'white';
  this.bold = bold || false;
  this.focused = false;
}

TextComponent.prototype.render = function() {
  var colors = {
    'red': '\x1b[31m',
    'green': '\x1b[32m',
    'yellow': '\x1b[33m',
    'blue': '\x1b[34m',
    'magenta': '\x1b[35m',
    'cyan': '\x1b[36m',
    'white': '\x1b[37m',
    'gray': '\x1b[90m',
    'reset': '\x1b[0m',
    'bold': '\x1b[1m'
  };
  
  var output = '';
  if (this.bold) output += colors['bold'];
  output += colors[this.color] || colors['white'];
  output += this.text;
  output += colors['reset'];
  
  return output;
};

TextComponent.prototype.setText = function(text) {
  this.text = text;
};

// Box component with border
function BoxComponent(title, content, width, height, color) {
  this.title = title || '';
  this.content = content || '';
  this.width = width || 50;
  this.height = height || 10;
  this.color = color || 'white';
  this.focused = false;
}

BoxComponent.prototype.render = function() {
  var colors = {
    'red': '\x1b[31m',
    'green': '\x1b[32m',
    'yellow': '\x1b[33m',
    'blue': '\x1b[34m',
    'magenta': '\x1b[35m',
    'cyan': '\x1b[36m',
    'white': '\x1b[37m',
    'reset': '\x1b[0m'
  };
  
  var colorCode = colors[this.color] || colors['white'];
  var output = '';
  
  // Top border
  var border = this.focused ? '═' : '─';
  var corners = this.focused ? ['╔', '╗', '╚', '╝'] : ['┌', '┐', '└', '┘'];
  var sides = this.focused ? '║' : '│';
  
  output += colorCode + corners[0] + Array(this.width - 1).join(border) + corners[1] + colors['reset'] + '\n';
  
  // Title line
  if (this.title) {
    var titleText = ' ' + this.title + ' ';
    var padding = Math.max(0, this.width - titleText.length - 2);
    var leftPad = Math.floor(padding / 2);
    var rightPad = padding - leftPad;
    
    output += colorCode + sides + Array(leftPad + 1).join(' ') + titleText + Array(rightPad + 1).join(' ') + sides + colors['reset'] + '\n';
    output += colorCode + '├' + Array(this.width - 1).join(border) + '┤' + colors['reset'] + '\n';
  }
  
  // Content lines
  var lines = this.content.split('\n');
  var contentHeight = this.title ? this.height - 4 : this.height - 2;
  
  for (var i = 0; i < contentHeight; i++) {
    var line = lines[i] || '';
    var truncatedLine = line.length > this.width - 4 ? line.substring(0, this.width - 4) : line;
    var padding = this.width - truncatedLine.length - 3;
    
    output += colorCode + sides + ' ' + truncatedLine + Array(padding + 1).join(' ') + sides + colors['reset'] + '\n';
  }
  
  // Bottom border
  output += colorCode + corners[2] + Array(this.width - 1).join(border) + corners[3] + colors['reset'];
  
  return output;
};

BoxComponent.prototype.setContent = function(content) {
  this.content = content;
};

// Progress bar component
function ProgressComponent(label, value, max, width, color) {
  this.label = label || 'Progress';
  this.value = value || 0;
  this.max = max || 100;
  this.width = width || 30;
  this.color = color || 'green';
}

ProgressComponent.prototype.render = function() {
  var colors = {
    'red': '\x1b[31m',
    'green': '\x1b[32m',
    'yellow': '\x1b[33m',
    'blue': '\x1b[34m',
    'cyan': '\x1b[36m',
    'white': '\x1b[37m',
    'reset': '\x1b[0m'
  };
  
  var percentage = Math.min(100, Math.max(0, (this.value / this.max) * 100));
  var filled = Math.floor((percentage / 100) * this.width);
  var empty = this.width - filled;
  
  var colorCode = colors[this.color] || colors['green'];
  var bar = '[' + colorCode + Array(filled + 1).join('█') + colors['reset'] + Array(empty + 1).join('░') + ']';
  
  return this.label + ': ' + bar + ' ' + Math.round(percentage) + '%';
};

ProgressComponent.prototype.setValue = function(value) {
  this.value = value;
};

// Enhanced Counter App with multiple features
function EnhancedCounterApp() {
  this.count = 0;
  this.progress = 0;
  this.tui = new SimpleTUI();
  
  // Components
  this.titleComponent = new TextComponent('🚀 Enhanced TUI + Goja Demo', 'cyan', true);
  this.counterBox = new BoxComponent('Counter Display', 'Value: 0', 40, 6, 'green');
  this.progressComponent = new ProgressComponent('Progress', 0, 50, 25, 'blue');
  this.helpBox = new BoxComponent('Commands', 
    '+ : Increment counter\n' +
    '- : Decrement counter\n' +
    'r : Reset counter\n' +
    'p : Increase progress\n' +
    'q : Quit application', 50, 8, 'yellow');
  
  this.statusComponent = new TextComponent('Ready', 'gray');
  
  // Add components to TUI
  this.tui.addComponent(this.titleComponent);
  this.tui.addComponent(new TextComponent('', 'white')); // spacer
  this.tui.addComponent(this.counterBox);
  this.tui.addComponent(new TextComponent('', 'white')); // spacer
  this.tui.addComponent(this.progressComponent);
  this.tui.addComponent(new TextComponent('', 'white')); // spacer
  this.tui.addComponent(this.helpBox);
  this.tui.addComponent(new TextComponent('', 'white')); // spacer
  this.tui.addComponent(this.statusComponent);
  
  var self = this;
  this.tui.onInput(function(input) {
    if (input === '+') {
      self.count++;
      self.counterBox.setContent('Value: ' + self.count + '\nLast action: Increment');
      self.statusComponent.setText('Counter incremented to ' + self.count);
      return null;
    } else if (input === '-') {
      self.count--;
      self.counterBox.setContent('Value: ' + self.count + '\nLast action: Decrement');
      self.statusComponent.setText('Counter decremented to ' + self.count);
      return null;
    } else if (input === 'r') {
      self.count = 0;
      self.progress = 0;
      self.counterBox.setContent('Value: 0\nLast action: Reset');
      self.progressComponent.setValue(0);
      self.statusComponent.setText('Counter and progress reset');
      return null;
    } else if (input === 'p') {
      self.progress = Math.min(50, self.progress + 5);
      self.progressComponent.setValue(self.progress);
      self.statusComponent.setText('Progress increased to ' + self.progress);
      return null;
    } else if (input === 'q') {
      return 'quit';
    } else {
      self.statusComponent.setText('Unknown command: ' + input);
      return null;
    }
  });
}

EnhancedCounterApp.prototype.render = function() {
  return this.tui.render();
};

EnhancedCounterApp.prototype.handleInput = function(input) {
  return this.tui.handleInput(input);
};

// Export for different environments
var SimpleTUILib = {
  SimpleTUI: SimpleTUI,
  TextComponent: TextComponent,
  BoxComponent: BoxComponent,
  ProgressComponent: ProgressComponent,
  CounterApp: EnhancedCounterApp, // Use enhanced version
  EnhancedCounterApp: EnhancedCounterApp
};

// For Node.js
if (typeof module !== 'undefined' && module.exports) {
  module.exports = SimpleTUILib;
}

// For browser/goja global
if (typeof window !== 'undefined') {
  window.SimpleTUILib = SimpleTUILib;
} else if (typeof global !== 'undefined') {
  global.SimpleTUILib = SimpleTUILib;
}

