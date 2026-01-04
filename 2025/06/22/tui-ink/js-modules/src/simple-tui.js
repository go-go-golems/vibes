// Simple TUI library compatible with goja
// This creates a basic text-based interface without Node.js dependencies

function SimpleTUI() {
  this.components = [];
  this.currentScreen = '';
  this.inputHandlers = [];
}

SimpleTUI.prototype.addComponent = function(component) {
  this.components.push(component);
};

SimpleTUI.prototype.render = function() {
  var output = '';
  for (var i = 0; i < this.components.length; i++) {
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
    this.inputHandlers[i](input);
  }
};

// Text component
function TextComponent(text, color) {
  this.text = text || '';
  this.color = color || 'white';
}

TextComponent.prototype.render = function() {
  // Simple ANSI color codes
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
  return colorCode + this.text + colors['reset'];
};

TextComponent.prototype.setText = function(text) {
  this.text = text;
};

// Box component
function BoxComponent(title, content, width, height) {
  this.title = title || '';
  this.content = content || '';
  this.width = width || 40;
  this.height = height || 10;
}

BoxComponent.prototype.render = function() {
  var output = '';
  var border = '+' + Array(this.width - 1).join('-') + '+';
  
  output += border + '\n';
  if (this.title) {
    var titleLine = '| ' + this.title + Array(this.width - this.title.length - 2).join(' ') + '|';
    output += titleLine + '\n';
    output += border + '\n';
  }
  
  var lines = this.content.split('\n');
  for (var i = 0; i < this.height - 3; i++) {
    var line = lines[i] || '';
    var paddedLine = '| ' + line + Array(this.width - line.length - 2).join(' ') + '|';
    output += paddedLine + '\n';
  }
  
  output += border;
  return output;
};

// Counter app example
function CounterApp() {
  this.count = 0;
  this.tui = new SimpleTUI();
  this.titleComponent = new TextComponent('🚀 Simple TUI + Goja Test', 'cyan');
  this.counterComponent = new TextComponent('Counter: 0', 'green');
  this.helpComponent = new TextComponent('Commands: + (increment), - (decrement), q (quit)', 'yellow');
  
  this.tui.addComponent(this.titleComponent);
  this.tui.addComponent(this.counterComponent);
  this.tui.addComponent(this.helpComponent);
  
  var self = this;
  this.tui.onInput(function(input) {
    if (input === '+') {
      self.count++;
      self.counterComponent.setText('Counter: ' + self.count);
    } else if (input === '-') {
      self.count--;
      self.counterComponent.setText('Counter: ' + self.count);
    } else if (input === 'q') {
      return 'quit';
    }
  });
}

CounterApp.prototype.render = function() {
  return this.tui.render();
};

CounterApp.prototype.handleInput = function(input) {
  return this.tui.handleInput(input);
};

// Export for different environments
var SimpleTUILib = {
  SimpleTUI: SimpleTUI,
  TextComponent: TextComponent,
  BoxComponent: BoxComponent,
  CounterApp: CounterApp
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

