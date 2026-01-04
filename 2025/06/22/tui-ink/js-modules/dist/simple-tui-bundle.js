/*
 * ATTENTION: The "eval" devtool has been used (maybe by default in mode: "development").
 * This devtool is neither made for production nor for readable output files.
 * It uses "eval()" calls to create a separate source file in the browser devtools.
 * If you are trying to read the output file, select a different devtool (https://webpack.js.org/configuration/devtool/)
 * or disable the default devtool with "devtool: false".
 * If you are looking for production-ready output files, see mode: "production" (https://webpack.js.org/configuration/mode/).
 */
var SimpleTUILib;
/******/ (function() { // webpackBootstrap
/******/ 	var __webpack_modules__ = ({

/***/ "./src/enhanced-tui.js":
/*!*****************************!*\
  !*** ./src/enhanced-tui.js ***!
  \*****************************/
/***/ (function(module, __unused_webpack_exports, __webpack_require__) {

eval("// Enhanced TUI library compatible with goja\n// This creates a more sophisticated text-based interface\n\nfunction SimpleTUI() {\n  this.components = [];\n  this.currentScreen = '';\n  this.inputHandlers = [];\n  this.focusedComponent = 0;\n}\nSimpleTUI.prototype.addComponent = function (component) {\n  this.components.push(component);\n};\nSimpleTUI.prototype.render = function () {\n  var output = '';\n  for (var i = 0; i < this.components.length; i++) {\n    var component = this.components[i];\n    if (component.focused && this.focusedComponent === i) {\n      component.focused = true;\n    } else {\n      component.focused = false;\n    }\n    output += this.components[i].render() + '\\n';\n  }\n  this.currentScreen = output;\n  return output;\n};\nSimpleTUI.prototype.onInput = function (handler) {\n  this.inputHandlers.push(handler);\n};\nSimpleTUI.prototype.handleInput = function (input) {\n  for (var i = 0; i < this.inputHandlers.length; i++) {\n    var result = this.inputHandlers[i](input);\n    if (result) return result;\n  }\n};\n\n// Text component\nfunction TextComponent(text, color, bold) {\n  this.text = text || '';\n  this.color = color || 'white';\n  this.bold = bold || false;\n  this.focused = false;\n}\nTextComponent.prototype.render = function () {\n  var colors = {\n    'red': '\\x1b[31m',\n    'green': '\\x1b[32m',\n    'yellow': '\\x1b[33m',\n    'blue': '\\x1b[34m',\n    'magenta': '\\x1b[35m',\n    'cyan': '\\x1b[36m',\n    'white': '\\x1b[37m',\n    'gray': '\\x1b[90m',\n    'reset': '\\x1b[0m',\n    'bold': '\\x1b[1m'\n  };\n  var output = '';\n  if (this.bold) output += colors['bold'];\n  output += colors[this.color] || colors['white'];\n  output += this.text;\n  output += colors['reset'];\n  return output;\n};\nTextComponent.prototype.setText = function (text) {\n  this.text = text;\n};\n\n// Box component with border\nfunction BoxComponent(title, content, width, height, color) {\n  this.title = title || '';\n  this.content = content || '';\n  this.width = width || 50;\n  this.height = height || 10;\n  this.color = color || 'white';\n  this.focused = false;\n}\nBoxComponent.prototype.render = function () {\n  var colors = {\n    'red': '\\x1b[31m',\n    'green': '\\x1b[32m',\n    'yellow': '\\x1b[33m',\n    'blue': '\\x1b[34m',\n    'magenta': '\\x1b[35m',\n    'cyan': '\\x1b[36m',\n    'white': '\\x1b[37m',\n    'reset': '\\x1b[0m'\n  };\n  var colorCode = colors[this.color] || colors['white'];\n  var output = '';\n\n  // Top border\n  var border = this.focused ? '═' : '─';\n  var corners = this.focused ? ['╔', '╗', '╚', '╝'] : ['┌', '┐', '└', '┘'];\n  var sides = this.focused ? '║' : '│';\n  output += colorCode + corners[0] + Array(this.width - 1).join(border) + corners[1] + colors['reset'] + '\\n';\n\n  // Title line\n  if (this.title) {\n    var titleText = ' ' + this.title + ' ';\n    var padding = Math.max(0, this.width - titleText.length - 2);\n    var leftPad = Math.floor(padding / 2);\n    var rightPad = padding - leftPad;\n    output += colorCode + sides + Array(leftPad + 1).join(' ') + titleText + Array(rightPad + 1).join(' ') + sides + colors['reset'] + '\\n';\n    output += colorCode + '├' + Array(this.width - 1).join(border) + '┤' + colors['reset'] + '\\n';\n  }\n\n  // Content lines\n  var lines = this.content.split('\\n');\n  var contentHeight = this.title ? this.height - 4 : this.height - 2;\n  for (var i = 0; i < contentHeight; i++) {\n    var line = lines[i] || '';\n    var truncatedLine = line.length > this.width - 4 ? line.substring(0, this.width - 4) : line;\n    var padding = this.width - truncatedLine.length - 3;\n    output += colorCode + sides + ' ' + truncatedLine + Array(padding + 1).join(' ') + sides + colors['reset'] + '\\n';\n  }\n\n  // Bottom border\n  output += colorCode + corners[2] + Array(this.width - 1).join(border) + corners[3] + colors['reset'];\n  return output;\n};\nBoxComponent.prototype.setContent = function (content) {\n  this.content = content;\n};\n\n// Progress bar component\nfunction ProgressComponent(label, value, max, width, color) {\n  this.label = label || 'Progress';\n  this.value = value || 0;\n  this.max = max || 100;\n  this.width = width || 30;\n  this.color = color || 'green';\n}\nProgressComponent.prototype.render = function () {\n  var colors = {\n    'red': '\\x1b[31m',\n    'green': '\\x1b[32m',\n    'yellow': '\\x1b[33m',\n    'blue': '\\x1b[34m',\n    'cyan': '\\x1b[36m',\n    'white': '\\x1b[37m',\n    'reset': '\\x1b[0m'\n  };\n  var percentage = Math.min(100, Math.max(0, this.value / this.max * 100));\n  var filled = Math.floor(percentage / 100 * this.width);\n  var empty = this.width - filled;\n  var colorCode = colors[this.color] || colors['green'];\n  var bar = '[' + colorCode + Array(filled + 1).join('█') + colors['reset'] + Array(empty + 1).join('░') + ']';\n  return this.label + ': ' + bar + ' ' + Math.round(percentage) + '%';\n};\nProgressComponent.prototype.setValue = function (value) {\n  this.value = value;\n};\n\n// Enhanced Counter App with multiple features\nfunction EnhancedCounterApp() {\n  this.count = 0;\n  this.progress = 0;\n  this.tui = new SimpleTUI();\n\n  // Components\n  this.titleComponent = new TextComponent('🚀 Enhanced TUI + Goja Demo', 'cyan', true);\n  this.counterBox = new BoxComponent('Counter Display', 'Value: 0', 40, 6, 'green');\n  this.progressComponent = new ProgressComponent('Progress', 0, 50, 25, 'blue');\n  this.helpBox = new BoxComponent('Commands', '+ : Increment counter\\n' + '- : Decrement counter\\n' + 'r : Reset counter\\n' + 'p : Increase progress\\n' + 'q : Quit application', 50, 8, 'yellow');\n  this.statusComponent = new TextComponent('Ready', 'gray');\n\n  // Add components to TUI\n  this.tui.addComponent(this.titleComponent);\n  this.tui.addComponent(new TextComponent('', 'white')); // spacer\n  this.tui.addComponent(this.counterBox);\n  this.tui.addComponent(new TextComponent('', 'white')); // spacer\n  this.tui.addComponent(this.progressComponent);\n  this.tui.addComponent(new TextComponent('', 'white')); // spacer\n  this.tui.addComponent(this.helpBox);\n  this.tui.addComponent(new TextComponent('', 'white')); // spacer\n  this.tui.addComponent(this.statusComponent);\n  var self = this;\n  this.tui.onInput(function (input) {\n    if (input === '+') {\n      self.count++;\n      self.counterBox.setContent('Value: ' + self.count + '\\nLast action: Increment');\n      self.statusComponent.setText('Counter incremented to ' + self.count);\n      return null;\n    } else if (input === '-') {\n      self.count--;\n      self.counterBox.setContent('Value: ' + self.count + '\\nLast action: Decrement');\n      self.statusComponent.setText('Counter decremented to ' + self.count);\n      return null;\n    } else if (input === 'r') {\n      self.count = 0;\n      self.progress = 0;\n      self.counterBox.setContent('Value: 0\\nLast action: Reset');\n      self.progressComponent.setValue(0);\n      self.statusComponent.setText('Counter and progress reset');\n      return null;\n    } else if (input === 'p') {\n      self.progress = Math.min(50, self.progress + 5);\n      self.progressComponent.setValue(self.progress);\n      self.statusComponent.setText('Progress increased to ' + self.progress);\n      return null;\n    } else if (input === 'q') {\n      return 'quit';\n    } else {\n      self.statusComponent.setText('Unknown command: ' + input);\n      return null;\n    }\n  });\n}\nEnhancedCounterApp.prototype.render = function () {\n  return this.tui.render();\n};\nEnhancedCounterApp.prototype.handleInput = function (input) {\n  return this.tui.handleInput(input);\n};\n\n// Export for different environments\nvar SimpleTUILib = {\n  SimpleTUI: SimpleTUI,\n  TextComponent: TextComponent,\n  BoxComponent: BoxComponent,\n  ProgressComponent: ProgressComponent,\n  CounterApp: EnhancedCounterApp,\n  // Use enhanced version\n  EnhancedCounterApp: EnhancedCounterApp\n};\n\n// For Node.js\nif ( true && module.exports) {\n  module.exports = SimpleTUILib;\n}\n\n// For browser/goja global\nif (typeof window !== 'undefined') {\n  window.SimpleTUILib = SimpleTUILib;\n} else if (typeof __webpack_require__.g !== 'undefined') {\n  __webpack_require__.g.SimpleTUILib = SimpleTUILib;\n}\n\n//# sourceURL=webpack://SimpleTUILib/./src/enhanced-tui.js?");

/***/ })

/******/ 	});
/************************************************************************/
/******/ 	// The module cache
/******/ 	var __webpack_module_cache__ = {};
/******/ 	
/******/ 	// The require function
/******/ 	function __webpack_require__(moduleId) {
/******/ 		// Check if module is in cache
/******/ 		var cachedModule = __webpack_module_cache__[moduleId];
/******/ 		if (cachedModule !== undefined) {
/******/ 			return cachedModule.exports;
/******/ 		}
/******/ 		// Create a new module (and put it into the cache)
/******/ 		var module = __webpack_module_cache__[moduleId] = {
/******/ 			// no module.id needed
/******/ 			// no module.loaded needed
/******/ 			exports: {}
/******/ 		};
/******/ 	
/******/ 		// Execute the module function
/******/ 		__webpack_modules__[moduleId](module, module.exports, __webpack_require__);
/******/ 	
/******/ 		// Return the exports of the module
/******/ 		return module.exports;
/******/ 	}
/******/ 	
/************************************************************************/
/******/ 	/* webpack/runtime/global */
/******/ 	!function() {
/******/ 		__webpack_require__.g = (function() {
/******/ 			if (typeof globalThis === 'object') return globalThis;
/******/ 			try {
/******/ 				return this || new Function('return this')();
/******/ 			} catch (e) {
/******/ 				if (typeof window === 'object') return window;
/******/ 			}
/******/ 		})();
/******/ 	}();
/******/ 	
/************************************************************************/
/******/ 	
/******/ 	// startup
/******/ 	// Load entry module and return exports
/******/ 	// This entry module is referenced by other modules so it can't be inlined
/******/ 	var __webpack_exports__ = __webpack_require__("./src/enhanced-tui.js");
/******/ 	SimpleTUILib = __webpack_exports__;
/******/ 	
/******/ })()
;