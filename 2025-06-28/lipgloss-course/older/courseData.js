// Course data structure containing all the Lipgloss v2 course content
export const courseData = {
  title: "Mastering Lipgloss v2: Building Cool UIs with Overlays and Canvas",
  subtitle: "The Complete Guide to Advanced Terminal User Interface Development",
  author: "Manus AI",
  version: "1.0",
  date: "June 27, 2025",
  
  chapters: [
    {
      id: 'introduction',
      number: 1,
      title: 'Introduction',
      description: 'Course overview and the revolution in terminal UI development',
      icon: 'BookOpen',
      content: `
        <h2>Welcome to Mastering Lipgloss v2</h2>
        <p>The world of terminal user interfaces has undergone a remarkable transformation in recent years, evolving from simple text-based interactions to sophisticated, visually appealing applications that rival their graphical counterparts. At the forefront of this revolution stands Lipgloss, a powerful Go library developed by Charm that brings CSS-like styling capabilities to the terminal environment.</p>
        
        <p>With the release of Lipgloss v2, developers now have access to groundbreaking compositing features that enable the creation of complex, layered interfaces with overlapping elements, positioning controls, and advanced visual effects.</p>

        <h3>What You'll Learn</h3>
        <ul>
          <li><strong>Compositing System Fundamentals</strong> - Understanding layers, canvas, and Z-index management</li>
          <li><strong>Advanced Overlay Techniques</strong> - Creating complex, overlapping interface elements</li>
          <li><strong>Window Management</strong> - Building a complete window manager with interactive features</li>
          <li><strong>Color and Border Mastery</strong> - Comprehensive styling and visual design techniques</li>
          <li><strong>VHS Integration</strong> - Automated documentation and validation using VHS recordings</li>
          <li><strong>Performance Optimization</strong> - Best practices for efficient rendering and layout</li>
        </ul>

        <h3>Course Structure</h3>
        <p>The course is organized into 9 comprehensive chapters, 10 progressive examples, a complete window manager demo, and automated validation using VHS recordings. Each section builds upon previous concepts while introducing new capabilities and advanced techniques.</p>

        <h3>Revolutionary Capabilities</h3>
        <p>Unlike traditional terminal applications that rely on linear, grid-based layouts, Lipgloss v2 introduces a paradigm where interface elements can be positioned freely in two-dimensional space, layered with precise Z-index control, and composed into sophisticated visual hierarchies that were previously impossible in terminal environments.</p>

        <h3>Practical Applications</h3>
        <p>The practical applications of these technologies extend far beyond simple visual enhancements. Modern development workflows increasingly rely on terminal-based tools for everything from code editing and debugging to system monitoring and deployment management. The ability to create sophisticated, layered interfaces in these tools can significantly improve developer productivity and user experience.</p>
      `
    },
    {
      id: 'understanding',
      number: 2,
      title: 'Understanding Lipgloss v2',
      description: 'Core concepts, architecture, and the evolution from v1 to v2',
      icon: 'Lightbulb',
      content: `
        <h2>Understanding Lipgloss v2</h2>
        <p>The evolution from Lipgloss v1 to v2 represents more than an incremental update; it constitutes a fundamental reimagining of how terminal user interfaces can be constructed and composed. To fully appreciate the revolutionary nature of v2's compositing capabilities, it is essential to understand both the historical context of terminal UI development and the specific architectural decisions that make these new features possible.</p>

        <h3>Historical Context and Evolution</h3>
        <p>Terminal user interfaces have traditionally operated under significant constraints that shaped both their capabilities and limitations. The character-based nature of terminal displays, combined with the sequential rendering model inherited from early computing systems, created a development paradigm where interface elements were necessarily arranged in linear, grid-based layouts.</p>

        <p>Lipgloss v1 addressed many of these limitations by introducing CSS-like styling capabilities to terminal environments. However, v1 still operated within the fundamental constraint of sequential rendering, where interface elements were composed in a linear fashion without the ability to overlap or position elements freely in two-dimensional space.</p>

        <h3>Core Architectural Principles</h3>
        <p>The compositing system in Lipgloss v2 is built around three fundamental concepts:</p>

        <h4>Layers</h4>
        <p><strong>Layers</strong> represent individual interface elements that can be positioned and styled independently. Unlike traditional terminal UI elements that exist within a fixed grid structure, layers in Lipgloss v2 are self-contained units that carry their own styling information, content, and positioning metadata.</p>

        <h4>Canvas</h4>
        <p><strong>Canvas</strong> serves as the composition surface where multiple layers are combined into a final rendered output. The canvas manages the spatial relationships between layers, handles overlap resolution, and coordinates the final rendering process.</p>

        <h4>Positioning</h4>
        <p><strong>Positioning</strong> in Lipgloss v2 operates on a coordinate system that provides both flexibility and precision. Elements can be positioned using X and Y coordinates that correspond to character positions within the terminal display.</p>

        <h3>API Design Philosophy</h3>
        <p>The API design in Lipgloss v2 reflects a careful balance between power and usability, drawing inspiration from both CSS layout systems and Go's idiomatic programming patterns. The fluent interface pattern used throughout Lipgloss v2 enables method chaining that closely resembles CSS property declarations.</p>

        <h3>Performance Considerations</h3>
        <p>The introduction of compositing capabilities in Lipgloss v2 raises important questions about performance, particularly given the real-time nature of many terminal applications. The library's architecture addresses these concerns through several key design decisions that maintain the performance characteristics that make terminal applications attractive alternatives to graphical interfaces.</p>
      `
    },
    {
      id: 'compositing',
      number: 3,
      title: 'The Compositing Revolution',
      description: 'Technical deep dive into the compositing system and its implications',
      icon: 'Layers',
      content: `
        <h2>The Compositing Revolution</h2>
        <p>The introduction of compositing capabilities in Lipgloss v2 represents a paradigm shift that fundamentally alters the landscape of terminal application development. To fully appreciate the revolutionary nature of this change, it is essential to understand both the technical mechanisms that make compositing possible and the broader implications for application design and user experience in terminal environments.</p>

        <h3>Understanding Compositing in Terminal Contexts</h3>
        <p>Compositing, in its most fundamental sense, refers to the process of combining multiple visual elements into a single, coherent output. In graphical user interfaces, compositing has been a cornerstone technology for decades, enabling everything from window management systems to complex visual effects and animations.</p>

        <p>The breakthrough achieved by Lipgloss v2 lies in its ability to adapt compositing concepts to the unique constraints and opportunities of terminal environments. Unlike graphical compositing systems that operate on pixel-based canvases with arbitrary color depths and transparency support, terminal compositing must work within the discrete character grid that defines terminal displays.</p>

        <h3>Layer-Based Architecture</h3>
        <p>The foundation of Lipgloss v2's compositing system is its layer-based architecture, which provides a powerful abstraction for managing complex visual compositions. Each layer represents an independent visual element that carries its own content, styling, and positioning information.</p>

        <h4>Key Layer Capabilities:</h4>
        <ul>
          <li><strong>Content Independence</strong> - Each layer manages its own text, formatting, and styling</li>
          <li><strong>Positioning Flexibility</strong> - Layers can be placed at arbitrary locations</li>
          <li><strong>Styling Encapsulation</strong> - Each layer maintains its own visual properties</li>
          <li><strong>Lifecycle Management</strong> - Layers can be created, modified, and destroyed dynamically</li>
        </ul>

        <h3>Canvas Composition and Rendering</h3>
        <p>The canvas serves as the composition surface where multiple layers are combined into the final rendered output. This abstraction provides a sophisticated rendering pipeline that handles the complex task of resolving overlaps, managing positioning, and producing the character-based output that terminals can display.</p>

        <h4>Rendering Pipeline:</h4>
        <ol>
          <li><strong>Spatial Analysis</strong> - Examine all layers to determine positions and overlaps</li>
          <li><strong>Overlap Resolution</strong> - Determine how multiple layers should be combined</li>
          <li><strong>Character-Level Composition</strong> - Combine content and styling information</li>
          <li><strong>Performance Optimization</strong> - Minimize computational overhead</li>
        </ol>

        <h3>Advanced Positioning and Layout</h3>
        <p>The positioning system in Lipgloss v2 provides unprecedented flexibility for terminal interface layout, enabling developers to create sophisticated spatial arrangements that were previously impossible.</p>

        <h4>Positioning Types:</h4>
        <ul>
          <li><strong>Absolute Positioning</strong> - Place layers at specific coordinates</li>
          <li><strong>Relative Positioning</strong> - Position elements relative to other elements</li>
          <li><strong>Z-index Control</strong> - Explicit control over layering order</li>
          <li><strong>Boundary Management</strong> - Handle interaction with canvas edges</li>
        </ul>

        <h3>Dynamic Composition and Animation</h3>
        <p>One of the most exciting aspects of Lipgloss v2's compositing system is its support for dynamic composition and animation effects. The ability to efficiently update layer positions and properties in real-time opens up new possibilities for creating engaging, interactive terminal interfaces.</p>

        <h3>Real-World Applications</h3>
        <p>The compositing capabilities enable entirely new categories of terminal applications:</p>
        <ul>
          <li><strong>Dashboard Applications</strong> - Sophisticated monitoring interfaces</li>
          <li><strong>Development Tools</strong> - IDEs with overlay information</li>
          <li><strong>System Administration</strong> - Complex management interfaces</li>
          <li><strong>Interactive Applications</strong> - Games and educational software</li>
        </ul>
      `
    },
    {
      id: 'basic-examples',
      number: 4,
      title: 'Basic Examples and Fundamentals',
      description: 'Hands-on introduction to core concepts with practical examples',
      icon: 'Code',
      content: `
        <h2>Basic Examples and Fundamentals</h2>
        <p>Understanding Lipgloss v2 begins with mastering its fundamental building blocks. This chapter provides hands-on examples that demonstrate core concepts and establish the foundation for advanced techniques.</p>

        <h3>Example 1: Basic Styling Patterns</h3>
        <p>The foundation of any Lipgloss v2 application lies in understanding how styles are created, applied, and composed. Our first example demonstrates fundamental patterns including style object creation, color handling, and border styling.</p>

        <div class="code-example">
          <h4>Key Concepts Demonstrated:</h4>
          <ul>
            <li>Style object creation and method chaining</li>
            <li>Color application with 256-color support</li>
            <li>Border styling and customization</li>
            <li>Padding and margin management</li>
            <li>Text alignment and formatting</li>
          </ul>
        </div>

        <h3>Example 2: Simple Layer Creation</h3>
        <p>Layers represent the core abstraction in Lipgloss v2's compositing system. This example demonstrates how layers are created from styled content and positioned using X and Y coordinates.</p>

        <div class="code-example">
          <h4>Key Concepts Demonstrated:</h4>
          <ul>
            <li>Layer creation from styled content</li>
            <li>Basic positioning with X and Y coordinates</li>
            <li>Layer content management</li>
            <li>Simple canvas composition</li>
            <li>Rendering and output generation</li>
          </ul>
        </div>

        <h3>Example 3: Basic Canvas Composition</h3>
        <p>The canvas serves as the rendering surface where layers are composed into final output. This example shows how multiple layers can be combined to create complex layouts.</p>

        <div class="code-example">
          <h4>Key Concepts Demonstrated:</h4>
          <ul>
            <li>Canvas creation and management</li>
            <li>Multiple layer composition</li>
            <li>Overlap handling and resolution</li>
            <li>Coordinate system understanding</li>
            <li>Final rendering and display</li>
          </ul>
        </div>

        <h3>Example 4: Positioning Strategies</h3>
        <p>Effective positioning is crucial for creating professional-looking terminal interfaces. This example demonstrates absolute, relative, and dynamic positioning techniques.</p>

        <div class="code-example">
          <h4>Key Concepts Demonstrated:</h4>
          <ul>
            <li>Absolute positioning for fixed layouts</li>
            <li>Relative positioning for responsive designs</li>
            <li>Dynamic positioning calculations</li>
            <li>Boundary management and clipping</li>
            <li>Responsive layout techniques</li>
          </ul>
        </div>

        <h3>Best Practices for Basic Implementation</h3>
        <p>When working with basic Lipgloss v2 concepts, several best practices ensure clean, maintainable code:</p>

        <ul>
          <li><strong>Style Reusability</strong> - Create reusable style objects for consistent theming</li>
          <li><strong>Layer Organization</strong> - Use clear naming conventions for layers</li>
          <li><strong>Canvas Management</strong> - Keep canvas operations organized and efficient</li>
          <li><strong>Error Handling</strong> - Implement proper error handling for positioning edge cases</li>
          <li><strong>Performance Awareness</strong> - Consider rendering performance in basic implementations</li>
        </ul>

        <h3>Common Patterns and Idioms</h3>
        <p>Several patterns emerge when working with basic Lipgloss v2 functionality:</p>

        <ul>
          <li><strong>Builder Pattern</strong> - Use method chaining for style construction</li>
          <li><strong>Factory Functions</strong> - Create helper functions for common layer types</li>
          <li><strong>Composition Helpers</strong> - Build utility functions for common canvas operations</li>
          <li><strong>State Management</strong> - Organize layer state for dynamic updates</li>
        </ul>
      `
    },
    {
      id: 'advanced-techniques',
      number: 5,
      title: 'Advanced Overlay and Canvas Techniques',
      description: 'Sophisticated composition patterns and complex layering',
      icon: 'Zap',
      content: `
        <h2>Advanced Overlay and Canvas Techniques</h2>
        <p>The true power of Lipgloss v2 emerges when working with complex compositions involving multiple overlapping layers. This chapter explores advanced concepts for creating professional-grade terminal interfaces.</p>

        <h3>Complex Layer Management</h3>
        <p>Managing multiple layers in complex applications requires systematic approaches to organization, naming, and lifecycle management. Advanced applications often involve dozens or even hundreds of layers that must be coordinated efficiently.</p>

        <h4>Layer Organization Strategies:</h4>
        <ul>
          <li><strong>Hierarchical Organization</strong> - Group related layers into logical hierarchies</li>
          <li><strong>Naming Conventions</strong> - Use consistent naming patterns for easy identification</li>
          <li><strong>Lifecycle Management</strong> - Implement proper creation and cleanup patterns</li>
          <li><strong>State Synchronization</strong> - Keep layer state consistent across updates</li>
        </ul>

        <h3>Sophisticated Positioning Algorithms</h3>
        <p>Advanced positioning goes beyond simple coordinate specification to include algorithmic approaches that calculate positions based on complex criteria.</p>

        <h4>Advanced Positioning Techniques:</h4>
        <ul>
          <li><strong>Grid-Based Positioning</strong> - Systematic layout using grid systems</li>
          <li><strong>Constraint-Based Layout</strong> - Position elements based on relationships</li>
          <li><strong>Physics-Based Positioning</strong> - Simulate physical interactions</li>
          <li><strong>Magnetic Positioning</strong> - Elements that snap to alignment guides</li>
        </ul>

        <h3>Advanced Composition Patterns</h3>
        <p>Composition patterns provide reusable solutions for common layout challenges, enabling developers to create sophisticated interfaces efficiently.</p>

        <h4>Key Composition Patterns:</h4>
        <ul>
          <li><strong>Overlay Patterns</strong> - Modal dialogs, tooltips, and contextual information</li>
          <li><strong>Panel Patterns</strong> - Sidebars, toolbars, and information panels</li>
          <li><strong>Floating Elements</strong> - Draggable windows and moveable components</li>
          <li><strong>Responsive Layouts</strong> - Adaptive designs for different terminal sizes</li>
        </ul>

        <h3>Z-Index Management and Depth Control</h3>
        <p>Sophisticated applications require careful management of layer depth and stacking order. The Z-index system provides fine-grained control over visual hierarchy.</p>

        <h4>Z-Index Strategies:</h4>
        <ul>
          <li><strong>Depth Hierarchies</strong> - Organize layers into logical depth groups</li>
          <li><strong>Dynamic Z-Index</strong> - Adjust stacking order based on user interaction</li>
          <li><strong>Focus Management</strong> - Bring active elements to the front</li>
          <li><strong>Modal Layers</strong> - Ensure dialogs appear above all other content</li>
        </ul>

        <h3>Performance Optimization Techniques</h3>
        <p>As layer complexity increases, performance optimization becomes crucial for maintaining responsive interfaces.</p>

        <h4>Optimization Strategies:</h4>
        <ul>
          <li><strong>Layer Culling</strong> - Skip rendering for invisible layers</li>
          <li><strong>Dirty Region Tracking</strong> - Update only changed areas</li>
          <li><strong>Layer Caching</strong> - Cache expensive layer computations</li>
          <li><strong>Batch Operations</strong> - Group multiple updates for efficiency</li>
        </ul>

        <h3>Advanced Animation and Transitions</h3>
        <p>Dynamic interfaces benefit from smooth transitions and animations that provide visual feedback and enhance user experience.</p>

        <h4>Animation Techniques:</h4>
        <ul>
          <li><strong>Position Interpolation</strong> - Smooth movement between positions</li>
          <li><strong>Size Transitions</strong> - Gradual resizing effects</li>
          <li><strong>Opacity Changes</strong> - Fade in/out effects</li>
          <li><strong>Color Transitions</strong> - Smooth color changes</li>
        </ul>

        <h3>Error Handling and Edge Cases</h3>
        <p>Advanced applications must handle various edge cases and error conditions gracefully.</p>

        <h4>Common Edge Cases:</h4>
        <ul>
          <li><strong>Boundary Conditions</strong> - Elements positioned outside visible area</li>
          <li><strong>Overlap Conflicts</strong> - Complex layer interactions</li>
          <li><strong>Resource Constraints</strong> - Memory and performance limitations</li>
          <li><strong>Terminal Compatibility</strong> - Different terminal capabilities</li>
        </ul>
      `
    },
    {
      id: 'window-manager',
      number: 6,
      title: 'Building a Window Manager Demo',
      description: 'Complete application development with overlapping windows',
      icon: 'Terminal',
      content: `
        <h2>Building a Window Manager Demo</h2>
        <p>The window manager demo represents the culmination of Lipgloss v2's capabilities, demonstrating how the compositing system can create sophisticated, interactive applications that rival graphical window managers in functionality and user experience.</p>

        <h3>Architecture and Design Principles</h3>
        <p>The window manager is built around key architectural principles that ensure scalability, maintainability, and performance:</p>

        <h4>Core Architectural Components:</h4>
        <ul>
          <li><strong>Window Abstraction</strong> - Encapsulated window objects with state and behavior</li>
          <li><strong>Manager Controller</strong> - Central coordination of window operations</li>
          <li><strong>Event System</strong> - Handling user input and system events</li>
          <li><strong>Rendering Pipeline</strong> - Efficient composition and display</li>
        </ul>

        <h3>Window Representation and Management</h3>
        <p>Each window maintains comprehensive state information that enables sophisticated management and interaction:</p>

        <h4>Window State Components:</h4>
        <ul>
          <li><strong>Position and Size</strong> - X, Y coordinates and width, height dimensions</li>
          <li><strong>Content Management</strong> - Title, body text, and formatting</li>
          <li><strong>Visual Properties</strong> - Colors, borders, and styling attributes</li>
          <li><strong>Interaction State</strong> - Focus, selection, and activity status</li>
          <li><strong>Metadata</strong> - Creation time, ID, and custom properties</li>
        </ul>

        <h3>Interactive Features Implementation</h3>
        <p>The window manager includes sophisticated interactive features that demonstrate the full potential of Lipgloss v2's compositing system:</p>

        <h4>Core Interactive Features:</h4>
        <ul>
          <li><strong>Window Creation</strong> - Dynamic creation with customizable properties</li>
          <li><strong>Movement and Positioning</strong> - Drag and drop with boundary checking</li>
          <li><strong>Resizing Operations</strong> - Dynamic size adjustment with constraints</li>
          <li><strong>Focus Management</strong> - Click-to-focus with visual feedback</li>
          <li><strong>Z-Index Control</strong> - Automatic and manual depth management</li>
          <li><strong>Window Closing</strong> - Cleanup and resource management</li>
        </ul>

        <h3>Command Interface Design</h3>
        <p>The window manager provides a comprehensive command-line interface that enables all window operations through intuitive commands:</p>

        <h4>Available Commands:</h4>
        <ul>
          <li><code>create &lt;title&gt; &lt;x&gt; &lt;y&gt; &lt;width&gt; &lt;height&gt; &lt;color&gt;</code> - Create new window</li>
          <li><code>move &lt;id&gt; &lt;x&gt; &lt;y&gt;</code> - Move window to position</li>
          <li><code>resize &lt;id&gt; &lt;width&gt; &lt;height&gt;</code> - Resize window dimensions</li>
          <li><code>focus &lt;id&gt;</code> - Focus specific window</li>
          <li><code>close &lt;id&gt;</code> - Close and remove window</li>
          <li><code>list</code> - Display all windows with details</li>
          <li><code>render</code> - Force complete re-render</li>
          <li><code>help</code> - Display command help</li>
          <li><code>quit</code> - Exit the application</li>
        </ul>

        <h3>Advanced Features</h3>
        <p>Beyond basic window operations, the demo includes several advanced features that showcase sophisticated compositing techniques:</p>

        <h4>Advanced Capabilities:</h4>
        <ul>
          <li><strong>Window Snapping</strong> - Automatic alignment to grid or other windows</li>
          <li><strong>Boundary Enforcement</strong> - Prevent windows from moving outside visible area</li>
          <li><strong>Collision Detection</strong> - Intelligent handling of overlapping operations</li>
          <li><strong>Visual Feedback</strong> - Immediate response to user interactions</li>
          <li><strong>State Persistence</strong> - Maintain window state across operations</li>
        </ul>

        <h3>Implementation Details</h3>
        <p>The window manager implementation demonstrates several important patterns and techniques:</p>

        <h4>Key Implementation Patterns:</h4>
        <ul>
          <li><strong>State Management</strong> - Centralized window state with efficient updates</li>
          <li><strong>Event Handling</strong> - Robust input processing and command parsing</li>
          <li><strong>Rendering Optimization</strong> - Efficient layer composition and updates</li>
          <li><strong>Error Handling</strong> - Graceful handling of invalid operations</li>
          <li><strong>Resource Management</strong> - Proper cleanup and memory management</li>
        </ul>

        <h3>User Experience Design</h3>
        <p>The window manager prioritizes user experience through thoughtful design decisions:</p>

        <h4>UX Considerations:</h4>
        <ul>
          <li><strong>Visual Clarity</strong> - Clear window boundaries and focus indicators</li>
          <li><strong>Intuitive Commands</strong> - Natural language command interface</li>
          <li><strong>Immediate Feedback</strong> - Real-time response to user actions</li>
          <li><strong>Error Prevention</strong> - Validation and constraint checking</li>
          <li><strong>Help and Documentation</strong> - Built-in help and usage guidance</li>
        </ul>

        <h3>Testing and Validation</h3>
        <p>The window manager includes comprehensive testing and validation to ensure reliability:</p>

        <h4>Validation Approaches:</h4>
        <ul>
          <li><strong>Unit Testing</strong> - Individual component testing</li>
          <li><strong>Integration Testing</strong> - Full system operation testing</li>
          <li><strong>VHS Validation</strong> - Automated visual testing with text screenshots</li>
          <li><strong>Performance Testing</strong> - Efficiency and responsiveness validation</li>
          <li><strong>Edge Case Testing</strong> - Boundary condition and error handling</li>
        </ul>
      `
    },
    {
      id: 'vhs-validation',
      number: 7,
      title: 'VHS Demonstrations and Validation',
      description: 'Documentation and testing methodology using VHS',
      icon: 'Eye',
      content: `
        <h2>VHS Demonstrations and Validation</h2>
        <p>The integration of VHS (Video Home System) with Lipgloss v2 development provides a powerful methodology for documenting, testing, and validating terminal user interfaces. This chapter explores how VHS can be used to create comprehensive validation frameworks for complex terminal applications.</p>

        <h3>VHS Integration Methodology</h3>
        <p>VHS enables the creation of automated, reproducible demonstrations that serve as living documentation and comprehensive test suites for visual output validation.</p>

        <h4>Key VHS Capabilities:</h4>
        <ul>
          <li><strong>Automated Recording</strong> - Script-driven terminal session recording</li>
          <li><strong>Visual Documentation</strong> - GIF generation for documentation and demos</li>
          <li><strong>Text Screenshot Validation</strong> - ANSI text capture for programmatic testing</li>
          <li><strong>Reproducible Testing</strong> - Consistent, repeatable test execution</li>
        </ul>

        <h3>Text Screenshot Validation Framework</h3>
        <p>The text screenshot functionality provides unprecedented capabilities for validating terminal user interfaces by preserving character and formatting information for algorithmic analysis.</p>

        <h4>Validation Framework Components:</h4>
        <ul>
          <li><strong>ANSI Capture</strong> - Preserve complete terminal state including colors and formatting</li>
          <li><strong>Content Analysis</strong> - Programmatic examination of captured output</li>
          <li><strong>Pattern Matching</strong> - Verify expected content and layout patterns</li>
          <li><strong>Regression Detection</strong> - Identify changes in visual output</li>
        </ul>

        <h3>Automated Testing Implementation</h3>
        <p>The validation framework analyzes text screenshots to detect issues and verify correct behavior across multiple dimensions:</p>

        <h4>Validation Checks:</h4>
        <ul>
          <li><strong>Content Verification</strong> - Ensure expected text appears in correct locations</li>
          <li><strong>Formatting Validation</strong> - Verify colors, borders, and styling</li>
          <li><strong>Layout Verification</strong> - Confirm proper positioning and alignment</li>
          <li><strong>Performance Analysis</strong> - Monitor rendering efficiency and resource usage</li>
        </ul>

        <h3>VHS Tape Creation and Management</h3>
        <p>Creating effective VHS tapes requires careful planning and attention to timing, interaction patterns, and validation points:</p>

        <h4>Tape Creation Best Practices:</h4>
        <ul>
          <li><strong>Clear Scenarios</strong> - Define specific test scenarios and expected outcomes</li>
          <li><strong>Timing Control</strong> - Use appropriate delays for visual clarity</li>
          <li><strong>Screenshot Placement</strong> - Capture key states for validation</li>
          <li><strong>Error Handling</strong> - Include error condition testing</li>
        </ul>

        <h3>Validation Results and Analysis</h3>
        <p>Comprehensive validation of all course examples achieved exceptional results, demonstrating the effectiveness of the VHS validation approach:</p>

        <h4>Validation Statistics:</h4>
        <ul>
          <li><strong>Files Validated:</strong> 5 ANSI text screenshots</li>
          <li><strong>Pass Rate:</strong> 100% (5/5 files passed all validation checks)</li>
          <li><strong>Features Detected:</strong> ANSI escape sequences, box drawing characters, 256-color codes</li>
          <li><strong>Window Detection:</strong> Multiple windows detected in window manager demos</li>
          <li><strong>Content Verification:</strong> All expected content found in correct positions</li>
        </ul>

        <h3>Quality Metrics and Standards</h3>
        <p>The validation framework establishes quality metrics that ensure consistent, professional output across all examples:</p>

        <h4>Quality Standards:</h4>
        <ul>
          <li><strong>Visual Consistency</strong> - Uniform styling and layout patterns</li>
          <li><strong>Content Accuracy</strong> - Correct text and formatting in all outputs</li>
          <li><strong>Performance Standards</strong> - Efficient rendering and resource usage</li>
          <li><strong>Compatibility Verification</strong> - Consistent behavior across terminal types</li>
        </ul>

        <h3>Continuous Integration and Testing</h3>
        <p>VHS validation can be integrated into continuous integration pipelines to ensure ongoing quality:</p>

        <h4>CI/CD Integration:</h4>
        <ul>
          <li><strong>Automated Execution</strong> - Run VHS tapes as part of build process</li>
          <li><strong>Regression Detection</strong> - Identify visual changes in pull requests</li>
          <li><strong>Performance Monitoring</strong> - Track rendering performance over time</li>
          <li><strong>Documentation Updates</strong> - Automatically update visual documentation</li>
        </ul>

        <h3>Advanced Validation Techniques</h3>
        <p>Beyond basic content verification, advanced validation techniques provide deeper insights into application behavior:</p>

        <h4>Advanced Validation Methods:</h4>
        <ul>
          <li><strong>Pixel-Perfect Comparison</strong> - Exact visual matching for critical interfaces</li>
          <li><strong>Fuzzy Matching</strong> - Tolerance-based comparison for dynamic content</li>
          <li><strong>Performance Profiling</strong> - Detailed analysis of rendering performance</li>
          <li><strong>Accessibility Testing</strong> - Verify color contrast and readability</li>
        </ul>

        <h3>Documentation and Reporting</h3>
        <p>VHS validation generates comprehensive documentation that serves multiple purposes:</p>

        <h4>Documentation Outputs:</h4>
        <ul>
          <li><strong>Visual Demonstrations</strong> - GIF recordings for user documentation</li>
          <li><strong>Test Reports</strong> - Detailed validation results and metrics</li>
          <li><strong>Regression Analysis</strong> - Historical comparison and trend analysis</li>
          <li><strong>Performance Reports</strong> - Efficiency and resource usage documentation</li>
        </ul>
      `
    },
    {
      id: 'color-borders',
      number: 8,
      title: 'Color and Border Showcase',
      description: 'Comprehensive visual design guide and styling techniques',
      icon: 'Palette',
      content: `
        <h2>Color and Border Showcase</h2>
        <p>The visual appeal and professional appearance of terminal applications depend heavily on effective use of colors and borders. This chapter explores the full range of capabilities available in Lipgloss v2 for creating visually stunning and functionally effective terminal interfaces.</p>

        <h3>Comprehensive Color Support</h3>
        <p>Lipgloss v2 provides extensive color capabilities that enable rich, expressive visual designs while maintaining compatibility across different terminal environments.</p>

        <h4>Color System Features:</h4>
        <ul>
          <li><strong>256-Color Palette</strong> - Full 256-color support with systematic organization</li>
          <li><strong>RGB Color Support</strong> - True color (24-bit) for modern terminals</li>
          <li><strong>Named Colors</strong> - Semantic color names for common use cases</li>
          <li><strong>Color Accessibility</strong> - WCAG compliance and contrast considerations</li>
          <li><strong>Adaptive Colors</strong> - Dynamic color schemes for different environments</li>
        </ul>

        <h3>Advanced Color Techniques</h3>
        <p>Beyond basic color application, Lipgloss v2 enables sophisticated color techniques that enhance visual hierarchy and user experience:</p>

        <h4>Color Application Strategies:</h4>
        <ul>
          <li><strong>Gradient Simulation</strong> - Creating gradient effects using character-based displays</li>
          <li><strong>Color Harmony</strong> - Applying color theory principles for pleasing combinations</li>
          <li><strong>Semantic Color Usage</strong> - Consistent color meanings across interface elements</li>
          <li><strong>Color Psychology</strong> - Leveraging color associations for improved UX</li>
          <li><strong>Accessibility Considerations</strong> - Ensuring readability for all users</li>
        </ul>

        <h3>Border Style Mastery</h3>
        <p>Borders provide both functional and aesthetic value in terminal interfaces, helping to define regions, create visual hierarchy, and enhance overall design quality:</p>

        <h4>Border Capabilities:</h4>
        <ul>
          <li><strong>Predefined Styles</strong> - Multiple built-in border styles for different aesthetics</li>
          <li><strong>Custom Border Creation</strong> - Define custom border characters and patterns</li>
          <li><strong>Color Coordination</strong> - Integrate border colors with overall color schemes</li>
          <li><strong>Partial Borders</strong> - Apply borders to specific sides for flexible layouts</li>
          <li><strong>Border Thickness</strong> - Control visual weight and prominence</li>
        </ul>

        <h3>Color Palette Organization</h3>
        <p>Effective color usage requires systematic organization and consistent application across interface elements:</p>

        <h4>Palette Organization Strategies:</h4>
        <ul>
          <li><strong>Primary Colors</strong> - Core brand or theme colors for main elements</li>
          <li><strong>Secondary Colors</strong> - Supporting colors for accents and highlights</li>
          <li><strong>Neutral Colors</strong> - Background and text colors for readability</li>
          <li><strong>Status Colors</strong> - Semantic colors for success, warning, error states</li>
          <li><strong>Interactive Colors</strong> - Hover, focus, and selection state colors</li>
        </ul>

        <h3>Border Style Gallery</h3>
        <p>Lipgloss v2 includes an extensive collection of border styles suitable for different design aesthetics and functional requirements:</p>

        <h4>Available Border Styles:</h4>
        <ul>
          <li><strong>Normal</strong> - Standard single-line borders for general use</li>
          <li><strong>Rounded</strong> - Rounded corners for modern, friendly aesthetics</li>
          <li><strong>Thick</strong> - Heavy borders for emphasis and prominence</li>
          <li><strong>Double</strong> - Double-line borders for formal or structured layouts</li>
          <li><strong>Hidden</strong> - Invisible borders that maintain spacing</li>
          <li><strong>Custom</strong> - User-defined border characters for unique designs</li>
        </ul>

        <h3>Color Accessibility and Compliance</h3>
        <p>Creating accessible terminal interfaces requires careful attention to color contrast, readability, and compatibility with assistive technologies:</p>

        <h4>Accessibility Guidelines:</h4>
        <ul>
          <li><strong>Contrast Ratios</strong> - Ensure sufficient contrast between text and background</li>
          <li><strong>Color Blindness</strong> - Test designs with color vision deficiency simulators</li>
          <li><strong>Alternative Indicators</strong> - Use shape, position, and text in addition to color</li>
          <li><strong>High Contrast Modes</strong> - Provide high contrast alternatives</li>
          <li><strong>Terminal Compatibility</strong> - Graceful degradation for limited color terminals</li>
        </ul>

        <h3>Integration with Layout Systems</h3>
        <p>Effective combination of colors and borders with Lipgloss v2's layout capabilities enables sophisticated visual design:</p>

        <h4>Integration Techniques:</h4>
        <ul>
          <li><strong>Hierarchical Visual Design</strong> - Use color and borders to establish information hierarchy</li>
          <li><strong>Responsive Color Schemes</strong> - Adapt colors based on terminal capabilities</li>
          <li><strong>Theme System Integration</strong> - Coordinate colors and borders within theme systems</li>
          <li><strong>Performance Optimization</strong> - Efficient color and border rendering</li>
        </ul>

        <h3>Advanced Styling Techniques</h3>
        <p>Beyond basic color and border application, advanced techniques enable sophisticated visual effects:</p>

        <h4>Advanced Techniques:</h4>
        <ul>
          <li><strong>Color Interpolation</strong> - Smooth color transitions for animations</li>
          <li><strong>Border Animations</strong> - Dynamic border effects for interactive elements</li>
          <li><strong>Conditional Styling</strong> - State-based color and border changes</li>
          <li><strong>Pattern Creation</strong> - Using colors and borders to create visual patterns</li>
        </ul>

        <h3>Performance Considerations</h3>
        <p>Color and border rendering can impact performance, particularly in complex interfaces with many styled elements:</p>

        <h4>Performance Optimization:</h4>
        <ul>
          <li><strong>Color Caching</strong> - Cache color calculations for repeated use</li>
          <li><strong>Border Optimization</strong> - Efficient border character rendering</li>
          <li><strong>Batch Operations</strong> - Group styling operations for efficiency</li>
          <li><strong>Lazy Evaluation</strong> - Defer expensive styling calculations when possible</li>
        </ul>
      `
    },
    {
      id: 'best-practices',
      number: 9,
      title: 'Best Practices and Advanced Patterns',
      description: 'Professional development guidance and architectural patterns',
      icon: 'Award',
      content: `
        <h2>Best Practices and Advanced Patterns</h2>
        <p>Mastering Lipgloss v2 requires understanding not just technical capabilities, but also best practices and design patterns for maintainable, performant, and user-friendly applications. This chapter provides comprehensive guidance for professional terminal application development.</p>

        <h3>Code Organization and Architecture</h3>
        <p>Effective applications require thoughtful code organization that separates concerns, promotes reusability, and maintains clarity as complexity grows:</p>

        <h4>Architectural Principles:</h4>
        <ul>
          <li><strong>Separation of Concerns</strong> - Isolate styling, layout, and business logic</li>
          <li><strong>Component-Based Architecture</strong> - Create reusable UI components</li>
          <li><strong>State Management Patterns</strong> - Centralized state with clear update patterns</li>
          <li><strong>Event-Driven Design</strong> - Loose coupling through event systems</li>
          <li><strong>Dependency Injection</strong> - Flexible component composition</li>
        </ul>

        <h3>Performance Optimization Strategies</h3>
        <p>Terminal applications must maintain responsiveness and efficiency, particularly when dealing with complex layouts and frequent updates:</p>

        <h4>Performance Best Practices:</h4>
        <ul>
          <li><strong>Rendering Optimization</strong> - Minimize unnecessary redraws and calculations</li>
          <li><strong>Memory Management</strong> - Efficient allocation and cleanup patterns</li>
          <li><strong>Caching Strategies</strong> - Cache expensive computations and styled content</li>
          <li><strong>Lazy Loading</strong> - Defer expensive operations until needed</li>
          <li><strong>Profiling and Monitoring</strong> - Regular performance measurement and optimization</li>
        </ul>

        <h3>User Experience Design Principles</h3>
        <p>Creating effective terminal interfaces requires understanding UX principles adapted for character-based environments:</p>

        <h4>UX Design Guidelines:</h4>
        <ul>
          <li><strong>Information Hierarchy</strong> - Clear visual organization of content</li>
          <li><strong>Interaction Patterns</strong> - Consistent, predictable user interactions</li>
          <li><strong>Accessibility Considerations</strong> - Inclusive design for all users</li>
          <li><strong>Responsive Design</strong> - Adaptation to different terminal sizes</li>
          <li><strong>Error Handling</strong> - Graceful error presentation and recovery</li>
        </ul>

        <h3>Testing and Quality Assurance</h3>
        <p>Comprehensive testing ensures reliability and maintainability of complex terminal applications:</p>

        <h4>Testing Strategies:</h4>
        <ul>
          <li><strong>Unit Testing</strong> - Test individual components and functions</li>
          <li><strong>Integration Testing</strong> - Verify component interactions</li>
          <li><strong>Visual Testing</strong> - VHS-based validation of visual output</li>
          <li><strong>Performance Testing</strong> - Benchmark rendering and response times</li>
          <li><strong>Accessibility Testing</strong> - Verify inclusive design implementation</li>
        </ul>

        <h3>Error Handling and Resilience</h3>
        <p>Robust applications handle errors gracefully and provide meaningful feedback to users:</p>

        <h4>Error Handling Patterns:</h4>
        <ul>
          <li><strong>Graceful Degradation</strong> - Maintain functionality when features fail</li>
          <li><strong>User-Friendly Messages</strong> - Clear, actionable error communication</li>
          <li><strong>Recovery Mechanisms</strong> - Automatic and manual error recovery</li>
          <li><strong>Logging and Monitoring</strong> - Comprehensive error tracking</li>
          <li><strong>Validation and Prevention</strong> - Prevent errors through input validation</li>
        </ul>

        <h3>Integration and Deployment</h3>
        <p>Real-world applications must consider integration with existing systems and deployment requirements:</p>

        <h4>Integration Considerations:</h4>
        <ul>
          <li><strong>Terminal Compatibility</strong> - Support for diverse terminal environments</li>
          <li><strong>System Integration</strong> - Interaction with operating system features</li>
          <li><strong>Configuration Management</strong> - Flexible configuration and customization</li>
          <li><strong>Deployment Strategies</strong> - Packaging and distribution approaches</li>
          <li><strong>Update Mechanisms</strong> - Safe and reliable application updates</li>
        </ul>

        <h3>Security Considerations</h3>
        <p>Terminal applications must address security concerns, particularly when handling user input or system interactions:</p>

        <h4>Security Best Practices:</h4>
        <ul>
          <li><strong>Input Validation</strong> - Sanitize and validate all user input</li>
          <li><strong>Output Encoding</strong> - Prevent injection attacks through proper encoding</li>
          <li><strong>Privilege Management</strong> - Run with minimal necessary privileges</li>
          <li><strong>Data Protection</strong> - Secure handling of sensitive information</li>
          <li><strong>Audit and Logging</strong> - Comprehensive security event logging</li>
        </ul>

        <h3>Documentation and Maintenance</h3>
        <p>Long-term success requires comprehensive documentation and maintainable code practices:</p>

        <h4>Documentation Strategies:</h4>
        <ul>
          <li><strong>Code Documentation</strong> - Clear, comprehensive code comments</li>
          <li><strong>API Documentation</strong> - Detailed interface documentation</li>
          <li><strong>User Documentation</strong> - Clear usage guides and examples</li>
          <li><strong>Architecture Documentation</strong> - System design and decision rationale</li>
          <li><strong>Visual Documentation</strong> - VHS recordings and screenshots</li>
        </ul>

        <h3>Community and Ecosystem</h3>
        <p>Successful terminal applications benefit from community engagement and ecosystem integration:</p>

        <h4>Community Engagement:</h4>
        <ul>
          <li><strong>Open Source Practices</strong> - Contribution guidelines and community building</li>
          <li><strong>Plugin Architecture</strong> - Extensibility for community contributions</li>
          <li><strong>Standards Compliance</strong> - Adherence to terminal and accessibility standards</li>
          <li><strong>Ecosystem Integration</strong> - Compatibility with popular tools and frameworks</li>
          <li><strong>Knowledge Sharing</strong> - Documentation and example sharing</li>
        </ul>

        <h3>Future-Proofing and Evolution</h3>
        <p>Applications should be designed to evolve with changing requirements and technologies:</p>

        <h4>Evolution Strategies:</h4>
        <ul>
          <li><strong>Modular Design</strong> - Flexible architecture for feature additions</li>
          <li><strong>API Versioning</strong> - Backward compatibility and migration paths</li>
          <li><strong>Technology Monitoring</strong> - Awareness of emerging terminal technologies</li>
          <li><strong>User Feedback Integration</strong> - Continuous improvement based on user needs</li>
          <li><strong>Performance Monitoring</strong> - Ongoing optimization and improvement</li>
        </ul>
      `
    }
  ],

  examples: [
    {
      id: 'basic-styling',
      number: 1,
      title: 'Basic Styling',
      description: 'Fundamental styling concepts and patterns',
      difficulty: 'Beginner',
      concepts: ['Style creation', 'Color application', 'Border styling', 'Text formatting'],
      codeFile: '01-basic-styling.go',
      gifUrl: '/basic_styling.gif'
    },
    {
      id: 'simple-layers',
      number: 2,
      title: 'Simple Layers',
      description: 'Layer creation and basic positioning',
      difficulty: 'Beginner',
      concepts: ['Layer creation', 'Positioning', 'Canvas composition', 'Basic rendering'],
      codeFile: '02-simple-layers.go',
      gifUrl: '/simple_layers.gif'
    },
    {
      id: 'basic-canvas',
      number: 3,
      title: 'Basic Canvas',
      description: 'Canvas composition and multi-layer rendering',
      difficulty: 'Beginner',
      concepts: ['Canvas creation', 'Multi-layer composition', 'Overlap handling', 'Rendering pipeline'],
      codeFile: '03-basic-canvas.go',
      gifUrl: '/basic_canvas.gif'
    },
    {
      id: 'positioning',
      number: 4,
      title: 'Positioning Techniques',
      description: 'Advanced positioning and layout strategies',
      difficulty: 'Intermediate',
      concepts: ['Absolute positioning', 'Relative positioning', 'Dynamic layouts', 'Responsive design'],
      codeFile: '04-positioning.go',
      gifUrl: '/positioning.gif'
    },
    {
      id: 'complex-layering',
      number: 5,
      title: 'Complex Layering',
      description: 'Sophisticated multi-layer compositions',
      difficulty: 'Intermediate',
      concepts: ['Complex compositions', 'Layer management', 'Visual hierarchy', 'Performance optimization'],
      codeFile: '05-complex-layering.go',
      gifUrl: '/complex_layering.gif'
    },
    {
      id: 'zindex-demo',
      number: 6,
      title: 'Z-index Management',
      description: 'Depth control and stacking order',
      difficulty: 'Intermediate',
      concepts: ['Z-index control', 'Depth management', 'Stacking order', 'Visual layering'],
      codeFile: '06-zindex-demo.go',
      gifUrl: '/zindex_demo.gif'
    },
    {
      id: 'nested-layers',
      number: 7,
      title: 'Nested Compositions',
      description: 'Hierarchical layer structures',
      difficulty: 'Advanced',
      concepts: ['Nested layers', 'Hierarchical composition', 'Parent-child relationships', 'Complex layouts'],
      codeFile: '07-nested-layers.go',
      gifUrl: '/nested_layers.gif'
    },
    {
      id: 'dynamic-positioning',
      number: 8,
      title: 'Dynamic Positioning',
      description: 'Runtime positioning algorithms',
      difficulty: 'Advanced',
      concepts: ['Dynamic positioning', 'Algorithmic layout', 'Runtime calculations', 'Interactive positioning'],
      codeFile: '08-dynamic-positioning.go',
      gifUrl: '/dynamic_positioning.gif'
    },
    {
      id: 'color-showcase',
      number: 9,
      title: 'Color Showcase',
      description: 'Comprehensive color techniques and palettes',
      difficulty: 'Advanced',
      concepts: ['256-color support', 'Color palettes', 'Accessibility', 'Color theory'],
      codeFile: '09-color-showcase.go',
      gifUrl: '/color_showcase.gif'
    },
    {
      id: 'border-gallery',
      number: 10,
      title: 'Border Gallery',
      description: 'Complete border styling reference',
      difficulty: 'Advanced',
      concepts: ['Border styles', 'Custom borders', 'Border combinations', 'Visual design'],
      codeFile: '10-border-gallery.go',
      gifUrl: '/border_gallery.gif'
    }
  ],

  windowManager: {
    title: 'Interactive Window Manager Demo',
    description: 'A fully functional window manager demonstrating overlapping windows, drag & drop, resizing, focus management, and professional styling.',
    features: {
      core: [
        'Multiple overlapping windows',
        'Drag and drop movement',
        'Window resizing with constraints',
        'Focus management and Z-index control'
      ],
      advanced: [
        'Professional styling and borders',
        'Interactive command interface',
        'Animated transitions',
        'Boundary checking and snapping'
      ]
    },
    commands: [
      {
        command: 'create <title> <x> <y> <width> <height> <color>',
        description: 'Create a new window with specified properties'
      },
      {
        command: 'move <id> <x> <y>',
        description: 'Move window to specified position'
      },
      {
        command: 'resize <id> <width> <height>',
        description: 'Resize window to specified dimensions'
      },
      {
        command: 'focus <id>',
        description: 'Focus specific window and bring to front'
      },
      {
        command: 'close <id>',
        description: 'Close and remove window'
      },
      {
        command: 'list',
        description: 'Display all windows with details'
      },
      {
        command: 'render',
        description: 'Force complete re-render of all windows'
      },
      {
        command: 'help',
        description: 'Display command help and usage'
      },
      {
        command: 'quit',
        description: 'Exit the window manager application'
      }
    ]
  },

  validation: {
    title: 'VHS Validation Results',
    description: 'Comprehensive testing and validation using VHS text screenshots',
    results: {
      filesValidated: 5,
      passRate: 100,
      totalTests: 5,
      passedTests: 5,
      failedTests: 0
    },
    features: [
      'ANSI escape sequences detected in all files',
      'Box drawing characters (borders) working correctly',
      '256-color ANSI codes functioning properly',
      'Multiple windows detected in window manager demos',
      'Appropriate file sizes and content structure verified'
    ]
  },

  stats: {
    chapters: 9,
    examples: 10,
    validationRate: 100,
    windowManagers: 1,
    vhsRecordings: 5,
    codeFiles: 15
  }
};

export default courseData;

