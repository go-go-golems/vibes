- Rendering the prompt should be done in the agent, not the command, or at least give the agent a way to add their own variables to render the prompt
- What about that system prompt, do we even need it in the yaml?

- test the file collection with 256 output tokens to cause it to split

- proper document on when to stream output and how to setup the router

- remove the info logging
- add flag to store files on disk
  - agents should be able to add their own parameter layers to the command