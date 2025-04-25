# Suna - Open Source Generalist AI Agent

<div align="center">
  <img src="https://github.com/kortix-ai/suna/assets/11191224/14c94f2a-9a1f-4063-8b9e-9a8ddf780c5c" alt="suna_logo" width="200">
</div>

Suna is an open-source generalist AI agent that can act on your behalf.

Suna has launched → [www.suna.so](https://www.suna.so)

Join our active [Discord community](https://discord.gg/UeYp8chH2q)! 

## What is Suna?

Suna is a generalist agent that can perform a wide variety of tasks, from web browsing and research to interacting with websites. If you have ever been frustrated by the limitations of conventional large language models, Suna offers a solution.

For example, here's a list of tasks Suna can assist with:

- "Research top competitor marketing strategies for a new SaaS startup"
- "Find me an airbnb in London for 2 people next weekend and share the link"
- "Read through this website and summarize the key features of this product"
- "Fill out this form for me with my details"
- "I need a list of the top 10 AI startups that raised funding in the past 3 months. For each, find their website, what they do, and how much they raised"

And much more...

Watch a quick demo:

https://github.com/kortix-ai/suna/assets/11191224/85ae26cb-00f9-4aa0-850e-eee93a4b76a3

## How It Works

Suna combines the strengths of advanced AI language models with specialized tools to create a versatile agent that can handle a wide range of tasks effectively. The architecture revolves around:

- **Advanced Language Models**: Suna leverages state-of-the-art models like Claude-3 and GPT-4o to understand context, create plans, and generate responses.
- **Tool Orchestration**: The agent seamlessly coordinates a suite of tools for web searches, browsing, and website interactions.
- **Iterative Execution**: For complex tasks, Suna breaks down objectives into manageable steps, executing them systematically while providing progress updates.

## Why We Built Suna

While LLMs have demonstrated remarkable capabilities, they face significant constraints in addressing everyday needs:

- They lack the ability to search the web for current information
- They can't navigate websites or perform online tasks
- They struggle with long-running, multi-step processes

Suna addresses these limitations by creating an agent with the ability to plan, browse the web, and take actions based on real-time information.

## Getting Started Locally

### Prerequisites

1. Clone this repository:
   ```
   git clone https://github.com/kortix-ai/suna.git
   cd suna
   ```

2. Set up environment variables:
   - Create a `.env` file in the `backend` directory with the following:
     ```
     ANTHROPIC_API_KEY=your_anthropic_api_key  # Required if using Anthropic (claude-3-7-sonnet-latest)
     OPENAI_API_KEY=your_openai_api_key  # Required if using OpenAI (gpt-4o)
     TAVILY_API_KEY=your_tavily_api_key  # Optional but recommended for better search
     LLAMA_INDEX_API_KEY=your_llama_api_key  # Optional but recommended for faster web extraction
     RAPID_API_KEY=your_rapid_api_key  # Required for automated web browsing
     
     # Superbase database connection details
     SUPABASE_URL=your_supabase_url
     SUPABASE_KEY=your_supabase_key
     ```

3. Set up Supabase:
   - Create a new Supabase project
   - Set up authentication (email and password)
   - Create the necessary tables using the SQL in `backend/db/init.sql`

4. Configure frontend environment:
   - Create a `.env.local` file in the `frontend` directory with the following:
     ```
     NEXT_PUBLIC_SUPABASE_URL=your_supabase_url
     NEXT_PUBLIC_SUPABASE_ANON_KEY=your_supabase_anon_key
     NEXT_PUBLIC_API_URL=http://localhost:8000
     ```

5. Install dependencies:
   - Frontend:
     ```
     cd frontend
     npm install
     ```
   - Backend:
     ```
     cd backend
     pip install -r requirements.txt
     ```

6. Start the application:
   - In one terminal, start the frontend:
     ```
     cd frontend
     npm run dev
     ```
   - In another terminal, start the backend:
     ```
     cd backend
     python api.py
     ```

7. Access Suna:
   - Open your browser and navigate to `http://localhost:3000`
   - Sign up for an account using the Supabase authentication
   - Start using your self-hosted Suna instance!

## License

Kortix Suna is licensed under the Apache License, Version 2.0. See [LICENSE](LICENSE) for the full license text.