// app.ts - Main TypeScript entry point for our application

// Import types from Go (these will be generated later)
// import { User, UserCreateParams } from '@go/user';

/**
 * This is our main application entry point that will be executed by Goja
 * 
 * In a real application, this would contain business logic that calls into
 * the Go-exposed APIs.
 */

// Declare the Go functions we expect to be available
// These will be properly typed once we generate the TypeScript definitions
declare function CreateUser(params: any): any;

// Example usage of the Go-exposed function
function main() {
  try {
    console.log("TypeScript application starting...");
    
    // Call the Go function exposed to our VM
    const user = CreateUser({
      username: "testuser",
      email: "test@example.com"
    });
    
    console.log("User created successfully:");
    console.log(`ID: ${user.id}`);
    console.log(`Username: ${user.username}`);
    console.log(`Email: ${user.email}`);
    console.log(`Created at: ${user.createdAt}`);
    console.log(`Active: ${user.isActive}`);
    
    return { success: true, user };
  } catch (error) {
    console.error("Error creating user:", error);
    return { success: false, error: String(error) };
  }
}

// Execute the main function
const result = main();
console.log("Execution result:", result);

// Export the result for potential use by the Go host
module.exports = { result };
