console.log('Hello World');

// Import user management modules
import { UserService } from './src/services/UserService.js';
import { validateUserData, validateEmail } from './src/utils/validation.js';
import { apiConfig } from './src/config/api.js';

// Initialize user service
const userService = new UserService(apiConfig.baseUrl);

// Enhanced user validation
function validateUser(email) { 
  return validateEmail(email);
}

// User management functions
async function createNewUser(userData) {
  const validation = validateUserData(userData);
  if (!validation.isValid) {
    throw new Error(`Validation failed: ${Object.values(validation.errors).join(', ')}`);
  }
  
  try {
    const user = await userService.createUser(userData);
    console.log('User created successfully:', user);
    return user;
  } catch (error) {
    console.error('Failed to create user:', error);
    throw error;
  }
}

// Example usage
const exampleUser = {
  name: 'John Doe',
  email: 'john.doe@example.com',
  username: 'johndoe'
};

console.log('User validation result:', validateUser(exampleUser.email));
