export const validateEmail = (email) => {
  const emailRegex = /^[^\s@]+@[^\s@]+\.[^\s@]+$/;
  return emailRegex.test(email);
};

export const validatePassword = (password) => {
  // Password must be at least 8 characters with uppercase, lowercase, and number
  const passwordRegex = /^(?=.*[a-z])(?=.*[A-Z])(?=.*\d)[a-zA-Z\d@$!%*?&]{8,}$/;
  return passwordRegex.test(password);
};

export const validateUsername = (username) => {
  // Username must be 3-20 characters, alphanumeric and underscores only
  const usernameRegex = /^[a-zA-Z0-9_]{3,20}$/;
  return usernameRegex.test(username);
};

export const validateRequired = (value) => {
  return value && value.trim().length > 0;
};

export const validateUserData = (userData) => {
  const errors = {};

  if (!validateRequired(userData.name)) {
    errors.name = 'Name is required';
  }

  if (!validateRequired(userData.email)) {
    errors.email = 'Email is required';
  } else if (!validateEmail(userData.email)) {
    errors.email = 'Invalid email format';
  }

  if (!validateRequired(userData.username)) {
    errors.username = 'Username is required';
  } else if (!validateUsername(userData.username)) {
    errors.username = 'Username must be 3-20 characters, alphanumeric and underscores only';
  }

  if (userData.password && !validatePassword(userData.password)) {
    errors.password = 'Password must be at least 8 characters with uppercase, lowercase, and number';
  }

  return {
    isValid: Object.keys(errors).length === 0,
    errors
  };
};
