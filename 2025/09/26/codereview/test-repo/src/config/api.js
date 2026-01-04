const API_CONFIG = {
  development: {
    baseUrl: 'http://localhost:3000/api',
    timeout: 5000,
    retries: 3
  },
  production: {
    baseUrl: 'https://api.example.com',
    timeout: 10000,
    retries: 2
  },
  test: {
    baseUrl: 'http://localhost:3001/api',
    timeout: 2000,
    retries: 1
  }
};

const environment = process.env.NODE_ENV || 'development';

export const apiConfig = API_CONFIG[environment];

export const createApiUrl = (endpoint) => {
  return `${apiConfig.baseUrl}${endpoint}`;
};

export const getAuthHeaders = () => {
  const token = localStorage.getItem('authToken');
  return token ? { Authorization: `Bearer ${token}` } : {};
};

export const handleApiError = (error) => {
  if (error.response) {
    // Server responded with error status
    const { status, data } = error.response;
    switch (status) {
      case 401:
        // Unauthorized - redirect to login
        localStorage.removeItem('authToken');
        window.location.href = '/login';
        break;
      case 403:
        throw new Error('Access forbidden');
      case 404:
        throw new Error('Resource not found');
      case 500:
        throw new Error('Server error');
      default:
        throw new Error(data.message || 'An error occurred');
    }
  } else if (error.request) {
    // Network error
    throw new Error('Network error - please check your connection');
  } else {
    throw new Error(error.message || 'An unexpected error occurred');
  }
};
