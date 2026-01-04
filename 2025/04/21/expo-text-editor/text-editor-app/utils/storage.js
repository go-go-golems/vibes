import AsyncStorage from '@react-native-async-storage/async-storage';

/**
 * Store data in local storage
 * @param {string} key - Storage key
 * @param {string} value - Content to store
 */
export const storeData = async (key, value) => {
  try {
    await AsyncStorage.setItem(key, value);
  } catch (error) {
    console.error('Error storing data:', error);
    throw error;
  }
};

/**
 * Retrieve data from local storage
 * @param {string} key - Storage key
 * @returns {Promise<string>} - Stored content
 */
export const getData = async (key) => {
  try {
    const value = await AsyncStorage.getItem(key);
    return value;
  } catch (error) {
    console.error('Error retrieving data:', error);
    throw error;
  }
};

/**
 * Remove data from local storage
 * @param {string} key - Storage key to remove
 */
export const removeData = async (key) => {
  try {
    await AsyncStorage.removeItem(key);
  } catch (error) {
    console.error('Error removing data:', error);
    throw error;
  }
};
