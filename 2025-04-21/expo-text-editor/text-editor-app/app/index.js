import React, { useState, useEffect } from 'react';
import { StyleSheet, View, SafeAreaView, Platform } from 'react-native';
import { Provider as PaperProvider, DefaultTheme, Text, Button } from 'react-native-paper';
import TextEditor from '../components/TextEditor';
import { storeData, getData } from '../utils/storage';

const theme = {
  ...DefaultTheme,
  colors: {
    ...DefaultTheme.colors,
    primary: '#6200ee',
    accent: '#03dac4',
  },
};

export default function App() {
  const [content, setContent] = useState('');
  const [isLoading, setIsLoading] = useState(true);

  useEffect(() => {
    // Load saved content on startup
    loadContent();
  }, []);

  const loadContent = async () => {
    try {
      const savedContent = await getData('editor-content');
      if (savedContent) {
        setContent(savedContent);
      }
    } catch (error) {
      console.error('Error loading content:', error);
    } finally {
      setIsLoading(false);
    }
  };

  const saveContent = async (newContent) => {
    try {
      await storeData('editor-content', newContent);
      console.log('Content saved successfully');
    } catch (error) {
      console.error('Error saving content:', error);
    }
  };

  const handleContentChange = (newContent) => {
    setContent(newContent);
    saveContent(newContent);
  };

  if (isLoading) {
    return (
      <View style={styles.container}>
        <Text>Loading editor...</Text>
      </View>
    );
  }

  return (
    <PaperProvider theme={theme}>
      <SafeAreaView style={styles.container}>
        <View style={styles.header}>
          <Text style={styles.title}>Minimalist Text Editor</Text>
        </View>
        <TextEditor 
          initialContent={content}
          onContentChange={handleContentChange}
        />
      </SafeAreaView>
    </PaperProvider>
  );
}

const styles = StyleSheet.create({
  container: {
    flex: 1,
    backgroundColor: '#f5f5f5',
  },
  header: {
    padding: 16,
    backgroundColor: '#6200ee',
  },
  title: {
    fontSize: 20,
    fontWeight: 'bold',
    color: 'white',
  },
});
