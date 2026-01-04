import React, { useState, useEffect, useRef } from 'react';
import { StyleSheet, View, ScrollView, Platform } from 'react-native';
import { Surface, IconButton, TextInput, Portal, Dialog, Button, Text } from 'react-native-paper';
import CodeEditor, { CodeEditorSyntaxStyles } from '@rivascva/react-native-code-editor';

/**
 * TextEditor component - A minimalist text editor with highlighting and commenting capabilities
 * 
 * @param {Object} props - Component props
 * @param {string} props.initialContent - Initial content to display in the editor
 * @param {Function} props.onContentChange - Callback function when content changes
 */
const TextEditor = ({ initialContent = '', onContentChange }) => {
  const [text, setText] = useState(initialContent);
  const [selection, setSelection] = useState({ start: 0, end: 0 });
  const [highlights, setHighlights] = useState([]);
  const [comments, setComments] = useState({});
  const [showCommentDialog, setShowCommentDialog] = useState(false);
  const [activeHighlightId, setActiveHighlightId] = useState(null);
  const [currentComment, setCurrentComment] = useState('');
  
  useEffect(() => {
    // Initialize with any saved content
    if (initialContent) {
      setText(initialContent);
    }
  }, [initialContent]);

  const handleTextChange = (newText) => {
    setText(newText);
    if (onContentChange) {
      onContentChange(newText);
    }
  };

  const handleSelectionChange = ({ nativeEvent }) => {
    if (nativeEvent && nativeEvent.selection) {
      setSelection(nativeEvent.selection);
    }
  };

  const highlightSelectedText = () => {
    if (selection.start !== selection.end) {
      const newHighlightId = Date.now().toString();
      const newHighlight = {
        id: newHighlightId,
        start: selection.start,
        end: selection.end,
        color: '#ffeb3b' // Default yellow highlight
      };
      
      setHighlights([...highlights, newHighlight]);
      setActiveHighlightId(newHighlightId);
      setShowCommentDialog(true);
    }
  };

  const addComment = () => {
    if (currentComment.trim() && activeHighlightId) {
      setComments({
        ...comments,
        [activeHighlightId]: currentComment
      });
      setCurrentComment('');
      setShowCommentDialog(false);
    }
  };

  const cancelComment = () => {
    setCurrentComment('');
    setShowCommentDialog(false);
    
    // Remove the highlight if no comment was added
    if (activeHighlightId && !comments[activeHighlightId]) {
      setHighlights(highlights.filter(h => h.id !== activeHighlightId));
    }
    
    setActiveHighlightId(null);
  };

  return (
    <Surface style={styles.container}>
      <View style={styles.editorContainer}>
        <CodeEditor
          style={{
            ...styles.editor,
            inputLineHeight: 24,
            highlighterLineHeight: 24,
          }}
          language="javascript"
          syntaxStyle={CodeEditorSyntaxStyles.github}
          showLineNumbers
          initialValue={text}
          onChange={handleTextChange}
          onSelectionChange={handleSelectionChange}
        />
        
        <View style={styles.toolbar}>
          <IconButton
            icon="format-color-highlight"
            onPress={highlightSelectedText}
            disabled={selection.start === selection.end}
          />
        </View>
      </View>
      
      {/* Comments display in margin */}
      <View style={styles.commentsMargin}>
        <Text style={styles.commentsHeader}>Comments</Text>
        {Object.entries(comments).map(([highlightId, comment]) => {
          const highlight = highlights.find(h => h.id === highlightId);
          if (!highlight) return null;
          
          return (
            <View key={highlightId} style={styles.commentBubble}>
              <Text style={styles.commentText}>{comment}</Text>
            </View>
          );
        })}
      </View>

      {/* Comment Dialog */}
      <Portal>
        <Dialog visible={showCommentDialog} onDismiss={cancelComment}>
          <Dialog.Title>Add Comment</Dialog.Title>
          <Dialog.Content>
            <TextInput
              mode="outlined"
              label="Comment"
              value={currentComment}
              onChangeText={setCurrentComment}
              multiline
              numberOfLines={3}
            />
          </Dialog.Content>
          <Dialog.Actions>
            <Button onPress={cancelComment}>Cancel</Button>
            <Button onPress={addComment}>Add</Button>
          </Dialog.Actions>
        </Dialog>
      </Portal>
    </Surface>
  );
};

const styles = StyleSheet.create({
  container: {
    flex: 1,
    flexDirection: 'row',
    margin: 16,
    borderRadius: 8,
    overflow: 'hidden',
  },
  editorContainer: {
    flex: 3,
    flexDirection: 'column',
  },
  editor: {
    flex: 1,
    fontSize: 16,
    backgroundColor: 'white',
    minHeight: '100%',
  },
  toolbar: {
    flexDirection: 'row',
    padding: 8,
    backgroundColor: '#f0f0f0',
    borderTopWidth: 1,
    borderTopColor: '#e0e0e0',
  },
  commentsMargin: {
    flex: 1,
    backgroundColor: '#f0f0f0',
    padding: 8,
    borderLeftWidth: 1,
    borderLeftColor: '#e0e0e0',
  },
  commentsHeader: {
    fontWeight: 'bold',
    marginBottom: 8,
    fontSize: 16,
  },
  commentBubble: {
    backgroundColor: 'white',
    borderRadius: 8,
    padding: 8,
    marginBottom: 8,
    borderLeftWidth: 4,
    borderLeftColor: '#6200ee',
  },
  commentText: {
    fontSize: 14,
  },
});

export default TextEditor;
