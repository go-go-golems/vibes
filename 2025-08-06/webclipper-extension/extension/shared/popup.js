// Popup script for Web Clipper extension
document.addEventListener('DOMContentLoaded', async function() {
  const form = document.getElementById('clipForm');
  const urlField = document.getElementById('url');
  const pageTitleField = document.getElementById('pageTitle');
  const selectedTextField = document.getElementById('selectedText');
  const titleField = document.getElementById('title');
  const categoryField = document.getElementById('category');
  const noteField = document.getElementById('note');
  const saveBtn = document.getElementById('saveBtn');
  const cancelBtn = document.getElementById('cancelBtn');
  const status = document.getElementById('status');

  // Get current tab information
  try {
    const [tab] = await chrome.tabs.query({ active: true, currentWindow: true });
    
    if (tab) {
      urlField.value = tab.url;
      pageTitleField.value = tab.title;
      
      // Auto-fill title with page title initially
      if (!titleField.value) {
        titleField.value = tab.title;
      }
      
      // Get selected text from content script
      try {
        const response = await chrome.tabs.sendMessage(tab.id, { action: 'getSelectedText' });
        if (response && response.selectedText) {
          selectedTextField.value = response.selectedText;
          
          // If we have selected text and no custom title, use first part of selection
          if (response.selectedText.length > 0 && titleField.value === tab.title) {
            const firstLine = response.selectedText.split('\n')[0].trim();
            if (firstLine.length > 0) {
              titleField.value = firstLine.length > 60 ? firstLine.substring(0, 60) + '...' : firstLine;
            }
          }
        }
      } catch (error) {
        console.log('Could not get selected text:', error);
      }
    }
  } catch (error) {
    console.error('Error getting tab info:', error);
    showStatus('Error getting page information', 'error');
  }

  // Form submission
  form.addEventListener('submit', async function(e) {
    e.preventDefault();
    
    if (!titleField.value.trim() || !categoryField.value) {
      showStatus('Please fill in all required fields', 'error');
      return;
    }

    saveBtn.disabled = true;
    saveBtn.classList.add('loading');
    
    const clipData = {
      timestamp: new Date().toISOString(),
      url: urlField.value,
      title: titleField.value.trim(),
      category: categoryField.value,
      selectedText: selectedTextField.value.trim(),
      note: noteField.value.trim(),
      pageTitle: pageTitleField.value,
      domain: extractDomain(urlField.value)
    };

    try {
      // Send to background script for native messaging
      const response = await chrome.runtime.sendMessage({
        action: 'saveClip',
        data: clipData
      });

      if (response && response.success) {
        showStatus('Clip saved successfully!', 'success');
        setTimeout(() => {
          window.close();
        }, 1500);
      } else {
        throw new Error(response?.error || 'Failed to save clip');
      }
    } catch (error) {
      console.error('Error saving clip:', error);
      showStatus('Error saving clip: ' + error.message, 'error');
    } finally {
      saveBtn.disabled = false;
      saveBtn.classList.remove('loading');
    }
  });

  // Cancel button
  cancelBtn.addEventListener('click', function() {
    window.close();
  });

  // Auto-focus title field
  titleField.focus();
  titleField.select();

  function showStatus(message, type) {
    status.textContent = message;
    status.className = `status ${type}`;
    status.classList.remove('hidden');
    
    if (type === 'success') {
      setTimeout(() => {
        status.classList.add('hidden');
      }, 3000);
    }
  }

  function extractDomain(url) {
    try {
      return new URL(url).hostname;
    } catch {
      return 'unknown';
    }
  }
});

