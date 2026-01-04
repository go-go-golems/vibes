# Pelican Farm Management - Static Assets

## Overview
Modern, responsive static assets for the Pelican Farm Management System with professional styling, enhanced user experience, and comprehensive functionality.

## Files Structure

### CSS Files
- **`pelican-farm.css`** - Main stylesheet with modern design system
- **`print.css`** - Print-optimized styles for reports and documentation

### JavaScript Files
- **`pelican-farm.js`** - Enhanced UX functionality and interactions

### Other Assets
- **`manifest.json`** - Progressive Web App configuration
- **`README.md`** - This documentation file

## Features Implemented

### 🎨 Modern Design System
- Custom CSS variables for consistent theming
- Gradient backgrounds and modern color palette
- Professional card designs with subtle shadows
- Responsive layout optimizations

### ✨ Visual Enhancements
- Smooth animations and transitions
- Hover effects on interactive elements
- Custom status badges with pulse animations
- Enhanced navigation with gradient backgrounds
- Beautiful empty state designs

### 🚀 User Experience
- Real-time form validation with visual feedback
- Auto-save functionality for forms
- Enhanced confirmation dialogs
- Search functionality for tables
- Loading states for buttons
- Smooth page load animations

### 📱 Progressive Web App
- PWA manifest for installable app experience
- Custom app icons and branding
- Mobile-optimized interface
- Responsive design for all screen sizes

### 🖨️ Print Support
- Dedicated print stylesheet
- Optimized layouts for printing
- Clean black-and-white formatting
- Page break controls

### 🎯 Accessibility
- High contrast ratios
- Keyboard navigation support
- Screen reader friendly markup
- Responsive text sizing

## Usage

The static assets are automatically included in the layout template:

```html
<link href="/static/css/pelican-farm.css" rel="stylesheet">
<link href="/static/css/print.css" rel="stylesheet" media="print">
<script src="/static/js/pelican-farm.js"></script>
```

## JavaScript API

Global functions available:
- `PelicanFarm.editFarm(id, name, location, description)`
- `PelicanFarm.editPelican(id, name, species, farmId, status, age, description)`
- `PelicanFarm.deleteFarm(id, name)`
- `PelicanFarm.deletePelican(id, name)`
- `PelicanFarm.showNotification(message, type, duration)`
- `PelicanFarm.showConfirmDialog(title, message, type, onConfirm)`

## Browser Support
- Modern browsers (Chrome 88+, Firefox 85+, Safari 14+)
- Mobile browsers (iOS Safari, Chrome Mobile)
- Progressive enhancement for older browsers

## Development Notes
- Uses CSS custom properties for easy theming
- Modular JavaScript with error handling
- Optimized for performance
- Follows modern web standards

## Future Enhancements
- Dark mode toggle
- Additional animations
- Advanced filtering options
- Offline support
- Data visualization charts
