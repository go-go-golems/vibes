// Pelican Farm Management - JavaScript Enhancements

document.addEventListener('DOMContentLoaded', function() {
    // Initialize all components
    initializeTooltips();
    initializeAnimations();
    initializeFormValidation();
    initializeSearchAndFilters();
    initializeAutoSave();
    initializeNotifications();
    
    console.log('🐦 Pelican Farm Management System initialized');
});

// Initialize Bootstrap tooltips
function initializeTooltips() {
    const tooltipTriggerList = [].slice.call(document.querySelectorAll('[data-bs-toggle="tooltip"]'));
    tooltipTriggerList.map(function (tooltipTriggerEl) {
        return new bootstrap.Tooltip(tooltipTriggerEl);
    });
}

// Smooth animations and interactions
function initializeAnimations() {
    // Add loading states to buttons
    document.querySelectorAll('form').forEach(form => {
        form.addEventListener('submit', function(e) {
            const submitButton = form.querySelector('button[type="submit"]');
            if (submitButton) {
                const originalText = submitButton.innerHTML;
                submitButton.innerHTML = '<span class="loading"></span> Processing...';
                submitButton.disabled = true;
                
                // Re-enable after 3 seconds as fallback
                setTimeout(() => {
                    submitButton.innerHTML = originalText;
                    submitButton.disabled = false;
                }, 3000);
            }
        });
    });

    // Animate cards on page load
    const cards = document.querySelectorAll('.card');
    cards.forEach((card, index) => {
        card.style.opacity = '0';
        card.style.transform = 'translateY(20px)';
        
        setTimeout(() => {
            card.style.transition = 'all 0.5s ease';
            card.style.opacity = '1';
            card.style.transform = 'translateY(0)';
        }, index * 100);
    });

    // Add hover effects to navigation
    document.querySelectorAll('.nav-link').forEach(link => {
        link.addEventListener('mouseenter', function() {
            this.style.transform = 'translateY(-2px)';
        });
        
        link.addEventListener('mouseleave', function() {
            this.style.transform = 'translateY(0)';
        });
    });
}

// Enhanced form validation
function initializeFormValidation() {
    // Real-time validation
    document.querySelectorAll('input[required], select[required]').forEach(input => {
        input.addEventListener('blur', function() {
            validateField(this);
        });
        
        input.addEventListener('input', function() {
            if (this.classList.contains('is-invalid')) {
                validateField(this);
            }
        });
    });

    // Custom validation messages
    function validateField(field) {
        const value = field.value.trim();
        const fieldName = field.previousElementSibling.textContent.replace('*', '').trim();
        
        if (field.hasAttribute('required') && !value) {
            showFieldError(field, `${fieldName} is required`);
            return false;
        }
        
        // Specific validations
        if (field.type === 'email' && value && !isValidEmail(value)) {
            showFieldError(field, 'Please enter a valid email address');
            return false;
        }
        
        if (field.name === 'age' && value && (value < 0 || value > 360)) {
            showFieldError(field, 'Age must be between 0 and 360 months');
            return false;
        }
        
        if (field.name === 'name' && value && value.length < 2) {
            showFieldError(field, 'Name must be at least 2 characters long');
            return false;
        }
        
        showFieldSuccess(field);
        return true;
    }
    
    function showFieldError(field, message) {
        field.classList.remove('is-valid');
        field.classList.add('is-invalid');
        
        let feedback = field.parentNode.querySelector('.invalid-feedback');
        if (!feedback) {
            feedback = document.createElement('div');
            feedback.className = 'invalid-feedback';
            field.parentNode.appendChild(feedback);
        }
        feedback.textContent = message;
    }
    
    function showFieldSuccess(field) {
        field.classList.remove('is-invalid');
        field.classList.add('is-valid');
        
        const feedback = field.parentNode.querySelector('.invalid-feedback');
        if (feedback) {
            feedback.remove();
        }
    }
    
    function isValidEmail(email) {
        return /^[^\s@]+@[^\s@]+\.[^\s@]+$/.test(email);
    }
}

// Search and filter functionality
function initializeSearchAndFilters() {
    // Add search functionality to tables
    const tables = document.querySelectorAll('table');
    tables.forEach(table => {
        const searchInput = createSearchInput(table);
        if (searchInput) {
            table.parentNode.insertBefore(searchInput, table);
        }
    });
    
    function createSearchInput(table) {
        if (table.rows.length <= 2) return null; // Skip if only header + 1 row
        
        const searchContainer = document.createElement('div');
        searchContainer.className = 'mb-3';
        searchContainer.innerHTML = `
            <div class="input-group">
                <span class="input-group-text">
                    <i class="bi bi-search"></i>
                </span>
                <input type="text" class="form-control" placeholder="Search...">
            </div>
        `;
        
        const searchInput = searchContainer.querySelector('input');
        searchInput.addEventListener('input', function() {
            filterTable(table, this.value);
        });
        
        return searchContainer;
    }
    
    function filterTable(table, searchTerm) {
        const rows = table.querySelectorAll('tbody tr');
        const term = searchTerm.toLowerCase();
        
        rows.forEach(row => {
            const text = row.textContent.toLowerCase();
            const shouldShow = text.includes(term);
            row.style.display = shouldShow ? '' : 'none';
        });
        
        // Show "no results" message if needed
        const visibleRows = Array.from(rows).filter(row => row.style.display !== 'none');
        updateNoResultsMessage(table, visibleRows.length === 0 && searchTerm);
    }
    
    function updateNoResultsMessage(table, show) {
        let noResultsRow = table.querySelector('.no-results-row');
        
        if (show && !noResultsRow) {
            const colspan = table.querySelector('thead tr').children.length;
            noResultsRow = document.createElement('tr');
            noResultsRow.className = 'no-results-row';
            noResultsRow.innerHTML = `
                <td colspan="${colspan}" class="text-center py-4 text-muted">
                    <i class="bi bi-search fs-1 mb-2"></i>
                    <br>No results found
                </td>
            `;
            table.querySelector('tbody').appendChild(noResultsRow);
        } else if (!show && noResultsRow) {
            noResultsRow.remove();
        }
    }
}

// Auto-save functionality for forms
function initializeAutoSave() {
    const autoSaveKey = 'pelican-farm-autosave';
    
    // Save form data on input
    document.querySelectorAll('form input, form select, form textarea').forEach(field => {
        field.addEventListener('input', function() {
            saveFormData(this.form);
        });
    });
    
    // Restore form data on page load
    document.querySelectorAll('form').forEach(form => {
        restoreFormData(form);
    });
    
    // Clear saved data on successful submit
    document.querySelectorAll('form').forEach(form => {
        form.addEventListener('submit', function() {
            clearSavedFormData(this);
        });
    });
    
    function saveFormData(form) {
        const formId = form.id || form.action || 'default';
        const formData = new FormData(form);
        const data = {};
        
        for (let [key, value] of formData.entries()) {
            data[key] = value;
        }
        
        localStorage.setItem(`${autoSaveKey}-${formId}`, JSON.stringify(data));
    }
    
    function restoreFormData(form) {
        const formId = form.id || form.action || 'default';
        const savedData = localStorage.getItem(`${autoSaveKey}-${formId}`);
        
        if (savedData) {
            try {
                const data = JSON.parse(savedData);
                Object.keys(data).forEach(key => {
                    const field = form.querySelector(`[name="${key}"]`);
                    if (field && data[key]) {
                        field.value = data[key];
                    }
                });
            } catch (e) {
                console.warn('Failed to restore form data:', e);
            }
        }
    }
    
    function clearSavedFormData(form) {
        const formId = form.id || form.action || 'default';
        localStorage.removeItem(`${autoSaveKey}-${formId}`);
    }
}

// Enhanced notifications
function initializeNotifications() {
    // Auto-dismiss alerts after 5 seconds
    document.querySelectorAll('.alert').forEach(alert => {
        if (!alert.querySelector('.btn-close')) return;
        
        setTimeout(() => {
            const bsAlert = new bootstrap.Alert(alert);
            bsAlert.close();
        }, 5000);
    });
    
    // Show notification function
    window.showNotification = function(message, type = 'info', duration = 3000) {
        const alertContainer = document.createElement('div');
        alertContainer.className = `alert alert-${type} alert-dismissible fade show position-fixed`;
        alertContainer.style.cssText = 'top: 20px; right: 20px; z-index: 9999; min-width: 300px;';
        alertContainer.innerHTML = `
            ${message}
            <button type="button" class="btn-close" data-bs-dismiss="alert"></button>
        `;
        
        document.body.appendChild(alertContainer);
        
        setTimeout(() => {
            if (alertContainer.parentNode) {
                const bsAlert = new bootstrap.Alert(alertContainer);
                bsAlert.close();
            }
        }, duration);
    };
}

// Enhanced edit functions with better UX
function editFarm(id, name, location, description) {
    const form = document.getElementById('editFarmForm');
    form.action = '/farms/' + id;
    
    // Populate fields with animation
    const fields = [
        { id: 'editName', value: name },
        { id: 'editLocation', value: location },
        { id: 'editDescription', value: description }
    ];
    
    fields.forEach((field, index) => {
        setTimeout(() => {
            const element = document.getElementById(field.id);
            element.value = field.value;
            element.classList.add('highlight');
            setTimeout(() => element.classList.remove('highlight'), 500);
        }, index * 100);
    });
}

function editPelican(id, name, species, farmId, status, age, description) {
    const form = document.getElementById('editPelicanForm');
    form.action = '/pelicans/' + id;
    
    // Populate fields with animation
    const fields = [
        { id: 'editName', value: name },
        { id: 'editSpecies', value: species },
        { id: 'editFarmId', value: farmId },
        { id: 'editStatus', value: status },
        { id: 'editAge', value: age },
        { id: 'editDescription', value: description }
    ];
    
    fields.forEach((field, index) => {
        setTimeout(() => {
            const element = document.getElementById(field.id);
            element.value = field.value;
            element.classList.add('highlight');
            setTimeout(() => element.classList.remove('highlight'), 500);
        }, index * 100);
    });
}

// Enhanced delete functions with better confirmation
function deleteFarm(id, name) {
    showConfirmDialog(
        'Delete Farm',
        `Are you sure you want to delete farm "${name}"? This will also affect any pelicans assigned to this farm.`,
        'danger',
        () => {
            const form = document.createElement('form');
            form.method = 'POST';
            form.action = '/farms/' + id;
            
            const methodInput = document.createElement('input');
            methodInput.type = 'hidden';
            methodInput.name = '_method';
            methodInput.value = 'DELETE';
            form.appendChild(methodInput);
            
            document.body.appendChild(form);
            showNotification('Deleting farm...', 'info');
            form.submit();
        }
    );
}

function deletePelican(id, name) {
    showConfirmDialog(
        'Delete Pelican',
        `Are you sure you want to delete pelican "${name}"?`,
        'danger',
        () => {
            const form = document.createElement('form');
            form.method = 'POST';
            form.action = '/pelicans/' + id;
            
            const methodInput = document.createElement('input');
            methodInput.type = 'hidden';
            methodInput.name = '_method';
            methodInput.value = 'DELETE';
            form.appendChild(methodInput);
            
            document.body.appendChild(form);
            showNotification('Deleting pelican...', 'info');
            form.submit();
        }
    );
}

// Custom confirmation dialog
function showConfirmDialog(title, message, type, onConfirm) {
    const modal = document.createElement('div');
    modal.className = 'modal fade';
    modal.innerHTML = `
        <div class="modal-dialog">
            <div class="modal-content">
                <div class="modal-header bg-${type} text-white">
                    <h5 class="modal-title">${title}</h5>
                    <button type="button" class="btn-close btn-close-white" data-bs-dismiss="modal"></button>
                </div>
                <div class="modal-body">
                    <p>${message}</p>
                </div>
                <div class="modal-footer">
                    <button type="button" class="btn btn-secondary" data-bs-dismiss="modal">Cancel</button>
                    <button type="button" class="btn btn-${type}" id="confirmAction">Confirm</button>
                </div>
            </div>
        </div>
    `;
    
    document.body.appendChild(modal);
    const bsModal = new bootstrap.Modal(modal);
    
    modal.querySelector('#confirmAction').addEventListener('click', () => {
        bsModal.hide();
        onConfirm();
    });
    
    modal.addEventListener('hidden.bs.modal', () => {
        modal.remove();
    });
    
    bsModal.show();
}

// Add CSS class for highlight animation
const style = document.createElement('style');
style.textContent = `
    .highlight {
        background-color: rgba(13, 110, 253, 0.1) !important;
        transition: background-color 0.5s ease;
    }
`;
document.head.appendChild(style);

// Utility functions
function debounce(func, wait) {
    let timeout;
    return function executedFunction(...args) {
        const later = () => {
            clearTimeout(timeout);
            func(...args);
        };
        clearTimeout(timeout);
        timeout = setTimeout(later, wait);
    };
}

// Export functions for global use
window.PelicanFarm = {
    editFarm,
    editPelican,
    deleteFarm,
    deletePelican,
    showNotification,
    showConfirmDialog
};
