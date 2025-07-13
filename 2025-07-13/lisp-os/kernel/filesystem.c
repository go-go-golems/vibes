/*
 * Lisp Operating System - File System
 * Simple in-memory file system for demonstration
 * Author: Manus AI
 * Date: July 13, 2025
 */

#include "kernel.h"

// File system constants
#define MAX_FILES 64
#define MAX_FILENAME 32
#define MAX_FILE_SIZE 4096

// File structure
typedef struct {
    char name[MAX_FILENAME];
    uint8_t* data;
    uint32_t size;
    uint32_t allocated_size;
    int in_use;
} file_t;

// File system state
static file_t files[MAX_FILES];
static int fs_initialized = 0;

/*
 * Initialize file system
 */
void init_filesystem(void)
{
    if (fs_initialized) return;
    
    // Clear file table
    for (int i = 0; i < MAX_FILES; i++) {
        files[i].in_use = 0;
        files[i].data = NULL;
        files[i].size = 0;
        files[i].allocated_size = 0;
        files[i].name[0] = '\0';
    }
    
    fs_initialized = 1;
    terminal_writeline("File system initialized");
}

/*
 * Find file by name
 */
static int find_file(const char* filename)
{
    for (int i = 0; i < MAX_FILES; i++) {
        if (files[i].in_use && strcmp(files[i].name, filename) == 0) {
            return i;
        }
    }
    return -1;
}

/*
 * Find free file slot
 */
static int find_free_slot(void)
{
    for (int i = 0; i < MAX_FILES; i++) {
        if (!files[i].in_use) {
            return i;
        }
    }
    return -1;
}

/*
 * Create a new file
 */
int fs_create(const char* filename)
{
    if (!fs_initialized) init_filesystem();
    
    // Check if file already exists
    if (find_file(filename) >= 0) {
        return -1; // File already exists
    }
    
    // Find free slot
    int slot = find_free_slot();
    if (slot < 0) {
        return -1; // No free slots
    }
    
    // Initialize file
    files[slot].in_use = 1;
    files[slot].size = 0;
    files[slot].allocated_size = 0;
    files[slot].data = NULL;
    
    // Copy filename
    int name_len = strlen(filename);
    if (name_len >= MAX_FILENAME) {
        name_len = MAX_FILENAME - 1;
    }
    
    for (int i = 0; i < name_len; i++) {
        files[slot].name[i] = filename[i];
    }
    files[slot].name[name_len] = '\0';
    
    return slot;
}

/*
 * Open existing file
 */
int fs_open(const char* filename)
{
    if (!fs_initialized) init_filesystem();
    
    return find_file(filename);
}

/*
 * Write data to file
 */
int fs_write(int file_handle, const void* data, uint32_t size)
{
    if (file_handle < 0 || file_handle >= MAX_FILES || !files[file_handle].in_use) {
        return -1;
    }
    
    file_t* file = &files[file_handle];
    
    // Check if we need to allocate or reallocate memory
    if (size > file->allocated_size) {
        uint32_t new_size = size;
        if (new_size > MAX_FILE_SIZE) {
            new_size = MAX_FILE_SIZE;
        }
        
        uint8_t* new_data = (uint8_t*)kmalloc(new_size);
        if (!new_data) {
            return -1; // Allocation failed
        }
        
        // Copy existing data if any
        if (file->data && file->size > 0) {
            uint32_t copy_size = file->size < new_size ? file->size : new_size;
            for (uint32_t i = 0; i < copy_size; i++) {
                new_data[i] = file->data[i];
            }
            kfree(file->data);
        }
        
        file->data = new_data;
        file->allocated_size = new_size;
    }
    
    // Write data
    uint32_t write_size = size < file->allocated_size ? size : file->allocated_size;
    const uint8_t* src = (const uint8_t*)data;
    
    for (uint32_t i = 0; i < write_size; i++) {
        file->data[i] = src[i];
    }
    
    file->size = write_size;
    return write_size;
}

/*
 * Read data from file
 */
int fs_read(int file_handle, void* buffer, uint32_t size)
{
    if (file_handle < 0 || file_handle >= MAX_FILES || !files[file_handle].in_use) {
        return -1;
    }
    
    file_t* file = &files[file_handle];
    
    if (!file->data || file->size == 0) {
        return 0; // Empty file
    }
    
    uint32_t read_size = size < file->size ? size : file->size;
    uint8_t* dest = (uint8_t*)buffer;
    
    for (uint32_t i = 0; i < read_size; i++) {
        dest[i] = file->data[i];
    }
    
    return read_size;
}

/*
 * Get file size
 */
uint32_t fs_size(int file_handle)
{
    if (file_handle < 0 || file_handle >= MAX_FILES || !files[file_handle].in_use) {
        return 0;
    }
    
    return files[file_handle].size;
}

/*
 * Close file (no-op in this simple implementation)
 */
void fs_close(int file_handle)
{
    // In a real file system, this would flush buffers, etc.
    (void)file_handle;
}

/*
 * Delete file
 */
int fs_delete(const char* filename)
{
    int file_handle = find_file(filename);
    if (file_handle < 0) {
        return -1; // File not found
    }
    
    file_t* file = &files[file_handle];
    
    // Free allocated memory
    if (file->data) {
        kfree(file->data);
    }
    
    // Mark slot as free
    file->in_use = 0;
    file->data = NULL;
    file->size = 0;
    file->allocated_size = 0;
    file->name[0] = '\0';
    
    return 0;
}

/*
 * List files
 */
void fs_list(void)
{
    if (!fs_initialized) init_filesystem();
    
    terminal_writeline("Files:");
    
    int count = 0;
    for (int i = 0; i < MAX_FILES; i++) {
        if (files[i].in_use) {
            terminal_write("  ");
            terminal_write(files[i].name);
            terminal_write(" (");
            terminal_write(itoa(files[i].size));
            terminal_writeline(" bytes)");
            count++;
        }
    }
    
    if (count == 0) {
        terminal_writeline("  No files");
    }
    
    terminal_write("Total files: ");
    terminal_write(itoa(count));
    terminal_write("/");
    terminal_write(itoa(MAX_FILES));
    terminal_writeline("");
}

/*
 * Lisp interface functions
 */

// External symbol references
extern lisp_object_t* nil_symbol;
extern lisp_object_t* t_symbol;

/*
 * (file-create "filename")
 */
lisp_object_t* builtin_file_create(lisp_object_t* args)
{
    if (!args || args == nil_symbol || args->type != LISP_CONS) {
        return nil_symbol;
    }
    
    lisp_object_t* filename_arg = lisp_eval(args->data.cons.car);
    if (!filename_arg || filename_arg->type != LISP_STRING) {
        return nil_symbol;
    }
    
    int result = fs_create(filename_arg->data.string);
    return result >= 0 ? t_symbol : nil_symbol;
}

/*
 * (file-write "filename" "content")
 */
lisp_object_t* builtin_file_write(lisp_object_t* args)
{
    if (!args || args == nil_symbol || args->type != LISP_CONS) {
        return nil_symbol;
    }
    
    lisp_object_t* filename_arg = lisp_eval(args->data.cons.car);
    if (!filename_arg || filename_arg->type != LISP_STRING) {
        return nil_symbol;
    }
    
    lisp_object_t* content_args = args->data.cons.cdr;
    if (!content_args || content_args == nil_symbol || content_args->type != LISP_CONS) {
        return nil_symbol;
    }
    
    lisp_object_t* content_arg = lisp_eval(content_args->data.cons.car);
    if (!content_arg || content_arg->type != LISP_STRING) {
        return nil_symbol;
    }
    
    // Open or create file
    int file_handle = fs_open(filename_arg->data.string);
    if (file_handle < 0) {
        file_handle = fs_create(filename_arg->data.string);
    }
    
    if (file_handle < 0) {
        return nil_symbol;
    }
    
    // Write content
    int result = fs_write(file_handle, content_arg->data.string, strlen(content_arg->data.string));
    fs_close(file_handle);
    
    return result >= 0 ? t_symbol : nil_symbol;
}

/*
 * (file-list)
 */
lisp_object_t* builtin_file_list(lisp_object_t* args)
{
    (void)args; // Unused
    
    fs_list();
    return t_symbol;
}

