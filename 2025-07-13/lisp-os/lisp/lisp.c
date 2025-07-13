/*
 * Lisp Operating System - Lisp Interpreter
 * Core Lisp evaluation engine and runtime
 * Author: Manus AI
 * Date: July 13, 2025
 */

#include "../kernel/kernel.h"

// Lisp heap management
static lisp_object_t* lisp_heap_start = NULL;
static lisp_object_t* lisp_heap_current = NULL;
static lisp_object_t* lisp_heap_end = NULL;
static lisp_object_t* free_list = NULL;

// Symbol table
#define SYMBOL_TABLE_SIZE 1024
static lisp_object_t* symbol_table[SYMBOL_TABLE_SIZE];

// Special symbols
lisp_object_t* nil_symbol = NULL;
lisp_object_t* t_symbol = NULL;

// Input buffer for REPL
#define INPUT_BUFFER_SIZE 1024
static char input_buffer[INPUT_BUFFER_SIZE];
static int input_pos = 0;

// Function prototypes
static lisp_object_t* lisp_read_expr(const char** input);
static lisp_object_t* lisp_read_list(const char** input);
static lisp_object_t* lisp_read_atom(const char** input);
static void skip_whitespace(const char** input);
static int is_delimiter(char c);
static uint32_t hash_string(const char* str);
static lisp_object_t* lookup_symbol(const char* name);
static lisp_object_t* intern_symbol(const char* name);

/*
 * Initialize Lisp heap
 */
void init_lisp_heap(void)
{
    lisp_heap_start = (lisp_object_t*)LISP_HEAP_START;
    lisp_heap_current = lisp_heap_start;
    lisp_heap_end = (lisp_object_t*)(LISP_HEAP_START + LISP_HEAP_SIZE);
    free_list = NULL;
    
    terminal_write("Lisp heap: ");
    terminal_write(itoa(LISP_HEAP_SIZE));
    terminal_writeline(" bytes allocated");
}

/*
 * Initialize symbol table
 */
void init_symbol_table(void)
{
    // Clear symbol table
    for (int i = 0; i < SYMBOL_TABLE_SIZE; i++) {
        symbol_table[i] = NULL;
    }
    
    // Create special symbols
    nil_symbol = intern_symbol("nil");
    nil_symbol->type = LISP_NIL;
    
    t_symbol = intern_symbol("t");
    t_symbol->type = LISP_SYMBOL;
    t_symbol->data.symbol.value = t_symbol; // t evaluates to itself
    
    terminal_writeline("Symbol table initialized");
}

/*
 * Initialize evaluator and load core functions
 */
void init_evaluator(void)
{
    terminal_writeline("Loading core Lisp functions...");
    // Core functions will be loaded here
}

/*
 * Load core Lisp functions
 */
void load_core_functions(void)
{
    // Define built-in functions
    lisp_object_t* plus_sym = intern_symbol("+");
    plus_sym->data.symbol.value = lisp_alloc(LISP_BUILTIN);
    plus_sym->data.symbol.value->data.builtin.func = builtin_plus;
    
    lisp_object_t* minus_sym = intern_symbol("-");
    minus_sym->data.symbol.value = lisp_alloc(LISP_BUILTIN);
    minus_sym->data.symbol.value->data.builtin.func = builtin_minus;
    
    lisp_object_t* cons_sym = intern_symbol("cons");
    cons_sym->data.symbol.value = lisp_alloc(LISP_BUILTIN);
    cons_sym->data.symbol.value->data.builtin.func = builtin_cons;
    
    lisp_object_t* car_sym = intern_symbol("car");
    car_sym->data.symbol.value = lisp_alloc(LISP_BUILTIN);
    car_sym->data.symbol.value->data.builtin.func = builtin_car;
    
    lisp_object_t* cdr_sym = intern_symbol("cdr");
    cdr_sym->data.symbol.value = lisp_alloc(LISP_BUILTIN);
    cdr_sym->data.symbol.value->data.builtin.func = builtin_cdr;
    
    lisp_object_t* list_sym = intern_symbol("list");
    list_sym->data.symbol.value = lisp_alloc(LISP_BUILTIN);
    list_sym->data.symbol.value->data.builtin.func = builtin_list;
    
    lisp_object_t* quote_sym = intern_symbol("quote");
    quote_sym->data.symbol.value = lisp_alloc(LISP_BUILTIN);
    quote_sym->data.symbol.value->data.builtin.func = builtin_quote;
    
    lisp_object_t* help_sym = intern_symbol("help");
    help_sym->data.symbol.value = lisp_alloc(LISP_BUILTIN);
    help_sym->data.symbol.value->data.builtin.func = builtin_help;
    
    // File system functions
    lisp_object_t* file_create_sym = intern_symbol("file-create");
    file_create_sym->data.symbol.value = lisp_alloc(LISP_BUILTIN);
    file_create_sym->data.symbol.value->data.builtin.func = builtin_file_create;
    
    lisp_object_t* file_write_sym = intern_symbol("file-write");
    file_write_sym->data.symbol.value = lisp_alloc(LISP_BUILTIN);
    file_write_sym->data.symbol.value->data.builtin.func = builtin_file_write;
    
    lisp_object_t* file_list_sym = intern_symbol("file-list");
    file_list_sym->data.symbol.value = lisp_alloc(LISP_BUILTIN);
    file_list_sym->data.symbol.value->data.builtin.func = builtin_file_list;
    
    terminal_writeline("Core functions loaded");
}

/*
 * Allocate a Lisp object
 */
lisp_object_t* lisp_alloc(lisp_type_t type)
{
    lisp_object_t* obj;
    
    // Try to get from free list first
    if (free_list) {
        obj = free_list;
        free_list = free_list->next;
    } else {
        // Allocate from heap
        if (lisp_heap_current >= lisp_heap_end) {
            terminal_writeline("Error: Lisp heap exhausted!");
            return NULL;
        }
        obj = lisp_heap_current++;
    }
    
    obj->type = type;
    obj->next = NULL;
    return obj;
}

/*
 * Free a Lisp object (add to free list)
 */
void lisp_free(lisp_object_t* obj)
{
    if (obj) {
        obj->next = free_list;
        free_list = obj;
    }
}

/*
 * Create integer object
 */
lisp_object_t* lisp_make_integer(int value)
{
    lisp_object_t* obj = lisp_alloc(LISP_INTEGER);
    if (obj) {
        obj->data.integer = value;
    }
    return obj;
}

/*
 * Create symbol object
 */
lisp_object_t* lisp_make_symbol(const char* name)
{
    return intern_symbol(name);
}

/*
 * Create string object
 */
lisp_object_t* lisp_make_string(const char* str)
{
    lisp_object_t* obj = lisp_alloc(LISP_STRING);
    if (obj) {
        // Simple string storage - in a real implementation, this would be more sophisticated
        obj->data.string = (char*)str;
    }
    return obj;
}

/*
 * Create cons cell
 */
lisp_object_t* lisp_make_cons(lisp_object_t* car, lisp_object_t* cdr)
{
    lisp_object_t* obj = lisp_alloc(LISP_CONS);
    if (obj) {
        obj->data.cons.car = car;
        obj->data.cons.cdr = cdr;
    }
    return obj;
}

/*
 * Hash function for strings
 */
static uint32_t hash_string(const char* str)
{
    uint32_t hash = 5381;
    while (*str) {
        hash = ((hash << 5) + hash) + *str++;
    }
    return hash % SYMBOL_TABLE_SIZE;
}

/*
 * Look up symbol in symbol table
 */
static lisp_object_t* lookup_symbol(const char* name)
{
    uint32_t hash = hash_string(name);
    lisp_object_t* sym = symbol_table[hash];
    
    while (sym) {
        if (sym->type == LISP_SYMBOL && 
            strcmp(sym->data.symbol.name, name) == 0) {
            return sym;
        }
        sym = sym->next;
    }
    return NULL;
}

/*
 * Intern a symbol (create if doesn't exist)
 */
static lisp_object_t* intern_symbol(const char* name)
{
    lisp_object_t* existing = lookup_symbol(name);
    if (existing) {
        return existing;
    }
    
    // Create new symbol
    lisp_object_t* sym = lisp_alloc(LISP_SYMBOL);
    if (!sym) return NULL;
    
    // Store name (in a real implementation, this would be copied)
    sym->data.symbol.name = (char*)name;
    sym->data.symbol.value = nil_symbol;
    
    // Add to symbol table
    uint32_t hash = hash_string(name);
    sym->next = symbol_table[hash];
    symbol_table[hash] = sym;
    
    return sym;
}

/*
 * Simple string comparison
 */
int strcmp(const char* s1, const char* s2)
{
    while (*s1 && *s2 && *s1 == *s2) {
        s1++;
        s2++;
    }
    return *s1 - *s2;
}

/*
 * Simple string length
 */
int strlen(const char* str)
{
    int len = 0;
    while (*str++) len++;
    return len;
}

/*
 * Read Lisp expression from string
 */
lisp_object_t* lisp_read(const char* input)
{
    const char* ptr = input;
    skip_whitespace(&ptr);
    
    if (*ptr == '\0') {
        return nil_symbol;
    }
    
    return lisp_read_expr(&ptr);
}

/*
 * Read a single expression
 */
static lisp_object_t* lisp_read_expr(const char** input)
{
    skip_whitespace(input);
    
    if (**input == '(') {
        return lisp_read_list(input);
    } else if (**input == '\'') {
        (*input)++; // Skip quote
        lisp_object_t* quoted = lisp_read_expr(input);
        return lisp_make_cons(intern_symbol("quote"), 
                             lisp_make_cons(quoted, nil_symbol));
    } else {
        return lisp_read_atom(input);
    }
}

/*
 * Read a list
 */
static lisp_object_t* lisp_read_list(const char** input)
{
    (*input)++; // Skip opening paren
    skip_whitespace(input);
    
    if (**input == ')') {
        (*input)++; // Skip closing paren
        return nil_symbol;
    }
    
    lisp_object_t* car = lisp_read_expr(input);
    lisp_object_t* cdr = lisp_read_list(input);
    
    return lisp_make_cons(car, cdr);
}

/*
 * Read an atom (number or symbol)
 */
static lisp_object_t* lisp_read_atom(const char** input)
{
    const char* start = *input;
    
    // Skip to delimiter
    while (**input && !is_delimiter(**input)) {
        (*input)++;
    }
    
    int len = *input - start;
    if (len == 0) return nil_symbol;
    
    // Check if it's a number
    int is_number = 1;
    int value = 0;
    int negative = 0;
    
    if (*start == '-') {
        negative = 1;
        start++;
        len--;
    }
    
    for (int i = 0; i < len; i++) {
        if (start[i] < '0' || start[i] > '9') {
            is_number = 0;
            break;
        }
        value = value * 10 + (start[i] - '0');
    }
    
    if (is_number) {
        return lisp_make_integer(negative ? -value : value);
    } else {
        // It's a symbol - create a temporary string
        static char symbol_buffer[64];
        int copy_len = len < 63 ? len : 63;
        for (int i = 0; i < copy_len; i++) {
            symbol_buffer[i] = (negative ? start - 1 : start)[i];
        }
        symbol_buffer[copy_len] = '\0';
        return intern_symbol(symbol_buffer);
    }
}

/*
 * Skip whitespace
 */
static void skip_whitespace(const char** input)
{
    while (**input == ' ' || **input == '\t' || **input == '\n' || **input == '\r') {
        (*input)++;
    }
}

/*
 * Check if character is a delimiter
 */
static int is_delimiter(char c)
{
    return c == ' ' || c == '\t' || c == '\n' || c == '\r' || 
           c == '(' || c == ')' || c == '\0';
}

/*
 * Evaluate Lisp expression
 */
lisp_object_t* lisp_eval(lisp_object_t* expr)
{
    if (!expr || expr == nil_symbol) {
        return nil_symbol;
    }
    
    switch (expr->type) {
        case LISP_INTEGER:
        case LISP_STRING:
            return expr; // Self-evaluating
            
        case LISP_SYMBOL:
            if (expr == nil_symbol) return nil_symbol;
            if (expr == t_symbol) return t_symbol;
            return expr->data.symbol.value;
            
        case LISP_CONS:
            // Function application
            {
                lisp_object_t* func = lisp_eval(expr->data.cons.car);
                if (func && func->type == LISP_BUILTIN) {
                    return func->data.builtin.func(expr->data.cons.cdr);
                }
                return nil_symbol;
            }
            
        default:
            return nil_symbol;
    }
}

/*
 * Print Lisp object
 */
void lisp_print(lisp_object_t* obj)
{
    if (!obj || obj == nil_symbol) {
        terminal_write("nil");
        return;
    }
    
    switch (obj->type) {
        case LISP_INTEGER:
            terminal_write(itoa(obj->data.integer));
            break;
            
        case LISP_SYMBOL:
            terminal_write(obj->data.symbol.name);
            break;
            
        case LISP_STRING:
            terminal_write("\"");
            terminal_write(obj->data.string);
            terminal_write("\"");
            break;
            
        case LISP_CONS:
            terminal_write("(");
            lisp_print(obj->data.cons.car);
            lisp_object_t* cdr = obj->data.cons.cdr;
            while (cdr && cdr != nil_symbol && cdr->type == LISP_CONS) {
                terminal_write(" ");
                lisp_print(cdr->data.cons.car);
                cdr = cdr->data.cons.cdr;
            }
            if (cdr && cdr != nil_symbol) {
                terminal_write(" . ");
                lisp_print(cdr);
            }
            terminal_write(")");
            break;
            
        default:
            terminal_write("#<unknown>");
            break;
    }
}

/*
 * Main REPL function
 */
void lisp_repl(void)
{
    terminal_writeline("Lisp REPL started. Type expressions to evaluate.");
    
    while (1) {
        terminal_write("lisp> ");
        
        // Read input (simplified - in a real implementation, this would be more robust)
        input_pos = 0;
        // For now, we'll just evaluate some test expressions
        
        // Test basic arithmetic
        lisp_object_t* expr1 = lisp_read("(+ 1 2 3)");
        lisp_object_t* result1 = lisp_eval(expr1);
        lisp_print(result1);
        terminal_writeline("");
        
        // Test cons and car/cdr
        lisp_object_t* expr2 = lisp_read("(car (cons 42 nil))");
        lisp_object_t* result2 = lisp_eval(expr2);
        lisp_print(result2);
        terminal_writeline("");
        
        // Test help
        lisp_object_t* expr3 = lisp_read("(help)");
        lisp_object_t* result3 = lisp_eval(expr3);
        lisp_print(result3);
        terminal_writeline("");
        
        break; // For now, just run tests once
    }
}

