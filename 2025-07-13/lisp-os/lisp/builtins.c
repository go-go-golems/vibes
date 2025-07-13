/*
 * Lisp Operating System - Built-in Functions
 * Core Lisp functions and system interface
 * Author: Manus AI
 * Date: July 13, 2025
 */

#include "../kernel/kernel.h"

// External references
extern lisp_object_t* nil_symbol;
extern lisp_object_t* t_symbol;

/*
 * Addition function: (+ arg1 arg2 ...)
 */
lisp_object_t* builtin_plus(lisp_object_t* args)
{
    int sum = 0;
    lisp_object_t* current = args;
    
    while (current && current != nil_symbol && current->type == LISP_CONS) {
        lisp_object_t* arg = lisp_eval(current->data.cons.car);
        if (arg && arg->type == LISP_INTEGER) {
            sum += arg->data.integer;
        }
        current = current->data.cons.cdr;
    }
    
    return lisp_make_integer(sum);
}

/*
 * Subtraction function: (- arg1 arg2 ...)
 */
lisp_object_t* builtin_minus(lisp_object_t* args)
{
    if (!args || args == nil_symbol) {
        return lisp_make_integer(0);
    }
    
    lisp_object_t* first_arg = lisp_eval(args->data.cons.car);
    if (!first_arg || first_arg->type != LISP_INTEGER) {
        return nil_symbol;
    }
    
    int result = first_arg->data.integer;
    lisp_object_t* current = args->data.cons.cdr;
    
    // If only one argument, negate it
    if (!current || current == nil_symbol) {
        return lisp_make_integer(-result);
    }
    
    // Subtract remaining arguments
    while (current && current != nil_symbol && current->type == LISP_CONS) {
        lisp_object_t* arg = lisp_eval(current->data.cons.car);
        if (arg && arg->type == LISP_INTEGER) {
            result -= arg->data.integer;
        }
        current = current->data.cons.cdr;
    }
    
    return lisp_make_integer(result);
}

/*
 * Multiplication function: (* arg1 arg2 ...)
 */
lisp_object_t* builtin_multiply(lisp_object_t* args)
{
    int product = 1;
    lisp_object_t* current = args;
    
    while (current && current != nil_symbol && current->type == LISP_CONS) {
        lisp_object_t* arg = lisp_eval(current->data.cons.car);
        if (arg && arg->type == LISP_INTEGER) {
            product *= arg->data.integer;
        }
        current = current->data.cons.cdr;
    }
    
    return lisp_make_integer(product);
}

/*
 * Division function: (/ arg1 arg2 ...)
 */
lisp_object_t* builtin_divide(lisp_object_t* args)
{
    if (!args || args == nil_symbol) {
        return lisp_make_integer(1);
    }
    
    lisp_object_t* first_arg = lisp_eval(args->data.cons.car);
    if (!first_arg || first_arg->type != LISP_INTEGER) {
        return nil_symbol;
    }
    
    int result = first_arg->data.integer;
    lisp_object_t* current = args->data.cons.cdr;
    
    while (current && current != nil_symbol && current->type == LISP_CONS) {
        lisp_object_t* arg = lisp_eval(current->data.cons.car);
        if (arg && arg->type == LISP_INTEGER && arg->data.integer != 0) {
            result /= arg->data.integer;
        } else {
            terminal_writeline("Error: Division by zero");
            return nil_symbol;
        }
        current = current->data.cons.cdr;
    }
    
    return lisp_make_integer(result);
}

/*
 * Cons function: (cons car cdr)
 */
lisp_object_t* builtin_cons(lisp_object_t* args)
{
    if (!args || args == nil_symbol || args->type != LISP_CONS) {
        return nil_symbol;
    }
    
    lisp_object_t* car_arg = lisp_eval(args->data.cons.car);
    
    lisp_object_t* cdr_args = args->data.cons.cdr;
    if (!cdr_args || cdr_args == nil_symbol || cdr_args->type != LISP_CONS) {
        return nil_symbol;
    }
    
    lisp_object_t* cdr_arg = lisp_eval(cdr_args->data.cons.car);
    
    return lisp_make_cons(car_arg, cdr_arg);
}

/*
 * Car function: (car list)
 */
lisp_object_t* builtin_car(lisp_object_t* args)
{
    if (!args || args == nil_symbol || args->type != LISP_CONS) {
        return nil_symbol;
    }
    
    lisp_object_t* list_arg = lisp_eval(args->data.cons.car);
    
    if (!list_arg || list_arg == nil_symbol) {
        return nil_symbol;
    }
    
    if (list_arg->type == LISP_CONS) {
        return list_arg->data.cons.car;
    }
    
    return nil_symbol;
}

/*
 * Cdr function: (cdr list)
 */
lisp_object_t* builtin_cdr(lisp_object_t* args)
{
    if (!args || args == nil_symbol || args->type != LISP_CONS) {
        return nil_symbol;
    }
    
    lisp_object_t* list_arg = lisp_eval(args->data.cons.car);
    
    if (!list_arg || list_arg == nil_symbol) {
        return nil_symbol;
    }
    
    if (list_arg->type == LISP_CONS) {
        return list_arg->data.cons.cdr;
    }
    
    return nil_symbol;
}

/*
 * List function: (list arg1 arg2 ...)
 */
lisp_object_t* builtin_list(lisp_object_t* args)
{
    if (!args || args == nil_symbol) {
        return nil_symbol;
    }
    
    lisp_object_t* result = nil_symbol;
    lisp_object_t* tail = NULL;
    lisp_object_t* current = args;
    
    while (current && current != nil_symbol && current->type == LISP_CONS) {
        lisp_object_t* arg = lisp_eval(current->data.cons.car);
        lisp_object_t* new_cons = lisp_make_cons(arg, nil_symbol);
        
        if (result == nil_symbol) {
            result = new_cons;
            tail = new_cons;
        } else {
            tail->data.cons.cdr = new_cons;
            tail = new_cons;
        }
        
        current = current->data.cons.cdr;
    }
    
    return result;
}

/*
 * Quote function: (quote expr)
 */
lisp_object_t* builtin_quote(lisp_object_t* args)
{
    if (!args || args == nil_symbol || args->type != LISP_CONS) {
        return nil_symbol;
    }
    
    // Return the argument without evaluating it
    return args->data.cons.car;
}

/*
 * Help function: (help)
 */
lisp_object_t* builtin_help(lisp_object_t* args)
{
    (void)args; // Unused parameter
    
    terminal_writeline("");
    terminal_writeline("Lisp OS - Available Functions:");
    terminal_writeline("  (+  a b ...)    - Add numbers");
    terminal_writeline("  (-  a b ...)    - Subtract numbers");
    terminal_writeline("  (*  a b ...)    - Multiply numbers");
    terminal_writeline("  (/  a b ...)    - Divide numbers");
    terminal_writeline("  (cons a b)      - Create cons cell");
    terminal_writeline("  (car list)      - Get first element");
    terminal_writeline("  (cdr list)      - Get rest of list");
    terminal_writeline("  (list a b ...)  - Create list");
    terminal_writeline("  (quote expr)    - Quote expression");
    terminal_writeline("  (help)          - Show this help");
    terminal_writeline("");
    terminal_writeline("Examples:");
    terminal_writeline("  (+ 1 2 3)       => 6");
    terminal_writeline("  (cons 1 2)      => (1 . 2)");
    terminal_writeline("  (car '(a b c))  => a");
    terminal_writeline("  (list 1 2 3)    => (1 2 3)");
    terminal_writeline("");
    
    return t_symbol;
}

/*
 * System function: (system command)
 * Placeholder for system commands
 */
lisp_object_t* builtin_system(lisp_object_t* args)
{
    if (!args || args == nil_symbol || args->type != LISP_CONS) {
        return nil_symbol;
    }
    
    lisp_object_t* cmd_arg = lisp_eval(args->data.cons.car);
    
    if (cmd_arg && cmd_arg->type == LISP_SYMBOL) {
        if (strcmp(cmd_arg->data.symbol.name, "halt") == 0) {
            terminal_writeline("System halt requested.");
            cli();
            while (1) {
                asm volatile("hlt");
            }
        } else if (strcmp(cmd_arg->data.symbol.name, "memory") == 0) {
            terminal_writeline("Memory information:");
            // Add memory status display here
            return t_symbol;
        }
    }
    
    return nil_symbol;
}

/*
 * Print function: (print expr)
 */
lisp_object_t* builtin_print(lisp_object_t* args)
{
    if (!args || args == nil_symbol || args->type != LISP_CONS) {
        return nil_symbol;
    }
    
    lisp_object_t* expr = lisp_eval(args->data.cons.car);
    lisp_print(expr);
    terminal_writeline("");
    
    return expr;
}

/*
 * Equal function: (= a b)
 */
lisp_object_t* builtin_equal(lisp_object_t* args)
{
    if (!args || args == nil_symbol || args->type != LISP_CONS) {
        return nil_symbol;
    }
    
    lisp_object_t* first = lisp_eval(args->data.cons.car);
    
    lisp_object_t* rest = args->data.cons.cdr;
    if (!rest || rest == nil_symbol || rest->type != LISP_CONS) {
        return nil_symbol;
    }
    
    lisp_object_t* second = lisp_eval(rest->data.cons.car);
    
    // Compare based on type
    if (!first || !second) {
        return (first == second) ? t_symbol : nil_symbol;
    }
    
    if (first->type != second->type) {
        return nil_symbol;
    }
    
    switch (first->type) {
        case LISP_INTEGER:
            return (first->data.integer == second->data.integer) ? t_symbol : nil_symbol;
            
        case LISP_SYMBOL:
            return (first == second) ? t_symbol : nil_symbol;
            
        case LISP_NIL:
            return t_symbol;
            
        default:
            return (first == second) ? t_symbol : nil_symbol;
    }
}

/*
 * Less than function: (< a b)
 */
lisp_object_t* builtin_less_than(lisp_object_t* args)
{
    if (!args || args == nil_symbol || args->type != LISP_CONS) {
        return nil_symbol;
    }
    
    lisp_object_t* first = lisp_eval(args->data.cons.car);
    
    lisp_object_t* rest = args->data.cons.cdr;
    if (!rest || rest == nil_symbol || rest->type != LISP_CONS) {
        return nil_symbol;
    }
    
    lisp_object_t* second = lisp_eval(rest->data.cons.car);
    
    if (first && second && 
        first->type == LISP_INTEGER && 
        second->type == LISP_INTEGER) {
        return (first->data.integer < second->data.integer) ? t_symbol : nil_symbol;
    }
    
    return nil_symbol;
}

