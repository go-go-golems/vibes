/*
 * Lisp Operating System Memory Management
 * Physical and virtual memory allocation
 * Author: Manus AI
 * Date: July 13, 2025
 */

#include "kernel.h"

// Physical memory management
static uint32_t* page_bitmap = NULL;
static uint32_t total_pages = 0;
static uint32_t used_pages = 0;
static uint32_t bitmap_size = 0;

// Kernel heap management
static uint8_t* kernel_heap_start = (uint8_t*)KERNEL_HEAP_START;
static uint8_t* kernel_heap_end = (uint8_t*)(KERNEL_HEAP_START + KERNEL_HEAP_SIZE);
static uint8_t* kernel_heap_current = (uint8_t*)KERNEL_HEAP_START;

// Simple heap block header
typedef struct heap_block {
    uint32_t size;
    uint32_t used;
    struct heap_block* next;
} heap_block_t;

static heap_block_t* heap_head = NULL;

/*
 * Initialize physical memory management
 */
void init_physical_memory(memory_map_entry_t* memory_map, uint16_t num_entries)
{
    terminal_writeline("Initializing physical memory allocator...");
    
    // Find the largest usable memory region
    uint64_t max_addr = 0;
    for (int i = 0; i < num_entries; i++) {
        if (memory_map[i].type == MEMORY_TYPE_USABLE) {
            uint64_t end_addr = memory_map[i].base_addr + memory_map[i].length;
            if (end_addr > max_addr) {
                max_addr = end_addr;
            }
        }
    }
    
    // Calculate total pages and bitmap size
    total_pages = (uint32_t)(max_addr / PAGE_SIZE);
    bitmap_size = (total_pages + 31) / 32; // 32 bits per uint32_t
    
    // Place bitmap after kernel heap
    page_bitmap = (uint32_t*)(KERNEL_HEAP_START + KERNEL_HEAP_SIZE);
    
    // Initialize bitmap - mark all pages as used initially
    for (uint32_t i = 0; i < bitmap_size; i++) {
        page_bitmap[i] = 0xFFFFFFFF;
    }
    
    // Mark usable pages as free
    for (int i = 0; i < num_entries; i++) {
        if (memory_map[i].type == MEMORY_TYPE_USABLE) {
            uint32_t start_page = (uint32_t)(memory_map[i].base_addr / PAGE_SIZE);
            uint32_t num_pages = (uint32_t)(memory_map[i].length / PAGE_SIZE);
            
            for (uint32_t page = start_page; page < start_page + num_pages; page++) {
                if (page < total_pages) {
                    // Mark page as free
                    uint32_t bitmap_index = page / 32;
                    uint32_t bit_index = page % 32;
                    page_bitmap[bitmap_index] &= ~(1 << bit_index);
                }
            }
        }
    }
    
    // Mark kernel and heap areas as used
    uint32_t kernel_start_page = 0; // Kernel starts at 0x10000 (page 16)
    uint32_t kernel_end_page = ((uint32_t)page_bitmap + bitmap_size * sizeof(uint32_t)) / PAGE_SIZE + 1;
    
    for (uint32_t page = kernel_start_page; page <= kernel_end_page; page++) {
        uint32_t bitmap_index = page / 32;
        uint32_t bit_index = page % 32;
        page_bitmap[bitmap_index] |= (1 << bit_index);
        used_pages++;
    }
    
    terminal_write("Total pages: ");
    terminal_write(itoa(total_pages));
    terminal_writeline("");
    terminal_write("Used pages: ");
    terminal_write(itoa(used_pages));
    terminal_writeline("");
    terminal_write("Free pages: ");
    terminal_write(itoa(total_pages - used_pages));
    terminal_writeline("");
}

/*
 * Allocate a physical page
 */
uint32_t alloc_page(void)
{
    for (uint32_t i = 0; i < bitmap_size; i++) {
        if (page_bitmap[i] != 0xFFFFFFFF) {
            // Found a free page in this bitmap entry
            for (int bit = 0; bit < 32; bit++) {
                if (!(page_bitmap[i] & (1 << bit))) {
                    // Mark page as used
                    page_bitmap[i] |= (1 << bit);
                    used_pages++;
                    return (i * 32 + bit) * PAGE_SIZE;
                }
            }
        }
    }
    return 0; // No free pages
}

/*
 * Free a physical page
 */
void free_page(uint32_t addr)
{
    uint32_t page = addr / PAGE_SIZE;
    uint32_t bitmap_index = page / 32;
    uint32_t bit_index = page % 32;
    
    if (page_bitmap[bitmap_index] & (1 << bit_index)) {
        page_bitmap[bitmap_index] &= ~(1 << bit_index);
        used_pages--;
    }
}

/*
 * Initialize virtual memory (placeholder for now)
 */
void init_virtual_memory(void)
{
    terminal_writeline("Virtual memory system initialized (identity mapped).");
}

/*
 * Initialize kernel heap
 */
void init_kernel_heap(void)
{
    terminal_writeline("Initializing kernel heap...");
    
    // Initialize first heap block
    heap_head = (heap_block_t*)kernel_heap_start;
    heap_head->size = KERNEL_HEAP_SIZE - sizeof(heap_block_t);
    heap_head->used = 0;
    heap_head->next = NULL;
    
    kernel_heap_current = kernel_heap_start + sizeof(heap_block_t);
    
    terminal_write("Kernel heap: ");
    terminal_write(itoa(KERNEL_HEAP_SIZE));
    terminal_writeline(" bytes available");
}

/*
 * Allocate memory from kernel heap
 */
void* kmalloc(size_t size)
{
    // Align size to 4-byte boundary
    size = (size + 3) & ~3;
    
    heap_block_t* current = heap_head;
    
    while (current) {
        if (!current->used && current->size >= size) {
            // Found a suitable block
            current->used = 1;
            
            // Split block if it's much larger than needed
            if (current->size > size + sizeof(heap_block_t) + 16) {
                heap_block_t* new_block = (heap_block_t*)((uint8_t*)current + sizeof(heap_block_t) + size);
                new_block->size = current->size - size - sizeof(heap_block_t);
                new_block->used = 0;
                new_block->next = current->next;
                
                current->size = size;
                current->next = new_block;
            }
            
            return (uint8_t*)current + sizeof(heap_block_t);
        }
        current = current->next;
    }
    
    return NULL; // No suitable block found
}

/*
 * Free memory from kernel heap
 */
void kfree(void* ptr)
{
    if (!ptr) return;
    
    heap_block_t* block = (heap_block_t*)((uint8_t*)ptr - sizeof(heap_block_t));
    block->used = 0;
    
    // Coalesce with next block if it's free
    if (block->next && !block->next->used) {
        block->size += sizeof(heap_block_t) + block->next->size;
        block->next = block->next->next;
    }
    
    // Coalesce with previous block if it's free
    heap_block_t* current = heap_head;
    while (current && current->next != block) {
        current = current->next;
    }
    
    if (current && !current->used) {
        current->size += sizeof(heap_block_t) + block->size;
        current->next = block->next;
    }
}

