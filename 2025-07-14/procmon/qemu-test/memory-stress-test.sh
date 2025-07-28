#!/bin/bash

# Memory stress testing script for QEMU VM
# This script creates memory pressure to test thrashing detection

echo "Starting memory stress test..."
echo "This will create memory pressure to test kernel thrashing detection"

# Function to create memory pressure
create_memory_pressure() {
    local memory_mb=$1
    echo "Creating ${memory_mb}MB memory pressure..."
    
    # Use stress-ng if available, otherwise use a simple memory allocator
    if command -v stress-ng >/dev/null 2>&1; then
        stress-ng --vm 1 --vm-bytes ${memory_mb}M --timeout 60s
    else
        # Simple memory allocator in bash
        python3 -c "
import time
import sys

def allocate_memory(mb):
    print(f'Allocating {mb}MB of memory...')
    data = []
    chunk_size = 1024 * 1024  # 1MB chunks
    
    for i in range(mb):
        try:
            chunk = bytearray(chunk_size)
            # Fill with random data to prevent optimization
            for j in range(0, chunk_size, 4096):
                chunk[j] = i % 256
            data.append(chunk)
            if i % 100 == 0:
                print(f'Allocated {i}MB...')
        except MemoryError:
            print(f'Memory allocation failed at {i}MB')
            break
    
    print(f'Total allocated: {len(data)}MB')
    print('Holding memory for 60 seconds...')
    time.sleep(60)
    print('Releasing memory...')

if __name__ == '__main__':
    mb = int(sys.argv[1]) if len(sys.argv) > 1 else 512
    allocate_memory(mb)
" $memory_mb
    fi
}

# Function to create page fault pressure
create_page_fault_pressure() {
    echo "Creating page fault pressure..."
    python3 -c "
import mmap
import os
import time

def create_page_faults():
    # Create a large memory-mapped region
    size = 100 * 1024 * 1024  # 100MB
    
    # Create anonymous memory mapping
    mm = mmap.mmap(-1, size, mmap.MAP_PRIVATE | mmap.MAP_ANONYMOUS)
    
    print('Creating page faults by accessing memory pages...')
    page_size = 4096
    
    for i in range(0, size, page_size):
        try:
            # Touch each page to create page faults
            mm[i] = i % 256
            if i % (1024 * 1024) == 0:  # Print every MB
                print(f'Touched {i // (1024 * 1024)}MB')
        except:
            break
    
    print('Holding memory mapping for 30 seconds...')
    time.sleep(30)
    mm.close()
    print('Memory mapping closed')

create_page_faults()
"
}

# Function to create swap pressure
create_swap_pressure() {
    echo "Creating swap pressure..."
    # This requires swap to be enabled
    if [ ! -f /proc/swaps ] || [ $(wc -l < /proc/swaps) -le 1 ]; then
        echo "Warning: No swap detected. Creating temporary swap file..."
        sudo fallocate -l 512M /tmp/swapfile
        sudo chmod 600 /tmp/swapfile
        sudo mkswap /tmp/swapfile
        sudo swapon /tmp/swapfile
    fi
    
    # Allocate more memory than available RAM to force swapping
    available_ram=$(free -m | awk '/^Mem:/{print $7}')
    target_memory=$((available_ram + 200))  # Allocate 200MB more than available
    
    echo "Available RAM: ${available_ram}MB, targeting: ${target_memory}MB"
    create_memory_pressure $target_memory
    
    # Clean up temporary swap if we created it
    if [ -f /tmp/swapfile ]; then
        sudo swapoff /tmp/swapfile
        sudo rm -f /tmp/swapfile
    fi
}

# Function to monitor system during stress test
monitor_system() {
    echo "=== System Monitoring During Stress Test ==="
    echo "Memory info:"
    free -h
    echo
    
    echo "Swap info:"
    cat /proc/swaps
    echo
    
    echo "Memory pressure (if available):"
    if [ -f /proc/pressure/memory ]; then
        cat /proc/pressure/memory
    else
        echo "PSI not available"
    fi
    echo
    
    echo "VM stats:"
    cat /proc/vmstat | grep -E "(pgfault|pgmajfault|pswpin|pswpout)"
    echo
}

# Main test execution
main() {
    echo "Memory Stress Test for Thrashing Detection"
    echo "=========================================="
    
    monitor_system
    
    case "${1:-all}" in
        "memory")
            create_memory_pressure ${2:-512}
            ;;
        "pagefault")
            create_page_fault_pressure
            ;;
        "swap")
            create_swap_pressure
            ;;
        "all")
            echo "Running comprehensive memory stress test..."
            create_memory_pressure 300
            sleep 5
            create_page_fault_pressure
            sleep 5
            create_swap_pressure
            ;;
        *)
            echo "Usage: $0 [memory|pagefault|swap|all] [memory_mb]"
            echo "  memory    - Allocate specified amount of memory (default: 512MB)"
            echo "  pagefault - Create page fault pressure"
            echo "  swap      - Force swapping by over-allocating memory"
            echo "  all       - Run all tests sequentially"
            exit 1
            ;;
    esac
    
    echo
    echo "=== Post-test System State ==="
    monitor_system
    
    echo "Memory stress test completed."
}

# Run main function with all arguments
main "$@"

