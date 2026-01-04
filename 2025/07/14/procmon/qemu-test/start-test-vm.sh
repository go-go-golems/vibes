#!/bin/bash

# Script to start a QEMU VM for memory pressure testing
# This VM will have limited memory to make thrashing easier to trigger

VM_NAME="memory-test-vm"
VM_IMAGE="test-vm.qcow2"
VM_MEMORY="256M"  # Limited memory to trigger thrashing more easily
VM_CPUS="1"

echo "Starting QEMU VM for memory pressure testing..."
echo "VM Configuration:"
echo "  Name: $VM_NAME"
echo "  Memory: $VM_MEMORY"
echo "  CPUs: $VM_CPUS"
echo "  Image: $VM_IMAGE"
echo

# Check if VM image exists
if [ ! -f "$VM_IMAGE" ]; then
    echo "Error: VM image $VM_IMAGE not found!"
    echo "Please create the VM image first with:"
    echo "  qemu-img create -f qcow2 $VM_IMAGE 2G"
    exit 1
fi

# Start QEMU VM with limited resources
# Note: This will start a VM without an OS - for real testing, you'd need a bootable image
echo "Starting QEMU VM..."
echo "Note: This VM has no OS installed. For real testing, you would need:"
echo "1. A bootable Linux ISO or pre-installed image"
echo "2. Network configuration for SSH access"
echo "3. The procmon tool installed inside the VM"
echo

# Start VM in background with VNC display
qemu-system-x86_64 \
    -name "$VM_NAME" \
    -m "$VM_MEMORY" \
    -smp "$VM_CPUS" \
    -drive file="$VM_IMAGE",format=qcow2 \
    -netdev user,id=net0,hostfwd=tcp::2222-:22 \
    -device e1000,netdev=net0 \
    -vnc :1 \
    -daemonize \
    -pidfile vm.pid

if [ $? -eq 0 ]; then
    echo "VM started successfully!"
    echo "VM PID: $(cat vm.pid)"
    echo "VNC display: localhost:5901"
    echo "SSH port forwarding: localhost:2222 -> VM:22"
    echo
    echo "To connect via VNC: vncviewer localhost:5901"
    echo "To stop VM: kill $(cat vm.pid)"
    echo
    echo "For memory pressure testing on the host system instead,"
    echo "you can run the memory stress test directly:"
    echo "  ./memory-stress-test.sh all"
else
    echo "Failed to start VM!"
    exit 1
fi

