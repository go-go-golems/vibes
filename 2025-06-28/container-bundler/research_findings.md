# Container-to-Binary Research Findings

## OCI Runtime Implementations

Based on the official OCI runtime spec implementations list:

### Low-Level Container Runtimes
- **runc** - Reference implementation of OCI runtime (Go)
- **crun** - Runtime implementation in C (containers/crun)
- **youki** - Runtime implementation in Rust (containers/youki)
- **systemd-nspawn** - Runtime implementation in C (via --oci-bundle option)

### Hypervisor-Based Runtimes
- **kata-containers** - Hypervisor-based OCI runtime
- **gVisor/runsc** - User-space kernel with sandboxed containers
- **runv** - Hypervisor-based runtime for OCI

### Static Binary Options
- **podman-static** - Static binaries available from mgoltzsche/podman-static
- **runc static** - Static binaries available from official releases

## Key Insights So Far
1. OCI runtimes are designed to be minimal and focused on container execution
2. Static binaries are available for some runtimes (runc, podman)
3. Container images can be embedded using Go's embed directive
4. Need to investigate Go libraries for working with container images and OCI bundles

## Next Steps
- Investigate Go libraries for container image manipulation
- Look into embedding container images in Go binaries
- Research existing solutions for bundling containers into single executables



## Existing Container-to-Binary Solutions

### 1. dockerc (NilsIrl/dockerc) - 4.6k stars
**Approach**: Compile Docker images into standalone portable binaries
- **Language**: Zig (98.4%), C (1.6%)
- **Features**:
  - Rootless containers
  - Cross-platform support (MacOS, Windows via QEMU)
  - Multi-arch support (x86_64, arm64)
  - No Docker daemon required for execution
  - Direct network access without port mapping
  - Supports container arguments (-e, -v)
- **How it works**: Uses crun (OCI runtime), fuse-overlayfs, squashfs, and other tools embedded in binary
- **Usage**: `dockerc --image docker://oven/bun --output bun`
- **Dependencies**: Custom patched Zig compiler

### 2. docker2exe (rzane/docker2exe) - 1.6k stars  
**Approach**: Convert Docker images to executables that still require Docker
- **Language**: Go
- **Features**:
  - Cross-platform binaries (Linux, macOS, Windows)
  - Two modes: pull image on demand or embed image in binary
  - Embedded mode creates self-contained executables
- **How it works**: 
  - Embeds compressed Docker image tarball in executable
  - On execution, checks if image exists locally
  - If not found, extracts and loads embedded image via `docker load`
- **Limitation**: Still requires Docker daemon to be installed
- **Usage**: `docker2exe --name alpine --image alpine:3.9 --embed`

### 3. chainguard-dev/kontext
**Approach**: Library for building self-extracting container images
- **Status**: Recently created (Oct 2024)
- **Focus**: Self-extracting container images

## Go Libraries for Container Manipulation

### 1. google/go-containerregistry - 3.4k stars
**Purpose**: Go library for working with container registries and images
- **Features**:
  - Immutable views of Images, Layers, ImageIndex
  - Support for multiple backends (registry, tarball, daemon)
  - Functional mutations via mutate package
  - Tools: crane, gcrane, krane
- **Key packages**: 
  - `pkg/v1` - Core interfaces
  - `pkg/v1/remote` - Registry operations
  - `pkg/v1/tarball` - Tarball operations
  - `pkg/v1/mutate` - Image mutations

### 2. containers/image/v5
**Purpose**: Library for pulling and pushing images from container registries
- **Features**: Works with docker.io, quay.io and other registries
- **Integration**: Used by Podman, Skopeo, and other container tools

## Technical Approaches Identified

### Approach 1: Embed Runtime + Image (dockerc style)
- Bundle OCI runtime (crun/runc) + container image + filesystem tools
- Create single executable that can run containers without Docker
- Requires significant binary size but fully self-contained

### Approach 2: Embed Image + Require Docker (docker2exe style)  
- Embed compressed container image in Go binary
- Extract and load image using Docker daemon on execution
- Smaller implementation but requires Docker installation

### Approach 3: Pure Go Implementation
- Use Go libraries to manipulate container images
- Implement minimal container runtime in Go
- Bundle everything into single Go binary using embed directive

