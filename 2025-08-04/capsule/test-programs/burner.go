package main

import (
	"flag"
	"fmt"
	"runtime"
	"sync"
	"time"
)

func main() {
	var (
		cpuThreads = flag.Int("cpu-threads", 1, "Number of CPU threads to burn")
		memoryMB   = flag.Int("memory-mb", 100, "Amount of memory to allocate in MB")
		duration   = flag.Duration("duration", 30*time.Second, "How long to run the burner")
	)
	flag.Parse()

	fmt.Printf("Starting burner with %d CPU threads, %d MB memory for %v\n", 
		*cpuThreads, *memoryMB, *duration)
	fmt.Printf("GOMAXPROCS: %d\n", runtime.GOMAXPROCS(0))

	// Start memory allocation
	memData := make([][]byte, 0)
	allocSize := *memoryMB * 1024 * 1024 // Convert MB to bytes
	chunkSize := 1024 * 1024 // 1MB chunks
	
	fmt.Printf("Allocating %d MB of memory...\n", *memoryMB)
	for i := 0; i < allocSize; i += chunkSize {
		size := chunkSize
		if i+chunkSize > allocSize {
			size = allocSize - i
		}
		chunk := make([]byte, size)
		// Write to memory to ensure it's actually allocated
		for j := range chunk {
			chunk[j] = byte(j % 256)
		}
		memData = append(memData, chunk)
	}
	fmt.Printf("Memory allocated: %d chunks\n", len(memData))

	// Start CPU burning
	fmt.Printf("Starting %d CPU burning threads...\n", *cpuThreads)
	var wg sync.WaitGroup
	stopChan := make(chan bool)

	for i := 0; i < *cpuThreads; i++ {
		wg.Add(1)
		go func(threadID int) {
			defer wg.Done()
			counter := 0
			for {
				select {
				case <-stopChan:
					fmt.Printf("Thread %d stopped after %d iterations\n", threadID, counter)
					return
				default:
					// Burn CPU cycles
					for j := 0; j < 100000; j++ {
						counter++
					}
					// Occasionally access memory to keep it active
					if counter%1000000 == 0 {
						idx := counter % len(memData)
						if idx < len(memData) && len(memData[idx]) > 0 {
							memData[idx][0] = byte(counter % 256)
						}
					}
				}
			}
		}(i)
	}

	// Run for specified duration
	fmt.Printf("Running for %v...\n", *duration)
	time.Sleep(*duration)

	// Stop all threads
	close(stopChan)
	wg.Wait()

	fmt.Printf("Burner completed. Memory still allocated: %d MB\n", len(memData))
	
	// Keep memory allocated for a bit longer to see the effect
	time.Sleep(2 * time.Second)
	fmt.Println("Burner finished.")
}

