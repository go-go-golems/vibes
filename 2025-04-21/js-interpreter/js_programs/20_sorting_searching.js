// Sorting and searching algorithms
// Sample data
const unsortedArray = [64, 34, 25, 12, 22, 11, 90];
console.log("Original array:", unsortedArray);

// Bubble sort implementation
function bubbleSort(arr) {
    const n = arr.length;
    const result = [...arr]; // Create a copy to avoid modifying original
    
    for (let i = 0; i < n - 1; i++) {
        for (let j = 0; j < n - i - 1; j++) {
            if (result[j] > result[j + 1]) {
                // Swap elements
                [result[j], result[j + 1]] = [result[j + 1], result[j]];
            }
        }
    }
    
    return result;
}

const bubbleSorted = bubbleSort(unsortedArray);
console.log("Bubble sorted:", bubbleSorted);

// Quick sort implementation
function quickSort(arr) {
    if (arr.length <= 1) {
        return arr;
    }
    
    const result = [...arr]; // Create a copy
    const pivot = result[Math.floor(result.length / 2)];
    const left = [];
    const right = [];
    const equal = [];
    
    for (const val of result) {
        if (val < pivot) {
            left.push(val);
        } else if (val > pivot) {
            right.push(val);
        } else {
            equal.push(val);
        }
    }
    
    return [...quickSort(left), ...equal, ...quickSort(right)];
}

const quickSorted = quickSort(unsortedArray);
console.log("Quick sorted:", quickSorted);

// Binary search (on sorted array)
function binarySearch(arr, target) {
    let left = 0;
    let right = arr.length - 1;
    
    while (left <= right) {
        const mid = Math.floor((left + right) / 2);
        
        if (arr[mid] === target) {
            return mid; // Found the target
        }
        
        if (arr[mid] < target) {
            left = mid + 1; // Search in the right half
        } else {
            right = mid - 1; // Search in the left half
        }
    }
    
    return -1; // Target not found
}

const searchTarget = 25;
const searchResult = binarySearch(quickSorted, searchTarget);
console.log(`Binary search for ${searchTarget}:`, searchResult !== -1 ? `Found at index ${searchResult}` : "Not found");

// Linear search (works on unsorted arrays)
function linearSearch(arr, target) {
    for (let i = 0; i < arr.length; i++) {
        if (arr[i] === target) {
            return i;
        }
    }
    return -1;
}

const linearSearchResult = linearSearch(unsortedArray, searchTarget);
console.log(`Linear search for ${searchTarget}:`, linearSearchResult !== -1 ? `Found at index ${linearSearchResult}` : "Not found");
