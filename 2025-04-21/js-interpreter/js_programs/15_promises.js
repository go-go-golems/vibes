// Promises and async operations
// Creating a simple promise
const simplePromise = new Promise((resolve, reject) => {
    const success = true;
    
    if (success) {
        resolve("Operation successful");
    } else {
        reject("Operation failed");
    }
});

// Using then/catch with promises
simplePromise
    .then(result => {
        console.log("Promise resolved:", result);
        return "Next step";
    })
    .then(nextResult => {
        console.log("Chain result:", nextResult);
    })
    .catch(error => {
        console.log("Promise rejected:", error);
    });

// Promise.all - waiting for multiple promises
const promise1 = Promise.resolve("First");
const promise2 = Promise.resolve("Second");
const promise3 = Promise.resolve("Third");

Promise.all([promise1, promise2, promise3])
    .then(results => {
        console.log("All promises resolved:", results);
    })
    .catch(error => {
        console.log("At least one promise rejected:", error);
    });

// Promise.race - getting the first resolved promise
Promise.race([promise1, promise2, promise3])
    .then(result => {
        console.log("First promise resolved:", result);
    })
    .catch(error => {
        console.log("First promise rejected:", error);
    });

// Simulating async operations with setTimeout
const delayedPromise = new Promise(resolve => {
    setTimeout(() => {
        resolve("Delayed operation completed");
    }, 100);
});

delayedPromise.then(result => {
    console.log(result);
});
