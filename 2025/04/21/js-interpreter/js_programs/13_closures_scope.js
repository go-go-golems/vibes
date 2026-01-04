// Closures and scope
function createCounter() {
    let count = 0;  // Private variable
    
    return {
        increment: function() {
            count++;
            return count;
        },
        decrement: function() {
            count--;
            return count;
        },
        getValue: function() {
            return count;
        }
    };
}

const counter = createCounter();
console.log("Initial value:", counter.getValue());
console.log("After increment:", counter.increment());
console.log("After increment again:", counter.increment());
console.log("After decrement:", counter.decrement());

// Another closure example
function createMultiplier(factor) {
    return function(number) {
        return number * factor;
    };
}

const double = createMultiplier(2);
const triple = createMultiplier(3);

console.log("Double 5:", double(5));
console.log("Triple 5:", triple(5));

// Demonstrating lexical scope
function outer() {
    const outerVar = "I'm from outer";
    
    function inner() {
        const innerVar = "I'm from inner";
        console.log(outerVar);  // Can access outer variable
    }
    
    inner();
    // console.log(innerVar);  // This would cause an error - innerVar not accessible here
}

outer();
