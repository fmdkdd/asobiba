function t() { return this; }

console.log(t() === global);
console.log(t.apply(t) === t);
console.log(t.apply(null) === global);
console.log(t.apply(this) === this);

// In Firefox, this === window, but in node, this is an empty object
// {}?

// What would you use f.apply(f) for?

console.log(Object.getOwnPropertyNames(t.apply(t)));

// [ 'length', 'name', 'arguments', 'caller', 'prototype' ]
