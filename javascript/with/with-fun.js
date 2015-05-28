function f(x,y) {
  console.log(x,y,z);
}

// with ({z: 3}) {
//   f(1,2);
// }
// ReferenceError: z is not defined

eval(f.toString().replace(/(console\.log\(x,y,z\);)/, 'with({z:3}) {$1}'));

console.log(f.toString());

f(1,2);
