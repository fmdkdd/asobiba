function curry(f) {
  return curr(f, f.length);

  function curr(f, n) {
    if (n === 1)
      return f;

    else
      return function(a) {
        var g = function(/* args */) {
          var args = [].slice.call(arguments, 0);
          args.unshift(a);
          return f.apply(null, args);
        };
        return curr(g, n-1);
      };
  }
}

function add(a,b,c,d) {
  return a + b + c + d;
}

var addC = curry(add);

console.log(addC(1)(1)(1)(1));
// 4

console.log(addC(1,1)(1,1));
// [Function]

// First version requires passing the arguments one at a time.
function curry2(f) {
  return curr(f, f.length);

  function curr(f, n) {
    if (n < 1)
      return f();

    else
      return function(/* new_args */) {
        var new_args = [].slice.call(arguments, 0);
        var g = function(/* args */) {
          var args = [].slice.call(arguments, 0);
          return f.apply(null, new_args.concat(args));
        };
        return curr(g, n - new_args.length);
      };
  }
}


var addC = curry2(add);

console.log(addC(1)(1)(1)(1));
// 4

console.log(addC(1,1)(1,1));
// 4

console.log(addC(1,1)(1,1)(1,1))
// TypeError: number is not a function

// Lingering issue: Function.prototype.length is unhelpful for
// functions with multiple signatures.  But how should these be
// curried anyway?  We need a way to specify which variant of `jQuery`
// we want to call before currying it: the 1-argument version should
// be executed directly, while the 2-arguments version should not.  If
// both variants accept the same type for their first argument, how to
// know which is intended?
