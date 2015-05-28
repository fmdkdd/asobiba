// Extend String prototype without affecting the global scope

// Method #1: non-enumerable property of String.prototype
// Unsatisfying: still affects the global scope -- wrong, can remove
// property after use, but still verbose

function method1() {

  Object.defineProperty(String.prototype, 'padAround', {
    value: function(regex) {
	    return this.replace(regex, ' $1 ');
    },
    configurable: true,
    enumerable: false,
    writeable: true,
  });

  Object.defineProperty(String.prototype, 'collapseWhitespace', {
    value: function() {
	    return this.replace(/\s+/g, ' ');
    },
    configurable: true,
    enumerable: false,
    writeable: true,
  });

  Object.defineProperty(String.prototype, 'normalizeWhitespace', {
    value: function() {
	    return this.replace(/[\f\n\r\t\v]/g, ' ');
    },
    configurable: true,
    enumerable: false,
    writeable: true,
  });

  // Test

  var operators = /(:=|=|λ)/g;
  var separators = /(;|\(|\)|\[|\]|\.)/g;

  var source = '[(λx.[y z])e]';

  var words = source
	  .padAround(separators)
	  .padAround(operators)
	  .normalizeWhitespace()
	  .trim()
    .collapseWhitespace()
	  .split(' ');

  console.log(words);

}

// Method #2: wrap primitives by extending String
// Not working: toString is not generic :'(
// Cons: needs explicit wrapping to SourceString

function method2() {

  function toSourceString(str) {
    var ss = Object.create(new String(str));
    ss.padAround = function(regex) {
	    return this.replace(regex, ' $1 ');
    };

    ss.collapseWhitespace = function() {
      return this.replace(/\s+/g, ' ');
    };

    ss.normalizeWhitespace = function() {
      return this.replace(/[\f\n\r\t\v]/g, ' ');
    };

    return ss;
  }

  // Test

  var operators = /(:=|=|λ)/g;
  var separators = /(;|\(|\)|\[|\]|\.)/g;

  var source = '[(λx.[y z])e]';

  var words = toSourceString(source)
	  .padAround(separators)
	  .padAround(operators)
	  .normalizeWhitespace()
	  .trim()
    .collapseWhitespace()
	  .split(' ');

  console.log(words);

}

// Method #3: delegation
// Working!
// Cons: verbose, have to delegate all methods
// Cons: have to explicitly wrap strings and unwrap sourceStrings

function method3() {

  function toSourceString(str) {
    var ss = Object.create(new String(str));
    ss.padAround = function(regex) {
	    return toSourceString(str.replace(regex, ' $1 '));
    };

    ss.collapseWhitespace = function() {
      return toSourceString(str.replace(/\s+/g, ' '));
    };

    ss.normalizeWhitespace = function() {
      return toSourceString(str.replace(/[\f\n\r\t\v]/g, ' '));
    };

    ss.toString = function() {
      return toSourceString(String.prototype.toString.apply(str, arguments));
    };

    ss.valueOf = function() {
      return toSourceString(String.prototype.valueOf.apply(str, arguments));
    };

    ss.trim = function() {
      return toSourceString(String.prototype.trim.apply(str, arguments));
    };

    ss.split = function() {
      return toSourceString(String.prototype.split.apply(str, arguments));
    };

    ss.raw = function() {
      return str;
    };

    return ss;
  }

  // Test

  var operators = /(:=|=|λ)/g;
  var separators = /(;|\(|\)|\[|\]|\.)/g;

  var source = '[(λx.[y z])e]';

  var words = toSourceString(source)
	  .padAround(separators)
	  .padAround(operators)
	  .normalizeWhitespace()
	  .trim()
    .collapseWhitespace()
	  .split(' ')
    .raw();

  console.log(words);

}


// Method #4: intercede the prototype!
// Ok not really.  I tried the following proto chain:
// source (string litteral) -> SourceString -> String.prototype
// But that fails once a method from String.prototype is called and
// returns a brand new String with a clean proto
//
// So, this works
// source -> String.prototype -> SourceString -> Object.prototype
// And is essentially the same as #1 but much less verbose and can
// be activated/deactivated in one line.
// Pros: no wrapping since string litterals have String.prototype as
// __proto__
// Cons: still not local, can potentially conflict with other such extensions

function method4() {

  var SourceString = {
    padAround: function(regex) {
	    return this.replace(regex, ' $1 ');
    },

    collapseWhitespace: function() {
      return this.replace(/\s+/g, ' ');
    },

    normalizeWhitespace: function() {
      return this.replace(/[\f\n\r\t\v]/g, ' ');
    },
  };

  // String.prototype.__proto__ = SourceString;

  // Better yet:

  SourceString.__proto__ = String.prototype.__proto__;
  String.prototype.__proto__ = SourceString;

  // Can then chain such extensions:
  // String.prototype -> Object.prototype
  // String.prototype -> SourceString -> Object.prototype
  // String.prototype -> SourceString -> MyString -> Object.prototype

  // Test

  var operators = /(:=|=|λ)/g;
  var separators = /(;|\(|\)|\[|\]|\.)/g;

  var source = '[(λx.[y z])e]';

  var words = source
	  .padAround(separators)
	  .padAround(operators)
	  .normalizeWhitespace()
	  .trim()
    .collapseWhitespace()
	  .split(' ');

  console.log(words);

  // Restore proto

  console.log('bleh'.padAround);

  String.prototype.__proto__ = SourceString.__proto__;

  console.log('bleh'.padAround);
}

// Here is method #4 reified

function method5() {

  function insertPrototype(obj, prototype) {
    prototype.__proto__ = obj.__proto__;
    obj.__proto__ = prototype;
    return function undo() {
      obj.__proto__ = prototype.__proto__;
    };
  }

  var SourceString = {
    padAround: function(regex) {
	    return this.replace(regex, ' $1 ');
    },

    collapseWhitespace: function() {
      return this.replace(/\s+/g, ' ');
    },

    normalizeWhitespace: function() {
      return this.replace(/[\f\n\r\t\v]/g, ' ');
    },
  };

  var deleteProto = insertPrototype(String.prototype, SourceString);

  // Test

  var operators = /(:=|=|λ)/g;
  var separators = /(;|\(|\)|\[|\]|\.)/g;

  var source = '[(λx.[y z])e]';

  var words = source
	  .padAround(separators)
	  .padAround(operators)
	  .normalizeWhitespace()
	  .trim()
    .collapseWhitespace()
	  .split(' ');

  console.log(words);

  // Restore proto

  console.log('bleh'.padAround);

  deleteProto();

  console.log('bleh'.padAround);
}

// Method #6: subclassing
// And now I find out that it's a known pattern called "subclassing".
// Let's try it out, from http://www.2ality.com/2012/10/proto.html
//
// Aaaaaaaand it doesn't work because replace returns plain String
// objects.  This is then similar to method #2, except extended
// methods are placed on a shared prototype (better perf).
function method6() {

  var myStringProto = Object.create(String.prototype);
  myStringProto.padAround = function(regex) {
	  return this.replace(regex, ' $1 ');
  };

  myStringProto.collapseWhitespace = function() {
    return this.replace(/\s+/g, ' ');
  };

  myStringProto.normalizeWhitespace = function() {
    return this.replace(/[\f\n\r\t\v]/g, ' ');
  };

  function createMyString(str) {
	  var s = new String(str);
    s.__proto__ = myStringProto;
    return s;
  }

  // Test

  var operators = /(:=|=|λ)/g;
  var separators = /(;|\(|\)|\[|\]|\.)/g;

  var source = '[(λx.[y z])e]';

  var words = createMyString(source)
	  .padAround(separators)
	  .padAround(operators)
	  .normalizeWhitespace()
	  .trim()
    .collapseWhitespace()
	  .split(' ');

  console.log(words);

  // Restore proto

  console.log('bleh'.padAround);

  deleteProto();

  console.log('bleh'.padAround);
}

method6();
