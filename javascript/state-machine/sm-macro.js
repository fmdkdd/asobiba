// var transitions = {};

// String.prototype.to = function(toState) {
// 	var fromState = this;
// 	addTransition(fromState, toState);
// 	return toState;
// };

// var addTransition = function(from, to) {
// 	if (!transitions[from])
// 		transitions[from] = [];
// 	transitions[from].push(to);
// };

// Red -> Green -> Orange -> Red
// * -> Blinking orange -> Red

// macro t {
// 	case ( $from -> $to ) => { $from.to($to); }
// }

// macro mt {
// 	case ( $a ) => { $a }
// 	//case ( $a -> $b -> $c ) => { t($a -> $b) t($b -> $c) }
// 	case ( $a -> $rest ) => { t($a -> mt($rest)) }
// }

// macro mtc {
// 	case ( $a ) => { [$a] }
// 	//case ( $a -> $b -> $c ) => { t($a -> $b) t($b -> $c) }
// 	case ( $a -> $rest (->) ... ) => { [$a mtc ($rest ...)] }
// }

// mtc (red green)
// mtc (red green orange)
// mtc (red green orange red)

// mtc (red -> green)
// mtc (red -> green -> orange)
// mtc (red -> green -> orange -> red)

var spec = 'red -> green -> orange -> red\n* -> blinking orange -> red';
var identifier = /(([a-z]+( [a-z]+)?)|\*)/g;
var arrow = / -> ('(([a-z]+( [a-z]+)?)|\*)')/g;

var stringed = spec.replace(identifier, "'$1'");
var end = stringed.replace(arrow, '.to($1)');
console.log(stringed);
console.log(end);
