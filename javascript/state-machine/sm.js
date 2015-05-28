// Street light as a State Machine

// Red -> Green -> Orange -> Red
// * -> Blinking orange -> Red

// Naive example 1: if/else cascade

var state;

// Behavior for each state
var print = function() {
	if (state == 'red')
		console.log('Stop!');
	else if (state == 'green')
		console.log('Pass.');
	else if (state == 'orange')
		console.log('Speed up!');
	else if (state == 'blinking orange')
		console.log('Pass carefully.');
}

state = 'green';
print();

state = 'orange';
print();

state = 'red';
print();

// Transition links are not enforced
// Can do the following

state = 'green';
print();

state = 'red';
print();

// Must define valid transitions first

var transitionTo = function(newState) {
	if (state == 'red'
		 && (newState == 'green'
			  || newState == 'blinking orange'))
		state = newState;
	else if (state == 'green'
			  && (newState == 'orange'
					|| newState == 'blinking orange'))
		state = newState;
	else if (state == 'orange'
				&& (newState == 'red'
					|| newState == 'blinking orange'))
		state = newState;
	else if (state == 'blinking orange'
			  && (newState == 'red'))
		state = newState;
	else
		console.error('Illegal transition', state, 'to', newState);
}

// We still need to initialize `state`

function init() {
	state = 'red';
}

init();
print();

transitionTo('green');
print();

transitionTo('orange');
print();

transitionTo('blinking orange');
print();

// Invalid transitions are taken care off

init();
print();

transitionTo('orange');
print();

// At this point I'm not satisfied with the expression of my state
// machine.  There are many repetitions of the form `if/else -> state
// = newState` in the `transitionTo` function.  The control flow is
// the same in each `if`: it depends only on the actual transitions my
// state machine is composed of.
//
// If I extract the variable parts, I get a generic transition
// function that I can reuse for other state machines.

var transitions = {
	'red': ['green', 'blinking orange'],
	'green': ['orange', 'blinking orange'],
	'orange': ['red', 'blinking orange'],
	'blinking orange': ['red'],
};

var transitionTo = function(newState) {
	if (transitions[state].indexOf(newState) > -1)
		state = newState;
	else
		console.error('Illegal transition', state, 'to', newState);
}

init();
print();

transitionTo('green');
print();

transitionTo('orange');
print();

init();
print();

transitionTo('orange');
print();

// That's much better.  Now if I need another state machine, I just
// have to adjust the `transitions` variable describing the
// transitions, not the `transitionTo` function.  I encoded the
// transitions using a data structure rather than control flow, but
// the relations between the states are the same.
//
// The advantage here is that I expressed my intentions more clearly
// using an easy-to-read data structure rather than repeating code.
// However, there is a drawback.  Data structures are static by
// nature, while code is dynamic.  Code can easily be changed to
// accomodate new requirements, while the relations that data
// structures describe are immutable.  The evolution of any software
// system is going from a vibrant, dynamic state where everything is
// possible but intent is not clear, to a cooler, static state where
// intent is clearly expressed, but where modifications are hard to
// accomodate with few changes.

// I can even better express my intentions with the following.

var transitionTo = function(newState) {
	if (legalTransition(state, newState))
		state = newState;
	else
		console.error('Illegal transition', state, 'to', newState);
}

var legalTransition = function(from, to) {
	return transitions[from].indexOf(to) > -1;
}

// The `transitionTo` function does not need to know that we use
// objects and arrays.  It just checks whether the transition is
// legal.

// And even further, we can more accurately say

var legalTransition = function(from, to) {
	return transitions[from].contains(to);
}

// If we define `contains` on Array.

Array.prototype.contains = function(element) {
	return this.indexOf(element) > -1;
}

// There is one additional repetition I'd like to get rid of
// concerning the 'blinking orange' state.  All states can lead to the
// blinking orange (except for blinking orange itself, as we are
// ignoring transitions to self for now).  If I had a hundred states,
// I would have to write explicitely 'blinking orange' a hundred
// times.  That's not right.

// I have more than one way to solve this issue.  As we saw, a
// transition is defined by its presence in the `transitions` object,
// but also by its use in the `legalTransition` function.  Both act in
// concert to express the concept of a legal transition in code.
// So I could modify the `transitions` object, or I could modify the
// `legalTransition` function to express the fact that all states can
// transit to 'blinking orange'.

// The simpler solution would be :

var transitions = {
	'red': ['green'],
	'green': ['orange'],
	'orange': ['red'],
	'blinking orange': ['red'],
};

var legalTransition = function(from, to) {
	return transitions[from].contains(to)
		|| to == 'blinking orange';
};

// That's the dynamic solution.  I can express anything in code, but
// the intent is not very clear.  I have constants mixed with
// variables, but constants should stay with data.  Now if I want to
// use my code with another state machine I have to change the
// `legalTransition` function as well, as it has become specific to my
// street light state machine.
//
// In a software system, we aim to minimize the places in the code we
// have to change to accomodate new requirements.  Fewer places to
// change entails faster effective change, and fewer potential
// mistakes.
//
// So, if I want the `legalTransition` function to be generic, I must
// change only the `transitions` object.

var transitions = {
	'red': ['green'],
	'green': ['orange'],
	'orange': ['red'],
	'blinking orange': ['red'],
};

transitions.forEach(function(from) {
	from.push('blinking orange');
});

var legalTransition = function(from, to) {
	return transitions[from].contains(to);
};

// Great, I modified only the `transitions` object to eliminate the
// repetition.  My structure is static, but I can modify it with code.
//
// Again, I recognize that the code can straightforwardly be
// generalized to add a 'well' transition.

var transitions = {
	'red': ['green'],
	'green': ['orange'],
	'orange': ['red'],
	'blinking orange': ['red'],
};

addWellTransition('blinking orange');

var addWellTransition = function(well) {
	transitions.forEach(function(from) {
		from.push(well);
	});
};

// And now we can easily add other 'well' transitions if we wanted to,
// with the code clearly expressing our intent.  The `legalTransition`
// and `transitionTo` functions are still generic, and we did not need
// to look at them to accomodate these changes.

// There is however a difference between the two versions.  The static
// one will add the well state to all transitions that existed before
// the call to `addWellTransition`.  The dynamic solution will work
// for all transitions that exist when `legalTransition` is called; it
// will still be correct regardless of the order the transitions are
// defined in.  But, in the static solution, you must define 'well'
// transitions after all the other transitions are defined.  What
// happens with two well transitions?

addWellTransition('blinking orange');
addWellTransition('blinking blue');

// Which should translate to
//
// * -> Blinking orange
// * -> Blinking blue
//
// So, in particular we are saying that we have transitions:
//
// Blinking orange -> Blinking Blue
// Blinking blue -> Blinking orange
//
// But that is not the case with `addWellTransition`.  We get:

transitions =
{ red:
   [ 'green',
     'blinking orange',
     'blinking blue' ],
  green:
   [ 'orange',
     'blinking orange',
     'blinking blue' ],
  orange:
   [ 'red',
     'blinking orange',
     'blinking blue' ],
  'blinking orange':
   [ 'red',
     'blinking orange',
     'blinking blue' ] }

// We are missing the transition
//
// Blinking blue -> Blinking orange
//
// Because the state 'blinking blue' was not declared when adding the
// well transition to 'blinking orange', so this transition can not be
// added statically.  `addWellTransition` correctly handles existing
// states, but does not account for any state declared after its call.

// We could declare all states before defining transitions, but that
// would lead to duplications in the specification for the state
// machine.  Instead, the preferable option is to /not expand/ the
// wildcard, and let `legalTransition` use it as-is.

var transitions = {
	'red': ['green'],
	'green': ['orange'],
	'orange': ['red'],
	'blinking orange': ['red'],
	'*': ['blinking orange'],
};

var legalTransition = function(from, to) {
	return transitions[from].contains(to)
		|| transitions['*'].contains(to);
};

// We are back to modifying `legalTransition`, the more flexible
// option.  This time we generalize the concept of well transition,
// and our `transitions` object looks quite like the original
// specification.

// Ultimately, we want to be as close as possible to the original
// definition of the street light state machine :
//
// Red -> Green -> Orange -> Red
// * -> Blinking orange -> Red
//
// We are nearly there, currently we have extra verbosity due the
// object and array syntax.
//
// First, recognize that we can equivalently write
//
// Red -> Green -> Orange -> Red
// * -> Blinking orange -> Red
//
// or
//
// Red -> Green
// Green -> Orange
// Orange -> Red
// * -> Blinking orange
// Blinking orange -> Red
//
// And that this transformation is straightforward, and can be
// generalized for any chain of states.  (The inverse transformation
// might be less trivial.)
//
// So, let's focus on writing the second form to start with, and going
// back to the first one will only be a matter of transformation.

// As I said, we are very close to the original specification, except
// for matters of syntax.  How to remedy that?  We could say something
// like this:

'red'.to('green').to('orange').to('red');
'*'.to('blinking orange').to('red');

// But there is still extraneous syntax: dots, quotes, parentheses,
// semicolons.  At least, we gain a level of abstraction.  Let's do it
// for fun.
//
// We must redefine the String prototype to add the 'to' method.  It
// is not very good practice to modify the prototype of global
// Objects.  So will do it in a non-destructive manner.

var transitions = {};

var oldTo = String.prototype.to; // Probably undefined
String.prototype.to = function(toState) {
	if (!transitions[this])
		transitions[this] = [];
	transitions[this].push(toState);

	return toState;
}

'red'.to('green').to('orange').to('red');
'*'.to('blinking orange').to('red');

// When transitions are defined, revert changes to String.prototype
// FIXME: we are still 'defining' the 'to' property, instead of
// deleting it if it did not exist before.  Not quite clean if it is
// enumerable.  Can we modify global objects locally?
String.prototype.to = oldTo;

// We get the following transitions object:

{ red: [ 'green' ],
  green: [ 'orange' ],
  orange: [ 'red' ],
  '*': [ 'blinking orange' ],
  'blinking orange': [ 'red' ] }

// Great!  So this is nicer, but maybe not as convenient to type as
// the original specification with arrows.  We can do better.  But
// first, a little refactoring to better convey intent.

String.prototype.to = function(toState) {
	var fromState = this;
	addTransition(fromState, toState);
	return toState;
};

var addTransition = function(from, to) {
	if (!transitions[from])
		transitions[from] = [];
	transitions[from].push(to);
};

// Now, `'red'.to('green')` is just an alias for 'addTransition('red',
// 'green')'.  Similarly, we want `red -> green` to be another such
// alias.  To erase the obnoxious syntax, we can use macros.

.. using macros here would be transcendent

// The virtue of our transformations is that we have hidden the
// underlying data structure we used to represent transitions.  Now,
// as a user of my state machine library, I'm only concerned by the
// expression of the states and their transitions.  I don't care for
// implementation details.  I want to only have to say 'these are my
// transitions', now give me a state machine that I can execute'.
// Using transformations give users the ability to cleary express
// intent.
//
// But, as developpers of the state machine library, we also benefit
// from hiding the data structure.  Indeed, if we decide that we want
// to change the data structure, we just have to alter the
// transformations.  The specifications are never modified.


// Try to think of future requirements: what if have another such
// state from all other states to transition to?  What if I also have
// a transition from a state to all the others?
