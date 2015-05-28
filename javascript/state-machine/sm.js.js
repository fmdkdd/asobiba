
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
