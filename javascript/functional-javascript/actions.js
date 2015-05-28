function existy(x) {
	return x != null;
}

function actions(acts, done) {
	return function(seed) {
    var init = { values: [], state: seed };

    var intermediate = acts.reduce(function(stateObj, action) {
      var result = action(stateObj.state);

      if(existy(result)) {
        var values = stateObj.values.concat([result]);
        return { values: values, state: result };
      } else {
        return stateObj;
      }
    }, init);

    var keep = intermediate.values.filter(existy);
    return done(keep, intermediate.state);
  };
}

// Test

function sqr(x) { return x * x; }
function note(msg) { console.log('NOTE:', msg); }
function neg(x) { return -x; }

actions([sqr, sqr, note, neg], console.log)(2);
