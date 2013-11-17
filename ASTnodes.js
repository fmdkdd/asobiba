var nodes = {
  object: nodeMaker('object', 'name', 'methods'),
  method: nodeMaker('method', 'name', 'body'),
  identifier: nodeMaker('identifier', 'value'),
  methodCall: nodeMaker('methodCall', 'receiver', 'methodName'),
};

function nodeMaker(type) {
  var rest = [].slice.call(arguments, 1);
  rest.unshift('type');
  return objectCreator.apply(this, rest).bind(undefined, type);
}

function objectCreator() {
  var names = [].slice.call(arguments);

  return function() {
    var args = [].slice.call(arguments);

    var o = {};
    for (var i in names) {
      o[names[i]] = args[i];
    }

    return o;
  }
}

module.exports = nodes;
