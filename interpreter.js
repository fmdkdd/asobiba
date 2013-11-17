function interpretNode(ASTnode) {
  if (ASTnode.type in rules)
    return rules[ASTnode.type](ASTnode);
  else
    throw new Error("Unknown node type " + ASTnode.type);
}

var EmptyObject = function() {
	return Object.create(null);
}

var Obobject = {
  methods: EmptyObject(),

  invoke: function(name) {
    return this.methods[name].invoke(this);
  },

  update: function(name, newMethod) {
    // FIXME: Mutable semantics
	  this.methods[name] = newMethod;
  }
}

var Method = {
  name: undefined,
  body: undefined,

  invoke: function(self) {
    Runtime.identifiers.self = self;
	  return interpretNode(this.body);
  }
}

var Runtime = {
  identifiers: EmptyObject(),
}

var rules = {
  function: function(node) {
    throw "not yet";
  },

  identifier: function(node) {
    if (node.value in Runtime.identifiers)
	    return Runtime.identifiers[node.value];
    else
      throw new Error('unbound, ' + node.value);
  },

  object: function(node) {
	  var obj = clone(Obobject);

    for (var i in node.methods) {
      var method = node.methods[i];
      obj.methods[method.name] = interpretNode(method);
    }

    return obj;
  },

  method: function(node) {
    var m = clone(Method);

    m.name = node.name;
    m.body = node.body;

    return m;
  },

  methodCall: function(node) {
	  var r = interpretNode(node.receiver);
    var m = node.methodName.value;

    return r.invoke(m);
  },
}

function clone(obj) {
  var cloned = EmptyObject();

	for (var prop in obj) {
    var value = obj[prop];

    if (isPrimitive(value))
      cloned[prop] = value;
    else
      cloned[prop] = clone(value);
  }

  return cloned;
}

function isPrimitive(obj) {
  return typeof obj !== 'object';
}


module.exports = interpretNode;
