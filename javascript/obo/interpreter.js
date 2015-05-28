function interpretNode(ASTnode) {
  if (ASTnode.type in rules)
    return rules[ASTnode.type](ASTnode);
  else
    throw new Error("Unknown node type " + ASTnode.type);
}

var EmptyObject = function() {
	return Object.create(null);
};

var Obobject = {
  methods: EmptyObject(),

  invoke: function(name) {
    return this.methods[name].invoke(this);
  },

  update: function(name, newMethodBody) {
    // FIXME: Mutable semantics
	  this.methods[name].body = newMethodBody;
    return this;
  }
};

var Method = {
  name: undefined,
  body: undefined,

  invoke: function(self) {
    Runtime.identifiers.self = self;
	  return interpretNode(this.body);
  }
};

var Lambda = {
  argName: undefined,
  body: undefined,

  call: function(arg) {
    Runtime.identifiers[this.argName] = arg;
	  return interpretNode(this.body);
  }
};

var Runtime = {
  identifiers: EmptyObject(),
};

var rules = {
  func: function(node) {
    var f = clone(Lambda);

    f.argName = node.argument;
    f.body = node.body;

    return f;
  },

  apply: function(node) {
    var f = interpretNode(node.func);
    var a = interpretNode(node.argument);

    // Assume f is a Lambda object

    return f.call(a);
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

  methodUpdate: function(node) {
	  var r = interpretNode(node.receiver);
    var m = node.methodName.value;
    var v = node.body;

    return r.update(m, v);
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
