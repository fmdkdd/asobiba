var n = require('./ASTnodes');

function desugarNode(ASTnode) {
	if (ASTnode.type in rules)
    return rules[ASTnode.type](ASTnode);
  else
    return ASTnode;
}

var rules = {
  func: function(node) {
    var arg = desugarNode(node.argument);
    var body = desugarNode(node.body);

    return n.object(
      null,
      [
        n.method('arg', arg),
        n.method('val', body)
      ]
    );
  },

  apply: function(node) {
    var func = desugarNode(node.func);
    var arg = desugarNode(node.argument);

    return n.methodCall(
      n.methodUpdate(
        func,
        identifier('arg'),
        arg)
      identifier('val'),
    );
  },
};

modules.exports = desugarNode;
